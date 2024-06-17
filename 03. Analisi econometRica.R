# BSC Thesis: "L'impatto dei tassi di interesse sul settore bancario"
# Owner: Thomas De Massari
# Last update: 2024-06-17

rm(list=ls())

# SEZIONE 1: LIBRERIE ----
library(lmtest)
library(whitestrap)
library(tseries)
library(strucchange)
library(urca)
library(vars)
library(moments)
library(sandwich)
library(sasld)
library(timeSeries)
library(systemfit)

# Rimuovere i warnings
options(warn = -1)

# Acronimi delle banche
banks_acronym = c("hsbc", "bnp", "aca", "sanx", "bar", "gle", "dbk", "lloy", "isp", "inga", "ucg", 
                  "nwg", "stan", "bbva")

# SEZIONE 2:FUNZIONI ----
# Impostare i livelli del pvalue ----
# Questa funzione imposta un segno in base alla significativà del valore stimato
sign_pvalue = function(pvalue){ 
  levels = NULL
  
  for (j in 1:length(pvalue)){
    if ((pvalue[j] >= 0) & pvalue[j] <= 0.001){
      tmp = "***" 
    } else{
        if ((pvalue[j] > 0.001) & pvalue[j] <= 0.01){
          tmp = "**" 
        } else{
            if ((pvalue[j] > 0.01) & pvalue[j] <= 0.05){
              tmp = "*" 
            } else {
                if ((pvalue[j] > 0.05) & pvalue[j] <= 0.1){
                  tmp = "." 
                } else {
                  tmp = " "
                }
            }
        }
    }
  } 
  levels = tmp
  return(levels) 
}

# Esogeneità (correlazione semplice tra residui e esplicative) ---- 
# In input prende i residui del modello OLS e una dataset (cbind) con tutti i regressori. Se almeno
# uno di questi è correlato con il termine di errore allora restituisce FALSE, se no TRUE
exogeneity = function(res, X){
  cor_boolean = NA
  correlations_saved = NA
  for (i in 1:length(X[1,])){
    x = X[, i]
    cor_tmp = cor(x, res)
    if (round(cor_tmp,2) == 0){
      cor_boolean[i] = TRUE
    } else{
      cor_boolean[i] = FALSE
      correlations_saved = c(correlations_saved, paste0("- x", i, ",res: ", round(cor_tmp, 2)))
    }
  }
  
  correlations_saved = correlations_saved[-1]
  
  if (any(cor_boolean == FALSE)){
    return(paste(correlations_saved))
  } else{
    return(TRUE)
  }
}

# Stime dei modelli OLS -----
# In input la funzione prende le singole variabili come panel. È quindi necessario fornire un filtro.
# È importante rispettare la seguente sintassi:
# 1. Variabile dipendente
# 2. Regressori passati come cbind
# 3. Dummy passati come cbind. Si può omettere
# 4. Names_of_models: nome dei modelli da stimare. Da includere anche se si fa un solo modello
# 5. Flitro: variabile per filtrare il panel
# 6. intercetta = TRUE se si vuole includere l'interecetta
# 7. rendimenti = TRUE se si vogliono usare le differenze prime per il modello (sia nei regressori
# che nella dipendente)

OLSregression = function(dipendente, regressori, dummy, names_of_models, filtro, intercetta = TRUE, rendimenti = FALSE){
  if (!missing(dummy)){
    number_of_coefficients = length(regressori[1, ]) + length(dummy[1, ]) + (length(dummy[1, ])*length(regressori[1,]))
  } else{
    number_of_coefficients = length(regressori[1, ])
  }
  
  number_of_correlations = choose(number_of_coefficients, 2) # Questa mi serve dopo per la matrice della multicollinearità
  
  names_of_coefficients = NA; sd_matrix_names = NA
  for (j in 1:number_of_coefficients){
    names_of_coefficients = c(names_of_coefficients, paste0("b", j), paste0("pvalue - b", j))
    sd_matrix_names = c(sd_matrix_names, paste0("s.e. OLS - b", j), paste0("s.e. HC3 - b", j))
  }
  names_of_coefficients = names_of_coefficients[-1]
  sd_matrix_names = sd_matrix_names[-1]
  
  if (intercetta == TRUE){
    number_of_coefficients = number_of_coefficients + 1
    names_of_coefficients = c("b0", "pvalue - b0", names_of_coefficients)
    sd_matrix_names = c("s.e. OLS - b0", "s.e. HC3 - b0", sd_matrix_names)
  }

  # Matrice dei coefficienti (con i relativi p-value)
  coef_matrix = matrix(NA, nrow = length(names_of_models), ncol = (number_of_coefficients*2))
  rownames(coef_matrix) = names_of_models
  colnames(coef_matrix) = names_of_coefficients
  
  # Matrice degli standard error
  sds_matrix = matrix(NA, nrow = length(names_of_models), ncol = length(sd_matrix_names))
  rownames(sds_matrix) = names_of_models
  colnames(sds_matrix) = sd_matrix_names
  
  # Matrice dei residui 
  res_matrix = matrix(NA, nrow = length(names_of_models), ncol = length(dipendente[filtro == 1]))
  rownames(res_matrix) = names_of_models
  colnames(res_matrix) = 1:length(dipendente[filtro == 1])
  
  # Matrice dei test
  # NB: verranno calcolati solo i test che ho deciso di fare io. L'utente non ha modo di modificare
  # questa opzione
  test_names = c("R2", "R2 adj", "Test F (pvalue)", "Esogeneità","Goldfeld-Quandt (pvalue)", "White (pvalue)", 
                 "Shapiro-Wilk (pvalue)", "Jarque-Bera (pvalue)", "Curtosi", "Asimmetria", "Durbin-Watson (pvalue)", 
                 "Ljung-Box (pvalue)", "CUSUM (pvalue)", "RESET (pvalue)")
  test_matrix = matrix(NA, nrow = length(names_of_models), ncol = length(test_names))
  colnames(test_matrix) = test_names
  rownames(test_matrix) = names_of_models
  # Matrice dove andrò a salvare le correlazioni tra regressori
  multicollinearity_matrix = matrix(NA, nrow = length(names_of_models), ncol = number_of_correlations)
  rownames(multicollinearity_matrix) = names_of_models
  
  
  # Sistemo le matrici (devo togliere un'osservazione)
  if (rendimenti == TRUE){
    res_matrix = res_matrix[, -ncol(res_matrix)]
  }
  
  formula = NA # Formula del modello
  models = NA  # Oggetto modello 
  
  # STIMA DEL MODELLO
  for (i in 1:length(names_of_models)){
    if(rendimenti == TRUE){
      # CONSIDERO I RENDIMENTI
      
      # Controllo se ci sono valori negativi (siccome devo fare i logaritmi questi non possono esserci)
      # Nel caso di questa tesi solamente i tassi di interesse potrebbero essere negativi, ma il codice
      # lo scrivo per generalizzare e fare il controllo su tutte le variabili
      # Data la natura del problema, se il valore è negativo lo sostiuisco con uno 0.00001 (valore piccolo,
      # molto prossimo allo 0, scelto a piacere)
      for (j in 1:(length(regressori[1, ]))){
        for(z in 1:(length(regressori[, 1]) - 1)){
          if(as.numeric(regressori[z, j]) <= 0){
            regressori[z, j] = 0.00001
          }
        }
      }
      
      # Dataframe con le variabili
      dataframe_tmp = data.frame(y = rep(NA, (length(dipendente[filtro == 1]) - 1)))
      # Variabile dipendente
      y_log = log(dipendente[filtro == i]); y_model = diff(y_log)
      dataframe_tmp[, "y"] = y_model
      
      # Variabili esplicative
      for (j in 1:length(regressori[1, ])){
        x_name = paste0("x", j, "_model")
        x_log = log(regressori[, j][filtro == i]); x_logdiff = diff(x_log)
        assign(x_name, x_logdiff)
        dataframe_tmp[[x_name]] = get(x_name)
      }
      if (!missing(dummy)){
        # Dummy e interazione tra dummy e esplicative
        for (j in 1:length(dummy[1, ])){
          d_name = paste0("d", j, "_model")
          assign(d_name, dummy[, j][filtro == i])
          dataframe_tmp[[d_name]] = get(d_name)
          
          for (z in 1:length(regressori[1, ])){
            dx_name = paste0("d",j,"x",z,"_model")
            x_log = log(regressori[, z][filtro == i]); x_logdiff = diff(x_log)
            assign(dx_name, (x_logdiff) * (dummy[, j][filtro == i]))
            dataframe_tmp[[dx_name]] = get(dx_name)
          }
        }
      }
      
    } else{
      # NON CONSIDERO I RENDIMENTI
      # Dataframe con le variabili
      dataframe_tmp = data.frame(y = rep(NA, length(dipendente[filtro == 1])))
      # Variabile dipendente
      y_model = dipendente[filtro == i]
      dataframe_tmp = data.frame(y = y_model)
      
      # Variabili esplicative
      for (j in 1:length(regressori[1, ])){
        x_name = paste0("x", j, "_model")
        assign(x_name, regressori[, j][filtro == i])
        dataframe_tmp[[x_name]] = get(x_name)
      }
      
      if (!missing(dummy)){
        # Dummy e interazione tra dummy e esplicative
        for (j in 1:length(dummy[1, ])){
          d_name = paste0("d", j, "_model")
          assign(d_name, dummy[, j][filtro == i])
          dataframe_tmp[[d_name]] = get(d_name)
          
          for (z in 1:length(regressori[1, ])){
            dx_name = paste0("d",j,"x",z,"_model")
            assign(dx_name, (regressori[, z][filtro == i]) * (dummy[, j][filtro == i]))
            dataframe_tmp[[dx_name]] = get(dx_name)
          }
        }
      }
    }
    
    # Insieme dei regressori: x, d, xd
    # (mi serve dopo il calcolo dell'esogeneità come correlazione semplice)
    regressori_model = rep(NA, length(y_model))
    for (index_tmp in 2:length(dataframe_tmp)){
      regressori_model = cbind(regressori_model, dataframe_tmp[, index_tmp])
    }
    regressori_model = regressori_model[, -1]
    
    # STIMA DEI MODELLI
    model_OLS = lm(y ~ ., data = dataframe_tmp)
    summary_model_OLS = summary(model_OLS)
    
    # RESIDUI
    res_matrix[i, ] = model_OLS$residuals
    
    # Salvo la formula del modello e il modello
    formula = c(formula, model_OLS$call)
    models = c(models, model_OLS)
    
    # Correzione degli s.e. con HC3
    hc3 = coeftest(model_OLS, vcov = vcovHC(model_OLS, type = "HC3"))
    
    # STANDARD ERRORS
    se_OLS = summary_model_OLS$coefficients[, "Std. Error"]
    se_hc3 = hc3[, "Std. Error"]
    # Salvo gli s.e. in una matrice
    index_seols = seq(1, (number_of_coefficients * 2), by = 2)
    index_sehc3 = seq(2, (number_of_coefficients * 2), by = 2)
    for (j in 1:length(index_seols)){
      sds_matrix[i, index_seols[j]] = se_OLS[j]
      sds_matrix[i, index_sehc3[j]] = se_hc3[j]
    }
    
    # COEFFICIENTI
    index_betas = seq(1, (number_of_coefficients * 2), by = 2)
    index_pvalue = seq(2, (number_of_coefficients * 2), by = 2)
    for (j in 1:number_of_coefficients){
      coef_matrix[i, index_betas[j]] = round(hc3[, "Estimate"][j], 5)
      coef_matrix[i, index_pvalue[j]] = paste(round(hc3[, "Pr(>|t|)"][j], 5), 
                                              sign_pvalue(hc3[, "Pr(>|t|)"][j]))
    }
    
    # R2 e R2 adj
    test_matrix[i, 1] = round(summary_model_OLS$r.squared, 5)     # R2
    test_matrix[i, 2] = round(summary_model_OLS$adj.r.squared, 5) # R2 adj
    
    # Test F
    f = summary_model_OLS$fstatistic
    pvalue_f = pf(f[1], f[2], f[3], lower.tail = FALSE)
    pvalue_f_sign = sign_pvalue(pvalue_f)
    test_matrix[i, 3] = paste(round(pvalue_f, 5),  pvalue_f_sign)
    
    # Esogeneità (calcolata come semplice correlazione tra regressori e residui)
    exogeneity_justcorr = exogeneity(residuals(model_OLS), regressori_model)
    test_matrix[i, 4] = as.character(exogeneity_justcorr) 
    
    # Multicollineartà
    dataframe_for_multicollinearity = dataframe_tmp[, -1]
    index_multicollinearity = 1
    colnames_multicollinearity = NA
    for (j in 1:length(dataframe_for_multicollinearity[1, ])){
      for (z in 1:length(dataframe_for_multicollinearity[1, ])){
        if (j < z){
          correlation = cor(dataframe_for_multicollinearity[, j], dataframe_for_multicollinearity[, z])
          multicollinearity_matrix[i, index_multicollinearity] = round(correlation, 5)
          colnames_multicollinearity = c(colnames_multicollinearity, paste0("cor(",colnames(dataframe_for_multicollinearity[j])
                                                                            ,",",colnames(dataframe_for_multicollinearity[z]),")"))
          index_multicollinearity = index_multicollinearity + 1
        }
      }
    }
    colnames_multicollinearity = colnames_multicollinearity[-1]
    colnames(multicollinearity_matrix) = colnames_multicollinearity
    
    # TEST DI OMOSCHEDASTICITA'
    # Goldfeld-Quant, White
    gq_pvalue = as.numeric(gqtest(model_OLS)[5])
    test_matrix[i, 5] = paste(round(gq_pvalue, 5), sign_pvalue(gq_pvalue))
    
    white_pvalue = as.numeric(white_test(model_OLS)[2])
    test_matrix[i, 6] = paste(round(white_pvalue,5), sign_pvalue(white_pvalue))
    
    # TEST DI NORMALITA'
    # Shapiro-Wilk, Jarque-Bera, Curtusi, Asimmetria
    shapiro_pvalue = as.numeric(shapiro.test(model_OLS$residuals)[2])
    test_matrix[i, 7] = paste(round(shapiro_pvalue, 5), sign_pvalue(shapiro_pvalue))
    
    jb_pvalue = as.numeric(jarque.bera.test(model_OLS$residuals)[3])
    test_matrix[i, 8] = paste(round(jb_pvalue, 5), sign_pvalue(jb_pvalue))
    
    test_matrix[i, 9] = (round(kurtosis(residuals(model_OLS)), 5) + 3)
    test_matrix[i, 10] = round(skewness(residuals(model_OLS)), 5)
    
    # TEST DI ASSENZA DI AUTOCORRELAZIONE
    dw_pvalue = as.numeric(dwtest(model_OLS)[4])
    test_matrix[i, 11] = paste(round(dw_pvalue, 5), sign_pvalue(dw_pvalue))
    
    ljungbox_pvalue = as.numeric(Box.test(residuals(model_OLS), lag = 1, type = "Ljung-Box")[3])
    test_matrix[i, 12] = paste(round(ljungbox_pvalue, 5), sign_pvalue(ljungbox_pvalue))
    
    # TEST DI ASSENZA DI BREAK STRUTTURALI
    cusum_pvalue = as.numeric(sctest(model_OLS, type = "cusum")[2])
    test_matrix[i, 13] = paste(round(cusum_pvalue, 5), sign_pvalue(cusum_pvalue))
    
    # TEST DI CORRETTA FORMA FUNZIONALE
    reset_pvalue = as.numeric(resettest(model_OLS)[4])
    test_matrix[i, 14] = paste(round(reset_pvalue, 5), sign_pvalue(reset_pvalue))
  }
  
  formula = formula[-1]; models = models[-1]
  
  return(list(formula = formula(), models = models, coef = coef_matrix, test = test_matrix, sds = sds_matrix, 
              res = res_matrix, multicollinearity = multicollinearity_matrix))
}

# Funzione per prendere le stime dei modelli e salvarli in una matrice ----
# Questa funzione mi è utile per generalizzare un pò il codice che segue per non ripetere sempre
# le solite istruzioni. Sicuramente funziona per il SUR
# Restituisce la matrice che viene passata all'inizio. Importante che abbia sulle righe i nomi
# dei modelli stimati e sulle colonne i parametri. 
model2csv = function(model, matrix_of_result){
  for (i in 1:nrow(matrix_of_result)){
    summary_of_model = summary(model$eq[[i]])
    coef = summary_of_model$coefficients[, "Estimate"]
    pvalue = summary_of_model$coefficients[, "Pr(>|t|)"]
    
    index_tmp = 1; j = 1
    for (index_tmp in 1:(length(coef))){
      coef_tmp = coef[index_tmp]; pvalue_tmp = pvalue[index_tmp]
      matrix_of_result[i, j] = round(coef_tmp, 5)
      matrix_of_result[i, (j + 1)] = paste(round(pvalue_tmp, 5), sign_pvalue(pvalue_tmp))
      j = j + 2
      index_tmp = index_tmp + 1
    }
  }
  return(matrix_of_result)
}

# SUR OR NOT SUR -----
# La funzione prende in input la matrice di correlazione (completa, quindi inclusa di diagonale e 
# duplicaizoni) e resituisce poi la statistica test e il pvalue per saggiare
# HO: cov12 = cov13 = ... = cov_nm. Se rifiuta allora vuol dire che le covarianze (e quindi le correlazioni)
# sono diverse da 0 e quindi si dovrebbe usare il SUR

surORNOTsur = function(correlations_full, times, number_of_equations){
  vector_of_corrs = NA
  for (i in 1:nrow(correlations_full)){
    for (j in 1:ncol(correlations_full)){
      if (i < j){
        vector_of_corrs = c(vector_of_corrs, correlations_full[i, j])
      }
    }
  }
  vector_of_corrs = vector_of_corrs[-1]
  test_statistic = (sum(vector_of_corrs^2))*times
  df = ((number_of_equations)*(number_of_equations-1))/2
  p_value = 1 - pchisq(test_statistic, df)
  
  return(list(test_statistic = test_statistic, pvalue = p_value)) 
}



# Funzione per applicare i test necessari al SUR ----
# È importante che le variabili vengono passate con cbind e che ci sia anche la variabile dipendente
# A differenza dell'altra, questa funzione funziona solo per il caso in questione

test_SUR  = function(modello, dipendente, regressori, filtro, names_of_models, number_of_coefficients, rendimenti = FALSE){
  # Matrice per i risultati dei test
  test_names = c("R2", "R2 adj", "Test F (pvalue)", "Esogeneità","Goldfeld-Quandt (pvalue)", "White (pvalue)", 
                 "Shapiro-Wilk (pvalue)", "Jarque-Bera (pvalue)", "Curtosi", "Asimmetria", "Durbin-Watson (pvalue)", 
                 "Ljung-Box (pvalue)", "CUSUM (pvalue)", "RESET (pvalue)")
  test_matrix = matrix(NA, nrow = length(names_of_models), ncol = length(test_names))
  colnames(test_matrix) = test_names
  rownames(test_matrix) = names_of_models
  
  number_of_correlations = choose(number_of_coefficients, 2) # Questa mi serve dopo per la matrice della multicollinearità
  
  # Matrice dove andrò a salvare le correlazioni tra regressori
  multicollinearity_matrix = matrix(NA, nrow = length(names_of_models), ncol = number_of_correlations)
  rownames(multicollinearity_matrix) = names_of_models
  
  for(j in 1:nrow(test_matrix)){
    # Prendo il modello tra i SUR che mi sono stati passati in input
    model = modello$eq[[j]]
    summary_model = summary(model)
    
    
    # Calcolo dei rendimenti
    if(rendimenti == TRUE){
      # Controllo se ci sono valori negativi (siccome devo fare i logaritmi questi non possono esserci)
      # Data la natura del problema, se il valore è negativo lo sostiuisco con uno 0.00001 (valore piccolo,
      # molto prossimo allo 0, scelto a piacere)
      for (i in 1:(length(regressori[1, ]))){
        for(z in 1:(length(regressori[, 1]) - 1)){
          if(as.numeric(regressori[z, i]) <= 0){
            regressori[z, i] = 0.00001
          }
        }
      }
      
      
      y_tmp = dipendente[filtro == j]
      
      # Dipendente
      log_y = log(y_tmp); diff_y = diff(log_y)
      y = diff_y
      dataframe_tmp = data.frame(y = y)
      
      # Regressori
      for(z in 1:length(regressori[1, ])){
        x_name = paste0("x", z)
        x_log = log(regressori[, z][filtro == j]); x_logdiff = diff(x_log)
        assign(x_name, x_logdiff)
        dataframe_tmp[[x_name]] = get(x_name)
      }
    } else{
      # Variabile dipendente
      y = dipendente[filtro == j]
      dataframe_tmp = data.frame(y = y)
      
      # Regressori
      for(z in 1:length(regressori[1, ])){
        x_name = paste0("x", z)
        assign(x_name, regressori[, z][filtro == j])
        dataframe_tmp[[x_name]] = get(x_name)
      }
    }
    
    # Insieme dei regressori
    # (mi serve dopo il calcolo dell'esogeneità come correlazione semplice)
    regressori_model = rep(NA, length(y))
    for (index_tmp in 2:length(dataframe_tmp)){
      regressori_model = cbind(regressori_model, dataframe_tmp[, index_tmp])
    }
    regressori_model = regressori_model[, -1]
    
    # Multicollineartà
    dataframe_for_multicollinearity = dataframe_tmp[, -1]
    index_multicollinearity = 1
    colnames_multicollinearity = NA
    for (i in 1:length(dataframe_for_multicollinearity[1, ])){
      for (z in 1:length(dataframe_for_multicollinearity[1, ])){
        if (i < z){
          correlation = cor(dataframe_for_multicollinearity[, i], dataframe_for_multicollinearity[, z])
          multicollinearity_matrix[j, index_multicollinearity] = round(correlation, 5)
          colnames_multicollinearity = c(colnames_multicollinearity, paste0("cor(",colnames(dataframe_for_multicollinearity[i])
                                                                            ,",",colnames(dataframe_for_multicollinearity[z]),")"))
          index_multicollinearity = index_multicollinearity + 1
        }
      }
    }
    colnames_multicollinearity = colnames_multicollinearity[-1]
    colnames(multicollinearity_matrix) = colnames_multicollinearity
    
    # Aggiungo e salvo test 
    test_matrix[j, 1] = round(summary_model$r.squared, 5) # R2
    test_matrix[j, 2] = round(summary_model$adj.r.squared, 5) # R2 adj
    
    # Non si può calcolare il test F
    test_matrix[j, 3] = "--NA--"
    
    # Esogeneità (calcolata come semplice correlazione tra regressori e residui)
    exogeneity_justcorr = exogeneity(residuals(model), regressori_model)
    test_matrix[j, 4] = as.character(paste(exogeneity_justcorr, collapse = " ")) # Esogeneità come semplice correlazione
    # pvalue del test di Goldfeld-Quandt (per saggiare l'ipotesi di assenza di eteroschedasticità)
    gq_pvalue = as.numeric(gqtest(model)[5])
    test_matrix[j, 5] = paste(round(gq_pvalue, 5), sign_pvalue(gq_pvalue))
    # pvalue del test di White (per saggiare l'ipotesi di assenza di eteroschedasticità)
    white_pvalue = as.numeric(white_test(model)[2])
    test_matrix[j, 6] = paste(round(white_pvalue,5), sign_pvalue(white_pvalue))
    # pvalue del test di Shapiro-Wilk (per saggiare l'ipotesi di normalità)
    shapiro_pvalue = as.numeric(shapiro.test(model$residuals)[2])
    test_matrix[j, 7] = paste(round(shapiro_pvalue, 5), sign_pvalue(shapiro_pvalue))
    # pvalue del test di Jarque-Bera (per saggiare l'ipotesi di normalità)
    jb_pvalue = as.numeric(jarque.bera.test(model$residuals)[3])
    test_matrix[j, 8] = paste(round(jb_pvalue, 5), sign_pvalue(jb_pvalue))
    # Curtosi e asimmetria
    test_matrix[j, 9] = (round(kurtosis(residuals(model)), 5) + 3)
    test_matrix[j, 10] = round(skewness(residuals(model)), 5)
    # pvalue del test di Durbin-Watson (per saggiare l'ipotesi di assenza di autocorrelazione tra i residui)
    dw_pvalue = as.numeric(dwtest(model)[4])
    test_matrix[j, 11] = paste(round(dw_pvalue, 5), sign_pvalue(dw_pvalue))
    # pvalue del test di Ljung-Box
    ljungbox_pvalue = as.numeric(Box.test(residuals(model), lag = 1, type = "Ljung-Box")[3])
    test_matrix[j, 12] = paste(round(ljungbox_pvalue, 5), sign_pvalue(ljungbox_pvalue))
    # pvalue del test CUSUM (per saggiare l'assenza di break strutturali)
    cusum_pvalue = as.numeric(sctest(efp(residuals(model) ~ 1, type = "OLS-CUSUM"))[2])
    test_matrix[j, 13] = paste(round(cusum_pvalue, 5), sign_pvalue(cusum_pvalue))
    # pvalue del test RESET di Ramsey (per controllare la forma funzionale del modello)
    reset_pvalue = as.numeric(resettest(model)[4])
    test_matrix[j, 14] = paste(round(reset_pvalue, 5), sign_pvalue(reset_pvalue))
  }
  return(list(test = test_matrix, multicollinearity = multicollinearity_matrix))
}

# SEZIONE 3: UPLOAD DEI DATASETS ----
setwd("/Users/thomasdemassari/Library/CloudStorage/OneDrive-Personal/Università/BSc/Tesi/Data/")

# Sezione 3a: PANELS ---- 
# Dati di bilancio e tassi di interesse (elaborazione personale, vedi codice in Python)
data_fs = read.csv("financial_statement_panel.csv") 
# Dati sui prezzi azionari, indici di mercato e tassi di intresse (fonte: elaborazione personale, vedi codice in Python)
data_sp = read.csv("stock_price_panel.csv")

# Sezione 3b: EUROZONA e ECB ----
# Dato sull'inflazione europea (fonte: ECB)
eu_inflation_csv = read.csv("Macroeconomics conditions/EU Inflation - annual.csv")
eu_inflation = window(ts(eu_inflation_csv$Value, start = 1997, frequency = 1), start = 2006)
# Bilancio della ECB, annuale (fonte: ECB)
ecb_balancesheet_csv = read.csv("Macroeconomics conditions/ECB Balance sheet.csv", header = TRUE, sep = ",")
ecb_deposit = ts(as.numeric(subset(ecb_balancesheet_csv[29,], select = -c(1))), start = 1999, frequency = 1)
# Euribor mensile con serie storica completa (fonte: ECB)
euribor_m_csv = read.csv("Interest rate/EURIBOR - monthly.csv", header = TRUE, sep = ",")
euribor_m = ts(euribor_m_csv[3], start = c(1994, 1), frequency = 12)
euribor_m_9923 = window(euribor_m, start = c(1999, 1), end = c(2023, 12))
# Euribor annuale con serie storica completa (fonte: mia elaborazione su dati ECB)
euribor_a_csv = read.csv("Interest rate/EURIBOR - annual.csv", header = TRUE, sep = ",")
euribor_a = ts(euribor_a_csv[2], start = 1994, frequency = 1)
euribor_a_9923 = window(euribor_a, start = 1999, end = 2023)
# Tasso sui depositi, monthly (fonte: FRED)
ecb_depositrate_m_csv = read.csv("Macroeconomics conditions/ECB Deposit rate - monthly.csv", header = TRUE, sep = ",")
ecb_depositrate_m = ts(ecb_depositrate_m_csv[2], start = c(1999,1), frequency = 12)
ecb_depositrate_m_9923 = window(ecb_depositrate_m, start = c(1999, 1), end = c(2023, 12))
# Tasso overnight, monthly (fonte: FRED)
ecb_overnightrate_m_csv = read.csv("Macroeconomics conditions/ECB Overnight rate - monthly.csv", header = TRUE, sep = ",")
ecb_overnightrate_m = ts(ecb_overnightrate_m_csv[2], start = c(1999,1), frequency = 12)
ecb_overnightrate_m_9923 = window(ecb_overnightrate_m, start = c(1999, 1), end = c(2023, 12))
# Tasso di rifinanziamento ufficiale, monthly (fonte: FRED e ECB)
ecb_mro_m_csv = read.csv("Macroeconomics conditions/ECB MRO - monthly.csv", header = TRUE, sep = ",")
ecb_mro_m = ts(ecb_mro_m_csv[2], start = c(1999,1), frequency = 12)
ecb_mro_m_9923 = window(ecb_mro_m, start = c(1999, 1), end = c(2023, 12))
# TIER 1 EU
tier1 = read.csv("Banks/TIER 1 EU.csv", header = TRUE, sep = ",")
tier1 = ts(as.numeric(tier1[, "OBS.VALUE"]), start = 2007, frequency = 1)
tier1_1522 = window(tier1, start = 2015)
# CET 1 EU
cet1 = read.csv("Banks/CET 1 EU.csv", header = TRUE, sep = ",")
cet1 = ts(as.numeric(cet1[, "OBS.VALUE"]), start = 2014, frequency = 1)
cet1_1522 = window(cet1, start = 2015)

# Correlazione tra inflazione e euribor
cor(eu_inflation, data_fs$InterestRate[data_fs$Bank == "ucg"])

# Sezione 2c: FED ----
# Federal Funds Rate (fonte: FRED)
fed_ffr_csv = read.csv("Macroeconomics conditions/FED Federal Funds Rate - monthly.csv", header = TRUE, sep = ",")
fed_ffr = ts(fed_ffr_csv[2], start = c(1954, 7), frequency = 12)
fed_ffr_9024 = window(fed_ffr, start = c(1990), end = c(2024, 2))
# M1 (fonte: FRED)
fed_m1_csv = read.csv("Macroeconomics conditions/FED M1 - monthly.csv", header = TRUE, sep = ",")
fed_m1 = ts(fed_m1_csv[2], start = c(1975, 1), frequency = 12)
fed_m1_9024 = window(fed_m1, start = c(1990), end = c(2024, 2))
# M0 (fonte: FRED)
fed_m0_csv = read.csv("Macroeconomics conditions/FED M0 - monthly.csv", header = TRUE, sep = ",")
fed_m0 = ts(fed_m0_csv[2], start = c(1959, 1), frequency = 12)
fed_m0_9024 = window(fed_m0, start = c(1990), end = c(2024, 2))
# Reserve (fonte: FRED)
fed_reserve_csv = read.csv("Macroeconomics conditions/FED Reserve - monthly.csv", header = TRUE, sep = ",")
fed_reserve = ts(fed_reserve_csv[2], start = c(1959, 1), frequency = 12)
fed_reserve_9024 = window(fed_reserve, start = c(1990), end = c(2024, 2))
# Bilancio della FED, annual (fonte: FRED)
fed_assets.csv = read.csv("Macroeconomics conditions/FED Total assets.csv", header = TRUE, sep = ",")
fed_assets = ts(fed_assets.csv[2], start = 2003, frequency = 1)
fed_assets_05.23 = window(fed_assets, start = 2005, end = 2023)


# SEZIONE 4: ANALISI ECONOMETRICA ----
# SEZIONE 4a: TEST DI STAZIONARIETA' ----
# Test di stazionarietà sui tassi di interesse (annuali) ----
# Matrice dove salverò i risultati dei test 
intrate_stationarity = matrix(c(0,0),2,4)
colnames(intrate_stationarity) = c("kpss", "pp", "adf", "ar(1)")
rownames(intrate_stationarity) = c("Libor", "Euribor")

# Libor
libor = data_fs$InterestRate[data_fs$Bank == "hsbc"]                      # Tasso libor
plot(ts(libor, start = 2006)) 
intrate_stationarity[1, 1] = as.numeric(kpss.test(libor, "Trend")[3])     # pvalue del kpss
intrate_stationarity[1, 2] = as.numeric(pp.test(libor)[4])                # pvalue del pp
intrate_stationarity[1, 3] = as.numeric(adf.test(libor)[4])               # pvalue di adf
intrate_stationarity[1, 4] = as.numeric(arima(libor, c(1,0,0))$coef[1])   # coefficiente di AR(1)
acf(libor, main = "ACF Libor")
# Euribor
euribor = data_fs$InterestRate[data_fs$Bank == "ucg"]                     # Tasso Euribor
plot(ts(euribor, start = 2006))
intrate_stationarity[2, 1] = as.numeric(kpss.test(euribor, "Trend")[3])   # pvalue del kpss
intrate_stationarity[2, 2] = as.numeric(pp.test(euribor)[4])              # pvalue del pp
intrate_stationarity[2, 3] = as.numeric(adf.test(euribor)[4])             # pvalue di adf
intrate_stationarity[2, 4] = as.numeric(arima(euribor, c(1,0,0))$coef[1]) # coefficiente di AR(1)
acf(euribor, main = "ACF Euribor")

# Risultati dei test
print(intrate_stationarity)
nome_file = "Tassi di interesse (annuali).csv"
write.csv(intrate_stationarity, paste0("/Users/thomasdemassari/Library/CloudStorage/OneDrive-Personal/Università/BSc/Tesi/Data/Results/Test di stazionarietà/",nome_file))
# Secondo tutti 3 i test sia il libor che l'euribor non sono stazionario ma il coeff dell'ar(1) è 0.75/0.8

# Test di stazionarietà sul ROA, NII, OI, PROV ----
# ID delle singole banche
index = c(1:14)  

# Grafici
par(mfrow = c(4,4))
variabile = "roa"
for (i in 1:length(index)){
  if (variabile == "roa"){
    roa_ts = ts(data_fs$ROA[data_fs$ID == i], start = 2006, frequency = 1)
    plot(roa_ts, main = paste("ROA - ", banks_acronym[i]))
  } else {
    if (variabile == "nii"){
      nii_ts = ts(data_fs$NII[data_fs$ID == i], start = 2006, frequency = 1)
      plot(nii_ts, main = paste("NII - ", banks_acronym[i]))
    } else{
      if (variabile == "oi"){
        oi_ts = ts(data_fs$OI[data_fs$ID == i], start = 2006, frequency = 1)
        plot(oi_ts, main = paste("OI - ", banks_acronym[i]))
      } else {
        if (variabile == "prov"){
          prov_ts = ts(data_fs$PROV[data_fs$ID == i], start = 2006, frequency = 1)
          plot(prov_ts, main = paste("PROV - ", banks_acronym[i]))
        }
      }
    }
  }
}
par(mfrow = c(1,1))

# Matrice dove salverò i risultati dei test
# ROA
roa_stationarity = matrix(c(0,0),14,4)       
colnames(roa_stationarity) = c("kpss", "pp", "adf", "ar(1)")
rownames(roa_stationarity) = banks_acronym
# NII
nii_stationarity = matrix(c(0,0),14,4)
colnames(nii_stationarity) = c("kpss", "pp", "adf", "ar(1)")
rownames(nii_stationarity) = banks_acronym
# OI
oi_stationarity = matrix(c(0,0),14,4)
colnames(oi_stationarity) = c("kpss", "pp", "adf", "ar(1)")
rownames(oi_stationarity) = banks_acronym
# PROV
prov_stationarity = matrix(c(0,0),14,4)
colnames(prov_stationarity) = c("kpss", "pp", "adf", "ar(1)")
rownames(prov_stationarity) = banks_acronym

for (i in 1:length(index)){
  # ROA
  roa = data_fs$ROA[data_fs$ID == i]
  roa_stationarity[i, 1] = as.numeric(kpss.test(roa)[3])                           # pvalue di kpss
  roa_stationarity[i, 2] = as.numeric(pp.test(roa)[4])                             # pvalue di pp
  roa_stationarity[i, 3] = as.numeric(adf.test(roa)[4])                            # pvalue di adf
  roa_stationarity[i, 4] = as.numeric(as.numeric(arima(roa, c(1,0,0))$coef[1]))    # coefficiente di AR(1)
  
  # NII
  nii = data_fs$NII_Assets[data_fs$ID == i]
  nii_stationarity[i, 1] = as.numeric(kpss.test(nii)[3])                           # pvalue di kpss
  nii_stationarity[i, 2] = as.numeric(pp.test(nii)[4])                             # pvalue di pp
  nii_stationarity[i, 3] = as.numeric(adf.test(nii)[4])                            # pvalue di adf
  nii_stationarity[i, 4] = as.numeric(as.numeric(arima(nii, c(1,0,0))$coef[1]))    # coefficiente di AR(1)
  
  # OI
  oi = data_fs$OI_Assets[data_fs$ID == i]
  oi_stationarity[i, 1] = as.numeric(kpss.test(oi)[3])                             # pvalue di kpss
  oi_stationarity[i, 2] = as.numeric(pp.test(oi)[4])                               # pvalue di pp
  oi_stationarity[i, 3] = as.numeric(adf.test(oi)[4])                              # pvalue di adf
  oi_stationarity[i, 4] = as.numeric(as.numeric(arima(oi, c(1,0,0))$coef[1]))      # coefficiente di AR(1)
  
  # PROV
  prov = data_fs$PROV_Assets[data_fs$ID == i]
  prov_stationarity[i, 1] = as.numeric(kpss.test(prov)[3])                          # pvalue di kpss
  prov_stationarity[i, 2] = as.numeric(pp.test(prov)[4])                            # pvalue di pp
  prov_stationarity[i, 3] = as.numeric(adf.test(prov)[4])                           # pvalue di adf
  prov_stationarity[i, 4] = as.numeric(as.numeric(arima(prov, c(1,0,0))$coef[1]))   # coefficiente di AR(1)
}

nome_file = "ROA.csv"
write.csv(roa_stationarity, paste0("/Users/thomasdemassari/Library/CloudStorage/OneDrive-Personal/Università/BSc/Tesi/Data/Results/Test di stazionarietà/",nome_file))
nome_file = "NII.csv"
write.csv(nii_stationarity, paste0("/Users/thomasdemassari/Library/CloudStorage/OneDrive-Personal/Università/BSc/Tesi/Data/Results/Test di stazionarietà/",nome_file))
nome_file = "OI.csv"
write.csv(oi_stationarity, paste0("/Users/thomasdemassari/Library/CloudStorage/OneDrive-Personal/Università/BSc/Tesi/Data/Results/Test di stazionarietà/",nome_file))
nome_file = "PROV.csv"
write.csv(prov_stationarity, paste0("/Users/thomasdemassari/Library/CloudStorage/OneDrive-Personal/Università/BSc/Tesi/Data/Results/Test di stazionarietà/",nome_file))


# Risultati dei test 
print(roa_stationarity)
# ROA: Secondo il kpss (per la maggior parte delle banche) il ROA è stazionario, ma secondo pp e adf
# (per la maggior parte delle banche) il ROA è non stazionario. I coefficienti di AR(1) non sono
# alti (salvo per stan = 0.8 e bbva = 0,7). Quindi?
print(nii_stationarity)
# NII: stesso discorso di sopra
print(oi_stationarity)
# OI: stesso discorso di sopra
print(prov_stationarity)
# PROV: stesso discorso di sopra

# ACF
par(mfrow = c(4,4))
variabile = "oi"
for (i in 1:length(index)){
  if (variabile == "roa"){
    acf(data_fs$ROA[data_fs$ID == i], main = paste("ACF - ROA di", banks_acronym[i]))
  } else {
    if (variabile == "nii"){
      acf(data_fs$NII[data_fs$ID == i], main = paste("ACF - NII di", banks_acronym[i]))
    } else{
      if (variabile == "oi"){
        acf(data_fs$OI[data_fs$ID == i], main = paste("ACF - OI di", banks_acronym[i]))
      } else {
        if (variabile == "prov"){
          acf(data_fs$PROV[data_fs$ID == i], main = paste("ACF - PROV di", banks_acronym[i]))
        }
      }
    }
  }
}
par(mfrow = c(1,1))


# Test di stazionarietà sulle riserve in banca centrale ----
# Matrice dove salverò i risultati dei test
dcb_stationarity = matrix(c(0,0),14,4)       
colnames(dcb_stationarity) = c("kpss", "pp", "adf", "ar(1)")
rownames(dcb_stationarity) = banks_acronym

par(mfrow = c(4,4))
for (i in 1:length(index)){
  dcb = data_fs$DepositCentralBank_Assets[data_fs$ID == i]
  dcb_stationarity[i, 1] = as.numeric(kpss.test(dcb, "Trend")[3])                 # pvalue di kpss
  dcb_stationarity[i, 2] = as.numeric(pp.test(dcb)[4])                             # pvalue di pp
  dcb_stationarity[i, 3] = as.numeric(adf.test(dcb)[4])                            # pvalue di adf
  dcb_stationarity[i, 4] = as.numeric(as.numeric(arima(dcb, c(1,0,0))$coef[1]))    # coefficiente di AR(1)
  
  # plot(ts(dcb, start = c(2006,1), frequency = 1), main = paste("DCB - ", banks_acronym[i]))
  acf(dcb, main = paste("ACF - Riserve di", banks_acronym[i]))
}
par(mfrow = c(1,1))

# Risultati
print(dcb_stationarity)
nome_file = "Riserve in banca centrale.csv"
write.csv(dcb_stationarity, paste0("/Users/thomasdemassari/Library/CloudStorage/OneDrive-Personal/Università/BSc/Tesi/Data/Results/Test di stazionarietà/",nome_file))
# Le riserve in banca centrale non sono stazionarie

# Test di stazionarietà sui prezzi delle azioni ----
pricestock_stationarity = matrix(c(0,0),14,4)
colnames(pricestock_stationarity) = c("kpss", "pp", "adf", "ar(1)")
rownames(pricestock_stationarity) = banks_acronym

# Test
par(mfrow = c(4,4))
for (i in 1:length(index)){
    sp = data_sp$Stockprice[data_sp$ID == i]
    pricestock_stationarity[i, 1] = as.numeric(kpss.test(sp)[3])                           # pvalue di kpss
    pricestock_stationarity[i, 2] = as.numeric(pp.test(sp)[4])                             # pvalue di pp
    pricestock_stationarity[i, 3] = as.numeric(adf.test(sp)[4])                            # pvalue di adf
    pricestock_stationarity[i, 4] = as.numeric(as.numeric(arima(sp, c(1,0,0))$coef[1]))    # coefficiente di AR(1)
    
    # plot(ts(sp, start = c(2006,1), frequency = 12), main = paste("Stock price - ", banks_acronym[i]))
    acf(sp)
}
par(mfrow = c(1,1))

# Risultati
print(pricestock_stationarity)
nome_file = "Azioni.csv"
write.csv(pricestock_stationarity, paste0("/Users/thomasdemassari/Library/CloudStorage/OneDrive-Personal/Università/BSc/Tesi/Data/Results/Test di stazionarietà/",nome_file))
# I prezzi delle azioni sono non stazionari

# Test di stazionarietà sui tassi di interesse (mensili) ----
# Matrice dove salverò i risultati dei test 
intrate_monthly_stationarity = matrix(c(0,0),2,4)
colnames(intrate_monthly_stationarity) = c("kpss", "pp", "adf", "ar(1)")
rownames(intrate_monthly_stationarity) = c("Libor", "Euribor")

# Libor
libor_monthly = data_sp$InterestRate[data_sp$Bank == "hsbc"]                              # Tasso libor
plot(ts(libor_monthly, start = c(2006, 1), frequency = 12)) 
intrate_monthly_stationarity[1, 1] = as.numeric(kpss.test(libor_monthly, "Trend")[3])     # pvalue del kpss
intrate_monthly_stationarity[1, 2] = as.numeric(pp.test(libor_monthly)[4])                # pvalue del pp
intrate_monthly_stationarity[1, 3] = as.numeric(adf.test(libor_monthly)[4])               # pvalue di adf
intrate_monthly_stationarity[1, 4] = as.numeric(arima(libor_monthly, c(1,0,0))$coef[1])   # coefficiente di AR(1)
acf(libor_monthly)
# Euribor
euribor_monthly = data_sp$InterestRate[data_sp$Bank == "ucg"]                            # Tasso Euribor
plot(ts(euribor_monthly, start = c(2006, 1), frequency = 12)) 
intrate_monthly_stationarity[2, 1] = as.numeric(kpss.test(euribor_monthly, "Trend")[3])   # pvalue del kpss
intrate_monthly_stationarity[2, 2] = as.numeric(pp.test(euribor_monthly)[4])              # pvalue del pp
intrate_monthly_stationarity[2, 3] = as.numeric(adf.test(euribor_monthly)[4])             # pvalue di adf
intrate_monthly_stationarity[2, 4] = as.numeric(arima(euribor_monthly, c(1,1,0))$coef[1]) # coefficiente di AR(1)
acf(euribor_monthly)

# Risultati dei test
print(intrate_monthly_stationarity)    
nome_file = "Tassi di interesse (mensili).csv"
write.csv(intrate_stationarity, paste0("/Users/thomasdemassari/Library/CloudStorage/OneDrive-Personal/Università/BSc/Tesi/Data/Results/Test di stazionarietà/",nome_file))

# Il tasso di interesse mensile non è stazionario

# Test di stazionarietà sull'indice di mercato ----
marketindex_stationarity = matrix(c(0,0),6,4)
colnames(marketindex_stationarity) = c("kpss", "pp", "adf", "ar(1)")
rownames(marketindex_stationarity) = c("ftse100", "ftse_mib", "cac40", "ibex35", "dax", "aex")

marketindex_name = c("ftse100", "ftse_mib", "cac40", "ibex35", "dax", "aex")

ftse100 = data_sp$Market_Index[data_sp$Bank == "hsbc"]
ftse_mib = data_sp$Market_Index[data_sp$Bank == "ucg"]
cac40 = data_sp$Market_Index[data_sp$Bank == "bnp"]
ibex35 = data_sp$Market_Index[data_sp$Bank == "bbva"]
dax = data_sp$Market_Index[data_sp$Bank == "dbk"]
aex = data_sp$Market_Index[data_sp$Bank == "inga"]
df_marketindex = cbind(ftse100, ftse_mib, cac40, ibex35, dax, aex)

# Test
par(mfrow = c(3,2))
for (i in 1:6){
  m_index = df_marketindex[, i]
  marketindex_stationarity[i, 1] = as.numeric(kpss.test(m_index)[3])                           # pvalue di kpss
  marketindex_stationarity[i, 2] = as.numeric(pp.test(m_index)[4])                             # pvalue di pp
  marketindex_stationarity[i, 3] = as.numeric(adf.test(m_index)[4])                            # pvalue di adf
  marketindex_stationarity[i, 4] = as.numeric(as.numeric(arima(m_index, c(1,0,0))$coef[1]))    # coefficiente di AR(1)
  
  # plot(ts(m_index, start = c(2006,1), frequency = 12), main = paste("Index - ", marketindex_name[i]))
  acf(m_index)
}
par(mfrow = c(1,1))

# Risultati
print(marketindex_stationarity)
nome_file = "Indice di mercato.csv"
write.csv(marketindex_stationarity, paste0("/Users/thomasdemassari/Library/CloudStorage/OneDrive-Personal/Università/BSc/Tesi/Data/Results/Test di stazionarietà/",nome_file))
# L'indice di mercato è non stazionario

# Test di stazionarietà sulle differenze dei log del prezzo delle azioni ----
logdiff_pricestock_stationarity = matrix(c(0,0),14,4)
colnames(logdiff_pricestock_stationarity) = c("kpss", "pp", "adf", "ar(1)")
rownames(logdiff_pricestock_stationarity) = banks_acronym

# Test
par(mfrow = c(4,4))
for (i in 1:length(index)){
  log_sp = log(data_sp$Stockprice[data_sp$ID == i])
  diffs_log_sp = diff(log_sp)
  logdiff_pricestock_stationarity[i, 1] = as.numeric(kpss.test(diffs_log_sp)[3])                           # pvalue di kpss
  logdiff_pricestock_stationarity[i, 2] = as.numeric(pp.test(diffs_log_sp)[4])                             # pvalue di pp
  logdiff_pricestock_stationarity[i, 3] = as.numeric(adf.test(diffs_log_sp)[4])                            # pvalue di adf
  logdiff_pricestock_stationarity[i, 4] = as.numeric(as.numeric(arima(diffs_log_sp, c(1,0,0))$coef[1]))    # coefficiente di AR(1)
  
  # plot(ts(diffs_log_sp, start = c(2006,1), frequency = 12), main = paste("log stock price - ", banks_acronym[i]))
  acf(diffs_log_sp)
}
par(mfrow = c(1,1))

# Risultati
print(logdiff_pricestock_stationarity)
nome_file = "Azioni - log delle differenze prime.csv"
write.csv(logdiff_pricestock_stationarity, paste0("/Users/thomasdemassari/Library/CloudStorage/OneDrive-Personal/Università/BSc/Tesi/Data/Results/Test di stazionarietà/",nome_file))
# Sono stazionarie

# Test di stazionarietà sulle differenze dei log degli indici di mercato ----
logdiff_marketindex_stationarity = matrix(c(0,0),6,4)
colnames(logdiff_marketindex_stationarity) = c("kpss", "pp", "adf", "ar(1)")
rownames(logdiff_marketindex_stationarity) = c("ftse100", "ftse_mib", "cac40", "ibex35", "dax", "aex")

# Test
par(mfrow = c(3,2))
for (i in 1:6){
  log_m_index = log(df_marketindex[, i])
  logdiff_m_index = diff(log_m_index)
  logdiff_marketindex_stationarity[i, 1] = as.numeric(kpss.test(logdiff_m_index)[3])                           # pvalue di kpss
  logdiff_marketindex_stationarity[i, 2] = as.numeric(pp.test(logdiff_m_index)[4])                             # pvalue di pp
  logdiff_marketindex_stationarity[i, 3] = as.numeric(adf.test(logdiff_m_index)[4])                            # pvalue di adf
  logdiff_marketindex_stationarity[i, 4] = as.numeric(as.numeric(arima(logdiff_m_index, c(1,0,0))$coef[1]))    # coefficiente di AR(1)
  
  # plot(ts(logdiff_m_index, start = c(2006,1), frequency = 12), main = paste("Index - ", marketindex_name[i]))
  acf(logdiff_m_index)
}
par(mfrow = c(1,1))

# Risultati
print(logdiff_marketindex_stationarity)
nome_file = "Indice di mercato - log delle differenze prime.csv"
write.csv(logdiff_marketindex_stationarity, paste0("/Users/thomasdemassari/Library/CloudStorage/OneDrive-Personal/Università/BSc/Tesi/Data/Results/Test di stazionarietà/",nome_file))
# Sono stazionarie

# Test di stazionarietà sulle differenze dei log dei tassi di interesse (mensili) ----
# Matrice dove salverò i risultati dei test 
logdiffs_intrate_monthly_stationarity = matrix(c(0,0),2,4)
colnames(logdiffs_intrate_monthly_stationarity) = c("kpss", "pp", "adf", "ar(1)")
rownames(logdiffs_intrate_monthly_stationarity) = c("Libor", "Euribor")

# Libor 
log_libor_monthly = log(data_sp$InterestRate[data_sp$Bank == "hsbc"])                                       # Tasso libor
logdiffs_libor_monthly = diff(log_libor_monthly)
plot(ts(logdiffs_libor_monthly, start = c(2006, 1), frequency = 12)) 
logdiffs_intrate_monthly_stationarity[1, 1] = as.numeric(kpss.test(logdiffs_libor_monthly, "Trend")[3])     # pvalue del kpss
logdiffs_intrate_monthly_stationarity[1, 2] = as.numeric(pp.test(logdiffs_libor_monthly)[4])                # pvalue del pp
logdiffs_intrate_monthly_stationarity[1, 3] = as.numeric(adf.test(logdiffs_libor_monthly)[4])               # pvalue di adf
logdiffs_intrate_monthly_stationarity[1, 4] = as.numeric(arima(logdiffs_libor_monthly, c(1,0,0))$coef[1])   # coefficiente di AR(1)
acf(logdiffs_libor_monthly)

# Euribor
# Siccome l'euribor è negativo in alcune osservazioni (valore minimo - 0.58%, molto vicino a 0), e 
# siccome non posso calcolare il log di un numero negativo allora metto 0 dove euribor è negativo
# nb: metto quasi 0 se no log(0) = -inf
euribor_monthly_adjusted = NULL
for (j in 1:length(euribor_monthly)){
  if (euribor_monthly[j] <= 0){
    euribor_monthly_adjusted[j] = 0.00001
  } else{
    euribor_monthly_adjusted[j] = euribor_monthly[j]
  }
}

plot(cbind(ts(euribor_monthly_adjusted), ts(euribor_monthly)), plot.type = "single", col = c("red", "blue"))
abline(h = 0, lty = "dashed")

log_euribor_monthly =  log(euribor_monthly_adjusted)                      
logdiffs_euribor_monthly = diff(log_euribor_monthly)
plot(ts(logdiffs_euribor_monthly, start = c(2006, 1), frequency = 12)) 
logdiffs_intrate_monthly_stationarity[2, 1] = as.numeric(kpss.test(logdiffs_euribor_monthly, "Trend")[3])   # pvalue del kpss
logdiffs_intrate_monthly_stationarity[2, 2] = as.numeric(pp.test(logdiffs_euribor_monthly)[4])              # pvalue del pp
logdiffs_intrate_monthly_stationarity[2, 3] = as.numeric(adf.test(logdiffs_euribor_monthly)[4])             # pvalue di adf
logdiffs_intrate_monthly_stationarity[2, 4] = as.numeric(arima(logdiffs_euribor_monthly, c(1,1,0))$coef[1]) # coefficiente di AR(1)
acf(logdiffs_euribor_monthly)

# Risultati dei test
print(logdiffs_intrate_monthly_stationarity)
nome_file = "Tassi di interesse (mensili) - log delle differenze prime.csv"
write.csv(logdiffs_intrate_monthly_stationarity, paste0("/Users/thomasdemassari/Library/CloudStorage/OneDrive-Personal/Università/BSc/Tesi/Data/Results/Test di stazionarietà/",nome_file))
# Sono stazionari (secondo pp e adf)

# SEZIONE 4b: TEST DI COINTEGRAZIONE ----
# Test di cointegrazione sui prezzi delle azioni ----
stockprice_cointegration = matrix(c(0,0),14,1)
colnames(stockprice_cointegration) = c("Engle-Granger")
rownames(stockprice_cointegration) = banks_acronym

# Valore critico (MacKinnon da "Introduction to Modern Time Series Analysis)
cv3 = -3.74

par(mfrow = c(4,4))
for (i in 1:length(banks_acronym)){
  prices = data_sp$Stockprice[data_sp$ID == i]
  market_index = data_sp$Market_Index[data_sp$ID == i]
  
  # Definizione del tasso
  if ((data_sp$Nation[data_sp$ID == i])[1] == "uk"){
    intrate_sp = data_sp$InterestRate[data_sp$Bank == "hsbc"]
  } else{
    intrate_sp = data_sp$InterestRate[data_sp$Bank == "ucg"]
  }
  
  # Stima del modello con OLS
  model = lm(prices ~ intrate_sp + market_index)
  errors = residuals(model)
  eg = as.numeric(ur.df(errors, type = "none", selectlags = "AIC")@teststat)

  if(abs(eg) > abs(cv3)){
    result = "cointegration"
  } else{
    result = "no cointegration"
  }
  
  stockprice_cointegration[i] = paste(round(eg, 4), "-", result)

  # plot(ts(errors), main = paste(banks_acronym[i], "- stock price residuals"), ylab = "Residuals")
  # acf(errors)
}
par(mfrow = c(1,1))
nome_file = "Modello azioni ~ intrate + mkt.csv"
write.csv(stockprice_cointegration, paste0("/Users/thomasdemassari/Library/CloudStorage/OneDrive-Personal/Università/BSc/Tesi/Data/Results/Test di cointegrazione/",nome_file))

# SEZIONE 4c: STIMA DEI MODELLI ----
# Modelli OLS -----
path = "/Users/thomasdemassari/Library/CloudStorage/OneDrive-Personal/Università/BSc/Tesi/Data/Results/Stime"

# Variabili 
roa = data_fs$ROA; nii = data_fs$NII_Assets; oi = data_fs$OI_Assets; prov = data_fs$PROV_Assets
intrate = data_fs$InterestRate; dcb = data_fs$DepositCentralBank_Assets; crisis = data_fs$crisis

prezzi = data_sp$Stockprice; intrate_m = data_sp$InterestRate; mkt = data_sp$Market_Index

# Modelli con le riserve in banca centrale
lm_roa_dcb = OLSregression(dipendente = roa, regressori = cbind(intrate, dcb), dummy = cbind(crisis), 
                   names_of_models = banks_acronym, filtro = data_fs$ID)
lm_nii_dcb = OLSregression(dipendente = nii, regressori = cbind(intrate, dcb), dummy = cbind(crisis), 
                       names_of_models = banks_acronym, filtro = data_fs$ID)
lm_oi_dcb = OLSregression(dipendente = oi, regressori = cbind(intrate, dcb), dummy = cbind(crisis), 
                       names_of_models = banks_acronym, filtro = data_fs$ID)
lm_prov_dcb = OLSregression(dipendente = prov, regressori = cbind(intrate, dcb), dummy = cbind(crisis), 
                       names_of_models = banks_acronym, filtro = data_fs$ID)

# Modelli senza le riserve in banca centrale
lm_roa_semplice = OLSregression(dipendente = roa, regressori = cbind(intrate), dummy = cbind(crisis), 
                           names_of_models = banks_acronym, filtro = data_fs$ID)
lm_nii_semplice = OLSregression(dipendente = nii, regressori = cbind(intrate), dummy = cbind(crisis), 
                           names_of_models = banks_acronym, filtro = data_fs$ID)
lm_oi_semplice = OLSregression(dipendente = oi, regressori = cbind(intrate), dummy = cbind(crisis), 
                          names_of_models = banks_acronym, filtro = data_fs$ID)
lm_prov_semplice = OLSregression(dipendente = prov, regressori = cbind(intrate), dummy = cbind(crisis), 
                            names_of_models = banks_acronym, filtro = data_fs$ID)

# Modelli con il prezzo delle azioni
lm_azioni = OLSregression(dipendente = prezzi, regressori = cbind(intrate_m, mkt), 
                          names_of_models = banks_acronym, filtro = data_sp$ID, rendimenti = TRUE)

# Salvo i coefficienti e i test in un csv
write.csv(lm_roa_dcb$coef, file = paste0(path, "/OLS - ROA coeff (con dcb).csv"))
write.csv(lm_roa_dcb$test, file = paste0(path, "/OLS - ROA test (con dcb).csv"))

write.csv(lm_roa_semplice$coef, file = paste0(path, "/OLS - ROA coeff (senza dcb).csv"))
write.csv(lm_roa_semplice$test, file = paste0(path, "/OLS - ROA test (senza dcb).csv"))

write.csv(lm_nii_dcb$coef, file = paste0(path, "/OLS - NII coeff (con dcb).csv"))
write.csv(lm_nii_dcb$test, file = paste0(path, "/OLS - NII test (con dcb).csv"))

write.csv(lm_nii_semplice$coef, file = paste0(path, "/OLS - NII coeff (senza dcb).csv"))
write.csv(lm_nii_semplice$test, file = paste0(path, "/OLS - NII test (senza dcb).csv"))

write.csv(lm_oi_dcb$coef, file = paste0(path, "/OLS - OI coeff (con dcb).csv"))
write.csv(lm_oi_dcb$test, file = paste0(path, "/OLS - OI test (con dcb).csv"))

write.csv(lm_oi_semplice$coef, file = paste0(path, "/OLS - OI coeff (senza dcb).csv"))
write.csv(lm_oi_semplice$test, file = paste0(path, "/OLS - OI test (senza dcb).csv"))

write.csv(lm_prov_dcb$coef, file = paste0(path, "/OLS - PROV coeff (con dcb).csv"))
write.csv(lm_prov_dcb$test, file = paste0(path, "/OLS - PROV test (con dcb).csv"))

write.csv(lm_prov_semplice$coef, file = paste0(path, "/OLS - PROV coeff (senza dcb).csv"))
write.csv(lm_prov_semplice$test, file = paste0(path, "/OLS - PROV test (senza dcb).csv"))

write.csv(lm_azioni$coef, file = paste0(path, "/OLS - AZIONI coeff.csv"))
write.csv(lm_azioni$test, file = paste0(path, "/OLS - AZIONI test.csv"))

# Analisi degli errori e verifica se è il caso di usare SUR ----
# Matrici dove salverò i risultati
corr_error_roa_dcb = matrix(NA, 14, 14); colnames(corr_error_roa_dcb) = banks_acronym; rownames(corr_error_roa_dcb) = banks_acronym
corr_error_nii_dcb = matrix(NA, 14, 14); colnames(corr_error_nii_dcb) = banks_acronym; rownames(corr_error_nii_dcb) = banks_acronym
corr_error_oi_dcb = matrix(NA, 14, 14); colnames(corr_error_oi_dcb) = banks_acronym; rownames(corr_error_oi_dcb) = banks_acronym
corr_error_prov_dcb = matrix(NA, 14, 14); colnames(corr_error_prov_dcb) = banks_acronym; rownames(corr_error_prov_dcb) = banks_acronym

corr_error_roa_semplice = matrix(NA, 14, 14); colnames(corr_error_roa_semplice) = banks_acronym; rownames(corr_error_roa_semplice) = banks_acronym
corr_error_nii_semplice = matrix(NA, 14, 14); colnames(corr_error_nii_semplice) = banks_acronym; rownames(corr_error_nii_semplice) = banks_acronym
corr_error_oi_semplice = matrix(NA, 14, 14); colnames(corr_error_oi_semplice) = banks_acronym; rownames(corr_error_oi_semplice) = banks_acronym
corr_error_prov_semplice = matrix(NA, 14, 14); colnames(corr_error_prov_semplice) = banks_acronym; rownames(corr_error_prov_semplice) = banks_acronym

corr_error_azioni = matrix(NA, 14, 14); colnames(corr_error_azioni) = banks_acronym; rownames(corr_error_azioni) = banks_acronym

# Correlazioni
for (i in 1:14){
  for (j in 1:14){
    corr_error_roa_dcb[i, j] = cor(lm_roa_dcb$res[i, ], lm_roa_dcb$res[j, ])
    corr_error_nii_dcb[i, j] = cor(lm_nii_dcb$res[i, ], lm_nii_dcb$res[j, ])
    corr_error_oi_dcb[i, j] = cor(lm_oi_dcb$res[i, ], lm_oi_dcb$res[j, ])
    corr_error_prov_dcb[i, j] = cor(lm_prov_dcb$res[i, ], lm_prov_dcb$res[j, ])
    
    corr_error_roa_semplice[i, j] = cor(lm_roa_semplice$res[i, ], lm_roa_semplice$res[j, ])
    corr_error_nii_semplice[i, j] = cor(lm_nii_semplice$res[i, ], lm_nii_semplice$res[j, ])
    corr_error_oi_semplice[i, j] = cor(lm_oi_semplice$res[i, ], lm_oi_semplice$res[j, ])
    corr_error_prov_semplice[i, j] = cor(lm_prov_semplice$res[i, ], lm_prov_semplice$res[j, ])
    
    corr_error_azioni[i, j] = cor(lm_azioni$res[i, ], lm_azioni$res[j, ])
  }
}

# Matrice riassuntiva dei test
summary_matrix = matrix(NA, 2, 5); rownames(summary_matrix) = c("dcb", "semplice")
colnames(summary_matrix) = c("ROA", "NII", "OI", "PROV", "**Azioni**")

# Test
times = 18; number_of_equations = 14
summary_matrix[1, 1]  = surORNOTsur(corr_error_roa_dcb, times, number_of_equations)$pvalue
summary_matrix[1, 2]  = surORNOTsur(corr_error_nii_dcb, times, number_of_equations)$pvalue
summary_matrix[1, 3]  = surORNOTsur(corr_error_oi_dcb, times, number_of_equations)$pvalue
summary_matrix[1, 4]  = surORNOTsur(corr_error_prov_dcb, times, number_of_equations)$pvalue

summary_matrix[2, 1] = surORNOTsur(corr_error_roa_semplice, times, number_of_equations)$pvalue
summary_matrix[2, 2] = surORNOTsur(corr_error_nii_semplice, times, number_of_equations)$pvalue
summary_matrix[2, 3] = surORNOTsur(corr_error_oi_semplice, times, number_of_equations)$pvalue
summary_matrix[2, 4] = surORNOTsur(corr_error_prov_semplice, times, number_of_equations)$pvalue

summary_matrix[2, 5]  = surORNOTsur(corr_error_azioni, times, number_of_equations)$pvalue

# SUR ----
# Preparazione dei dataset per il SUR
sur_fs = data.frame(rep("##tmp##", 18))
sur_sp = data.frame(rep("##tmp##", 215))

for (i in 1:14){
  y1 = paste0("roa", i)
  assign(y1, data_fs$ROA[data_fs$ID == i])
  assign("roa", data_fs$ROA[data_fs$ID == i])
  sur_fs[[y1]] = get(y1)
  
  y2 = paste0("nii", i)
  assign(y2, data_fs$NII_Assets[data_fs$ID == i])
  assign("nii", data_fs$NII_Assets[data_fs$ID == i])
  sur_fs[[y2]] = get(y2)
  
  y3 = paste0("oi", i)
  assign(y3, data_fs$OI_Assets[data_fs$ID == i])
  assign("oi", data_fs$OI_Assets[data_fs$ID == i])
  sur_fs[[y3]] = get(y3)
  
  y4 = paste0("prov", i)
  assign(y4, data_fs$PROV_Assets[data_fs$ID == i])
  assign("prov", data_fs$PROV_Assets[data_fs$ID == i])
  sur_fs[[y4]] = get(y4)
  
  x1 = paste0("intrate", i)
  assign(x1, data_fs$InterestRate[data_fs$ID == i])
  assign("intrate", data_fs$InterestRate[data_fs$ID == i])
  sur_fs[[x1]] = get(x1)
  
  x2 = paste0("dcb", i)
  assign(x2, data_fs$DepositCentralBank_Assets[data_fs$ID == i])
  assign("dcb", data_fs$DepositCentralBank_Assets[data_fs$ID == i])
  sur_fs[[x2]] = get(x2)
  
  x3 = paste0("crisis", i)
  assign(x3, data_fs$crisis[data_fs$ID == i])
  assign("crisis", data_fs$crisis[data_fs$ID == i])
  sur_fs[[x3]] = get(x3)
  
  x4 = paste0("c_intrate", i)
  assign(x4, I(data_fs$InterestRate[data_fs$ID == i] * crisis))
  assign("c_intrate", I(data_fs$InterestRate[data_fs$ID == i] * crisis))
  sur_fs[[x4]] = get(x4)
  
  x5 = paste0("c_dcb", i)
  assign(x5, I(data_fs$DepositCentralBank_Assets[data_fs$ID == i] * crisis))
  assign("c_dcb", I(data_fs$DepositCentralBank_Assets[data_fs$ID == i] * crisis))
  sur_fs[[x5]] = get(x5)
  
  rendimento = paste0("returns_price", i)
  log_rendimento = log(data_sp$Stockprice[data_sp$ID == i])
  diff_log_rendimento = diff(log_rendimento)
  assign(rendimento, diff_log_rendimento)
  assign("returns_price", diff_log_rendimento)
  sur_sp[[rendimento]] = get(rendimento)
  
  intrate_monthly = paste0("intrate_m", i)
  intrate_monthly_tmp = data_sp$InterestRate[data_sp$ID == i]
  for(index_tmp in 1:length(intrate_monthly_tmp)){
    if(as.numeric(intrate_monthly_tmp[index_tmp]) <= 0){
      intrate_monthly_tmp[index_tmp] = 0.00001
    }
  }
  log_intrate = log(intrate_monthly_tmp)
  diff_log_intrate = diff(log_intrate)
  assign(intrate_monthly, diff_log_intrate)
  assign("intrate_m", diff_log_intrate)
  sur_sp[[intrate_monthly]] = get(intrate_monthly)
  
  mkt = paste0("returns_mkt", i)
  log_mkt = log(data_sp$Market_Index[data_sp$ID == i])
  diff_log_mkt = diff(log_mkt)
  assign(mkt, diff_log_mkt)
  assign("returns_mkt", diff_log_mkt)
  sur_sp[[mkt]] = get(mkt)

  # Equazioni con dcb
  roa_name_eq_dcb = paste0("roa_eq",i, "_dcb")
  roa_eq_tmp_dcb = paste0("roa", i, " ~ intrate", i, " + dcb", i, " + crisis", i, " + c_intrate", i, " + c_dcb", i)
  assign(roa_name_eq_dcb, as.formula(roa_eq_tmp_dcb))
  
  nii_name_eq_dcb = paste0("nii_eq",i, "_dcb")
  nii_eq_tmp_dcb = paste0("nii", i, " ~ intrate", i, " + dcb", i, " + crisis", i, " + c_intrate", i, " + c_dcb", i)
  assign(nii_name_eq_dcb, as.formula(nii_eq_tmp_dcb))
  
  oi_name_eq_dcb = paste0("oi_eq",i, "_dcb")
  oi_eq_tmp_dcb = paste0("oi", i, " ~ intrate", i, " + dcb", i, " + crisis", i, " + c_intrate", i, " + c_dcb", i)
  assign(oi_name_eq_dcb, as.formula(oi_eq_tmp_dcb))
  
  prov_name_eq_dcb = paste0("prov_eq",i, "_dcb")
  prov_eq_tmp_dcb = paste0("oi", i, " ~ intrate", i, " + dcb", i, " + crisis", i, " + c_intrate", i, " + c_dcb", i)
  assign(prov_name_eq_dcb, as.formula(prov_eq_tmp_dcb))
  
  # Equazioni senza dcb
  roa_name_eq_semplice = paste0("roa_eq",i, "_semplice")
  roa_eq_tmp_semplice = paste0("roa", i, " ~ intrate", i, " + crisis", i, " + c_intrate", i)
  assign(roa_name_eq_semplice, as.formula(roa_eq_tmp_semplice))
  
  nii_name_eq_semplice = paste0("nii_eq",i, "_semplice")
  nii_eq_tmp_semplice = paste0("nii", i, " ~ intrate", i, " + crisis", i, " + c_intrate", i)
  assign(nii_name_eq_semplice, as.formula(nii_eq_tmp_semplice))
  
  oi_name_eq_semplice = paste0("oi_eq",i, "_semplice")
  oi_eq_tmp_semplice = paste0("oi", i, " ~ intrate", i, " + crisis", i, " + c_intrate", i)
  assign(oi_name_eq_semplice, as.formula(oi_eq_tmp_semplice))
  
  prov_name_eq_semplice = paste0("prov_eq",i, "_semplice")
  prov_eq_tmp_semplice = paste0("oi", i, " ~ intrate", i, " + crisis", i, " + c_intrate", i)
  assign(prov_name_eq_semplice, as.formula(prov_eq_tmp_semplice))
  
  # Azioni
  azioni_name_eq = paste0("azioni_eq", i)
  azioni_eq_tmp = paste0("returns_price", i, " ~ intrate_m", i, " + returns_mkt", i)
  assign(azioni_name_eq, as.formula(azioni_eq_tmp))
}

# Stima dei SUR 
# Con dcb
roa_formulas_dcb = list(eq1 = roa_eq1_dcb, eq2 = roa_eq2_dcb, eq3 = roa_eq3_dcb, eq4 = roa_eq4_dcb, 
                    eq5 = roa_eq5_dcb, eq6 = roa_eq6_dcb, eq7 = roa_eq7_dcb, eq8 = roa_eq8_dcb, 
                    eq9 = roa_eq9_dcb, eq10 = roa_eq10_dcb, eq11 = roa_eq11_dcb, eq12 = roa_eq12_dcb, 
                    eq13 = roa_eq13_dcb, eq14 = roa_eq14_dcb)
sur_roa_dcb = systemfit(roa_formulas_dcb, data = sur_fs, method = "SUR")
summary_sur_roa_dcb = summary(sur_roa_dcb)

nii_formulas_dcb = list(eq1 = nii_eq1_dcb, eq2 = nii_eq2_dcb, eq3 = nii_eq3_dcb, eq4 = nii_eq4_dcb, 
                        eq5 = nii_eq5_dcb, eq6 = nii_eq6_dcb, eq7 = nii_eq7_dcb, eq8 = nii_eq8_dcb, 
                        eq9 = nii_eq9_dcb, eq10 = nii_eq10_dcb, eq11 = nii_eq11_dcb, 
                        eq12 = nii_eq12_dcb, eq13 = nii_eq13_dcb, eq14 = nii_eq14_dcb)
sur_nii_dcb = systemfit(nii_formulas_dcb, data = sur_fs, method = "SUR")
summary_sur_nii_dcb = summary(sur_nii_dcb)

oi_formulas_dcb = list(eq1 = oi_eq1_dcb, eq2 = oi_eq2_dcb, eq3 = oi_eq3_dcb, eq4 = oi_eq4_dcb, 
                       eq5 = oi_eq5_dcb, eq6 = oi_eq6_dcb, eq7 = oi_eq7_dcb, eq8 = oi_eq8_dcb, 
                       eq9 = oi_eq9_dcb, eq10 = oi_eq10_dcb, eq11 = oi_eq11_dcb, eq12 = oi_eq12_dcb, 
                       eq13 = oi_eq13_dcb, eq14 = oi_eq14_dcb)
sur_oi_dcb = systemfit(oi_formulas_dcb, data = sur_fs, method = "SUR")
summary_sur_oi_dcb = summary(sur_oi_dcb)

prov_formulas_dcb = list(eq1 = prov_eq1_dcb, eq2 = prov_eq2_dcb, eq3 = prov_eq3_dcb, eq4 = prov_eq4_dcb, 
                         eq5 = prov_eq5_dcb, eq6 = prov_eq6_dcb, eq7 = prov_eq7_dcb, eq8 = prov_eq8_dcb, 
                         eq9 = prov_eq9_dcb, eq10 = prov_eq10_dcb, eq11 = prov_eq11_dcb, 
                         eq12 = prov_eq12_dcb, eq13 = prov_eq13_dcb, eq14 = prov_eq14_dcb)
sur_prov_dcb = systemfit(prov_formulas_dcb, data = sur_fs, method = "SUR")
summary_sur_prov_dcb = summary(sur_prov_dcb)

# Senza dcb
roa_formulas_semplice = list(eq1 = roa_eq1_semplice, eq2 = roa_eq2_semplice, eq3 = roa_eq3_semplice, 
                             eq4 = roa_eq4_semplice, eq5 = roa_eq5_semplice, eq6 = roa_eq6_semplice, 
                             eq7 = roa_eq7_semplice, eq8 = roa_eq8_semplice, eq9 = roa_eq9_semplice, 
                             eq10 = roa_eq10_semplice, eq11 = roa_eq11_semplice, eq12 = roa_eq12_semplice,
                             eq13 = roa_eq13_semplice, eq14 = roa_eq14_semplice)
sur_roa_semplice = systemfit(roa_formulas_semplice, data = sur_fs, method = "SUR")
summary_sur_roa_semplice = summary(sur_roa_semplice)

nii_formulas_semplice = list(eq1 = nii_eq1_semplice, eq2 = nii_eq2_semplice, eq3 = nii_eq3_semplice, 
                             eq4 = nii_eq4_semplice, eq5 = nii_eq5_semplice, eq6 = nii_eq6_semplice, 
                             eq7 = nii_eq7_semplice, eq8 = nii_eq8_semplice, eq9 = nii_eq9_semplice, 
                             eq10 = nii_eq10_semplice, eq11 = nii_eq11_semplice, eq12 = nii_eq12_semplice, 
                             eq13 = nii_eq13_semplice, eq14 = nii_eq14_semplice)
sur_nii_semplice = systemfit(nii_formulas_semplice, data = sur_fs, method = "SUR")
summary_sur_nii_semplice = summary(sur_nii_semplice)

oi_formulas_semplice = list(eq1 = oi_eq1_semplice, eq2 = oi_eq2_semplice, eq3 = oi_eq3_semplice, 
                            eq4 = oi_eq4_semplice, eq5 = oi_eq5_semplice, eq6 = oi_eq6_semplice, 
                            eq7 = oi_eq7_semplice, eq8 = oi_eq8_semplice, eq9 = oi_eq9_semplice, 
                            eq10 = oi_eq10_semplice, eq11 = oi_eq11_semplice, eq12 = oi_eq12_semplice, 
                            eq13 = oi_eq13_semplice, eq14 = oi_eq14_semplice)
sur_oi_semplice = systemfit(oi_formulas_semplice, data = sur_fs, method = "SUR")
summary_sur_oi_semplice = summary(sur_oi_semplice)

prov_formulas_semplice = list(eq1 = prov_eq1_semplice, eq2 = prov_eq2_semplice, eq3 = prov_eq3_semplice, 
                              eq4 = prov_eq4_semplice, eq5 = prov_eq5_semplice, eq6 = prov_eq6_semplice, 
                              eq7 = prov_eq7_semplice, eq8 = prov_eq8_semplice, eq9 = prov_eq9_semplice, 
                              eq10 = prov_eq10_semplice, eq11 = prov_eq11_semplice, eq12 = prov_eq12_semplice, 
                              eq13 = prov_eq13_semplice, eq14 = prov_eq14_semplice)
sur_prov_semplice = systemfit(prov_formulas_semplice, data = sur_fs, method = "SUR")
summary_sur_prov_semplice = summary(sur_prov_semplice)

# Azioni
azioni_formulas = list(eq1 = azioni_eq1, eq2 = azioni_eq2, eq3 = azioni_eq3, eq4 = azioni_eq4, eq5 = azioni_eq5,
                       eq6 = azioni_eq6, eq7 = azioni_eq7, eq8 = azioni_eq8, eq9 = azioni_eq9, eq10 = azioni_eq10,
                       eq11 = azioni_eq11, eq12 = azioni_eq12, eq13 = azioni_eq13, eq14 = azioni_eq14)
sur_azioni = systemfit(azioni_formulas, data = sur_sp, method = "SUR")
summary_sur_azioni = summary(sur_azioni)




# Salvo i risultati in csv per leggere i dati più facilmente
names_cols_dcb =  c("b0","pvalue_b0", "b1 (intrate)", "pvalue_b1", "b2 (dcb)", "pvalue_b2", 
                           "b3 (crisis)", "pvalue_b3", "b4 (intrate * crisis)", "pvalue_b4", 
                           "b5 (dcb * crisis)", "pvalue_b5")

names_cols_semplice =  c("b0","pvalue_b0", "b1 (intrate)", "pvalue_b1", "b2 (crisis)", "pvalue_b2", 
                       "b3 (intrate * crisis)", "pvalue_b3")

# Matrice per i risultati
# Con dcb
sur_roa_coef_dcb = matrix(NA, 14, 12); rownames(sur_roa_coef_dcb) = banks_acronym; colnames(sur_roa_coef_dcb) = names_cols_dcb
sur_nii_coef_dcb = matrix(NA, 14, 12); rownames(sur_nii_coef_dcb) = banks_acronym; colnames(sur_nii_coef_dcb) = names_cols_dcb
sur_oi_coef_dcb = matrix(NA, 14, 12); rownames(sur_oi_coef_dcb) = banks_acronym; colnames(sur_oi_coef_dcb) = names_cols_dcb
sur_prov_coef_dcb = matrix(NA, 14, 12); rownames(sur_prov_coef_dcb) = banks_acronym; colnames(sur_prov_coef_dcb) = names_cols_dcb

# Senza dcb
sur_roa_coef_semplice = matrix(NA, 14, 8); rownames(sur_roa_coef_semplice) = banks_acronym; colnames(sur_roa_coef_semplice) = names_cols_semplice
sur_nii_coef_semplice = matrix(NA, 14, 8); rownames(sur_nii_coef_semplice) = banks_acronym; colnames(sur_nii_coef_semplice) = names_cols_semplice
sur_oi_coef_semplice = matrix(NA, 14, 8); rownames(sur_oi_coef_semplice) = banks_acronym; colnames(sur_oi_coef_semplice) = names_cols_semplice
sur_prov_coef_semplice = matrix(NA, 14, 8); rownames(sur_prov_coef_semplice) = banks_acronym; colnames(sur_prov_coef_semplice) = names_cols_semplice

# Azioni
sur_azioni_coef = matrix(NA, 14, 6); rownames(sur_azioni_coef) = banks_acronym
colnames(sur_azioni_coef) = c("b0","pvalue_b0", "b1 (intrate)", "pvalue_b1", "b2 (mkt)", "pvalue_b2")

# CSV
path = "/Users/thomasdemassari/Library/CloudStorage/OneDrive-Personal/Università/BSc/Tesi/Data/Results/Stime/"

write.csv(model2csv(sur_roa_dcb, sur_roa_coef_dcb), paste0(path, "SUR - ROA coeff (con dcb).csv"))
write.csv(model2csv(sur_nii_dcb, sur_nii_coef_dcb), paste0(path, "SUR - NII coeff (con dcb).csv"))
write.csv(model2csv(sur_oi_dcb, sur_oi_coef_dcb), paste0(path, "SUR - OI coeff (con dcb).csv"))
write.csv(model2csv(sur_prov_dcb, sur_prov_coef_dcb), paste0(path, "SUR - PROV coeff (con dcb).csv"))

write.csv(model2csv(sur_roa_semplice, sur_roa_coef_semplice), paste0(path, "SUR - ROA coeff (senza dcb).csv"))
write.csv(model2csv(sur_nii_semplice, sur_nii_coef_semplice), paste0(path, "SUR - NII coeff (senza dcb).csv"))
write.csv(model2csv(sur_oi_semplice, sur_oi_coef_semplice), paste0(path, "SUR - OI coeff (senza dcb).csv"))
write.csv(model2csv(sur_prov_semplice, sur_prov_coef_semplice), paste0(path, "SUR - PROV coeff (senza dcb).csv"))

write.csv(model2csv(sur_azioni, sur_azioni_coef), paste0(path, "SUR - AZIONI coeff.csv"))

# TEST
# Con dcb
regressori_dcb = cbind(data_fs$InterestRate, data_fs$DepositCentralBank_Assets, data_fs$crisis,
                       I(data_fs$crisis * data_fs$InterestRate), I(data_fs$crisis * data_fs$DepositCentralBank_Assets))

sur_roa_test_dcb = test_SUR(modello = sur_roa_dcb, dipendente = data_fs$ROA, regressori = regressori_dcb,
                            filtro = data_fs$ID, names_of_models = banks_acronym, number_of_coefficients = 5)
write.csv(sur_roa_test_dcb, paste0(path, "SUR - ROA test (con dcb).csv"))

sur_nii_test_dcb = test_SUR(modello = sur_nii_dcb, dipendente = data_fs$NII_Assets, regressori = regressori_dcb,
                            filtro = data_fs$ID, names_of_models = banks_acronym, number_of_coefficients = 5)
write.csv(sur_nii_test_dcb, paste0(path, "SUR - NII test (con dcb).csv"))

sur_oi_test_dcb = test_SUR(modello = sur_oi_dcb, dipendente = data_fs$OI_Assets, regressori = regressori_dcb,
                            filtro = data_fs$ID, names_of_models = banks_acronym, number_of_coefficients = 5)
write.csv(sur_oi_test_dcb, paste0(path, "SUR - OI test (con dcb).csv"))

sur_prov_test_dcb = test_SUR(modello = sur_prov_dcb, dipendente = data_fs$PROV_Assets, regressori = regressori_dcb,
                            filtro = data_fs$ID, names_of_models = banks_acronym, number_of_coefficients = 5)
write.csv(sur_prov_test_dcb, paste0(path, "SUR - PROV test (con dcb).csv"))


# Senza dcb
regressori_semplice = cbind(data_fs$InterestRate, data_fs$crisis, I(data_fs$crisis * data_fs$InterestRate))

sur_roa_test_semplice = test_SUR(modello = sur_roa_semplice, dipendente = data_fs$ROA, regressori = regressori_semplice,
                            filtro = data_fs$ID, names_of_models = banks_acronym, number_of_coefficients = 3)
write.csv(sur_roa_test_semplice, paste0(path, "SUR - ROA test (senza dcb).csv"))

sur_nii_test_semplice = test_SUR(modello = sur_nii_semplice, dipendente = data_fs$NII_Assets, regressori = regressori_semplice,
                            filtro = data_fs$ID, names_of_models = banks_acronym, number_of_coefficients = 3)
write.csv(sur_nii_test_semplice, paste0(path, "SUR - NII test (senza dcb).csv"))

sur_oi_test_semplice = test_SUR(modello = sur_oi_semplice, dipendente = data_fs$OI_Assets, regressori = regressori_semplice,
                           filtro = data_fs$ID, names_of_models = banks_acronym, number_of_coefficients = 3)
write.csv(sur_oi_test_semplice, paste0(path, "SUR - OI test (senza dcb).csv"))

sur_prov_test_semplice = test_SUR(modello = sur_prov_semplice, dipendente = data_fs$PROV_Assets, regressori = regressori_semplice,
                             filtro = data_fs$ID, names_of_models = banks_acronym, number_of_coefficients = 3)
write.csv(sur_prov_test_semplice, paste0(path, "SUR - PROV test (senza dcb).csv"))


# Azioni
regressori_azioni = cbind(data_sp$InterestRate, data_sp$Market_Index)

sur_azioni_test = test_SUR(modello = sur_azioni, dipendente = data_sp$Stockprice, regressori = regressori_azioni,
                           filtro = data_sp$ID, names_of_models = banks_acronym, number_of_coefficients = 2, rendimenti = TRUE)
write.csv(sur_azioni_test, paste0(path, "SUR - AZIONI test.csv"))

          

# Analisi dei residui - QQ Plot 
variabile_dipendente = "roa"
par(mfrow = c(4,4))
for (i in 1:14){
  if(variabile_dipendente == "roa"){
    qqnorm(summary_sur_roa$eq[[i]]$residuals, main = paste("QQ Plot - ROA di", banks_acronym[i]), xlab = "Quantili teorici", ylab = "Quantili osservati")
    qqline(summary_sur_roa$eq[[i]]$residuals, col = "black")
  }
  
  if(variabile_dipendente == "nii"){
    qqnorm(summary_sur_nii$eq[[i]]$residuals, main = paste("QQ Plot - NII di", banks_acronym[i]), xlab = "Quantili teorici", ylab = "Quantili osservati")
    qqline(summary_sur_nii$eq[[i]]$residuals, col = "black")
  }
  
  if(variabile_dipendente == "oi"){
    qqnorm(summary_sur_oi$eq[[i]]$residuals, main = paste("QQ Plot - OI di", banks_acronym[i]), xlab = "Quantili teorici", ylab = "Quantili osservati")
    qqline(summary_sur_oi$eq[[i]]$residuals, col = "black")
  }
  
  if(variabile_dipendente == "prov"){
    qqnorm(summary_sur_prov$eq[[i]]$residuals, main = paste("QQ Plot - PROV di", banks_acronym[i]), xlab = "Quantili teorici", ylab = "Quantili osservati")
    qqline(summary_sur_prov$eq[[i]]$residuals, col = "black")
  }
  
  if(variabile_dipendente == "azioni"){
    qqnorm(summary_sur_azioni$eq[[i]]$residuals, main = paste("QQ Plot - AZIONI di", banks_acronym[i]), xlab = "Quantili teorici", ylab = "Quantili osservati")
    qqline(summary_sur_azioni$eq[[i]]$residuals, col = "black")
  }
  
}
par(mfrow = c(1,1))


# SEZIONE 5: GRAFICI ----
# Euribor and Libor ----
plot(cbind(ts(euribor_monthly, start = c(2006,1), frequency = 12), ts(libor_monthly, start = c(2006,1), frequency = 12)), 
     plot.type = "single", col = c("black", "grey"), type = "l", xlab = "Tempo", ylab = "Tasso di interesse (%)")
abline(h = 0, lty = "dashed")
legend("topright", legend = c("Euribor", "Libor"),col = c("black", "grey"), lty = 1)
axis(1, at = c(2006:2023), las = 1)

# Andamento delle componenti di bilancio ----
roa_median = NULL; roa_q05per = NULL; roa_q25per = NULL; roa_q75per = NULL; roa_q95per = NULL 
nii_median = NULL; nii_q05per = NULL; nii_q25per = NULL; nii_q75per = NULL; nii_q95per = NULL
oi_median = NULL; oi_q05per = NULL; oi_q25per = NULL; oi_q75per = NULL; oi_q95per = NULL 
prov_median = NULL; prov_q05per = NULL; prov_q25per = NULL; prov_q75per = NULL; prov_q95per = NULL 
years = c(2006:2023)

for (i in 1:length(years)){
  # ROA
  roa_tmp = data_fs$ROA[data_fs$Years == years[i]]
  roa_median[i] = median(roa_tmp)
  roa_q05per[i] = as.numeric(quantile(roa_tmp, probs = c(0.05)))
  roa_q25per[i] = as.numeric(quantile(roa_tmp, probs = c(0.25)))
  roa_q75per[i] = as.numeric(quantile(roa_tmp, probs = c(0.75)))
  roa_q95per[i] = as.numeric(quantile(roa_tmp, probs = c(0.95)))
  # NII
  nii_tmp = data_fs$NII_Assets[data_fs$Years == years[i]]
  nii_median[i] = median(nii_tmp)
  nii_q05per[i] = as.numeric(quantile(nii_tmp, probs = c(0.05)))
  nii_q25per[i] = as.numeric(quantile(nii_tmp, probs = c(0.25)))
  nii_q75per[i] = as.numeric(quantile(nii_tmp, probs = c(0.75)))
  nii_q95per[i] = as.numeric(quantile(nii_tmp, probs = c(0.95)))
  # OI
  oi_tmp = data_fs$OI_Assets[data_fs$Years == years[i]]
  oi_median[i] = median(oi_tmp)
  oi_q05per[i] = as.numeric(quantile(oi_tmp, probs = c(0.05)))
  oi_q25per[i] = as.numeric(quantile(oi_tmp, probs = c(0.25)))
  oi_q75per[i] = as.numeric(quantile(oi_tmp, probs = c(0.75)))
  oi_q95per[i] = as.numeric(quantile(oi_tmp, probs = c(0.95)))
  # PROV
  prov_tmp = data_fs$PROV_Assets[data_fs$Years == years[i]]
  prov_median[i] = median(prov_tmp)
  prov_q05per[i] = as.numeric(quantile(prov_tmp, probs = c(0.05)))
  prov_q25per[i] = as.numeric(quantile(prov_tmp, probs = c(0.25)))
  prov_q75per[i] = as.numeric(quantile(prov_tmp, probs = c(0.75)))
  prov_q95per[i] = as.numeric(quantile(prov_tmp, probs = c(0.95)))
}
par(mfrow = c(2,2))
# ROA
plot(ts(roa_median, start = 2006), ylim = c(min(roa_q05per), max(roa_q95per)), 
     main = "Andamento del ROA", ylab = "ROA (%)", xlab = "Anno")
polygon(c(years, rev(years)), c(roa_q05per, rev(roa_q95per)), col = "gray90", border = NA)
polygon(c(years, rev(years)), c(roa_q25per, rev(roa_q75per)), col = "gray80", border = NA)
lines(years, roa_median, col = "black", lwd = 2)
axis(1, at = c(2006:2023), las = 1)
# Aggiungi una legenda
legend("bottomright",  legend = c("Mediana", "Area 25°-75° percentile", "Area 5°-95° percentile"),  
       col = c("black", "gray80", "gray90"),  lty = c(1, NA, NA), lwd = c(2,NA,NA),
       fill = c(NA, "gray80", "gray90"), border = NA, cex = 0.8, bty = "n")
# NII
plot(ts(nii_median, start = 2006), ylim = c(min(nii_q05per), max(nii_q95per)), 
     main = "Andamento del NII", ylab = "NII(t)/Assets(t-1) (%)", xlab = "Anno")
polygon(c(years, rev(years)), c(nii_q05per, rev(nii_q95per)), col = "gray90", border = NA)
polygon(c(years, rev(years)), c(nii_q25per, rev(nii_q75per)), col = "gray80", border = NA)
lines(years, nii_median, col = "black", lwd = 2)
axis(1, at = c(2006:2023), las = 1)
# Aggiungi una legenda
legend("topright",  legend = c("Mediana", "Area 25°-75° percentile", "Area 5°-95° percentile"),  
       col = c("black", "gray80", "gray90"),  lty = c(1, NA, NA), lwd = c(2,NA,NA),
       fill = c(NA, "gray80", "gray90"), border = NA, cex = 0.8, bty = "n")
# OI
plot(ts(oi_median, start = 2006), ylim = c(min(oi_q05per), max(oi_q95per)), 
     main = "Andamento del OI", ylab = "OI(t)/Assets(t-1) (%)", xlab = "Anno")
polygon(c(years, rev(years)), c(oi_q05per, rev(oi_q95per)), col = "gray90", border = NA)
polygon(c(years, rev(years)), c(oi_q25per, rev(oi_q75per)), col = "gray80", border = NA)
lines(years, oi_median, col = "black", lwd = 2)
axis(1, at = c(2006:2023), las = 1)
# Aggiungi una legenda
legend("topright",  legend = c("Mediana", "Area 25°-75° percentile", "Area 5°-95° percentile"),  
       col = c("black", "gray80", "gray90"),  lty = c(1, NA, NA), lwd = c(2,NA,NA), 
       fill = c(NA, "gray80", "gray90"), border = NA, cex = 0.8, bty = "n")
# PROV
plot(ts(prov_median, start = 2006), ylim = c(min(prov_q05per), max(prov_q95per)), 
     main = "Andamento del PROV", ylab = "PROV(t)/Assets(t-1) (%)", xlab = "Anno")
polygon(c(years, rev(years)), c(prov_q05per, rev(prov_q95per)), col = "gray90", border = NA)
polygon(c(years, rev(years)), c(prov_q25per, rev(prov_q75per)), col = "gray80", border = NA)
lines(years, prov_median, col = "black", lwd = 2)
axis(1, at = c(2006:2023), las = 1)
# Aggiungi una legenda
legend("topright",  legend = c("Mediana", "Area 25°-75° percentile", "Area 5°-95° percentile"),  
       col = c("black", "gray80", "gray90"),  lty = c(1, NA, NA), lwd = c(2,NA,NA), 
       fill = c(NA, "gray80", "gray90"), border = NA, cex = 0.8, bty = "n")
par(mfrow = c(1,1))

# Istogramma con i s.e. con stime OLS e HC3 per il ROA -----
sds_list = list("ROA" = lm_roa$sds, "NII" = lm_nii$sds, "OI" = lm_oi$sds, 
                  "PROV" = lm_prov$sds, "AZIONI" = lm_azioni$sds)
names_list = c("ROA", "NII", "OI", "PROV", "AZIONI")
for (i in 1:5){
  if (names_list[i] == "ROA"){
    sds = sds_list$ROA
  } else{
    if (names_list[i] == "NII"){
      sds = sds_list$NII
    } else{
      if (names_list[i] == "OI"){
        sds = sds_list$OI
      } else{
        if (names_list[i] == "PROV"){
          sds = sds_list$PROV
        } else{
          if (names_list[i] == "AZIONI"){
            sds = sds_list$AZIONI
          }
        }
      }
    }
  }
  
  png(file = paste0("/Users/thomasdemassari/Library/CloudStorage/OneDrive-Personal/Università/BSc/Tesi/Data/Results/Grafici/sds_", names_list[i], ".png"),
      width=1550, height=850)
  
  {
    par(mfrow = c(3,2))
    
    if (names_list[i] != "AZIONI"){
      matrix_b0 = rbind(sds[, "s.e. OLS - b0"], sds[, "s.e. HC3 - b0"])
      barplot(matrix_b0, beside = TRUE, col = c("black", "grey"), legend.text = c("s.e. OLS", "s.e. HC3"),
              args.legend = list(x = "topleft", bty = "n"), 
              main = paste("Standard error dell'intercetta del modello con", names_list[i]))
      
      matrix_b1 = rbind(sds[, "s.e. OLS - intrate"], sds[, "s.e. HC3 - intrate"])
      barplot(matrix_b1, beside = TRUE, col = c("black", "grey"), legend.text = c("s.e. OLS", "s.e. HC3"),
              args.legend = list(x = "topleft", bty = "n"), 
              main = paste("Standard error di b_intrate del modello con", names_list[i]))
      
      matrix_b2 = rbind(sds[, "s.e. OLS - dcb"], sds[, "s.e. HC3 - dcb"])
      barplot(matrix_b2, beside = TRUE, col = c("black", "grey"), legend.text = c("s.e. OLS", "s.e. HC3"),
              args.legend = list(x = "topleft", bty = "n"), 
              main = paste("Standard error di b_dcb del modello con", names_list[i]))
      
      matrix_b3 = rbind(sds[, "s.e. OLS - crisis"], sds[, "s.e. HC3 - crisis"])
      barplot(matrix_b3, beside = TRUE, col = c("black", "grey"), legend.text = c("s.e. OLS", "s.e. HC3"),
              args.legend = list(x = "topleft", bty = "n"), 
              main = paste("Standard error di b_crisis del modello con", names_list[i]))
      
      matrix_b4 = rbind(sds[, "s.e. OLS - c*int"], sds[, "s.e. HC3 - c*int"])
      barplot(matrix_b4, beside = TRUE, col = c("black", "grey"), legend.text = c("s.e. OLS", "s.e. HC3"),
              args.legend = list(x = "topleft", bty = "n"), 
              main = paste("Standard error di b_c*int del modello con", names_list[i]))
      
      matrix_b5 = rbind(sds[, "s.e. OLS - c*dcb"], sds[, "s.e. HC3 - c*dcb"])
      barplot(matrix_b5, beside = TRUE, col = c("black", "grey"), legend.text = c("s.e. OLS", "s.e. HC3"),
              args.legend = list(x = "topleft", bty = "n"), 
              main = paste("Standard error di b_c*dcb del modello con", names_list[i]))
    } else{
      matrix_b0 = rbind(sds[, "s.e. OLS - b0"], sds[, "s.e. HC3 - b0"])
      barplot(matrix_b0, beside = TRUE, col = c("black", "grey"), legend.text = c("s.e. OLS", "s.e. HC3"),
              args.legend = list(x = "topleft", bty = "n"), 
              main = paste("Standard error dell'intercetta del modello con", names_list[i]))
      
      matrix_b1 = rbind(sds[, "s.e. OLS - intrate"], sds[, "s.e. HC3 - intrate"])
      barplot(matrix_b1, beside = TRUE, col = c("black", "grey"), legend.text = c("s.e. OLS", "s.e. HC3"),
              args.legend = list(x = "topleft", bty = "n"), 
              main = paste("Standard error di b_intrate del modello con", names_list[i]))
      
      matrix_b2 = rbind(sds[, "s.e. OLS - mkt"], sds[, "s.e. HC3 - mkt"])
      barplot(matrix_b2, beside = TRUE, col = c("black", "grey"), legend.text = c("s.e. OLS", "s.e. HC3"),
              args.legend = list(x = "topleft", bty = "n"), 
              main = paste("Standard error di b_mkt del modello con", names_list[i]))
    }
  }
  dev.off()
  par(mfrow = c(1,1))
}

# Rappresentazione grafica del teorema dell'immunizzazione ----
x = c(1:20)
y1 = ts((0.15*x^3 + 50), start = 1, frequency = 1)
y2 = ts((0.25*x^3 + 10), start = 1, frequency = 1)
y3 = ts((0.05*x^3 + 90), start = 1, frequency = 1)

plot(cbind(y1,y2,y3), plot.type = "single",type = "l", col = c("black", "grey", "black"), lwd = 2,
     xlab = "Tempo", ylab = "Montante", ylim = c(0, 600), xlim = c(2,18), lty = c("solid", "solid", "dashed"), axes = FALSE)
box() # Rimetto la cornice che prima ho tolto
legend("topleft", legend = c("Tasso di interesse stabile", "Aumento del tasso di interesse", "Diminuzione del tasso di interesse"), 
       col = c("black", "grey", "black"), lty = c("solid", "solid", "dashed"))

segments(7.37, -18, 7.37, 110, lty = "dashed", col = "black") # Segmento Duration
points(7.37, 110, pch = 19, col = "black") # Etichetta Duration
axis(1, at = 7.37, labels = "Duration", pos = -20) # Punto incontro delle tre curve
# ECB - Corridoio dei tassi ----
plot(cbind(ecb_depositrate_m_9923, ecb_overnightrate_m_9923, euribor_m_9923, ecb_mro_m_9923), plot.type = "single",
     xlab = "Anni", ylab = "Tasso di interesse (%)", col = c("black", "grey", "black", "grey"), lty = c("solid", "solid", "dashed", "dotdash"), lwd = 2)
abline(h = 0, col = "black", lty = "dotted")
legend("bottomleft", legend = c("Tasso sui depositi", "Tasso di rifinanziamento margianle", "Euribor", "Tasso ufficiale di rifinanziamento"), 
       col = c("black", "grey", "black", "grey"), lty = c("solid", "solid", "dashed", "dotdash"), lwd = 2)
axis(1, at = c(1999:2023), las = 1)
# Analisi dell'andamento delle riserve in ECB e Euribor ----
plot(ecb_deposit, plot.type = "single", ylab = "Depositi presso la ECB (Milioni di euro)", xlab = "Anni", lwd = 2, xaxt = "n")
par(new = TRUE)
plot(euribor_a_9923, type = "l", col = "grey", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd = 2)
axis(1, at = c(1999:2023), las = 2)
mtext("Euribor (%)", side = 4, line = 2.5)
legend("top", legend = c("Depositi presso la ECB", "Euribor"), col = c("black", "grey"), lty = 1, xjust = 0.5, yjust = 1.5, bty = "n")
abline(h = 0, lty = "dashed")
# Esempio con omo e eteroschedasticità -----
x = runif(50, 2, 10)
y_omo = 10*x + 2*rnorm(50)
y_et = 7*x + rnorm(50)*(x)

plot(x, y_omo, xlab = "x", ylab = "y")
abline(lm(y_omo ~ x))
plot(x, y_et, xlab = "x", ylab = "y")
abline(lm(y_et ~ x))

# Andamento del ROA delle 14 banche ----
roa_data = matrix(NA, 18, 14)
colnames(roa_data) = banks_acronym
rownames(roa_data) = 2006:2023

par(mfrow = c(4,4))
for (i in 1:14){
  roa_data[, i] = data_fs$ROA[data_fs$ID == i]
  plot(ts(data_fs$ROA[data_fs$ID == i], start = 2006), 
       main = paste("Andamento del ROA di", banks_acronym[i]))
  
}
par(mfrow = c(1,1))



# Andamento dei rendimenti di HSCB e Lloyds ----
return_hsbc = ts((returns(data_sp$Stockprice[data_sp$Bank == "hsbc"])[-1])*100, start = c(2006, 2), frequency = 12)
return_lloy = ts((returns(data_sp$Stockprice[data_sp$Bank == "lloy"])[-1])*100, start = c(2006, 2), frequency = 12)


sd_hscb = sd((returns(data_sp$Stockprice[data_sp$Bank == "hsbc"])[-1])*100)
sd_lloy = sd((returns(data_sp$Stockprice[data_sp$Bank == "lloy"])[-1])*100)

par(mfrow = c(1,2))
hist(return_hsbc, main = "Distribuzione dei rendimenti di HSBC", xlab = "Rendimenti (%)", ylab = "Frequenza")
text(x = min(return_hsbc)+4, y = max(hist(return_hsbc, plot = FALSE)$counts)-2, 
     labels = paste0("Mediana = ", round(median(return_hsbc), 3), "%"), pos = 3, cex = 1, col = "black")
text(x = min(return_hsbc)+4, y = max(hist(return_hsbc, plot = FALSE)$counts)-4, 
     labels = paste0("Media = ", round(mean(return_hsbc), 3), "%"), pos = 3, cex = 1, col = "black")
text(x = min(return_hsbc)+4, y = max(hist(return_hsbc, plot = FALSE)$counts)-6, 
     labels = paste0("s.e. = ", round(sd_hscb, 3), "%"), pos = 3, cex = 1, col = "black")


hist(return_lloy, main = "Distribuzione dei rendimenti di Lloyds", xlab = "Rendimenti (%)", ylab = "Frequenza")
text(x = min(return_lloy)+4, y = max(hist(return_lloy, plot = FALSE)$counts)-2, 
     labels = paste0("Mediana = ", round(median(return_lloy), 3), "%"), pos = 3, cex = 1, col = "black")
text(x = min(return_lloy)+4, y = max(hist(return_lloy, plot = FALSE)$counts)-5, 
     labels = paste0("Media = ", round(mean(return_lloy), 3), "%"), pos = 3, cex = 1, col = "black")
text(x = min(return_lloy)+4, y = max(hist(return_lloy, plot = FALSE)$counts)-8, 
     labels = paste0("s.e. = ", round(sd_lloy, 3), "%"), pos = 3, cex = 1, col = "black")
par(mfrow = c(1,1))

plot(cbind(return_hsbc, return_lloy), plot.type = "single", col = c("black", "grey"))

summary(return_hsbc)
summary(return_lloy)


# QQ plot dei residui ----
par(mfrow = c(4,4))
for (i in 1:14){
  res = lm_roa$res[i, ]
  # res = lm_nii$res[i, ]
  # res = lm_oi$res[i, ]
  # res = lm_prov$res[i, ]
  # res = lm_azioni$res[i, ]
  qqnorm(res, main = paste("QQ Plot - ROA di", banks_acronym[i]), xlab = "Quantili teorici", ylab = "Quantili osservati")
  qqline(res, col = "black")
}
par(mfrow = c(1,1))

# Distribuzione dei residui ----
par(mfrow = c(4,4))
for (i in 1:14){
  # res = lm_roa$res[i, ]
  # res = lm_nii$res[i, ]
  # res = lm_oi$res[i, ]
  # res = lm_prov$res[i, ]
  res = lm_azioni$res[i, ]
  hist(res, main = paste("Residui - Rendimenti di", banks_acronym[i]), xlab = "Residui")
}
par(mfrow = c(1,1))

# SEZIONE 6: SIMULAZIONI MC ----
# Verifica della potenza del test di stazionarietà in piccoli campioni ----
# Simulazione:
{
  last_time = Sys.time()  # Tempo a inizio ciclo
  time_elapsed = 0
  intervallo = 15 # secondi che passano tra una stampa e l'altra per sapere dove sono nel ciclo
  
  set.seed(226091)
  obs = 18       # Numero di osservazioni
  reps = 1000000 # Numero di simulazioni
  
  # Matrici in cui salverò i pvalue dei test
  results_02 = matrix(c(0,0), reps, 3)
  results_05 = matrix(c(0,0), reps, 3)
  results_08 = matrix(c(0,0), reps, 3)
  results_095 = matrix(c(0,0), reps, 3)
  results_1 = matrix(c(0,0), reps, 3)
  colnames(results_02) = c("kpss - phi=0.2", "pp - phi=0.2", "adf - phi=0.2")
  colnames(results_05) = c("kpss - phi=0.5", "pp - phi=0.5", "adf - phi=0.5")
  colnames(results_08) = c("kpss - phi=0.8", "pp - phi=0.8", "adf - phi=0.8")
  colnames(results_095) = c("kpss - phi=0.95", "pp - phi=0.95", "adf - phi=0.95")
  colnames(results_1) = c("kpss - phi=01", "pp - phi=1", "adf - phi=1")
  rownames(results_02) = c(1:reps)
  rownames(results_05) = c(1:reps)
  rownames(results_08) = c(1:reps)
  rownames(results_095) = c(1:reps)
  rownames(results_1) = c(1:reps)
  
  rifiuti_02 = matrix(c(0,0), reps, 3)
  rifiuti_05 = matrix(c(0,0), reps, 3)
  rifiuti_08 = matrix(c(0,0), reps, 3)
  rifiuti_095 = matrix(c(0,0), reps, 3)
  rifiuti_1 = matrix(c(0,0), reps, 3)
  colnames(rifiuti_02) = c("kpss - phi=0.2", "pp - phi=0.2", "adf - phi=0.2")
  colnames(rifiuti_05) = c("kpss - phi=0.5", "pp - phi=0.5", "adf - phi=0.5")
  colnames(rifiuti_08) = c("kpss - phi=0.8", "pp - phi=0.8", "adf - phi=0.8")
  colnames(rifiuti_095) = c("kpss - phi=0.95", "pp - phi=0.95", "adf - phi=0.95")
  colnames(rifiuti_1) = c("kpss - phi=01", "pp - phi=1", "adf - phi=1")
  rownames(rifiuti_02) = c(1:reps)
  rownames(rifiuti_05) = c(1:reps)
  rownames(rifiuti_08) = c(1:reps)
  rownames(rifiuti_095) = c(1:reps)
  rownames(rifiuti_1) = c(1:reps)
  
  # Livello di significatività 
  alpha = 0.05
  
  for (i in 1:reps){
    # Simulazioni dei processi stazionari
    ar_02 = arima.sim(list(order = c(1,0,0), ar = 0.2), n = obs)
    ar_05 = arima.sim(list(order = c(1,0,0), ar = 0.5), n = obs)
    ar_08 = arima.sim(list(order = c(1,0,0), ar = 0.8), n = obs)
    ar_095 = arima.sim(list(order = c(1,0,0), ar = 0.95), n = obs)
    # Simulazioni di un processo non stazionario
    rw = 5 + cumsum(rnorm(obs, mean = 0, sd = 1))
    
    # Stima dei test
    results_02[i, 1] = as.numeric(kpss.test(ar_02)[3])                             # pvalue di kpss
    results_02[i, 2] = as.numeric(pp.test(ar_02)[4])                               # pvalue di pp
    results_02[i, 3] = as.numeric(adf.test(ar_02)[4])                              # pvalue di adf
    
    # Verifica dei p-value
    # Se è minore di 0.05 allora aggiunte alla matrice rifiuti 1, altrimenti lascia 0
    j = 0
    for (j in 1:3){
      if(results_02[i, j] < 0.05){
        rifiuti_02[i, j] = 1
      }
    }
    
    # Stima dei test
    results_05[i, 1] = as.numeric(kpss.test(ar_05)[3])                             # pvalue di kpss
    results_05[i, 2] = as.numeric(pp.test(ar_05)[4])                               # pvalue di pp
    results_05[i, 3] = as.numeric(adf.test(ar_05)[4])                              # pvalue di adf
    
    # Verifica dei p-value
    # Se è minore di 0.05 allora aggiunte alla matrice rifiuti 1, altrimenti lascia 0
    j = 0
    for (j in 1:3){
      if(results_05[i, j] < 0.05){
        rifiuti_05[i, j] = 1
      }
    }
    
    # Stima dei test
    results_08[i, 1] = as.numeric(kpss.test(ar_08)[3])                             # pvalue di kpss
    results_08[i, 2] = as.numeric(pp.test(ar_08)[4])                               # pvalue di pp
    results_08[i, 3] = as.numeric(adf.test(ar_08)[4])                              # pvalue di adf
    
    # Verifica dei p-value
    # Se è minore di 0.05 allora aggiunte alla matrice rifiuti 1, altrimenti lascia 0
    j = 0
    for (j in 1:3){
      if(results_08[i, j] < 0.05){
        rifiuti_08[i, j] = 1
      }
    }
    
    # Stima dei test
    results_095[i, 1] = as.numeric(kpss.test(ar_095)[3])                           # pvalue di kpss
    results_095[i, 2] = as.numeric(pp.test(ar_095)[4])                             # pvalue di pp
    results_095[i, 3] = as.numeric(adf.test(ar_095)[4])                            # pvalue di adf
    
    # Verifica dei p-value
    # Se è minore di 0.05 allora aggiunte alla matrice rifiuti 1, altrimenti lascia 0
    j = 0
    for (j in 1:3){
      if(results_095[i, j] < 0.05){
        rifiuti_095[i, j] = 1
      }
    }
    
    # Stima dei test
    results_1[i, 1] = as.numeric(kpss.test(rw)[3])                                 # pvalue di kpss
    results_1[i, 2] = as.numeric(pp.test(rw)[4])                                   # pvalue di pp
    results_1[i, 3] = as.numeric(adf.test(rw)[4])                                  # pvalue di adf
    
    # Verifica dei p-value
    # Se è minore di 0.05 allora aggiunte alla matrice rifiuti 1, altrimenti lascia 0
    j = 0
    for (j in 1:3){
      if(results_1[i, j] < 0.05){
        rifiuti_1[i, j] = 1
      }
    }
    
    # Aggiungo delle istruzioni per sapere a che punto sono nel ciclo visto che probabilmente 
    # ci metterà un pò a simulare su grandi campioni
    # Tempo trascorso
    current_time = Sys.time()  
    time_elapsed = as.numeric(difftime(current_time, last_time, units = "secs")) 
    
    # Se è passato più di 10 secondi dall'ultima stampa, stampiamo
    if (time_elapsed >= intervallo) {
      # Stampa il messaggio e aggiorna l'ultimo tempo di stampa
      cat("Sono passati", round(as.numeric(difftime(current_time, last_time, units = "secs")), 1), "secondi e sono alla simulazione numero:", i, "\n")
      last_time = current_time  # Aggiorna l'ultimo tempo di stampa
    }
  }
  
  # Percentuale di rifiuti
  # Matrice riassuntiva
  powers = matrix(c(0,0), 5, 3)
  colnames(powers) = c("kpss", "pp", "adf")
  rownames(powers) = c("phi = 0.2", "phi = 0.5", "phi = 0.8", "phi = 0.95", "phi = 1")
  # AR(1) con phi = 0.2
  powers[1,1] = (sum(rifiuti_02[, 1]))/reps
  powers[1,2] = sum(rifiuti_02[, 2])/reps
  powers[1,3] = sum(rifiuti_02[, 3])/reps
  # AR(1) con phi = 0.5
  powers[2,1] = (sum(rifiuti_05[, 1]))/reps
  powers[2,2] = sum(rifiuti_05[, 2])/reps
  powers[2,3] = sum(rifiuti_05[, 3])/reps
  # AR(1) con phi = 0,8
  powers[3,1] = (sum(rifiuti_08[, 1]))/reps
  powers[3,2] = sum(rifiuti_08[, 2])/reps
  powers[3,3] = sum(rifiuti_08[, 3])/reps
  # AR(1) con phi = 0.95
  powers[4,1] = (sum(rifiuti_095[, 1]))/reps
  powers[4,2] = sum(rifiuti_095[, 2])/reps
  powers[4,3] = sum(rifiuti_095[, 3])/reps
  # Random walk
  powers[5,1] = (sum(rifiuti_1[, 1]))/reps
  powers[5,2] = sum(rifiuti_1[, 2])/reps
  powers[5,3] = sum(rifiuti_1[, 3])/reps
  # Riassunto
  print(powers)
  # Rappresentazione grafica
  par(mfrow = c(2,2))
  # KPSS
  plot(ts(powers[, "kpss"], start = 1), type = "l", xaxt = "n", xlab = "", main = "Rifiuti di KPSS", 
       ylab = "Percentuale di rifiuto")
  axis(1, at = c(1:5), labels = c("phi = 0.2", "phi = 0.5", "phi = 0.8", "phi = 0.95", "phi = 1"), las = 2)
  points(1:5, powers[, "kpss"], pch = 1, col = "black", cex = 1)
  # PP
  plot(ts(powers[, "pp"], start = 1), type = "l", xaxt = "n", xlab = "", main = "Rifiuti di PP",
       ylab = "Percentuale di rifiuto")
  axis(1, at = c(1:5), labels = c("phi = 0.2", "phi = 0.5", "phi = 0.8", "phi = 0.95", "phi = 1"), las = 2)
  points(1:5, powers[, "pp"], pch = 1, col = "black", cex = 1)
  # ADF
  plot(ts(powers[, "adf"], start = 1), type = "l", xaxt = "n", xlab = "", main = "Rifiuti di ADF",
       ylab = "Percentuale di rifiuto")
  axis(1, at = c(1:5), labels = c("phi = 0.2", "phi = 0.5", "phi = 0.8", "phi = 0.95", "phi = 1"), las = 2)
  points(1:5, powers[, "adf"], pch = 1, col = "black", cex = 1)
  par(mfrow = c(1,1))
}

write.csv(powers, file = "Results/Simulazione sui test di (non) stazionarietà.csv")

# Plot dell'ultima simulazione
par(mfrow = c(3,2))
plot(ar_02, type = "l", main = "AR(1) - phi = 0.2")
plot(ar_05, type = "l", main = "AR(1) - phi = 0.5")
plot(ar_08, type = "l", main = "AR(1) - phi = 0.8")
plot(ar_095, type = "l", main = "AR(1) - phi = 0.95")
plot(rw, type = "l", main = "Random walk")
par(mfrow = c(1,1))

# Rappresentazione grafica
sim_result = read.csv("Results/Simulazioni/Simulazione sui test di (non) stazionarietà.csv", header = TRUE)

# Gracici
par(mfrow = c(1,3))
# KPSS
plot(ts(sim_result[, "kpss"], start = 1), type = "l", xaxt = "n", xlab = "", main = "Rifiuti di KPSS", 
     ylab = "Percentuale di rifiuto")
axis(1, at = c(1:5), labels = c("phi = 0.2", "phi = 0.5", "phi = 0.8", "phi = 0.95", "phi = 1"), las = 2)
points(1:5, sim_result[, "kpss"], pch = 1, col = "black", cex = 1)
# PP
plot(ts(sim_result[, "pp"], start = 1), type = "l", xaxt = "n", xlab = "", main = "Rifiuti di PP",
     ylab = "Percentuale di rifiuto")
axis(1, at = c(1:5), labels = c("phi = 0.2", "phi = 0.5", "phi = 0.8", "phi = 0.95", "phi = 1"), las = 2)
points(1:5, sim_result[, "pp"], pch = 1, col = "black", cex = 1)
# ADF
plot(ts(sim_result[, "adf"], start = 1), type = "l", xaxt = "n", xlab = "", main = "Rifiuti di ADF",
     ylab = "Percentuale di rifiuto")
axis(1, at = c(1:5), labels = c("phi = 0.2", "phi = 0.5", "phi = 0.8", "phi = 0.95", "phi = 1"), las = 2)
points(1:5, sim_result[, "adf"], pch = 1, col = "black", cex = 1)
par(mfrow = c(1,1))

# Test di omoschedasticità in piccoli campioni ------
{
  last_time = Sys.time()  # Tempo a inizio ciclo
  time_elapsed = 0
  intervallo = 15 # secondi che passano tra una stampa e l'altra per sapere dove sono nel ciclo
  
  obs = 18      # Numero di osservazioni
  rep = 1000000 # Ripetizioni 

  # Matrici dove andro a salvare i risultati
  homoskedasticity_result = matrix(NA, rep, 5)
  rownames(homoskedasticity_result) = 1:rep
  colnames(homoskedasticity_result) = c("Goldfeld-Quandt", "White", "s.e. OLS", "s.e. White", "Differenza % tra s.e.")
  
  eteroskedasticity_result = matrix(NA, rep, 5)
  rownames(eteroskedasticity_result) = 1:rep
  colnames(eteroskedasticity_result) = c("Goldfeld-Quandt", "White", "s.e. OLS", "s.e. White", "Differenza % tra s.e.")
  
  # NB: con differenza intendo (s.e. White - s.e. OLS)/(s.e. OLS), quindi quanto sono grandi/piccoli
  # gli s.e. di White rispetto a quelli stimati con OLS
  
  for (i in 1:rep){
    # Simulazione dei due processi
    x = runif(obs, 2, 10)
    y_omo = 10*x + 2*rnorm(obs)
    y_et = 7*x + rnorm(obs)*(x)
    
    # OMOSCHEDASTICITA'
    omo = lm(y_omo ~ x)
    gq_omo = as.numeric(gqtest(omo)[5])
    white_omo = as.numeric(white_test(omo)[2])
    
    if(gq_omo < 0.05){
        homoskedasticity_result[i, 1] = 1
    } else{
        homoskedasticity_result[i, 1] = 0
    }
    
    if(white_omo < 0.05){
      homoskedasticity_result[i, 2] = 1
    } else{
      homoskedasticity_result[i, 2] = 0
    }
    
    # s.e. OLS
    ols_se_omo = coefficients(summary(omo))[4]
    homoskedasticity_result[i, 3] = ols_se_omo
    # s.e. di White
    robust_se_omo = coeftest(omo, vcov = vcovHC(omo, type = "HC3"))[4]
    homoskedasticity_result[i, 4] = robust_se_omo
    # Differenze
    diff_se_omo = ((robust_se_omo - ols_se_omo)/(ols_se_omo))*100
    homoskedasticity_result[i, 5] = diff_se_omo
    
    
    # ETEROSCHEDASTICITA'
    etero = lm(y_et ~ x)
    gq_etero = as.numeric(gqtest(etero)[5])
    white_etero = as.numeric(white_test(etero)[2])
    
    if(gq_etero < 0.05){
      eteroskedasticity_result[i, 1] = 1
    } else{
      eteroskedasticity_result[i, 1] = 0
    }
    
    if(white_etero < 0.05){
      eteroskedasticity_result[i, 2] = 1
    } else{
      eteroskedasticity_result[i, 2] = 0
    }
    
    # s.e. OLS
    ols_se_etero = coefficients(summary(etero))[4]
    eteroskedasticity_result[i, 3] = ols_se_etero
    # s.e. di White
    robust_se_etero = coeftest(etero, vcov = vcovHC(etero, type = "HC3"))[4]
    eteroskedasticity_result[i, 4] = robust_se_etero
    # Differenze
    diff_se_etero = ((robust_se_etero - ols_se_etero)/(ols_se_etero))*100
    eteroskedasticity_result[i, 5] = diff_se_etero
    
    # Aggiungo delle istruzioni per sapere a che punto sono nel ciclo visto che probabilmente 
    # ci metterà un pò a simulare su grandi campioni
    current_time = Sys.time()  # Tempo corrente
    time_elapsed = as.numeric(difftime(current_time, last_time, units = "secs"))  # Tempo trascorso in secondi
    
    # Se è passato più di 10 secondi dall'ultima stampa, stampiamo
    if (time_elapsed >= intervallo) {
      # Stampa il messaggio e aggiorna l'ultimo tempo di stampa
      cat("Sono passati", round(as.numeric(difftime(current_time, last_time, units = "secs")), 1), "secondi e sono alla simulazione numero:", i, "\n")
      last_time = current_time  # Aggiorna l'ultimo tempo di stampa
    }
  }
  
  # Percentuale di rifiuti
  # Matrice riassuntiva
  rifiuti = matrix(c(0,0), 2, 2)
  colnames(rifiuti) = c("Goldfeld-Quandt", "White")
  rownames(rifiuti) = c("Omoschedasticità", "Eteroschedasticità")
  
  rifiuti[1,1] = sum(homoskedasticity_result[,1])/rep
  rifiuti[1,2] = sum(homoskedasticity_result[,2])/rep
  
  rifiuti[2,1] = sum(eteroskedasticity_result[,1])/rep
  rifiuti[2,2] = sum(eteroskedasticity_result[,2])/rep
  
  write.csv(rifiuti, "Results/Risultati simulazione omo-eteroschedasticità.csv")
  write.csv(homoskedasticity_result, "Results/Risultati omoschedasticità.csv")
  write.csv(eteroskedasticity_result, "Results/Risultati eteroschedasticità.csv")
}

rifiuti = read.csv("Results/Risultati simulazione omo-eteroschedasticità.csv", header = TRUE)
homoskedasticity = read.csv("Results/Risultati omoschedasticità.csv", header = TRUE)
eteroskedasticity = read.csv("Results/Risultati eteroschedasticità.csv", header = TRUE)

homos_ratio = NULL; etero_ratio = NULL
for (i in 1:length(homoskedasticity$s.e..OLS)){
  tmp_omo = homoskedasticity$s.e..OLS[i]/homoskedasticity$s.e..White[i] 
  tmp_ete = eteroskedasticity$s.e..OLS[i]/eteroskedasticity$s.e..White[i] 
  homos_ratio[i] = tmp_omo
  etero_ratio[i] = tmp_ete
}

t.test(homoskedasticity$s.e..OLS, homoskedasticity$s.e..White)
t.test(homos_ratio, mu = 1)

t.test(eteroskedasticity$s.e..OLS, eteroskedasticity$s.e..White)
t.test(etero_ratio, mu = 1)

# SUR in piccoli campioni ----
# L'obiettivo della simulazione è capire come si distribuisce la stima del beta di un SUR con errori
# NON distribuiti normalmente (proprio come nella mia stima)
# stimato su un campione di 18 osservazioni
{
  obs = 18  # Numero di osservazioni
  N = 10^6  # Numero di ripetizioni 
  
  # Vettori dove andrò a salvare i coefficienti e gli errori
  coeff_eq1 = matrix(NA, nrow = N, ncol = 3); rownames(coeff_eq1) = 1:N; colnames(coeff_eq1) = c("b", "se", "pvalue")
  coeff_eq2 = matrix(NA, nrow = N, ncol = 3); rownames(coeff_eq2) = 1:N; colnames(coeff_eq2) = c("b", "se", "pvalue")
  coeff_eq3 = matrix(NA, nrow = N, ncol = 3); rownames(coeff_eq3) = 1:N; colnames(coeff_eq3) = c("b", "se", "pvalue")
  errors_eq1 = matrix(NA, nrow = N, ncol = obs); rownames(errors_eq1) = 1:N; colnames(errors_eq1) = 1:obs
  errors_eq2 = matrix(NA, nrow = N, ncol = obs); rownames(errors_eq2) = 1:N; colnames(errors_eq2) = 1:obs
  errors_eq3 = matrix(NA, nrow = N, ncol = obs); rownames(errors_eq3) = 1:N; colnames(errors_eq3) = 1:obs
  
  last_time = Sys.time()  # Tempo a inizio ciclo
  time_elapsed = 0
  intervallo = 15 # secondi che passano tra una stampa e l'altra per sapere dove sono nel ciclo
  
  for (i in 1:N){
    # Creazione dei valori
    x1 = 2*rnorm(obs)
    x2 = 3*runif(obs)
    x3 = 0.5*rnorm(obs)
    e1 = rcauchy(obs)
    e2 = (0.5*e1) + (2*rnorm(obs))
    e3 = e2 + (0.5*rnorm(obs))
    y1 = x1 + e1
    y2 = 3*x2 + e2
    y3 = 0.5*x3 + e3
    # Ho scelto di estrarre il termine di errore da una Cauchy semplicemente per non farlo con una normale
    # Ho fatto tre casi: uno dove errori e x hanno peso uguale, uno con x con peso maggiore e uno invertito
    
    # Dataset 
    df = data.frame(
      x1 = x1,
      x2 = x2,
      x3 = x3,
      y1 = y1,
      y2 = y2,
      y3 = y3
    )
    
    eq1_simulation = as.formula("y1 ~ x1")
    eq2_simulation = as.formula("y2 ~ x2")
    eq3_simulation = as.formula("y3 ~ x3")
    
    sur_formulas = list(eq1 = eq1_simulation, eq2 = eq2_simulation, eq3 = eq3_simulation)
    sur_estimate = systemfit(sur_formulas, data = df, method = "SUR")
    sur_summary = summary(sur_estimate)
    
    # Prendo i coefficienti e li salvo in una matrice
    coeff_eq1[i, 1] = sur_summary$coefficients[2, 1]
    coeff_eq2[i, 1] = sur_summary$coefficients[4, 1]
    coeff_eq3[i, 1] = sur_summary$coefficients[6, 1]
    # Prenso gli s.e
    coeff_eq1[i, 2] = sur_summary$coefficients[2, 2]
    coeff_eq2[i, 2] = sur_summary$coefficients[4, 2]
    coeff_eq3[i, 2] = sur_summary$coefficients[6, 2]
    # Prendo i pvalue
    coeff_eq1[i, 3] = sur_summary$coefficients[2, 4]
    coeff_eq2[i, 3] = sur_summary$coefficients[4, 4]
    coeff_eq3[i, 3] = sur_summary$coefficients[6, 4]
    # Prendo i residui e gli salvo in una matrice
    errors_eq1[i ,] = sur_summary$residuals[, 1]
    errors_eq2[i ,] = sur_summary$residuals[, 2]
    errors_eq3[i ,] = sur_summary$residuals[, 3]
    
    # Aggiungo delle istruzioni per sapere a che punto sono nel ciclo visto che probabilmente 
    # ci metterà un pò a simulare su grandi campioni
    current_time = Sys.time()  # Tempo corrente
    time_elapsed = as.numeric(difftime(current_time, last_time, units = "secs"))  # Tempo trascorso in secondi
    
    # Se è passato più di 10 secondi dall'ultima stampa, stampiamo
    if (time_elapsed >= intervallo) {
      # Stampa il messaggio e aggiorna l'ultimo tempo di stampa
      cat("Sono passati", round(as.numeric(difftime(current_time, last_time, units = "secs")), 1), "secondi e sono alla simulazione numero:", i, "\n")
      last_time = current_time  # Aggiorna l'ultimo tempo di stampa
    }
  }
  
  write.csv(coeff_eq1, "Results/Simulazioni/SUR - b1.csv")
  write.csv(coeff_eq2, "Results/Simulazioni/SUR - b2.csv")
  write.csv(coeff_eq3, "Results/Simulazioni/SUR - b3.csv")
  write.csv(errors_eq1, "Results/Simulazioni/SUR - e1.csv")
  write.csv(errors_eq2, "Results/Simulazioni/SUR - e2.csv")
  write.csv(errors_eq3, "Results/Simulazioni/SUR - e3.csv")
}

coeff_eq1 = read.csv("Results/Simulazioni/SUR - b1.csv", header = TRUE)
coeff_eq2 = read.csv("Results/Simulazioni/SUR - b2.csv", header = TRUE)
coeff_eq3 = read.csv("Results/Simulazioni/SUR - b3.csv", header = TRUE)
errors_eq1 = read.csv("Results/Simulazioni/SUR - e1.csv", header = TRUE)
errors_eq2 = read.csv("Results/Simulazioni/SUR - e2.csv", header = TRUE)
errors_eq3 = read.csv("Results/Simulazioni/SUR - e3.csv", header = TRUE)

# Analisi dei residui
mean(as.numeric(errors_eq1[1, ][-1]))
mean(as.numeric(errors_eq2[1, ][-1]))
mean(as.numeric(errors_eq3[1, ][-1]))
# Hanno media nulla, come da ipotesi

cor(as.numeric(errors_eq1[1, ][-1]), as.numeric(errors_eq2[1, ][-1]))
cor(as.numeric(errors_eq1[1, ][-1]), as.numeric(errors_eq3[1, ][-1]))
cor(as.numeric(errors_eq2[1, ][-1]), as.numeric(errors_eq3[1, ][-1]))
# Gli errori sono (altamente) correlati, come da ipotesi

jarque.bera.test(as.numeric(errors_eq1[1, ][-1]))
jarque.bera.test(as.numeric(errors_eq2[1, ][-1]))
jarque.bera.test(as.numeric(errors_eq3[1, ][-1]))
shapiro.test(as.numeric(errors_eq1[1, ][-1]))
shapiro.test(as.numeric(errors_eq2[1, ][-1]))
shapiro.test(as.numeric(errors_eq3[1, ][-1]))
kurtosis(as.numeric(errors_eq1[1, ][-1]))
kurtosis(as.numeric(errors_eq2[1, ][-1]))
kurtosis(as.numeric(errors_eq3[1, ][-1]))
skewness(as.numeric(errors_eq1[1, ][-1]))
skewness(as.numeric(errors_eq2[1, ][-1]))
skewness(as.numeric(errors_eq3[1, ][-1]))
# Non si distribuiscono normalmente, come da ipotesi

Box.test(as.numeric(errors_eq1[1, ][-1]), lag = 1, type = "Ljung-Box")
Box.test(as.numeric(errors_eq2[1, ][-1]), lag = 1, type = "Ljung-Box")
Box.test(as.numeric(errors_eq3[1, ][-1]), lag = 1, type = "Ljung-Box")
# Gli errori non sono autocorrelati

# I residui rispettano le ipotesi iniziali

# Analisi delle stime dei coefficienti
bmean_eq1 = mean(coeff_eq1[, "b"]) # Valore vero: 1, valore medio stimato: 1.02
bmean_eq2 = mean(coeff_eq2[, "b"]) # Valore vero: 3, valore medio stimato: 2.97
bmean_eq3 = mean(coeff_eq3[, "b"]) # Valore vero: 0.5, valore stimato: 0.51

# Distorsione
distorsione_eq1 = ((bmean_eq1 - 1)/1)*100          # +2,01%
distorsione_eq2 = ((bmean_eq2 - 3)/3)*100          # -1,01%
distorsione_eq3 = ((bmean_eq3 - 0.5)/0.5)*100      # +3,45%

# Numero di beta stimati non significativi
counter1 = 0; counter2 = 0; counter3 = 0
for (i in 1:N){
  pvalue1 = coeff_eq1[i, "pvalue"]
  pvalue2 = coeff_eq2[i, "pvalue"]
  pvalue3 = coeff_eq2[i, "pvalue"]
  if(pvalue1 > 0.05){
    counter1 = counter1 + 1
  }
  
  if(pvalue2 > 0.05){
    counter2 = counter2 + 1
  }
  
  if(pvalue3 > 0.05){
    counter3 = counter3 + 1
  }
}
(counter1/N)*100; (counter2/N)*100; (counter3/N)*100

# Histogramma
par(mfrow = c(1,3))
hist(as.numeric(coeff_eq1[, "b"]), main = "Distribuzione delle stime di b (1)", xlab = "Coefficiente", ylab = "Frequenza",
     xlim = c(min(coeff_eq1[, "b"]), max(coeff_eq1[, "b"])))
hist(as.numeric(coeff_eq2[, "b"]), main = "Distribuzione delle stime di b (2)", xlab = "Coefficiente", ylab = "Frequenza",
     xlim = c(min(coeff_eq2[, "b"]), max(coeff_eq2[, "b"])))
hist(as.numeric(coeff_eq3[, "b"]), main = "Distribuzione delle stime di b (3)", xlab = "Coefficiente", ylab = "Frequenza",
     xlim = c(min(coeff_eq3[, "b"]), max(coeff_eq3[, "b"])))
par(mfrow = c(1,1))

# Non si distribuiscono normalmente, ma le stime sono comunque buone

# Test di normalità in piccoli campioni ----
{
  obs = 18
  N = 10^6
  
  rifiuti_normale_jb = 0; rifiuti_normale_sw = 0
  rifiuti_NONnormale_jb = 0; rifiuti_NONnormale_sw = 0
  
  for (i in 1:N){
    processo_normale = rnorm(obs)
    processo_NONnormale = runif(obs)
    
    # Test di normalità per un processo normale
    if (as.numeric(shapiro.test(processo_normale)[2]) < 0.05){
      rifiuti_normale_sw = rifiuti_normale_sw + 1
    }
    
    if (as.numeric(jarque.bera.test(processo_normale)[3]) < 0.05){
      rifiuti_normale_jb = rifiuti_normale_jb + 1
    }
    
    # Test di normalità per un processo non normale
    if (as.numeric(shapiro.test(processo_NONnormale)[2]) < 0.05){
      rifiuti_NONnormale_sw = rifiuti_NONnormale_sw + 1
    }
    
    if (as.numeric(jarque.bera.test(processo_NONnormale)[3]) < 0.05){
      rifiuti_NONnormale_jb = rifiuti_NONnormale_jb + 1
    }
  }
  rifiuti_summary = cbind(I((rifiuti_normale_sw/N)*100), I((rifiuti_normale_jb/N)*100),
           I((rifiuti_NONnormale_sw/N)*100), I((rifiuti_NONnormale_jb/N)*100))
  
  result = matrix(rifiuti_summary, nrow = 1, ncol = 4)
  colnames(result) = c("SW - normale", "JB - normale", "SW - Non normale", "JB - Non normale")
  
  write.csv(result, "Results/Simulazioni/Test di normalità.csv")
}

