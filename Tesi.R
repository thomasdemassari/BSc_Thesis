# BSC Thesis: "L'impatto dei tassi di interesse sul settore bancario"
# Owner: Thomas De Massari
# Last update: 2024-06-04

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

# Rimuovere i warnings
options(warn = -1)

# Acronimi delle banche
banks_acronym = c("hsbc", "bnp", "aca", "sanx", "bar", "gle", "dbk", "lloy", "isp", "inga", "ucg", 
                  "nwg", "stan", "bbva")

# SEZIONE 2:FUNZIONI ----
# Impostare i livelli del pvalue ----
# Questa funzione imposta un segno in base alla significativà del valore stimato
sign_pvalue = function(pavlue){ 
  levels = NULL
  
  for (j in 1:length(pavlue)){
    if ((pavlue[j] >= 0) & pavlue[j] <= 0.001){
      tmp = "***" 
    } else{
        if ((pavlue[j] > 0.001) & pavlue[j] <= 0.01){
          tmp = "**" 
        } else{
            if ((pavlue[j] > 0.01) & pavlue[j] <= 0.05){
              tmp = "*" 
            } else {
                if ((pavlue[j] > 0.05) & pavlue[j] <= 0.1){
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
  for (i in 1:length(X[1,])){
    x = X[, i]
    cor_tmp = cor(x, res)
    if (round(cor_tmp,2) == 0){
      cor_boolean[i] = TRUE
    } else{
      cor_boolean[i] = FALSE
    }
  }
  
  if (any(cor_boolean == FALSE)){
    return(FALSE)
  } else{
    return(TRUE)
  }
}

# Stime dei modelli -----
# La funzione serve per stimare i modelli necessari per l'analisi, calcolare i test che ho ritenuto 
# necessario fare e stampare in un file csv i risultati. La scrittura in una funzione unica permette
# di avere il codice più compatto e di semplificare la correzione di eventuali errori. In input la funzione
# prende il tipo di modello (bilancio o azioni) e il nome della y (roa, nii, oi, prov, azioni). 
# Restituisce una lista con dentro quattro matrici:
# 1. Una contenente le stime dei coefficienti con i relativi pvalue (calcolati su statistiche test)
# che fanno riferimento a s.e. stimati. con HC3
# 2. Una contenente i pvalue dei test
# 3. Un'ultima matrice con gli s.e. stimati con OLS e con HC3, utile per un confronto
# 4. Una matrice con i residui 
# Per accendere a una delle matrici è sufficiente usare $coef, $test, $sds oppure $res
# La matrici vengono anche salvate nel percorso "path" indicato. Se non si vogliono salvare 
# basta mettere path = "non salvare"

estimate_models = function(type, y_name, path){
# Matrice dei coefficienti (e relativi pvalue), matrice degli standard error e matrice per i residui
if (type == "bilancio"){
    crisis = data_fs$crisis[data_fs$ID == 1] # Dummy per la crisi
    
    coef_matrix = matrix(NA, nrow = 14, ncol = 12)
    colnames(coef_matrix) =  c("b0","pvalue_b0", "b1 (intrate)", "pvalue_b1", "b2 (dcb)", "pvalue_b2", 
                               "b3 (crisis)", "pvalue_b3", "b4 (intrate * crisis)", "pvalue_b4", 
                               "b5 (dcb * crisis)", "pvalue_b5")
    rownames(coef_matrix) = banks_acronym
    
    sds_matrix = matrix(NA, nrow = 14, ncol = 12)
    colnames(sds_matrix) = c("s.e. OLS - b0", "s.e. HC3 - b0", "s.e. OLS - intrate", "s.e. HC3 - intrate",
                          "s.e. OLS - dcb", "s.e. HC3 - dcb", "s.e. OLS - crisis", "s.e. HC3 - crisis",
                          "s.e. OLS - c*int", "s.e. HC3 - c*int", "s.e. OLS - c*dcb", "s.e. HC3 - c*dcb")
    rownames(sds_matrix) = banks_acronym
    
    res_matrix = matrix(NA, nrow = 14, ncol = 18)
    colnames(res_matrix) = 1:18
    rownames(res_matrix) = banks_acronym
    
  } else{
      if (type == "azioni"){
      coef_matrix = matrix(NA, nrow = 14, ncol = 6)
      colnames(coef_matrix) = c("b0","pvalue_b0", "b1 (intrate)", "pvalue_b1", "b2 (mkt)", "pvalue_b2")
      rownames(coef_matrix) = banks_acronym
      
      sds_matrix = matrix(NA, nrow = 14, ncol = 6)
      colnames(sds_matrix) = c("s.e. OLS - b0", "s.e. HC3 - b0", "s.e. OLS - intrate", "s.e. HC3 - intrate",
                               "s.e. OLS - mkt", "s.e. HC3 - mkt")
      rownames(sds_matrix) = banks_acronym
      
      res_matrix = matrix(NA, nrow = 14, ncol = 215)
      colnames(res_matrix) = 1:215
      rownames(res_matrix) = banks_acronym
    } else{
    stop("Erroe. Indica il tipo di modello che stai usando")
    }
  }
  
  # Matrice per i risultati dei test
  test_matrix = matrix(NA, nrow = 14, ncol = 15)
  colnames(test_matrix) = c("R2", "R2 adj", "Test F (pvalue)", "Esogeneità", "cor(intrate, market index)", 
                             "Goldfeld-Quandt (pvalue)", "White (pvalue)", "Shapiro-Wilk (pvalue)", 
                             "Jarque-Bera (pvalue)", "Durbin-Watson (pvalue)", "Ljung-Box (pvalue)",
                             "CUSUM (pvalue)", "RESET (pvalue)", "Curtosi", "Asimmetria")
  rownames(test_matrix) = banks_acronym

  
  # Modello con i dati di bilancio
  for (j in 1:length(banks_acronym)){
    # Definizione della variabile dipendente
    if (type == "bilancio" & y_name == "roa"){
      y = data_fs$ROA[data_fs$ID == j]
    } else{
      if (type == "bilancio" & y_name == "nii"){
        y = data_fs$NII_Assets[data_fs$ID == j]
      } else{
        if (type == "bilancio" & y_name == "oi"){
          y = data_fs$OI_Assets[data_fs$ID == j]
        } else{
          if (type == "bilancio" & y_name == "prov"){
            y = data_fs$PROV_Assets[data_fs$ID == j]
          } else{
            break 
          }
        }
      }
    }
    
    # Definizio del tasso
    if ((data_fs$Nation[data_fs$ID == j])[1] == "uk"){
      intrate = libor
    } else{
      intrate = euribor
    }
    
    # Riserve
    dcb = data_fs$DepositCentralBank_Assets[data_fs$ID == j]
    
    # Stima del modello
    model = lm(y ~ intrate + dcb + crisis + I(intrate*crisis) + I(dcb*crisis))
    summary_model = summary(model)
    
    res_matrix[j, ] = residuals(model)
    
    # Standard error con OLS e con HC3
    se_ols = summary_model$coefficients[, "Std. Error"]
    hc3 = coeftest(model, vcov = vcovHC(model, type = "HC3"))
    se_hc3 = hc3[, "Std. Error"]
    
    # Matrice riassuntiva tra i due stimatori
    index_seols = c(1,3,5,7,9,11); index_sehc3 = c(2,4,6,8,10,12); i_se = 1
    for (i_se in 1:6){
      sds_matrix[j, index_seols[i_se]] = se_ols[i_se]
      sds_matrix[j, index_sehc3[i_se]] = se_hc3[i_se]
    }
    
    # Controllo se il pvalue è minore uguale di 0.05. Se si, mi salva i risultati,
    # altrimenti lascia 0 per il coefficiente, ma salvo comunque il pvalue da mostrare poi nel csv finale
    index_pvalue1 = 1; index_pvalue2 = 1
    for (index_pvalue1 in 1:length(hc3[,1])){
      if (hc3[, "Pr(>|t|)"][index_pvalue1] <= 0.05){ 
        coef_matrix[j, index_pvalue2] = round(hc3[, "Estimate"][index_pvalue1], 5)
        pvalue_coeff = hc3[, "Pr(>|t|)"][index_pvalue1]
        coef_matrix[j, (index_pvalue2+1)] = paste(round(pvalue_coeff, 5), sign_pvalue(pvalue_coeff))
      } else{
        coef_matrix[j, index_pvalue2] = 0
        pvalue_coeff = hc3[, "Pr(>|t|)"][index_pvalue1]
        coef_matrix[j, (index_pvalue2+1)] = paste(round(pvalue_coeff, 5), sign_pvalue(pvalue_coeff))
      }
      index_pvalue2 = index_pvalue2 + 2
    }
    
    # Aggiungo e salvo altri dati del modello
    test_matrix[j, 1] = round(summary_model$r.squared, 5) # R2
    test_matrix[j, 2] = round(summary_model$adj.r.squared, 5) # R2 adj
    # pvalue test F
    f = summary_model$fstatistic
    pvalue_f = pf(f[1], f[2], f[3], lower.tail = FALSE)
    pvalue_f_sign = sign_pvalue(pvalue_f)
    test_matrix[j, 3] = paste(round(pvalue_f, 5),  pvalue_f_sign)
    # Esogeneità (calcolata come semplice correlazione tra regressori e residui)
    exogeneity_justcorr = exogeneity(residuals(model), cbind(intrate, dcb))
    test_matrix[j, 4] = as.character(exogeneity_justcorr) # Esogeneità come semplice correlazione
    # Verifica della presenza della multicollinearità
    cor_intrate_dcb = cor(intrate, dcb)
    test_matrix[j, 5] = round(cor_intrate_dcb, 5)
    # pvalue del test di Goldfeld-Quandt (per saggiare l'ipotesi di assenza di eteroschedasticità)
    gq_pvalue = as.numeric(gqtest(model)[5])
    test_matrix[j, 6] = paste(round(gq_pvalue, 5), sign_pvalue(gq_pvalue))
    # pvalue del test di White (per saggiare l'ipotesi di assenza di eteroschedasticità)
    white_pvalue = as.numeric(white_test(model)[2])
    test_matrix[j, 7] = paste(round(white_pvalue,5), sign_pvalue(white_pvalue))
    # pvalue del test di Shapiro-Wilk (per saggiare l'ipotesi di normalità)
    shapiro_pvalue = as.numeric(shapiro.test(model$residuals)[2])
    test_matrix[j, 8] = paste(round(shapiro_pvalue, 5), sign_pvalue(shapiro_pvalue))
    # pvalue del test di Jarque-Bera (per saggiare l'ipotesi di normalità)
    jb_pvalue = as.numeric(jarque.bera.test(model$residuals)[3])
    test_matrix[j, 9] = paste(round(jb_pvalue, 5), sign_pvalue(jb_pvalue))
    # pvalue del test di Durbin-Watson (per saggiare l'ipotesi di assenza di autocorrelazione tra i residui)
    dw_pvalue = as.numeric(dwtest(model)[4])
    test_matrix[j, 10] = paste(round(dw_pvalue, 5), sign_pvalue(dw_pvalue))
    # pvalue del test di Ljung-Box
    ljungbox_pvalue = as.numeric(Box.test(residuals(model), lag = 1, type = "Ljung-Box")[3])
    test_matrix[j, 11] = paste(round(ljungbox_pvalue, 5), sign_pvalue(ljungbox_pvalue))
    # pvalue del test CUSUM (per saggiare l'assenza di break strutturali)
    cusum_pvalue = as.numeric(sctest(model, type = "cusum")[2])
    test_matrix[j, 12] = paste(round(cusum_pvalue, 5), sign_pvalue(cusum_pvalue))
    # pvalue del test RESET di Ramsey (per controllare la forma funzionale del modello)
    reset_pvalue = as.numeric(resettest(model)[4])
    test_matrix[j, 13] = paste(round(reset_pvalue, 5), sign_pvalue(reset_pvalue))
    # Curtosi e asimmetria
    test_matrix[j, 14] = round(kurtosis(residuals(model)), 5)
    test_matrix[j, 15] = round(skewness(residuals(model)), 5)
  }
  
  # Modello con i prezzi delle azioni
  for (j in 1:length(banks_acronym)){
    if (type != "azioni" | y_name != "azioni"){
      break
    } 
    
    # Differenze logaritmiche delle variabili
    log_prezzo = log(data_sp$Stockprice[data_sp$ID == j])
    diffslog_prezzo = diff(log_prezzo)
    log_market_index = log(data_sp$Market_Index[data_sp$ID == j])
    diffslog_marketindex = diff(log_market_index)
    
    # Definizio del tasso
    if (data_sp$Nation[data_sp$ID == j][1] == "uk"){
      intrate_monthly = log(libor_monthly)
    } else{
      intrate_monthly = log(euribor_monthly_adjusted)
    }
    diffslog_intrate_monthly = diff(intrate_monthly)
    
    # Stima del modello
    azioni_OLS = lm(diffslog_prezzo ~ diffslog_intrate_monthly + diffslog_marketindex)
    summary_azioni_OLS = summary(azioni_OLS)
    
    res_matrix[j, ] = residuals(azioni_OLS)
    
    # Standard error con OLS e con HC3
    se_ols = summary_azioni_OLS$coefficients[, "Std. Error"]
    hc3 = coeftest(azioni_OLS, vcov = vcovHC(azioni_OLS, type = "HC3"))
    se_hc3 = hc3[, "Std. Error"]
    
    # Matrice riassuntiva tra i due stimatori
    index_seols = c(1,3,5); index_sehc3 = c(2,4,6); i_se = 1
    for (i_se in 1:3){
      sds_matrix[j, index_seols[i_se]] = se_ols[i_se]
      sds_matrix[j, index_sehc3[i_se]] = se_hc3[i_se]
    }
    
    # Ciclo for per controllare se il pvalue è minore uguale di 0.05. Se si, mi salva i risultati,
    # altrimenti lascia 0 per il coefficiente, ma salvo comunque il pvalue
    index_pvalue1 = 1; index_pvalue2 = 1
    for (index_pvalue1 in 1:length(hc3[,1])){
      if (hc3[, "Pr(>|t|)"][index_pvalue1] <= 0.05){ 
        coef_matrix[j, index_pvalue2] = as.numeric(round(hc3[, "Estimate"][index_pvalue1], 5))
        pvalue_coeff = hc3[, "Pr(>|t|)"][index_pvalue1]
        coef_matrix[j, (index_pvalue2+1)] = paste(round(pvalue_coeff, 5), sign_pvalue(pvalue_coeff))
      } else{
        coef_matrix[j, index_pvalue2] = as.numeric(0)
        pvalue_coeff = hc3[, "Pr(>|t|)"][index_pvalue1]
        coef_matrix[j, (index_pvalue2+1)] = paste(round(pvalue_coeff, 5), sign_pvalue(pvalue_coeff))
      }
      index_pvalue2 = index_pvalue2 + 2
    }
    
    # Aggiungo e salvo altri dati del modello
    test_matrix[j, 1] = round(summary_azioni_OLS$r.squared, 5)     # R2
    test_matrix[j, 2] = round(summary_azioni_OLS$adj.r.squared, 5) # R2 adj
    # pvalue test F
    f = summary_azioni_OLS$fstatistic
    pvalue_f = pf(f[1], f[2], f[3], lower.tail = FALSE)
    pvalue_f_sign = sign_pvalue(pvalue_f)
    test_matrix[j, 3] = paste(round(pvalue_f, 5),  pvalue_f_sign)
    # Esogeneità (calcolata come semplice correlazione tra regressori e residui)
    exogeneity_justcorr = exogeneity(residuals(azioni_OLS), cbind(diffslog_intrate_monthly, diffslog_marketindex))
    test_matrix[j, 4] = as.character(exogeneity_justcorr) # Esogeneità come semplice correlazione
    # Verifica della presenza della multicollinearità
    cor_intrate_marketindex = cor(diffslog_intrate_monthly, diffslog_marketindex)
    test_matrix[j, 5] = round(cor_intrate_marketindex, 5)
    # pvalue del test di Goldfeld-Quandt (per saggiare l'ipotesi di assenza di eteroschedasticità)
    gq_pvalue = as.numeric(gqtest(azioni_OLS)[5])
    test_matrix[j, 6] = paste(round(gq_pvalue, 5), sign_pvalue(gq_pvalue))
    # pvalue del test di White (per saggiare l'ipotesi di assenza di eteroschedasticità)
    white_pvalue = as.numeric(white_test(azioni_OLS)[2])
    test_matrix[j, 7] = paste(round(white_pvalue,5), sign_pvalue(white_pvalue))
    # pvalue del test di Shapiro-Wilk (per saggiare l'ipotesi di normalità)
    shapiro_pvalue = as.numeric(shapiro.test(azioni_OLS$residuals)[2])
    test_matrix[j, 8] = paste(round(shapiro_pvalue, 5), sign_pvalue(shapiro_pvalue))
    # pvalue del test di Jarque-Bera (per saggiare l'ipotesi di normalità)
    jb_pvalue = as.numeric(jarque.bera.test(azioni_OLS$residuals)[3])
    test_matrix[j, 9] = paste(round(jb_pvalue, 5), sign_pvalue(jb_pvalue))
    # pvalue del test di Durbin-Watson (per saggiare l'ipotesi di assenza di autocorrelazione tra i residui)
    dw_pvalue = as.numeric(dwtest(azioni_OLS)[4])
    test_matrix[j, 10] = paste(round(dw_pvalue, 5), sign_pvalue(dw_pvalue))
    # pvalue del test Ljung-Box
    ljungbox_pvalue = as.numeric(Box.test(residuals(azioni_OLS), lag = 1, type = "Ljung-Box")[3])
    test_matrix[j, 11] = paste(round(ljungbox_pvalue, 5), sign_pvalue(ljungbox_pvalue))
    # pvalue del test CUSUM (per saggiare l'assenza di break strutturali)
    cusum_pvalue = as.numeric(sctest(azioni_OLS, type = "cusum")[2])
    test_matrix[j, 12] = paste(round(cusum_pvalue, 5), sign_pvalue(cusum_pvalue))
    # pvalue del test RESET di Ramsey (per controllare la forma funzionale del modello)
    reset_pvalue = as.numeric(resettest(azioni_OLS)[4])
    test_matrix[j, 13] = paste(round(reset_pvalue, 5), sign_pvalue(reset_pvalue))
    # Curtosi e asimmetria
    test_matrix[j, 14] = round(kurtosis(residuals(azioni_OLS)), 5)
    test_matrix[j, 15] = round(skewness(residuals(azioni_OLS)), 5)
  }
  
  if (path != "non salvare"){
    write.csv(coef_matrix, file = paste0(path, "/", y_name, "_coef.csv"))
    write.csv(test_matrix, file = paste0(path, "/", y_name, "_test.csv"))
    write.csv(sds_matrix, file = paste0(path, "/", y_name, "_sds.csv"))
    write.csv(res_matrix, file = paste0(path, "/", y_name, "_residuals.csv"))
  }
  
  return(list(coef = coef_matrix, test = test_matrix, sds = sds_matrix, res = res_matrix))
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
    euribor_monthly_adjusted[j] = 0.0001
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
# path = "/Users/thomasdemassari/Library/CloudStorage/OneDrive-Personal/Università/BSc/Tesi/Data/Results/Estimates"
path = "non salvare"

lm_roa = estimate_models("bilancio", "roa", path)
lm_nii = estimate_models("bilancio", "nii", path)
lm_oi = estimate_models("bilancio", "oi", path)
lm_prov = estimate_models("bilancio", "prov", path)
lm_azioni = estimate_models("azioni", "azioni", path)

# Matrice riassuntiva dei coefficienti per i modelli con dati di bilancio
sumup_coef = matrix(NA, nrow = 14, ncol = 27)
rownames(sumup_coef) = banks_acronym
colnames(sumup_coef) = c(rep(c("Intercetta", "Tasso di interesse", "Riserve", "Crisis", 
                             "Tasso di interesse x crisis", "Riserve x crisis"), 4), 
                         "Interecetta", "Tasso di interesse", "Indice di mercato")

index_coef = 1
for (i in 1:6){
  sumup_coef[, i] = lm_roa$coef[, index_coef]
  sumup_coef[, i+6] = lm_nii$coef[, index_coef]
  sumup_coef[, i+12] = lm_oi$coef[, index_coef]
  sumup_coef[, i+18] = lm_prov$coef[, index_coef]
  
  index_coef = index_coef + 2
}

sumup_coef[, 25] = lm_azioni$coef[, 1]
sumup_coef[, 26] = lm_azioni$coef[, 3]
sumup_coef[, 27] = lm_azioni$coef[, 5]

Tsumup_coef = t(sumup_coef)

write.csv(Tsumup_coef, "/Users/thomasdemassari/Desktop/coeff_summary.csv")

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

# SEZIONE 6: SIMULAZIONI ----
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
