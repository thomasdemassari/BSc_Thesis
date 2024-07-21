# %% [markdown]
# # Bachelor thesis

# %% [markdown]
# ## Librerie

# %%
import os
import csv
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import random
import math

import statsmodels.api as sm
import statsmodels.formula.api as smf
import statsmodels.stats.outliers_influence as smo
import statsmodels.stats.diagnostic as smd
import statsmodels.stats.stattools as stools
from statsmodels.stats.anova import anova_lm

from scipy import stats

from datetime import datetime

from collections import Counter

from openpyxl import load_workbook
from openpyxl.styles import Alignment

import warnings
warnings.filterwarnings("ignore", message="kurtosistest only valid for n>=20")

from dwtest import dwtest

now = datetime.now().strftime('%Y-%m-%d at %H.%M')
print(f"Last update: {datetime.now().strftime('%Y-%m-%d at %H.%M')}")

# %% [markdown]
# ## Funzioni

# %% [markdown]
# ### Set working directory
# Funzione per impostare la cartella di lavoro (e verificare al tempo stesso che esita). In input è sufficiente inserire il percorso della cartella.
# 
# Esempio: *setwd(path)*

# %%
def setwd(path):
    try:
        import os
        directory = path

        # Verifica che il percorso della cartella esista
        if os.path.exists(directory):
            # Imposta la cartella di lavoro corrente
            os.chdir(directory)
            #print(f"\nCartella di lavoro impostata su: {directory}")
        else:
            print(f"\nIl percorso della cartella '{directory}' non esiste.")
    except Exception:
        print("\nErrore durante l'impostazione della cartella di lavoro.")

# %% [markdown]
# ### Time series
# Funzione per convertire un dataset in una serie storica (R-style). Gli argomenti sono:
# - data: valori della serie storica;
# - start: tupla con (primo anno, primo sottoperiodo). Se la serie è su base annuale bisogna comunque inserire (x, 1).
# - end: tupla con (ultimo anno, ultimo sottoperiodo). Se la serie è su base annuale bisogna comunque inserire (x, 1).
# - frequency: sottoperiodi dei vari anni (es. semestrale = 2, trimestrale = 4, mensile = 12). Se la serie è su base annuale bisogna comunque inserire frequency = 1.
# 
# La funzione utilizza la libreria Pandas e restituisce la serie storica in formato Pandas Series (*class 'pandas.core.series.Series'*). 
# 
# NB: La funzione funziona solo con serie storiche su base annuale, semestrale, trimestrale, quadrimestrale, mensile. Non funziona quindi con serie storiche su base giornaliera.
# 
# Esempio per creare una serie storica con i valori contenuti in *dataset* per il periodo 2024-2025 (su base mensile, a partire da gennaio 2024):    
# *dataset = list(range(1, 25))*  
# *timeseries = ts(dataset, (2024, 1), (2025, 12), 12)*   
# *print(timeseries)* 
# 
# NB: Per fare l'equivalente della funzione *window()* di R è sufficiente usare il metodo *.iloc["lower limit" : "upper limit"]* della libreria Pandas.

# %%
# Funzione per creare una time series
def ts(data, start, end, frequency):
    try:
        first_year = start[0]
        first_subperiod = start[1]
        last_year = end[0]
        last_subperiod = end[1]

        years = list(range(first_year, (last_year+1)))
        times = list()

        if frequency == 1: # Annual
            times.append(years)
        else:
            if frequency == 2: # Biannual 
                for i in range(len(years)):
                    times.append(f"S1/{years[i]}")
                    times.append(f"S2/{years[i]}")
            else: 
                if frequency == 3: # Four-month
                    for i in range(len(years)):
                        times.append(f"Q1/{years[i]}")
                        times.append(f"Q2/{years[i]}")
                        times.append(f"Q3/{years[i]}")
                else:
                    if frequency == 4: # Quarterly
                        for i in range(len(years)):
                            times.append(f"Q1/{years[i]}")
                            times.append(f"Q2/{years[i]}")
                            times.append(f"Q3/{years[i]}")
                            times.append(f"Q4/{years[i]}")
                    else:
                        if frequency == 12: # Monthly
                            months = list(range(1,13))
                            for i in range(len(years)):
                                for j in range(len(months)):
                                    times.append(f"{months[j]}/{years[i]}")

        if first_subperiod != 1:
                times = times[first_subperiod - 1:]

        if last_subperiod != frequency:
            removeindex = frequency - last_subperiod
            times = times[:-removeindex]

        timeseries = pd.Series(list(data), index = times)

        return timeseries
    
    except Exception:
        print("Something went wrong. Check that you have entered the parameters correctly.")

# %% [markdown]
# ### Funzione per prendere i dati di bilancio che mi servono
# Funzione che prende in input i bilanci che ho scaricato da Orbis in formato xlsx ed estrae il ROA, il reddito netto da interessi, il reddito non da interessi, il totale dell'attivo e il margine netto di interesse. Questi dati vengono poi salvati in un nuovo file csv, il quale viene a sua volta salvato nella stessa cartella dove sono stati salvati i bilanci iniziali.    
# Il parametro *print_report* impostato su *True* serve semplicemente per stampare una stringa se l'esito della funzione è positivo. Di default è impostato su *False*.
# 
# NB: in input è richiesto il percorso del file dei bilanci, non il nome del file (il quale deve essere "Financial statement.xlsx").
# 
# Esempio:    
# *getmydata("/Users/thomasdemassari/Documents/Università/BSc/Tesi/Data/Banks/14. Unicredit")*

# %%
def getmydata(path, print_report = False):
    try:
        # Lettura dei datasets
        balance_sheet = pd.read_excel(f"{path}/Financial statement.xlsx", sheet_name="Stato patrimoniale")
        income_statement = pd.read_excel(f"{path}/Financial statement.xlsx", sheet_name="Conto economico")
        indices = pd.read_excel(f"{path}/Financial statement.xlsx", sheet_name="Indici")
        
        namebanks = path.split("/")

        # Prendo i dati (e gli inverto, i datasets sono 2023 - 2005)
        nii = income_statement.iloc[11][::-1] # Net interest income
        oi = income_statement.iloc[14][::-1] # Non interest income
        prov = income_statement.iloc[20][::-1] # Loan loss provision
        assets = balance_sheet.iloc[28][::-1] # Total assets
        centralbank = balance_sheet.iloc[25][::-1] # Deposit at Central Bank
        roa = indices.iloc[42][::-1] # ROA
        int_margin = indices.iloc[36][::-1] # Net interest margin

        years = list(range(2005, 2024))

        # Creazione di un file riassuntivo
        os.chdir(path)
        dataset = np.array(list(zip(years, nii, oi, prov, assets, centralbank, roa, int_margin)))

        # Creo il nuovo csv
        nome_file = "Financial statement - summary.csv"
        with open(nome_file, 'w', newline ='') as file:
            writer = csv.writer(file)
            writer.writerow(["Year", "Net Interest Income", "Non Interest Income", "Loan loss provision", "Assets", "Deposit at Central Bank", "ROA", "Net Interest Margin"])

            for riga in dataset:
                writer.writerow(riga)
        
        if print_report == True:
            print(f"The file {namebanks[-1]} was created correctly.")

    except Exception:
        print(f"Something went wrong. The file {namebanks[-1]} was not created.")

# %% [markdown]
# ### Funzione per impostare i livelli di significativa del pvalue
# La funzione restituisce una lista con dei simboli (R-Style) per vedere ad occhio la significativa dei parametri. È utile quando si va a creare un csv riassuntivo con i parametri dei modelli stimati.
# 
# Legenda:
# - 0 <= pavalue <= 0.001: ***
# - 0.001 < pavalue <= 0.01: **
# - 0.01 < pavalue <= 0.05: *
# - 0.05 < pavalue <= 0.1: .
# - 0.1 < pavalue <= 1: 

# %%
def sign_pvalue(pavlue):
    try:
        levels = list()
        for j in range(len(pavlue)):
            if (pavlue[j] >= 0) and pavlue[j] <= 0.001:
                tmp = "***" 
            else:
                if (pavlue[j] > 0.001) and pavlue[j] <= 0.01:
                    tmp = "**" 
                else:
                    if (pavlue[j] > 0.01) and pavlue[j] <= 0.05:
                        tmp = "*" 
                    else:
                        if (pavlue[j] > 0.05) and pavlue[j] <= 0.1:
                            tmp = "." 
                        else:
                            tmp = " "
            levels.append(tmp)
        
        return levels 
    
    except Exception:
        print("Something went wrong. Make sure you have entered a list of p-values.")  

# %% [markdown]
# ### Funzione per trasformare un dataset da mensile a annuale
# Funzione per convertire una serie storica su base mensile in una su base annuale (media annuale). I parametri richiesti sono:
# - path: percorso (nome incluos) del file contenente la serie storica su base giornaliera
# - col_date: numero (notazione python) della colonna in cui sono contenute le date
# - col_numero: numero (notazione python) della colonna in cui sono contenuti i valori
# - year_index: tupla contentene la prima e l'ultima+1 posizione delò'anno nella stringa delle date (es. se la data è 02/2002 la tupla in year_index sarà (3, 7))
# - years_range: tupla contentente il primo anno e l'ultimo anno+1 della serie storica (es. se la serie storica va dal 2000 al 2005 la tupla in years_range sarà (2000, 2006))
# - new_path: nuovo percorso (nome incluso) del file da creare
# - reverse: condizione boolena per inveritre il dataset (es. l'ultima data diventa la prima, con i relativi valori). Di default è impostato su *False*

# %%
def mon2ann(path, col_date, col_values, year_index, years_range, new_path, reverse = False):
    try:
        dataset = np.genfromtxt(path, delimiter=",", skip_header=1, dtype=None, encoding=None)

        if (reverse == True):
            dataset = np.flip(dataset, axis=0)

        result = 0
        dataset_annual = list()
        dataset_times = list()
        years = list(range(years_range[0], years_range[1]))
        counter = 0

        j = 0
        i = 0
        while i < len(dataset):
            if (int(dataset[i][col_date][year_index[0]:year_index[1]]) == int(years[j])):
                result = result + float(dataset[i][col_values])
                counter = counter + 1
                i = i + 1
                continue
            else:
                average = result/counter
                dataset_annual.append(average)
                dataset_times.append(f"{dataset[i-1][col_date][year_index[0]:year_index[1]]}")
                counter = 0
                result = 0
                j = j + 1

        average_final = result/counter
        dataset_annual.append(average_final)

        #Creazione il nuovo file CSV
        newdataset = np.array(list(zip(dataset_times, dataset_annual)))

        with open(new_path, 'w', newline ='') as file:
            writer = csv.writer(file)
            writer.writerow(["Times", "Value"])

            for riga in newdataset:
                writer.writerow(riga)

        print("The file was created correctly.")

    except Exception:
        print(f"Something went wrong. The file was not created.")

# %% [markdown]
# ### Funzione per trasformare un dataset da giornaliero a mensile
# Funzione per convertire una serie storica su base giornaliera in una su base mensile (media mensile). I parametri richiesti sono:
# - path: percorso (nome incluos) del file contenente la serie storica su base giornaliera
# - col_date: numero (notazione python) della colonna in cui sono contenute le date
# - col_numero: numero (notazione python) della colonna in cui sono contenuti i valori
# - month_index: tupla contentene la prima e l'ultima+1 posizione del mese nella stringa delle date (es. se la data è 02/2002 la tupla in month_index sarà (0, 2))
# - year_index: tupla contentene la prima e l'ultima+1 posizione delò'anno nella stringa delle date (es. se la data è 02/2002 la tupla in year_index sarà (3, 7))
# - years_range: tupla contentente il primo anno e l'ultimo anno+1 della serie storica (es. se la serie storica va dal 2000 al 2005 la tupla in years_range sarà (2000, 2006))
# - new_path: nuovo percorso (nome incluso) del file da creare
# - reverse: condizione boolena per inveritre il dataset (es. l'ultima data diventa la prima, con i relativi valori). Di default è impostato su *False*

# %%
def day2mon(path, col_date, col_values, month_index, year_index, years_range, new_path, reverse = False):
    try:
        dataset = np.genfromtxt(path, delimiter=",", skip_header=1, dtype=None, encoding=None)

        if (reverse == True):
            dataset = np.flip(dataset, axis=0)

        month = ["01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"]
        month = month * (years_range[1] - years_range[0])
        result = 0
        dataset_monthly = list()
        dataset_times = list()
        j = 0
        counter = 0

        i = 0
        while i < len(dataset):
            if dataset[i][col_date][month_index[0]:month_index[1]] == month[j]:
                result = result + dataset[i][col_values]
                counter = counter + 1
                i = i + 1
                continue
            else:
                average = result/counter
                dataset_monthly.append(average)
                dataset_times.append(f"{month[j]}-{dataset[i-1][col_date][year_index[0]:year_index[1]]}")
                counter = 0
                result = 0
                j = j + 1  

        average_final = result/counter
        dataset_monthly.append(average_final)

        #Creazione il nuovo file CSV
        newdataset = np.array(list(zip(dataset_times, dataset_monthly)))

        with open(new_path, 'w', newline ='') as file:
            writer = csv.writer(file)
            writer.writerow(["Times", "Value"])

            for riga in newdataset:
                writer.writerow(riga)

        print("The file was created correctly.")

    except Exception:
        print(f"Something went wrong. The file was not created.")

# %% [markdown]
# ## Gestione dei datasets

# %% [markdown]
# ### Manipolazione dei datasets

# %%
# Calcolo della media mensile dei valori del FTSE 100
path = "/Users/thomasdemassari/Documents/Università/BSc/Tesi/Data/Macroeconomics conditions/FTSE 100 - daily.csv"
new_path = "/Users/thomasdemassari/Documents/Università/BSc/Tesi/Data/Macroeconomics conditions/FTSE 100 - monthly.csv"
day2mon(path, 0, 4, (0,2), (6,8), (1896, 2025), new_path, reverse = True)

# Calcolo della media annuale dei valori dell'inflazione europea
path = "/Users/thomasdemassari/Documents/Università/BSc/Tesi/Data/Macroeconomics conditions/EU Inflation - monthly.csv"
new_path = "/Users/thomasdemassari/Documents/Università/BSc/Tesi/Data/Macroeconomics conditions/EU Inflation - annual.csv"
mon2ann(path, 6, 7, (0,4), (1997, 2025), new_path)

# %%
# Prendo i dati di bilancio che mi servono con la funzione scritta prima
banks = ["1. HSBC",      
        "2. BNP Paribas",
        "3. Crédit Agricole",         
        "4. Banco Santander",
        "5. Barclays",
        "8. Société Générale",
        "9. Deutsche Bank",                   
        "11. Lloyds Banking Group",            
        "12. Intesa Sanpaolo",            
        "13. ING Groep",                   
        "14. UniCredit",                    
        "15. NatWest Group",                 
        "16. Standard Chartered",          
        "18. Banco Bilbao Vizcaya Argentaria"]

for j in range(len(banks)):
    getmydata(f"/Users/thomasdemassari/Documents/Università/BSc/Tesi/Data/Banks/{banks[j]}")

print("The files were created correctly.")

# %% [markdown]
# ### Creazione del panel

# %%
setwd("/Users/thomasdemassari/Documents/Università/BSc/Tesi/Data/")

# Upload delle variabili comuni

# INTEREST RATES
# Euribor - annuale
euribor_a_csv = pd.read_csv("Interest rate/EURIBOR - annual.csv")
euribor_a = ts(euribor_a_csv.iloc[:,1], (1994, 1), (2024, 1), 1)
euribor_a_0623 = euribor_a.loc[2006:2023]
# Euribor - mensile
euribor_m_csv = pd.read_csv("Interest rate/EURIBOR - monthly.csv")
euribor_m = ts(euribor_m_csv.iloc[:,2], (1994, 1), (2024, 2), 12)
euribor_m_0623 = euribor_m.loc["1/2006":"12/2023"]
euribor_m_0623_list = list(euribor_m_0623)
# Libor - annuale
libor_a_csv = pd.read_csv("Interest rate/LIBOR - annual.csv")
libor_a = ts(libor_a_csv.iloc[:,1], (1986, 1), (2023, 1), 1)
libor_a_0623 = libor_a.loc[2006:2023]
# Libor - mensile
libor_m_csv = pd.read_csv("Interest rate/LIBOR - monthly.csv")
libor_m = ts(libor_m_csv.iloc[:,2], (1986, 1), (2023, 12), 12)
libor_m_0623 = libor_m.loc["1/2006":"12/2023"]
libor_m_0623_list = list(libor_m_0623)

# PENDENZA DELLA CURVA DEI RENDIMENTI
# Eurozona 
# Bond 10y
eu_bond10y_csv = pd.read_csv("Interest rate/Eurozone Bond 10y - annual.csv")
eu_bond10y = ts(eu_bond10y_csv.iloc[:, 1], (1970, 1), (2023, 1), 1)
eu_bond10y = eu_bond10y.loc[2006:2023] 
# Pendenza 
eu_slope = eu_bond10y - euribor_a_0623

# UK
# Bond 10y
uk_bond10y_csv = pd.read_csv("Interest rate/UK Bond 10y - annual.csv")
uk_bond10y = ts(uk_bond10y_csv.iloc[:, 1], (1986, 1), (2023, 1), 1)
uk_bond10y = uk_bond10y.loc[2006:2023] 
# Pendenza 
uk_slope = uk_bond10y - libor_a_0623

# STOCK MARKET INDEX
# FTSE 100 (UK)
ftse100_m_csv = pd.read_csv("Macroeconomics conditions/FTSE 100 - monthly.csv")
ftse100_m = ts(ftse100_m_csv.iloc[:,1], (1986, 1), (2024, 2), 12)
ftse100_m_0623 = ftse100_m.loc["1/2006":"12/2023"]
# FTSE MIB (ITA)
ftsemib_m_csv = pd.read_csv("Macroeconomics conditions/FTSE MIB - monthly.csv")
ftsemib_m = ts(ftsemib_m_csv.iloc[:,1], (1997, 12), (2024, 2), 12)
ftsemib_m_0623 = ftsemib_m.loc["1/2006":"12/2023"]
# CAC 40 (FRA)
cac40_m_csv = pd.read_csv("Macroeconomics conditions/CAC 40 - monthly.csv")
cac40_m = ts(cac40_m_csv.iloc[:,4], (1990, 3), (2024, 3), 12)
cac40_m_0623 = cac40_m.loc["1/2006":"12/2023"]
# DAX (GER)
dax_m_csv = pd.read_csv("Macroeconomics conditions/DAX - monthly.csv")
dax_m = ts(dax_m_csv.iloc[:,4], (1988, 1), (2024, 3), 12)
dax_m_0623 = dax_m.loc["1/2006":"12/2023"]
# IBEX 35 (SPA)
ibex35_m_csv = pd.read_csv("Macroeconomics conditions/IBEX 35 - monthly.csv")
ibex35_m = ts(ibex35_m_csv.iloc[:,4], (1993, 8), (2024, 3), 12)
ibex35_m_0623 = ibex35_m.loc["1/2006":"12/2023"]
# AEX (NL)
aex_m_csv = pd.read_csv("Macroeconomics conditions/AEX - monthly.csv")
aex_m = ts(aex_m_csv.iloc[:,4], (1992, 11), (2024, 4), 12)
aex_m_0623 = aex_m.loc["1/2006":"12/2023"]

# Controlli
if len(ftse100_m_0623) == len(ftsemib_m_0623) == len(cac40_m_0623) == len(dax_m_0623) == len(ibex35_m_0623) == len(aex_m_0623) == len(libor_m_0623) == len(euribor_m_0623):
    print(f"I datasets mensili hanno tutti lunghezza pari a {len(libor_m_0623)}")
else:
    raise Exception("I datasets mensili non hanno tutti la stessa lunghezza.")

if len(libor_a_0623) == len(euribor_a_0623):
    print(f"I datasets annuali hanno tutti lunghezza pari a {len(euribor_a_0623)}")
else:
    raise Exception("I datasets annuali non hanno tutti la stessa lunghezza.")

print("All datasets were uploaded correctly.")

# %%
# Creazione del panel
banks = ["1. HSBC", "2. BNP Paribas", "3. Crédit Agricole", "4. Banco Santander", "5. Barclays", "8. Société Générale", "9. Deutsche Bank", "11. Lloyds Banking Group", "12. Intesa Sanpaolo", "13. ING Groep", "14. UniCredit", "15. NatWest Group", "16. Standard Chartered", "18. Banco Bilbao Vizcaya Argentaria"]

banks_acronym = ["hsbc", "bnp", "aca", "sanx", "bar", "gle", "dbk", "lloy", "isp", "inga", "ucg", "nwg", "stan", "bbva"]

nation = ["uk", "fra", "fra", "spa", "uk", "fra", "ger", "uk", "ita", "ned", "ita", "uk", "uk", "spa"]

if len(banks) != len(banks_acronym):
    raise Exception("I vettori banks e banks_acronym hanno lunghezze diverse")

data_fs = pd.DataFrame(columns=["Bank", "ID", "Nation", "Years", "NII_Assets", "OI_Assets", "PROV_Assets", "DepositCentralBank_Assets", "ROA", "InterestRate", "PendenzaYC", "crisis"])
data_sp = pd.DataFrame(columns=["Bank", "ID", "Nation", "Times", "Stockprice", "Stockprice_t0", "Stockprice_t1", "InterestRate", "Market_Index"])

id_fs = list(); id_sp = list(); banks_name_fs = list(); banks_name_sp = list()

# Variabile dummy per la crisi
years_crisis = list(range(2006,2024))
crisis = list()
for k in range(len(years_crisis)):
    if (years_crisis[k] > 2007) and (years_crisis[k] < 2013):
        crisis.append(1)
    else:
        crisis.append(0)

for i in range(len(banks)):
    bank = banks[i]
    bank_acr = banks_acronym[i]

    vars_names = [bank_acr + "_nii", bank_acr + "_oi", bank_acr + "_prov", bank_acr + "_assets", bank_acr + "_dcb", bank_acr + "_roa", 
                  bank_acr + "_stockprice", bank_acr + "_stockprice_t0", bank_acr + "_stockprice_t1"] # Nomi delle variabili da creare
    
    fs_csv = pd.read_csv(f"/Users/thomasdemassari/Documents/Università/BSc/Tesi/Data/Banks/{bank}/Financial statement - summary.csv") # CSV con i dati di bilancio
    sp_csv = pd.read_csv(f"/Users/thomasdemassari/Documents/Università/BSc/Tesi/Data/Banks/{bank}/stockprice.csv") # CSV con i prezzi delle azioni

    nii = ts(fs_csv.iloc[:,1], (2005, 1), (2023, 1), 1) # Net interest income (NII)
    nii_0623 = nii.loc[2006:2023]

    oi = ts(fs_csv.iloc[:,2], (2005, 1), (2023, 1), 1) # Non interest income (OI)
    oi_0623 = oi.loc[2006:2023]

    prov = ts(fs_csv.iloc[:,3], (2005, 1), (2023, 1), 1) # Loan loss provision (PROV)
    prov_0623 = prov.loc[2006:2023]

    assets = ts(fs_csv.iloc[:,4], (2005, 1), (2023, 1), 1) # Total assets (ASSETS)
    assets_0522 = assets.loc[2005:2022]

    dcb = ts(fs_csv.iloc[:,5], (2005, 1), (2023, 1), 1) # Deposit at central bank (DCB)
    dcb_0623 = dcb.loc[2006:2023]

    roa = ts(fs_csv.iloc[:,6], (2005, 1), (2023, 1), 1)  # ROA
    roa_0623 = roa.loc[2006:2023]

    dcb_over_assets = [(dcb_0623.iloc[j]/assets_0522.iloc[j])*100 for j in range(len(dcb_0623))]    # Deposit at central bank / total assets * 100
    nii_over_assets = [(nii_0623.iloc[j]/assets_0522.iloc[j])*100 for j in range(len(nii_0623))]    # Net interest income / total assets * 100
    oi_over_assets = [(oi_0623.iloc[j]/assets_0522.iloc[j])*100 for j in range(len(oi_0623))]       # Non interest income / total assets * 100
    prov_over_assets = [(prov_0623.iloc[j]/assets_0522.iloc[j])*100 for j in range(len(prov_0623))] # Loan loss provision / total assets * 100

    dcb_0623 = ts(dcb_over_assets, (2006, 1), (2023, 1), 1)
    nii_0623 = ts(nii_over_assets, (2006, 1), (2023, 1), 1)
    oi_0623 = ts(oi_over_assets, (2006, 1), (2023, 1), 1)
    prov_0623 = ts(prov_over_assets, (2006, 1), (2023, 1), 1)

    stockprice = ts(sp_csv.iloc[:,4], (2002, 1), (2023,12), 12)
    stockprice = stockprice.loc["1/2006":"12/2023"] # Stock price
    stockprice_t0 = stockprice.loc["1/2006":"11/2023"] # Stock price in t0
    stockprice_t1 = stockprice.loc["2/2006":"12/2023"]# Stock price in t1 

    stockprice_list = list(stockprice)
    stockprice_t0_list = list(stockprice_t0)
    stockprice_t0_list.append("NA")
    stockprice_t1_list = list(stockprice_t1)
    stockprice_t1_list.insert(0, 'NA')

    # Creazione del file
    # Indici delle unità osservate
    id_fs.append([i+1] * (len(nii_0623)))
    id_sp.append([i+1] * (len(stockprice_t1)+1))
    banks_name_fs.append([bank_acr] * (len(nii_0623)))
    banks_name_sp.append([bank_acr] * (len(stockprice_t1)+1))
    # Filtro del tasso di interesse per nazionalità
    if nation[i] == "uk":
        int_rate_a = libor_a_0623
        int_rate_m = libor_m_0623_list
        pendenza_yc = uk_slope
    else:
        int_rate_a = euribor_a_0623
        int_rate_m = euribor_m_0623_list
        pendenza_yc = eu_slope
    # Filtro dell'indice di mercato sulla base della nazionalità
    if nation[i] == "ita":
        market_index = ftsemib_m_0623
    else:
        if nation[i] == "uk":
            market_index = ftse100_m_0623
        else:
            if nation[i] == "fra":
                market_index = cac40_m_0623
            else:
                if nation[i] == "spa":
                    market_index = ibex35_m_0623
                else:
                    if nation[i] == "ger":
                        market_index = dax_m_0623
                    else:
                        if nation[i] == "ned":
                            market_index = aex_m_0623

    # Conversione in lista delle serie
    int_rate_m_list = list(int_rate_m)
    market_index_list = list(market_index)

    # Lunghezza della serie storica
    years = list(range(2006, 2024))
    times = list(stockprice.index)
    # Creazione dei datasets
    df_fs_tmp = pd.DataFrame({
        "Bank": banks_name_fs[i],
        "ID": id_fs[i],
        "Nation": nation[i],
        "Years": years,
        "NII_Assets": nii_0623, 
        "OI_Assets": oi_0623,
        "PROV_Assets": prov_0623,
        "DepositCentralBank_Assets": dcb_0623, 
        "ROA": roa_0623,
        "InterestRate": int_rate_a,
        "PendenzaYC": list(pendenza_yc),
        "crisis": crisis
    })
    data_fs = pd.concat([data_fs, df_fs_tmp], ignore_index=True)

    df_sp_tmp = pd.DataFrame({
        "Bank": banks_name_sp[i],
        "ID": id_sp[i],
        "Nation": nation[i],
        "Times": times,
        "Stockprice": stockprice_list,
        "Stockprice_t0": stockprice_t0_list, 
        "Stockprice_t1": stockprice_t1_list, 
        "InterestRate": int_rate_m_list,
        "Market_Index": market_index_list
    })
    data_sp = pd.concat([data_sp, df_sp_tmp], ignore_index=True)

data_fs.to_csv("/Users/thomasdemassari/Documents/Università/BSc/Tesi/Data/financial_statement_panel.csv", index=False)  
data_sp.to_csv("/Users/thomasdemassari/Documents/Università/BSc/Tesi/Data/stock_price_panel.csv", index=False)

# Controlli
if len(stockprice) == len(libor_m_0623):
    print(f"I datasets mensili hanno tutti lunghezza pari a {len(libor_m_0623)}")
else:
    raise Exception("I datasets mensili non hanno tutti la stessa lunghezza.")

if len(nii_0623) == len(nii_0623) == len(prov_0623) == len(assets_0522) == len(dcb_0623) == len(roa_0623) == len(euribor_a_0623):
    print(f"I datasets annuali hanno tutti lunghezza pari a {len(euribor_a_0623)}")
else:
    raise Exception("I datasets annuali non hanno tutti la stessa lunghezza.")
print("Tutti i file sono stati creati correttamente")


