# %% [markdown]
# # Bachelor Thesis - Dataset Manipulation

# %% [markdown]
# ## Libraries

# %%
import os
import csv
import pandas as pd
import numpy as np

from datetime import datetime

import warnings
warnings.filterwarnings("ignore", message="kurtosistest only valid for n>=20")

now = datetime.now().strftime('%Y-%m-%d at %H.%M')
print(f"Last update: {datetime.now().strftime('%Y-%m-%d at %H.%M')}")

# %% [markdown]
# ## Functions

# %% [markdown]
# ### Set working directory
# Function to set the working directory (and simultaneously verify its existence). The input only requires the folder path.
# 
# Example: *setwd(path)*

# %%
def setwd(path):
    try:
        import os
        directory = path

        # Verify that the folder path exists
        if os.path.exists(directory):
            # Set the current working directory
            os.chdir(directory)
        else:
            print(f"\nThe folder path '{directory}' does not exist.")
    except Exception:
        print("\nError setting the working directory.")

# %% [markdown]
# ### Time Series
# Function to convert a dataset into a time series (R-style). The arguments are:
# - data: values of the time series;
# - start: tuple with (first year, first sub-period). If the series is annual, you still need to enter (x, 1).
# - end: tuple with (last year, last sub-period). If the series is annual, you still need to enter (x, 1).
# - frequency: sub-periods of the various years (e.g., semi-annual = 2, quarterly = 4, monthly = 12). If the series is annual, you still need to enter frequency = 1.
# 
# The function uses the Pandas library and returns the time series in Pandas Series format (*class 'pandas.core.series.Series'*).
# 
# Note: The function only works with annual, semi-annual, quarterly, four-monthly, and monthly time series. It does not work with daily time series.
# 
# Example to create a time series with values contained in *dataset* for the period 2024-2025 (monthly basis, starting from January 2024):     
# ```python   
# dataset = list(range(1, 25))   
# timeseries = ts(dataset, (2024, 1), (2025, 12), 12)      
# print(timeseries)   
# ```
# 
# Note: To do the equivalent of the *window()* function in R, simply use the *.iloc["lower limit" : "upper limit"]* method from the Pandas library.

# %%
def ts(data, start, end, frequency):
    try:
        first_year = start[0]
        first_subperiod = start[1]
        last_year = end[0]
        last_subperiod = end[1]

        years = list(range(first_year, (last_year+1)))
        times = list()

        if frequency == 1:                                                    # Annual
            times.append(years)
        else:
            if frequency == 2:                                                # Biannual 
                for i in range(len(years)):
                    times.append(f"S1/{years[i]}")
                    times.append(f"S2/{years[i]}")
            else: 
                if frequency == 3:                                            # Four-month
                    for i in range(len(years)):
                        times.append(f"Q1/{years[i]}")
                        times.append(f"Q2/{years[i]}")
                        times.append(f"Q3/{years[i]}")
                else:
                    if frequency == 4:                                        # Quarterly
                        for i in range(len(years)):
                            times.append(f"Q1/{years[i]}")
                            times.append(f"Q2/{years[i]}")
                            times.append(f"Q3/{years[i]}")
                            times.append(f"Q4/{years[i]}")
                    else:
                        if frequency == 12:                                    # Monthly
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
# ### Function to Extract Financial Data
# This function takes as input the financial statements downloaded from Orbis in xlsx format and extracts the ROA, net interest income, non-interest income, total assets, and net interest margin. These data are then saved in a new CSV file, which is stored in the same folder where the original financial statements were saved.
# 
# The parameter *print_report*, set to *True*, is used simply to print a message if the function completes successfully. By default, it is set to *False*.
# 
# Note: The input requires the path to the financial statements file, not the file name (which should be "Financial statement.xlsx").
# 
# Example:
# ```python
# getmydata("/Users/thomasdemassari/Documents/University/BSc/Thesis/Data/Banks/14. Unicredit")
# ```

# %%
def getmydata(path, print_report = False):
    try:
        # Reading the dataset
        balance_sheet = pd.read_excel(f"{path}/Financial statement.xlsx", sheet_name="Stato patrimoniale")
        income_statement = pd.read_excel(f"{path}/Financial statement.xlsx", sheet_name="Conto economico")
        indices = pd.read_excel(f"{path}/Financial statement.xlsx", sheet_name="Indici")
        
        namebanks = path.split("/")

        # Retrieve the Data (and Reverse It; Datasets are from 2023 - 2005)
        nii = income_statement.iloc[11][::-1]                                     # Net interest income
        oi = income_statement.iloc[14][::-1]                                      # Non interest income
        prov = income_statement.iloc[20][::-1]                                    # Loan loss provision
        assets = balance_sheet.iloc[28][::-1]                                     # Total assets
        centralbank = balance_sheet.iloc[25][::-1]                                # Deposit at Central Bank
        roa = indices.iloc[42][::-1]                                              # ROA
        int_margin = indices.iloc[36][::-1]                                       # Net interest margin

        years = list(range(2005, 2024))

        # Creating a new dataset
        os.chdir(path)
        dataset = np.array(list(zip(years, nii, oi, prov, assets, centralbank, roa, int_margin)))

        # Creating a csv
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
# ### Function to Set Significance Levels for P-Values
# 
# This function returns a list of symbols (R-style) to visually assess the significance of parameters. It is useful when creating a summary CSV file with the parameters of the estimated models.
# 
# Legend:         
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
# ### Function to Transform a Dataset from Monthly to Annual
# This function converts a time series from a monthly basis to an annual basis (annual average). The required parameters are:
# - *path*: the path (including file name) to the file containing the daily time series.
# - *col_date*: the number (Python notation) of the column containing the dates.
# - *col_number*: the number (Python notation) of the column containing the values.
# - *year_index*: a tuple containing the first and last+1 position of the year in the date string (e.g., if the date is 02/2002, the tuple for year_index will be (3, 7)).
# - *years_range*: a tuple containing the first year and the last year+1 of the time series (e.g., if the time series ranges from 2000 to 2005, the tuple for years_range will be (2000, 2006)).
# - *new_path*: the new path (including file name) where the new file will be created.
# - *reverse*: a boolean condition to reverse the dataset (e.g., the last date becomes the first, with the corresponding values). Default is set to *False*.

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

        #Creating a new CSV
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
# ### Function to Transform a Dataset from Daily to Monthly
# This function converts a time series from a daily basis to a monthly basis (monthly average). The required parameters are:
# - *path*: the path (including file name) to the file containing the daily time series.
# - *col_date*: the number (Python notation) of the column containing the dates.
# - *col_number*: the number (Python notation) of the column containing the values.
# - *month_index*: a tuple containing the first and last+1 position of the month in the date string (e.g., if the date is 02/2002, the tuple for month_index will be (0, 2)).
# - *year_index*: a tuple containing the first and last+1 position of the year in the date string (e.g., if the date is 02/2002, the tuple for year_index will be (3, 7)).
# - *years_range*: a tuple containing the first year and the last year+1 of the time series (e.g., if the time series ranges from 2000 to 2005, the tuple for years_range will be (2000, 2006)).
# - *new_path*: the new path (including file name) where the new file will be created.
# - *reverse*: a boolean condition to reverse the dataset (e.g., the last date becomes the first, with the corresponding values). Default is set to *False*.

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

        #Creating the new CSV
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
# ## Dataset Management

# %% [markdown]
# ### Dataset Manipulation

# %%
# Retrieve the Financial Data I Need Using the Function Described Above
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
# ### Creating the panel

# %%
setwd("/Users/thomasdemassari/Documents/Università/BSc/Tesi/Data/")

# Upload of Common Variables

# INTEREST RATES
# Euribor - annual
euribor_a_csv = pd.read_csv("Interest rate/EURIBOR - annual.csv")
euribor_a = ts(euribor_a_csv.iloc[:,1], (1994, 1), (2024, 1), 1)
euribor_a_0623 = euribor_a.loc[2006:2023]
# Euribor - monthly
euribor_m_csv = pd.read_csv("Interest rate/EURIBOR - monthly.csv")
euribor_m = ts(euribor_m_csv.iloc[:,2], (1994, 1), (2024, 2), 12)
euribor_m_0623 = euribor_m.loc["1/2006":"12/2023"]
euribor_m_0623_list = list(euribor_m_0623)
# Libor - annual
libor_a_csv = pd.read_csv("Interest rate/LIBOR - annual.csv")
libor_a = ts(libor_a_csv.iloc[:,1], (1986, 1), (2023, 1), 1)
libor_a_0623 = libor_a.loc[2006:2023]
# Libor - monthly
libor_m_csv = pd.read_csv("Interest rate/LIBOR - monthly.csv")
libor_m = ts(libor_m_csv.iloc[:,2], (1986, 1), (2023, 12), 12)
libor_m_0623 = libor_m.loc["1/2006":"12/2023"]
libor_m_0623_list = list(libor_m_0623)

# YIELD CURVE SLOPE
# Eurozone
# Bond 10y
eu_bond10y_csv = pd.read_csv("Interest rate/Eurozone Bond 10y - annual.csv")
eu_bond10y = ts(eu_bond10y_csv.iloc[:, 1], (1970, 1), (2023, 1), 1)
eu_bond10y = eu_bond10y.loc[2006:2023] 
# Slope 
eu_slope = eu_bond10y - euribor_a_0623

# UK
# Bond 10y
uk_bond10y_csv = pd.read_csv("Interest rate/UK Bond 10y - annual.csv")
uk_bond10y = ts(uk_bond10y_csv.iloc[:, 1], (1986, 1), (2023, 1), 1)
uk_bond10y = uk_bond10y.loc[2006:2023] 
# Slope 
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

# Controls
if len(ftse100_m_0623) == len(ftsemib_m_0623) == len(cac40_m_0623) == len(dax_m_0623) == len(ibex35_m_0623) == len(aex_m_0623) == len(libor_m_0623) == len(euribor_m_0623):
    print(f"Monthly datasets all have a length of {len(libor_m_0623)}")
else:
    raise Exception("Monthly datasets do not all have the same length.")

if len(libor_a_0623) == len(euribor_a_0623):
    print(f"Annual datasets all have a length of {len(euribor_a_0623)}")
else:
    raise Exception("Annual datasets do not all have the same length.")

print("All datasets were uploaded correctly.")

# %%
# Creating the panel
banks = ["1. HSBC", "2. BNP Paribas", "3. Crédit Agricole", "4. Banco Santander", "5. Barclays", "8. Société Générale", "9. Deutsche Bank", "11. Lloyds Banking Group", "12. Intesa Sanpaolo", "13. ING Groep", "14. UniCredit", "15. NatWest Group", "16. Standard Chartered", "18. Banco Bilbao Vizcaya Argentaria"]

banks_acronym = ["hsbc", "bnp", "aca", "sanx", "bar", "gle", "dbk", "lloy", "isp", "inga", "ucg", "nwg", "stan", "bbva"]

nation = ["uk", "fra", "fra", "spa", "uk", "fra", "ger", "uk", "ita", "ned", "ita", "uk", "uk", "spa"]

if len(banks) != len(banks_acronym):
    raise Exception("I vettori banks e banks_acronym hanno lunghezze diverse")

data_fs = pd.DataFrame(columns=["Bank", "ID", "Nation", "Years", "NII_Assets", "OI_Assets", "PROV_Assets", "DepositCentralBank_Assets", "ROA", "InterestRate", "PendenzaYC", "crisis"])
data_sp = pd.DataFrame(columns=["Bank", "ID", "Nation", "Times", "Stockprice", "Stockprice_t0", "Stockprice_t1", "InterestRate", "Market_Index"])

id_fs = list(); id_sp = list(); banks_name_fs = list(); banks_name_sp = list()

# Dummy variable for Crisis (2008-2012)
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
    
    fs_csv = pd.read_csv(f"/Users/thomasdemassari/Documents/Università/BSc/Tesi/Data/Banks/{bank}/Financial statement - summary.csv") 
    sp_csv = pd.read_csv(f"/Users/thomasdemassari/Documents/Università/BSc/Tesi/Data/Banks/{bank}/stockprice.csv") 

    nii = ts(fs_csv.iloc[:,1], (2005, 1), (2023, 1), 1)                                            # Net interest income (NII)
    nii_0623 = nii.loc[2006:2023]

    oi = ts(fs_csv.iloc[:,2], (2005, 1), (2023, 1), 1)                                             # Non interest income (OI)
    oi_0623 = oi.loc[2006:2023]

    prov = ts(fs_csv.iloc[:,3], (2005, 1), (2023, 1), 1)                                            # Loan loss provision (PROV)
    prov_0623 = prov.loc[2006:2023]

    assets = ts(fs_csv.iloc[:,4], (2005, 1), (2023, 1), 1)                                          # Total assets (ASSETS)
    assets_0522 = assets.loc[2005:2022]

    dcb = ts(fs_csv.iloc[:,5], (2005, 1), (2023, 1), 1)                                             # Deposit at central bank (DCB)
    dcb_0623 = dcb.loc[2006:2023]

    roa = ts(fs_csv.iloc[:,6], (2005, 1), (2023, 1), 1)                                             # ROA
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

    # File Creation
    # Indices of Observed Units
    id_fs.append([i+1] * (len(nii_0623)))
    id_sp.append([i+1] * (len(stockprice_t1)+1))
    banks_name_fs.append([bank_acr] * (len(nii_0623)))
    banks_name_sp.append([bank_acr] * (len(stockprice_t1)+1))
    # Filtering Interest Rate by Nationality
    if nation[i] == "uk":
        int_rate_a = libor_a_0623
        int_rate_m = libor_m_0623_list
        pendenza_yc = uk_slope
    else:
        int_rate_a = euribor_a_0623
        int_rate_m = euribor_m_0623_list
        pendenza_yc = eu_slope
    # Filtering Market Index Based on Nationality
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

    # Conversion of Series to List
    int_rate_m_list = list(int_rate_m)
    market_index_list = list(market_index)

    # Length of the Time Series
    years = list(range(2006, 2024))
    times = list(stockprice.index)
    # Creating the Datasets
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

# Controls
if len(stockprice) == len(libor_m_0623):
    print(f"Monthly datasets all have a length of {len(libor_m_0623)}")
else:
    raise Exception("Monthly datasets do not all have the same length.")

if len(nii_0623) == len(nii_0623) == len(prov_0623) == len(assets_0522) == len(dcb_0623) == len(roa_0623) == len(euribor_a_0623):
    print(f"Annual datasets all have a length of {len(euribor_a_0623)}")
else:
    raise Exception("Annual datasets do not all have the same length.")
print("All datasets were uploaded correctly.")


