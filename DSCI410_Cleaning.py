"""
Cleaning program for DSCI 410 Data
"""

import pandas as pd

def select_dates(df, start, end, filename):
    df['calltime'] = pd.to_datetime(df['calltime'])
    start = pd.to_datetime(start)
    end = pd.to_datetime(end)
    mask = (df['calltime'] >= start) & (df['calltime'] <= end)
    df = df.loc[mask]
    df.to_csv(f"{filename}.csv", index=False)

def convert_signs(df):
    df['agency'] = [""]*len(df)
    CAHOOTS_Signs = ["CAHOT","CAHOOT","CAH",r"/[0-9]J[0-9]{2}/"]
    for index, row in df.iterrows():
        if row['primeunit'] not in CAHOOTS_Signs:
            df.at[index, 'agency'] = 'EPD'
        else:
            df.at[index, 'agency'] = 'CAHOOTS'
    return df

EPD_2024 = convert_signs(pd.read_csv("class_data_2024.csv", low_memory=False).drop_duplicates())
EPD_2025 = convert_signs(pd.read_csv("class_data_2025.csv", low_memory=False).drop_duplicates())

select_dates(EPD_2024, "2024-03-24", "2024-04-21", "EPD_2024_Clean")
select_dates(EPD_2025, "2025-03-24", "2025-04-21", "EPD_2025_Clean")

def clean_EMS(df, filename):
    df = df[df['City'] == 'EUG']
    df.to_csv(f"{filename}.csv", index=False)

EMS_2024 = pd.read_csv("EMS_Dispatch_Log_2024.csv").drop_duplicates()
EMS_2025 = pd.read_csv("EMS_Dispatch_Log_2025.csv").drop_duplicates()

clean_EMS(EMS_2024, "EMS_2024_Clean")
clean_EMS(EMS_2025, "EMS_2025_Clean")

