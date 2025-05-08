"""
Dispatch_Scrape.py
Script for scraping either Eugene Police or EMS public dispatch logs, one date at a time
Created by Jacob Dirkx for the University of Oregon's DSCI 410L/Applied Data Science for Social Justice Class
Lasted updated April 2025
"""

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import Select
from time import sleep
from datetime import datetime, timedelta
import numpy as np
import pandas as pd

def write_to_csv(file_name, df):
    "writes table_data to file_name.csv"

    df.to_csv(f"{file_name}.csv", index=False, encoding="utf-8")
    print(f"Data has been written to '{file_name}.csv'.")

def scrape(date, link, mode, df):
    "Takes single date (string) and returns dispatch logs from that date (df initiation only for smoothness)"
    "Returns df for iteration"
    
    #set up selenium for Chrome
    options = Options()
    options.add_argument("--window-size=1920,1080")
    driver = webdriver.Chrome(options=options)
    try:
        #open the page and get the body
        driver.get(link)
        WebDriverWait(driver, 20).until(
            EC.visibility_of_element_located((By.TAG_NAME, "body"))
        )

        if mode == 'EPD':
            #wait for date from field
            sleep(2)
            WebDriverWait(driver, 20).until(
                EC.element_to_be_clickable((By.ID, "DateFrom"))
            )
            #set dates
            driver.find_element(By.ID, "DateFrom").clear()
            driver.find_element(By.ID, "DateFrom").send_keys(date)
            sleep(np.random.randint(1,3))
            driver.find_element(By.ID, "DateThrough").clear()
            driver.find_element(By.ID, "DateThrough").send_keys(date)
            sleep(np.random.randint(1,3))

            #wait and hit submit
            WebDriverWait(driver, 20).until(
                EC.element_to_be_clickable((By.XPATH, '//input[@type="submit" and @value="Search"]'))
            ).click()
            sleep(np.random.randint(1,3))

            #wait for new page and the table
            WebDriverWait(driver, 60).until(
                EC.presence_of_element_located((By.ID, "calls"))
            )
        
        elif mode == 'EMS':

            #extract date values
            date = datetime.strptime(date, "%m/%d/%Y")
            month_val = date.month - 1
            year_val = date.year
            day_val = date.day

            sleep(2)
            WebDriverWait(driver, 20).until(
                EC.visibility_of_element_located((By.CLASS_NAME, "ui-datepicker-title"))
            )

            # Select the month
            month_dropdown = driver.find_element(By.CLASS_NAME, "ui-datepicker-month")
            Select(month_dropdown).select_by_value(str(month_val))

            # Select the year
            year_dropdown = driver.find_element(By.CLASS_NAME, "ui-datepicker-year")
            Select(year_dropdown).select_by_value(str(year_val))

            #Select the day
            WebDriverWait(driver, 20).until(EC.visibility_of_element_located((By.CLASS_NAME, "ui-datepicker-calendar")))
            day = driver.find_element(By.XPATH, f'//a[text()="{day_val}" and contains(@class, "ui-state-default")]')
            day.click()
            
            #wait for the table
            WebDriverWait(driver, 60).until(
                EC.presence_of_element_located((By.ID, "calls"))
            )

        #get the table and headers
        table = driver.find_element(By.ID, "calls")
        thead = table.find_element(By.TAG_NAME, "thead")
        headers = thead.find_elements(By.TAG_NAME, "th")
        header_texts = [header.text for header in headers]
        table_data = [header_texts]

        #get all the data from the rows
        rows = table.find_elements(By.TAG_NAME, "tr")
        for row in rows:
            columns = row.find_elements(By.TAG_NAME, "td")
            row_data = [column.text for column in columns]
            if row_data:
                table_data.append(row_data)
        
        #create or concatenate dataframe
        headers = [s.strip() for s in table_data[0]]
        rows = table_data[1:]
        new_df = pd.DataFrame(rows, columns=headers)

        # Add date column to the new day's dataE
        new_df['Date'] = date

        # Then concatenate
        if df is not None and not df.empty:
            df = pd.concat([df, new_df], ignore_index=True)
        else:
            df = new_df
        
        return df

    finally:
        driver.quit()

def main():

    df = None
    print("Thanks for using this Eugene public service dispatch scraping program!")

    #modality
    mode = None
    mode = input("Please enter exactly which service you'd like to look at ('EPD' or 'EMS'):    ")
    while mode not in ['EPD', 'EMS']:
        print("Invalid organization, please reenter")
        mode = input("Please enter exactly which service you'd like to look at ('EPD' or 'EMS'):    ")

    #setting dates
    start = input("Please enter a start date in this format: MM/DD/YYYY:    ")
    start_dt = datetime.strptime(start, "%m/%d/%Y")
    end = input("Please enter an end date in this format: MM/DD/YYYY:   ")
    end_dt = datetime.strptime(end, "%m/%d/%Y")
    current_dt = start_dt

    #iterate to compile dataframe
    if mode == 'EPD':
        link = "https://coeapps.eugene-or.gov/epddispatchlog/Home/Search"
        while current_dt <= end_dt:
            date = datetime.strftime(current_dt, "%m/%d/%Y")
            df = scrape(date, link, mode, df)
            current_dt += timedelta(days=1)
    elif mode == 'EMS':
        link = "https://coeapps.eugene-or.gov/ruralfirecad"
        while current_dt <= end_dt:
            date = datetime.strftime(current_dt, "%m/%d/%Y")
            df = scrape(date, link, mode, df)
            current_dt += timedelta(days=1)
    
    #writing to csv
    file_name = f"{mode}_Dispatch_Log"
    write_to_csv(file_name, df)
    print("All done scraping! Check cwd for your csv.")

if __name__ == "__main__":
    main()