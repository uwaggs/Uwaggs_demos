import requests
import pandas as pd
from bs4 import BeautifulSoup

# script to grab premier league standings by match week 

def get_html_tables(url):
    response = requests.get(url)
    if response.status_code != 200:
        print(f"Failed to retrieve the webpage: {response.status_code}")
        return
    
    soup = BeautifulSoup(response.text, 'html.parser')
    tables = soup.find_all('table')

    df = pd.read_html(str(tables[3]))[0]

    return df

tables = []

for i in range(1,39):
    link = "https://www.worldfootball.net/schedule/eng-premier-league-2020-2021-spieltag/" + str(i) + "/"
    table = get_html_tables(link)
    table['week'] = i
    tables.append(table)

final_df = pd.concat(tables, ignore_index=True)

final_df.to_csv("2020-2021_premier_league_table.csv")
