#!/usr/bin/env python
# coding: utf-8

# In[1]:


import requests
import pandas as pd
import urllib3
from lxml import html
from lxml import etree
import json
from bs4 import BeautifulSoup


# In[3]:


listing_path = "~/data/ListingLinks.csv"
listing_df = pd.read_csv(listing_path)
listing_df = listing_df.drop_duplicates(subset=['Link', 'Price'])
urls = list(listing_df["Link"])
price = list(listing_df["Price"])
condos_data = []
all_amenity_lst = []
print(len(full_urls), len(full_price))


# In[10]:


with requests.Session() as s:
    for j in range(len(urls)):
        url = urls[j]
        if ("unit" in url): # Check whether it's condo or not
            soup = BeautifulSoup(s.get(url, headers = {
                'User-agent':'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/95.0.4638.69 Safari/537.36',
                'origin':'https://condos.ca',
                "referer":'https://condos.ca/',
            }).text, 'html.parser')

            address = soup.find_all("h1", {"class": "styles___Title-sc-ka5njm-6 ccBSix"})[0].text
            tax_block = soup.find_all("div", {"class": "styles___RightBlock-sc-ka5njm-14 gGsbwf"})[0]
            tax_2021 = tax_block.find_all("span", {"class":"styles___BlurCont-sc-qq1hs5-0"})
            if (len(tax_2021) != 0):
                tax_2021 = tax_2021[0].text
            else:
                tax_2021 = "NA"
            
            basic_info = soup.find_all("div", {"class": 
                                               "styles___NoMarginRow-sc-146e13k-1 styles___ListingDetailKeyContainer-sc-14y6qt-0 gkHXVW ftRoLh ZtOZv"})
            info_lst = basic_info[0].find_all("div", {"class": 
                                            "styles___BlockContainer-sc-1cv9cf1-1 eWVqJL styles___Container-sc-1cv9cf1-0 TitleValueBlockV2___Styled_Container-sc-xxjho6-0 QcERn styles___TitleValueBlock-sc-14y6qt-2 fEpIdS"})
            expose = "NA"
            maintenance_fees = "NA"
            possession = "NA"
            age_of_building = "NA"
            outdoor_space = "NA"
            locker = "NA"
            for info in info_lst:
                label_value_pair = info.find_all("div")
                label = label_value_pair[0].text
                if (label == "Exposure"):
                    expose = label_value_pair[1].text
                if (label == "Maintenance fees"):
                    maintenance_fees = label_value_pair[1].text
                if (label == "Exposure"):
                    expose = label_value_pair[1].text
                if (label == "Maintenance fees"):
                    maintenance_fees = label_value_pair[1].text
                if (label == "Possession"):
                    possession = label_value_pair[1].text
                if (label == "Age of building"):
                    age_of_building = label_value_pair[1].text
                if (label == "Outdoor space"):
                    outdoor_space = label_value_pair[1].text
                if (label == "Locker"):
                    locker = label_value_pair[1].text
                
            
            
            property_details = soup.find_all("div", {"class": "styles___NoMarginRow-sc-146e13k-1 gkHXVW ZtOZv"})[0]
            property_lst = property_details.find_all("div")
            heating_type = "NA"
            parking_type = "NA"
            property_type = "NA"
            area = "NA"
            ensuite_laundry = "NA"
            corp_num = "NA"
            size_range = "NA"
            for i in range(len(property_lst)):
                if (property_lst[i].text == "Heating type:"):
                    heating_type = property_lst[i+1].text
                if (property_lst[i].text == "Parking type:"):
                    parking_type = property_lst[i+1].text
                if (property_lst[i].text == "Property type:"):
                    property_type = property_lst[i+1].text
                if (property_lst[i].text == "Ensuite laundry:"):
                    ensuite_laundry = property_lst[i+1].text
                if (property_lst[i].text == "Area:"):
                    area = property_lst[i+1].text
                if (property_lst[i].text == "Corp #:"):
                    corp_num = property_lst[i+1].text
                if (property_lst[i].text == "Size:"):
                    size_range = property_lst[i+1].text
                
                
            window_script = soup.find_all('script')[1]
            soup_str = str(soup)
            query_str = soup_str[soup_str.find("window.__REACT_QUERY_STATE__"):]
            query_str = query_str[query_str.find("{"):query_str.find("</script>")-11]
            bed_string = query_str[query_str.find("bed_type")+11:]
            num_of_bed = bed_string[:bed_string.find('",')]
            num_of_bath = query_str[query_str.find("bathrooms")+11:query_str.find("bathrooms")+12]
            num_of_parking = query_str[query_str.find("parking_spots")+15: query_str.find("parking_spots")+16]
            size_string = query_str[query_str.find('"sqft":')+7:]
            actual_size = size_string[:size_string.find(',')]

            amenities = query_str[query_str.find('"amenities":['):]
            amenities = amenities[:amenities.find("]")+1]

            json_nodes = json.loads("{" + amenities +"}")
            amenity_lst = []
            for node in json_nodes["amenities"]:
                amenity_lst.append(node["name"])
                all_amenity_lst.append(node["name"])

            condo_data = [address, price[j], num_of_bed,num_of_bath, num_of_parking, tax_2021, actual_size,
                          expose, maintenance_fees, possession, age_of_building, outdoor_space, locker,
                          heating_type, parking_type, property_type, area, ensuite_laundry, corp_num, size_range,
                         amenity_lst]
            condos_data.append(condo_data)


# In[59]:


full_df = pd.DataFrame(condos_data)
full_df.columns = ["address", "price", "num_of_bed","num_of_bath", "num_of_parking", "tax_2021", "actual_size",
                          "expose", "maintenance_fees", "possession", "age_of_building", "outdoor_space", "locker",
                          "heating_type", "parking_type", "property_type", "area", "ensuite_laundry", "corp_num", "size_range",
                         "amenity_lst"]
full_df.head()


# In[32]:


all_amenity_set = set(all_amenity_lst)
print(all_amenity_set)


# In[61]:


file_name = "~/data/condos_info.csv"
full_df.to_csv(file_name, encoding='utf-8', index=False)

