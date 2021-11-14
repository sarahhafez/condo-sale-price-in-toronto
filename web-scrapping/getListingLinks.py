import requests
from bs4 import BeautifulSoup
from time import sleep
import pandas as pd   

s = requests.session()
HEADERS = {
    'User-agent':'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36',
    'origin':'https://condos.ca',
    'referer' :'https://condos.ca/'
    }
login_payload = { 
    "email": "",
    "hasMobileApp": False,
    "password": ""
    }
log_req = s.post('https://api.condos.ca/v1/users/login', headers = HEADERS, data = login_payload)
# print(log_req.status_code)
# cookies = log_req.cookies

# The neighbourhoods id, for example, 738 means Alexander Park
neighbourhood_id = [758,759,750,741,756,749,752,744,745,761,755,757,753,743,754,742,740,746,751,747,748,760]

Links = []
Prices = []

def getLinks(url):
    soup = BeautifulSoup(s.get(url, headers=HEADERS).text, 'html.parser')
    listing_num = soup.select('span[class*="styles___ListingCount"]')[0].text
    if listing_num == "500+":
        page_num = 11
    else:
        page_num = int(listing_num)//50+1
    # print(page_num)
    for page in range(1, page_num+1):
        url_page = url+"&page="+str(page)
        soup_page = BeautifulSoup(s.get(url_page, headers=HEADERS).text, 'html.parser')
        for link in soup.select('a[class*="styles___Link"]'):
            if link.has_attr('href'):
                Links.append("https://condos.ca"+link['href'])
        for price in soup.select('div[class*="styles___AskingPrice"]'):
                Prices.append(price.text)
    sleep(0.1)

for neighbourhood in neighbourhood_id:
    url = "https://condos.ca/toronto/condos-for-sale?mode=Sold&end_date_unix=relative,-365&neighbourhood_id="+str(neighbourhood)
    # [750,749,752,753,754,746,747] these areas have more than 500+ listings but the website only shows the
    # the lastest 501. So I split the request by bedrooms number. 
    if neighbourhood in [750,749,752,753,754,746,747]:
        beds_list = ["0.1-0.1","1-1","1.1-1.9","2-2","2.1-2.9","3-99"]
        for beds in beds_list:
            url_bed = url+"&beds="+beds
            getLinks(url_bed)
    else:
        getLinks(url)

# print(len(Links))
# print(len(Prices))
# print(Links)
# print(Prices)
i=1
newPrices = []
for price in Prices:
    if i%2==1:newPrices.append(price.split(" ")[0])
    i+=1
data ={"Link":Links,"Price":newPrices}
df = pd.DataFrame(data) 
# print(df)
df.to_csv("ListingLinks.csv")


    
