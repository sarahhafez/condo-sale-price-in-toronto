library(rvest)
library(tidyverse)

csv_path <- ""

scrape_condos_data_from_url <- function(urls) {
  number_of_condos <- length(urls)
  condos <- tibble("id"=numeric(),
                   "number_of_bed"=numeric(),
                   "number_of_den"=numeric(),
                   "number_of_bath"=numeric(),
                   "number_of_parking"=numeric(),
                   "actual_size"=numeric(),
                   "maintenance_fees"=numeric(),
                   "exposure"=numeric(),
                   "possession"=numeric(),
                   "age_of_building"=numeric(),
                   "price/sqft"=numeric(),
                   "outdoor_space"=numeric(),
                   "locker"=numeric(),
                   "heat_type"=numeric(),
                   "")
  
  for (i in 1:number_of_condos) {
    
  } 
  
  
  return(condos)
}

mock_url_lst <- c("https://condos.ca/toronto/the-canary-district-455-front-st-e/unit-S105-C5421300", 
                  "https://condos.ca/toronto/fashion-house-lofts-560-king-st-w-461-adelaide-st-w/unit-538-C5417879",
                  "https://condos.ca/toronto/the-regency-yorkville-68-yorkville-ave/unit-705-C5407934")
