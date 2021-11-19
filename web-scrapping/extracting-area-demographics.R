library(tidyverse)
library(magrittr)
library(jsonlite)

area_and_url <- read.csv("area-url.csv")

extract_area <- function (area_name, link) {
  
  data <- 
    jsonlite::fromJSON(link) %>%
    .[["data"]] %>% .[["attributes"]] 
  
  entry <- matrix(area_name) 
  colnames(entry) <- "area"
  
  attributes <- names(data)
  
  for (attr in attributes){
    
    values <- data[[attr]][["variables"]][["value"]] %>%
      matrix(nrow=1) 
    
    colnames(values) <- data[[attr]][["variables"]][["variable"]]
    
    entry %<>% 
      cbind(values)
  }
  
  return(entry)
}

full_table<-extract_area(area_and_url$area.name[1], area_and_url$url[1])

for(i in 2:22) {
  new_row <- extract_area(area_and_url$area.name[i], area_and_url$url[i]) 
  full_table <- rbind(full_table,new_row) 
}

write.csv(full_table, "demographics.csv")

