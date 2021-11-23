#libraries
library(tidyverse)
library(magrittr)
library(forcats)


#assume wd is in the project directory
demographics <- read.csv("data/demographics.csv")
condos <- read.csv("data/clean_condos_info.csv")

condos %<>%
  mutate(area=location_area) %>%
  select(-c("tax_2021", #taxes are dependent on property price
            "amenity_lst", #already extracted relevant data in other columns
           "size_range", #created a size_range_lvl variable instead
           "address", #not relevant for regression
           "corp_num", #??
           "location_area" #changed name to area
           ))

price_and_size <- condos %>%
  select(c("price", "actual_size","area")) %>%
  drop_na()


#order of area by the average condo price
areas_ordered <- price_and_size %>%
  group_by(area) %>%
  dplyr::summarise(mean = mean(price, na.rm=TRUE)) %>%
  arrange(desc(mean)) %>%
  .[['area']]



##Graph 1: Seeing the relationship between price and size in the most and least expensive areas
set.seed(6)
random_area <- c(areas_ordered[1], areas_ordered[2], areas_ordered[3], areas_ordered[19], areas_ordered[20], areas_ordered[21])


price_and_size %>%
  filter(area %in% random_area)  %>%
  ggplot(aes(y = price, x = actual_size, colour =as.factor(area), group=area)) +
  scale_color_brewer(palette ="Spectral", name="Area(most expensive to least)", breaks=random_area) +
  # scale_color_paletteer_d(ggsci) + 
  geom_smooth(method = "lm", se= F) +
  labs(title="Relationship between Price and Size", x="Size(sqm)", y="Price") + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))  



condos %<>%
  mutate(age_of_building = as.integer(age_of_building)) %>% #some values are not in integer form but a range, this will turn them into NA and they will be removed later
  drop_na() %>%
  mutate(price_per_sqm = round(price/actual_size),
         mainenance_per_sqm = signif(maintenance_fees/actual_size,3))%>% #standardizing the price and maitenance fees per sqm
   select(-c("size_range_level",
            "actual_size",
            "price",
            "maintenance_fees"))

areas_ordered <- condos %>%
  group_by(area) %>%
  dplyr::summarise(mean = mean(price_per_sqm, na.rm=TRUE)) %>%
  arrange(desc(mean)) %>%
  .[['area']]


##Regression Model 1: Regress Price/Sqm on everything else (without area in mind YET)

mod_one <- condos %>%
  select(-c("area")) %>%
  lm(price_per_sqm ~ . , data=.) 

anova(mod_one)
summary(mod_one)

# 
# Interesting factors: exposure, age_of_building, heating_type, property_type, include_rooftop_deck,
# include_security_system, include_security_guard, include_pet_restriction, include_concierge        
# include_gym, media_room, mainenance_per_sqm     



#One thing we notice is that exposure EW increases pricing by 304. 
#Q: Do expensive areas have more exposure to that area?

#Sunrise happens in the southwest and Sunset in the Southwest

condos %>%
  group_by(exposure, area) %>%
  summarise(count=n()) %>% #count the number of exposures in each area
  filter(exposure=="EW") %>%
  arrange(match(area, areas_ordered), desc(exposure), desc(count)) #reorder data so that we see the most expensive areas first
#nothing really interesting



#Q: Do expensive areas have "younger" buildings?

condos %>%
  group_by(area) %>%
  summarise(avg_building_age=mean(age_of_building)) %>% #the average building age in each area
  arrange(match(area, areas_ordered), desc(avg_building_age))%>% #reorder data so that we see the most expensive areas first
  view()
#expensive and inexpensive areas have a mix of old and new buildings


#Q: Do expensive areas have less pet restrictions?
condos %>%
  group_by(area) %>%
  summarise(count_pet_restictions=sum(include_pet_restriction),
            count = n()) %>% #the number of pet restictions  in each area
  arrange(match(area, areas_ordered), desc(count_pet_restictions))%>% #reorder data so that we see the most expensive areas first
  view()

#Not necassrily


#Q: Do expensive areas have more parking spots?
condos %>%
  group_by(area) %>%
  summarise( spots_per_condo = mean(num_of_parking)) %>% #the average building age in each area
  arrange(match(area, areas_ordered), desc(spots_per_condo))%>% #reorder data so that we see the most expensive areas first
  view()

#Not really


#Q: Do expensive areas have more gyms?
condos %>%
  group_by(area) %>%
  summarise(percentage_with_gym = mean(include_gym)) %>% #the average building age in each area
  arrange(match(area, areas_ordered), desc(percentage_with_gym))%>% #reorder data so that we see the most expensive areas first
  view()

#almost all expensive areas have a gym but not the least expensive areas


#Q: Do expensive areas have more rooftop decks?
condos %>%
  group_by(area) %>%
  summarise(percentage_with_deck = mean(include_rooftop_deck)) %>% #the average building age in each area
  arrange(match(area, areas_ordered), desc(percentage_with_deck))%>% #reorder data so that we see the most expensive areas first
  view()

#not really


#Q: Do expensive areas have  better service "concierge"?
condos %>%
  group_by(area) %>%
  summarise(percentage_with_concierge = mean(include_concierge)) %>% #the average building age in each area
  arrange(match(area, areas_ordered), desc(percentage_with_concierge))%>% #reorder data so that we see the most expensive areas first
  view()

#expensive areas have a higher rate on avg with concierge services 


#Q: Do expensive areas have  better security?
condos %>%
  mutate(sec = include_security_system*include_security_guard) %>%
  group_by(area) %>%
  summarise(percentage_with_good_security = mean(sec)) %>% #the average building age in each area
  arrange(match(area, areas_ordered), desc(percentage_with_good_security))%>% #reorder data so that we see the most expensive areas first
  view()

#not really

mod_two <- condos %>%
  lm(price_per_sqm ~ num_of_bed, data=.) 

anova(mod_two)
summary(mod_two)

condos %>%
  filter(area %in% random_area)  %>%
  ggplot(aes(y = price_per_sqm, x = age_of_building, colour =as.factor(area), group=area)) +
  scale_color_brewer(palette ="Spectral", name="Area(most expensive to least)", breaks=random_area) +
  # scale_color_paletteer_d(ggsci) + 
  geom_smooth(method = "lm", se= F) +
  labs(title="Relationship between Price and Age", x="Size(sqm)", y="Price") + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))  

