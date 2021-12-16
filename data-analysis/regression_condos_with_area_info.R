library(tidyverse)
library(pander)
library(tidyr)
library(plyr)
library(magrittr)

condo.path = "../data/clean_condos_info.csv"
geo.path = "../data/demographics.csv"
condos <- read_csv(condo.path)
areas <- read.csv(geo.path)
condos <- condos %>%
  mutate(area=location_area) %>%
  select(-c("tax_2021", #taxes are dependent on property price
            "amenity_lst", #already extracted relevant data in other columns
            "size_range", #created a size_range_lvl variable instead
            "address", #not relevant for regression
            "corp_num", #??
            "location_area" #changed name to area
  )) %>%
  mutate(exposure_EW = as.integer(as.logical(exposure=="EW"))) %>%
  mutate(age_of_building = as.integer(age_of_building)) %>%
  mutate(price_per_sqm = round(price/actual_size),
         mainenance_per_sqm = signif(maintenance_fees/actual_size,3))%>% #standardizing the price and maitenance fees per sqm
  select(-c("size_range_level",
            "actual_size",
            "maintenance_fees"))

areas <- areas %>%
  mutate_at(vars(-area, -avg_household_income,-avg_individual_income,-avg_children_per_household,-total_individuals),
            .funs=funs(. * 100)) %>%
  mutate(X0_to_19 = X0_to_4+X5_to_9+X10_to_14+X15_to_19) %>%
  mutate(X65_plus = X65_to_79 + X80_plus) %>%
  mutate(house = single_detached + semi_detached + duplex + row_houses) %>%
  select(-c("other", "multi_person", "total_individuals", "renters", "X0_to_19", "other.1", 
            "trade_certificate", "single_detached", "semi_detached", "duplex", "row_houses", "house", 
            "children_0", "children_1", "children_2", "children_3_plus", "X0_to_4", "X5_to_9", "X10_to_14", "X15_to_19",
            "X65_to_79", "X80_plus"))

all.info <- condos %>%
  merge(areas, by="area")

# Fit the prediction model with area label
data1 <- condos %>%
  select(-c("price_per_sqm")) %>%
  drop_na()
  
full1 <- lm(formula = price~., data1)
backward_selection_1 <- step(full1)
summary(backward_selection_1)

# And based on final selection, the area-yorkville is statistics significant
# So Why? which factor is the most important reason? We'll remove the area
# We only consider the factors in backward_selection_1 for condos' information
data2 <- all.info %>% 
  select(-c("price_per_sqm","num_amenities", "include_rooftop_deck", "include_visitor_parking", "include_rec_room", 
            "include_bbq_permitted", "include_meeting_room", "include_guest_suites", "include_party_room", "include_media_room",
            "include_security_system", "parking_type", "locker", "heating_type", "property_type", "outdoor_space"))
full2 <- lm(formula = price~., data2)
backward_selection_2 <- step(full2)
summary(backward_selection_2)


# Next, we will focus on price per sqm
select <- all.info %>%
  select(c("price_per_sqm","num_of_bed","num_of_bath","num_of_parking","exposure", "outdoor_space",
           "locker","property_type","ensuite_laundry","age_of_building","include_party_room","include_meeting_room",
           "include_rooftop_deck","include_visitor_parking","include_security_system","include_security_guard","include_guest_suites",
           "include_gym","include_rec_room","include_public_transit","include_bbq_permitted","include_indoor_pool",
           "num_amenities", "area", "transit", "foot", "bicycle", "drive", "single_family", "multi_family", "single_person", 
            "total_individuals", "X0_to_4", "X5_to_9", "X10_to_14", "X15_to_19", "X20_to_34", "X35_to_49",
            "X50_to_64", "X65_to_79", "owners", "en_only", "fr_only", "en_and_fr", "no_high_school",
            "high_school", "trade_certificate", "college_certificate", "bachelor_degree", "avg_children_per_household",
             "single_detached", "semi_detached", "duplex", "row_houses", "apartment_1_to_4_floors", "children_0",
             "children_1", "children_2", "mandarin", "cantonese", "english"))
no.miss <- na.omit(select)
sqm1 <- lm(formula = price_per_sqm~num_of_bed + num_of_bath+num_of_parking+exposure+outdoor_space +
             locker + property_type + ensuite_laundry+age_of_building + include_party_room + include_meeting_room +
             include_rooftop_deck + include_visitor_parking + include_security_system + include_security_guard + 
             include_guest_suites + include_gym + include_rec_room + include_public_transit + include_bbq_permitted +
             include_indoor_pool + num_amenities + area, no.miss)
backward_selection_3 <- step(sqm1)
summary(backward_selection_3)
# And you will noticed the multiple areas is SS now. It is due to the fact that yorkville has average largest sqm for condos,
# thus having a extremely high price.

data2 <- all.info %>% 
  select(-c("price","exposure", "num_amenities", "include_rooftop_deck", "include_visitor_parking", "include_rec_room", 
            "include_bbq_permitted", "include_meeting_room", "include_public_transit", "include_guest_suites", "avg_individual_income",
            "avg_household_income","include_party_room", "include_indoor_pool", "include_media_room", "ensuite_laundry", "area", "X", 
            "X20_to_34", "X35_to_49", "X50_to_64", "include_security_system", "parking_type", "locker", 
            "heating_type", "property_type", "outdoor_space", "mainenance_per_sqm",
            "french", "min_nan_chaochow_teochow_fukien_taiwanese", "vietnamese", "english", "cantonese", "mandarin", "latvian", "russian",
            "spanish", "other.2"))
sqm2 <- lm(formula = price_per_sqm ~ ., data2)
backward_selection_4 <- step(sqm2)
summary(backward_selection_4)

areas_ordered <- condos %>%
  group_by(area) %>%
  dplyr::summarise(mean = mean(price_per_sqm, na.rm=TRUE)) %>%
  arrange(desc(mean)) %>%
  .[['area']]

expensive_area_condos <- all.info %>%
  filter(area %in% areas_ordered[1:5]) %>%
  mutate(exposure_EW = as.integer(as.logical(exposure=="EW")))
non_expensive_area_condos <- all.info %>%
  filter(area %in% areas_ordered[6:23])%>%
  mutate(exposure_EW = as.integer(as.logical(exposure=="EW")))

field = c("average_children", "age_of_building",  "include_gym", "include_pet_restrictions",
          "multi_family_percentage", "french_only_percentage")
expensive_areas = c(mean(expensive_area_condos$avg_children_per_household), mean(expensive_area_condos$age_of_building, na.rm=TRUE),
                    mean(expensive_area_condos$include_gym), mean(expensive_area_condos$include_pet_restriction),
                    mean(expensive_area_condos$multi_family), mean(expensive_area_condos$fr_only))
non_expensive_areas = c(mean(non_expensive_area_condos$avg_children_per_household), mean(non_expensive_area_condos$age_of_building,na.rm=TRUE),
                        mean(non_expensive_area_condos$include_gym), mean(non_expensive_area_condos$include_pet_restriction),
                        mean(non_expensive_area_condos$multi_family), mean(non_expensive_area_condos$fr_only))
unit_effect_on_condo_price = c(-5361.8847, -11.9234, 34.2827, -61.8511, 31652.7176, 36017.8021)
total_effect_value = round((expensive_areas - non_expensive_areas) * unit_effect_on_condo_price, digits = 4)
view(cbind(field, expensive_areas, non_expensive_areas, unit_effect_on_condo_price, total_effect_value))
