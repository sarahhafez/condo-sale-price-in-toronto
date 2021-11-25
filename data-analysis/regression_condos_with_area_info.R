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
  mutate(age_of_building = as.integer(age_of_building)) %>%
  mutate(price_per_sqm = price/actual_size)
all.info <- condos %>%
  merge(areas, by="area")

# Fit the prediction model with area label
full1 <- lm(formula = price~num_of_bed+num_of_bath+num_of_parking+outdoor_space+
               locker+property_type+ensuite_laundry+age_of_building+size_range_level+include_party_room+include_meeting_room+
               include_rooftop_deck+include_visitor_parking+include_security_system+include_security_guard+include_guest_suites+
               include_gym+include_rec_room+include_public_transit+include_bbq_permitted+include_indoor_pool+
               num_amenities+area, all.info)
backward_selection_1 <- step(full1)
summary(backward_selection_1)

# And based on final selection, the area-yorkville is statistics significant
# So Why? which factor is the most important reason? We'll remove the area
# We only consider the factors in backward_selection_1 for condos' information
full2 <- lm(formula = price~num_of_bed + num_of_bath + num_of_parking + 
               outdoor_space + locker + property_type + ensuite_laundry + 
               age_of_building + size_range_level + include_rooftop_deck + 
               include_security_guard + include_gym + include_public_transit +
               transit + foot + bicycle + drive + single_family + multi_family + single_person + 
               total_individuals + X0_to_4 + X5_to_9 + X10_to_14 + X15_to_19 + X20_to_34 + X35_to_49 +
               X50_to_64 + X65_to_79 + owners + en_only + fr_only + en_and_fr + no_high_school +
              high_school + trade_certificate + college_certificate + bachelor_degree + avg_children_per_household +
              single_detached + semi_detached + duplex + row_houses + apartment_1_to_4_floors + children_0 +
              children_1 + children_2 + mandarin + cantonese + english, all.info)
backward_selection_2 <- step(full2)
summary(backward_selection_2)


# Next, we will focus on price per sqm
select <- all.info %>%
  select(c("price_per_sqm","num_of_bed","num_of_bath","num_of_parking","outdoor_space",
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
sqm1 <- lm(formula = price_per_sqm~num_of_bed + num_of_bath+num_of_parking+outdoor_space +
             locker + property_type + ensuite_laundry+age_of_building + include_party_room + include_meeting_room +
             include_rooftop_deck + include_visitor_parking + include_security_system + include_security_guard + 
             include_guest_suites + include_gym + include_rec_room + include_public_transit + include_bbq_permitted +
             include_indoor_pool + num_amenities + area, no.miss)
backward_selection_3 <- step(sqm1)
summary(backward_selection_3)
# And you will noticed the multiple areas is SS now. It is due to the fact that yorkville has average largest sqm for condos,
# thus having a extremely high price.

sqm2 <- lm(formula = price_per_sqm ~ num_of_bed + num_of_parking + outdoor_space + 
             property_type + ensuite_laundry + age_of_building + include_rooftop_deck + 
             include_security_system + include_security_guard + include_public_transit + 
             include_indoor_pool + transit + foot + bicycle + drive + single_family + multi_family + single_person + 
             total_individuals + X0_to_4 + X5_to_9 + X10_to_14 + X15_to_19 + X20_to_34 + X35_to_49 +
             X50_to_64 + X65_to_79 + owners + en_only + fr_only + en_and_fr + no_high_school +
             high_school + trade_certificate + college_certificate + bachelor_degree + avg_children_per_household +
             single_detached + semi_detached + duplex + row_houses + apartment_1_to_4_floors + children_0 +
             children_1 + children_2 + mandarin + cantonese + english, no.miss)
backward_selection_4 <- step(sqm2)
summary(backward_selection_4)

