library(tidyverse)
library(pander)
library(tidyr)
library(plyr)
library(magrittr)

# Step0: Read csv file
condo.path = "../data/condos_info.csv"
condos <- read_csv(condo.path)

# Actual size : null & 0 -> NA
size.lst <- condos$actual_size
size.lst[size.lst == 0] = NA
size.lst[size.lst == "null"] = NA
condos$actual_size <- size.lst

# Remove $ sign
condos <- condos %>%
  filter(price != "TBD") %>%
  mutate(price = as.numeric(gsub('[\\$,]', '', price))) %>%
  mutate(maintenance_fees = as.numeric(gsub('[\\$,]', '', maintenance_fees)))

# Fix inconsistent size_range split way
size.range.col <- condos$size_range
size.range.col[size.range.col == "107 sqm"] = "93-111 sqm"
size.range.col[size.range.col == "73 sqm"] = "65-74 sqm"
size.range.col[size.range.col == "35 sqm"] = "0-46 sqm"
size.range.col[size.range.col == "43 sqm"] = "0-46 sqm"
size.range.col[size.range.col == "54 sqm"] = "46-56 sqm"
size.range.col[size.range.col == "62 sqm"] = "56-65 sqm"
size.range.col[size.range.col == "63 sqm"] = "56-65 sqm"
size.range.col[size.range.col == "67 sqm"] = "65-74 sqm"
size.range.col[size.range.col == "68 sqm"] = "65-74 sqm"
size.range.col[size.range.col == "73 sqm"] = "65-74 sqm"
size.range.col[size.range.col == "76 sqm"] = "74-84 sqm"
condos$size_range = size.range.col
# Remove some invalid data
condos <- condos %>%
  filter(size_range != "232-279 sqm", size_range != "139-186 sqm", size_range != "102-139 sqm")


# Use number to denote size range of condo
condos <- condos %>%
  mutate(size_range_level = case_when(
    size_range == "0-46 sqm" ~ 0,
    size_range == "46-56 sqm" ~ 1,
    size_range == "56-65 sqm" ~ 2,
    size_range == "65-74 sqm" ~ 3,
    size_range == "74-84 sqm" ~ 4,
    size_range == "84-93 sqm" ~ 5,
    size_range == "93-111 sqm" ~ 6,
    size_range == "111-130 sqm" ~ 7,
    size_range == "130-149 sqm" ~ 8,
    size_range == "149-167 sqm" ~ 9,
    size_range == "167-186 sqm" ~ 10,
    size_range == "186-209 sqm" ~ 11,
    size_range == "209-232 sqm" ~ 12,
    size_range == "232-255 sqm" ~ 13,
    size_range == "255-279 sqm" ~ 14,
    size_range == "279-302 sqm" ~ 15,
    size_range == "302-325 sqm" ~ 16,
    size_range == "325-350 sqm" ~ 17, # fake
    size_range == "350-375 sqm" ~ 18, # fake
    size_range == "375-395 sqm" ~ 19, # fake
    size_range == "395-418 sqm" ~ 20, # fake
    size_range == "418-441 sqm" ~ 21,
    size_range == "441-464 sqm" ~ 22,
    size_range == "+465 sqm" ~ 23
  ))

# amenity_lst -> new multiple columns
amenities <- list()
for (amenity in condos$amenity_lst) {
  amenity.lst <- as.list(strsplit(substring(amenity, 2, nchar(amenity)-1), ", "))
  amenities <- append(amenities, amenity.lst)
}
amenities <- unlist(amenities, recursive=FALSE)
unique.amenities <- unique(amenities)
amenities.count <- list()
for (amenity in unique.amenities) {
  count <- sum(amenities == amenity)
  amenities.count <- append(amenities.count, count)
}
amenities.count <- unlist(amenities.count, recursive=FALSE)
count.df <- tibble(count = amenities.count, amenity = unique.amenities)
count.df <- count.df %>%
  arrange(desc(count))
for (i in 1:20) {
  amenity <- count.df[i,]$amenity
  print(amenity)
}
condos <- condos %>%
  mutate(include_party_room = as.integer(as.logical(grepl("'Party Room'", amenity_lst, fixed=TRUE))),
         include_meeting_room = as.integer(as.logical(grepl("'Meeting Room'", amenity_lst, fixed=TRUE))),
         include_rooftop_deck = as.integer(as.logical(grepl("'Rooftop Deck'", amenity_lst, fixed=TRUE))),
         include_visitor_parking = as.integer(as.logical(grepl("'Visitor Parking'", amenity_lst, fixed=TRUE))),
         include_security_system = as.integer(as.logical(grepl("Security System'", amenity_lst, fixed=TRUE))),
         include_security_guard = as.integer(as.logical(grepl("'Security Guard'", amenity_lst, fixed=TRUE))),
         include_pet_restriction = as.integer(as.logical(grepl("'Pet Restriction'", amenity_lst, fixed=TRUE))),
         include_guest_suites = as.integer(as.logical(grepl("'Guest Suites'", amenity_lst, fixed=TRUE))),
         include_concierge = as.integer(as.logical(grepl("'Concierge'", amenity_lst, fixed=TRUE))),
         include_meeting_room = as.integer(as.logical(grepl("'Sauna'", amenity_lst, fixed=TRUE))),
         include_gym = as.integer(as.logical(grepl("'Gym'", amenity_lst, fixed=TRUE))),
         include_rec_room = as.integer(as.logical(grepl("'Rec Room'", amenity_lst, fixed=TRUE))),
         include_media_room = as.integer(as.logical(grepl("'Media Room'", amenity_lst, fixed=TRUE))),
         include_public_transit = as.integer(as.logical(grepl("'Public Transit'", amenity_lst, fixed=TRUE))),
         include_bbq_permitted = as.integer(as.logical(grepl("'BBQ Permitted'", amenity_lst, fixed=TRUE))),
         include_indoor_pool = as.integer(as.logical(grepl("'Indoor Pool'", amenity_lst, fixed=TRUE))))

# Drop possession
condos$possession <- NULL

# Add num of amenities
condos$num_amenities <- lengths(as.list(strsplit(substring(condos$amenity_lst, 2, nchar(condos$amenity_lst)-1), ", ")))

# Clean age of building
age.lst <- condos$age_of_building
age.lst[age.lst=="New"] = "0"
condos$age_of_building <- age.lst

condos %<>%
  mutate(age_of_building = gsub(" years old", "", age_of_building))

#Make area name consistent with demographics file

condos %<>%
  mutate(location_area=gsub("-|[.]", "", tolower(location_area))) %>%
  mutate(location_area=gsub(" ", "", tolower(location_area)))

  
condos %>% write_csv("../data/clean_condos_info.csv")
