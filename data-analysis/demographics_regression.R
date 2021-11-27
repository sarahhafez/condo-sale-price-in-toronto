library(tidyverse)
library(MASS)

demographics <- read.csv("../data/demographics.csv")
condos <- read.csv("../data/clean_condos_info.csv")

get_average_price <- function(area){
  filter_condos <- condos %>% 
    filter(location_area==area)
  avg_price <- mean(filter_condos$price, na.rm=TRUE)
  print(avg_price)
}

avg_condo_price <- sapply(demographics$area, get_average_price,USE.NAMES=FALSE)
demographics <- demographics %>%
  mutate(avg_condo_price = avg_condo_price)

head(demographics)

# select columns we need in model
demographics <- demographics %>%
  mutate(X0_to_19 = X0_to_4+X5_to_9+X10_to_14+X15_to_19) %>%
  mutate(X20_to_64 = X20_to_34+X35_to_49+X50_to_64) %>%
  mutate(X65_plus = X65_to_79+X80_plus)

# regression model
fit1 <- lm(avg_condo_price~transit+foot+bicycle+drive+total_individuals+owners+
             en_only+fr_only+en_and_fr+no_high_school+trade_certificate+college_certificate+
             university_certificate+bachelor_degree+avg_children_per_household+
             apartment_1_to_4_floors+X0_to_19+X20_to_64, data=demographics)
summary(fit1)
backstep1 <- stepAIC(fit1)

#avg_condo_price ~ transit + foot + bicycle + drive + total_individuals + 
#owners + en_only + fr_only + en_and_fr + trade_certificate + 
#  college_certificate + university_certificate + bachelor_degree + 
#  avg_children_per_household + X0_to_19 + X20_to_64
#  Adjusted R-squared:  0.8125 
summary(backstep1)

# binary classification
median_price <- median(demographics$avg_condo_price)
demographics <- demographics %>%
  mutate(expensive_area = ifelse(avg_condo_price>=median_price,1,0))
fit2 <- glm(expensive_area~transit+foot+bicycle+drive+total_individuals+owners+
             en_only+fr_only+en_and_fr+no_high_school+trade_certificate+college_certificate+
             university_certificate+bachelor_degree+avg_children_per_household+
             apartment_1_to_4_floors+X0_to_19+X20_to_64, data=demographics,family=binomial())
summary(fit2)
backstep2 <- stepAIC(fit2)
summary(backstep2)
# lm(formula = expensive_area ~ transit + foot + drive + total_individuals + 
#  owners + en_only + fr_only + no_high_school + trade_certificate + 
#  college_certificate + university_certificate + X0_to_19 + 
#  X20_to_64, data = demographics
# Adjusted R-squared:  0.7284

demographics$expensive_area
expensive <- demographics %>% 
  filter(expensive_area==1)
not_expensive <- demographics %>% 
  filter(expensive_area==0)
wilcox.test(expensive$foot, not_expensive$foot)
wilcox.test(expensive$total_individuals, not_expensive$total_individuals)
wilcox.test(expensive$owners, not_expensive$owners)
wilcox.test(expensive$en_only, not_expensive$en_only)
wilcox.test(expensive$fr_only, not_expensive$fr_only)
wilcox.test(expensive$no_high_school, not_expensive$no_high_school)
wilcox.test(expensive$trade_certificate, not_expensive$trade_certificate)
wilcox.test(expensive$post_graduate_degree, not_expensive$post_graduate_degree)
wilcox.test(expensive$X0_to_19, not_expensive$X0_to_19)
wilcox.test(expensive$X20_to_64, not_expensive$X20_to_64)
# significant: avg_children_per_household
mean(expensive$avg_children_per_household)
mean(not_expensive$avg_children_per_household)
wilcox.test(expensive$avg_children_per_household, not_expensive$avg_children_per_household)
wilcox.test(expensive$single_person, not_expensive$single_person)

# model on price/sqm
condos <- condos%>%
  mutate(price_per_sqm = price/actual_size)

get_average_sqm_price <- function(area){
  filter_condos <- condos %>% 
    filter(location_area==area)
  avg_price <- mean(filter_condos$price_per_sqm, na.rm=TRUE)
  return(avg_price)
}

get_average_sqm_price(demographics$area[1])

avg_condo_sqm_price <- sapply(demographics$area, get_average_sqm_price,USE.NAMES=FALSE)
avg_condo_sqm_price
demographics <- demographics %>%
  mutate(avg_condo_sqm_price = avg_condo_sqm_price)

demographics <- na.omit(demographics)

fit3 <- lm(avg_condo_sqm_price~transit+foot+bicycle+drive+total_individuals+owners+
             en_only+fr_only+en_and_fr+no_high_school+trade_certificate+college_certificate+
             university_certificate+bachelor_degree+avg_children_per_household+
             apartment_1_to_4_floors+X0_to_19+X20_to_64, data=demographics)
summary(fit3)
backstep3 <- stepAIC(fit3)
#lm(formula = avg_condo_sqm_price ~ transit + foot + bicycle + 
#drive + total_individuals + owners + en_only + fr_only + 
#  en_and_fr + no_high_school + trade_certificate + university_certificate + 
#  bachelor_degree + avg_children_per_household + apartment_1_to_4_floors + 
#  X0_to_19 + X20_to_64, data = demographics)
# Adjusted R-squared:  0.8096
summary(backstep3)

median_price <- median(demographics$avg_condo_sqm_price,na.rm = TRUE)
median_price
demographics <- demographics %>%
  mutate(expensive_area = ifelse(avg_condo_sqm_price>=median_price,1,0))
fit4 <- glm(expensive_area~transit+foot+bicycle+drive+total_individuals+owners+
             en_only+fr_only+en_and_fr+no_high_school+trade_certificate+college_certificate+
             university_certificate+bachelor_degree+avg_children_per_household+
             apartment_1_to_4_floors+X0_to_19+X20_to_64, data=demographics, family=binomial)
summary(fit4)
backstep4 <- stepAIC(fit4)
#lm(formula = expensive_area ~ transit + foot + bicycle + drive + 
#     total_individuals + owners + en_only + fr_only + en_and_fr + 
#     no_high_school + college_certificate + university_certificate + 
#     bachelor_degree + avg_children_per_household + apartment_1_to_4_floors + 
#     X0_to_19 + X20_to_64, data = demographics)
summary(backstep4)
#Adjusted R-squared:  -0.255 
