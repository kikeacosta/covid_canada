rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)

# functions
source("Code/00_functions.R")

# reading Canada data
# db_can_age <- read_rds("Output/cfr_by_age_sex.rds")
# 
# db_oth <- read_rds("Output/other_regions_by_age_sex.rds")

db <- read_rds("Output/covid_data_by_age_sex.rds")

# Aggregating in 10-year age groups

# age_int <- 10
# 
# db2 <- db %>% 
#   mutate(Age = floor(Age / age_int) * age_int) %>%
#   group_by(Region, Sex, Age) %>%
#   summarise(Cases = sum(Cases),
#             Deaths = sum(Deaths)) %>% 
#   ungroup()
# 

# CFR decomposition at one specific date
########################################

db_can_oth <- db %>% 
  group_by(Region, Date, Sex) %>% 
  mutate(CFR = Deaths / Cases,
         age_dist = Cases / sum(Cases),
         Cases_t = sum(Cases),
         Deaths_t = sum(Deaths),
         CFR_t = Deaths_t / Cases_t) %>% 
  ungroup() 

table(db_can_oth$Region, db_can_oth$Sex)

tx <- 8
s <- "b"

# Across countries
##################

rgs <- c("Belgium",
         "Denmark",
         "Germany",
         "Italy",
         "Netherlands",
         "Sweden",
         "USA")

rfs <- c("Canada")

db_counts <- diffs_ref(db_can_oth, rfs, rgs, "Countries", 2.5)

# Provinces and Canada
######################

rgs <- c("Alberta",
         "British Columbia",
         "Ontario",
         "Quebec")
rfs <- c("Canada")
db_provs <- diffs_ref(db_can_oth, rfs, rgs, "Provinces", 2)

rgs <- c("Canada", 
         "Alberta",
         "British Columbia",
         "Ontario",
         "Quebec")
rfs <- c("Alberta",
         "British Columbia",
         "Ontario",
         "Quebec")
diffs_ref(db_can_oth, rfs, rgs, "Provinces", 5)

# Across cities
###############

rgs <- c("Montreal",
         "Toronto", 
         "Berlin", 
         "NYC")
rfs <- c("Montreal", 
         "Toronto")
db_cities <- diffs_ref(db_can_oth, rfs, rgs, "Cities", 2.7)


