rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)

# functions
source("Code/00_functions.R")

# reading Canada data
db_can_age <- read_rds("Output/cfr_by_age_sex.rds")

db_oth <- read_rds("Output/other_regions_by_age_sex.rds")

# CFR decomposition at one specific date
########################################

tx <- 8

s <- "b"
unique(db_oth$Region)

# grouping both sexes in Canada and Ontario
ca_b <- db_can_age %>% 
  select(-new_c, -new_d) %>% 
  filter(Date == "2020-07-09",
         Code == "CA") %>% 
  group_by(Region, Code, Date, Age) %>% 
  summarise(Cases = sum(Cases),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Sex = "b")

db_can_oth <- db_can_age %>% 
  select(-new_c, -new_d) %>% 
  filter(Date == "2020-07-09") %>% 
  bind_rows(ca_b, db_oth) %>% 
  mutate(Age = case_when(Age >= 80 ~ 80, 
                         Age <= 10 ~ 0, 
                         TRUE ~ Age)) %>% 
  group_by(Region, Date, Sex, Age) %>% 
  summarise(Cases = sum(Cases),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  group_by(Region, Date, Sex) %>% 
  mutate(CFR = Deaths / Cases,
         age_dist = Cases / sum(Cases),
         Cases_t = sum(Cases),
         Deaths_t = sum(Deaths),
         CFR_t = Deaths_t / Cases_t) %>% 
  ungroup() %>% 
  mutate(Region = ifelse((Region == "Toronto" & Date == ymd("2020-10-24")), "Toronto_2", Region),
         Region = ifelse(Region == "All", "Canada", Region))

table(db_can_oth$Region, db_can_oth$Sex)

unique(db_can_oth$Region)

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
db_provs <- diffs_ref(db_can_oth, rfs, rgs, "Provinces", 5)

# Across cities
###############

rgs <- c("Montreal",
         "Toronto", 
         "Berlin", 
         "NYC")
rfs <- c("Montreal", 
         "Toronto")
db_cities <- diffs_ref(db_can_oth, rfs, rgs, "Cities", 2.7)


