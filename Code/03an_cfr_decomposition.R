rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)

# functions
source("Code/00_functions.R")

# reading Canada data
db_can <- read_rds("Data_output/canada_cases_deaths.rds")

db_can_age <- read_rds("Data_output/cfr_by_age.rds")

db_can2 <- db_can %>% 
  select(Region, date_f, Cases, Deaths, CFR) %>% 
  rename(Cases_t = Cases,
         Deaths_t = Deaths,
         CFR_t = CFR)

db_can_age2 <- db_can_age %>% 
  select(Region, date_f, Age, Cases, Deaths, CFR) %>% 
  arrange(Region, date_f, Age) %>% 
  group_by(Region, date_f) %>%  
  mutate(age_dist = Cases / sum(Cases)) %>% 
  ungroup() %>% 
  left_join(db_can2) %>% 
  filter(!is.na(CFR_t),
         CFR_t > 0) %>% 
  mutate(CFR = replace_na(CFR, 0),
         Region = ifelse(Region == 'British Columbia', "BC", Region))

dates_all <- db_can_age2 %>% 
  filter(Region != "All") %>% 
  select(Region, date_f) %>% 
  unique() %>% 
  mutate(n = 1) %>% 
  spread(Region, n) %>% 
  mutate(av = Alberta + BC + Montreal + Ontario + Quebec) %>% 
  filter(av == 5)

p1 <- "Quebec"
p2 <- "Montreal"
d_exc <- c(ymd("2020-04-23"), ymd("2020-04-28"), ymd("2020-04-29"))
d_exc <- c(ymd("2020-04-23"))
d_exc <- c(ymd("2020-04-28"), ymd("2020-04-29"))

decomp(p1, p2, d_exc)
