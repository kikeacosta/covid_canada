rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)
library(readxl)
# source("Code/00_functions.R")

ym <- 2014
db_baseline <- read_csv(paste0("Output/baseline_mortality_", ym, ".csv")) 

# excess mortality by age since the begining of the pandemic in Canada, 
# February 22 (week 8),until the end of the first wave, July 4 (week 27)
first_week <- 8
last_week <- 27

db_excess <- db_baseline %>% 
  rename(Baseline = pred) %>% 
  replace_na(list(Age = "All")) %>% 
  filter(Year == 2020,
         Week >= first_week & Week <= last_week) %>% 
  select(Region, Sex, Age, Date, Week, Deaths, Exposure, Baseline, lp, up) %>% 
  mutate(Age = as.character(Age),
         Out = ifelse(Deaths > up | Deaths < lp, 1, 0),
         Baseline
         Excess = Deaths - Baseline,
         Excess_lp = ifelse(Deaths > up, Deaths - up, 0),
         Excess_up = ifelse(Deaths > lp, Deaths - lp, 0))




%>% 
  group_by(Region, Sex, Age) %>% 
  summarise(Exposure = max(Exposure),
            Deaths = sum(Deaths),
            Baseline = sum(Baseline),
            Excess = sum(Excess),
            Excess_lp = sum(Excess_lp),
            Excess_up = sum(Excess_up)) %>% 
  ungroup() %>% 
  arrange(Region, Sex, suppressWarnings(as.integer(Age))) %>% 
  mutate(p_score = Deaths / Baseline)

# for all ages
db_excess_all <- db_excess_age %>% 
  group_by(Sex) %>% 
  summarise(Exposure = max(Exposure),
            Deaths = sum(Deaths),
            Baseline = sum(Baseline),
            Excess = sum(Excess),
            Excess_lp = sum(Excess_lp),
            Excess_up = sum(Excess_up),
            last_week = max(last_week)) %>% 
  mutate(Age = "All") %>% 
  ungroup()

db_excess_4 <- bind_rows(db_excess_age, db_excess_all) %>% 
  mutate(last_date = as.Date(paste(2020, last_week, 1, sep="-"), "%Y-%U-%u")) %>% 
  arrange(Sex, suppressWarnings(as.integer(Age)))

# saving excess mortality 
write_csv(db_excess_4,  path = "Output/spain_excess_until_seroprev.csv")

# # Plotting excess mortality
# db_excess_age %>% 
#   select(Sex, Age, Deaths, Baseline) %>% 
#   gather(Deaths, Baseline, key = "Mortality", value = "Value") %>% 
#   ggplot()+
#   geom_point(aes(Age, Value, col = Mortality))
# 
# db_excess_age %>% 
#   select(Sex, Age, Excess) %>% 
#   mutate(Age = as.integer(Age)) %>% 
#   ggplot()+
#   geom_line(aes(Age, Excess, col = Sex))+
#   scale_y_log10()



