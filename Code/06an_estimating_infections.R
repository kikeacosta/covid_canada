rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)
library(readxl)

# setwd("U:/gits/covid_canada/")

# source("Code/00_functions.R")


# COVID deaths and infections for Canada and provinces
db_ifrs <- read_rds("Data_output/ifrs_can_age10.rds")
db_can_age <- read_rds("Data_output/cfr_by_age.rds")

unique(db_can_age$Region)

db_can_age2 <- db_can_age %>% 
  select(Region, date_f, Age, Cases, Deaths) %>% 
  arrange(Region, date_f, Age) %>% 
  mutate(Region = ifelse(Region == 'British Columbia', "BC", Region)) %>% 
  group_by(Region, date_f) %>%  
  mutate(Deaths_all = sum(Deaths)) %>% 
  ungroup() %>% 
  filter(Deaths_all > 0)

dates_all <- db_can_age2 %>% 
  # filter(Region != "All") %>% 
  select(Region, date_f) %>% 
  unique() %>% 
  mutate(n = 1) %>% 
  spread(Region, n) %>% 
  mutate(av = Alberta + BC + Montreal + Ontario + Quebec + All) %>% 
  # mutate(av = Alberta + BC + Montreal + Ontario + Quebec) %>% 
  filter(av == 6) %>% 
  select(date_f)

# end of first wave
end_wave <- "2020-07-09"

db_infs <- db_can_age2 %>% 
  filter(date_f == end_wave) %>% 
  left_join(db_ifrs) %>% 
  mutate(Deaths = ifelse(Age == 0, 0, Deaths),
         Infs = Deaths / IFR,
         under = ifelse(Deaths == 0, NA, Cases / Infs)) %>% 
  drop_na()
  
db_infs2 <- db_infs %>% 
  select(Region, Age, Cases, Infs) %>% 
  gather(Cases, Infs, key = Measure, value = Value)


db_infs2 %>% 
  ggplot()+
  geom_point(aes(Age, Value, col = Measure))+
  facet_wrap(~ Region, scales = "free")+
  labs(title = "COVID cases and infections")+
  theme_bw()

ggsave("Figures/cases_infections.png")

db_infs %>% 
  ggplot()+
  geom_point(aes(Age, under, col = Region))+
  geom_hline(yintercept = 1)+
  scale_y_log10(limits = c(0.05, 2))+
  labs(title = "Underestimation of COVID infections")+
  theme_bw()

ggsave("Figures/underest_infections.png")


