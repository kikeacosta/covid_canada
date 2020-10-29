rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)
library(readxl)

# setwd("U:/gits/covid_canada/")

# source("Code/00_functions.R")


# COVID deaths and infections for Canada and provinces
db_ifrs <- read_rds("Output/ifr_age_sex_canada.rds")
db_can_age <- read_rds("Output/cfr_by_age_sex.rds")

unique(db_can_age$Region)
unique(db_can_age$Age)
unique(db_can_age$Sex)

# excluding dates previous the first COVID death
db_can_age2 <- db_can_age %>% 
  select(Region, Date, Sex, Age, Cases, Deaths) %>% 
  arrange(Region, Date, Sex, Age) %>% 
  mutate(Region = ifelse(Region == 'British Columbia', "BC", Region)) %>% 
  group_by(Region, Date, Sex) %>%  
  mutate(Deaths_all = sum(Deaths)) %>% 
  ungroup() %>% 
  filter(Deaths_all > 0,
         Region != "Montreal")

# looking for dates common to all regions
dates_all <- db_can_age2 %>% 
  # filter(Region != "All") %>% 
  select(Region, Date) %>% 
  unique() %>% 
  mutate(n = 1) %>% 
  spread(Region, n) %>% 
  mutate(av = Alberta + BC + Ontario + Quebec + All) %>% 
  filter(av == 5) %>% 
  select(Date)

# date closest to the end of first wave (end June - begining July)
end_wave <- "2020-07-09"

# Adjusting IFRs in 10-year age groups
db_ifrs2 <- db_ifrs %>% 
  mutate(Age = Age - 5) %>% 
  filter(Age %in% seq(0, 100, 10))

# Merging IFRs and Canada data
db_infs <- db_can_age2 %>% 
  filter(Date == end_wave) %>% 
  left_join(db_ifrs) %>% 
  mutate(Deaths = ifelse(Age == 0, 0, Deaths),
         Infs = Deaths / IFR,
         under = ifelse(Deaths == 0, NA, Cases / Infs)) %>% 
  drop_na()
  

# Calculating infections by age and sex
db_infs2 <- db_infs %>% 
  select(Region, Sex, Age, Cases, Infs, Source) %>% 
  gather(Cases, Infs, key = Measure, value = Value)


# both sexes
db_infs2 %>% 
  filter(Sex == "b") %>% 
  ggplot()+
  geom_point(aes(Age, Value, col = Measure))+
  facet_wrap(~ Region, scales = "free")+
  labs(title = "COVID cases and infections")+
  theme_bw()

# ggsave("Figures/cases_infections.png")

db_infs %>% 
  filter(Sex == "b") %>% 
  ggplot()+
  geom_point(aes(Age, under))+
  facet_wrap(~ Region)+
  geom_hline(yintercept = 1)+
  scale_y_log10(limits = c(0.05, 2))+
  labs(title = "Underestimation of COVID infections")+
  theme_bw()

# ggsave("Figures/underest_infections.png")

# by sexes
db_infs2 %>% 
  filter(Sex != "b") %>% 
  ggplot()+
  geom_point(aes(Age, Value, col = Measure, shape = Sex))+
  facet_wrap(~ Region, scales = "free")+
  labs(title = "COVID cases and infections")+
  theme_bw()

# ggsave("Figures/cases_infections.png")

db_infs %>% 
  filter(Sex != "b") %>% 
  ggplot()+
  geom_point(aes(Age, under, col = Sex))+
  facet_wrap(~ Region)+
  geom_hline(yintercept = 1)+
  scale_y_log10(limits = c(0.05, 2))+
  labs(title = "Underestimation of COVID infections")+
  theme_bw()

# ggsave("Figures/underest_infections.png")


