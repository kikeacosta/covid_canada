rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)
library(readxl)
# source("Code/00_functions.R")

date_cut <- 

db_exc <- read_csv("Output/excess_weeks_8_27.csv")

db_can <- read_rds("Output/cfr_by_age_sex.rds")


db_exc2 <- db_exc %>% 
  filter(Age == "All") %>% 
  select(Region, Sex, Excess_pos) %>% 
  rename(Deaths = Excess_pos) %>% 
  mutate(Source = "Excess")

db_exc2 <- db_exc %>% 
  filter(Age != "All") %>% 
  select(Region, Sex, Excess_pos) %>% 
  rename(Deaths = Excess_pos) %>% 
  group_by(Region, Sex) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Source = "Excess")

ca_b <- db_can %>% 
  filter(Date == "2020-07-09",
         Code == "CA") %>% 
  group_by(Region, Code, Age) %>% 
  summarise(Cases = sum(Cases),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Sex = "b")

db_can2 <- db_can %>% 
  filter(Date == "2020-07-09",
         Region != "Montreal") %>% 
  bind_rows(ca_b) %>% 
  select(-new_c, -new_d, -Date, -Cases) %>% 
  group_by(Region, Sex) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Source = "Diagnosed",
         Region = ifelse(Region == "All", "Canada", Region))

db_deaths <- bind_rows(db_can2, db_exc2)

db_deaths %>% 
  ggplot()+
  geom_point(aes(Sex, Deaths, col = Source))+
  facet_wrap(~ Region, scales = "free")+
  scale_color_manual(values = c("red", "black"))+
  labs(title = "Identified vs Excess mortality",
       x = "Sex")+
  theme_bw()

ggsave("Figures/deaths_covid_vs_excess.png")





