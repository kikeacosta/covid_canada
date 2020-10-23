rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(readr)
library(lubridate)
library(ggplot2)
library(osfr)

# reading Canada data
db_nal <- read_rds("Data/201020_covid_canada.rds") %>% 
  mutate(Code = str_replace(Code, Date, "")) %>% 
  select(Country, Region, Code, Date, Sex, Age, AgeInt, Measure, Value)
  

osf_retrieve_file("43ucn") %>%
  osf_download(conflicts = "overwrite") 

# This reads it in
db_cov <-  read_csv("Output_10.zip",
                    skip = 3,
                    col_types = "ccccciiddd")

# looking for regions included in COVerAGE-DB
db_cov %>% 
  filter(Country == "Canada") %>% 
  mutate(Code = str_replace(Code, Date, "")) %>%
  pull(Code) %>% 
  unique()

# only keeping regions from regional sources
db_prv <- db_cov %>% 
  filter(Country == "Canada") %>% 
  mutate(Code = str_replace(Code, Date, "")) %>% 
  filter(Code %in% c("CA_AB", "CA_BC", "CA_MTL", "CA_ON", "CA_QC")) %>% 
  select(-Tests)

unique(db_prv$Code)

######################################################
# looking at all population
######################################################

db_prv_all <- db_prv %>% 
  filter(Sex == "b") %>% 
  group_by(Region, Code, Date) %>% 
  summarise(Cases = sum(Cases),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(date_f = dmy(Date)) %>% 
  arrange(Region, date_f)

db_nal_all <- db_nal %>% 
  filter(Sex == "b",
         Age == "TOT") %>% 
  spread('Measure', 'Value') %>% 
  select(Region, Code, Date, Cases, Deaths) %>% 
  mutate(date_f = dmy(Date)) %>% 
  arrange(date_f)
  
db_can <- bind_rows(db_prv_all, db_nal_all)

##########################
# temporal adjustments 
# exclusion of Mtl last dates, which have wrong data
db_can <- db_can %>% 
  filter(!(Region == "Montreal" & date_f >= "2020-10-09"))

# adjustment of BC deaths which are mutiplied by 100
db_can <- db_can %>% 
  mutate(Deaths = ifelse(Code == "CA_BC" & Date %in% c("28.04.2020", "29.04.2020"), Deaths / 100, Deaths))

##########################


# estimating new cases and new deaths
db_can2 <- db_can %>% 
  arrange(Region, date_f) %>% 
  group_by(Region) %>% 
  mutate(new_c = Cases - lag(Cases),
         new_d = Deaths - lag(Deaths)) %>% 
  ungroup()

# plotting cumulative cases 
db_can2 %>% 
  filter(date_f >= "2020-03-01") %>% 
  drop_na(Cases) %>% 
  ggplot()+
  geom_point(aes(date_f, Cases, col = Region), size = 0.7, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  labs(title = "Cumulative cases over time")+
  theme_bw()

ggsave("Figures/cum_cases_over_time.png")

# plotting cumulative deaths
db_can2 %>% 
  filter(date_f >= "2020-03-01") %>% 
  drop_na(Deaths) %>% 
  ggplot()+
  geom_point(aes(date_f, Deaths, col = Region), size = 0.7, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  labs(title = "Cumulative deaths over time")+
  theme_bw()

ggsave("Figures/cum_deaths_over_time.png")

# plotting new cases 
db_can2 %>% 
  filter(date_f >= "2020-03-01") %>% 
  drop_na(new_c) %>% 
  ggplot()+
  geom_point(aes(date_f, new_c, col = Region), size = 0.7, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  labs(title = "New cases over time")+
  theme_bw()

ggsave("Figures/new_cases_over_time.png")

# plotting new deaths
db_can2 %>% 
  filter(date_f >= "2020-03-01") %>% 
  drop_na(new_d) %>% 
  ggplot()+
  geom_point(aes(date_f, new_d, col = Region), size = 0.7, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  labs(title = "New deaths over time")+
  theme_bw()

ggsave("Figures/new_deaths_over_time.png")

# CFRs
db_can3 <- db_can2 %>% 
  mutate(CFR = Deaths / Cases)

# plotting CFR over time
db_can3 %>% 
  drop_na(CFR) %>% 
  filter(date_f >= "2020-03-01") %>% 
  ggplot()+
  geom_point(aes(date_f, CFR, col = Region), size = 0.7, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  labs(title = "Overall CFR over time")+
  theme_bw()

ggsave("Figures/all_CFR_over_time.png")

db_can3 %>% 
  drop_na(Deaths, Cases) %>% 
  filter(date_f >= "2020-03-01") %>% 
  ggplot()+
  geom_point(aes(Cases, Deaths, col = Region), size = 0.7, alpha = 0.8)+
  # scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  labs(title = "Slopes of overall CFR over time")+
  theme_bw()

ggsave("Figures/all_CFR_slopes.png")


######################################################
######################################################
# by age both sexes
######################################################
######################################################

db_prv_age <- db_prv %>% 
  filter(Sex == "b") %>% 
  select(Region, Code, Date, Age, Cases, Deaths) %>% 
  mutate(date_f = dmy(Date)) %>%
  arrange(Region, date_f, Age)

##########################
# temporal adjustments

# provisionally with 20-year age groups until 07.07.2020, not needed anymore when 
# Canadian data will pass through the pipeline
# for now better to exclude all national before 07.07.2020

# db_nal_age20 <- db_nal %>% 
#   mutate(date_f = dmy(Date)) %>% 
#   filter(Age != "TOT",
#          date_f <= "2020-07-07") %>% 
#   mutate(Age = as.numeric(Age) - as.numeric(Age) %% 20,
#          Sex = "b") %>% 
#   group_by(Region, Code, Date, Age, Measure, date_f) %>% 
#   summarise(Value = sum(Value)) %>% 
#   ungroup()

db_nal_age <- db_nal %>% 
  mutate(date_f = dmy(Date)) %>% 
  filter(Age != "TOT",
         date_f > "2020-07-07") %>% 
  mutate(Sex = "b") %>% 
  group_by(Region, Code, Date, Age, Measure, date_f) %>% 
  summarise(Value = sum(Value)) %>% 
  mutate(Age = as.integer(Age)) %>% 
  ungroup() %>% 
  spread('Measure', 'Value')
  
# db_nal_age <- bind_rows(db_nal_age20, db_nal_age10) %>% 
#   spread('Measure', 'Value') %>% 
#   mutate(date_f = dmy(Date)) %>% 
#   arrange(date_f, Age)

# Provintial data with closing age group at 80+
db_prv_age <- db_prv_age %>% 
  mutate(Age = ifelse(Age >= 80, 80, Age)) %>% 
  group_by(Region, Code, Date, Age, date_f) %>% 
  summarise(Cases = sum(Cases),
            Deaths = sum(Deaths)) %>% 
  ungroup()

# Quebec with problems in dates 16.05 and 23.05
db_prv_age <- db_prv_age %>% 
  filter(!(Region == "Quebec" & Date %in% c("16.05.2020", "23.05.2020")))

# exclusion of Mtl last dates, which have wrong data
db_prv_age <- db_prv_age %>% 
  filter(!(Region == "Montreal" & date_f >= "2020-10-09"))

# adjustment of BC deaths which are mutiplied by 100
# db_can <- db_can %>% 
#   mutate(Deaths = ifelse(Code == "CA_BC" & Date %in% c("28.04.2020", "29.04.2020"), Deaths / 100, Deaths))


##########################

# binding national and provintial
db_can_age <- bind_rows(db_prv_age, db_nal_age) %>% 
  drop_na()

# estimating new cases and new deaths
db_can_age2 <- db_can_age %>% 
  arrange(Region, Age, date_f) %>% 
  group_by(Region, Age) %>% 
  mutate(new_c = Cases - lag(Cases),
         new_d = Deaths - lag(Deaths)) %>% 
  ungroup()

# plotting cumulative cases 
db_can_age2 %>% 
  filter(date_f >= "2020-03-01") %>% 
  drop_na(Cases) %>% 
  ggplot()+
  geom_point(aes(date_f, Cases, col = Region), size = 0.7, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  facet_grid(~ Age)+
  labs(title = "Cumulative cases over time")+
  theme_bw()

ggsave("Figures/age_cum_cases_over_time.png")

# plotting cumulative deaths
db_can_age2 %>% 
  filter(date_f >= "2020-03-01") %>% 
  drop_na(Deaths) %>% 
  ggplot()+
  geom_point(aes(date_f, Deaths, col = Region), size = 0.7, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  facet_grid(~ Age)+
  labs(title = "Cumulative deaths over time")+
  theme_bw()

ggsave("Figures/age_cum_deaths_over_time.png")

# plotting new cases 
db_can_age2 %>% 
  filter(date_f >= "2020-03-01", new_c >= 0 & new_c <= 3000) %>% 
  drop_na(new_c) %>% 
  ggplot()+
  geom_point(aes(date_f, new_c, col = Region), size = 0.7, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  facet_grid(~ Age)+
  labs(title = "New cases over time")+
  theme_bw()

ggsave("Figures/age_new_cases_over_time.png")

# plotting new deaths
db_can_age2 %>% 
  filter(date_f >= "2020-03-01", new_d >= 0 & new_d <= 1000) %>% 
  drop_na(new_d) %>% 
  ggplot()+
  geom_point(aes(date_f, new_d, col = Region), size = 0.7, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  facet_wrap(~ Age, scales = "free")+
  labs(title = "New deaths over time")+
  theme_bw()

ggsave("Figures/age_new_deaths_over_time.png")

# CFRs
db_can_age3 <- db_can_age2 %>% 
  mutate(CFR = Deaths / Cases)

# plotting CFR over time
db_can_age3 %>% 
  drop_na(CFR) %>% 
  filter(date_f >= "2020-03-01", CFR <= 1) %>% 
  ggplot()+
  geom_point(aes(date_f, CFR, col = Region), size = 0.7, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  facet_wrap(~ Age, scales = "free")+
  labs(title = "Overall CFR over time")+
  theme_bw()

ggsave("Figures/age_CFR_over_time.png")


# some ages >= 50
db_can_age3 %>% 
  drop_na(CFR) %>% 
  filter(date_f >= "2020-03-01", CFR <= 1, Age >= 50,
         !(Age == 50 & CFR > 0.03),
         !(Age == 60 & CFR > 0.10),
         !(Age == 80 & CFR > 0.80)) %>% 
  ggplot()+
  geom_point(aes(date_f, CFR, col = Region), size = 0.7, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  facet_wrap(~ Age, scales = "free")+
  labs(title = "Overall CFR over time")+
  theme_bw()

ggsave("Figures/age_over_50_CFR_over_time.png")

db_can_age3 %>% 
  drop_na(Deaths, Cases) %>% 
  filter(date_f >= "2020-03-01") %>% 
  ggplot()+
  geom_point(aes(Cases, Deaths, col = Region), size = 0.7, alpha = 0.8)+
  facet_wrap(~ Age, scales = "free")+
  # scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  labs(title = "Slopes of overall CFR over time")+
  theme_bw()

ggsave("Figures/age_all_CFR_slopes.png")



