rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)
library(ggplot2)

# reading Canada data
db_can <- read_rds("Output/canada_cases_deaths.rds")
unique(db_can$Sex)
unique(db_can$Region)

# plotting cumulative cases 
db_can %>% 
  filter(Date >= "2020-03-01",
         Sex == "b") %>% 
  drop_na(Cases) %>% 
  ggplot()+
  geom_point(aes(Date, Cases, col = Region), size = 0.7, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  labs(title = "Cumulative cases over time")+
  theme_bw()

ggsave("Figures/cum_cases_over_time.png")

# plotting cumulative deaths
db_can %>% 
  filter(Date >= "2020-03-01",
         Sex == "b") %>% 
  drop_na(Deaths) %>% 
  ggplot()+
  geom_point(aes(Date, Deaths, col = Region), size = 0.9, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  labs(title = "Cumulative deaths over time")+
  theme_bw()

ggsave("Figures/cum_deaths_over_time.png")

# plotting new cases 
db_can %>% 
  filter(Date >= "2020-03-01",
         Sex == "b") %>% 
  drop_na(new_c) %>% 
  ggplot()+
  geom_point(aes(Date, new_c, col = Region), size = 0.9, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  labs(title = "New cases over time")+
  theme_bw()

ggsave("Figures/new_cases_over_time.png")

# plotting new cases (censored)
db_can %>% 
  filter(Date >= "2020-03-01",
         Sex == "b") %>% 
  drop_na(new_c) %>% 
  ggplot()+
  geom_point(aes(Date, new_c, col = Region), size = 0.9, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  scale_y_continuous(limits = c(0, 2000))+
  labs(title = "New cases over time (censored at 2000)")+
  theme_bw()

ggsave("Figures/new_cases_over_time_(censored).png")

# plotting new deaths
db_can %>% 
  filter(Date >= "2020-03-01",
         Sex == "b",
         new_d >= 0) %>% 
  drop_na(new_d) %>% 
  ggplot()+
  geom_point(aes(Date, new_d, col = Region), size = 0.9, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  labs(title = "New deaths over time")+
  theme_bw()

ggsave("Figures/new_deaths_over_time.png")

# plotting new deaths (censored)
db_can %>% 
  filter(Date >= "2020-03-01",
         Sex == "b") %>% 
  drop_na(new_d) %>% 
  ggplot()+
  geom_point(aes(Date, new_d, col = Region), size = 0.9, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  scale_y_continuous(limits = c(0, 20))+
  labs(title = "New deaths over time (censored at 20)")+
  theme_bw()

ggsave("Figures/new_deaths_over_time_(censored).png")

# plotting CFR over time
db_can %>% 
  drop_na(CFR) %>% 
  filter(Date >= "2020-03-01",
         Sex == "b",
         CFR < 1) %>% 
  ggplot()+
  geom_point(aes(Date, CFR, col = Region), size = 0.7, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  labs(title = "Overall CFR over time")+
  theme_bw()

ggsave("Figures/all_CFR_over_time.png")

db_can %>% 
  drop_na(Deaths, Cases) %>% 
  filter(Date >= "2020-03-01",
         Sex == "b") %>% 
  ggplot()+
  geom_point(aes(Cases, Deaths, col = Region), size = 0.7, alpha = 0.8)+
  # scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  labs(title = "Slopes of overall CFR over time")+
  theme_bw()

ggsave("Figures/all_CFR_slopes.png")

######################################################
# by age both sexes
######################################################

db_can_age <- read_rds("Output/cfr_by_age_sex.rds")

# plotting cumulative cases 
db_can_age %>% 
  filter(Date >= "2020-03-01",
         Sex == "b") %>% 
  drop_na(Cases) %>% 
  ggplot()+
  geom_point(aes(Date, Cases, col = Region), size = 0.7, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  facet_wrap(~ Age, scales = "free")+
  labs(title = "Cumulative cases over time")+
  theme_bw()


ggsave("Figures/age_cum_cases_over_time.png")

# plotting cumulative deaths
db_can_age %>% 
  filter(Date >= "2020-03-01",
         Sex == "b") %>% 
  drop_na(Deaths) %>% 
  ggplot()+
  geom_point(aes(Date, Deaths, col = Region), size = 0.7, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  facet_wrap(~ Age, scales = "free")+
  labs(title = "Cumulative deaths over time")+
  theme_bw()

ggsave("Figures/age_cum_deaths_over_time.png")

# plotting new cases 
db_can_age %>% 
  filter(Date >= "2020-03-01",
         !(Region == "Quebec" & Date == "2020-10-13"),
         Sex == "b", 
         new_c >= 0 & new_c <= 3000) %>% 
  drop_na(new_c) %>% 
  ggplot()+
  geom_point(aes(Date, new_c, col = Region), size = 0.7, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  facet_wrap(~ Age, scales = "free")+
  labs(title = "New cases over time")+
  theme_bw()

ggsave("Figures/age_new_cases_over_time.png")

# plotting new deaths
db_can_age %>% 
  filter(Date >= "2020-03-01",
         Sex == "b", 
         new_d >= 0 & new_d <= 1000) %>% 
  drop_na(new_d) %>% 
  ggplot()+
  geom_point(aes(Date, new_d, col = Region), size = 0.7, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  facet_wrap(~ Age, scales = "free")+
  labs(title = "New deaths over time")+
  theme_bw()

ggsave("Figures/age_new_deaths_over_time.png")


# plotting age-specific CFR over time
db_can_age %>% 
  drop_na(CFR) %>% 
  filter(Date >= "2020-03-01",
         Sex == "b", 
         CFR <= 1) %>% 
  ggplot()+
  geom_point(aes(Date, CFR, col = Region), size = 0.7, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  facet_wrap(~ Age, scales = "free")+
  labs(title = "Age-specific CFR over time")+
  theme_bw()

ggsave("Figures/age_CFR_over_time.png")

# plotting age-specific CFR over time (scales adjusted)
db_can_age %>% 
  drop_na(CFR) %>% 
  filter(Date >= "2020-03-01",
         Sex == "b", 
         CFR <= 1,
         !(Age == 30 & CFR > 0.01),
         !(Age == 40 & CFR > 0.03),
         !(Age == 50 & CFR > 0.03),
         !(Age == 60 & CFR > 0.10),
         !(Age == 80 & CFR > 0.80)) %>% 
  ggplot()+
  geom_point(aes(Date, CFR, col = Region), size = 0.7, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  facet_wrap(~ Age, scales = "free")+
  labs(title = "Age-specific CFR over time (censored)")+
  theme_bw()

ggsave("Figures/age_CFR_over_time_(censored).png")


# some ages >= 50
db_can_age %>% 
  drop_na(CFR) %>% 
  filter(Date >= "2020-03-01",
         Sex == "b", 
         CFR <= 1, Age >= 50,
         !(Age == 50 & CFR > 0.03),
         !(Age == 60 & CFR > 0.10),
         !(Age == 80 & CFR > 0.80)) %>% 
  ggplot()+
  geom_point(aes(Date, CFR, col = Region), size = 0.7, alpha = 0.8)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  facet_wrap(~ Age, scales = "free")+
  labs(title = "Age-specific CFR over time")+
  theme_bw()

ggsave("Figures/age_over_50_CFR_over_time.png")

db_can_age %>% 
  drop_na(Deaths, Cases) %>% 
  filter(Date >= "2020-03-01",
         Sex == "b") %>% 
  ggplot()+
  geom_point(aes(Cases, Deaths, col = Region), size = 0.7, alpha = 0.8)+
  facet_wrap(~ Age, scales = "free")+
  # scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  labs(title = "Slopes of overall CFR over time")+
  theme_bw()

ggsave("Figures/age_all_CFR_slopes.png")



