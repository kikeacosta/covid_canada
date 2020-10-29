rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)
library(readxl)
source("Code/00_functions.R")

# reading mortality and population data from StatCan files
deaths <- read_csv(unzip("Data/13100768-eng.zip", "13100768.csv"))
pop <- read_csv(unzip("Data/17100005-eng.zip", "17100005.csv"))


# adjusting mortality data
###########################

deaths2 <- deaths %>% 
  rename(Region = GEO,
         Date = REF_DATE,
         Age = 'Age at time of death',
         Deaths = VALUE) %>% 
  select(Region, Date, Age, Sex, Deaths) %>% 
  mutate(Region = str_remove(Region, ", place of occurrence"),
         Age = str_remove(Age, "Age at time of death, "),
         Age = str_trim(str_sub(Age, 1, 2)),
         Age = ifelse(Age == "al", "All", Age),
         Sex = case_when(Sex == "Both sexes" ~ "b",
                         Sex == "Males" ~ "m",
                         Sex == "Females" ~ "f"),
         Year = year(Date))


unique(deaths2$Age)
unique(deaths2$Region)

r <- "Alberta"
a <- "85"

deaths2 %>% 
  filter(Region == r,
         Age == a,
         Sex == "b") %>% 
  ggplot()+
  geom_line(aes(Date, Deaths))



# adjusting population data
###########################
age_excls <- c("18 years and over", 
               "65 years and over", 
               "90 years and over", 
               "Median age", 
               "Average age")

pop2 <- pop %>% 
  rename(Region = GEO,
         Year = REF_DATE,
         Age = 'Age group',
         Exposure = VALUE) %>% 
  select(Region, Year, Age, Sex, Exposure) %>% 
  filter(!str_detect(Age, " to "),
         !(Age %in% age_excls),
         Year >= 2009) %>% 
  mutate(Age = str_remove_all(Age, c(" years and over| years| year")),
         Age = ifelse(Age == "All ages", "All", Age),
         Sex = case_when(Sex == "Both sexes" ~ "b",
                         Sex == "Males" ~ "m",
                         Sex == "Females" ~ "f"))


pop3 <- pop2 %>% 
  filter(Age != "All") %>% 
  mutate(Age_gr = case_when(Age <= 44 ~ "0",
                            Age >= 45 & Age <= 64 ~ "45",
                            Age >= 65 & Age <= 84 ~ "65",
                            Age >= 85 ~ "85")) %>% 
  group_by(Region, Year, Sex, Age_gr) %>% 
  summarise(Exposure = sum(Exposure)) %>% 
  ungroup() %>% 
  rename(Age = Age_gr) %>% 
  bind_rows(pop2 %>% 
              filter(Age == "All"))

unique(pop3$Age)
unique(pop3$Region)

r <- "Alberta"
a <- "85"

pop3 %>% 
  filter(Region == r,
         Age == a,
         Sex == "b") %>% 
  ggplot()+
  geom_line(aes(Year, Exposure))

db_canada <- deaths2 %>% 
  left_join(pop3)

write_rds(db_canada, "Output/weekly_canada.rds")

