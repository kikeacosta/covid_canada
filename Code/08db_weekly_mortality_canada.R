rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)
library(readxl)
source("Code/00_functions.R")

# reading mortality and population data from StatCan files
deaths <- read_csv("Data/201113_13100768-eng.zip",
                   col_types = cols(.default = "c"))

qc <- read_csv2("Data/DecesSemaine_QC_2010-2020_GrAge.csv",
                skip = 5) %>% 
  rename(Year = 1,
         Age = 3) %>% 
  drop_na(Age) %>% 
  select(-Statut)

pop <- read_csv("Data/17100005-eng.zip",
                col_types = cols(.default = "c"))


# adjusting mortality data
###########################
deaths2 <- deaths %>% 
  rename(Region = GEO,
         Date = REF_DATE,
         Age = 'Age at time of death',
         Deaths = VALUE) %>% 
  select(Region, Date, Age, Sex, Deaths) %>% 
  mutate(Region = str_remove(Region, ", place of occurrence"),
         Date = ymd(Date),
         Age = str_remove(Age, "Age at time of death, "),
         Age = str_trim(str_sub(Age, 1, 2)),
         Age = ifelse(Age == "al", "All", Age),
         Sex = case_when(Sex == "Both sexes" ~ "b",
                         Sex == "Males" ~ "m",
                         Sex == "Females" ~ "f"),
         Year = year(Date),
         Deaths = as.integer(Deaths))


unique(deaths2$Age)
unique(deaths2$Region)

r <- "Quebec"
a <- "85"

deaths2 %>% 
  filter(Region == r,
         Age == a,
         Sex == "b") %>% 
  ggplot()+
  geom_line(aes(Date, Deaths))


weeks <- deaths2 %>% 
  filter(Region == r,
         Age == a,
         Sex == "b") %>% 
  select(Date) %>%
  mutate(Year = year(Date)) %>% 
  group_by(Year) %>% 
  mutate(Week = 1:n())

# Quebec data
qc2 <- qc %>% 
  gather(-Year, -Age, key = "Week", value = "Deaths") %>% 
  mutate(Age = str_sub(Age, 1, 2),
         Age = case_when(Age == "0-" ~ "0",
                         Age == "To" ~ "All",
                         TRUE ~ Age),
         Deaths = as.integer(str_replace(Deaths, " ", "")),
         Year = as.integer(Year),
         Week = as.integer(Week),
         Region = "Quebec_isq",
         Sex = "b") %>% 
  arrange(Age, Year, Week)


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
                         Sex == "Females" ~ "f"),
         Exposure = as.integer(Exposure),
         Year = as.integer(Year))

# population by age for Canada data
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


# population by age for Quebec data

pop4 <- pop2 %>% 
  filter(Age != "All") %>% 
  mutate(Age_gr = case_when(Age <= 49 ~ "0",
                            Age >= 50 & Age <= 69 ~ "50",
                            Age >= 70 ~ "70")) %>% 
  group_by(Region, Year, Sex, Age_gr) %>% 
  summarise(Exposure = sum(Exposure)) %>% 
  ungroup() %>% 
  rename(Age = Age_gr) %>% 
  bind_rows(pop2 %>% 
              filter(Age == "All"))

db_canada <- deaths2 %>% 
  left_join(pop3)



write_rds(db_canada, "Output/weekly_canada.rds")

