rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")
detach(package:MASS)

library(tidyverse)
library(lubridate)
library(readxl)
source("Code/00_functions.R")

# provisionally, only for the main provinces and national

# loading mortality data from StatCan files
# https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1310076801
deaths <- read_csv("Data/201113_13100768-eng.zip",
                   col_types = cols(.default = "c"))

# loading mortality from ISQ
# "https://statistique.quebec.ca/docs-ken/multimedia/DecesSemaine_QC_2010-2020_GrAge.xlsx"
qc <- read_xlsx("Data/DecesSemaine_QC_2010-2020_GrAge.xlsx",
                skip = 5) %>% 
  rename(Year = 1,
         Age = 3) %>% 
  drop_na(Age) %>% 
  select(-Statut)

# loading population data from StatCan files
pop <- read_csv("Data/17100005-eng.zip",
                col_types = cols(.default = "c"))


# weeks and dates
#################
weeks <- expand_grid(Year = seq(2010, 2020, 1), Week = seq(1, 52, 1)) %>% 
  bind_rows(tibble(Year = c(2014, 2020), Week = 53)) %>% 
  arrange(Year, Week)

# weeks dates before 2020
dates_w <- deaths2 %>% 
  filter(Region == r,
         Age == a,
         Sex == "b",
         Date < "2020-01-04") %>% 
  select(Date) 

# week 1 2020
first_date_w_2020 <- ymd("2020-01-04")

dates_w2 <- dates_w %>% 
  bind_rows(tibble(Date = first_date_w_2020 + 7 * seq(0, 52, 1)))

weeks2 <- bind_cols(dates_w2, weeks) %>% 
  mutate(t = 1:n())

write_csv(weeks2, "Output/date_weeks_2010_2020.csv")



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
  arrange(Age, Year, Week) %>% 
  drop_na() %>% 
  left_join(weeks2, by = c("Year", "Week"))

deaths3 <- deaths2 %>% 
  left_join(weeks2, by = c("Date"))

deaths_all <- bind_rows(deaths3, qc2)

unique(deaths_all$Region)


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
         Year = as.integer(Year),
         Week = 26) %>% 
  left_join(weeks2) %>% 
  mutate(t = ifelse(Year == 2009, -26, t))

