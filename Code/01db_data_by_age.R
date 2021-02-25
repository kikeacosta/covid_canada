rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")
library(tidyverse)
library(readr)
library(lubridate)
library(ggplot2)
library(osfr)

# reading Canada data
#####################


# OSF Data - Output_5
osf_retrieve_file("7tnfh") %>%
  osf_download(path = "Data/", conflicts = "overwrite")

db_cov <-  read_csv("Data/Output_5.zip",
                    skip = 3)

# adjusted data from Ontario and Alberta
db_on_ab <- read_rds("Output/db_on_to_ab_cases&deaths.rds")


# looking for regions included in COVerAGE-DB
db_cov %>% 
  filter(Country == "Canada") %>% 
  mutate(Code = str_replace(Code, Date, ""),
         Date = dmy(Date)) %>%
  pull(Code) %>% 
  unique()

# only keeping regions from Canada
db_prv <- db_cov %>% 
  filter(Country == "Canada") %>% 
  mutate(Code = str_replace(Code, Date, ""),
         Date = dmy(Date)) %>% 
  select(-Tests, -AgeInt) 

# data availability by region
#############################

# last date of each province
db_prv %>% 
  group_by(Code) %>% 
  summarise(last_date = max(Date))

# common dates for all regions
comm_dates <- db_prv %>% 
  drop_na() %>% 
  select(Region, Date) %>% 
  unique() %>% 
  mutate(id = 1) %>% 
  spread(Region, id)

##############################################################################

# as the 1st wave of the pandemic wade out at mid-July, we selected July 15th, 
# Only BC has no data for the 15, but it has for the 16

##############################################################################

d1 <- "2020-07-15"

unique(db_prv$Region)
unique(db_prv$Code)
unique(db_prv$Sex)

db_prv2 <- db_prv %>% 
  filter(Date == d1,
         Code != "CA_BC") %>% 
  bind_rows(db_prv %>% 
              filter(Code == "CA_BC", 
                     Sex == "b", 
                     Date == "2020-07-16"))

# Provisional adjustment for Ontario and Alberta data
# replace values from the adjusted database

db_on_ab2 <- db_on_ab %>% 
  mutate(Code = case_when(Region == "Alberta" ~ "CA_AB",
                            Region == "Ontario" ~ "CA_ON",
                            Region == "Toronto" ~ "CA_TOR"),
         Date = ymd(Date)) %>% 
  spread(Measure, Value)

db_on_ab_all_sex <- db_on_ab2 %>% 
  group_by(Region, Code, Date, Age) %>% 
  summarise(Cases = sum(Cases),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Sex = "b")

db_on_ab3 <- db_on_ab2 %>% 
  bind_rows(db_on_ab_all_sex)

db_prv3 <- db_prv2 %>% 
  filter(!(Region %in% c("Alberta", "Ontario", "Toronto"))) %>% 
  bind_rows(db_on_ab3) %>% 
  mutate(Region = ifelse(Region == "All", "Canada", Region),
         Country = "Canada") 

# write_rds(db_prv4, "Output/cfr_by_age_sex.rds")

#######################################
# Other countries and cities to compare ------------------------------
#######################################

db_cov2 <- db_cov %>% 
  mutate(Code = str_replace(Code, Date, ""),
         Date = dmy(Date)) 

last_dates <- db_cov2 %>%  
  group_by(Country, Region, Code) %>% 
  summarise(last_date = max(Date))

# library(covidAgeData)
# cts <- c("Denmark", "Germany", "Italy", "Netherlands", "Spain", "Sweden", "Switzerland", "USA")

# SCm <- read_subset_covid(
#   zippath = "Data/Output_5.zip",
#   data = "Output_5",
#   Country = cts,
#   Sex = "b")

# # German data for 15-07-2020
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # "https://docs.google.com/spreadsheets/d/1ojjUdXSYLG6wwY5aGS48QBKc_56z8gbgoewYILC1owE/edit#gid=1067163235"
# db_ger <- read_sheet("https://docs.google.com/spreadsheets/d/1ojjUdXSYLG6wwY5aGS48QBKc_56z8gbgoewYILC1owE/edit#gid=1067163235")
# 
# db_ger2 <- db_ger %>% 
#   mutate(Sex = case_when(Geschlecht == "M" ~ "m",
#                          Geschlecht == "W" ~ "f",
#                          Geschlecht == "unbekannt" ~ "UNK"),
#          Age = case_when(Altersgruppe == "A00-A04" ~ "0",
#                          Altersgruppe == "A05-A14" ~ "5",
#                          Altersgruppe == "A15-A34" ~ "15",
#                          Altersgruppe == "A35-A59" ~ "35",
#                          Altersgruppe == "A60-A79" ~ "60",
#                          Altersgruppe == "A80+" ~ "80",
#                          Altersgruppe == "unbekannt" ~ "UNK"),
#          date_f = ymd(str_sub(Meldedatum, 1, 10)),
#          Cases = ifelse(AnzahlFall < 0, 0, AnzahlFall),
#          Deaths = ifelse(AnzahlTodesfall < 0, 0, AnzahlTodesfall),
#          Region = Bundesland) %>% 
#   select(Sex, Age, Cases, Deaths, Region) %>% 
#   pivot_longer(Cases:Deaths, names_to = "Measure", values_to ="Value") %>% 
#   group_by(Region, Sex, Measure, Age) %>% 
#   summarize(Value = sum(Value)) %>% 
#   ungroup()
# 
# db_ger_all <- db_ger2 %>% 
#   group_by(Sex, Measure, Age) %>% 
#   summarize(Value = sum(Value)) %>% 
#   ungroup() %>% 
#   mutate(Region = "Germany")
# 
# db_berlin <- db_ger2 %>% 
#   filter(Region == "Berlin")
# 
# db_ger3 <- bind_rows(db_ger_all, db_berlin)
# 
# write_rds(db_ger3, "Output/germany_berlin_20200715.rds")

cds <- c("DE_",
         "DK",
         "ES_",
         "NL",
         "SE",
         "ITbol",
         "CH",
         "US_NYC",
         "DE_BE_",
         "US")

db_cov3 <- db_cov2 %>% 
  filter(Code %in% cds) %>% 
  select(-Tests, -AgeInt) %>% 
  drop_na()

unique(db_cov3$Country)

dates <- db_cov3 %>% 
  select(Code, Date) %>% 
  unique() %>% 
  filter(Date >= "2020-06-25" & Date <= "2020-07-30") %>% 
  mutate(id = 1) %>% 
  spread(Code, id)

#########################################
# 
# Available dates close to 15 July
# The Netherlands: 12 or 19 July,
# Italy: 14 July
# The US: 26 June
# 
#########################################

cts_sample <- db_cov3 %>% 
  filter(((Code == "DE_" | 
             Code == "CH" | 
             Code == "DK" | 
             Code == "ES_" | 
             Code == "SE" | 
             Code == "US_NYC" | 
             Code == "DE_BE_") & 
           Date == "2020-07-15") | 
           (Code == "ITbol" & Date == "2020-07-14") | 
           (Code == "NL" & Date == "2020-07-12") | 
           (Code == "US" & Date == "2020-06-27")) %>% 
  mutate(Region = case_when(Code == "CH" ~ "Switzerland",
                            Code == "DK" ~ "Denmark",
                            Code == "DE_" ~ "Germany",
                            Code == "ES_" ~ "Spain",
                            Code == "ITbol" ~ "Italy",
                            Code == "NL" ~ "Netherlands",
                            Code == "SE" ~ "Sweden",
                            Code == "US" ~ "USA",
                            TRUE ~ Region)) %>% 
  select(Region, Code, Date, Sex, Age, Cases, Deaths)

out <- db_prv3 %>% 
  bind_rows(cts_sample)

write_rds(out, "Output/covid_data_by_age_sex.rds")

