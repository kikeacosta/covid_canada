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

# Canadian data
db_nal <- read_rds("Data/201029_covid_canada.rds") %>% 
  mutate(Code = "CA", Region = "All") %>% 
  rename(Date = date) %>% 
  select(Region, Code, Date, Sex, Age, Measure, Value)

# Toronto data
db_t1 <- read_csv("Data/Toronto/Toronto_Cases_Deaths_df_01.07.2020.csv")
db_t2 <- read_csv("Data/Toronto/Toronto_Cases_Deaths_df_24.10.2020.csv")

# OSF Data - Output_10
osf_retrieve_file("43ucn") %>%
  osf_download(path = "Data/", conflicts = "overwrite") 

db_cov <-  read_csv("Data/Output_10.zip",
                    skip = 3,
                    col_types = "ccccciiddd")

# looking for regions included in COVerAGE-DB
db_cov %>% 
  filter(Country == "Canada") %>% 
  mutate(Code = str_replace(Code, Date, ""),
         Date = dmy(Date)) %>%
  pull(Code) %>% 
  unique()

# only keeping regions from regional sources
db_prv <- db_cov %>% 
  filter(Country == "Canada") %>% 
  mutate(Code = str_replace(Code, Date, ""),
         Date = dmy(Date)) %>% 
  select(-Tests)

# common dates for all regions
comm_dates <- db_prv %>% 
  drop_na() %>% 
  select(Region, Date) %>% 
  unique() %>% 
  mutate(id = 1) %>% 
  spread(Region, id)

unique(db_prv$Region)
unique(db_prv$Code)
unique(db_prv$Sex)

# last date of each province
db_prv %>% 
  group_by(Code) %>% 
  summarise(last_date = max(Date))

######################################################
# looking at all population
######################################################

db_prv_all <- db_prv %>% 
  # filter(Sex == "b") %>% 
  group_by(Region, Sex, Code, Date) %>% 
  summarise(Cases = sum(Cases),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  arrange(Region, Date)

db_nal_all <- db_nal %>% 
  filter(Age == "TOT") %>% 
  spread('Measure', 'Value') %>% 
  select(Region, Sex, Code, Date, Cases, Deaths) %>% 
  arrange(Date)
  
db_can <- bind_rows(db_prv_all, db_nal_all)

##########################
# temporal adjustments 
# exclusion of Mtl last dates, which have wrong data
db_can <- db_can %>% 
  filter(!(Region == "Montreal" & Date >= "2020-10-09"))

# adjustment of BC deaths which are mutiplied by 100
db_can <- db_can %>% 
  mutate(Deaths = ifelse(Code == "CA_BC" & Date %in% ymd(c("2020-04-28", "2020-04-29")), Deaths / 100, Deaths))

##########################

# estimating new cases and new deaths
db_can2 <- db_can %>% 
  arrange(Region, Sex, Date) %>% 
  group_by(Region, Sex) %>% 
  mutate(new_c = Cases - lag(Cases),
         new_d = Deaths - lag(Deaths)) %>% 
  ungroup()

# CFRs
db_can3 <- db_can2 %>% 
  mutate(CFR = Deaths / Cases)

# saving database
write_rds(db_can3, "Output/canada_cases_deaths.rds")


######################################################
######################################################
# by age and sex
######################################################
######################################################

db_prv_age <- db_prv %>% 
  select(Region, Sex, Code, Date, Age, Cases, Deaths) %>% 
  arrange(Region, Sex, Date, Age)

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
  filter(Age != "TOT",
         Date > "2020-07-07") %>% 
  group_by(Region, Code, Sex, Date, Age, Measure) %>% 
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
  group_by(Region, Code, Sex, Date, Age) %>% 
  summarise(Cases = sum(Cases),
            Deaths = sum(Deaths)) %>% 
  ungroup()

# Quebec with problems in dates 16.05 and 23.05
db_prv_age <- db_prv_age %>% 
  filter(!(Region == "Quebec" & Date %in% ymd(c("2020-05-16", "2020-05-23"))))

# exclusion of Mtl last dates, which have wrong data
db_prv_age <- db_prv_age %>% 
  filter(!(Region == "Montreal" & Date >= "2020-10-09"))

# adjustment of BC deaths which are mutiplied by 100
# db_can <- db_can %>% 
#   mutate(Deaths = ifelse(Code == "CA_BC" & Date %in% c("28.04.2020", "29.04.2020"), Deaths / 100, Deaths))


##########################

# binding national and provintial
db_can_age <- bind_rows(db_prv_age, db_nal_age) %>% 
  drop_na()

# estimating new cases and new deaths
db_can_age2 <- db_can_age %>% 
  arrange(Region, Sex, Age, Date) %>% 
  group_by(Region, Sex, Age) %>% 
  mutate(new_c = Cases - lag(Cases),
         new_d = Deaths - lag(Deaths)) %>% 
  ungroup()

# CFRs
db_can_age3 <- db_can_age2 %>% 
  mutate(CFR = Deaths / Cases)

write_rds(db_can_age3, "Output/cfr_by_age_sex.rds")





#######################################
# Other countries and cities to compare
#######################################

# Toronto
#########

db_t1_2 <- db_t1 %>% 
  mutate(Code = str_replace(Code, Date, ""),
         Date = dmy(Date)) %>% 
  group_by() %>% 
  filter(Date == max(Date))

db_t2_2 <- db_t2 %>% 
  mutate(Code = str_replace(Code, Date, ""),
         Date = dmy(Date)) %>% 
  group_by() %>% 
  filter(Date == max(Date))

db_t3 <- bind_rows(db_t1_2, db_t2_2) %>% 
  spread(Measure, Value) %>% 
  mutate(Age = as.integer(Age)) %>% 
  drop_na()


# Rest
######

db_cov2 <- db_cov %>% 
  mutate(Code = str_replace(Code, Date, ""),
         Date = dmy(Date)) 

last_dates <- db_cov2 %>%  
  group_by(Country, Region, Code) %>% 
  summarise(last_date = max(Date))

cds <- c("DE_",
         "DK",
         "NL",
         "SE",
         "ITbol",
         "GB_EN",
         "BE",
         "US_NYC",
         "DE_BE_",
         "US")

db_cov3 <- db_cov2 %>% 
  filter(Code %in% cds) %>% 
  select(-Tests) %>% 
  drop_na()

unique(db_cov3$Code)

db_cov3 %>% 
  select(Code, Date) %>% 
  unique() %>% 
  filter(Date == "2020-07-09")

dates <- db_cov3 %>% 
  select(Code, Date) %>% 
  unique() %>% 
  filter(Date >= "2020-06-25" & Date <= "2020-07-15")

cts_sample <- db_cov3 %>% 
  filter(((Code == "DE_" | 
             Code == "BE" | 
             Code == "DK" | 
            Code == "SE" | 
            Code == "US_NYC" | 
            Code == "DE_BE_") & 
           Date == "2020-07-09") | 
           (Code == "ITbol" & Date == "2020-07-07") | 
           (Code == "NL" & Date == "2020-07-05") | 
           (Code == "US" & Date == "2020-06-27")) %>% 
  bind_rows(db_t3) %>% 
  mutate(CFR = Deaths / Cases,
         Region = case_when(Code == "BE" ~ "Belgium",
                            Code == "DK" ~ "Denmark",
                            Code == "DE_" ~ "Germany",
                            Code == "ITbol" ~ "Italy",
                            Code == "NL" ~ "Netherlands",
                            Code == "SE" ~ "Sweden",
                            Code == "US" ~ "USA",
                            TRUE ~ Region)) %>% 
  select(Region, Code, Date, Sex, Age, Cases, Deaths, CFR)

unique(cts_sample$Code)
unique(cts_sample$Region)
unique(cts_sample$Sex)




unique(db_can_tojoin$Sex)
unique(db_can_tojoin$Age)

unique(db_can$Sex)




db_can_tojoin1 <- db_can %>% 
  filter(Region == "All",
         Sex == "b") %>% 
  group_by(Region, Date) %>% 
  summarise(Cases = sum(Cases),
            Deaths = sum(Deaths)) %>%
  ungroup() %>% 
  mutate(Region = "Canada",
         Code = "CA") %>% 
  group_by(Region) %>% 
  mutate(new_c = Cases - lag(Cases),
         new_d = Deaths - lag(Deaths)) %>% 
  ungroup() %>% 
  drop_na()

           
db_cov4 <- db_cov3 %>% 
  mutate(Region = case_when(Code == "BE" ~ "Belgium",
                            Code == "DK" ~ "Denmark",
                            Code == "DE_" ~ "Germany",
                            Code == "ITbol" ~ "Italy",
                            Code == "NL" ~ "Netherlands",
                            Code == "SE" ~ "Sweden",
                            Code == "US" ~ "USA",
                            TRUE ~ Region)) %>% 
  select(Region, Code, Date, Sex, Age, Cases, Deaths) %>% 
  filter(Sex == "b") %>% 
  group_by(Region, Code, Date) %>% 
  summarise(Cases = sum(Cases),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  arrange(Region, Date) %>% 
  group_by(Region) %>% 
  mutate(new_c = Cases - lag(Cases),
         new_d = Deaths - lag(Deaths)) %>% 
  ungroup() %>% 
  drop_na() %>% 
  bind_rows(db_can_tojoin1)

unique(db_cov4$Sex)  
unique(db_cov4$Code)  

library("HMDHFDplus")

# Username of HMD
hmd_name <- "kikepaila@gmail.com"
# Password of HMD
hmd_pass <- "secreto"


country_list <- c("USA", "BEL", "SWE", "DNK", "DEUTNP", "NLD", "ITA", "CAN")
country <- "USA"
db_es <- NULL

for (country in country_list) {
  # Extracting data from HMD ####
  # deaths 1x1 by country
  # exposures 1x1 by country
  db_e <- readHMDweb(CNTRY = country, 
                     item = "Exposures_1x1", 
                     username = hmd_name, 
                     password = hmd_pass,
                     fixup = TRUE) %>% 
    as_tibble() %>% 
    select(Year, Age, Total) %>% 
    group_by() %>% 
    filter(Year == max(Year)) %>% 
    summarise(Exposure = sum(Total)) %>% 
    mutate(Region = country) %>% 
    ungroup()
  db_es <- db_es %>% 
    bind_rows(db_e)
}


db_es2 <- db_es %>% 
  mutate(Region = case_when(Region == "BEL" ~ "Belgium",
                            Region == "DNK" ~ "Denmark",
                            Region == "DEUNTP" ~ "Germany",
                            Region == "ITA" ~ "Italy",
                            Region == "NLD" ~ "Netherlands",
                            Region == "SWE" ~ "Sweden",
                            Region == "USA" ~ "USA",
                            Region == "CAN" ~ "Canada",
                            TRUE ~ Region))


db_cov5 <- db_cov4 %>% 
  left_join(db_es2) %>% 
  drop_na()

unique(db_cov5$Region)

write_rds(cts_sample, "Output/other_regions_by_age_sex.rds")
write_rds(db_cov5, "Output/other_regions_all.rds")


