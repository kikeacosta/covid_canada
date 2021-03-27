# rm(list=ls())
source("Code/00_functions.R")

# Countries, provinces and cities to compare
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
countries <- c("CA", "DE", "CH", "DK", "ES", "SE", "IT", "NL", "US")
provinces <- c("CA_ON", "CA_BC", "CA_AB", "CA_QC", "CA_MB", "CA_SK")
cities <- c("ES_M", "US_NYC", "DE_BE", "CA_MTL", "CA_CAL", "CA_EDM", "CA_TNT", "CA_OTW")

# loading COVID data outside COVerAGE-DB 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_add <- read_rds("Output/additional_covid_harmonized.rds")

db_add2 <- db_add %>% 
  mutate(Code = recode(Region,
                       "Alberta" = "CA_AB",
                       "British Columbia" = "CA_BC",
                       "Ontario" = "CA_ON",
                       "Quebec" = "CA_QC",
                       "Toronto" = "CA_TNT",
                       "Canada" = "CA",
                       "Manitoba" = "CA_MB",
                       "Saskatchewan" = "CA_SK",
                       "Germany" = "DE",
                       "Berlin" = "DE_BE",
                       "Italy" = "IT",
                       "Edmonton" = "CA_EDM",
                       "Calgary" = "CA_CAL",
                       "Ottawa" = "CA_OTW"),
         Value = round(Value),
         Country = recode(Region,
                          "Alberta" = "Canada",
                          "British Columbia" = "Canada",
                          "Ontario" = "Canada",
                          "Quebec" = "Canada",
                          "Toronto" = "Canada",
                          "Canada" = "Canada",
                          "Manitoba" = "Canada",
                          "Saskatchewan" = "Canada",
                          "Germany" = "Germany",
                          "Berlin" = "Germany",
                          "Italy" = "Italy",
                          "Edmonton" = "Canada",
                          "Calgary" = "Canada")) %>%
  spread(Measure, Value) %>% 
  mutate(Sex = "b")

# Loading COVerAGE-DB data
# OSF Data - Output_5
# osf_retrieve_file("7tnfh") %>%
#   osf_download(path = "Data/", conflicts = "overwrite")

# db_cov <-  read_csv("Data/Output_5.zip",
#                     skip = 3)

# filtering coeuntries and Canadian provinces
cds <- c("DE_",
         "DK",
         "ES_",
         "ES_M_",
         "NL",
         "SE",
         "IT",
         "CH",
         "US_NYC",
         "DE_BE_",
         "US")

# db_cov2 <- db_cov %>% 
#   mutate(Code = str_replace(Code, Date, ""),
#          Date = dmy(Date)) %>% 
#   filter(Country == "Canada" | Code %in% cds) %>% 
#   select(-Tests, -AgeInt) %>% 
#   drop_na()
# 
# write_rds(db_cov2, "Data/backup/coverage_filtered_v20210220.rds")
db_cov2 <- read_rds("Data/backup/coverage_filtered_v20210220.rds")

db_cov3 <- db_cov2 %>% 
  filter(Sex == "b", 
         Region != "Alberta",
         Region != "Ontario",
         !(Country == "Germany" & Date <= "2020-12-31")) %>% 
  mutate(Region = ifelse(Region == "All", Country, Region),
         Region = recode(Region,
                         "NYC" = "New York City"),
         Code = recode(Code,
                       "ES_" = "ES",
                       "ES_M_" = "ES_M",
                       "DE_" = "DE",
                       "DE_BE_" = "DE_BE")) %>% 
bind_rows(db_add2)

unique(db_cov3$Code)

last_dates <- db_cov3 %>%  
  group_by(Country, Region, Code) %>% 
  summarise(last_date = max(Date))

obs <-
  db_cov3 %>% 
  filter(Age == 0) %>% 
  group_by(Country, Region) %>% 
  summarise(n())

# ~~~~~~~~~~~~~~~~~~~~~~~
# Canadian provinces data
# ~~~~~~~~~~~~~~~~~~~~~~~

db_prv <- db_cov3 %>% 
  filter(Country == "Canada") %>% 
  mutate(Region = ifelse(Region == "All", "Canada", Region))

db_prv2 <- bind_rows(db_prv) %>% 
  mutate(Code = recode(Code,
                       "CA_QC_" = "CA_QC"))

unique(db_prv2$Code)

# data availability by region
#############################
# last date of each province
db_prv2 %>% 
  group_by(Code) %>% 
  summarise(last_date = max(Date))

# common dates for all regions
comm_dates <- db_prv2 %>% 
  drop_na() %>% 
  select(Region, Date) %>% 
  unique() %>% 
  mutate(id = 1) %>% 
  spread(Region, id)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# End of the 1st wave of the pandemic in mid-July: we selected July 15th, 
# BC has no data for the 15, but it has for the 16
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

d1 <- "2020-07-15"

db_prv_1st <- db_prv2 %>% 
  filter(Date == d1,
         Code != "CA_BC") %>% 
  bind_rows(db_prv %>% 
              filter(Code == "CA_BC", 
                     Sex == "b", 
                     Date == "2020-07-16")) %>% 
  mutate(Wave = 1,
         Type = "Province") %>% 
  filter(Code %in% provinces)

# write_rds(db_prv4, "Output/cfr_by_age_sex.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# End of the 2nd wave of the pandemic in end-February
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db_prv_2nd <- db_prv2 %>% 
  group_by(Region) %>% 
  filter(Date == max(Date)) %>% 
  mutate(Wave = 2,
         Type = "Province") %>% 
  filter(Code %in% provinces)

unique(db_prv_1st$Code) %>% sort
unique(db_prv_2nd$Code) %>% sort

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Other countries and cities to compare 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

ctrs <- db_cov3 %>% 
  filter(Region == Country)

# Countries
ctrs_1st <- db_cov3 %>% 
  filter((Code %in% countries & Date == "2020-07-15") | 
           (Code == "IT" & Date == "2020-07-14") | 
           (Code == "NL" & Date == "2020-07-12") | 
           (Code == "US" & Date == "2020-06-27")) %>% 
  mutate(Wave = 1,
         Type = "Country")

ctrs_2nd <- db_cov3 %>% 
  filter(Code %in% countries) %>% 
  group_by(Code) %>% 
  filter(Date == max(Date)) %>% 
  ungroup() %>% 
  mutate(Wave = 2,
         Type = "Country")

# Cities
ctys_1st <- db_cov3 %>% 
  filter(Code %in% cities & Date == "2020-07-15") %>% 
  mutate(Wave = 1,
         Type = "City")

ctys_2nd <- db_cov3 %>% 
  filter(Code %in% cities) %>% 
  group_by(Code) %>% 
  filter(Date == max(Date)) %>% 
  ungroup() %>% 
  mutate(Wave = 2,
         Type = "City")

unique(ctys_1st$Code)
unique(ctys_2nd$Code)

out <- bind_rows(db_prv_2nd, db_prv_1st,
                ctrs_2nd, ctrs_1st,
                ctys_2nd, ctys_1st)
                
write_rds(out, "Output/covid_data_by_age_sex.rds")

