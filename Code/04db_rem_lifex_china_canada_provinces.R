rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)
library(readxl)

# setwd("U:/gits/covid_canada/")
source("Code/00_functions.R")
############################################
# Importing remaining life expectancy by age
############################################

# Canada and Provinces
######################
regs <- c("Canada - ",
          "Que. - ",
          "Ont. - ",
          "Alta. - ",
          "B.C. - ")

sxs <- c("Both sexes", 
         "Males", 
         "Females")

db_ex_ca <- NULL
# sx <- "Females"
# rg <- "Canada - "
for(rg in regs){
  for(sx in sxs){
    sheet_name <- paste0(rg, sx)
    temp_ex <- read_xlsx("Data/2016-2018_Tbl-eng.xlsx",
                            sheet = sheet_name,
                            skip = 3) %>% 
      select(Age, ex) %>% 
      drop_na() %>% 
      separate(Age, c("Age", "trash"), sep = " ") %>% 
      select(-trash) %>% 
      mutate(Region = rg,
             Sex = sx)
    
    db_ex_ca <- db_ex_ca %>% 
      bind_rows(temp_ex)
  }
}

db_ex_ca2 <- db_ex_ca %>% 
  mutate(Region = case_when(Region == "Canada - " ~ "All",
                            Region == "Que. - " ~ "Quebec",
                            Region == "Ont. - " ~ "Ontario",
                            Region == "Alta. - " ~ "Alberta",
                            Region == "B.C. - " ~ "BC"),
         Sex = case_when(Sex == "Both sexes" ~ "b",
                         Sex == "Males" ~ "m",
                         Sex == "Females" ~ "f"),
         Age = as.numeric(Age),
         ex = as.numeric(ex))


# Spain
#######
db_ex_es_m <- read_delim("Data/fltper_1x1.txt", delim = " ", skip = 1) %>% 
  rename(Year = 1,
         Age = 2,
         ex = 10) %>% 
  select(Year, Age, ex) %>% 
  mutate(Sex = "m")

db_ex_es_f <- read_delim("Data/mltper_1x1.txt", delim = " ", skip = 1) %>% 
  rename(Year = 1,
         Age = 2,
         ex = 10) %>% 
  select(Year, Age, ex) %>% 
  mutate(Sex = "f")

db_ex_es <- bind_rows(db_ex_es_m, db_ex_es_f) %>% 
  mutate(Year = as.numeric(Year),
         Age = str_trim((Age)),
         Age = ifelse(Age == "110+", 110, as.integer(Age)),
         ex = as.numeric(str_trim(ex)),
         Region = "Spain") %>% 
  filter(Year == 2018) %>% 
  select(-Year)


# China
#######

# life tables from WPP
lt_1950_2020 <- read_xlsx("Data/WPP2019_MORT_F17_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx",
                          sheet = 1,
                          skip = 16)

ctr <- "China"
db_ex_ch <- lt_1950_2020 %>%
  rename(Region = 3,
         Year = 8,
         Age = 9,
         ex = 19) %>% 
  filter(Year == "2015-2020",
         Region == ctr) %>% 
  select(Region, Age, ex) %>% 
  mutate(Sex = "b",
         ex = as.numeric(ex))
  
# binding all together
db_ex_all <- bind_rows(db_ex_ca2, db_ex_es, db_ex_ch)

##############################################################################
# equivalent thanatological ages for Canada and provinces from China and Spain
##############################################################################

# ungrouping remaining life expectancies in 0.001-year intervals
regs <- unique(db_ex_all$Region)
exs_ungr_all <- NULL

for(rg in regs){
  temp1 <- db_ex_all %>% 
    filter(Region == rg)
  sxs <- unique(temp1$Sex)
  for(sx in sxs){
    temp2 <- temp1 %>% 
      filter(Sex == sx)
    temp3 <- ungr_life_ex(temp2, 0.001) %>% 
      mutate(Region = rg,
             Sex = sx,
             ex = round(ex, 3))
    exs_ungr_all <- exs_ungr_all %>% 
    bind_rows(temp3)  
  }
}


exs_ungr_ca <- exs_ungr_all %>% 
  filter(!(Region == "Spain" | Region == "China"))

exs_ungr_es <- exs_ungr_all %>% 
  filter(Region == "Spain") %>% 
  rename(Age_es = Age) %>% 
  select(-Region)

exs_ungr_ch <- exs_ungr_all %>% 
  filter(Region == "China") %>% 
  rename(Age_ch = Age) %>% 
  select(-Region)

exs_ca_es <- exs_ungr_ca %>% 
  left_join(exs_ungr_es, by = c("Sex", "ex")) %>% 
  filter(Age_es %in% seq(0, 100, 1)) %>% 
  arrange(Region, Sex, Age) %>% 
  group_by(Region, Sex, Age_es) %>% 
  mutate(q = 1:n(),
         qt = floor(n()/2)) %>% 
  ungroup() %>% 
  mutate(qt = ifelse(qt == 0, 1, qt)) %>% 
  filter(q == qt) %>% 
  select(-q, -qt, -ex)

write_rds(db_exs3, "Data_output/thanat_age_canada_spain.rds")

exs_ca_ch <- exs_ungr_ca %>% 
  left_join(exs_ungr_ch, by = c("Sex", "ex")) %>% 
  filter(Age_ch %in% seq(0, 100, 1)) %>% 
  arrange(Region, Sex, Age) %>% 
  group_by(Region, Sex, Age_ch) %>% 
  mutate(q = 1:n(),
         qt = floor(n()/2)) %>% 
  ungroup() %>% 
  mutate(qt = ifelse(qt == 0, 1, qt)) %>% 
  filter(q == qt) %>% 
  select(-q, -qt, -ex)

write_rds(db_exs3, "Data_output/thanat_age_canada_china.rds")

