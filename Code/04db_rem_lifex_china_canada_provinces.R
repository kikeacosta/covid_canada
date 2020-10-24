rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)
library(readxl)

setwd("U:/gits/covid_canada/")

######################
# Canada and Provinces
######################
regs <- c("Canada - Both sexes",
          "Que. - Both sexes",
          "Ont. - Both sexes",
          "Alta. - Both sexes",
          "B.C. - Both sexes")

# rg <- "Canada - Both sexes"

db_exs <- NULL
for(rg in regs){
  temp_ex <- read_xlsx("Data/2016-2018_Tbl-eng.xlsx",
                          sheet = rg,
                          skip = 3) %>% 
    drop_na() %>% 
    separate(Age, c("Age", "trash"), sep = " ") %>% 
    select(Age, ex) %>% 
    mutate(Region = rg)
  
  db_exs <- db_exs %>% 
    bind_rows(temp_ex)
}

db_exs2 <- db_exs %>% 
  mutate(Region = case_when(Region == "Canada - Both sexes" ~ "All",
                            Region == "Que. - Both sexes" ~ "Quebec",
                            Region == "Ont. - Both sexes" ~ "Ontario",
                            Region == "Alta. - Both sexes" ~ "Alberta",
                            Region == "B.C. - Both sexes" ~ "BC"),
         Age = as.numeric(Age),
         Sex = "b")

#######
# China
#######

# life tables from WPP
lt_1950_2020 <- read_xlsx("Data/WPP2019_MORT_F17_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx",
                          sheet = 1,
                          skip = 16)

ctr <- "China"
ex_c <- lt_1950_2020 %>%
  rename(Region = 3,
         Year = 8,
         Age = 9,
         ex = 19) %>% 
  filter(Year == "2015-2020",
         Region == ctr) %>% 
  select(Region, Age, ex) %>% 
  mutate(Sex = "b")
  
db_exs3 <- ex_c %>% 
  bind_rows(db_exs2)


write_rds(db_exs3, "Data_output/lexs_canada_china.rds")