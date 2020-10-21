rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(readr)
library(lubridate)
library(ggplot2)
library(osfr)

# reading Canada data
setwd("U:/nextcloud/Projects/COVID_19/COVerAge-DB/canada/statcan_pdf/")
db <- read_rds("201020_covid_canada.rds")

osf_retrieve_file("43ucn") %>%
  osf_download(conflicts = "overwrite") 

# This reads it in
Output_10 <-  read_csv("Output_10.zip",
                       skip = 3,
                       col_types = "ccccciiddd")

# convert to date class
Output_10 <- 
  Output_10 %>% 
  mutate(Date = dmy(Date))
