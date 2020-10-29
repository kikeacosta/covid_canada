rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

pkgs <- c("tidyverse",
          "lubridate",
          "haven",
          "readxl", 
          "stats", 
          "splines",
          "MASS",
          "gnm",
          'doParallel', 
          'foreach')

lapply(pkgs, require, character.only = T)
select <- dplyr::select
registerDoParallel(cores = 4)
source("Code/00_functions.R")


# reading mortality and population data from StatCan files
db <- read_rds("Output/weekly_canada.rds") 


# definition of flu seasons and heat waves in Canada 
####################################################
# Euromomo definition
exc_type <- "emomo" 
flu_season <- c(seq(1, 14, 1), seq(46, 54, 1))
heat_waves <- seq(27, 35, 1)
# option 2 definition
exc_type <- "long_flu" 
flu_season <- c(seq(1, 18, 1), seq(42, 54, 1))
heat_waves <- 0
  
# Initial year for baseline estimation
######################################
ym <- 2010

# database for baseline estimation
##################################
db2 <- db %>% 
  mutate(Deaths = Deaths + 1) %>%
  group_by(Year, Region, Age, Sex) %>% 
  mutate(Week = 1:n()) %>% 
  ungroup() %>% 
  arrange(Region, Age, Sex, Year) %>% 
  group_by(Region, Age, Sex) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  mutate(sn52 = sin((2*pi*t)/(52)),
         cs52 = cos((2*pi*t)/(52)),
         # excluding winter (wks 46-14), summer (wks 27-35), 2009 and COVID-19 pandemics
         include = ifelse(!(Week %in% heat_waves | Week %in% flu_season) &
                            (Year != 2020 & Year != 2009),
                          1, 0))

# skip_to_next <- F

# Testing single populations
############################
# c <- "Quebec"
# s <- "m"
# a <- "All"
# 
# temp <- db2 %>%
#   filter(Region == c,
#          Sex == s,
#          Age == a,
#          Year >= ym)
# 
# fit_baseline(temp)


# Fitting all regions, sexes, and ages in Canada
################################################
cts <- unique(db2$Region)
cts <- c("Canada", "Ontario", "Quebec")
cts <- c("British Columbia", "Alberta", "Canada", "Ontario", "Quebec")
sxs <- unique(db2$Sex)
ags <- unique(db2$Age)

db_all_blns <- NULL

for (c in cts) {
  for (s in sxs) {
    for (a in ags) {
      temp <- db2 %>% 
        filter(Region == c,
               Sex == s,
               Age == a,
               Year >= ym)
      cat(paste(c, s, a, "\n", sep = "_"))
      temp2 <- fit_baseline(temp, exc_type)
      db_all_blns <- db_all_blns %>% 
        bind_rows(temp2)
    }
  }
}

write_csv(db_all_blns, path = paste0("Output/baseline_mortality_", ym, "_", exc_type, ".csv"))
detach(package:MASS)

