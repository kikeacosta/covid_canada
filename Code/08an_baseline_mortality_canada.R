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

# database for baseline estimation
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
         include = ifelse(((Week >= 15 & Week <= 26) |
                             (Week >= 36 & Week <= 45)) &
                            (Year != 2020 & Year != 2009),
                          1, 0))

skip_to_next <- F

# c <- "Canada"
# s <- "b"
# a <- "All"
ym <- 2014

# temp <- db2 %>% 
#   filter(Region == c,
#          Sex == s,
#          Age == a,
#          Year >= ym)
# 
# test <- fit_baseline(temp)

# db2 <- temp

cts <- unique(db2$Region)

cts <- c("Canada", "Ontario", "Quebec")
cts <- c("British Columbia", "Alberta")
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
      temp2 <- fit_baseline(temp)
      db_all_blns <- db_all_blns %>% 
        bind_rows(temp2)
    }
  }
}

#########################
# appending all estimates
#########################

# db_all <- NULL
# getwd()
# 
# temp = list.files("Data_output/single_ests/", pattern="*.csv")
# 
# length(temp)
# # i <- 1
# for (i in 1:length(temp)) {
#   db_temp <- read_csv(paste0("Data_output/single_ests/", temp[i])) %>% 
#     as_tibble() %>% 
#     mutate(Sex = as.character(Sex),
#            Sex = ifelse(Sex == "FALSE", "f", Sex))
#   db_all <- bind_rows(db_all, db_temp)
# }
# 
# detach(package:MASS)
# 
write_csv(db_all_blns, path = "Output/baseline_mortality2.csv")
