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
registerDoParallel(cores = 40)
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
# exc_type <- "long_flu" 
# flu_season <- c(seq(1, 18, 1), seq(42, 54, 1))
# heat_waves <- 0
  
# Initial year for baseline estimation
######################################
ym <- 2010

# database for baseline estimation
##################################
db2 <- db %>% 
  mutate(sn52 = sin((2*pi*t)/(52)),
         cs52 = cos((2*pi*t)/(52)),
         # excluding winter (wks 46-14), summer (wks 27-35), 2009 and COVID-19 pandemics
         include = ifelse(!(Week %in% heat_waves | Week %in% flu_season) &
                            (Year != 2020 & Year != 2009),
                          1, 0))

# skip_to_next <- F

# Testing single populations
############################
# c <- "Quebec_isq"
# s <- "b"
# a <- "0"
# 
# temp <- db2 %>%
#   filter(Region == c,
#          Sex == s,
#          Age == a,
#          Year >= ym)
# 
# test <- fit_baseline(temp)
# 
# test %>%
#   ggplot()+
#   geom_line(aes(Date, Deaths))+
#   geom_ribbon(aes(Date, ymin = lp, ymax = up), fill = "#01BAEF", alpha = 0.25)+
#   geom_line(aes(Date, Baseline), col = "#01BAEF", alpha = 0.9, size = 0.6)+
#   scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "%Y")+
#   labs(title=paste0(c, "_", s, "_", a))+
#   theme_bw()+
#   theme(
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(size=13),
#     axis.text.x = element_text(size=10),
#     axis.text.y = element_text(size=10),
#     axis.title.x = element_text(size=11),
#     axis.title.y = element_text(size=11))

# Fitting all regions, sexes, and ages in Canada
################################################
cts <- unique(db2$Region)
cts <- c("Canada", "Quebec_isq")
cts <- c("British Columbia", "Alberta", "Canada", "Ontario", "Quebec", "Quebec_isq")
# cts <- c("Canada", "Quebec_isq")

db_all_blns <- NULL
skip_to_next <- F

# c <- "Quebec"


for (c in cts) {
  temp1 <- db2 %>% 
    filter(Region == c)
  sxs <- unique(temp1$Sex)
  ags <- unique(temp1$Age)
  for (s in sxs) {
    for (a in ags) {
      temp2 <- temp1 %>% 
        filter(Sex == s,
               Age == a)
      cat(paste(c, s, a, "\n", sep = "_"))
      temp3 <- fit_baseline(temp2)
      
      temp3 %>%
        ggplot()+
        geom_line(aes(Date, Deaths))+
        geom_ribbon(aes(Date, ymin = lp, ymax = up), fill = "#01BAEF", alpha = 0.25)+
        geom_line(aes(Date, Baseline), col = "#01BAEF", alpha = 0.9, size = 0.6)+
        scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "%Y")+
        labs(title=paste0(c, "_", s, "_", a))+
        theme_bw()+
        theme(
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=13),
          axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=10),
          axis.title.x = element_text(size=11),
          axis.title.y = element_text(size=11))+
        ggsave(paste0("Figures/baseline_singles/", c, "_", s, "_", a, "_", ym, ".png"), dpi = 300, width = 6, height = 4)
      
      db_all_blns <- db_all_blns %>% 
        bind_rows(temp3)
    }
  }
}

write_csv(db_all_blns, path = paste0("Output/baseline_mortality_", ym, ".csv"))
detach(package:MASS)

