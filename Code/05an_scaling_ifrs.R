rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)
library(readxl)

# setwd("U:/gits/covid_canada/")

source("Code/00_functions.R")
##############
# reading data
##############

# IFRs by age from Verity et al.
verity <- read_csv("Data/IFR_age_Verity_et_al.csv")

# remaining life expectancy for China and Canada by Province
db_exs <- read_rds("Data_output/lexs_canada_china.rds")

###############################
# ungrouping Verity et al. IFRs
###############################

# function provided by Christina
################################
# to_ungroup <- function(to_ungroup,nr_grouped_years){
# 
#   seq_ungrouped_years <- seq(0,length(to_ungroup)*nr_grouped_years)
#   cumsum_to_ungroup <- cumsum(c(sum(to_ungroup),to_ungroup))
#   grouped_time_points <- c(0,(1:length(to_ungroup))*nr_grouped_years)
# 
#   applied_smooth_spline <- smooth.spline(x=grouped_time_points,y=cumsum_to_ungroup)
#   predict_cumsum_ungroup <- predict(applied_smooth_spline,x=seq_ungrouped_years)$y
#   ungrouped <- diff(predict_cumsum_ungroup)
#   return(ungrouped)
# }
# 
# # Verity ages and IFR by age
# ifrs <- verity %>% pull(IFR)
# ages <- verity %>% pull(Age)
# 
# ifrs_ungr <- to_ungroup(to_ungroup = ifrs, nr_grouped_years = 10)
# 
# # constructing a table with grouped and ungrouped values
# ifrs_all <- tibble(Age = seq(0, 89, 1), IFR = ifrs_ungr, source = "ungrouped") %>%
#   bind_rows(tibble(Age = seq(5, 85, 10), IFR = ifrs, source = "verity"))
# 
# # plotting both
# ifrs_all %>%
#   ggplot()+
#   geom_point(aes(Age, IFR, col = source))+
#   scale_x_continuous(breaks = seq(0, 90, 10))+
#   scale_y_log10()
# # ggsave("Figures/plot1.png")

# # very far from original values!!!

# trying my own!
################
ifrs <- verity %>% pull(IFR)
# adding 5 years to place IFRs in the middle of the age interval
ages <- verity %>% pull(Age) + 5

md1 <- smooth.spline(x = ages, y = ifrs)
pr1 <- predict(md1, x = seq(0, 89, 1))$y

ifrs_ungr <- tibble(Age_ch = seq(0, 89, 1), IFR = pr1)

ifrs_all <- tibble(Age = seq(0, 89, 1), IFR = pr1, source = "ungr") %>% 
  bind_rows(tibble(Age = seq(5, 85, 10), IFR = ifrs, source = "verity"))

ifrs_all %>% 
  ggplot()+
  geom_point(aes(Age, IFR, col = source))+
  scale_x_continuous(breaks = seq(0, 90, 10))+
  scale_y_log10()
# ggsave("Figures/plot2.png")

# it fits very well, I do not know something here?



####################################
# scaling IFRs to another population
####################################

# ungrouping remaining life expectancy

# function provided by Christina
################################

# get_ungrouped_ex_2015_2020 <- function(country_name, lt_1950_2020){
#   current_period_data <- lt_1950_2020[which(lt_1950_2020[,8]=="2015-2020"),]
#   current_period_data <- current_period_data[which(current_period_data[,3]==country_name),]  
#   current_ex_data <- as.numeric(current_period_data[,19])
#   smooth_current_ex_data <- smooth.spline(x=c(0,1,seq(5,100,5)),y=current_ex_data)
#   new_x <- c(seq(0,0.99,0.01),seq(1,4.99,0.01),seq(5,100,0.01))
#   predict_smooth_current_ex_data <- predict(smooth_current_ex_data,new_x,len=new_x)
#   return(predict_smooth_current_ex_data)
# }


# my own
# ungrouping Canada in 0.001-year intervals
regs <- db_exs %>% 
  filter(Region != "China") %>% 
  pull(Region) %>% 
  unique()

exs_ungr_ca <- NULL
for(r in regs){
  exs_ungr_ca <- exs_ungr_ca %>% 
    bind_rows(ungr_life_ex(r, 0.001))  
}

# ungrouping china in 1-year intervals
exs_ungr_ch <- ungr_life_ex("China", 1) %>% 
  mutate(ex = round(ex, 3)) %>% 
  rename(Age_ch = Age) %>% 
  select(-Region)

# finding equivalent ages betwwn China and Canada
exs_ungr_ca2 <- exs_ungr_ca %>% 
  mutate(ex = round(ex, 3)) %>% 
  left_join(exs_ungr_ch, by = "ex") %>% 
  drop_na() %>% 
  arrange(Region, Age) %>% 
  group_by(Region, Age_ch) %>% 
  mutate(q = 1:n()) %>% 
  ungroup() %>% 
  filter(q == 1)

# attributing IFR from China to Canada
ifrs_ca <- exs_ungr_ca2 %>% 
  left_join(ifrs_ungr) %>% 
  drop_na()


ifrs_ca_adj <- NULL
for(r in regs){
  ifrs_ca_adj <- ifrs_ca_adj %>% 
    bind_rows(adj_ifrs_can(r))  
}


ifrs_ca_adj %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Region))+
  scale_y_log10()

ifrs_ca_adj %>% 
  filter(Age >= 60) %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Region))+
  scale_y_log10()

ifrs_ca_adj %>% 
  bind_rows(ifrs_ungr %>% 
              rename(Age = Age_ch) %>% 
              mutate(Region = "China")) %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Region))+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 90, 10))+
  labs(title = "Age-specific IFRs by region")+
  theme_bw()

ggsave("Figures/age-spe_ifr_canada_china.png")

ifrs_ca_adj %>% 
  bind_rows(ifrs_ungr %>% 
              rename(Age = Age_ch) %>% 
              mutate(Region = "China")) %>% 
  filter(Age >= 60) %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Region))+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 90, 10))+
  labs(title = "Age-specific IFRs by region ages >60")+
  theme_bw()

ggsave("Figures/age-spe_ifr_60plus.png")

ifr_10age_can <- ifrs_ca_adj %>% 
  filter(Age %in% seq(5, 85, 10)) %>% 
  mutate(Age = Age -5)

write_rds(ifr_10age_can, "Data_output/ifrs_can_age10.rds")
