rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)

# functions
source("Code/00_functions.R")

# reading Canada data
# db_can_age <- read_rds("Output/cfr_by_age_sex.rds")
# 
# db_oth <- read_rds("Output/other_regions_by_age_sex.rds")

db <- read_rds("Output/covid_data_by_age_sex.rds")

# Aggregating in 10-year age groups

# age_int <- 10
# 
# db2 <- db %>% 
#   mutate(Age = floor(Age / age_int) * age_int) %>%
#   group_by(Region, Sex, Age) %>%
#   summarise(Cases = sum(Cases),
#             Deaths = sum(Deaths)) %>% 
#   ungroup()
# 

# CFR decomposition at one specific date
########################################

db_can_oth <- db %>% 
  group_by(Region, Date, Sex) %>% 
  mutate(CFR = Deaths / Cases,
         age_dist = Cases / sum(Cases),
         Cases_t = sum(Cases),
         Deaths_t = sum(Deaths),
         CFR_t = Deaths_t / Cases_t) %>% 
  ungroup() 

table(db_can_oth$Region, db_can_oth$Sex)

tx <- 8
s <- "b"

# Across countries
##################

rgs <- c("Spain",
         "Denmark",
         "Germany",
         "Italy",
         "Netherlands",
         "Sweden",
         "USA")

rfs <- c("Canada")

cfr_cts <- diffs_ref(db_can_oth, rfs, rgs, "Countries", 2.5)

# Provinces and Canada
######################

rgs <- c("Alberta",
         "British Columbia",
         "Ontario",
         "Quebec")
rfs <- c("Canada")
cfr_provs <- diffs_ref(db_can_oth, rfs, rgs, "Provinces", 2)

rgs <- c("Canada", 
         "Alberta",
         "British Columbia",
         "Ontario",
         "Quebec")
rfs <- c("Alberta",
         "British Columbia",
         "Ontario",
         "Quebec")
cfr_provs2 <- diffs_ref(db_can_oth, rfs, rgs, "Provinces", 5)

# Across cities
###############

rgs <- c("Montreal",
         "Toronto", 
         "Berlin", 
         "NYC")
rfs <- c("Montreal", 
         "Toronto")
cfr_cities <- diffs_ref(db_can_oth, rfs, rgs, "Cities", 2.7)



# Provinces vs countries
########################

rgs <- c("Spain",
         "Denmark",
         "Germany",
         "Italy",
         "Netherlands",
         "Sweden",
         "USA")

rfs <- c("Ontario",
         "Quebec")

diffs_ref(db_can_oth, rfs, rgs, "Provs_ctrs", 4)

unique(test$Region)


lvs <- c("Canada", 
         "Spain",
         "Denmark",
         "Germany",
         "Italy",
         "Netherlands",
         "Sweden",
         "USA",
         "Alberta",
         "British Columbia",
         "Ontario",
         "Quebec",
         "Montreal",
         "Toronto", 
         "Berlin", 
         "NYC")

test <- db %>% 
  filter(Sex == "b") %>% 
  gather(Cases, Deaths, key = "Measure", value = Value) %>% 
  group_by(Region, Measure) %>% 
  mutate(val_p = Value / sum(Value)) %>% 
  ungroup() %>% 
  mutate(val_p = ifelse(Measure == "Cases", val_p * -1, val_p),
         Region = factor(Region, levels = lvs))



test %>% 
  filter(Region == "Alberta",
         Measure == "Cases") %>% 
  mutate(Value = ifelse(Sex == "f", Value, -1*Value)) %>% 
  ggplot()+
  geom_area(aes(Age, Value, fill = Sex, col = Sex), alpha = 0.7)

cols <- c("#2a9d8f", "#e76f51")

test %>% 
  ggplot()+
  geom_bar(aes(Age, val_p, fill = Measure, col = Measure), stat = "identity", alpha = 0.5)+
  geom_hline(yintercept = 0, col = "black", size = 0.3, alpha = 1)+
  scale_x_continuous(breaks = seq(0, 100, 20))+
  facet_wrap(~ Region)+
  coord_flip()+
  scale_fill_manual(values = cols)+
  scale_color_manual(values = cols)+
  labs(x = "Age",
       y = "Distribution")+
  theme_bw()+
  theme(
    legend.position="bottom",
    legend.title = element_text(size = tx),
    legend.text = element_text(size = tx - 1),
    legend.key.size = unit(0.5,"line"),
    strip.background = element_rect(fill="transparent"),
    strip.text = element_text(size = tx - 2),
    axis.text.x = element_text(size = tx - 1.5),
    axis.text.y = element_text(size = tx - 1),
    axis.title.x = element_text(size = tx + 1),
    axis.title.y = element_text(size = tx + 1)
  )
ggsave("Figures/age_distribution.png", width = 5, height = 6)
