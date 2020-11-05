rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)

# functions
source("Code/00_functions.R")

# reading Canada data
db_can <- read_rds("Output/canada_cases_deaths.rds")

db_can_age <- read_rds("Output/cfr_by_age_sex.rds")

db_oth <- read_rds("Output/other_regions_by_age_sex.rds")

db_can2 <- db_can %>% 
  filter(Sex == "b") %>% 
  select(Region, Date, Cases, Deaths, CFR) %>% 
  rename(Cases_t = Cases,
         Deaths_t = Deaths,
         CFR_t = CFR) 

db_can_age2 <- db_can_age %>% 
  filter(Sex == "b") %>% 
  select(Region, Date, Age, Cases, Deaths, CFR) %>% 
  arrange(Region, Date, Age) %>% 
  group_by(Region, Date) %>%  
  mutate(age_dist = Cases / sum(Cases)) %>% 
  ungroup() %>% 
  left_join(db_can2) %>% 
  filter(!is.na(CFR_t),
         CFR_t > 0) %>% 
  mutate(CFR = replace_na(CFR, 0),
         Region = ifelse(Region == 'British Columbia', "BC", Region))

dates_all <- db_can_age2 %>% 
  filter(Region != "All") %>% 
  select(Region, Date) %>% 
  unique() %>% 
  mutate(n = 1) %>% 
  spread(Region, n) %>% 
  mutate(av = Alberta + BC + Montreal + Ontario + Quebec) %>% 
  filter(av == 5)

p1 <- "Quebec"
p2 <- "Montreal"
d_exc <- c(ymd("2020-04-23"), ymd("2020-04-28"), ymd("2020-04-29"))
d_exc <- c(ymd("2020-04-23"))
d_exc <- c(ymd("2020-04-28"), ymd("2020-04-29"))

# decomp(p1, p2, d_exc)


# CFR decomposition at one specific date
########################################

unique(db_oth$Region)

# grouping both sexes in Canada and Ontario
ca_b <- db_can_age %>% 
  select(-new_c, -new_d) %>% 
  filter(Date == "2020-07-09",
         Code == "CA") %>% 
  group_by(Region, Code, Date, Age) %>% 
  summarise(Cases = sum(Cases),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Sex = "b")

db_can_oth <- db_can_age %>% 
  select(-new_c, -new_d) %>% 
  filter(Date == "2020-07-09") %>% 
  bind_rows(ca_b, db_oth) %>% 
  mutate(Age = case_when(Age >= 80 ~ 80, 
                         Age <= 10 ~ 0, 
                         TRUE ~ Age)) %>% 
  group_by(Region, Date, Sex, Age) %>% 
  summarise(Cases = sum(Cases),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  group_by(Region, Date, Sex) %>% 
  mutate(CFR = Deaths / Cases,
         age_dist = Cases / sum(Cases),
         Cases_t = sum(Cases),
         Deaths_t = sum(Deaths),
         CFR_t = Deaths_t / Cases_t) %>% 
  ungroup() %>% 
  mutate(Region = ifelse((Region == "Toronto" & Date == ymd("2020-10-24")), "Toronto_2", Region),
         Region = ifelse(Region == "All", "Canada", Region))

table(db_can_oth$Region, db_can_oth$Sex)

unique(db_can_oth$Region)


p1 <- "Netherlands"
db <- db_can_oth

cities <- c("Berlin", "Toronto", "NYC",  "Montreal")

cts <- c("Canada", 
         "British Columbia", 
         "Alberta", 
         "Ontario",
         "Quebec",
         "Belgium", 
         "Denmark", 
         "Germany", 
         "Italy", 
         "Netherlands", 
         "Sweden",
         "USA")

refs <- c("Canada", "Quebec", "Ontario")
for(p1 in refs){
  db_diffs_can <- NULL
  for(p2 in cts){
    db_diffs_can <- db_diffs_can %>% 
      bind_rows(bind_cols(tibble(P1 = p1, P2 = p2), kitagawa(db_can_oth, p1, p2, s)))
  }
  
  db_diffs_can2 <- db_diffs_can %>% 
    gather(alpha, betha, key = "Component", value = Value) %>% 
    filter(P2 != p1)
  
  db_diffs_can2 %>% 
    ggplot()+
    geom_bar(aes(reorder(P2, -diff), Value, fill = Component, col = Component), stat = "identity")+
    geom_point(aes(reorder(P2, -diff), diff), col = "black", size = 2)+
    geom_hline(yintercept = 0, col = "black", size = 0.3, alpha = 0.5)+
    scale_y_continuous(limits = c(-0.1, 0.1))+
    scale_color_manual(values = c("blue", "red"))+
    scale_fill_manual(values = c("blue", "red"))+
    labs(title = paste0("Decomposition of CFR, ", p1, " as reference"),
         x = "Countries")+
    theme_bw()+
    coord_flip()+
    theme(legend.position="bottom")
  
  ggsave(paste0("Figures/cfr_diff_reference_", p1, ".png"))
}

refs <- c("Montreal", "Toronto")
for(p1 in refs){
  db_diffs_can <- NULL
  for(p2 in cities){
    db_diffs_can <- db_diffs_can %>% 
      bind_rows(bind_cols(tibble(P1 = p1, P2 = p2), kitagawa(db_can_oth, p1, p2, s)))
  }
  
  db_diffs_can2 <- db_diffs_can %>% 
    gather(alpha, betha, key = "Component", value = Value) %>% 
    filter(P2 != p1)
  
  db_diffs_can2 %>% 
    ggplot()+
    geom_hline(yintercept = 0, col = "black", size = 0.3, alpha = 0.5)+
    geom_bar(aes(reorder(P2, -diff), Value, fill = Component, col = Component), stat = "identity")+
    geom_point(aes(reorder(P2, -diff), diff), col = "black", size = 2)+
    scale_y_continuous(limits = c(-0.1, 0.1))+
    geom_hline(yintercept = 0, col = "black", size = 0.3, alpha = 0.5)+
    scale_color_manual(values = c("blue", "red"))+
    scale_fill_manual(values = c("blue", "red"))+
    labs(title = paste0("Decomposition of CFR, ", p1, " as reference"),
         x = "Cities")+
    theme_bw()+
    coord_flip()+
    theme(legend.position="bottom")
  
  ggsave(paste0("Figures/cfr_diff_reference_", p1, ".png"))
}


