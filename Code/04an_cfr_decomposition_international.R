rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)

# functions
source("Code/00_functions.R")

# reading Canada data
db_can_age <- read_rds("Output/cfr_by_age_sex.rds")

db_oth <- read_rds("Output/other_regions_by_age_sex.rds")

# CFR decomposition at one specific date
########################################

tx <- 8

s <- "b"
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
         "Alberta",
         "British Columbia",
         "Ontario",
         "Quebec")
         # "Belgium", 
         # "Denmark", 
         # "Germany", 
         # "Italy", 
         # "Netherlands", 
         # "Sweden",
         # "USA")

refs <- c("Canada", "Quebec", "Ontario", "Alberta", "British Columbia")
for(p1 in refs){
  db_diffs_can <- NULL
  for(p2 in cts){
    db_diffs_can <- db_diffs_can %>% 
      bind_rows(bind_cols(tibble(P1 = p1, P2 = p2), kitagawa(db_can_oth, p1, p2, s)))
  }
  
  db_diffs_can2 <- db_diffs_can %>% 
    gather(alpha, beta, key = "Component", value = Value) %>% 
    filter(P2 != p1) %>% 
    mutate(P2cfr = paste0(P2, " (", round(CFR2, 3), ")"))
  
  cfr_ref <- db_diffs_can2 %>% pull(CFR1) %>% unique() %>% round(., 3)
  
  db_diffs_can2 %>% 
    ggplot()+
    geom_bar(aes(reorder(P2cfr, -diff), Value, fill = Component, col = Component), stat = "identity", alpha = 0.5)+
    geom_point(aes(reorder(P2cfr, -diff), diff), col = "black", size = 2)+
    geom_hline(yintercept = 0, col = "black", size = 0.3, alpha = 0.5)+
    scale_y_continuous(limits = c(-0.1, 0.1))+
    scale_color_manual(values = c("#43a2ca", "#e34a33"), labels = c("Age structure", "Fatality"))+
    scale_fill_manual(values = c("#43a2ca", "#e34a33"), labels = c("Age structure", "Fatality"))+
    labs(title = paste0("Decomposition of CFR, ", p1, " (", cfr_ref, ") as reference"),
         x = "Countries",
         y = "CFR difference")+
    theme_bw()+
    coord_flip()+
    theme(
      legend.position="bottom",
      legend.title = element_text(size = tx),
      legend.text = element_text(size = tx - 1),
      legend.key.size = unit(0.5,"line"),
      plot.title = element_text(size = tx + 2),
      axis.text.x = element_text(size = tx),
      axis.text.y = element_text(size = tx),
      axis.title.x = element_text(size = tx + 1),
      axis.title.y = element_text(size = tx + 1)
    )
  
  ggsave(paste0("Figures/cfr_diff_reference_", p1, "2.png"), width = 5, height = 2)
}

refs <- c("Montreal", "Toronto")
for(p1 in refs){
  db_diffs_can <- NULL
  for(p2 in cities){
    db_diffs_can <- db_diffs_can %>% 
      bind_rows(bind_cols(tibble(P1 = p1, P2 = p2), kitagawa(db_can_oth, p1, p2, s)))
  }
  
  db_diffs_can2 <- db_diffs_can %>% 
    gather(alpha, beta, key = "Component", value = Value) %>% 
    filter(P2 != p1) %>% 
    mutate(P2cfr = paste0(P2, " (", round(CFR2, 3), ")"))
  
  cfr_ref <- db_diffs_can2 %>% pull(CFR1) %>% unique() %>% round(., 3)
  
  db_diffs_can2 %>% 
    ggplot()+
    geom_hline(yintercept = 0, col = "black", size = 0.3, alpha = 0.5)+
    geom_bar(aes(reorder(P2cfr, -diff), Value, fill = Component, col = Component), stat = "identity", alpha = 0.5)+
    geom_point(aes(reorder(P2cfr, -diff), diff), col = "black", size = 2)+
    scale_y_continuous(limits = c(-0.1, 0.1))+
    geom_hline(yintercept = 0, col = "black", size = 0.3, alpha = 0.5)+
    scale_color_manual(values = c("#43a2ca", "#e34a33"), labels = c("Age structure", "Fatality"))+
    scale_fill_manual(values = c("#43a2ca", "#e34a33"), labels = c("Age structure", "Fatality"))+
    labs(title = paste0("Decomposition of CFR, ", p1, " (", cfr_ref, ") as reference"),
         x = "Cities",
         y = "CFR difference")+
    theme_bw()+
    coord_flip()+
    theme(
      legend.position="bottom",
      legend.title = element_text(size = tx),
      legend.text = element_text(size = tx - 1),
      legend.key.size = unit(0.5,"line"),
      plot.title = element_text(size = tx + 2),
      axis.text.x = element_text(size = tx),
      axis.text.y = element_text(size = tx),
      axis.title.x = element_text(size = tx + 1),
      axis.title.y = element_text(size = tx + 1)
    )
  

  ggsave(paste0("Figures/cfr_diff_reference_", p1, ".png"), width = 5, height = 1.8)
}


