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
  mutate(av = Alberta + BC + Ontario + Quebec) %>% 
  filter(av == 4)


# Two dates to evaluate the evolution of CFR

d1 <- "2020-04-15"
d2 <- "2020-07-09"

c <- "Quebec"

db_decomp <- NULL
for(c in c("Alberta", "BC", "Montreal", "Ontario", "Quebec")){

db_d1 <- db_can_age2 %>% 
  filter(Region == c) %>% 
  filter(Date == d1)

db_d2 <- db_can_age2 %>% 
  filter(Region == c) %>% 
  filter(Date == d2)

db_decomp <- apply_kitagawa(db_d1, db_d2) %>% 
  mutate(Region = c,
         Date1 = d1,
         Date2 = d2) %>% 
  select(Region, Date1, Date2, everything()) %>% 
  bind_rows(db_decomp)

}

db_decomp2 <- db_decomp %>% 
  gather(alpha, beta, key = "Component", value = Value) %>% 
  mutate(Region2 = paste0(Region, " (", round(CFR1, 3), " - ", round(CFR2, 3), ")"))

# cfr_ref <- db_diffs_can2 %>% pull(CFR1) %>% unique() %>% round(., 3)
tx <- 8
db_decomp2 %>% 
  ggplot()+
  geom_bar(aes(Region2, Value, fill = Component, col = Component), stat = "identity", alpha = 0.5)+
  geom_point(aes(Region2, diff), col = "black", size = 2)+
  geom_hline(yintercept = 0, col = "black", size = 0.3, alpha = 0.5)+
  scale_y_continuous(limits = c(-0.1, 0.1))+
  scale_color_manual(values = c("#43a2ca", "#e34a33"), labels = c("Age structure", "Fatality"))+
  scale_fill_manual(values = c("#43a2ca", "#e34a33"), labels = c("Age structure", "Fatality"))+
  labs(title = paste0("Decomposition of CFR change between April 15 and July 9"),
       x = "Province",
       y = "CFR difference")+
  theme_bw()+
  coord_flip()+
  theme(
    legend.position="bottom",
    legend.title = element_text(size = tx),
    legend.text = element_text(size = tx - 1),
    legend.key.size = unit(0.5,"line"),
    plot.title = element_text(size = tx + 1),
    axis.text.x = element_text(size = tx),
    axis.text.y = element_text(size = tx),
    axis.title.x = element_text(size = tx + 1),
    axis.title.y = element_text(size = tx + 1)
  )

ggsave("Figures/cfr_time_diff.png", width = 5, height = 2)


