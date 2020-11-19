rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)
library(readxl)
# source("Code/00_functions.R")

ym <- 2010
exc_type <- "long_flu" 

db_baseline <- read_csv(paste0("Output/baseline_mortality_", ym, ".csv"),
                        col_types = cols(Age = col_character()))

# excess mortality by age since the beginning of the pandemic in Canada, 
# February 22 (week 8),until the end of the first wave, July 4 (week 27)
first_week <- 8
last_week <- 27
last_week_ont <- 27
weeks <- last_week - first_week + 1
weeks_ont <- last_week_ont - first_week + 1

levels_provs <- c("British Columbia", "Alberta", "Ontario", "Quebec", "Quebec_isq", "Canada")

db_exc <- db_baseline %>% 
  filter(Year == 2020,
         Week >= first_week & Week <= last_week,
         !(Region == "Ontario" & Week >= last_week_ont)) %>% 
  select(Region, Sex, Age, Date, Week, Deaths, Exposure, Baseline, lp, up) %>% 
  mutate(Region = factor(Region, levels = levels_provs),
         Out_intvls = ifelse(Deaths > up | Deaths < lp, 1, 0),
         # Baseline = ifelse(Baseline > Deaths, Deaths, Baseline),
         Baseline2 = ifelse(Baseline > Deaths, Deaths, Baseline),
         Excess = Deaths - Baseline,
         Excess_lp = ifelse(Deaths > up, Deaths - up, 0),
         Excess_up = ifelse(Deaths > lp, Deaths - lp, 0),
         Excess_pos = ifelse(Excess > 0, Excess, 0),
         Excess_sig = ifelse(Out_intvls == 1, Excess, 0))

db_exc2 <- db_exc %>%  
  group_by(Region, Sex, Age) %>% 
  summarise(Exposure = mean(Exposure),
            Deaths = sum(Deaths),
            Baseline = sum(Baseline),
            Baseline2 = sum(Baseline2),
            Excess = sum(Excess),
            Excess_pos = sum(Excess_pos),
            Excess_sig = sum(Excess_sig),
            Excess_lp = sum(Excess_lp),
            Excess_up = sum(Excess_up)) %>% 
  ungroup() %>% 
  arrange(Region, Sex, suppressWarnings(as.integer(Age))) %>% 
  mutate(Exposure = ifelse(Region != "Ontario",  Exposure / weeks, Exposure / weeks_ont),
         p_score = (Deaths / Baseline - 1) * 100,
         p_score_pos = (Deaths / Baseline2 - 1) * 100,
         p_score_sig = ((Baseline + Excess_sig) / Baseline - 1) * 100,
         Excess_rate = 100000 * Excess_pos / Exposure)

db_exc2 %>% 
  ggplot()+
  geom_point(aes(Age, p_score, col = Sex))+
  facet_grid(~ Region, scales="free_x")+
  geom_hline(yintercept = 0)+
  theme_bw()

db_exc2 %>% 
  ggplot()+
  geom_point(aes(Age, p_score_pos, col = Sex))+
  facet_grid(~ Region, scales="free_x")+
  geom_hline(yintercept = 0)+
  labs(y = "%")+
  theme_bw()

db_exc2 %>% 
  ggplot()+
  geom_point(aes(Age, Excess_pos, col = Sex))+
  facet_grid(~ Region, scales="free_x")+
  geom_hline(yintercept = 0)+
  labs(y = "Excess (counts)")+
  theme_bw()

db_exc2 %>% 
  ggplot()+
  geom_point(aes(Age, Excess_rate, col = Sex))+
  facet_grid(~ Region, scales="free_x")+
  geom_hline(yintercept = 0)+
  scale_y_log10()+
  labs(y = "Excess (Rates / 100K)")+
  theme_bw()


# saving excess mortality 
write_csv(db_exc2,  path = "Output/excess_weeks_8_27.csv")

