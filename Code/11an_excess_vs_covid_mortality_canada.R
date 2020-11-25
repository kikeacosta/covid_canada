rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)
library(readxl)
library(osfr)
library(scales)

# source("Code/00_functions.R")
unique(db_m_age3$Region)
levs <- c("Canada", "Alberta", "British Columbia", "Ontario", "Quebec", "Quebec_isq")



db_exc <- read_csv("Output/excess_weeks_8_27.csv")

# OSF Data - Output_5
osf_retrieve_file("7tnfh") %>%
  osf_download(path = "Data/", conflicts = "overwrite") 

db_can <-  read_csv("Data/Output_5.zip",
                    skip = 3,
                    col_types = "ccccciiddd")

db_exc_all <- db_exc %>% 
  filter(Age != "All") %>% 
  select(Region, Sex, Excess_epi, epi_lp, epi_up) %>% 
  # rename(Deaths = Excess_epi) %>% 
  group_by(Region, Sex) %>% 
  summarise(Excess = sum(Excess_epi),
            lp = sum(epi_lp),
            up = sum(epi_up)) %>% 
  ungroup() %>% 
  gather(Excess, lp, up, key = "Source", value = "Deaths")
  
# looking for regions included in COVerAGE-DB
db_can2 <- db_can %>% 
  filter(Country == "Canada") %>% 
  mutate(Code = str_replace(Code, Date, ""),
         Date = dmy(Date)) %>% 
  select(-Tests) %>% 
  filter(Date == "2020-07-09",
         Region != "Montreal")

db_can_all <- db_can2 %>% 
  group_by(Region, Sex) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Source = "Diagnosed",
         Region = ifelse(Region == "All", "Canada", Region))

isq <- db_can_all %>% 
  filter(Region == "Quebec") %>% 
  mutate(Region = "Quebec_isq")


############################
# Ratios excess/confirmed
############################

db_deaths <- bind_rows(db_can_all, isq, db_exc_all) %>% 
  mutate(Region = factor(Region, levels = levs))

db_deaths2 <- db_deaths %>% 
  spread(Source, Deaths) %>% 
  drop_na() %>% 
  mutate(Ratio = Excess / Diagnosed,
         up = up / Diagnosed,
         lp = lp / Diagnosed,
         Age = "All")

db_exc_age <- db_exc %>% 
  filter(Age != "All") %>% 
  select(Region, Sex, Age, Excess_epi, epi_lp, epi_up) %>% 
  rename(Excess = Excess_epi,
         lp = epi_lp,
         up = epi_up) %>% 
  gather(Excess, lp, up, key = "Source", value = "Deaths")

unique(db_exc_age$Age)
unique(db_exc_age$Region)

db_can_age <- db_can2 %>% 
  select(Region, Sex, Age, Deaths) %>% 
  mutate(Age = case_when(Age < 45 ~ 0,
                         Age >= 45 & Age < 65 ~ 45,
                         Age >= 65 & Age < 85 ~ 65,
                         Age >= 85 ~ 85)) %>% 
  group_by(Region, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths))%>% 
  ungroup() %>% 
  mutate(Source = "Diagnosed",
         Region = ifelse(Region == "All", "Canada", Region),
         Age = as.character(Age)) %>% 
  drop_na()

db_isq_age <- db_can2 %>% 
  select(Region, Sex, Age, Deaths) %>% 
  filter(Region == "Quebec") %>% 
  mutate(Age = case_when(Age < 49 ~ 0,
                         Age >= 50 & Age < 69 ~ 50,
                         Age >= 70 ~ 70)) %>% 
  group_by(Region, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths))%>% 
  ungroup() %>% 
  mutate(Source = "Diagnosed",
         Region = "Quebec_isq",
         Age = as.character(Age)) %>% 
  drop_na()

db_d_age <- bind_rows(db_can_age, db_exc_age, db_isq_age) %>% 
  mutate(Age = case_when(Region != "Quebec_isq" & Age == "0" ~ "0-44",
                         Region != "Quebec_isq" & Age == "45" ~ "45-64",
                         Region != "Quebec_isq" & Age == "65" ~ "65-84",
                         Region != "Quebec_isq" & Age == "85" ~ "85+",
                         Region == "Quebec_isq" & Age == "0" ~ "0-49",
                         Region == "Quebec_isq" & Age == "50" ~ "50-69",
                         Region == "Quebec_isq" & Age == "70" ~ "70+"),
         Region = factor(Region, levels = levs))

db_d_age2 <- db_d_age %>% 
  spread(Source, Deaths) %>% 
  mutate(Ratio = Excess / Diagnosed,
         up = up / Diagnosed,
         lp = lp / Diagnosed)

db_ratios <- db_deaths2 %>% 
  bind_rows(db_d_age2) %>% 
  mutate(age_all = ifelse(Age == "All", "s", "n"))

tx <- 8

db_ratios %>% 
  filter(Sex == "b") %>% 
  ggplot(aes(Age, Ratio))+
  geom_point(aes(col = age_all), alpha = 0.8)+
  geom_errorbar(aes(ymin = lp, ymax = up, col = age_all), width = .1)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_y_log10(breaks = c(0.2, 0.5, 1, 2, 4, 8, 20, 50, 100, 200))+
  scale_color_manual(values = c("black", "blue"))+
  facet_wrap(~ Region, scales = "free", ncol = 3)+
  labs(x = "Age")+
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(5,5,5,5,"mm"),
    plot.title = element_text(size=tx-1),
    axis.text.x = element_text(size=tx-1, angle = 60, hjust = 1),
    axis.text.y = element_text(size=tx-2),
    axis.title.x = element_text(size=tx-1),
    axis.title.y = element_text(size=tx-1),
    strip.text.x = element_text(size = 8),
    strip.background = element_rect(fill = "transparent")
  )

ggsave("Figures/deaths_covid_vs_excess_ratio_can_conf_int.png", width = 5, height = 4)


###########
# Rates
###########

exps <- db_exc %>% 
  filter(Age != "All") %>% 
  select(Region, Sex, Age, Exposure)

db_m_age <- bind_rows(db_can_age, db_exc_age, db_isq_age) %>% 
  left_join(exps) %>% 
  mutate(Age = case_when(Region != "Quebec_isq" & Age == "0" ~ "0-44",
                         Region != "Quebec_isq" & Age == "45" ~ "45-64",
                         Region != "Quebec_isq" & Age == "65" ~ "65-84",
                         Region != "Quebec_isq" & Age == "85" ~ "85+",
                         Region == "Quebec_isq" & Age == "0" ~ "0-49",
                         Region == "Quebec_isq" & Age == "50" ~ "50-69",
                         Region == "Quebec_isq" & Age == "70" ~ "70+"))

db_m_age2 <- db_m_age %>% 
  mutate(Mx = 100000 * Deaths / Exposure,
         Region = factor(Region, levels = levs)) %>% 
  select(-Deaths, -Exposure) %>% 
  spread(Source, Mx) %>% 
  gather(Diagnosed, Excess, key = "Source", value = "Rate")

# all ages
db_m_all <- db_m_age %>%
  group_by(Region, Sex, Source) %>% 
  summarise(Deaths = sum(Deaths),
            Exposure = sum(Exposure)) %>% 
  ungroup() %>% 
  mutate(Mx = 100000 * Deaths / Exposure,
         Region = factor(Region, levels = levs)) %>% 
  select(-Deaths, -Exposure) %>% 
  spread(Source, Mx) %>% 
  gather(Diagnosed, Excess, key = "Source", value = "Rate") %>% 
  mutate(Age = "All")

db_m_can <- db_m_age2 %>% 
  bind_rows(db_m_all)

db_ints <- db_m_can %>% 
  filter(Sex == "b") %>% 
  mutate(Source = ifelse(Source == "Excess" & Age == "All", "Excess_all", Source))


db_m_can %>% 
  filter(Sex == "b") %>% 
  ggplot()+
  geom_errorbar(data = db_ints, aes(Age, Rate, ymin = lp, ymax = up, col = Source), width = .15)+
  geom_point(aes(Age, Rate, col = Source), alpha = 0.7)+
  scale_y_log10(labels = comma_format(accuracy = 1), breaks = c(1, 10, 100, 1000, 10000, 100000), limits = c(1, 150000))+
  scale_color_manual(values = c("red", "black", "blue"), breaks = c("Diagnosed", "Excess"))+
  facet_wrap(~ Region, scales = "free_x", ncol = 3)+
  labs(title = "Death Rates by age",
       x = "Age",
       y = "Rate (100K)")+
  theme_bw()+
  theme(
    legend.position = "bottom",
    legend.title = element_text(size=tx-1),
    legend.text = element_text(size=tx-2),
    panel.grid.minor = element_blank(),
    plot.margin = margin(5,5,5,5,"mm"),
    plot.title = element_text(size=tx-1),
    axis.text.x = element_text(size=tx-1, angle = 60, hjust = 1),
    axis.text.y = element_text(size=tx-2),
    axis.title.x = element_text(size=tx-1),
    axis.title.y = element_text(size=tx-1),
    strip.text.x = element_text(size = 8),
    strip.background = element_rect(fill = "transparent")
  )

ggsave("Figures/deaths_covid_vs_excess_rates_can_conf_int.png", width = 5, height = 5)

