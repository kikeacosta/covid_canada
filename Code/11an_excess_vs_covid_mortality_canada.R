rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)
library(readxl)
library(osfr)
# source("Code/00_functions.R")

db_exc <- read_csv("Output/excess_weeks_8_27.csv")

# OSF Data - Output_5
osf_retrieve_file("7tnfh") %>%
  osf_download(path = "Data/", conflicts = "overwrite") 

db_can <-  read_csv("Data/Output_5.zip",
                    skip = 3,
                    col_types = "ccccciiddd")

db_exc_all <- db_exc %>% 
  filter(Age != "All") %>% 
  select(Region, Sex, Excess_pos) %>% 
  rename(Deaths = Excess_pos) %>% 
  group_by(Region, Sex) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Source = "Excess")

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
         Region = ifelse(Region == "All", "Canada", Region)) %>% 
  drop_na()

db_deaths <- bind_rows(db_can_all, db_exc_all)

db_deaths %>% 
  ggplot()+
  geom_point(aes(Sex, Deaths, col = Source))+
  facet_wrap(~ Region, scales = "free")+
  scale_color_manual(values = c("red", "black"))+
  labs(title = "Identified vs Excess mortality",
       x = "Sex")+
  theme_bw()

ggsave("Figures/deaths_covid_vs_excess.png")

db_deaths2 <- db_deaths %>% 
  spread(Source, Deaths) %>% 
  drop_na() %>% 
  mutate(Ratio = Excess / Diagnosed)
tx <- 8
db_deaths2 %>% 
  filter(Sex == "b") %>% 
  ggplot()+
  geom_point(aes(Region, Ratio))+
  scale_y_log10(limits = c(0.1, 10), breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  labs(title = "Ratio Excess/Identified COVID-19 deaths all ages",
       x = "Province")+
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(5,5,5,5,"mm"),
    plot.title = element_text(size=tx-1),
    axis.text.x = element_text(size=tx-1),
    axis.text.y = element_text(size=tx-2),
    axis.title.x = element_text(size=tx-1),
    axis.title.y = element_text(size=tx-1)
  )

ggsave("Figures/deaths_covid_vs_excess_ratio_all.png", width = 5, height = 2)

#########
# by age
#########

db_exc_age <- db_exc %>% 
  filter(Age != "All") %>% 
  select(Region, Sex, Age, Excess_pos) %>% 
  rename(Deaths = Excess_pos) %>% 
  mutate(Source = "Excess")

unique(db_exc_age$Age)

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

db_d_age <- bind_rows(db_can_age, db_exc_age) %>% 
  mutate(Age = case_when(Age == "0" ~ "0-44",
                         Age == "45" ~ "45-64",
                         Age == "65" ~ "65-84",
                         Age == "85" ~ "85+"))

db_d_age %>% 
  filter(Sex == "b") %>% 
  ggplot()+
  geom_point(aes(Age, Deaths, col = Source))+
  facet_wrap(~ Region, scales = "free", ncol = 1)+
  scale_color_manual(values = c("red", "black"))+
  labs(title = "Identified vs Excess mortality by age",
       x = "Age")+
  theme_bw()

ggsave("Figures/deaths_covid_vs_excess_age.png")

db_d_age2 <- db_d_age %>% 
  spread(Source, Deaths) %>% 
  mutate(Ratio = Excess / Diagnosed)

db_d_age2 %>% 
  filter(Sex == "b") %>% 
  ggplot()+
  geom_point(aes(Age, Ratio))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_y_log10(breaks = c(0.2, 0.5, 0.8, 1, 1.5, 2, 5, 10, 20, 100, 200))+
  facet_wrap(~ Region, scales = "free", ncol = 3)+
  labs(title = "Ratio Excess/Identified COVID-19 mortality by age",
       x = "Age")+
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
    strip.background = element_rect(color = "black", 
                                    fill = "transparent", 
                                    size = 0.3, 
                                    linetype="solid")
  )

ggsave("Figures/deaths_covid_vs_excess_ratio_age.png", width = 5, height = 4.5)


col_country <- c("Other Prairies" = "#666666",
                 "Canada" = "black",
                 "Alberta" = "#66a61e",
                 "Atlantic" = "#e6ab02",
                 "British Columbia" = "#d95f02", 
                 "Territories" = "#1b9e77",
                 "Quebec" = "#1E8FCC",
                 "Ontario" = "#e7298a") 

db_d_age2 %>% 
  filter(Sex == "b") %>% 
  ggplot()+
  geom_point(aes(Age, Ratio, col = Region))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_y_log10(breaks = c(0.2, 0.5, 0.75, 1, 2, 5, 10, 20, 100, 200))+
  # scale_x_continuous(breaks = seq(0, 100, 10))+
  scale_colour_manual(values = col_country)+
  labs(title = "Ratio Excess/Identified COVID-19 mortality by age")+
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(5,5,5,5,"mm"),
    plot.title = element_text(size=tx-1),
    legend.text=element_text(size=tx-2),
    legend.title=element_text(size=tx-1),
    axis.text.x = element_text(size=tx),
    axis.text.y = element_text(size=tx-2),
    axis.title.x = element_text(size=tx-1),
    axis.title.y = element_text(size=tx-1),
    strip.text.x = element_text(size = 8),
    strip.background = element_rect(color = "black", 
                                    fill = "transparent", 
                                    size = 0.3, 
                                    linetype="solid")
  )

ggsave("Figures/deaths_covid_vs_excess_ratio_age_v2.png", width = 5, height = 3)







