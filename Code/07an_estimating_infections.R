rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)
library(readxl)
library(scales)
library(osfr)

# COVID deaths and infections for Canada and provinces
db_ifrs <- read_rds("Output/ifr_age_sex_canada.rds")
db_cov <- read_rds("Output/covid_data_by_age_sex.rds") 

db_can_age <- db_cov %>% 
  filter(Country == "Canada") %>% 
  filter(!(Region %in% c("Montreal", "Toronto"))) %>% 
  mutate(Age = ifelse(Age >= 90, 90, Age),
         Age = floor(Age / 10) * 10) %>% 
  group_by(Region, Date, Sex, Age) %>% 
  summarise(Cases = sum(Cases),
            Deaths = sum(Deaths)) %>% 
  ungroup()

unique(db_can_age$Region)
unique(db_can_age$Age)
unique(db_can_age$Sex)

# excluding dates previous the first COVID death
db_can_age2 <- db_can_age %>% 
  select(Region, Date, Sex, Age, Cases, Deaths) %>% 
  arrange(Region, Date, Sex, Age) %>% 
  group_by(Region, Date, Sex) %>%  
  mutate(Deaths_all = sum(Deaths)) %>% 
  ungroup() %>% 
  filter(Deaths_all > 0)

# Adjusting IFRs in 10-year age groups
db_ifrs2 <- db_ifrs %>% 
  mutate(Age = Age - 5,
         Region = case_when(Region == 'BC' ~ "British Columbia", 
                            Region == 'All' ~ "Canada", 
                            TRUE ~ Region)) %>% 
  filter(Age %in% seq(0, 90, 10)) %>% 
  spread(Est, IFR)

# Merging IFRs and Canada data
db_infs <- db_can_age2 %>% 
  filter(round(Deaths, 0) > 50) %>% 
  left_join(db_ifrs2) %>% 
  mutate(Infs_l = Deaths / upper,
         Infs = Deaths / Central,
         Infs_u = Deaths / lower,
         under_l = Cases / Infs_u,
         under = Cases / Infs,
         under_u = Cases / Infs_l,
         Region = case_when(Region == "All" ~ "Canada",
                       Region == "BC" ~ "British Columbia",
                       TRUE ~ Region),
         Age = as.character(Age)) 

db_infs_all <- db_infs %>% 
  filter(Sex == "b") %>% 
  group_by(Region, Sex) %>% 
  summarise(Deaths = sum(Deaths),
            Cases = sum(Cases),
            Infs = sum(Infs),
            Infs_l = sum(Infs_l),
            Infs_u = sum(Infs_u)) %>% 
  ungroup() %>% 
  mutate(under = Cases / Infs,
         Age = "All",
         under_l = Cases / Infs_u,
         under = Cases / Infs,
         under_u = Cases / Infs_l)


levs <- c("Canada", "Alberta", "British Columbia", "Ontario", "Quebec")

db_infs_can <- db_infs %>% 
  bind_rows(db_infs_all) %>% 
  mutate(age_all = ifelse(Age == "All", "s", "n"),
         Region = factor(Region, levels = levs))

# underestimation for both sexes by province
tx <- 8
db_infs_can %>% 
  filter(Sex == "b") %>% 
  ggplot()+
  geom_point(aes(Age, under, col = age_all))+
  geom_errorbar(aes(Age, under, ymin=under_l, ymax=under_u, col = age_all), width=.1) +
  facet_wrap(~ Region)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_y_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 4), labels = percent_format(accuracy = 1L))+
  scale_color_manual(values = c("black", "blue"))+
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(5,5,5,5,"mm"),
    plot.title = element_text(size=tx-1),
    axis.text.x = element_text(size=tx),
    axis.text.y = element_text(size=tx-2),
    axis.title.x = element_text(size=tx-1),
    axis.title.y = element_text(size=tx-1),
    strip.text.x = element_text(size = 8),
    strip.background = element_rect(fill = "transparent")
  )

ggsave("Figures/underest_infections.png", width = 5, height = 3.5)


