source("Code/00_functions.R")
levs <- c("Canada", "Alberta", "British Columbia", "Manitoba", "Ontario", "Saskatchewan", "Quebec", "Quebec_isq")
labs <- c("Canada", "Alberta", "BC", "Manitoba", "Ontario", "Saskatchewan", "Quebec", "Quebec_isq")
db_exc <- read_csv("Output/excess_weeks_8_53.csv") %>% 
  mutate(Sex = ifelse(Sex == "b", "t", Sex))
db_cov <- read_rds("Output/covid_data_by_age_sex_dates_excess.rds")

# grouping ages according to weekly all cause mortality categories
db_cov_age <- db_cov %>% 
  filter(Region != "Quebec_isq",
         Sex == "t") %>% 
  select(Region, Sex, Age, Deaths) %>% 
  mutate(Age = case_when(Age < 45 ~ 0,
                         Age >= 45 & Age < 65 ~ 45,
                         Age >= 65 & Age < 85 ~ 65,
                         Age >= 85 ~ 85)) %>% 
  group_by(Region, Sex, Age) %>% 
  summarise(Diagnosed = sum(Deaths))%>% 
  ungroup() %>% 
  mutate(Region = ifelse(Region == "All", "Canada", Region),
         Age = as.character(Age)) %>% 
  drop_na()

db_isq_age <- db_cov %>% 
  select(Region, Sex, Age, Deaths) %>% 
  filter(Region == "Quebec_isq") %>% 
  mutate(Age = case_when(Age < 49 ~ 0,
                         Age >= 50 & Age < 69 ~ 50,
                         Age >= 70 ~ 70)) %>% 
  group_by(Region, Sex, Age) %>% 
  summarise(Diagnosed = sum(Deaths))%>% 
  ungroup() %>% 
  mutate(Region = "Quebec_isq",
         Age = as.character(Age)) %>% 
  drop_na()

db_cov2 <- bind_rows(db_cov_age, db_isq_age) 

db_exc_age <- db_exc %>% 
  filter(Age != "All",
         Sex == "t") %>% 
  select(Region, Sex, Age, Exposure, Excess_epi, epi_lp, epi_up) %>% 
  rename(Excess = Excess_epi,
         lp = epi_lp,
         up = epi_up) 

db_d_age <- db_cov2 %>% 
  left_join(db_exc_age) %>% 
  gather(Diagnosed, Excess, lp, up, key = "Source", value = "Deaths") %>% 
  mutate(Age = case_when(Region != "Quebec_isq" & Age == "0" ~ "0-44",
                         Region != "Quebec_isq" & Age == "45" ~ "45-64",
                         Region != "Quebec_isq" & Age == "65" ~ "65-84",
                         Region != "Quebec_isq" & Age == "85" ~ "85+",
                         Region == "Quebec_isq" & Age == "0" ~ "0-49",
                         Region == "Quebec_isq" & Age == "50" ~ "50-69",
                         Region == "Quebec_isq" & Age == "70" ~ "70+"),
         Region = factor(Region, levels = levs, labels = labs)) %>% 
  select(-Sex)

db_d_all_ages <- db_d_age %>% 
  group_by(Region, Source) %>% 
  summarise(Deaths = sum(Deaths),
            Exposure = sum(Exposure)) %>% 
  ungroup() %>% 
  mutate(Age = "All")

# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Ratios confirmed/excess
# ~~~~~~~~~~~~~~~~~~~~~~~~~

db_d <- 
  bind_rows(db_d_age, db_d_all_ages) %>% 
  spread(Source, Deaths) %>% 
  mutate(Ratio = Diagnosed / Excess,
         up = Diagnosed / up,
         lp = Diagnosed / lp,
         age_all = ifelse(Age == "All", "All Ages", "By Age"),
         percent = 100 * (Ratio - 1))

tx <- 8
db_d %>% 
  filter(!Region %in% c("Manitoba", "Saskatchewan", "Quebec_isq")) %>% 
  mutate(Age = factor(Age, levels = c("All", "0-44", "45-64", "65-84", "85+"))) %>% 
  ggplot()+
  geom_point(aes(Age, Ratio, col = age_all), alpha = 1)+
  geom_errorbar(aes(Age, Ratio, ymin = lp, ymax = up, col = age_all), width = .1) +
  facet_grid(Region ~ Age, scales = "free_x", space = "free_x", switch="both")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_y_log10(breaks = c(0.02, 0.1, 0.25, 0.5, 1, 2, 4, 8))+
  coord_cartesian(ylim = c(0.005, 8))+
  scale_color_manual(values = c("#e76f51", "#264653"))+
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(5,5,1,2,"mm"),
    plot.title = element_text(size=tx-1),
    axis.text.x = element_text(size=tx),
    axis.text.y = element_text(size=tx-2),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=tx-1),
    strip.text.x = element_blank(),
    strip.text.y = element_text(size = 7),
    strip.background = element_rect(fill = "transparent")
  )

ggsave("Figures/6a_deaths_covid_vs_excess_ratio_can_conf_int.png", width = 5, height = 4)

db_d %>% 
  filter(Region == "Quebec_isq") %>% 
  mutate(Age = factor(Age, levels = c("All", "0-49", "50-69", "70+"))) %>%
  ggplot()+
  geom_point(aes(Age, Ratio, col = age_all), alpha = 1)+
  geom_errorbar(aes(Age, Ratio, ymin = lp, ymax = up, col = age_all), width = .1) +
  facet_grid(Region ~ Age, scales = "free_x", space = "free_x", switch="both")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_y_log10(breaks = c(0.02, 0.1, 0.25, 0.5, 1, 2, 4, 8, 16))+
  coord_cartesian(ylim = c(0.005, 8))+
  scale_color_manual(values = c("#e76f51", "#264653"))+
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(1,5,5,4,"mm"),
    plot.title = element_text(size=tx-1),
    axis.text.x = element_text(size=tx),
    axis.text.y = element_text(size=tx-2),
    axis.title.x = element_text(size=tx),
    axis.title.y = element_blank(),
    strip.text.x = element_blank(),
    strip.text.y = element_text(size = 7),
    strip.background = element_rect(fill = "transparent")
  )

ggsave("Figures/6b_deaths_covid_vs_excess_ratio_can_conf_int.png", width = 4.5, height = 1.2)


# only for summary statistics
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Ratios excess/confirmed
# ~~~~~~~~~~~~~~~~~~~~~~~~~

db_aberrant_excess <- 
  bind_rows(db_d_age, db_d_all_ages) %>% 
  spread(Source, Deaths) %>% 
  mutate(Ratio = Excess / Diagnosed)






# ~~~~~~~~~
# Rates
# ~~~~~~~~~

db_r <- 
  bind_rows(db_d_age, db_d_all_ages) %>% 
  mutate(Mx = 100000 * Deaths / Exposure) %>% 
  select(Region, Age, Source, Mx, age_all) %>% 
  spread(Source, Mx)

db_ints <- db_r %>% 
  filter(Source %in% c("lp", "up")) %>% 
  spread(Source, Mx)

db_r2 <- db_r %>% 
  filter(!Source %in% c("lp", "up")) %>% 
  left_join(db_ints) %>% 
  mutate(lp = ifelse(Source == "Excess", lp, NA),
         up = ifelse(Source == "Excess", up, NA),
         age_all = case_when(Age == "All" & Source == "Excess" ~ "All ages excess",
                             Age == "All" & Source == "Diagnosed" ~ "All ages diagnosed",
                             Age != "All" & Source == "Excess" ~ "By age excess",
                             Age != "All" & Source == "Diagnosed" ~ "By age diagnosed"),
         age_all = factor(age_all, 
                          levels = c("All ages excess", "By age excess", "All ages diagnosed", "By age diagnosed"),
                          labels = c("All_e", "Excess", "A_d", "Diagnosed")))

db_r2 %>%
  filter(!Region %in% c("Manitoba", "Saskatchewan", "Quebec_isq")) %>% 
  mutate(Age = factor(Age, levels = c("All", "0-44", "45-64", "65-84", "85+"))) %>% 
  ggplot()+
  geom_errorbar(aes(Age, Mx, ymin = lp, ymax = up, col = age_all), width = .1, alpha = 0.5) +
  geom_point(aes(Age, Mx, col = age_all), alpha = 0.7)+
  facet_grid(Region ~ Age, scales = "free_x", space = "free_x", switch="both")+
  scale_y_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000))+
  coord_cartesian(ylim = c(0.1, 5000))+
  scale_color_manual(values = c("#e76f51", "#264653", "#2a9d8f", "#2a9d8f"))+
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(5,5,1,2,"mm"),
    plot.title = element_text(size=tx-1),
    axis.text.x = element_text(size=tx),
    axis.text.y = element_text(size=tx-2),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=tx-1),
    strip.text.x = element_blank(),
    strip.text.y = element_text(size = 7),
    strip.background = element_rect(fill = "transparent")
  )

ggsave("Figures/s4a_deaths_covid_vs_excess_rates_can_conf_int.png", width = 5, height = 4)


db_r2 %>%
  filter(Region == "Quebec_isq") %>% 
  mutate(Age = factor(Age, levels = c("All", "0-49", "50-69", "70+"))) %>%
  ggplot()+
  geom_errorbar(aes(Age, Mx, ymin = lp, ymax = up, col = age_all), width = .1, alpha = 0.5) +
  geom_point(aes(Age, Mx, col = age_all), alpha = 0.7)+
  facet_grid(Region ~ Age, scales = "free_x", space = "free_x", switch="both")+
  scale_y_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000))+
  coord_cartesian(ylim = c(0.1, 5000))+
  scale_color_manual(values = c("#e76f51", "#264653", "#2a9d8f", "#2a9d8f"), 
                     breaks = c("Excess", "Diagnosed"))+
  theme_bw()+
  labs(color = "Deaths")+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = tx),
    legend.text = element_text(size = tx - 1),
    legend.margin = margin(0,0,0,0,"mm"),
    plot.margin = margin(1,5,5,5,"mm"),
    plot.title = element_text(size=tx-1),
    axis.text.x = element_text(size=tx),
    axis.text.y = element_text(size=tx-2),
    axis.title.x = element_text(size=tx),
    axis.title.y = element_blank(),
    strip.text.x = element_blank(),
    strip.text.y = element_text(size = 7),
    strip.background = element_rect(fill = "transparent")
  )

ggsave("Figures/s4b_deaths_covid_vs_excess_rates_can_conf_int.png", width = 5, height = 1.6)

