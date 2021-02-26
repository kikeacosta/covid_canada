source("Code/00_functions.R")
levs <- c("Canada", "Alberta", "British Columbia", "Manitoba", "Ontario", "Saskatchewan", "Quebec", "Quebec_isq")

db_exc <- read_csv("Output/excess_weeks_8_53.csv") %>% 
  mutate(Sex = ifelse(Sex == "b", "t", Sex))
db_cov <- read_rds("Output/covid_data_by_age_sex_dates_excess.rds")

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

db_cov_all <- db_cov %>% 
  group_by(Region, Sex) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Source = "Diagnosed")

############################
# Ratios excess/confirmed
############################

db_deaths <- bind_rows(db_cov_all, db_exc_all) %>% 
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

db_cov_age <- db_cov %>% 
  filter(Region != "Quebec_isq") %>% 
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

db_isq_age <- db_cov %>% 
  select(Region, Sex, Age, Deaths) %>% 
  filter(Region == "Quebec_isq") %>% 
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

db_d_age <- bind_rows(db_cov_age, db_isq_age, db_exc_age) %>% 
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
  mutate(Ratio = Diagnosed / Excess,
         up = Diagnosed / up,
         lp = Diagnosed / lp)

db_ratios <- db_deaths2 %>% 
  bind_rows(db_d_age2) %>% 
  mutate(age_all = ifelse(Age == "All", "All Ages", "By Age"))


tx <- 8
db_ratios %>% 
  filter(!Region %in% c("Manitoba", "Saskatchewan", "Quebec_isq"), 
         Sex == "t",
         Age != "0-44",
         Diagnosed > 50) %>% 
  mutate(Age = factor(Age, levels = c("All", "45-64", "65-84", "85+"))) %>% 
  ggplot()+
  geom_bar(aes(Age, Ratio, col = age_all, fill = age_all), stat="identity", alpha = 0.2)+
  geom_errorbar(aes(Age, Ratio, ymin = lp, ymax = up, col = age_all), width = .1) +
  facet_grid(Region ~ age_all, scales = "free", space = "free_x", switch="both")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_y_log10(breaks = c(0.05, 0.1, 0.25, 0.5, 1, 2, 4, 8), labels = percent_format(accuracy = 1L))+
  scale_color_manual(values = c("black", "black"))+
  scale_fill_manual(values = c("black", "transparent"))+
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(5,5,5,5,"mm"),
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

db_ratios %>% 
  filter(Region == "Quebec_isq", 
         Sex == "t",
         # Age != "0-49",
         Diagnosed > 50) %>% 
  # mutate(Age = factor(Age, levels = c("All", "45-64", "65-84", "85+"))) %>% 
  ggplot()+
  geom_bar(aes(Age, Ratio, col = age_all, fill = age_all), stat="identity", alpha = 0.2)+
  geom_errorbar(aes(Age, Ratio, ymin = lp, ymax = up, col = age_all), width = .1) +
  facet_grid(Region ~ age_all, scales = "free", space = "free_x", switch="both")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_y_log10(breaks = c(0.05, 0.1, 0.25, 0.5, 1, 2, 4, 8), labels = percent_format(accuracy = 1L))+
  scale_color_manual(values = c("black", "black"))+
  scale_fill_manual(values = c("black", "transparent"))+
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
    strip.text.x = element_blank(),
    strip.text.y = element_text(size = 7),
    strip.background = element_rect(fill = "transparent")
  )

ggsave("Figures/6b_deaths_covid_vs_excess_ratio_can_conf_int.png", width = 5, height = 1.2)

# tx <- 8
# 
# db_ratios %>% 
#   filter(!Region %in% c("Manitoba", "Saskatchewan")) %>% 
#   filter(Sex == "t") %>% 
#   ggplot(aes(Age, Ratio))+
#   geom_point(aes(col = age_all), alpha = 0.8)+
#   geom_errorbar(aes(ymin = lp, ymax = up, col = age_all), width = .1)+
#   geom_hline(yintercept = 1, linetype = "dashed")+
#   scale_y_log10(breaks = c(0.2, 0.5, 1, 2, 4, 8, 20, 50, 100, 200))+
#   scale_color_manual(values = c("black", "blue"))+
#   facet_wrap(~ Region, scales = "free", ncol = 3)+
#   labs(x = "Age")+
#   theme_bw()+
#   theme(
#     panel.grid.minor = element_blank(),
#     legend.position = "none",
#     plot.margin = margin(5,5,5,5,"mm"),
#     plot.title = element_text(size=tx-1),
#     axis.text.x = element_text(size=tx-1, angle = 60, hjust = 1),
#     axis.text.y = element_text(size=tx-2),
#     axis.title.x = element_text(size=tx-1),
#     axis.title.y = element_text(size=tx-1),
#     strip.text.x = element_text(size = 8),
#     strip.background = element_rect(fill = "transparent")
#   )
# 
# ggsave("Figures/6_deaths_covid_vs_excess_ratio_can_conf_int.png", width = 5, height = 4)


###########
# Rates
###########

exps <- db_exc %>% 
  filter(Age != "All") %>% 
  select(Region, Sex, Age, Exposure)

db_m_age <- bind_rows(db_cov_age, db_exc_age, db_isq_age) %>% 
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
  filter(Sex == "t") %>% 
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

