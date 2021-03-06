rm(list=ls())
source("Code/00_functions.R")
ym <- 2010
db_baseline <- read_csv(paste0("Output/baseline_mortality_", ym, ".csv"),
                        col_types = cols(Age = col_character()))

unique(db_baseline$Region)

# excess mortality by age since the beginning of the pandemic in Canada, 
# February 22 (week 8),until the last observation week in each province
first_week <- 8

levels_provs <- c("British Columbia", "Alberta", "Ontario", "Quebec", "Quebec_isq", "Canada", "Manitoba", "Saskatchewan")

unique(db_baseline$Region)

db_exc <- db_baseline %>% 
  filter(Year >= 2020 & Week >= first_week) %>% 
  drop_na(Deaths) %>% 
  select(Region, Sex, Age, Date, Week, Deaths, Exposure, Baseline, lp, up) %>% 
  mutate(Region = factor(Region, levels = levels_provs),
         Exposure = Exposure / 52,
         Epi_per = ifelse(Deaths > up, 1, 0),
         Baseline2 = ifelse(Baseline > Deaths, Deaths, Baseline),
         Excess = Deaths - Baseline,
         lp = ifelse(Deaths > up, Deaths - up, 0),
         up = ifelse(Deaths > lp, Deaths - lp, 0),
         Excess_pos = ifelse(Excess > 0, Excess, 0),
         pos_lp = ifelse(Deaths > up, Deaths - up, 0),
         pos_up = ifelse(Deaths > lp, Deaths - lp, 0),
         Excess_epi = ifelse(Epi_per == 1, Excess, 0),
         epi_lp = ifelse(Epi_per == 1, Deaths - up, 0),
         epi_up = ifelse(Epi_per == 1, Deaths - lp, 0))

db_exc %>% 
  group_by(Region) %>% 
  summarise(max(Date))


db_exc2 <- db_exc %>% 
  group_by(Region, Sex, Age) %>% 
  summarise(Exposure = sum(Exposure),
            Deaths = sum(Deaths),
            Baseline = sum(Baseline),
            Baseline2 = sum(Baseline2),
            Excess = sum(Excess),
            Excess_pos = sum(Excess_pos),
            Excess_epi = sum(Excess_epi),
            lp = sum(lp),
            up = sum(up),
            epi_lp = sum(epi_lp),
            epi_up = sum(epi_up)) %>% 
  ungroup() %>% 
  arrange(Region, Sex, suppressWarnings(as.integer(Age))) %>% 
  mutate(p_score = (Deaths / Baseline - 1) * 100,
         p_score_pos = ((Baseline + Excess_pos) / Baseline2 - 1) * 100,
         p_score_epi = ((Baseline + Excess_epi) / Baseline - 1) * 100,
         Excess_pos_rate = 100000 * Excess_pos / Exposure,
         Excess_epi_rate = 100000 * Excess_epi / Exposure)

db_exc2 %>% 
  ggplot()+
  geom_point(aes(Age, p_score_epi, col = Sex))+
  facet_grid(~ Region, scales = "free_x")+
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
  geom_point(aes(Age, Excess_epi, col = Sex))+
  facet_grid(~ Region, scales="free_x")+
  geom_hline(yintercept = 0)+
  labs(y = "Excess (counts)")+
  theme_bw()

db_exc2 %>% 
  ggplot()+
  geom_point(aes(Age, Excess_epi_rate, col = Sex))+
  facet_grid(~ Region, scales="free_x")+
  geom_hline(yintercept = 0)+
  scale_y_log10()+
  labs(y = "Excess (Rates / 100K)")+
  theme_bw()


# saving excess mortality 
write_csv(db_exc2, "Output/excess_weeks_8_53.csv")

