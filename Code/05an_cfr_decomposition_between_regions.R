# functions
source("Code/00_functions.R")

db <- read_rds("Output/covid_data_by_age_sex.rds")

# Aggregating in 10-year age groups
# age_int <- 10
# db2 <- db %>%
#   mutate(Age = floor(Age / age_int) * age_int) %>%
#   group_by(Region, Code, Date, Sex, Age, Type, Wave) %>%
#   summarise(Cases = sum(Cases),
#             Deaths = sum(Deaths)) %>%
#   ungroup()


# adding Canada in the Provinces subset
can_prov <- db %>% 
  filter(Region == "Canada") %>% 
  mutate(Type = "Province")

db2 <- db %>% 
  filter(Code != "CH") %>% 
  bind_rows(can_prov)


# CFR decomposition at one specific date
########################################

db_cfr <- db2 %>% 
  group_by(Region, Code, Date, Type) %>% 
  mutate(CFR = Deaths / Cases,
         age_dist = Cases / sum(Cases),
         Cases_t = sum(Cases),
         Deaths_t = sum(Deaths),
         CFR_t = Deaths_t / Cases_t) %>% 
  ungroup()

# Countries and Canada
# ~~~~~~~~~~~~~~~~~~~~
tx <- 8
rfs <- c("Canada")
cfr_cts_w1 <- diffs_ref(db_cfr, rfs, "Country", 1, 0.06, 2.5, 2)
cfr_cts_w2 <- diffs_ref(db_cfr, rfs, "Country", 2, 0.02, 2.5, 2)

# contribution of alpha and beta components (to absolute value of change)
props <- 
  cfr_cts_w2 %>% 
  arrange(P2) %>% 
  spread(Components, Value) %>% 
  select(-c(P2cfr, P1cfr, t)) %>% 
  mutate(abs_diff = abs(alpha) + abs(beta),
         alpha_abs_prop = abs(alpha) / abs_diff,
         beta_abs_prop = abs(beta) / abs_diff)

props %>% 
  summarise(alpha_abs_prop = mean(alpha_abs_prop),
            beta_abs_prop = mean(beta_abs_prop))


# Provinces and Canada
# ~~~~~~~~~~~~~~~~~~~~
tx <- 8
rfs <- c("Canada")
cfr_provs_w1 <- diffs_ref(db_cfr, rfs, "Province", 1, 0.07, 2, 4)
cfr_provs_w2 <- diffs_ref(db_cfr, rfs, "Province", 2, 0.015, 2.5, 4)

props_prs <- 
  cfr_provs_w2 %>% 
  arrange(P2) %>% 
  spread(Components, Value) %>% 
  select(-c(P2cfr, P1cfr, t)) %>% 
  mutate(abs_diff = abs(alpha) + abs(beta),
         alpha_abs_prop = abs(alpha) / abs_diff,
         beta_abs_prop = abs(beta) / abs_diff)

props_prs %>% 
  summarise(alpha_abs_prop = mean(alpha_abs_prop),
            beta_abs_prop = mean(beta_abs_prop))



# Cities
# ~~~~~~~~~~~~~~~~~~~~
rfs <- c("Toronto")
cfr_city_w1 <- diffs_ref(db_cfr, rfs, "City", 1, 0.1, 2, 5)
cfr_city_w2 <- diffs_ref(db_cfr, rfs, "City", 2, 0.025, 2.5, 5)





lvs <- c("Canada", 
         "Spain",
         "Denmark",
         "Germany",
         "Italy",
         "Netherlands",
         "Sweden",
         "USA",
         "Alberta",
         "British Columbia",
         "Ontario",
         "Quebec",
         "Manitoba",
         "Saskatchewan",
         "Montreal",
         "Toronto", 
         "Ottawa",
         "Edmonton",
         "Calgary",
         "Berlin", 
         "New York City",
         "Madrid")

test <- db %>% 
  filter(Wave == 2,
         Code != "CH") %>% 
  gather(Cases, Deaths, key = "Measure", value = Value) %>% 
  group_by(Code, Measure) %>% 
  filter(Date == max(Date)) %>% 
  mutate(val_p = Value / sum(Value)) %>% 
  ungroup() %>% 
  mutate(val_p = ifelse(Measure == "Cases", val_p * -1, val_p),
         Region = factor(Region, levels = lvs))

xmins <- test %>% select(val_p) %>% min * 1.01
xmaxs <- test %>% select(val_p) %>% max * 1.01

cols <- c("#2a9d8f", "#e76f51")

test %>% 
  filter(Type == "Country") %>%
  ggplot()+
  geom_bar(aes(Age, val_p, fill = Measure, col = Measure), stat = "identity", alpha = 0.5)+
  geom_hline(yintercept = 0, col = "black", size = 0.3, alpha = 1)+
  scale_x_continuous(breaks = seq(0, 100, 20))+
  scale_y_continuous(limits = c(xmins, xmaxs))+
  facet_wrap(~ Region, ncol = 4)+
  coord_flip()+
  scale_fill_manual(values = cols)+
  scale_color_manual(values = cols)+
  labs(x = "Age",
       y = "Distribution")+
  theme_bw()+
  theme(
    plot.margin = margin(1,1,0,3,"mm"),
    legend.position="none",
    legend.title = element_text(size = tx),
    legend.text = element_text(size = tx - 1),
    legend.key.size = unit(0.5,"line"),
    strip.background = element_rect(fill="transparent"),
    strip.text = element_text(size = tx - 2, margin = margin(.5,.5,.5,.5, "mm")),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = tx - 2),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
ggsave("Figures/s1a_age_distribution_countries.png", width = 4, height = 1.7)

test %>% 
  filter(Type == "Province",
         !(Region == "Canada" & Type == "Province")) %>%
  ggplot()+
  geom_bar(aes(Age, val_p, fill = Measure, col = Measure), stat = "identity", alpha = 0.5)+
  geom_hline(yintercept = 0, col = "black", size = 0.3, alpha = 1)+
  scale_x_continuous(breaks = seq(0, 100, 20))+
  scale_y_continuous(limits = c(xmins, xmaxs))+
  facet_wrap(~ Region, ncol = 4)+
  coord_flip()+
  scale_fill_manual(values = cols)+
  scale_color_manual(values = cols)+
  labs(x = "Age",
       y = "Distribution")+
  theme_bw()+
  theme(
    plot.margin = margin(0,1,0,0,"mm"),
    legend.position="none",
    legend.title = element_text(size = tx),
    legend.text = element_text(size = tx - 1),
    legend.key.size = unit(0.5,"line"),
    strip.background = element_rect(fill="transparent"),
    strip.text = element_text(size = tx - 2, margin = margin(.5,.5,.5,.5, "mm")),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = tx - 2),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = tx - 1)
  )
ggsave("Figures/s1b_age_distribution.png", width = 4, height = 1.7)

test %>% 
  filter(Type == "City") %>%
  ggplot()+
  geom_bar(aes(Age, val_p, fill = Measure, col = Measure), stat = "identity", alpha = 0.5)+
  geom_hline(yintercept = 0, col = "black", size = 0.3, alpha = 1)+
  scale_x_continuous(breaks = seq(0, 100, 20))+
  scale_y_continuous(limits = c(xmins, xmaxs))+
  facet_wrap(~ Region, ncol = 4)+
  coord_flip()+
  scale_fill_manual(values = cols)+
  scale_color_manual(values = cols)+
  labs(x = "Age",
       y = "Distribution")+
  theme_bw()+
  theme(
    plot.margin = margin(0,1,0,3,"mm"),
    legend.margin = margin(0,0,0,0,"mm"),
    legend.position="bottom",
    legend.title = element_text(size = tx),
    legend.text = element_text(size = tx - 1),
    legend.key.size = unit(0.5,"line"),
    strip.background = element_rect(fill="transparent"),
    strip.text = element_text(size = tx - 2, margin = margin(.5,.5,.5,.5, "mm")),
    axis.text.x = element_text(size = tx - 2),
    axis.text.y = element_text(size = tx - 2),
    axis.title.x = element_text(size = tx),
    axis.title.y = element_blank()
  )
ggsave("Figures/s1c_age_distribution.png", width = 4, height = 2.2)



# 
# 
test %>% 
  filter(!(Region == "Canada" & Type == "Province")) %>%
  ggplot()+
  geom_bar(aes(Age, val_p, fill = Measure, col = Measure), stat = "identity", alpha = 0.5)+
  geom_hline(yintercept = 0, col = "black", size = 0.3, alpha = 1)+
  scale_x_continuous(breaks = seq(0, 100, 20))+
  facet_wrap(~ Region, scales = "free", ncol = 3)+
  coord_flip()+
  scale_fill_manual(values = cols)+
  scale_color_manual(values = cols)+
  labs(x = "Age",
       y = "Distribution")+
  theme_bw()+
  theme(
    plot.margin = margin(1,1,1,2,"mm"),
    legend.position="none",
    legend.title = element_text(size = tx),
    legend.text = element_text(size = tx - 1),
    legend.key.size = unit(0.5,"line"),
    strip.background = element_rect(fill="transparent"),
    strip.text = element_text(size = tx - 2, margin = margin(.5,.5,.5,.5, "mm")),
    axis.text.x = element_text(size = tx - 2),
    axis.text.y = element_text(size = tx - 2),
    axis.title.x = element_text(size = tx - 2),
    axis.title.y = element_text(size = tx - 2)
  )
ggsave("Figures/s1_age_distribution_countries.png", width = 4, height = 5)
