source("Code/00_functions.R")

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
  filter(Deaths_all > 0) %>% 
  mutate(Sex = recode(Sex,
                     "b" = "t"))

# Adjusting IFRs in 10-year age groups
db_ifrs2 <- db_ifrs %>% 
  mutate(Age = Age - 5,
         Region = recode(Region,
                         'BC' = "British Columbia", 
                         'All' = "Canada")) %>% 
  filter(Age %in% seq(0, 90, 10)) %>% 
  spread(Estimate, IFR)

# Merging IFRs and Canada data
db_infs <- db_can_age2 %>% 
  filter(round(Deaths, 0) > 50) %>% 
  left_join(db_ifrs2) %>% 
  mutate(Infs_l = Deaths / Upper,
         Infs = Deaths / Central,
         Infs_u = Deaths / Lower,
         under_l = Cases / Infs_u,
         under = Cases / Infs,
         under_u = Cases / Infs_l,
         Age = as.character(Age)) 

db_infs_all <- db_infs %>% 
  filter(Sex == "t") %>% 
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
levs_age <- c("All", "50-59", "60-69", "70-79", "80-89", "90+")
unique(db_infs_can$Age)

db_infs_can <- db_infs %>% 
  bind_rows(db_infs_all) %>% 
  mutate(age_all = ifelse(Age == "All", "s", "n"),
         Region = factor(Region, levels = levs),
         Age = recode(Age,
                      "50" = "50-59",
                      "60" = "60-69",
                      "70" = "70-79",
                      "80" = "80-89",
                      "90" = "90+"),
         Age = factor(Age, levels = levs_age))

# underestimation for both sexes by province
tx <- 8
db_infs_can %>% 
  filter(Sex == "t") %>% 
  ggplot()+
  geom_bar(aes(Age, under, col = age_all, fill = age_all), stat="identity", alpha = 0.5)+
  geom_errorbar(aes(Age, under, ymin=under_l, ymax=under_u, col = age_all), width=.1) +
  facet_wrap(~ Region)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_y_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 4), labels = percent_format(accuracy = 1L))+
  scale_color_manual(values = c("black", "blue"))+
  scale_fill_manual(values = c("black", "blue"))+
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

ggsave("Figures/7_underest_infections.png", width = 5, height = 3.5)

db_infs_can %>% 
  filter(Sex == "t") %>% 
  ggplot()+
  geom_bar(aes(under, Age, <- col = age_all), fill = "transparent", stat="identity", alpha = 0.4)+
  geom_errorbar(aes(under, Age, xmin = under_l, xmax = under_u, col = age_all), width = 0.5) +
  facet_wrap(~ Region, ncol = 1)+
  geom_vline(xintercept = 1, linetype = "dashed", col = "grey50")+
  scale_x_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 4), labels = percent_format(accuracy = 1L))+
  scale_color_manual(values = c("black", "blue"))+
  scale_fill_manual(values = c("black", "transparent"))+
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(5,5,5,5,"mm"),
    plot.title = element_text(size = tx-1),
    axis.text.x = element_text(size = tx - 1),
    axis.text.y = element_text(size = tx - 1),
    axis.title.x = element_text(size = tx-1),
    axis.title.y = element_text(size = tx-1),
    strip.text.x = element_text(size = 7),
    strip.background = element_rect(fill = "transparent")
  )

ggsave("Figures/7_underest_infections.png", width = 3.5, height = 4.5)


db_infs_can %>% 
  filter(Sex == "t") %>% 
  ggplot()+
  geom_point(aes(under, Age, col = age_all), alpha = 0.4)+
  geom_errorbar(aes(under, Age, xmin = under_l, xmax = under_u, col = age_all), width = 0.5) +
  facet_wrap(~ Region, ncol = 1)+
  geom_vline(xintercept = 1, linetype = "dashed", col = "grey50")+
  scale_x_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 4), labels = percent_format(accuracy = 1L))+
  scale_color_manual(values = c("black", "black"))+
  scale_fill_manual(values = c("black", "transparent"))+
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    plot.margin = margin(5,5,5,5,"mm"),
    plot.title = element_text(size = tx-1),
    axis.text.x = element_text(size = tx - 1),
    axis.text.y = element_text(size = tx - 1.5),
    axis.title.x = element_text(size = tx-1),
    axis.title.y = element_text(size = tx-1),
    strip.text.x = element_text(size = 7),
    strip.background = element_rect(fill = "transparent")
  )

ggsave("Figures/7_underest_infections.png", width = 3.5, height = 4.5)


