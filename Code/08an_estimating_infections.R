source("Code/00_functions.R")

# COVID deaths and infections for Canada and provinces
db_ifrs <- read_rds("Output/ifr_age_sex_canada.rds")
db_cov <- read_rds("Output/covid_data_by_age_sex.rds") 

# filtering Canada and grouping into 10-year age groups 
db_can <- db_cov %>% 
  filter(Country == "Canada",
         !(Region == "Canada" & Type == "Province"),
         Type != "City",
         Wave == 2) %>% 
  mutate(Age = ifelse(Age >= 90, 90, Age),
         Age = floor(Age / 10) * 10) %>% 
  group_by(Region, Code, Date, Age) %>% 
  summarise(Cases = sum(Cases),
            Deaths = sum(Deaths)) %>% 
  ungroup()

# Adjusting IFRs in 10-year age groups
db_ifrs2 <- db_ifrs %>% 
  mutate(Age = Age - 5) %>% 
  filter(Age %in% seq(0, 90, 10),
         Sex == "t") %>% 
  spread(Estimate, IFR)
  

# Merging IFRs and Canada data
db_infs <- db_can %>% 
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
  group_by(Region, Code) %>% 
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


levs <- c("Canada", "Alberta", "British Columbia", "Manitoba", "Saskatchewan", "Ontario", "Quebec")
labs <- c("Canada", "Alberta", "BC", "Manitoba", "SK", "Ontario", "Quebec")
levs_code <- c("CA", "AB", "BC", "MA", "SK", "ON", "QC")
levs_age <- c("All", "40-49", "50-59", "60-69", "70-79", "80-89", "90+")
unique(db_infs_can$Age)

db_infs_can <- db_infs_all %>% 
  bind_rows(db_infs) %>% 
  mutate(age_type = ifelse(Age == "All", "All Ages", "By Age"),
         Region = factor(Region, levels = levs, labels = labs),
         Age = recode(Age,
                      "40" = "40-49",
                      "50" = "50-59",
                      "60" = "60-69",
                      "70" = "70-79",
                      "80" = "80-89",
                      "90" = "90+"),
         Age = factor(Age, levels = levs_age))

# underestimation for both sexes by province
tx <- 8
db_infs_can %>% 
  ggplot()+
  geom_point(aes(Age, under, col = age_type), alpha = 1)+
  geom_errorbar(aes(Age, under, ymin=under_l, ymax=under_u, col = age_type), width=.1) +
  facet_grid(Region ~ age_type, scales = "free_x", space = "free_x", switch="both")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_y_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 4), labels = percent_format(accuracy = 1L))+
  scale_color_manual(values = c("#e76f51", "#264653"))+
  # scale_fill_manual(values = c("black", "transparent"))+
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

ggsave("Figures/7_underest_infections.png", width = 4.5, height = 5)





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# sensitivity using Verity's IFR  estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# COVID deaths and infections for Canada and provinces
db_ifrs_ch <- read_rds("Output/ifr_age_sex_canada_verity.rds")

# Adjusting IFRs in 10-year age groups
db_ifrs_ch2 <- db_ifrs_ch %>% 
  mutate(Age = Age - 5) %>% 
  filter(Age %in% seq(0, 90, 10)) %>% 
  spread(Estimate, IFR)


# Merging IFRs and Canada data
db_infs_ch <- db_can %>% 
  filter(round(Deaths, 0) > 50) %>% 
  left_join(db_ifrs_ch2) %>% 
  mutate(Infs_l = Deaths / Upper,
         Infs = Deaths / Central,
         Infs_u = Deaths / Lower,
         under_l = Cases / Infs_u,
         under = Cases / Infs,
         under_u = Cases / Infs_l,
         Age = as.character(Age))

db_infs_ch_all <- db_infs_ch %>% 
  group_by(Region, Code) %>% 
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


unique(db_infs_can$Age)

db_infs_can_ch <- db_infs_ch_all %>% 
  bind_rows(db_infs_ch) %>% 
  mutate(age_type = ifelse(Age == "All", "All Ages", "By Age"),
         Region = factor(Region, levels = levs, labels = labs),
         Age = recode(Age,
                      "40" = "40-49",
                      "50" = "50-59",
                      "60" = "60-69",
                      "70" = "70-79",
                      "80" = "80-89",
                      "90" = "90+"),
         Age = factor(Age, levels = levs_age))

# underestimation for both sexes by province
tx <- 8
db_infs_can_ch %>% 
  ggplot()+
  geom_point(aes(Age, under, col = age_type), alpha = 1)+
  geom_errorbar(aes(Age, under, ymin=under_l, ymax=under_u, col = age_type), width=.1) +
  facet_grid(Region ~ age_type, scales = "free_x", space = "free_x", switch="both")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_y_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 4), labels = percent_format(accuracy = 1L))+
  scale_color_manual(values = c("#e76f51", "#264653"))+
  # scale_fill_manual(values = c("black", "transparent"))+
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

ggsave("Figures/s5_underest_infections_verity_ifrs.png", width = 4.5, height = 5)


db_infs_compar <- 
  bind_rows(db_infs_can %>% 
              mutate(Source = "Spain"),
            db_infs_can_ch %>% 
              mutate(Source = "China")) %>% 
  select(Region, Age, under, under_l, under_u, Source, age_type) %>%
  mutate(Source2 = paste0(Source, "_", age_type),
         Source2 = factor(Source2, 
                          levels = c("Spain_All Ages", "Spain_By Age", "China_All Ages", "China_By Age"),
                          labels = c("Spain_All Ages", "Spain", "China_All Ages", "China")))
  
unique(db_infs_compar$Source2)

db_infs_compar %>% 
  ggplot()+
  geom_point(aes(Age, under, col = Source), alpha = 0.7)+
  geom_errorbar(aes(Age, under, ymin=under_l, ymax=under_u, col = Source), width = .1) +
  facet_grid(Region ~ age_type, scales = "free_x", space = "free_x", switch="both")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_y_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 4), labels = percent_format(accuracy = 1L))+
  scale_color_manual(values = c("#264653", "#e63946"), breaks = c("Spain", "China"))+
  labs(color = "Origin of age-specific\nIFR estimates")+
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size=tx-1),
    legend.text = element_text(size=tx-1),
    legend.margin = margin(0,0,0,0,"mm"),
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
ggsave("Figures/s5_underest_infections_spain_verity_ifrs.png", width = 4.5, height = 5.5)
