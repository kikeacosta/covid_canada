rm(list=ls())
source("Code/00_functions.R")
# d_max <- "2021-02-23"

# Selected countries
# ~~~~~~~~~~~~~~~~~~
cts <- 
  c("Denmark",
    "Germany",
    "Italy",
    "Netherlands",
    "Sweden",
    "Spain",
    "US",
    "Canada")

prs <- 
  c("Canada",
    "Alberta",
    "Atlantic",
    "British Columbia", 
    "Manitoba", 
    "Ontario",
    "Quebec",
    "Saskatchewan")

db <- read_rds("Output/covid_data_all_ages_selected_regions_smoothed.rds")

db_cts <- db %>% 
  filter(region %in% cts) %>% 
  mutate(region = factor(region, levels = cts))

db_prs <- db %>% 
  filter(region %in% prs) %>% 
  mutate(region = factor(region, levels = prs))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Canada and other countries
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

col_country <- 
  c("Spain" = "grey60",
    "Canada" = "black",
    "Denmark" = "#66a61e", 
    "Italy" = "#d95f02", 
    "Netherlands" = "#7400b8", 
    "Sweden" = "#e6ab02", 
    "US" = "#1E8FCC",
    "Germany" = "#e7298a") 


d_max <- "2021-02-26"

d_x <- 0
d_xend <- max(db_cts$date) - 5
date <- Sys.Date()
limx <- max(db_cts$date) + 4

tx <- 8

db_cts %>%
  ggplot(aes(date, new_c_pcp_sm, col = region))+
  geom_line(size = .5, alpha = .9) +
  scale_y_continuous(limits = c(0, 800)) +
  scale_x_date(limits = ymd(c("2020-03-01", d_max)), date_breaks = "1 month", date_labels = "%m/%y")+
  theme_bw()+
  scale_colour_manual(values = col_country)+
  annotate(geom = "text", label = "A", 
           x = ymd("2020-03-01"), y = Inf, hjust = 0.5, vjust = 2)+
  labs(y = "COVID-19 cases per million")+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(2,1,0,1,"mm"),
    plot.title = element_text(size=tx-1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=tx-2),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=tx-2)
  )

ggsave("Figures/1a_new_cases_world.png", width = 5, height = 1.4)

db_cts %>%
  ggplot(aes(date, new_d_pcp_sm, col = region))+
  geom_line(size = .5, alpha = .9) +
  scale_y_continuous(limits = c(0, 18)) +
  scale_x_date(limits = ymd(c("2020-03-01", d_max)), date_breaks = "1 month", date_labels = "%m/%y")+
  theme_bw()+
  scale_colour_manual(values = col_country)+
  annotate(geom = "text", label = "B", 
           x = ymd("2020-03-01"), y = Inf, hjust = 0.5, vjust = 2)+
  labs(y = "COVID-19 deaths per million")+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(0,1,0,1,"mm"),
    plot.title = element_text(size=tx-1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=tx-2),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=tx-2, margin = margin(0, 3, 0, 0,"mm"))
  )

ggsave("Figures/1b_new_deaths_world.png", width = 5, height = 1.4)

db_cts %>%
  ggplot(aes(date, pos, col = region))+
  geom_line(size = .5, alpha = .9) +
  scale_y_continuous(labels = percent_format(accuracy = 1L), limits = c(0, 0.4)) +
  scale_x_date(limits = ymd(c("2020-03-01", d_max)), date_breaks = "1 month", date_labels = "%m/%y")+
  theme_bw()+
  scale_colour_manual(values = col_country)+
  annotate(geom = "text", label = "C", 
           x = ymd("2020-03-01"), y = Inf, hjust = 0.5, vjust = 2)+
  labs(x = "Date",
       y = "Positive rate")+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(1,1,1,1,"mm"),
    plot.title = element_text(size=tx-1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=tx-2),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=tx-2)
  )

ggsave("Figures/1c_pos_rate_world.png", width = 5, height = 1.4)

db_cts %>%
  ggplot(aes(date, cfr_sm, col = region))+
  geom_line(aes(size = country), size = .5, alpha = .9) +
  scale_y_continuous(labels = percent_format(accuracy = 1L)) +
  scale_x_date(limits = ymd(c("2020-03-01", d_max)), date_breaks = "1 month", date_labels = "%b/%Y")+
  scale_colour_manual(values = col_country)+
  scale_size_manual(values = c(1,1,1,1,1,1,1,2))+
  annotate(geom = "text", label = "D", 
           x = ymd("2020-03-01"), y = Inf, hjust = 0.5, vjust = 2)+
  labs(x = "Date",
       y = "Overall CFR",
       color = 'Country')+
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size=tx),
    legend.text = element_text(size=tx-1),
    legend.margin = margin(1,1,1,1,"mm"),
    plot.margin = margin(1,1,1,1,"mm"),
    plot.title = element_text(size=tx-1),
    axis.text.x = element_text(size=tx-2),
    axis.text.y = element_text(size=tx-2),
    axis.title.x = element_text(size=tx-1),
    axis.title.y = element_text(size=tx-2)
  )

ggsave("Figures/1d_all_CFR_world.png", width = 5, height = 2.3)

db_cts %>%
  group_by(region) %>% 
  filter(date == max(date)) %>% 
  select(region, cfr)

# ~~~~~~~~~~~~~~~~~~
# Canadian provinces
# ~~~~~~~~~~~~~~~~~~

col_country <- c("Other Prairies" = "grey70",
                 "Alberta" = "#66a61e",
                 "Atlantic" = "#e6ab02",
                 "British Columbia" = "#d95f02", 
                 "Territories" = "#7400b8",
                 "Canada" = "black",
                 "Quebec" = "#1E8FCC",
                 "Saskatchewan" = "grey60",
                 "Manitoba" = "#7400b8",
                 "Ontario" = "#e7298a") 

d_max <- "2021-02-26"

d_x <- 0
d_xend <- max(db_prs$date) - 5
date <- Sys.Date()
limx <- max(db_prs$date) + 4

tx <- 8


db_prs %>%
  ggplot()+
  geom_line(aes(date, new_c_pcp_sm, col = region), size = .5, alpha = .9) +
  scale_x_date(limits = ymd(c("2020-03-01", d_max)), date_breaks = "1 month", date_labels = "%m/%y")+
  theme_bw()+
  scale_colour_manual(values = col_country)+
  labs(y = "COVID-19 cases per million")+
  annotate(geom = "text", label = "A", 
           x = ymd("2020-03-01"), y = Inf, hjust = 0.5, vjust = 2)+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(0,1,0,1,"mm"),
    plot.title = element_text(size=tx-1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=tx-3),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=tx-2)
  )
ggsave("Figures/3a_new_cases_provinces.png", width = 5, height = 1.4)

db_prs %>%
  ggplot()+
  geom_line(aes(date, new_d_pcp_sm, col = region), size = .5, alpha = .9) +
  scale_x_date(limits = ymd(c("2020-03-01", d_max)), date_breaks = "1 month", date_labels = "%m/%y")+
  theme_bw()+
  scale_colour_manual(values = col_country)+
  labs(y = "COVID-19 deaths per million")+
  annotate(geom = "text", label = "B", 
           x = ymd("2020-03-01"), y = Inf, hjust = 0.5, vjust = 2)+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(0,1,0,1,"mm"),
    plot.title = element_text(size=tx-1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=tx-3),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=tx-2, margin = margin(0, 2, 0, 0,"mm"))
  )
ggsave("Figures/3b_new_deaths_provinces.png", width = 5, height = 1.4)


db_prs %>%
  ggplot()+
  geom_line(aes(date, pos, col = region), size = .5, alpha = .9) +
  scale_y_continuous(labels = percent_format(accuracy = 1L), limits = c(0, 0.4)) +
  scale_x_date(limits = ymd(c("2020-03-01", d_max)), date_breaks = "1 month", date_labels = "%m/%y")+
  theme_bw()+
  scale_colour_manual(values = col_country)+
  annotate(geom = "text", label = "C", 
           x = ymd("2020-03-01"), y = Inf, hjust = 0.5, vjust = 2)+
  labs(x = "Date",
       y = "Positive rate")+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(1,1,1,1,"mm"),
    plot.title = element_text(size=tx-1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=tx-3),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=tx-2)
  )
ggsave("Figures/3c_pos_rate_provinces.png", width = 5, height = 1.4)

db_prs %>%
  ggplot()+
  geom_line(aes(date, cfr_sm, col = region), size = .5, alpha = .8) +
  scale_y_continuous(labels = percent_format(accuracy = 1L)) +
  scale_x_date(limits = ymd(c("2020-03-01", d_max)), date_breaks = "1 month", date_labels = "%b/%y")+
  theme_bw()+
  scale_colour_manual(values = col_country)+
  annotate(geom = "text", label = "D", 
           x = ymd("2020-03-01"), y = Inf, hjust = 0.5, vjust = 2)+
  labs(x = "Date",
       y = "Overall CFR",
       color = 'Province')+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size=tx),
    legend.text = element_text(size=tx-1),
    legend.margin = margin(1,1,1,1,"mm"),
    plot.margin = margin(1,1,1,1,"mm"),
    plot.title = element_text(size=tx-1),
    axis.text.x = element_text(size=tx-2),
    axis.text.y = element_text(size=tx-3),
    axis.title.x = element_text(size=tx-1),
    axis.title.y = element_text(size=tx-2)
  )
ggsave("Figures/3d_all_CFR_over_time_provinces.png", width = 5, height = 2.3)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# summary statistics by province and wave
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

waves <- db_prs %>% 
  mutate(wave = case_when(date <= "2020-07-15" ~ "fst",
                          date >= "2020-09-15" ~ "snd",
                          TRUE ~ "0")) %>% 
  group_by(region) %>% 
  mutate(new_d = deaths - lag(deaths),
         new_c = cases - lag(cases)) %>% 
  replace_na(list(new_d = 0,
                  new_c = 0))

last <- db_prs %>% 
  group_by(region) %>% 
  filter(date == max(date)) %>% 
  select(region, cases, deaths) %>% 
  mutate(wave = "All")

summary <- waves %>% 
  group_by(region, wave) %>% 
  summarise(cases = sum(new_c),
            deaths = sum(new_d)) %>% 
  ungroup() %>% 
  filter(wave != "0") %>% 
  bind_rows(last)

sum_cases <- summary %>% 
  select(-deaths) %>% 
  spread(wave, cases) %>% 
  mutate(waves = fst + snd,
         prop_waves = waves / All,
         prop_fst = fst / All,
         prop_snd = snd / All)

sum_deaths <- summary %>% 
  select(-cases) %>% 
  spread(wave, deaths) %>% 
  mutate(waves = fst + snd,
         prop_waves = waves / All,
         prop_fst = fst / All,
         prop_snd = snd / All)

canada <- summary %>% 
  filter(region == "Canada") %>% 
  rename(all_cases = cases,
         all_deaths = deaths) %>% 
  select(-region)
  
sum_provs <- summary %>% 
  filter(region != "Canada") %>% 
  left_join(canada, by = "wave") %>% 
  mutate(prop_cases = cases / all_cases,
         prop_deaths = deaths / all_deaths)

  
