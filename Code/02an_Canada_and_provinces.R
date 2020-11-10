rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")
source("Code/00_functions.R")
library(readxl)
library(tidyverse)
library(ggrepel)
library(scales)
library(lubridate)
library(zoo)

# population data from StatCan
pop <- read_csv(unzip("Data/17100005-eng.zip", "17100005.csv"))

pop2 <- pop %>% 
  rename(province = GEO,
         Year = REF_DATE,
         Age = 'Age group',
         pop = VALUE) %>% 
  filter(Age == "All ages",
         Sex == "Both sexes",
         Year == 2020) %>% 
  select(province, pop)
  
#### covid19 deaths
db_d <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%
  gather(date, deaths, 5:ncol(.)) %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  rename(country = 'Country/Region',
         province = 'Province/State') %>%  
  filter(country == "Canada") %>% 
  select(province, date, deaths)
  

#### covid19 cases
db_c <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>%
  gather(date, cases, 5:ncol(.)) %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  rename(country = 'Country/Region',
         province = 'Province/State') %>%  
  filter(country == "Canada") %>% 
  select(province, date, cases)

unique(db1$province)

territories <- c("Yukon", "Northwest Territories", "Nunavut")
atlantic <- c("New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Prince Edward Island")
western <- c("Alberta", "Manitoba", "Saskatchewan", "British Columbia") 
prairies <- c("Manitoba", "Saskatchewan") 

exclude <- c("Diamond Princess", "Grand Princess")  


#### All data togethter
db1 <- db_c %>%
  left_join(db_d) %>% 
  left_join(pop2) %>%
  filter(!(province %in% exclude)) %>% 
  arrange(province, date) %>% 
  mutate(province = case_when(province %in% territories ~ "Territories",
                              province %in% atlantic ~ "Atlantic",
                              province %in% prairies ~ "Other Prairies",
                              TRUE ~ province)) %>% 
  group_by(province, date) %>%
  summarise(cases = sum(cases),
            deaths = sum(deaths),
            pop = sum(pop)) %>% 
  ungroup() %>% 
  group_by(province) %>% 
  mutate(new_c = cases - lag(cases),
         new_d = deaths - lag(deaths)) %>% 
  ungroup() %>% 
  mutate(new_c = ifelse(new_c < 0, 0, new_c),
         new_d = ifelse(new_d < 0, 0, new_d),
         new_c_pcp = 1000000 * (new_c + 1) / pop,
         new_d_pcp = 1000000 * (new_d + 1) / pop) %>% 
  filter(cases > 1) %>% 
  group_by(province) %>% 
  mutate(days = 1:n()) %>% 
  ungroup()

db_terr <- db1 %>% 
  filter(province == "Territories") %>% 
  mutate(new_c_pcp = 1000000 * (new_c) / pop,
         new_d_pcp = 1000000 * (new_d) / pop) %>% 
  filter(new_c_pcp > 0)

min_d <- 5
min_c <- 5
exc <- c("Territories")
ctrs <- db1 %>% filter(!(province %in% exc)) %>% pull(province) %>% unique()

db2 <- NULL
for(c in ctrs){
  
  temp <- db1 %>% 
    filter(province == c)
  
  xs <- temp %>% filter(deaths > min_d) %>% pull(days)
  ys <- temp %>% filter(deaths > min_d) %>% pull(new_d_pcp) %>% log()
  db_d_sm <- spline_this(xs, ys, 0.00001) %>% mutate(province = c, sm = exp(sm)) %>% rename(new_d_pcp_sm = sm)
  
  xs <- temp %>% filter(cases > min_c) %>% pull(days)
  ys <- temp %>% filter(cases > min_c) %>% pull(new_c_pcp) %>% log()
  db_c_sm <- spline_this(xs, ys, 0.00001) %>% mutate(province = c, sm = exp(sm)) %>% rename(new_c_pcp_sm = sm)
  
  temp2 <- temp %>% 
    left_join(db_d_sm) %>% 
    left_join(db_c_sm)
  
  db2 <- db2 %>% bind_rows(temp2) 
  
}

# # splines vs rolling average
# ############################
# library(zoo)
# db_ra <- db2 %>% 
#   group_by(province) %>% 
#   mutate(ra = rollapply(new_d_pcp, 7, mean, align = 'right', fill = NA)) %>% 
#   ungroup()
# 
# db_ra %>%
#   filter(province == "Ontario") %>% 
#   ggplot()+
#   geom_line(aes(date, new_d_pcp_sm)) +
#   geom_line(aes(date, ra), col = "red") +
#   geom_point(aes(date, new_d_pcp))
# 
# #############################

unique(db2$province)

col_country <- c("Alberta" = "#666666",
                 # "Canada" = "black",
                 "British Columbia" = "#1b9e77", 
                 "Manitoba" = "#d95f02", 
                 "New Brunswick" = "#66a61e", 
                 "Nova Scotia" = "#e6ab02", 
                 "Quebec" = "#1E8FCC",
                 "Saskatchewan" = "#e7298a",
                 "Ontario" = "#a6761d") 

col_country <- c("Western" = "#666666",
                 # "Canada" = "black",
                 "Ontario" = "#66a61e", 
                 "Quebec" = "#1E8FCC",
                 "Territories" = "#e7298a",
                 "Ontario" = "#a6761d") 

col_country <- c("Other Prairies" = "#666666",
                 "Alberta" = "#66a61e",
                 "Atlantic" = "#e6ab02",
                 "British Columbia" = "#d95f02", 
                 "Territories" = "black",
                 "Quebec" = "#1E8FCC",
                 "Ontario" = "#e7298a") 

d_x <- 0
d_xend <- max(db2$date) - 5

date <- Sys.Date()
limx <- max(db2$date) + 4

tx <- 8
labs <- db2 %>%
  bind_rows(db_terr %>% 
              mutate(new_c_pcp_sm = 10)) %>% 
  group_by(province) %>% 
  filter(date == max(date)) %>% 
  mutate(date = date + 5)

db2 %>%
  ggplot()+
  geom_line(aes(date, new_c_pcp_sm, col = province), size = .5, alpha = .9) +
  geom_point(data = db_terr, aes(date, new_c_pcp, col = province), size = .7, alpha = .8) +
  scale_x_date(limits = ymd(c("2020-03-01", "2020-12-01")), date_breaks = "1 month", date_labels = "%m/%y")+
  theme_bw()+
  geom_text_repel(data = labs,
                  aes(date, new_c_pcp_sm, label = province, col = province), size = 1.7, segment.color = NA, 
                  nudge_y = 0, nudge_x = 0, hjust = 0, force = .1, direction = "y", fontface = "bold") +
  scale_colour_manual(values = col_country)+
  labs(x = "Date",
       y = "COVID-19 cases per million",
       title = "Reported daily new COVID-19 cases per million people")+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(5,5,5,5,"mm"),
    plot.title = element_text(size=tx-1),
    axis.text.x = element_text(size=tx-3),
    axis.text.y = element_text(size=tx-3),
    axis.title.x = element_text(size=tx-1),
    axis.title.y = element_text(size=tx-1)
  )

ggsave("Figures/new_cases_provinces.png", width = 5, height = 2.4)

db_terr <- db1 %>% 
  filter(province == "Territories") %>% 
  mutate(new_c_pcp = 1000000 * (new_c) / pop,
         new_d_pcp = 1000000 * (new_d) / pop) %>% 
  filter(new_d_pcp > 0)

labs <- db2 %>%
  filter(new_d > 0) %>% 
  group_by(province) %>% 
  filter(date == max(date)) %>% 
  mutate(date = date + 3)

db2 %>%
  filter(new_d > 0) %>% 
  ggplot(aes(date, new_d_pcp_sm, col = province))+
  geom_line(size = .5, alpha = .9) +
  # scale_y_log10(expand = expansion(add = c(0,0.1))) +
  scale_x_date(limits = ymd(c("2020-03-01", "2020-12-01")), date_breaks = "1 month", date_labels = "%m/%y")+
  theme_bw()+
  geom_text_repel(data = labs,
                  aes(date, new_d_pcp_sm, label = province), size = 1.7, segment.color = NA, 
                  nudge_y = 0, nudge_x = 0, hjust = 0, force = .05, direction = "y", fontface = "bold") +
  scale_colour_manual(values = col_country)+
  labs(x = "Date",
       y = "COVID-19 deaths per million",
       title = "Reported daily new COVID-19 deaths per million people")+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(5,5,5,5,"mm"),
    plot.title = element_text(size=tx-1),
    axis.text.x = element_text(size=tx-3),
    axis.text.y = element_text(size=tx-3),
    axis.title.x = element_text(size=tx-1),
    axis.title.y = element_text(size=tx-1)
  )

ggsave("Figures/new_deaths_provinces.png", width = 5, height = 2.4)


db_t <- read_csv("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv")

unique(db_t$prname) %>% sort()
exc <- c("Newfoundland and Labrador", 
         "Northwest Territories", 
         "Nunavut", 
         "Prince Edward Island", 
         "Yukon", "Northwest Territories",
         "Repatriated travellers")

db_t2 <- db_t %>% 
  rename(province = prname) %>% 
  mutate(date = dmy(date),
         province = case_when(province %in% territories ~ "Territories",
                              province %in% atlantic ~ "Atlantic",
                              province %in% prairies ~ "Other Prairies",
                              TRUE ~ province)) %>% 
  group_by(province, date) %>% 
  summarise(n_c = sum(numtoday),
            n_t = sum(numtestedtoday)) %>%
  arrange(province, date) %>% 
  group_by(province) %>% 
  mutate(days = 1:n()) %>% 
  ungroup() %>% 
  filter(date <= "2020-10-31")
  drop_na()

# smoothing with splines
exc <- c("Territories", "Repatriated travellers")
ctrs <- db_t2 %>% filter(!(province %in% exc)) %>% pull(province) %>% unique()

db_t4 <- NULL
for(c in ctrs){
  
  temp <- db_t2 %>% 
    filter(province == c)
  
  xs <- temp %>% 
    pull(days)
  ys <- temp %>% 
    replace_na(list(n_c = 0)) %>% 
    mutate(n_c = n_c + 1) %>% 
    pull(n_c)
  db_c_sm <- spline_this(xs, ys, 0.00001) %>% mutate(province = c) %>% rename(n_c_sm = sm)
  
  xs <- temp %>% 
    pull(days)
  ys <- temp %>% 
    replace_na(list(n_t = 0)) %>% 
    mutate(n_t = n_t + 1) %>% 
    pull(n_t)
  db_t_sm <- spline_this(xs, ys, 0.00001) %>% mutate(province = c) %>% rename(n_t_sm = sm)
  
  temp2 <- temp %>% 
    left_join(db_c_sm) %>% 
    left_join(db_t_sm) %>% 
    mutate(pos = n_c_sm / n_t_sm)
  
  db_t4 <- db_t4 %>% bind_rows(temp2) 
  
}

db_t5 <- db_t4 %>% 
  filter(pos > 0)

db_t5 %>%
  ggplot()+
  geom_line(aes(date, pos, col = province))

pos_terr <- db_t2 %>% 
  filter(province == "Territories") %>% 
  drop_na() %>% 
  filter(n_c > 0,
         n_t >0) %>% 
  mutate(pos = n_c / n_t)

col_country <- c("Alberta" = "#666666",
                 "Canada" = "black",
                 "British Columbia" = "#1b9e77", 
                 "Manitoba" = "#d95f02", 
                 "New Brunswick" = "#66a61e", 
                 "Nova Scotia" = "#e6ab02", 
                 "Quebec" = "#1E8FCC",
                 "Saskatchewan" = "#e7298a",
                 "Ontario" = "#a6761d") 

col_country <- c("Other Prairies" = "#666666",
                 "Canada" = "black",
                 "Alberta" = "#66a61e",
                 "Atlantic" = "#e6ab02",
                 "British Columbia" = "#d95f02", 
                 "Territories" = "#1b9e77",
                 "Quebec" = "#1E8FCC",
                 "Ontario" = "#e7298a") 


labs <- db_t5 %>%
  bind_rows(pos_terr %>% 
              mutate(pos = 0.0244)) %>% 
  group_by(province) %>% 
  filter(date == max(date)) %>% 
  mutate(date = date + 3)

db_t5 %>%
  filter(date > "2020-03-15") %>% 
  ggplot()+
  geom_line(aes(date, pos, col = province), size = .5, alpha = .9) +
  geom_point(data = pos_terr, aes(date, pos, col = province), size = .7, alpha = .8) +
  scale_y_continuous(labels = percent_format(accuracy = 1L)) +
  scale_x_date(limits = ymd(c("2020-03-01", "2020-12-01")), date_breaks = "1 month", date_labels = "%m/%y")+
  theme_bw()+
  geom_text_repel(data = labs,
                  aes(date, pos, label = province, col = province), size = 1.7, segment.color = NA, 
                  nudge_y = 0, nudge_x = 0, hjust = 0, force = .1, direction = "y", fontface = "bold") +
  scale_colour_manual(values = col_country)+
  labs(x = "Date",
       y = "%",
       title = "Positive rate of COVID-19 tests")+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(5,5,5,5,"mm"),
    plot.title = element_text(size=tx-1),
    axis.text.x = element_text(size=tx-3),
    axis.text.y = element_text(size=tx-3),
    axis.title.x = element_text(size=tx-1),
    axis.title.y = element_text(size=tx-1)
  )

ggsave("Figures/pos_rate_provinces.png", width = 5, height = 2.4)


# our world in data
###################
# owid <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv",
#                  col_types = cols(.default = "c")) 
# 
# owid2 <- owid %>% 
#   select(location, date, positive_rate, total_cases, new_cases, total_tests, new_tests) %>% 
#   mutate(date = ymd(date),
#          n_c = as.numeric(new_cases),
#          t_c = as.numeric(total_cases),
#          t_t = as.numeric(total_tests),
#          n_t = as.numeric(new_tests),
#          pos = as.numeric(positive_rate),
#          n_pos = n_c / n_t,
#          t_pos = t_c / t_t,
#          diff1 = n_pos / pos,
#          diff2 = t_pos / pos) %>% 
#   drop_na()
  

# CFR by province

provs <- db_c %>%
  left_join(db_d) %>% 
  filter(!(province %in% exclude)) %>% 
  arrange(province, date) %>% 
  mutate(province = case_when(province %in% territories ~ "Territories",
                              province %in% atlantic ~ "Atlantic",
                              province %in% prairies ~ "Other Prairies",
                              TRUE ~ province)) %>% 
  group_by(province, date) %>%
  summarise(cases = sum(cases),
            deaths = sum(deaths)) %>% 
  ungroup()

canad <- provs %>% 
  group_by(date) %>% 
  summarise(cases = sum(cases),
            deaths = sum(deaths)) %>% 
  ungroup() %>% 
  mutate(province = "Canada")

all <- bind_rows(provs, canad) %>% 
  filter(deaths > 0) %>% 
  mutate(CFR = deaths / cases)

col_country <- c("Other Prairies" = "#666666",
                 "Canada" = "black",
                 "Alberta" = "#66a61e",
                 "Atlantic" = "#e6ab02",
                 "British Columbia" = "#d95f02", 
                 "Territories" = "#1b9e77",
                 "Quebec" = "#1E8FCC",
                 "Ontario" = "#e7298a") 

labs <- all %>%
  group_by(province) %>% 
  filter(date == max(date)) %>% 
  mutate(date = date + 3)
tx <- 8

all %>%
  ggplot(aes(date, CFR, col = province))+
  geom_point(size = .5, alpha = .8) +
  geom_vline(xintercept = ymd(c("2020-04-15")), size = .5, alpha = 0.5)+
  geom_vline(xintercept = ymd(c("2020-07-16")), size = .5, alpha = 0.5)+
  geom_vline(xintercept = ymd(c("2020-10-15")), size = .5, alpha = 0.5)+
  scale_y_continuous(labels = percent_format(accuracy = 1L)) +
  scale_x_date(limits = ymd(c("2020-03-01", "2020-12-01")), date_breaks = "1 month", date_labels = "%m/%y")+
  theme_bw()+
  geom_text_repel(data = labs,
                  aes(date, CFR, label = province), size = 2, segment.color = NA, 
                  nudge_y = 0, nudge_x = 0, hjust = 0, force = .1, direction = "y", fontface = "bold") +
  scale_colour_manual(values = col_country)+
  labs(x = "Date",
       y = "%",
       title = "CFR over time")+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(5,5,5,5,"mm"),
    plot.title = element_text(size=tx-1),
    axis.text.x = element_text(size=tx-3),
    axis.text.y = element_text(size=tx-3),
    axis.title.x = element_text(size=tx-1),
    axis.title.y = element_text(size=tx-1)
  )

ggsave("Figures/all_CFR_over_time_provinces(jhdb).png", width = 5, height = 2.4)
