rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")
source("Code/00_functions.R")
library(readxl)
library(tidyverse)
library(ggrepel)
library(scales)
library(lubridate)
# population data from UN's 2019 Revision of World Population Prospects
# https://population.un.org/wpp/Download/Standard/Population/
pop <- read_csv("data/pop_ctry_wpp.csv") %>% 
  mutate(country = case_when(country == "United States of America" ~ "US", 
                             country == "United Kingdom" ~ "UK",
                             country == "Iran (Islamic Republic of)" ~ "Iran",
                             country == "Bolivia (Plurinational State of)" ~ "Bolivia",
                             country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                             country == "China, Taiwan Province of China" ~ "Taiwan",
                             country == "Republic of Korea" ~ "South Korea",
                             country == "Czechia" ~ "Czech Republic",
                             country == "China, Hong Kong SAR" ~ "Hong Kong",
                             TRUE ~ country),
         pop = 1000 * pop)

#### covid19 deaths
corona_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%
  gather(date, deaths, 5:ncol(.)) %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  rename(country = 'Country/Region') %>%  
  group_by(country, date) %>%
  summarise(deaths = sum(deaths)) %>%
  ungroup() %>% 
  mutate(country = case_when(country == "United Kingdom" ~ "UK",
                             country == "Taiwan*" ~ "Taiwan",
                             country == "Korea, South" ~ "South Korea",
                             country == "Taiwan*" ~ "Taiwan",
                             TRUE ~ country))

#### covid19 cases
corona_cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>%
  gather(date, cases, 5:ncol(.)) %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  rename(country = 'Country/Region') %>%  
  group_by(country, date) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>% 
  mutate(country = case_when(country == "United Kingdom" ~ "UK",
                             country == "Taiwan*" ~ "Taiwan",
                             country == "Korea, South" ~ "South Korea",
                             country == "Taiwan*" ~ "Taiwan",
                             TRUE ~ country))


ctrs <- c("Belgium", 
          "Canada", 
          "Denmark", 
          "Netherlands",
          "Sweden",
          "US",
          "Italy",
          "Germany")


#### All data togethter
db1 <- corona_cases %>%
  left_join(corona_deaths) %>% 
  left_join(pop) %>% 
  filter(country %in% ctrs) %>% 
  group_by(country) %>% 
  mutate(new_c = cases - lag(cases),
         new_d = deaths - lag(deaths)) %>% 
  ungroup() %>% 
  mutate(new_c = ifelse(new_c < 0, 0, new_c),
         new_d = ifelse(new_d < 0, 0, new_d),
         new_c_pcp = 1000000 * (new_c + 1) / pop,
         new_d_pcp = 1000000 * (new_d + 1) / pop) %>% 
  filter(cases > 1) %>% 
  group_by(country) %>% 
  mutate(days = 1:n()) %>% 
  ungroup()

min_d <- 5
min_c <- 5

c <- "Canada"
db2 <- NULL
for(c in ctrs){

  temp <- db1 %>% 
    filter(country == c)
           
  xs <- temp %>% filter(deaths > min_d) %>% pull(days)
  ys <- temp %>% filter(deaths > min_d) %>% pull(new_d_pcp) %>% log()
  db_d_sm <- spline_this(xs, ys, 0.00001) %>% mutate(country = c) %>% rename(new_d_pcp_sm = sm)
  
  xs <- temp %>% filter(cases > min_c) %>% pull(days)
  ys <- temp %>% filter(cases > min_c) %>% pull(new_c_pcp) %>% log()
  db_c_sm <- spline_this(xs, ys, 0.00001) %>% mutate(country = c) %>% rename(new_c_pcp_sm = sm)
  
  temp2 <- temp %>% 
    left_join(db_d_sm) %>% 
    left_join(db_c_sm)
  
  db2 <- db2 %>% bind_rows(temp2) 

}

db2 %>%
  filter(country == "Sweden") %>% 
  ggplot()+
  geom_line(aes(date, new_d_pcp_sm)) +
  geom_point(aes(date, new_d_pcp))
  


unique(db2$country)

col_country <- c("Belgium" = "#666666",
                 "Canada" = "black",
                 "Denmark" = "#1b9e77", 
                 "Italy" = "#d95f02", 
                 "Netherlands" = "#66a61e", 
                 "Sweden" = "#e6ab02", 
                 "US" = "#1E8FCC",
                 "Italy" = "#e7298a", 
                 "Germany" = "#a6761d") 

d_x <- 0
d_xend <- max(db2$date) - 5
d_y <- min_rate
d_yend <- min_rate * (1 + log(2) / 2) ^ d_xend

date <- Sys.Date()
limx <- max(db2$date) + 4

tx <- 8
labs <- db2 %>%
  group_by(country) %>% 
  filter(date == max(date)) %>% 
  mutate(date = date + 5)

db2 %>%
  ggplot(aes(date, new_c_pcp_sm, col = country))+
  geom_line(size = .9, alpha = .8) +
  scale_y_log10(expand = expansion(add = c(0,0.1))) +
  scale_x_date(limits = ymd(c("2020-03-01", "2020-12-01")), date_breaks = "1 month", date_labels = "%m/%y")+
  theme_bw()+
  geom_text_repel(data = labs,
                  aes(date, new_c_pcp_sm, label = country), size = 2, segment.color = NA, 
                  nudge_y = 0, nudge_x = 0, hjust = 0, force = .1, direction = "y", fontface = "bold") +
  scale_colour_manual(values = col_country)+
  labs(x = "Date",
       y = "COVID-19 cases per million (log scale)",
       title = "Reported COVID-19 cases per million people")+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(5,5,5,5,"mm"),
    plot.title = element_text(size=tx),
    axis.text.x = element_text(size=tx-1),
    axis.text.y = element_text(size=tx-1),
    axis.title.x = element_text(size=tx),
    axis.title.y = element_text(size=tx)
  )

ggsave("Figures/new_cases_world.png", width = 5, height = 2.6)

labs <- db2 %>%
  group_by(country) %>% 
  filter(date == max(date)) %>% 
  mutate(date = date + 3)

db2 %>%
  ggplot(aes(date, new_d_pcp_sm, col = country))+
  geom_line(size = .9, alpha = .8) +
  scale_y_log10(expand = expansion(add = c(0,0.1))) +
  scale_x_date(limits = ymd(c("2020-03-01", "2020-12-01")), date_breaks = "1 month", date_labels = "%m/%y")+
  theme_bw()+
  geom_text_repel(data = labs,
                  aes(date, new_d_pcp_sm, label = country), size = 2, segment.color = NA, 
                  nudge_y = 0, nudge_x = 0, hjust = 0, force = .1, direction = "y", fontface = "bold") +
  scale_colour_manual(values = col_country)+
  labs(x = "Date",
       y = "COVID-19 deaths per million (log scale)",
       title = "Reported COVID-19 deaths per million people")+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(5,5,5,5,"mm"),
    plot.title = element_text(size=tx),
    axis.text.x = element_text(size=tx-1),
    axis.text.y = element_text(size=tx-1),
    axis.title.x = element_text(size=tx),
    axis.title.y = element_text(size=tx)
  )

ggsave("Figures/new_deaths_world.png", width = 5, height = 2.6)

db_t <- read_csv("Data/owid-covid-data.csv",
                  col_types = cols(.default = "c")) 

ctrs <- c("Belgium", 
          "Canada", 
          "Denmark", 
          "Netherlands",
          "Sweden",
          "US",
          "Italy",
          "Germany")



unique(db_t$location) %>% sort()

db_t2 <- db_t %>% 
  select(location, date, positive_rate) %>% 
  mutate(date = ymd(date),
         pos = as.numeric(positive_rate),
         location = ifelse(location == "United States", "US", location)) %>% 
  filter(location %in% ctrs) %>% 
  drop_na() %>% 
  rename(country = location) %>% 
  group_by(country) %>% 
  mutate(days = 1:n()) %>% 
  ungroup()

unique(db_t2$country) %>% sort()

db_t3 <- NULL
c <- "Canada"
for(c in ctrs){
  
  temp <- db_t2 %>% 
    filter(country == c)
  
  xs <- temp %>% pull(days)
  ys <- temp %>% pull(pos) %>% log()
  db_pos_sm <- spline_this(xs, ys, 0.0000001) %>% 
    mutate(country = c) %>% 
    rename(pos_sm = sm)
  
  temp2 <- temp %>% 
    left_join(db_pos_sm)
  
  db_t3 <- db_t3 %>% bind_rows(temp2) 
  
}

db_t3 %>%
  filter(country == "Canada") %>% 
  ggplot()+
  geom_line(aes(date, pos_sm)) +
  geom_point(aes(date, pos))



labs <- db_t3 %>%
  group_by(country) %>% 
  filter(date == max(date)) %>% 
  mutate(date = date + 3)

db_t3 %>%
  ggplot(aes(date, pos_sm, col = country))+
  geom_line(size = .9, alpha = .8) +
  scale_y_log10(expand = expansion(add = c(0,0.1)), labels = percent_format(accuracy = 0.1L)) +
  scale_x_date(limits = ymd(c("2020-03-01", "2020-12-01")), date_breaks = "1 month", date_labels = "%m/%y")+
  theme_bw()+
  geom_text_repel(data = labs,
                  aes(date, pos_sm, label = country), size = 2, segment.color = NA, 
                  nudge_y = 0, nudge_x = 0, hjust = 0, force = .1, direction = "y", fontface = "bold") +
  scale_colour_manual(values = col_country)+
  labs(x = "Date",
       y = "% (log scale)",
       title = "Positive rate of COVID-19 tests")+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(5,5,5,5,"mm"),
    plot.title = element_text(size=tx),
    axis.text.x = element_text(size=tx-1),
    axis.text.y = element_text(size=tx-1),
    axis.title.x = element_text(size=tx),
    axis.title.y = element_text(size=tx)
  )

ggsave("Figures/pos_rate_world.png", width = 5, height = 2.6)
