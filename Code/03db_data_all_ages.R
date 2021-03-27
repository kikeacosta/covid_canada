rm(list=ls())
source("Code/00_functions.R")
# d_max <- "2021-02-23"

# Selected countries
# ~~~~~~~~~~~~~~~~~~
ctrs <- c("Denmark",
          "Germany",
          "Italy",
          "Netherlands",
          "Sweden",
          "Spain",
          "US",
          "Canada")

territories <- c("Yukon", 
                 "Northwest Territories", 
                 "Nunavut")

atlantic <- c("New Brunswick", 
              "Newfoundland and Labrador", 
              "Nova Scotia", 
              "Prince Edward Island")

exclude <- c("Diamond Princess", 
             "Grand Princess", 
             "Repatriated Travellers")  

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data collected from the web on March 3
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # data for Quebec: https://www.inspq.qc.ca/covid-19/donnees
# # data for Ontario: https://covid-19.ontario.ca/data
# 
# # Population estimates
# # ~~~~~~~~~~~~~~~~~~~~
# # Population data from UN's 2019 Revision of World Population Prospects
# # https://population.un.org/wpp/Download/Standard/Population/
# pop_cts <- read_csv("data/pop_ctry_wpp.csv") %>%
#   mutate(region = case_when(country == "United States of America" ~ "US",
#                              TRUE ~ country),
#          pop = 1000 * pop) %>%
#   filter(region %in% ctrs,
#          region != "Canada") %>% 
#   select(region, pop)
# 
# # Population data from StatCan
# pop_can <- read_csv(unzip("Data/17100005-eng.zip", "17100005.csv")) %>% 
#   rename(region = GEO,
#          Year = REF_DATE,
#          Age = 'Age group',
#          pop = VALUE) %>% 
#   filter(Age == "All ages",
#          Sex == "Both sexes",
#          Year == 2020) %>% 
#   mutate(region = case_when(region %in% territories ~ "Territories",
#                             region %in% atlantic ~ "Atlantic",
#                               TRUE ~ region)) %>% 
#   group_by(region) %>%
#   summarise(pop = sum(pop)) %>% 
#   ungroup() %>% 
#   select(region, pop)
# 
# pop <- bind_rows(pop_cts, pop_can)
# 
# 
# # covid19 deaths
# # ~~~~~~~~~~~~~~
# # for countries from John Hopkins Database
# deaths_cts <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%
#   gather(date, deaths, 5:ncol(.)) %>%
#   rename(region = 'Country/Region',
#          prov = 'Province/State') %>%
#   filter(region %in% ctrs,
#          !prov %in% exclude) %>%
#   mutate(date = as.Date(date, "%m/%d/%y")) %>%
#   group_by(region, date) %>%
#   summarise(deaths = sum(deaths)) %>%
#   ungroup() 
# 
# # for Canadian provinces from John Hopkins Database
# deaths_can <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%
#   gather(date, deaths, 5:ncol(.)) %>%
#   mutate(date = as.Date(date, "%m/%d/%y")) %>%
#   rename(country = 'Country/Region',
#          region = 'Province/State') %>%  
#   filter(country == "Canada") %>% 
#   select(region, date, deaths) %>% 
#   filter(!(region %in% exclude),
#          !(region %in% c("Quebec", "Ontario"))) %>% 
#   mutate(region = case_when(region %in% territories ~ "Territories",
#                               region %in% atlantic ~ "Atlantic",
#                               TRUE ~ region)) %>% 
#   group_by(date, region) %>%
#   summarise(deaths = sum(deaths)) %>% 
#   ungroup()
# 
# # quebec from INSQ
# deaths_qc <- read_csv("Data/d_qc.csv") %>% 
#   rename(date = 1,
#          d1 = 2,
#          d2 = 3, 
#          d3 = 4,
#          d4 = 5) %>% 
#   mutate(date = ymd(date),
#          deaths = d1 + d2 + d3 + d4,
#          region = "Quebec") %>% 
#   select(region, date, deaths)
# 
# # ontario from covid-19.ontario
# deaths_on <- read_csv("Data/d_on.csv") %>% 
#   rename(deaths = 2) %>% 
#   mutate(date = mdy(str_sub(category, 5, 15)),
#          region = "Ontario") %>% 
#   select(region, date, deaths)
# 
# # merging death data
# corona_deaths <- 
#   bind_rows(deaths_cts,
#             deaths_can,
#             deaths_qc,
#             deaths_on) %>% 
#   arrange(region, date) %>% 
#   complete(region, date) %>% 
#   group_by(region) %>% 
#   fill(deaths)
# 
# 
# 
# # covid19 cases
# # ~~~~~~~~~~~~~
# # for countries from John Hopkins Database
# cases_cts <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>%
#   gather(date, cases, 5:ncol(.)) %>%
#   rename(region = 'Country/Region') %>%
#   filter(region %in% ctrs) %>%
#   mutate(date = as.Date(date, "%m/%d/%y")) %>%
#   group_by(region, date) %>%
#   summarise(cases = sum(cases)) %>%
#   ungroup()
# 
# # for Canadian provinces
# cases_can <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>%
#   gather(date, cases, 5:ncol(.)) %>%
#   mutate(date = as.Date(date, "%m/%d/%y")) %>%
#   rename(country = 'Country/Region',
#          region = 'Province/State') %>%  
#   filter(country == "Canada") %>% 
#   filter(!(region %in% exclude),
#          !(region %in% c("Quebec", "Ontario"))) %>% 
#   mutate(region = case_when(region %in% territories ~ "Territories",
#                             region %in% atlantic ~ "Atlantic",
#                             TRUE ~ region)) %>% 
#   group_by(date, region) %>%
#   summarise(cases = sum(cases)) %>% 
#   ungroup()
# 
# # quebec
# cases_qc <- read_csv("Data/c_qc.csv")%>% 
#   rename(date = 1,
#          c1 = 2,
#          c2 = 3) %>% 
#   separate(date, c("date", "trash"), sep = " ") %>% 
#   mutate(date = ymd(date),
#          cases = c1 + c2,
#          region = "Quebec") %>% 
#   select(region, date, cases)
# 
# # ontario
# cases_on <- read_csv("Data/c_on.csv") %>% 
#   rename(cases = 2) %>% 
#   mutate(date = mdy(str_sub(category, 5, 15)),
#          region = "Ontario") %>% 
#   select(region, date, cases)
# 
# # merging death data
# corona_cases <- 
#   bind_rows(cases_cts,
#             cases_can,
#             cases_qc,
#             cases_on) %>% 
#   arrange(region, date) %>% 
#   complete(region, date) %>% 
#   group_by(region) %>% 
#   fill(cases)
# 
# # Tests from Our World in Data
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# tests_cts <-
#   read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv",
#            col_types = cols(.default = "c")) %>%
#   rename(region = location) %>%
#   select(region, date, new_tests_smoothed) %>%
#   rename(new_t_rollav = new_tests_smoothed) %>% 
#   mutate(date = ymd(date),
#          new_t_rollav = as.double(new_t_rollav),
#          region = ifelse(region == "United States", "US", region)) %>%
#   filter(region %in% ctrs,
#          region != "Canada")
# 
# 
# # Testing data from StatCan
# # download.file("https://health-infobase.canada.ca/src/data/covidLive/covid19-download.csv", "Data/covid19-download.csv")
# exc <- c("Newfoundland and Labrador", 
#          "Northwest Territories", 
#          "Nunavut", 
#          "Prince Edward Island", 
#          "Yukon", "Northwest Territories",
#          "Repatriated travellers")
# 
# tests_can <- read_csv("Data/covid19-download.csv") %>% 
#   rename(region = prname) %>% 
#   mutate(region = case_when(region %in% territories ~ "Territories",
#                             region %in% atlantic ~ "Atlantic",
#                             TRUE ~ region)) %>% 
#   arrange(region, date) %>% 
#   group_by(region, date) %>% 
#   summarise(new_t = sum(numtestedtoday)) %>%
#   mutate(new_t_rollav = rollapply(new_t, 7, mean, align = 'center', fill = NA)) %>% 
#   ungroup() %>% 
#   select(region, date, new_t_rollav)
#   
# corona_tests <- 
#   bind_rows(tests_cts, tests_can)
# 
# # ~~~~~~~~~~~~~~~~~~
# # All data togethter
# # ~~~~~~~~~~~~~~~~~~
# 
# min_date <- ymd("2020-02-24")
# 
# db1 <-
#   corona_cases %>%
#   left_join(corona_deaths) %>%
#   left_join(corona_tests) %>%
#   left_join(pop) %>%
#   filter(date >= min_date) %>%
#   group_by(region) %>%
#   mutate(t = 1:n(),
#          new_c = cases - lag(cases),
#          new_d = deaths - lag(deaths)) %>%
#   ungroup() %>%
#   mutate(cfr = deaths / cases,
#          new_c = ifelse(new_c < 0, 0, new_c),
#          new_d = ifelse(new_d < 0, 0, new_d))
# #   
# write_rds(db1, "Output/covid_data_all_ages_selected_regions.rds")

db1 <- read_rds("Output/covid_data_all_ages_selected_regions.rds")

min_date <- ymd("2020-02-24")

db1
rollav <- 7
# l <- 0.00001

db1 %>% group_by(region) %>% 
  filter(date == min(date))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# smoothing series of new cases, new deaths, new tests, and CFRs
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp1 <- db1 %>%
  group_by(region) %>%
  mutate(new_c_rollav = rollapply(new_c, rollav, mean, align = 'center', fill = NA),
         new_d_rollav = rollapply(new_d, rollav, mean, align = 'center', fill = NA)) %>%
  ungroup()

db_sm_all_regions <- tibble()

rgs <- unique(db1$region)
r <- "Ontario"

for(r in rgs){

  temp2 <-
    temp1 %>%
    filter(region == r)

  db_sm_region <-
    tibble(t = 1:400,
           date = seq(min_date, min_date + 399, by = "1 day"))

  vars <- c("new_c_rollav", "new_d_rollav", "cfr", "new_t_rollav")
  # v <- "new_d_rollav"
  for(v in vars){

    temp3 <-
      temp2 %>%
      select(t, {{ v }}) %>%
      rename(input = {{ v }}) %>%
      drop_na(input) %>%
      mutate(log_input = log(input + 1e-10))

    md <-
      smooth.spline(x = temp3$t, y = temp3$log_input, spar = 0.3)

    varout <-
      paste0(v, "_sm")

    db_sm_region <-
      db_sm_region %>%
      left_join(tibble(t = temp3$t,
                       {{ varout }} := exp(predict(md, temp3$t)$y) - 1e-10))
  }

  temp4 <- temp2 %>%
    left_join(db_sm_region)

  db_sm_all_regions <-
    db_sm_all_regions %>%
    bind_rows(temp4)

}

# Visual inspection of smoothing results
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

measures <- c("new_c", "new_d", "cfr", "new_t")
rolls <- c("new_c_rollav", "new_d_rollav", "new_t_rollav")
splines <- c("cfr_sm", "new_c_rollav_sm", "new_d_rollav_sm", "new_t_rollav_sm")

test_sm <- db_sm_all_regions %>%
  select(region, date, new_c, new_d, cfr, new_c_rollav, new_d_rollav, cfr_sm, new_c_rollav_sm, new_d_rollav_sm) %>%
  gather(-region, -date, key = info, value = value) %>%
  mutate(source = case_when(info %in% measures ~ "original",
                            info %in% rolls ~ "roll_av",
                            info %in% splines ~ "spline"),
         measure = case_when(str_detect(info, "new_c") ~ "new_c",
                             str_detect(info, "new_d") ~ "new_d",
                             str_detect(info, "cfr") ~ "cfr"))

test_sm %>%
  ggplot()+
  geom_line(aes(date, value, col = source))+
  facet_wrap(region ~ measure, scales = "free", ncol = 3)
ggsave("Figures/visual_insp_smoothing_trends.png", width = 20, height = 20)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# estimating per capita values (per million)
db2 <- db_sm_all_regions %>%
  rename(new_c_sm = new_c_rollav_sm,
         new_d_sm = new_d_rollav_sm,
         new_t_sm = new_t_rollav_sm) %>%
  mutate(new_c_pcp_sm = 1000000 * (new_c_sm) / pop,
         new_d_pcp_sm = 1000000 * (new_d_sm) / pop,
         pos = new_c_sm / new_t_sm) %>%
  select(region, date, pop, cases, deaths, cfr, new_c_pcp_sm, new_d_pcp_sm, pos, cfr_sm)
  
write_rds(db2, "Output/covid_data_all_ages_selected_regions_smoothed.rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CFR estimates staring at the end of the first wave
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# reseting cumulative cases and deaths in July 15 2020

db2 <- read_rds("Output/covid_data_all_ages_selected_regions_smoothed.rds") %>% 
  filter(region != "Territories")

end_wave_1 <- db2 %>% 
  filter(date == "2020-07-15") %>% 
  select(region, cases, deaths) %>% 
  rename(cases_w1 = cases, 
         deaths_w1 = deaths)

db3 <- db2 %>% 
  filter(date > "2020-07-15") %>% 
  select(region, date, cases, deaths) %>% 
  left_join(end_wave_1) %>% 
  mutate(cases_w2 = cases - cases_w1,
         deaths_w2 = deaths - deaths_w1,
         cfr_w2 = deaths_w2 / cases_w2) %>% 
  group_by(region) %>% 
  mutate(t = 1:n()) %>% 
  ungroup()

rgs <- unique(db3$region)
r <- "Canada"
db_sm_region <- tibble()
for(r in rgs){
  
  temp3 <-
    db3 %>%
    filter(region == r) %>% 
    select(t, cfr_w2)%>% 
    rename(input = cfr_w2) %>%
    drop_na(input) %>%
    mutate(log_input = log(input + 1e-10))
    
  md <-
    smooth.spline(x = temp3$t, y = temp3$log_input, spar = 0.3)
  
  temp4 <- 
    tibble(t = temp3$t,
           cfr_w2_sm := exp(predict(md, temp3$t)$y) - 1e-10) %>% 
    mutate(region = r)
  
  db_sm_region <- db_sm_region %>%
    bind_rows(temp4)
  
}

db4 <- db3 %>% 
  left_join(db_sm_region)

write_rds(db4, "Output/covid_data_all_ages_selected_regions_wave2.rds")










