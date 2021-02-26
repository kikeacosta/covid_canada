
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(ungroup)

pop <- read_csv(unzip("Data/17100005-eng.zip", "17100005.csv"))

age_excls <- c("18 years and over", 
               "65 years and over", 
               "90 years and over", 
               "Median age", 
               "Average age",
               "All ages")

pop2 <- pop %>% 
  rename(Region = GEO,
         Year = REF_DATE,
         Age = 'Age group',
         Pop = VALUE) %>% 
  select(Region, Year, Age, Sex, Pop) %>% 
  filter(!str_detect(Age, " to "),
         !(Age %in% age_excls),
         Year == 2020) %>% 
  mutate(Age = str_remove_all(Age, c(" years and over| years| year")),
         Sex = case_when(Sex == "Both sexes" ~ "b",
                         Sex == "Males" ~ "m",
                         Sex == "Females" ~ "f"),
         Pop = as.integer(Pop),
         Year = as.integer(Year))

ont <- read_sheet("https://docs.google.com/spreadsheets/d/1ZnOfEJv322xMkLpo6z6W9wouMeXHz_lPajtpL5UGB7o/edit#gid=1402284341")
alb <- read_sheet("https://docs.google.com/spreadsheets/d/1ajQcErmfPKtqmBJZvb7nSnwE9_awPcPim_fubM3OFNo/edit#gid=2082007904")
qc_bc <- read_sheet("https://docs.google.com/spreadsheets/d/1X2GnK5yuQIO6sc2Wcwflq9gtHyqnJZacF9VlIR_20JA/edit#gid=0",
                    col_types = "c")
can <- read_sheet("https://docs.google.com/spreadsheets/d/1oxs36Zp5Y1NJW2uAElQYsPczJBIvHKIgyo82SxQNzGM/edit#gid=0")

qc <- read_csv("Data/quebec_deaths_age_historic.csv")

qc2 <- qc %>% 
  rename(Date = 1) %>% 
  gather(-Date, key = Age, value = Deaths) %>% 
  group_by(Age) %>% 
  mutate(Deaths = cumsum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Age = str_sub(Age, 1, 2),
         Age = ifelse(Age == "0-", 0, as.integer(Age)),
         Date = ymd(Date))

qc_5dec <- qc2 %>% 
  filter(Date == "2020-12-05")

qc_2ene <- qc2 %>% 
  filter(Date == "2021-01-02")


ont2 <- ont %>% 
  select(Age_Group, CLIENT_GENDER, OUTCOME1) %>% 
  filter(OUTCOME1 == "Fatal") %>% 
  rename(Age = Age_Group,
         Sex = CLIENT_GENDER) %>% 
  group_by(Age, Sex) %>% 
  summarise(Deaths = n()) %>% 
  ungroup() %>% 
  mutate(Sex = recode(Sex,
                      "FEMALE" = "f",
                      "MALE" = "m",
                      "UNSPECIFIED" = "o"),
         Age = ifelse(Age == "<20", "0", str_sub(Age, 1, 2)),
         Date = ymd("2021-10-31"),
         Region = "Ontario")

ont_dist <- ont2 %>% 
  filter(Sex != "o") %>% 
  group_by(Age) %>% 
  mutate(All_d = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(dist = Deaths / All_d) %>% 
  select(Age, Sex, dist)

ont_b <- ont2 %>% 
  group_by(Age) %>% 
  summarise(Deaths_b = sum(Deaths)) %>% 
  ungroup() 

ont3 <- ont_dist %>% 
  left_join(ont_b) %>% 
  mutate(Deaths = Deaths_b * dist) %>% 
  select(Age, Sex, Deaths) %>% 
  complete(Age, Sex, fill = list(Deaths = 0)) %>% 
  mutate(Date = ymd("2021-10-31"),
         Region = "Ontario")

alb2 <- alb %>% 
  rename(Sex = 4,
         Age = 5, 
         Status = 6) %>% 
  select(Sex, Age, Status) %>% 
  filter(Status == "Died") %>% 
  group_by(Sex, Age) %>% 
  summarise(Deaths = n()) %>% 
  ungroup() %>% 
  mutate(Sex = recode(Sex,
                      "Female" = "f",
                      "Male" = "m"),
         Age = str_sub(Age, 1, 2)) %>% 
  complete(Age = as.character(seq(0, 80, 10)), Sex, fill = list(Deaths = 0)) %>% 
  mutate(Date = ymd("2020-12-05"),
         Region = "Alberta")


qc_bc2 <- qc_bc %>% 
  mutate(Date = dmy(Date),
         Deaths = as.integer(Value)) %>% 
  select(Region, Date, Sex, Age, Deaths) 

qc <- qc_bc2 %>% 
  filter(Region == "Quebec")

qc_dist <- qc %>% 
  filter(Sex != "b") %>% 
  group_by(Date) %>% 
  mutate(all = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(dist = Deaths / all) %>% 
  select(Date, Sex, Age, dist)
  
qb_tot <- qc %>% 
  group_by(Date) %>%
  summarise(Deaths_all = sum(Deaths))

qc2 <- qc_dist %>% 
  left_join(qb_tot) %>% 
  mutate(Deaths = round(Deaths_all * dist)) %>% 
  select(Date, Sex, Age, Deaths) %>% 
  mutate(Region = "Quebec")
  
bc <- qc_bc2 %>% 
  filter(Region == "British Columbia") %>% 
  filter(Age != "TOT")

can2 <- can %>% 
  rename(Age = 1,
         b = 2,
         m = 3,
         f = 4) %>% 
  select(Age, b, m, f) %>% 
  gather(-Age, key = "Sex", value = "Deaths") %>% 
  separate(Deaths, c("Deaths", "trash"), sep = " ") %>% 
  mutate(Deaths = str_replace(Deaths, ",", ""),
         Deaths = as.integer(Deaths),
         Age = ifelse(Age == "0-19", "0", str_sub(Age, 1, 2)),
         Date = ymd("2020-11-07"),
         Region = "Canada") %>% 
  select(-trash)
  
db <- bind_rows(can2, qc2, bc, ont3, alb2) %>% 
  mutate(Deaths = round(Deaths),
         Age = as.integer(Age))

db %>% 
  group_by(Region, Date) %>% 
  summarise(Deaths = sum(Deaths))


# ungrouping ages
r <- "Quebec"
s <- "f"
d <- "2020-12-05"

db_ung <- tibble()
rs <- unique(db$Region)
for(r in rs){
  temp1 <- db %>% 
    filter(Region == r)
  sx <- unique(temp1$Sex)
  for(s in sx){
    temp2 <- temp1 %>% 
      filter(Sex == s)
    ds <- unique(temp2$Date)
    for(d in as.list(ds)){
      temp_d <- temp2 %>% 
        filter(Date == d)

      as <- temp_d$Age
      ds <- temp_d$Deaths
      last <- 101 - max(as)
      
      temp3 <- tibble(Age = 0:100, 
                      Deaths = pclm(x = as, 
                                    y = ds, 
                                    nlast = last)$fitted,
                     Region = r,
                     Sex = s,
                     Date = d)
      
      db_ung <- db_ung %>% 
        bind_rows(temp3)
    }
  }
}

db_ung5 <- db_ung %>% 
  mutate(Age = floor(Age / 5) * 5) %>%
  group_by(Region, Date, Sex, Age) %>%
  summarise(Deaths = round(sum(Deaths))) %>%
  ungroup

db_ung5_b <- db_ung5 %>% 
  filter(Sex != "b") %>% 
  group_by(Region, Date, Age) %>% 
  summarise(Deaths = round(sum(Deaths))) %>%
  ungroup() %>% 
  mutate(Sex = "b")

out <- bind_rows(db_ung5, db_ung5_b)

write_rds(out, "Output/covid_data_by_age_sex_dates_excess.rds")


# problems when including offsets

# temp_p <- pop2 %>% 
#   mutate(Age = as.integer(Age)) %>% 
#   filter(Region == r,
#          Sex == s,
#          Age < 100) %>% 
#   pull(Pop)
# 
# as <- temp_d$Age
# ds <- temp_d$Deaths
# last <- 101 - max(as)
# 
# temp2 <- tibble(Age = 0:99, 
#                 Deaths = pclm(x = as, 
#                               y = ds, 
#                               nlast = last, 
#                               offset = temp_p)$fitted * -temp_p)
# 
# temp3 <- tibble(Age = 0:100, 
#                 Deaths = pclm(x = as, 
#                               y = ds, 
#                               nlast = last)$fitted)
# 
# 
# test <- temp3 %>% 
#   mutate(Age = floor(Age / 10) * 10,
#          Age = ifelse(Age == 100, 90, Age)) %>% 
#   group_by(Age) %>% 
#   summarise(Deaths = round(sum(Deaths))) %>% 
#   ungroup
# 
# 


qc_5dec <- qc2 %>% 
  filter(Date == "2020-12-05") %>% 
  rename(Value = Deaths)

qc_2ene <- qc2 %>% 
  filter(Date == "2021-01-02") %>% 
  rename(Value = Deaths)


qc_5dec_ung <- 
  harmonize_age(qc_5dec) %>% 
  mutate(Region = "Quebec",
         Date = "2020-12-05")

qc_2ene_ung <- 
  harmonize_age(qc_2ene) %>% 
  mutate(Region = "Quebec_isq",
         Date = "2021-01-02")


qc <- bind_rows(qc_2ene_ung, qc_5dec_ung)

qc_2ene_ung %>% 
  summarise(sum(Value))

qc_2ene %>% 
  summarise(sum(Value))

age_int <- 5

qc2 <- qc %>% 
  mutate(Age = floor(Age / age_int) * age_int) %>%
  group_by(Date, Region, Age) %>%
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% 
  mutate(Sex = "t",
         Date = ymd(Date)) %>% 
  rename(Deaths = Value)


out <- read_rds("Output/covid_data_by_age_sex_dates_excess.rds") %>% 
  filter(Region != "Quebec") %>% 
  mutate(Sex = ifelse(Sex == "b", "t", Sex)) %>% 
  bind_rows(qc2)









db_d2020 <- read_csv("https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/individual_level/mortality_2020.csv")
db_d2021 <- read_csv("https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/individual_level/mortality_2021.csv")

manit_2020 <- db_d2020 %>% 
  filter(province == "Manitoba") %>% 
  select(age, sex, date_death_report)

manit_2021 <- db_d2021 %>% 
  filter(province == "Manitoba") %>% 
  select(age, sex, date_death_report)

unique(manit$Age)

manit <- bind_rows(manit_2020, manit_2021) %>% 
  rename(Date = 3,
         Age = age,
         Sex = sex) %>% 
  mutate(Date = dmy(Date),
         Age = str_sub(Age, 1, 3),
         Age = str_replace(Age, "-", ""),
         Age = case_when(Age == "<10" ~ "0", 
                         Age == "100" ~ "90",
                         TRUE ~ Age),
         Sex = recode(Sex, 
                      "Female" = "f",
                      "Male" = "m")) %>% 
  group_by(Date, Sex, Age) %>% 
  summarise(New = n()) %>% 
  ungroup() %>% 
  complete(Date, Sex, Age, fill = list(New = 0)) %>% 
  arrange(Date) %>% 
  group_by(Sex, Age) %>% 
  mutate(Deaths = cumsum(New)) %>% 
  ungroup()

recent_mani <- manit %>% 
  filter(Date == max(Date))

manit_31oct <- 
  manit %>% 
  filter(Date == "2020-10-31")

manit_31oct %>% 
  summarise(sum(Deaths))

# Saskatchewan

unique(db_d2020$province)
sask_2020 <- db_d2020 %>% 
  filter(province == "Saskatchewan") %>% 
  select(age, sex, date_death_report)

sask_2021 <- db_d2021 %>% 
  filter(province == "Saskatchewan") %>% 
  select(age, date_death_report)

table(sask$Age)

sask <- bind_rows(sask_2020, sask_2021) %>% 
  rename(Date = 3,
         Age = age) %>% 
  mutate(Date = dmy(Date),
         Age = str_sub(Age, 1, 2),
         Age = case_when(Age == ">8" | Age == ">9" | Age == "90" ~ "80",
                         Age == "64" ~ "60",
                         Age == "No" ~ "UNK",
                         TRUE ~ Age)) %>% 
  group_by(Date, Age) %>% 
  summarise(New = n()) %>% 
  ungroup() %>% 
  complete(Date, Age, fill = list(New = 0)) %>% 
  arrange(Date) %>% 
  group_by(Age) %>% 
  mutate(Deaths = cumsum(New)) %>% 
  ungroup()

recent_sask <- sask %>% 
  filter(Date == max(Date))

sask_16nov <- 
  sask %>% 
  filter(Date == "2020-11-20")





