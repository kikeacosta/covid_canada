library(tidyverse)
library(lubridate)
library(googledrive)
library(googlesheets4)
library(ungroup)
source("Code/00_functions.R")

email <- "kikepaila@gmail.com"
drive_auth(email = email)
gs4_auth(email = email)

# Ontario and Toronto Jul 15
on_ss <- "https://docs.google.com/spreadsheets/d/1jy0I5FYEQ28xILTkqE5NomLINQV7ERSsxm7guaoZ81A/edit#gid=1086950234"
# Alberta Jul 15
ab_ss <- "https://docs.google.com/spreadsheets/d/1Q4YLL0l1RZ52gBR4M2LSXYOnXa38WLtKRvbLKS3rlC0/edit#gid=573062633"

# Ontario
#########

# reading Ontario data from Drive
db_on <- read_sheet(on_ss, 
                    sheet = "Data_15.07.2020.csv", 
                    na = "NA")

db_on2 <- db_on %>% 
  rename(Age = 6,
         Sex = 7,
         outcome = 9,
         place = 13) %>% 
  select(Age, Sex, outcome, place) %>% 
  mutate(Age = str_remove(Age, "s"),
         Age = case_when(Age == "<20" ~ "0", 
                         Age == "UNKNOWN" ~ "unk",
                         TRUE ~ Age),
         Sex = case_when(Sex == "FEMALE" ~ "f",
                         Sex == "MALE" ~ "m",
                         TRUE ~ "o"))

table(db_on2$Age)

db_on_deaths <- db_on2 %>% 
  filter(outcome == "Fatal") %>% 
  group_by(Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(Sex, Age, fill = list(Value = 0)) %>% 
  mutate(Measure = "Deaths")


db_on_cases <- db_on2 %>% 
  group_by(Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(Sex, Age, fill = list(Value = 0)) %>% 
  mutate(Measure = "Cases")

db_on3 <- bind_rows(db_on_deaths, db_on_cases) %>% 
  mutate(Region = "Ontario")

# Toronto
#########
db_to_deaths <- db_on2 %>% 
  filter(place == "Toronto",
         outcome == "Fatal") %>% 
  group_by(Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(Sex, Age, fill = list(Value = 0)) %>% 
  mutate(Measure = "Deaths")

db_to_cases <- db_on2 %>% 
  filter(place == "Toronto") %>% 
  group_by(Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(Sex, Age, fill = list(Value = 0)) %>% 
  mutate(Measure = "Cases")

db_to3 <- bind_rows(db_to_deaths, db_to_cases) %>% 
  mutate(Region = "Toronto")


# Alberta
#########

# reading Alberta data from Drive
db_ab <- read_sheet(ab_ss, 
                    sheet = "Data_15.07.2020.csv", 
                    na = "NA")

db_ab2 <- db_ab %>% 
  rename(Age = 5,
         Sex = 4,
         outcome = 6,
         place = 3) %>% 
  select(Age, Sex, outcome, place) %>% 
  separate(Age, c("Age", "trash"), sep = "-") %>% 
  mutate(Sex = case_when(Sex == "Female" ~ "f",
                         Sex == "Male" ~ "m",
                         TRUE ~ "o"),
         Age = case_when(Age == "80+ years" ~ "80", 
                         Age == "Under 1 year" ~ "0",
                         Age == "Unknown" ~ "unk",
                         TRUE ~ Age))

ages <- unique(db_ab2$Age)

db_ab_deaths <- db_ab2 %>% 
  filter(outcome == "Died") %>% 
  group_by(Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(Sex, Age = ages, fill = list(Value = 0)) %>% 
  mutate(Measure = "Deaths")

db_ab_cases <- db_ab2 %>% 
  group_by(Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(Sex, Age = ages, fill = list(Value = 0)) %>% 
  mutate(Measure = "Cases")

db_ab3 <- bind_rows(db_ab_deaths, db_ab_cases) %>% 
  mutate(Region = "Alberta")


# distribution unknown ages and sex values
##########################################

db_canada <- bind_rows(db_ab3, db_on3, db_to3)

db_canada2 <- distribute_unknwons(db_canada) %>% 
  mutate(Age = as.numeric(Age)) %>% 
  arrange(Region, Measure, Sex, Age)

db_canada %>% 
  group_by(Region, Measure) %>% 
  summarise(sum(Value))

db_canada2 %>% 
  group_by(Region, Measure) %>% 
  summarise(sum(Value))


# harmonizing age groups
########################

db_harm2 <- tibble()

for(pp in c("Ontario", "Toronto", "Alberta")){
  for(ms in c("Cases", "Deaths")){
    for(sx in c("f", "m")){
      chunk <- db_canada2 %>% 
        filter(Region == pp,
               Measure == ms,
               Sex == sx)
      
      db_harm <- harmonize_age(chunk) %>% 
        mutate(Region = pp,
               Measure = ms,
               Sex = sx)
      
      db_harm2 <- bind_rows(db_harm2, db_harm)
      
    }
  }
}

db_harm2 %>% 
  group_by(Region, Measure) %>% 
  summarise(sum(Value))

db_canada2 %>% 
  group_by(Region, Measure) %>% 
  summarise(sum(Value))

age_int <- 5

db_harm3 <- db_harm2 %>% 
  mutate(Age = floor(Age / age_int) * age_int) %>%
  group_by(Region, Measure, Sex, Age) %>%
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% 
  mutate(Date = "2020-07-15")

write_rds(db_harm3, "Output/db_on_to_ab_cases&deaths.rds")


# testing Alberta data
db_harm3 %>% 
  filter(Region == "Alberta",
         Measure == "Cases") %>% 
  mutate(Value = ifelse(Sex == "f", Value, -1*Value)) %>% 
  ggplot()+
  geom_area(aes(Age, Value, fill = Sex, col = Sex), alpha = 0.7)
  


