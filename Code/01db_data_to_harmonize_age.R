library(ungroup)
source("Code/00_functions.R")

email <- "kikepaila@gmail.com"
drive_auth(email = email)
gs4_auth(email = email)

# Ontario Jul 15 2020
# on_link <- "https://docs.google.com/spreadsheets/d/1jy0I5FYEQ28xILTkqE5NomLINQV7ERSsxm7guaoZ81A/edit#gid=1086950234"
# Alberta Jul 15 2020
# ab_link <- "https://docs.google.com/spreadsheets/d/1Q4YLL0l1RZ52gBR4M2LSXYOnXa38WLtKRvbLKS3rlC0/edit#gid=573062633"
# Germany and Berlin Jul 15 2020
# de_link <- "https://docs.google.com/spreadsheets/d/1ojjUdXSYLG6wwY5aGS48QBKc_56z8gbgoewYILC1owE/edit#gid=1067163235"

# data All Ontario Jul 15 2020
db_on20 <- read_csv("Data/backup/ontario_data_20200715.csv")
# data All Ontario Feb 24 2021
db_on21 <- read_csv("Data/backup/ontario_data_20210224.csv")
# data All Alberta Jul 15 2020
db_ab20 <- read_csv("Data/backup/alberta_data_20200715.csv")
# data All Alberta Feb 23 2021
db_ab21 <- read_csv("Data/backup/alberta_data_20210223.csv")


# ~~~~~~~~~~~~~~~~~~~~~
# Ontario, July 15 2020
# ~~~~~~~~~~~~~~~~~~~~~
db_on20_2 <- db_on20 %>% 
  rename(Age = 6,
         Sex = 7,
         outcome = 9,
         place = 13) %>% 
  select(Age, Sex, outcome, place) %>% 
  mutate(Age = str_remove(Age, "s"),
         Age = case_when(Age == "<20" ~ "0", 
                         Age == "UNKNOWN" ~ "UNK",
                         TRUE ~ Age),
         Sex = case_when(Sex == "FEMALE" ~ "f",
                         Sex == "MALE" ~ "m",
                         TRUE ~ "UNK"))

table(db_on20_2$Age)

db_on20_deaths <- db_on20_2 %>% 
  filter(outcome == "Fatal") %>% 
  group_by(Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(Sex, Age, fill = list(Value = 0)) %>% 
  mutate(Measure = "Deaths")

db_on20_cases <- db_on20_2 %>% 
  group_by(Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(Sex, Age, fill = list(Value = 0)) %>% 
  mutate(Measure = "Cases")

db_on20_3 <- bind_rows(db_on20_deaths, db_on20_cases) %>% 
  mutate(Region = "Ontario")

# ~~~~~~~~
# Toronto July 2020
# ~~~~~~~~

# July 15 2020
db_to20_deaths <- db_on20_2 %>% 
  filter(place == "Toronto",
         outcome == "Fatal") %>% 
  group_by(Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(Sex, Age, fill = list(Value = 0)) %>% 
  mutate(Measure = "Deaths")

db_to20_cases <- db_on20_2 %>% 
  filter(place == "Toronto") %>% 
  group_by(Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(Sex, Age, fill = list(Value = 0)) %>% 
  mutate(Measure = "Cases")

db_to20 <- bind_rows(db_to20_deaths, db_to20_cases) %>% 
  mutate(Region = "Toronto")

# all ontario 2020
# ~~~~~~~~~~~~~~~~
db_on20_all <- bind_rows(db_to20,
                         db_on20_3) %>% 
  group_by(Region, Measure, Age) %>% 
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% 
  mutate(Date = ymd("2020-07-15"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Ontario, February 24 2021
# ~~~~~~~~~~~~~~~~~~~~~~~~~
db_on21_2 <- db_on21 %>% 
  rename(Age = 6,
         Sex = 7,
         outcome = 9,
         place = 14) %>% 
  select(Age, Sex, outcome, place) %>% 
  mutate(Age = str_remove(Age, "s"),
         Age = case_when(Age == "<20" ~ "0",
                         Age == "90+" ~ "90",
                         Age == "UNKNOWN" ~ "UNK",
                         TRUE ~ Age),
         Sex = case_when(Sex == "FEMALE" ~ "f",
                         Sex == "MALE" ~ "m",
                         TRUE ~ "UNK"))

table(db_on21_2$Age)
table(db_on21_2$place)

db_on21_deaths <- db_on21_2 %>% 
  filter(outcome == "Fatal") %>% 
  group_by(Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(Sex, Age, fill = list(Value = 0)) %>% 
  mutate(Measure = "Deaths")

db_on21_cases <- db_on21_2 %>% 
  group_by(Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(Sex, Age, fill = list(Value = 0)) %>% 
  mutate(Measure = "Cases")

db_on21_3 <- bind_rows(db_on21_deaths, db_on21_cases) %>% 
  mutate(Region = "Ontario")

# ~~~~~~~~~~~~~~~~~~~~~
# Toronto February 2021
# ~~~~~~~~~~~~~~~~~~~~~
db_to21_deaths <- db_on21_2 %>% 
  filter(place == "Toronto",
         outcome == "Fatal") %>% 
  group_by(Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(Sex, Age, fill = list(Value = 0)) %>% 
  mutate(Measure = "Deaths")

db_to21_cases <- db_on21_2 %>% 
  filter(place == "Toronto") %>% 
  group_by(Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(Sex, Age, fill = list(Value = 0)) %>% 
  mutate(Measure = "Cases")

db_to21 <- bind_rows(db_to21_deaths, db_to21_cases) %>% 
  mutate(Region = "Toronto") %>% 
  group_by(Region, Measure, Age) %>% 
  summarise(Value = sum(Value)) %>% 
  ungroup() 

# ~~~~~~~~~~~~~~~~~~~~~
# Ottawa February 2021
# ~~~~~~~~~~~~~~~~~~~~~
unique(db_on21_2$place)

db_ot21_deaths <- db_on21_2 %>% 
  filter(place == "Ottawa",
         outcome == "Fatal") %>% 
  group_by(Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(Sex, Age, fill = list(Value = 0)) %>% 
  mutate(Measure = "Deaths")

db_ot21_cases <- db_on21_2 %>% 
  filter(place == "Ottawa") %>% 
  group_by(Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(Sex, Age, fill = list(Value = 0)) %>% 
  mutate(Measure = "Cases")

db_ot21 <- bind_rows(db_ot21_cases, db_ot21_deaths) %>% 
  mutate(Region = "Ottawa") %>% 
  group_by(Region, Measure, Age) %>% 
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% 
  complete(Region, Measure, Age, fill = list(Value = 0))

# all Ontario 2021
# ~~~~~~~~~~~~~~~~
db_on21_all <- bind_rows(db_to21,
                         db_ot21,
                         db_on21_3) %>% 
  group_by(Region, Measure, Age) %>% 
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% 
  mutate(Date = ymd("2021-02-24"))

# ~~~~~~~~~~~~~~~~~
# Alberta July 2020
# ~~~~~~~~~~~~~~~~~
db_ab20_2 <- db_ab20 %>% 
  rename(Age = 5,
         Sex = 4,
         outcome = 6,
         place = 3) %>% 
  select(Age, Sex, outcome, place) %>% 
  separate(Age, c("Age", "trash"), sep = "-") %>% 
  mutate(Sex = case_when(Sex == "Female" ~ "f",
                         Sex == "Male" ~ "m",
                         TRUE ~ "UNK"),
         Age = case_when(Age == "80+ years" ~ "80", 
                         Age == "Under 1 year" ~ "0",
                         Age == "Unknown" ~ "UNK",
                         TRUE ~ Age))

ages <- unique(db_ab20_2$Age)

db_ab20_deaths <- db_ab20_2 %>% 
  filter(outcome == "Died") %>% 
  group_by(Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(Sex, Age = ages, fill = list(Value = 0)) %>% 
  mutate(Measure = "Deaths",
         Region = "Alberta")

db_ab20_cases <- db_ab20_2 %>% 
  group_by(Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(Sex, Age = ages, fill = list(Value = 0)) %>% 
  mutate(Measure = "Cases",
         Region = "Alberta")

db_ab20_all <- bind_rows(db_ab20_deaths, db_ab20_cases) %>% 
  group_by(Region, Measure, Age) %>% 
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% 
  mutate(Region = "Alberta",
         Date = ymd("2020-07-15"))

# ~~~~~~~~~~~~~~~~
# Alberta Feb 2021
# ~~~~~~~~~~~~~~~~
db_ab21_2 <- db_ab21 %>% 
  rename(Age = 4,
         Sex = 3,
         outcome = 5,
         place = 2) %>% 
  select(Age, Sex, outcome, place) %>% 
  separate(Age, c("Age", "trash"), sep = "-") %>% 
  mutate(Sex = case_when(Sex == "Female" ~ "f",
                         Sex == "Male" ~ "m",
                         TRUE ~ "UNK"),
         Age = case_when(Age == "80+ years" ~ "80", 
                         Age == "Under 1 year" ~ "0",
                         Age == "Unknown" ~ "UNK",
                         TRUE ~ Age))

ages <- unique(db_ab21_2$Age)

unique(db_ab21_2$outcome)

db_ab21_deaths <- db_ab21_2 %>% 
  filter(outcome == "Died") %>% 
  group_by(Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(Sex, Age = ages, fill = list(Value = 0)) %>% 
  mutate(Measure = "Deaths",
         Region = "Alberta")

db_ab21_cases <- db_ab21_2 %>% 
  group_by(Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(Sex, Age = ages, fill = list(Value = 0)) %>% 
  mutate(Measure = "Cases",
         Region = "Alberta")

# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Edmonton and Calgary 2021
# ~~~~~~~~~~~~~~~~~~~~~~~~~
db_ed_ca_deaths <- db_ab21_2 %>% 
  filter(outcome == "Died",
         place %in% c("Edmonton Zone", "Calgary Zone")) %>%
  group_by(place, Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(place, Sex, Age = ages, fill = list(Value = 0)) %>% 
  mutate(Measure = "Deaths",
         Region = recode(place,
                         "Edmonton Zone" = "Edmonton",
                         "Calgary Zone" = "Calgary")) %>% 
  select(-place)

db_ed_ca_cases <- db_ab21_2 %>% 
  filter(place %in% c("Edmonton Zone", "Calgary Zone")) %>% 
  group_by(place, Sex, Age) %>% 
  summarise(Value = sum(n())) %>% 
  ungroup() %>% 
  complete(place, Sex, Age = ages, fill = list(Value = 0)) %>% 
  mutate(Measure = "Cases",
         Region = recode(place,
                         "Edmonton Zone" = "Edmonton",
                         "Calgary Zone" = "Calgary")) %>% 
  select(-place)


# merging Alberta, Calgary, and Edmonton in February
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_ab21_all <- bind_rows(db_ab21_deaths,
                         db_ab21_cases,
                         db_ed_ca_deaths,
                         db_ed_ca_cases) %>% 
  group_by(Region, Measure, Age) %>% 
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% 
  mutate(Date = ymd("2021-02-23"))



# all data in Ontario and Alberta
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_on_ab <- bind_rows(db_on20_all,
                      db_on21_all,
                      db_ab20_all,
                      db_ab21_all)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~
# Germany and Berlin
# ~~~~~~~~~~~~~~~~~~
# db_de <- read_sheet(de_link)
# read_csv("Data/backup/germany_data_20200715.csv")
# db_de2 <- db_de %>%
#   mutate(Sex = case_when(Geschlecht == "M" ~ "m",
#                          Geschlecht == "W" ~ "f",
#                          Geschlecht == "unbekannt" ~ "UNK"),
#          Age = case_when(Altersgruppe == "A00-A04" ~ "0",
#                          Altersgruppe == "A05-A14" ~ "5",
#                          Altersgruppe == "A15-A34" ~ "15",
#                          Altersgruppe == "A35-A59" ~ "35",
#                          Altersgruppe == "A60-A79" ~ "60",
#                          Altersgruppe == "A80+" ~ "80",
#                          Altersgruppe == "unbekannt" ~ "UNK"),
#          date_f = ymd(str_sub(Meldedatum, 1, 10)),
#          Cases = ifelse(AnzahlFall < 0, 0, AnzahlFall),
#          Deaths = ifelse(AnzahlTodesfall < 0, 0, AnzahlTodesfall),
#          Region = Bundesland) %>%
#   select(Sex, Age, Cases, Deaths, Region) %>%
#   pivot_longer(Cases:Deaths, names_to = "Measure", values_to ="Value") %>%
#   group_by(Region, Sex, Measure, Age) %>%
#   summarize(Value = sum(Value)) %>%
#   ungroup()
# 
# db_de_all <- db_de2 %>%
#   group_by(Sex, Measure, Age) %>%
#   summarize(Value = sum(Value)) %>%
#   ungroup() %>%
#   mutate(Region = "Germany")
# 
# db_berlin <- db_de2 %>%
#   filter(Region == "Berlin")
# 
# db_de3 <- bind_rows(db_de_all, db_berlin)
# 
# # cleaning memory
# # rm(db_de); gc()
# 
# write_rds(db_ger3, "Output/germany_berlin_20200715.rds")
db_de_all <- read_rds("Output/germany_berlin_20200715.rds")


# grouping sexes and distributing unkown ages
db_de_all_sex <- db_de_all %>% 
  group_by(Region, Measure, Age) %>% 
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% 
  mutate(Date = ymd("2020-07-15"))


# ~~~~~~~~~~~~~~~~~~~~~~~
# february data in Canada
# ~~~~~~~~~~~~~~~~~~~~~~~
feb_link <- "https://docs.google.com/spreadsheets/d/1K811cyGx6FiSXADlHOgZOdosW01ug_nKtUciCES-r7k/edit#gid=0"
db_feb <- read_sheet(feb_link) %>% 
  mutate(Date = dmy(Date),
         Age = as.character(Age))

db_feb2 <- db_feb %>% 
  group_by(Region, Date, Age, Measure) %>% 
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% 
  arrange(Region, Date, Measure, Age)
  

# appending all data
# ~~~~~~~~~~~~~~~~~~~
db_all <- 
  bind_rows(db_feb2, db_de_all_sex, db_on_ab)

# imputing unknown ages
# ~~~~~~~~~~~~~~~~~~~~
db_all2 <- imput_age(db_all) %>% 
  mutate(Age = as.integer(Age)) %>% 
  arrange(Region, Date, Measure, Age)


# harmonizing age groups
########################
pps <- unique(db_all2$Region)

db_harm2 <- tibble()

for(pp in pps){
  temp1 <- db_all2 %>% filter(Region == pp)
  dds <- unique(temp1$Date)
  for(dd in seq_along(dds)){
    for(ms in c("Cases", "Deaths")){
    
      chunk <- temp1 %>% 
        filter(Date == dds[dd],
               Measure == ms)
      
      db_harm <- harmonize_age(chunk) %>% 
        mutate(Date = dds[dd],
               Region = pp,
               Measure = ms)
      
      db_harm2 <- bind_rows(db_harm2, db_harm)
      
    }
  }
}

db_harm2 %>% 
  group_by(Region, Measure) %>% 
  summarise(sum(Value))

db_all %>% 
  group_by(Region, Measure) %>% 
  summarise(sum(Value))

age_int <- 5

db_harm3 <- db_harm2 %>% 
  mutate(Age = floor(Age / age_int) * age_int) %>%
  group_by(Date, Region, Measure, Age) %>%
  summarise(Value = sum(Value)) %>% 
  ungroup() 

write_rds(db_harm3, "Output/additional_covid_harmonized.rds")


db_harm3 %>% 
  filter(Region == "Ontario") %>% 
  spread(Measure, Value) %>% 
  mutate(CFR = Deaths / Cases) %>% 
  ggplot()+
  geom_point(aes(Age, CFR, col = Date))


