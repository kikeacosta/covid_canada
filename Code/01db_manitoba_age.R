db_d2020 <- read_csv("https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/individual_level/mortality_2020.csv")
db_d2021 <- read_csv("https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/individual_level/mortality_2021.csv")

manit_2020 <- db_d2020 %>% 
  filter(province == "Manitoba") %>% 
  select(age, sex, date_death_report)

manit_2021 <- db_d2021 %>% 
  filter(province == "Manitoba") %>% 
  select(age, sex, date_death_report)

manit <- bind_rows(manit_2020, manit_2021) %>% 
  rename(Date = 3,
         Age = age,
         Sex = sex) %>% 
  mutate(Date = dmy(Date),
         Age = str_sub(Age, 1, 2),
         Age = ifelse(Age == "<1", "0", Age),
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

