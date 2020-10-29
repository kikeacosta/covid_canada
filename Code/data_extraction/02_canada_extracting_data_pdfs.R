rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)
library(pdftools)

extract_cases1 <- function(j){
  a1 <- str_split(cases[pos_c + j], "\\s{2,}")[[1]][2] %>% 
    str_replace_all(c(" " = "", "," = ""))
  v1 <- str_split(cases[pos_c + j], "\\s{2,}")[[1]][3] %>% 
    str_replace_all(c(" " = "", "," = ""))
  av <- tibble(Age = a1, Value = v1, Measure = "Cases", Sex = "b")
  return(av)
}
extract_death1 <- function(j){
  a1 <- str_split(deaths[pos_d + j], "\\s{2,}")[[1]][2] %>% 
    str_replace_all(c(" " = "", "," = ""))
  v1 <- str_split(deaths[pos_d + j], "\\s{2,}")[[1]][7] %>% 
    str_replace_all(c(" " = "", "," = ""))
  av <- tibble(Age = a1, Value = v1, Measure = "Deaths", Sex = "b")
  return(av)
}
extract_cases2 <- function(j){
  a1 <- str_split(cases[pos_c + j], "\\s{1,}")[[1]][2] %>%
    str_replace_all(c(" " = "", "," = ""))
  v1 <- str_split(cases[pos_c + j], "\\s{1,}")[[1]][5] %>%
    str_replace_all(c(" " = "", "," = ""))
  av <- tibble(Age = a1, Value = v1, Measure = "Cases", Sex = "b")
  return(av)
}
extract_death2 <- function(j){
  a1 <- str_split(deaths[pos_d + j], "\\s{2,}")[[1]][1] %>%
    str_replace_all(c(" " = "", "," = ""))
  v1 <- str_split(deaths[pos_d + j], "\\s{2,}")[[1]][6] %>%
    str_replace_all(c(" " = "", "," = ""))
  av <- tibble(Age = a1, Value = v1, Measure = "Deaths", Sex = "b")
  return(av)
}

# setwd("U:/nextcloud/Projects/COVID_19/COVerAge-DB/canada/statcan_pdf/")
files <- list.files(path = "Data/pdfs_canada", pattern = "pdf$")
i <- 5
all1 <- NULL
for(i in 1:50){
    
  l <- case_when(i <= 11 ~ 6,
                 i >= 12 ~ 7)
  lc <- 3
  
  file_loc <- paste0("Data/pdfs_canada/", files[i])
  # reading pdf
  txt <- pdf_text(file_loc)
  totals <- capture.output(cat(txt[grep("reported in Canada by location|Areas in Canada with cases", txt)]))
  cases <- capture.output(cat(txt[grep("Demographic characteristics|Age by sex distribution", txt)]))
  deaths <- capture.output(cat(txt[grep("Summary of severe cases|Deceased|Case Severity|Age distribution of ", txt)]))
  
  # add spaces for some columns in deaths to solve problem of single space in some dates
  for(cr in c("Female", "Male", "20-39", "40-59", "60-79", "80\\+")){
    deaths[grep(cr, deaths)] <- deaths[grep(cr, deaths)] %>%
      str_replace(cr, paste0(cr, "   "))
    cases[grep(cr, cases)] <- cases[grep(cr, cases)] %>% 
      str_replace(cr, paste0(cr, "   "))
  } 
  
  # finding lines of the tables
  pos_c <- grep("Age groups", cases)
  pos_d <- grep("Age groups", deaths)
  if(i == 18 | i == 19) pos_ct <- grep("Total", totals)[3]
  if(i != 18 & i != 19) pos_ct <- grep("Total", totals)[2]
  
  # totals cases and deaths
  cases2 <- tibble(Age = "TOT", 
                   Value = str_split(totals[pos_ct], "\\s{2,}")[[1]][3] %>% 
                     str_replace_all(c("\r" = "", " " = "")),
                   Measure = "Cases",
                   Sex = "b")
  
  death2 <- tibble(Age = "TOT", 
                   Value = str_split(totals[pos_ct], "\\s{2,}")[[1]][l] %>% 
                     str_replace_all(c("\r" = "", " " = "")),
                   Measure = "Deaths",
                   Sex = "b")

  # cases and deaths by age
  for(k in 1:5){
    cas_t <- extract_cases1(k)
    if(i <= 34 | i>= 39) dea_t <- extract_death1(k)
    cases2 <- bind_rows(cases2, cas_t)
    death2 <- bind_rows(death2, dea_t)
  }
  
  # cases and deaths by sex
  cas_sex <- bind_rows(extract_cases1(7), extract_cases1(8)) %>% 
    mutate(Sex = case_when(Age == "Male" ~ "m",
                           Age == "Female" ~ "f"),
           Age = "TOT")
  
  if(i <= 34 | i>= 39) dea_sex <- bind_rows(extract_death1(8), extract_death1(9)) %>% 
    mutate(Sex = case_when(Age == "Male" ~ "m",
                           Age == "Female" ~ "f"),
           Age = "TOT")
  
  # binding all data
  all_temp <- bind_rows(cases2, cas_sex, death2, dea_sex) %>% 
    mutate(date = ymd(str_sub(files[i], 1, 10)),
           Sex = case_when(Sex == "Female" ~ "f",
                           Sex == "Male" ~ "m",
                           TRUE ~ Sex))

  all1 <- all1 %>% 
    bind_rows(all_temp)
}
    
##################################
# after new format in June 9
##################################

extract_cases2 <- function(j){
  a1 <- str_split(cases[pos_c + j], "\\s{1,}")[[1]][2] %>%
    str_replace_all(" ", "")
  v1 <- str_split(cases[pos_c + j], "\\s{1,}")[[1]][5] %>%
    str_replace_all(c(" " = "", "," = "")) 
  v2 <- str_split(cases[pos_c + j], "\\s{1,}")[[1]][7] %>%
    str_replace_all(c(" " = "", "," = ""))
  av <- tibble(Age = a1, Value = c(v1, v2), Sex = c("m", "f"), Measure = "Cases")
  return(av)
}

extract_death2 <- function(j){
  a1 <- str_split(deaths[pos_d + j], "\\s{2,}")[[1]][1] %>%
    str_replace_all(" ", "")
  v1 <- str_split(deaths[pos_d + j], "\\s{2,}")[[1]][6] %>%
    str_replace_all(c(" " = "", "," = ""))
  av <- tibble(Age = a1, Value = v1, Sex = "b", Measure = "Deaths")
  return(av)
}

extract_death3 <- function(j){
  a1 <- str_split(deaths[pos_c + j], "\\s{1,}")[[1]][1] %>%
    str_replace_all(" ", "")
  v1 <- str_split(deaths[pos_c + j], "\\s{1,}")[[1]][4] %>%
    str_replace_all(c(" " = "", "," = "")) 
  v2 <- str_split(deaths[pos_c + j], "\\s{1,}")[[1]][6] %>%
    str_replace_all(c(" " = "", "," = ""))
  av <- tibble(Age = a1, Value = c(v1, v2), Sex = c("m", "f"), Measure = "Deaths")
  return(av)
}
extract_death4 <- function(j){
  a1 <- str_split(deaths[pos_c + j], "\\s{1,}")[[1]][2] %>%
    str_replace_all(" ", "")
  v1 <- str_split(deaths[pos_c + j], "\\s{1,}")[[1]][5] %>%
    str_replace_all(c(" " = "", "," = "")) 
  v2 <- str_split(deaths[pos_c + j], "\\s{1,}")[[1]][7] %>%
    str_replace_all(c(" " = "", "," = ""))
  av <- tibble(Age = a1, Value = c(v1, v2), Sex = c("m", "f"), Measure = "Deaths")
  return(av)
}
# str_split(deaths[pos_c + 1], "\\s{1,}")[[1]][5]
i <- 100
all2 <- NULL
for(i in 51:163){
  
  if(i < 94) l <- 6
  if(i >= 94) l <- 8
  
  lc <- 4
  
  # reading pdf
  file_loc <- paste0("Data/pdfs_canada/", files[i])
  txt <- pdf_text(file_loc)
  totals <- capture.output(cat(txt[grep("Areas in Canada with cases", txt)]))
  if(i < 65) cases <- capture.output(cat(txt[grep("Age by sex distribution", txt)]))
  if(i >= 65) cases <- capture.output(cat(txt[grep("Exposure setting", txt)]))
  if(i < 79) deaths <- capture.output(cat(txt[grep("Age distribution of ", txt)]))
  if(i >= 79 & i <= 82) deaths <- capture.output(cat(txt[grep("COVID-19 cases deceased", txt)[2]+1]))
  if((i >= 83 & i <= 93) | (i >= 100)) deaths <- capture.output(cat(txt[grep("COVID-19 cases deceased", txt)[2]]))
  if(i >= 94 & i <= 99) deaths <- capture.output(cat(txt[grep("COVID-19 cases deceased", txt)[2]]))
  
  # add spaces for some columns in deaths to solve problem of single space in some dates
  for(cr in c("Female", "Male", "20-39", "40-59", "60-79", "80\\+")){
    deaths[grep(cr, deaths)] <- deaths[grep(cr, deaths)] %>%
      str_replace(cr, paste0(cr, "   "))
    cases[grep(cr, cases)] <- cases[grep(cr, cases)] %>% 
      str_replace(cr, paste0(cr, "   "))
  } 

  # finding lines of the tables
  pos_c <- grep("proportion", cases)
  pos_d <- grep("years", deaths)
  pos_ct <- grep("Canada", totals)[2]

    # totals cases and deaths
  cases2 <- tibble(Age = "TOT", 
                   Value = str_split(totals[pos_ct], "\\s{1,}")[[1]][3] %>% 
                     str_replace_all(c("\r" = "", " " = "", "," = "")) ,
                   Measure = "Cases",
                   Sex = "b")
  
  death2 <- tibble(Age = "TOT", 
                   Value = str_split(totals[pos_ct], "\\s{1,}")[[1]][l] %>% 
                     str_replace_all(c("\r" = "", " " = "", "," = "")),
                   Measure = "Deaths", 
                   Sex = "b")
 
  # cases by age and sex
  for(k in 1:8){
    cas_t <- extract_cases2(k)
    cases2 <- bind_rows(cases2, cas_t)
  }
  # deaths by age and sex
  if(i < 79){
    for(k in 1:5){
      dea_t <- extract_death2(k)
      death2 <- bind_rows(death2, dea_t)
    }
    death2 <- bind_rows(extract_death2(12), extract_death2(13)) %>% 
      mutate(Sex = case_when(Age == "Male" ~ "m",
                             Age == "Female" ~ "f"),
             Age = "TOT") %>% 
      bind_rows(death2)
      
  }else if(i >= 79 & i <= 82){
    for(k in 1:8){
      dea_t <- extract_death3(k)
      death2 <- bind_rows(death2, dea_t)
    }
    
  }else if(i >= 83){
    for(k in 1:8){
      dea_t <- extract_death4(k)
      death2 <- bind_rows(death2, dea_t)
    }
  }

  # binding all data
  all_temp <- bind_rows(cases2,
                        death2) %>% 
    mutate(date = ymd(str_sub(files[i], 1, 10)),
           Sex = case_when(Sex == "Female" ~ "f",
                           Sex == "Male" ~ "m",
                           TRUE ~ Sex))
  
  all2 <- all2 %>% 
    bind_rows(all_temp)
}

dates_problems <- c("2020-05-24", "2020-05-25", "2020-05-26", "2020-05-27",
                    "2020-07-22", "2020-07-23", "2020-07-24", "2020-07-25", 
                    "2020-07-26", "2020-07-27") 

all <- bind_rows(all1, all2) %>% 
  separate(Age, c("Age", "trash"), sep = "-") %>% 
  mutate(Age = case_when(Age == "=19" ~ "0",
                         Age == "80+" ~ "80",
                         TRUE ~ Age),
         Value = as.numeric(Value)) %>% 
  select(-trash) %>% 
  drop_na() %>% 
  filter(!(date %in% ymd(dates_problems)))

unique(all$date)

write_rds(all, "Data/201029_covid_canada.rds")

db_final <- all %>% 
  mutate(Country = "Canada",
         Region = "All",
         Date = paste(sprintf("%02d", day(date)),
                      sprintf("%02d", month(date)),
                      year(date), sep = "."),
         Code = paste0("CA", Date),
         Metric = "Count",
         AgeInt = case_when(Age == "0" ~ "20",
                            Age == "20" & Measure == "Cases" & date <= "2020-06-08" ~ "20",
                            Age == "40" & Measure == "Cases" & date <= "2020-06-08" ~ "20",
                            Age == "60" & Measure == "Cases" & date <= "2020-06-08" ~ "20",
                            Age == "40" & Measure == "Cases" & date <= "2020-06-08" ~ "20",
                            Age == "20" & Measure == "Deaths" & date <= "2020-07-06" ~ "20",
                            Age == "40" & Measure == "Deaths" & date <= "2020-07-06" ~ "20",
                            Age == "60" & Measure == "Deaths" & date <= "2020-07-06" ~ "20",
                            Age == "40" & Measure == "Deaths" & date <= "2020-07-06" ~ "20",
                            Age == "80" ~ "25",
                            Age == "TOT" ~ "",
                            TRUE ~ "10")) %>% 
  arrange(date, Measure, Sex, suppressWarnings(as.integer(Age))) %>% 
  select(Country, Region, Code,  Date, Sex, Age, AgeInt, Metric, Measure, Value)


email <- "kikepaila@gmail.com"
library(googlesheets4)
library(googledrive)
drive_auth(email = email)
gs4_auth(email = email)

write_sheet(db_final,
            ss = 'https://docs.google.com/spreadsheets/d/1awgAawvZLUFHO0KzJ_ntMtHR68UcuJoMpc9kVLUhJd8/edit#gid=0',
            sheet = "database")


# setwd("U:/gits/covid_canada")
# write_rds(db_final, "Data/201029_covid_canada.rds")


# some visual verifications

all %>% 
  filter(Sex == "b",
         Age == "TOT",
         Measure == "Cases") %>% 
  ggplot()+
  geom_line(aes(date, Value))

all %>% 
  filter(Sex == "b",
         Age == "TOT",
         Measure == "Deaths") %>% 
  ggplot()+
  geom_line(aes(date, Value))

all %>% 
  filter(Sex == "f",
         Age == "TOT",
         Measure == "Cases") %>% 
  ggplot()+
  geom_line(aes(date, Value))

all %>% 
  filter(Sex == "f",
         Age == "TOT",
         Measure == "Deaths") %>% 
  ggplot()+
  geom_line(aes(date, Value))


all %>% 
  filter(Sex == "m",
         Measure == "Deaths") %>% 
  ggplot()+
  geom_line(aes(date, Value, col = Age))

