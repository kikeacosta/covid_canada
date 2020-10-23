rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)
library(ggplot2)

# functions
kita <- function(p1, p2, d){
  
  vals1 <- db_can_age2 %>% 
    filter(date_f %in% d,
           Region %in% c(p1, p2)) %>% 
    group_by(Region) %>% 
    summarise(CFR_t = max(CFR_t)) %>% 
    ungroup()
  
  vals2 <- bind_cols(vals1[1,2] %>% 
                       rename(CFR1 = CFR_t),
                     vals1[2,2] %>% 
                       rename(CFR2 = CFR_t))
  
  cfr1 <- db_can_age2 %>% 
    filter(date_f %in% d,
           Region %in% p1) %>% 
    select(Age, age_dist, CFR) %>% 
    rename(C1 = age_dist, 
           CFR1 = CFR)
  
  cfr2 <- db_can_age2 %>% 
    filter(date_f %in% d,
           Region %in% p2) %>% 
    select(Age, age_dist, CFR) %>% 
    rename(C2 = age_dist, 
           CFR2 = CFR)
  
  cfrs <- left_join(cfr1, cfr2) %>% 
    mutate(d_C = C2 - C1,
           d_CFR = CFR2 - CFR1,
           a_C = (C2 + C1) * 0.5,
           a_CFR = (CFR2 + CFR1) * 0.5)
  
  # decomposed into age and fatality component
  cfrs_dec <- cfrs %>% 
    group_by() %>% 
    summarise(alpha = sum(d_C * a_CFR),
              betha = sum(a_C * d_CFR)) %>%
    ungroup() %>% 
    mutate(diff = alpha + betha) %>% 
    select(diff, alpha, betha)
  
  result <- bind_cols(vals2, cfrs_dec) %>% 
    select(CFR1, CFR2, diff, alpha, betha)
  
  return(result)
}
decomp <- function(p1, p2, d_exc){
  
  dates2 <- db_can_age2 %>% 
    filter(Region %in% c(p1, p2),
           !(date_f %in% d_exc)) %>% 
    select(Region, date_f) %>% 
    unique() %>% 
    mutate(n = 1) %>% 
    spread(Region, n) %>% 
    rename(pop1 = 2,
           pop2 = 3) %>% 
    mutate(av = pop1 + pop2) %>% 
    filter(!is.na(av)) %>% 
    pull(date_f)
  ymd(dates2[12])
  
  db_res <- NULL
  
  for(d in dates2){
    res_t <- kita(p1, p2, d)
    db_res <- db_res %>% 
      bind_rows(res_t)
  }
  
  bd_res2 <- tibble(date_f = dates2, R1 = p1, R2 = p2) %>% 
    bind_cols(db_res)
  
  bd_res3 <- bd_res2 %>% 
    select(date_f, alpha, betha) %>% 
    gather(-date_f, key = "Component", value = "Value")
  
  diffs <- bd_res2 %>% 
    select(date_f, diff)
  
  bd_res4 <- bd_res3 %>% 
    left_join(diffs)
  
  bd_res4 %>% 
    ggplot()+
    geom_hline(yintercept = 0, col = "black", size = 0.3, alpha = 0.5)+
    geom_bar(aes(date_f, Value, col = Component), stat = "identity", fill = "transparent")+
    geom_point(aes(date_f, diff), col = "black")+
    scale_color_manual(values = c("blue", "red"))+
    labs(title = paste0("Decomposition of CFR difference between ", p1, " and ", p2, " over time"))+
    theme_bw()
  
  ggsave(paste0("Figures/cfr_diff_decomp_over_time_", p1, "_", p2, ".png"))
  
}

# reading Canada data
db_can <- read_rds("Data_output/canada_cases_deaths.rds")

db_can_age <- read_rds("Data_output/cfr_by_age.rds")

db_can2 <- db_can %>% 
  select(Region, date_f, Cases, Deaths, CFR) %>% 
  rename(Cases_t = Cases,
         Deaths_t = Deaths,
         CFR_t = CFR)

db_can_age2 <- db_can_age %>% 
  select(Region, date_f, Age, Cases, Deaths, CFR) %>% 
  arrange(Region, date_f, Age) %>% 
  group_by(Region, date_f) %>%  
  mutate(age_dist = Cases / sum(Cases)) %>% 
  ungroup() %>% 
  left_join(db_can2) %>% 
  filter(!is.na(CFR_t),
         CFR_t > 0) %>% 
  mutate(CFR = replace_na(CFR, 0),
         Region = ifelse(Region == 'British Columbia', "BC", Region))

dates_all <- db_can_age2 %>% 
  filter(Region != "All") %>% 
  select(Region, date_f) %>% 
  unique() %>% 
  mutate(n = 1) %>% 
  spread(Region, n) %>% 
  mutate(av = Alberta + BC + Montreal + Ontario + Quebec) %>% 
  filter(av == 5)

p1 <- "Quebec"
p2 <- "Montreal"
d_exc <- c(ymd("2020-04-23"), ymd("2020-04-28"), ymd("2020-04-29"))
d_exc <- c(ymd("2020-04-23"))
d_exc <- c(ymd("2020-04-28"), ymd("2020-04-29"))
# d_exc <- c()

decomp(p1, p2, d_exc)
