source("Code/00_functions.R")

##############
# reading data
##############
# IFRs by age from Spain
# ifrs_es <- read_csv("Data/spain_sex_age_ifr.csv") 

ifrs_es <- read_csv("https://raw.githubusercontent.com/kikeacosta/ifr_age_spain/master/Output/spain_sex_age_ifr_single_years.csv")
# equivalent Canadian thanatological ages compared to Spain
than_es <- read_rds("Output/thanat_age_canada_spain.rds")

########################
# scaling IFRs to Canada
########################

# From Spain
############

# interpolating IFRs in single-years age
ss <- unique(ifrs_es$Sex)
es <- unique(ifrs_es$Estimate)


# visualizing the splines
ifrs_es %>% 
  filter(Sex == "t") %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Estimate))+
  scale_y_log10()+
  labs(title = "Age-specific IFR, from Spain estimates")+
  theme_bw()
ggsave("Figures/ifrs_spain.png")


# attributing IFR from Spain to Canada
ifrs_ca_es <- than_es %>% 
  left_join(ifrs_es %>% 
              rename(Age_es = Age)) %>% 
  mutate(Source = "Spain") %>% 
  select(-Age_es)

ifrs_ca_adj <- NULL
rs <- unique(ifrs_ca_es$Region)
for(r in rs){
  for(s in ss){
    for(e in es){
      temp1 <- ifrs_ca_es %>% 
        filter(Region == r,
               Sex == s,
               Estimate == e)
      ifrs_ca_adj <- ifrs_ca_adj %>% 
      bind_rows(ungr_ifrs(temp1, 0.5) %>% 
                  mutate(Region = r,
                         Sex = s,
                         Estimate = e))  
    }
  }
}

ifrs_ca_adj %>%
  filter(Region == "Manitoba",
         Sex == "t") %>%
  ggplot()+
  geom_line(aes(Age, IFR, col = Estimate))+
  scale_y_log10()+
  labs(title = "Age-specific IFR, from Spain estimates")+
  theme_bw()

write_rds(ifrs_ca_adj, "Output/ifr_age_sex_canada.rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# sensitivity analysis with Verity estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ifrs_ch <- read_csv("Data/IFR_age_Verity_et_al.csv")
than_ch <- read_rds("Output/thanat_age_canada_china.rds")


ifrs_ch2 <- ifrs_ch %>% 
  rename(Central = IFR,
         Lower = IFR_l,
         Upper = IFR_u) %>% 
  gather(-Age, key = Estimate, value = IFR)

ax <- 5

db_ungr_ifrs <- NULL
es <- unique(ifrs_ch2$Estimate)
for(e in es){
  temp1 <- ifrs_ch2 %>% 
    filter(Estimate == e) 
  temp2 <- ungr_ifrs(temp1, ax) %>% 
    mutate(Estimate = e)
  db_ungr_ifrs <- db_ungr_ifrs %>% 
    bind_rows(temp2)
}


db_ungr_ifrs2 <- db_ungr_ifrs %>% 
  mutate(Type = "Ungrouped")

ifrs_ch3 <- ifrs_ch2 %>% 
  mutate(Type = "Grouped",
         Age = Age + 5) %>% 
  bind_rows(db_ungr_ifrs2)

e <- "Lower"
ifrs_ch3 %>% 
  filter(Estimate == e) %>% 
  ggplot()+
  geom_point(aes(Age, IFR, col = Type), alpha = 0.5)+
  scale_y_log10()

ifrs_ch3 %>% 
  filter(Type == "Ungrouped") %>% 
  spread(Estimate, IFR) %>% 
  ggplot()+
  geom_line(aes(Age, Central))+
  geom_ribbon(aes(Age, ymin = Lower, ymax = Upper), alpha = 0.3)+
  scale_x_continuous(breaks = seq(0, 100, 10))+
  scale_y_log10()+
  scale_color_manual(values = c("red", "black", "blue"))+
  scale_fill_manual(values = c("red", "black", "blue"))+
  theme_bw()+
  labs(y = "IFR")

write_csv(db_ungr_ifrs2,  "Output/verity_age_ifr_single_years.csv")


# attributing IFR from China to Canada
ifrs_ca_ch <- than_ch %>% 
  left_join(db_ungr_ifrs2 %>% 
              rename(Age_ch = Age)) %>% 
  mutate(Source = "China") %>% 
  select(-Age_ch)

ifrs_ca_ch_adj <- NULL
rs <- unique(ifrs_ca_ch$Region)
for(r in rs){
    for(e in es){
      temp1 <- ifrs_ca_ch %>% 
        filter(Region == r,
               Estimate == e)
      ifrs_ca_ch_adj <- ifrs_ca_ch_adj %>% 
        bind_rows(ungr_ifrs(temp1, 0.5) %>% 
                    mutate(Region = r,
                           Estimate = e))  
    }
}

ifrs_ca_ch_adj %>%
  filter(Region == "Manitoba") %>%
  ggplot()+
  geom_line(aes(Age, IFR, col = Estimate))+
  scale_y_log10()+
  labs(title = "Age-specific IFR, from China estimates")+
  theme_bw()

write_rds(ifrs_ca_ch_adj, "Output/ifr_age_sex_canada_verity.rds")

