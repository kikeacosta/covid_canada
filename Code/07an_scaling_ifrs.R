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
