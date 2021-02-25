source("Code/00_functions.R")

##############
# reading data
##############
# IFRs by age from Spain
ifrs_es <- read_csv("Data/spain_sex_age_ifr.csv") 

ifrs_es <- read_csv("https://raw.githubusercontent.com/kikeacosta/ifr_age_spain/master/Output/spain_sex_age_ifr_single_years.csv")
# equivalent Canadian thanatological ages compared to Spain
than_es <- read_rds("Output/thanat_age_canada_spain.rds")

########################
# scaling IFRs to Canada
########################

# From Spain
############

# interpolating IFRs in single-years age

ifrs_es2 <- ifrs_es %>% 
  gather(IFR, IFR_l, IFR_u, key = "Est", value = "IFR") %>% 
  mutate(Est = case_when(Est == "IFR" ~ "Central",
                         Est == "IFR_l" ~ "lower",
                         Est == "IFR_u" ~ "upper"))


ss <- unique(ifrs_es2$Sex)
es <- unique(ifrs_es2$Est)

ungr_ifrs_es <- NULL
for(s in ss){
  for(e in es){
    temp1 <- ifrs_es2 %>% 
      filter(Sex == s,
             Est == e) 
    temp2 <- ungr_ifrs(temp1, 2.5) %>% 
      mutate(Sex = s,
             Est = e)
    ungr_ifrs_es <- ungr_ifrs_es %>% 
      bind_rows(temp2)
  }
}


# visualizing the splines
ungr_ifrs_es %>% 
  filter(Sex == "b") %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Est))+
  scale_y_log10()

# attributing IFR from Spain to Canada
ifrs_ca_es <- than_es %>% 
  left_join(ungr_ifrs_es %>% 
              rename(Age_es = Age)) %>% 
  mutate(Source = "Spain") %>% 
  select(-Age_es)

# binding all IFRs for Canada
# ifrs_ca <- ifrs_ca_es %>% bind_rows(ifrs_ca_ch)

ifrs_ca_adj <- NULL
rs <- unique(ifrs_ca_es$Region)
for(r in rs){
  for(s in ss){
    for(e in es){
      temp1 <- ifrs_ca_es %>% 
        filter(Region == r,
               Sex == s,
               Est == e)
      ifrs_ca_adj <- ifrs_ca_adj %>% 
      bind_rows(ungr_ifrs(temp1, 0.5) %>% 
                  mutate(Region = r,
                         Sex = s,
                         Est = e))  
    }
  }
}

ifrs_ca_adj %>%
  filter(Region == "All",
         Sex == "b") %>%
  ggplot()+
  geom_line(aes(Age, IFR, col = Est))+
  scale_y_log10()+
  labs(title = "Age-specific IFR, from Spain estimates")+
  theme_bw()

ggsave("Figures/ifrs_spain.png")


# ifrs_ca_adj_vis <- ifrs_ca_adj %>% 
#   mutate(Source = case_when(Sex == "b" ~ "b_Verity et al.", 
#                             Sex == "f" ~ "f_Spain seroprev.",
#                             Sex == "m" ~ "m_Spain seroprev."))
# 
# ifrs_ca_adj_vis %>% 
#   filter(Source == "b_Verity et al.") %>% 
#   ggplot()+
#   geom_line(aes(Age, IFR, col = Region))+
#   scale_y_log10()

# ifrs_ca_adj_vis %>% 
#   filter(Source == "m_Spain seroprev.") %>% 
#   ggplot()+
#   geom_line(aes(Age, IFR, col = Region))+
#   scale_y_log10()
# 
# ifrs_ca_adj_vis %>% 
#   filter(Source == "f_Spain seroprev.") %>% 
#   ggplot()+
#   geom_line(aes(Age, IFR, col = Region))+
#   scale_y_log10()
# 
# ifrs_ca_adj_vis %>% 
#   filter(Region == "All") %>% 
#   ggplot()+
#   geom_line(aes(Age, IFR, col = Source))+
#   scale_y_log10()+
#   labs(title = "Age-specific IFR, Verity et al. vs Spain estimates")+
#   theme_bw()
# 
# ggsave("Figures/ifrs_spain_vs_verity.png")

# ggsave("Figures/age-spe_ifr_canada_china.png")
# 
# ifrs_ca_adj %>% 
#   bind_rows(ifrs_ungr %>% 
#               rename(Age = Age_ch) %>% 
#               mutate(Region = "China")) %>% 
#   filter(Age >= 60) %>% 
#   ggplot()+
#   geom_line(aes(Age, IFR, col = Region))+
#   scale_y_log10()+
#   scale_x_continuous(breaks = seq(0, 90, 10))+
#   labs(title = "Age-specific IFRs by region ages >60")+
#   theme_bw()
# 
# ggsave("Figures/age-spe_ifr_60plus.png")
# 
# ifr_10age_can <- ifrs_ca_adj %>% 
#   filter(Age %in% seq(5, 85, 10)) %>% 
#   mutate(Age = Age -5)

write_rds(ifrs_ca_adj, "Output/ifr_age_sex_canada.rds")
