# functions
source("Code/00_functions.R")

db <- read_rds("Output/covid_data_by_age_sex.rds")

# adding Canada in the Provinces subset

db2 <- db %>% 
  group_by(Region, Code, Date, Type, Wave) %>% 
  mutate(CFR = Deaths / Cases,
         age_dist = Cases / sum(Cases),
         Cases_t = sum(Cases),
         Deaths_t = sum(Deaths),
         CFR_t = Deaths_t / Cases_t) %>% 
  ungroup()

db_wave1 <- db2 %>% 
  filter(Wave == 1)

rgs_w1 <- unique(db_wave1$Region)

db_wave2 <- db2 %>% 
  filter(Wave == 2,
         Region %in% rgs_w1)


r <- "Canada"
regs <- rgs_w1
db_decomp <- NULL
for(r in regs){
  
  db_d1 <- db_wave1 %>% 
    filter(Region == r)
  
  d1 <- unique(db_d1$Date)
  
  db_d2 <- db_wave2 %>% 
    filter(Region == r)
  
  d2 <- unique(db_d2$Date)
  
  t <- unique(db_d2$Type)
  
  db_decomp <- apply_kitagawa(db_d1, db_d2) %>% 
    mutate(Region = r,
           Date1 = d1,
           Date2 = d2, 
           Type = t) %>% 
    select(Region, Date1, Date2, everything()) %>% 
    bind_rows(db_decomp)
  
}

levs_type <- c("Country", "Province", "City")

db_decomp2 <- db_decomp %>% 
  mutate(abs_diff = abs(alpha) + abs(beta),
         percent_dec = round(100 * (1 - CFR2 / CFR1))) %>% 
  gather(alpha, beta, key = "Component", value = Value) %>% 
  mutate(Region2 = paste0(Region, " (", round(CFR1, 3), " - ", round(CFR2, 3), ") (-", percent_dec, " %)"),
         prop = Value / diff,
         prop_abs = abs(Value) / abs_diff,
         Type = factor(Type, levels = levs_type))

tx <- 8
db_decomp2 %>% 
  ggplot()+
  geom_bar(aes(reorder(Region2, -diff), Value, fill = Component, col = Component), stat = "identity", alpha = 0.5)+
  geom_point(aes(reorder(Region2, -diff), diff, shape = "Total CFR difference"), col = "black", size = 2)+
  geom_hline(yintercept = 0, col = "black", size = 0.3, alpha = 0.5)+
  # scale_y_continuous(limits = c(-0.1, 0.005))+
  scale_color_manual(values = c("#43a2ca", "#e34a33"), labels = c("Age structure", "Fatality"))+
  scale_fill_manual(values = c("#43a2ca", "#e34a33"), labels = c("Age structure", "Fatality"))+
  labs(x = "",
       y = "CFR difference")+
  facet_grid(Type ~ ., scales = "free_y", space = "free")+
  guides(shape = guide_legend(title = "", order = 1), 
         fill = guide_legend(direction = "vertical"),
         color = guide_legend(direction = "vertical"))+
  theme_bw()+
  coord_flip()+
  theme(
    legend.position="bottom",
    # legend.position = c(0.3, 0.6),
    legend.title = element_text(size = tx),
    legend.text = element_text(size = tx - 1),
    legend.key.size = unit(0.5,"line"),
    strip.background = element_rect(fill="transparent"),
    strip.text.y = element_text(size = tx + 1),
    axis.text.x = element_text(size = tx),
    axis.text.y = element_text(size = tx),
    axis.title.x = element_text(size = tx + 1),
    axis.title.y = element_text(size = tx + 1)
  )
ggsave("Figures/s2_cfr_diff_between_waves.png", width = 5, height = 5)

# average decrease in CFR between waves (63.1%)
db_decomp2 %>% 
  summarise(mean(percent_dec))

# average contribution of age and fatality components (53.3% and 46.7%)
db_decomp2 %>% 
  group_by(Component) %>% 
  summarise(mean(prop_abs))
