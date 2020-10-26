Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")


# Kitagawa decomposition
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

# decomposing CFR differences between populations over time
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

# ungrouping remaining life expectancy
ungr_life_ex <- function(rg = "China", int = 1){
  
  db_exs2 <- db_exs %>% 
    filter(Region == rg)
  
  ages <- db_exs2 %>% pull(Age)
  exs <- db_exs2 %>% pull(ex)
  # smoothing remaining life exp
  
  new_x <- seq(0, 100, int)
  md2 <- smooth.spline(x = ages, y = exs)
  
  exs_ungr <- tibble(Region = rg,
                     Age = seq(0, 100, int),
                     ex = predict(md2, seq(0, 100, int))$y)
  
}

# fiting IFRs in Canada by province
rg <- "Quebec"
adj_ifrs_can <- function(rg){
  temp1 <- ifrs_ca %>% 
    filter(Region == rg)
  ages <- temp1 %>% pull(Age)
  log_ifrs <- temp1 %>% pull(IFR) %>% log()
  md1 <- smooth.spline(x = ages, y = log_ifrs)
  pr1 <- exp(predict(md1, x = seq(0, 89, 0.5))$y)
  ifrs_ungr <- tibble(Region = rg, Age = seq(0, 89, 0.5), IFR = pr1)
  return(ifrs_ungr)
}
