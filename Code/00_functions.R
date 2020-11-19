Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")


# weekly population interpolation
#################################

interpop <- function(db){
  ys <- db %>% pull(t)
  ps <- db %>% pull(Exposure)
  ws <- seq(1, 574, 1)
  # cubic interpolation
  md2 <- splinefun(x = ys, y = ps, method="fmm",  ties = mean)
  inter_pop <- tibble(t = ws,
                      Exposure = md2(ws))
  return(inter_pop)
}

# smoothing
###########
spline_this <- function(xs, ys, l){
  # smoothing remaining life exp
  md <- smooth.spline(x = xs, y = ys, lambda = l)
  res <- tibble(days = xs,
                sm = predict(md, xs)$y)
  return(res)
}

# Kitagawa decomposition
########################
kita <- function(db, p1, p2, d){
  
  vals1 <- db %>% 
    filter(Date %in% d,
           Region %in% c(p1, p2)) %>% 
    group_by(Region) %>% 
    summarise(CFR_t = max(CFR_t)) %>% 
    ungroup()
  
  vals2 <- bind_cols(vals1[1,2] %>% 
                       rename(CFR1 = CFR_t),
                     vals1[2,2] %>% 
                       rename(CFR2 = CFR_t))
  
  cfr1 <- db %>% 
    filter(Date %in% d,
           Region %in% p1) %>% 
    select(Age, age_dist, CFR) %>% 
    rename(C1 = age_dist, 
           CFR1 = CFR)
  
  cfr2 <- db %>% 
    filter(Date %in% d,
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
              beta = sum(a_C * d_CFR)) %>%
    ungroup() %>% 
    mutate(diff = alpha + beta) %>% 
    select(diff, alpha, beta)
  
  result <- bind_cols(vals2, cfrs_dec) %>% 
    select(CFR1, CFR2, diff, alpha, beta)
  
  return(result)
}

# Kitagawa decomposition v2
###########################

kitagawa <- function(db, p1, p2, s){
  two <- db %>% 
    filter(Region %in% c(p1, p2),
           Sex == s)
  
  vals1 <- two %>% 
    select(Region, CFR_t) %>% 
    unique()
  
  vals1 %>% filter(Region == p1) %>% pull(CFR_t)
  
  vals2 <- tibble(CFR1 = vals1 %>% filter(Region == p1) %>% pull(CFR_t),
                  CFR2 = vals1 %>% filter(Region == p2) %>% pull(CFR_t))
    
  cfr1 <- db %>% 
    filter(Region == p1,
           Sex == s) %>% 
    select(Age, age_dist, CFR) %>% 
    rename(C1 = age_dist, 
           CFR1 = CFR)
  
  cfr2 <- db %>% 
    filter(Region == p2,
           Sex == s) %>% 
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
              beta = sum(a_C * d_CFR)) %>%
    ungroup() %>% 
    mutate(diff = alpha + beta) %>% 
    select(diff, alpha, beta)
  
  result <- bind_cols(vals2, cfrs_dec) %>% 
    select(CFR1, CFR2, diff, alpha, beta)
  
  return(result)
}


# kitagawa function generalizable
##################################

apply_kitagawa <- function(db_d1, db_d2){
  vals <- tibble(CFR1 = db_d1 %>% 
                   pull(CFR_t) %>% 
                   unique(),
                 CFR2 = db_d2 %>% 
                   pull(CFR_t) %>% 
                   unique())
  
  cfr1 <- db_d1 %>% 
    select(Age, age_dist, CFR) %>% 
    rename(C1 = age_dist, 
           CFR1 = CFR)
  
  cfr2 <- db_d2 %>% 
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
              beta = sum(a_C * d_CFR)) %>%
    ungroup() %>% 
    mutate(diff = alpha + beta) %>% 
    select(diff, alpha, beta)
  
  result <- bind_cols(vals, cfrs_dec) %>% 
    select(CFR1, CFR2, diff, alpha, beta)
  
  return(result)
}



















# decomposing CFR differences between populations over time
###########################################################

decomp <- function(p1, p2, d_exc){
  
  # p1 <- "Quebec"
  # p2 <- "Montreal"
  # d_exc <- c(ymd("2020-04-28"), ymd("2020-04-29"))
  # 
  # 
  
  dates2 <- db_can_age2 %>% 
    filter(Region %in% c(p1, p2),
           !(Date %in% d_exc)) %>% 
    select(Region, Date) %>% 
    unique() %>% 
    mutate(n = 1) %>% 
    spread(Region, n) %>% 
    rename(pop1 = 2,
           pop2 = 3) %>% 
    mutate(av = pop1 + pop2) %>% 
    filter(!is.na(av)) %>% 
    pull(Date)
  
  ymd(dates2[12])
  
  db_res <- NULL
  
  for(d in dates2){
    res_t <- kita(db_can_age2, p1, p2, d)
    db_res <- db_res %>% 
      bind_rows(res_t)
  }
  
  bd_res2 <- tibble(Date = dates2, R1 = p1, R2 = p2) %>% 
    bind_cols(db_res)
  
  bd_res3 <- bd_res2 %>% 
    select(Date, alpha, beta) %>% 
    gather(-Date, key = "Component", value = "Value")
  
  diffs <- bd_res2 %>% 
    select(Date, diff)
  
  bd_res4 <- bd_res3 %>% 
    left_join(diffs)
  
  bd_res4 %>% 
    ggplot()+
    geom_hline(yintercept = 0, col = "black", size = 0.3, alpha = 0.5)+
    geom_bar(aes(Date, Value, col = Component), stat = "identity", fill = "transparent")+
    geom_point(aes(Date, diff), col = "black")+
    scale_color_manual(values = c("blue", "red"))+
    labs(title = paste0("Decomposition of CFR difference between ", p1, " and ", p2, " over time"))+
    theme_bw()
  
  ggsave(paste0("Figures/cfr_diff_decomp_over_time_", p1, "_", p2, ".png"))
}

# ungrouping remaining life expectancy
ungr_life_ex <- function(db, ax){
  ages <- db %>% pull(Age)
  exs <- db %>% pull(ex)
  # smoothing remaining life exp
  new_x <- seq(0, 100, ax)
  md2 <- smooth.spline(x = ages, y = exs)
  exs_ungr <- tibble(Age = seq(0, 100, ax),
                     ex = predict(md2, seq(0, 100, ax))$y)
}

# ungrouping IFRs
ungr_ifrs <- function(db, ax){
  ages <- db %>% pull(Age) + ax
  log_ifrs <- log(db %>% pull(IFR))
  # smoothing in single-years of age
  new_x <- seq(0, 100)
  md2 <- smooth.spline(x = ages, y = log_ifrs)
  ifrs_ungr <- tibble(Age = new_x,
                     IFR = exp(predict(md2, new_x)$y))
  return(ifrs_ungr)
}

# fiting IFRs in Canada by province
adj_ifrs_can <- function(db){
  ages <- db %>% pull(Age) + ax
  log_ifrs <- db %>% pull(IFR) %>% log()
  # smoothing in single-years of age
  new_x <- seq(0, 100)
  md1 <- smooth.spline(x = ages, y = log_ifrs)
  pr1 <- exp(predict(md1, new_x)$y)
  ifrs_ungr <- tibble(Region = rg, Age = seq(0, 100, 0.5), IFR = pr1)
  return(ifrs_ungr)
}



##################################
### functions for excess mortality
##################################

### function for bootstrapping 
##############################

boot_pi <- function(model, odata, pdata, n, p) {
  lp <- (1 - p) / 2
  up <- 1 - lp
  set.seed(2020)
  seeds <- round(runif(n, 1, 1000), 0)
  boot_y <- foreach(i = 1:n, .combine = rbind) %dopar% {
    set.seed(seeds[i])
    bdata <- odata[sample(seq(nrow(odata)), size = nrow(odata), replace = TRUE), ]
    bpred <- predict(update(model, data = bdata), type = "response", newdata = pdata)
    rpois(length(bpred), lambda = bpred)
  }
  boot_ci <- t(apply(boot_y, 2, quantile, c(lp, up)))
  return(data.frame(Baseline = predict(model, newdata = pdata, type = "response"), 
                    lp = boot_ci[, 1], 
                    up = boot_ci[, 2]))
}



### function for fitting model for each Region, sex, and age
#############################################################

# ct <- "Spain"
# sx <- "b"
# ag <- 80
# ymin <- 2014
# db2 <- temp2

fit_baseline <- function(db2) {
  
  skip_to_next <- F
  
  # # data to include in the model 
  db_bline <- db2 %>%
    filter(include == 1)
  
  # model fitting evaluation
  ##########################
  # evaluate the seasonal parameter with AIC
  train_base <- db_bline %>% 
    filter(row_number() <= floor(nrow(db_bline)/2))
  
  valid_base <- db_bline %>% 
    filter(row_number() > floor(nrow(db_bline)/2))
  
  no_sea1 = gnm(Deaths ~ ns(t, 3) + offset(log(Exposure)), 
                data = train_base, 
                family = poisson(link="log"))
  
  no_sea2 = gnm(Deaths ~ ns(t, 3) + offset(log(Exposure)), 
                data = valid_base, 
                contrain = "*", 
                contrainTo = coef(reg1),
                family=poisson(link="log"))
  
  sea1 = gnm(Deaths ~ ns(t, 3) + sn52 + cs52 + offset(log(Exposure)), 
             data = train_base, 
             family = poisson(link="log"))
  
  sea2 = gnm(Deaths ~ ns(t, 3) + sn52 + cs52 + offset(log(Exposure)), 
             data = valid_base, 
             contrain = "*", 
             contrainTo = coef(reg1),
             family=poisson(link="log"))
  
  if (no_sea2$aic - sea2$aic > 6) {
    # evaluating for overdispersion adjustment for seasonal model
    # Poisson model
    base_po <- glm(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(Exposure)), 
                   family = poisson, data = db_bline)
    
    # negative binomial to account for overdispersion
    base_nb <- try(MASS::glm.nb(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(Exposure)), 
                                data = db_bline), silent = T)
    ov_sign <- try(base_nb$theta / base_nb$SE.theta > 1.96, silent = T)
    if (class(base_nb)[1] == "try-error" | is.na(ov_sign)) {
      base_nb$aic <- base_po$aic
      base_nb$converged <- F
      ov_sign <- F
    }
    # compare AIC between Poisson and Negative binomial and fit the best model
    if ((base_po$aic - base_nb$aic >= 6) & (ov_sign) & (base_nb$converged) & class(base_nb)[1] != "try-error") {
      base <- MASS::glm.nb(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(Exposure)), 
                           data = db_bline)
    } else {
      base <- glm(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(Exposure)), 
                  family = poisson, data = db_bline)
    }
  } else {
    # evaluating for overdispersion adjustment for non-seasonal model
    # Poisson model
    base_po <- glm(Deaths ~ splines::ns(t, 3) + offset(log(Exposure)), 
                   family = poisson, data = db_bline)
    
    # Negative binomial to account for overdispersion
    base_nb <- try(MASS::glm.nb(Deaths ~ splines::ns(t, 3) + offset(log(Exposure)), 
                                data = db_bline), silent = T)
    ov_sign <- try(base_nb$theta / base_nb$SE.theta > 1.96, silent = T)
    if (class(base_nb)[1] == "try-error" | is.na(ov_sign)) {
      base_nb$aic <- base_po$aic
      base_nb$converged <- F
      ov_sign <- F
    }
    # compare AIC between Poisson and Negative binomial and fit the best model
    if ((base_po$aic - base_nb$aic >= 6) & (ov_sign) & (base_nb$converged) & class(base_nb)[3] != "try-error") {
      base <- MASS::glm.nb(Deaths ~ splines::ns(t, 3) + offset(log(Exposure)), 
                           data = db_bline)
    } else {
      base <- glm(Deaths ~ splines::ns(t, 3) + offset(log(Exposure)), 
                  family = poisson, data = db_bline)
    }
  }
  
  # predicting values and 95% confidence intervals
  ################################################
  
  # bootstrapping
  tryCatch({
    db3 <- cbind(db2, 
                 boot_pi(base, db_bline, db2, 2000, 0.95))
  }, error=function(e){ skip_to_next <<- TRUE})
  
  # if(skip_to_next) { next }
  
  return(db3)
}
