Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

# install pacman to streamline further package installation
if(!require("pacman", character.only = TRUE)) {
  install.packages("pacman", dep = TRUE)
  if (!require("pacman", character.only = TRUE))
    stop("Package pacman not found")
}

library(pacman)

# Required CRAN packages
pkgs <- c("tidyverse",
          "here",
          "lubridate",
          "readxl",
          "vroom",
          "ungroup",
          "HMDHFDplus",
          "parallel",
          "parallelsugar",
          "ISOweek",
          "scales",
          "zoo",
          "googlesheets4",
          "googledrive",
          "osfr")


# required packages for baseline estimation
pkgs_bsl <- c("stats", 
              "splines",
              "MASS",
              "gnm",
              'doParallel', 
              'foreach')


# Install required CRAN packages if not available yet
if(!sum(!p_isinstalled(c(pkgs, pkgs_bsl)))==0) {
  p_install(
    package = pkgs[!p_isinstalled(pkgs)], 
    character.only = TRUE
  )
}

# loading basic packages
p_load(pkgs, character.only = TRUE)


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


# Adjustments for unknown values
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
imput_age <- function(db){
  db_unk_age <- db %>% 
    filter(Age == "UNK" | Age == "unk") %>% 
    rename(unk = Value) %>% 
    select(-Age)
  db_de_age_all <- db %>% 
    filter(Age != "UNK" & Age != "unk") %>% 
    group_by(Region, Date, Measure) %>% 
    mutate(dist = Value / sum(Value)) %>% 
    ungroup() %>% 
    left_join(db_unk_age) %>% 
    replace_na(list(unk = 0)) %>% 
    mutate(Value = Value + dist * unk) %>% 
    select(-dist, -unk)
  return(db_de_age_all)
}

# distribution unknown ages and sex values
distribute_unknwons <- function(db){
  
  db_known <- db %>% 
    filter(Age != "unk",
           Sex != "o") 
  
  db_unk_sex <- db %>% 
    filter(Sex == "o") %>% 
    select(-Sex) %>% 
    rename(unk_sex = Value)
  
  db_unk_age <- db %>% 
    filter(Age == "unk") %>% 
    select(-Age) %>% 
    rename(unk_age = Value) 
  
  dists <- db_known %>% 
    group_by(Region, Measure, Sex) %>% 
    mutate(dist_age = Value / sum(Value)) %>% 
    ungroup() %>%  
    group_by(Region, Measure, Age) %>% 
    mutate(dist_sex = Value / sum(Value)) %>% 
    ungroup() %>% 
    left_join(db_unk_sex) %>% 
    left_join(db_unk_age) %>% 
    mutate(imp_sex = unk_sex * dist_sex,
           imp_age = unk_age * dist_age) %>% 
    replace_na(list(imp_sex = 0, imp_age = 0)) %>% 
    mutate(Value_imp = Value + imp_sex + imp_age) %>% 
    select(Region, Measure, Sex, Age, Value_imp) %>% 
    rename(Value = Value_imp)
  
  return(dists)
}


# harmonizing age groups
########################
harmonize_age <- function(db, lambda = 100){
  
  Age <- db$Age %>% as.integer()
  Value <- db$Value
  nlast <- 105 - max(Age)
  V1 <- pclm(x = Age, 
             y = Value, 
             nlast = nlast, 
             control = list(lambda = lambda, deg = 3))$fitted
  
  out <- tibble(Age = seq(0, 104, 1), Value = V1) 
  
  return(out)
}


# Kitagawa decomposition
########################
kitagawa <- function(db, p1, p2){
  two <- db %>% 
    filter(Region %in% c(p1, p2))
  
  vals1 <- two %>% 
    select(Region, CFR_t) %>% 
    unique()
  
  vals1 %>% filter(Region == p1) %>% pull(CFR_t)
  
  vals2 <- tibble(CFR1 = vals1 %>% filter(Region == p1) %>% pull(CFR_t),
                  CFR2 = vals1 %>% filter(Region == p2) %>% pull(CFR_t))
    
  cfr1 <- two %>% 
    filter(Region == p1) %>% 
    select(Age, age_dist, CFR) %>% 
    rename(C1 = age_dist, 
           CFR1 = CFR)
  
  cfr2 <- two %>% 
    filter(Region == p2) %>% 
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


# db <- db_cfr
# rfs <- rfs
# geo_level <- "Country"
# w <- 1
# h <- 2.5
# p2 <- "Germany"
# p1 <- "Canada"
diffs_ref <- function(db, rfs, geo_level, w, l, h, f){
  db_diffs_all <- NULL
  for(p1 in rfs){
    db_diffs_ref <- NULL
    db2 <- db %>% 
      filter(Type == geo_level,
             Wave == w)
    rgs <- unique(db2$Region)
    for(p2 in rgs){
      db_diffs_ref <- db_diffs_ref %>% 
        bind_rows(bind_cols(tibble(P1 = p1, P2 = p2), kitagawa(db2, p1, p2)))
    }
    
    db_diffs_ref2 <- db_diffs_ref %>% 
      gather(alpha, beta, key = "Components", value = Value) %>% 
      mutate(P2cfr = paste0(P2, " (", round(CFR2, 3), ")"),
             P1cfr = paste0(P1, " (", round(CFR1, 3), ") as reference"))
    
    db_diffs_all <- db_diffs_all %>% 
      bind_rows(db_diffs_ref2) %>% 
      filter(P2 != P1) %>% 
      mutate(t = "Total CFR difference")
  }
  
  db_diffs_all %>% 
    ggplot()+
    geom_hline(yintercept = 0, col = "black", size = 0.3, alpha = 0.5)+
    geom_bar(aes(reorder(P2cfr, -diff), Value, fill = Components, col = Components), stat = "identity", alpha = 0.5)+
    geom_point(aes(reorder(P2cfr, -diff), diff, shape = t), col = "black", size = 2)+
    facet_wrap(~ P1cfr, ncol = 1, scales = "free_y")+
    scale_y_continuous(limits = c(-l, l))+
    geom_hline(yintercept = 0, col = "black", size = 0.3, alpha = 0.5)+
    scale_color_manual(values = c("#43a2ca", "#e34a33"), labels = c("Age structure", "Fatality"))+
    scale_fill_manual(values = c("#43a2ca", "#e34a33"), labels = c("Age structure", "Fatality"))+
    # scale_shape_manual(values = colors)+
    guides(shape = guide_legend(title = "", order = 1))+
    labs(x = geo_level,
         y = "CFR difference")+
    theme_bw()+
    coord_flip()+
    theme(
      legend.position="bottom",
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
  ggsave(paste0("Figures/", f, "_cfr_diff_reference_", geo_level, "_wave_", w, ".png"), width = 5, height = h)
  return(db_diffs_all)
}


# kitagawa function generalizable
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

fit_baseline <- function(db2, a) {
  
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
  
  no_sea1 = gnm(Deaths ~ ns(t, 4) + offset(log(Exposure)), 
                data = train_base, 
                family = poisson(link="log"))
  
  no_sea2 = gnm(Deaths ~ ns(t, 4) + offset(log(Exposure)), 
                data = valid_base, 
                contrain = "*", 
                contrainTo = coef(reg1),
                family = poisson(link="log"))
  
  sea1 = gnm(Deaths ~ ns(t, 4) + sn52 + cs52 + offset(log(Exposure)), 
             data = train_base, 
             family = poisson(link="log"))
  
  sea2 = gnm(Deaths ~ ns(t, 4) + sn52 + cs52 + offset(log(Exposure)), 
             data = valid_base, 
             contrain = "*", 
             contrainTo = coef(reg1),
             family=poisson(link="log"))
  
  if (no_sea2$aic - sea2$aic > 6 | a >= 55) {
    # evaluating for overdispersion adjustment for seasonal model
    # Poisson model
    base_po <- glm(Deaths ~ splines::ns(t, 4) + sn52 + cs52 + offset(log(Exposure)), 
                   family = poisson, data = db_bline)
    
    # negative binomial to account for overdispersion
    base_nb <- try(MASS::glm.nb(Deaths ~ splines::ns(t, 4) + sn52 + cs52 + offset(log(Exposure)), 
                                data = db_bline), silent = T)
    ov_sign <- try(base_nb$theta / base_nb$SE.theta > 1.96, silent = T)
    if (class(base_nb)[1] == "try-error" | is.na(ov_sign)) {
      base_nb$aic <- base_po$aic
      base_nb$converged <- F
      ov_sign <- F
    }
    # compare AIC between Poisson and Negative binomial and fit the best model
    if ((base_po$aic - base_nb$aic >= 6) & (ov_sign) & (base_nb$converged) & class(base_nb)[1] != "try-error") {
      base <- MASS::glm.nb(Deaths ~ splines::ns(t, 4) + sn52 + cs52 + offset(log(Exposure)), 
                           data = db_bline)
    } else {
      base <- glm(Deaths ~ splines::ns(t, 4) + sn52 + cs52 + offset(log(Exposure)), 
                  family = poisson, data = db_bline)
    }
  } else {
    # evaluating for overdispersion adjustment for non-seasonal model
    # Poisson model
    base_po <- glm(Deaths ~ splines::ns(t, 4) + offset(log(Exposure)), 
                   family = poisson, data = db_bline)
    
    # Negative binomial to account for overdispersion
    base_nb <- try(MASS::glm.nb(Deaths ~ splines::ns(t, 4) + offset(log(Exposure)), 
                                data = db_bline), silent = T)
    ov_sign <- try(base_nb$theta / base_nb$SE.theta > 1.96, silent = T)
    if (class(base_nb)[1] == "try-error" | is.na(ov_sign)) {
      base_nb$aic <- base_po$aic
      base_nb$converged <- F
      ov_sign <- F
    }
    # compare AIC between Poisson and Negative binomial and fit the best model
    if ((base_po$aic - base_nb$aic >= 6) & (ov_sign) & (base_nb$converged) & class(base_nb)[3] != "try-error") {
      base <- MASS::glm.nb(Deaths ~ splines::ns(t, 4) + offset(log(Exposure)), 
                           data = db_bline)
    } else {
      base <- glm(Deaths ~ splines::ns(t, 4) + offset(log(Exposure)), 
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


# Previous Kitagawa functions

# decomposing CFR differences between populations over time
###########################################################

# decomp <- function(p1, p2, d_exc){
#   
#   # p1 <- "Quebec"
#   # p2 <- "Montreal"
#   # d_exc <- c(ymd("2020-04-28"), ymd("2020-04-29"))
#   # 
#   # 
#   
#   dates2 <- db_can_age2 %>% 
#     filter(Region %in% c(p1, p2),
#            !(Date %in% d_exc)) %>% 
#     select(Region, Date) %>% 
#     unique() %>% 
#     mutate(n = 1) %>% 
#     spread(Region, n) %>% 
#     rename(pop1 = 2,
#            pop2 = 3) %>% 
#     mutate(av = pop1 + pop2) %>% 
#     filter(!is.na(av)) %>% 
#     pull(Date)
#   
#   ymd(dates2[12])
#   
#   db_res <- NULL
#   
#   for(d in dates2){
#     res_t <- kita(db_can_age2, p1, p2, d)
#     db_res <- db_res %>% 
#       bind_rows(res_t)
#   }
#   
#   bd_res2 <- tibble(Date = dates2, R1 = p1, R2 = p2) %>% 
#     bind_cols(db_res)
#   
#   bd_res3 <- bd_res2 %>% 
#     select(Date, alpha, beta) %>% 
#     gather(-Date, key = "Component", value = "Value")
#   
#   diffs <- bd_res2 %>% 
#     select(Date, diff)
#   
#   bd_res4 <- bd_res3 %>% 
#     left_join(diffs)
#   
#   bd_res4 %>% 
#     ggplot()+
#     geom_hline(yintercept = 0, col = "black", size = 0.3, alpha = 0.5)+
#     geom_bar(aes(Date, Value, col = Component), stat = "identity", fill = "transparent")+
#     geom_point(aes(Date, diff), col = "black")+
#     scale_color_manual(values = c("blue", "red"))+
#     labs(title = paste0("Decomposition of CFR difference between ", p1, " and ", p2, " over time"))+
#     theme_bw()
#   
#   ggsave(paste0("Figures/cfr_diff_decomp_over_time_", p1, "_", p2, ".png"))
# }


# db_harm2 <- tibble()
# 
# for(pp in c("Ontario", "Toronto", "Alberta")){
#   for(ms in c("Cases", "Deaths")){
#     for(sx in c("f", "m")){
#       chunk <- db_canada2 %>% 
#         filter(Region == pp,
#                Measure == ms,
#                Sex == sx)
#       
#       db_harm <- harmonize_age(chunk) %>% 
#         mutate(Region = pp,
#                Measure = ms,
#                Sex = sx)
#       
#       db_harm2 <- bind_rows(db_harm2, db_harm)
#       
#     }
#   }
# }

# Kitagawa decomposition
########################
# kita <- function(db, p1, p2, d){
#   
#   vals1 <- db %>% 
#     filter(Date %in% d,
#            Region %in% c(p1, p2)) %>% 
#     group_by(Region) %>% 
#     summarise(CFR_t = max(CFR_t)) %>% 
#     ungroup()
#   
#   vals2 <- bind_cols(vals1[1,2] %>% 
#                        rename(CFR1 = CFR_t),
#                      vals1[2,2] %>% 
#                        rename(CFR2 = CFR_t))
#   
#   cfr1 <- db %>% 
#     filter(Date %in% d,
#            Region %in% p1) %>% 
#     select(Age, age_dist, CFR) %>% 
#     rename(C1 = age_dist, 
#            CFR1 = CFR)
#   
#   cfr2 <- db %>% 
#     filter(Date %in% d,
#            Region %in% p2) %>% 
#     select(Age, age_dist, CFR) %>% 
#     rename(C2 = age_dist, 
#            CFR2 = CFR)
#   
#   cfrs <- left_join(cfr1, cfr2) %>% 
#     mutate(d_C = C2 - C1,
#            d_CFR = CFR2 - CFR1,
#            a_C = (C2 + C1) * 0.5,
#            a_CFR = (CFR2 + CFR1) * 0.5)
#   
#   # decomposed into age and fatality component
#   cfrs_dec <- cfrs %>% 
#     group_by() %>% 
#     summarise(alpha = sum(d_C * a_CFR),
#               beta = sum(a_C * d_CFR)) %>%
#     ungroup() %>% 
#     mutate(diff = alpha + beta) %>% 
#     select(diff, alpha, beta)
#   
#   result <- bind_cols(vals2, cfrs_dec) %>% 
#     select(CFR1, CFR2, diff, alpha, beta)
#   
#   return(result)
# }


