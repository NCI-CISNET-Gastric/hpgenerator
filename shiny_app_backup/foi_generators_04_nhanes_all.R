


library(tidyverse)
library(data.table)
library(doParallel)
library(foreach)

generate_foi_linear_birth_cohort_alt <- function(df_alpha_up,
                                             df_gamma_up,
                                             age = c(0:90), 
                                             race = c("Hispanic", "NH Black", "NH White", "Other"), 
                                             cohort = 1908:1998,
                                             birthplace = c("US", "Foreign")) {
  
  if (!all(age %in% c(0:90))) {
    stop("age must be a number or numeric vector between 0 and 90 years")
  }
  
  valid_races <- c("Hispanic", "NH Black", "NH White", "Other")
  if (!all(race %in% valid_races)) {
    stop("Race must be a character or vector with the following races: Hispanic, NH Black, NH White, Other")
  }
  
  
  options(dplyr.summarise.inform = FALSE)
  
  
  levels_birthplace <- c("US", "Foreign")
  birthplace <- factor(birthplace,levels = levels_birthplace)
  birthplace <- sort(birthplace)
  
  
  levels_race <- c("Hispanic", "NH Black", "NH White", "Other")
  
  race <- factor(race, levels = levels_race)
  
  race <- sort(race)
  
  num_race <- as.character(as.numeric(race))
  
  
  n_cohort <- length(cohort)
  
  
  cohort <- as.character(cohort)
  
  cohort <- factor(as.numeric(cohort), ordered = T)
  
  cohort <- as.character(sort(cohort))
  
  v_num_cohort <- as.numeric(cohort)-1908
  
  v_age <- age
  
  
  df_alpha <- df_alpha_up %>%
    dplyr::filter(v_race %in% race)
  
  v_alpha0 = df_alpha$v_alpha0
  v_alpha1 = df_alpha$v_alpha1
  v_alpha2 = df_alpha$v_alpha2
  
  df_gamma <- df_gamma_up %>%
    dplyr::filter(v_race %in% race)
  
  v_gamma0 = df_gamma$v_gamma0
  v_gamma1 = df_gamma$v_gamma1
  v_gamma2 = df_gamma$v_gamma2
  
  v_num_cohort <- as.numeric(cohort)-1908
  
  df_estimates <- data.frame(NULL)
  
  for (bp_i in birthplace){#sex_i = "Female"
    if(bp_i == "US"){
      num_bp <- 0
    } else if (bp_i == "Foreign"){
      num_bp <- 1
    } 
    
    no_cores <- parallel::detectCores() - 7   # detect number of cores
    cl <- parallel::makeCluster(no_cores)      # initialize cluster object
    doParallel::registerDoParallel(cl)
    
    df_final <- foreach::foreach (cohort_i = seq_along(v_num_cohort), .combine = rbind) %do% { #period_i = 1
      num_cohort = v_num_cohort[cohort_i]
      v_alpha_est <- ilogit(v_alpha0 + v_alpha1*num_bp + v_alpha2*num_cohort) 
      v_gamma_est <- exp(v_gamma0 + v_gamma1*num_bp + v_gamma2*num_cohort)
      v_age <- matrix(age, nrow = 1)
      
      m_foi_hat <- ((v_alpha_est*v_gamma_est) * exp(-(v_gamma_est %*% v_age)))/(1-v_alpha_est*(1-exp(-(v_gamma_est %*% v_age))))
      
      colnames(m_foi_hat) <- paste0("age_",as.character(age))
      
      df_foi_hat <- data.frame(sim = 1:3000,
                               race = rep(race, each = 3000),
                               cohort = as.numeric(cohort[cohort_i]),
                               birthplace = bp_i,
                               m_foi_hat,
                               check.names = F)
      
      #m_estimates <- rbind(m_estimates, m_foi_hat)
      #df_estimates <- rbind.data.frame(df_estimates, df_foi_hat)
      
      
      df_foi_hat
      
    }
    parallel::stopCluster(cl)
    
    df_estimates <- rbind.data.frame(df_estimates, df_final)
    
    df_estimates_final <- as.data.table(df_estimates) %>% melt(id.vars = c("sim", "race", "cohort", "birthplace"))
    LB <- 0.025
    UB <- 0.975
    
    df_estimates_final <- df_estimates_final[, .(
      foi_mean = mean(value),
      foi_cri_lb = quantile(value, probs = LB, na.rm = TRUE),
      foi_cri_ub = quantile(value, probs = UB, na.rm = TRUE)
    ), by = .(race, cohort, birthplace, variable)]
    
    df_estimates_final <- df_estimates_final[, age := as.numeric(str_remove(variable, "age_"))]
    
    #  if (!percs) {
    #    df_foi_final <- df_foi_final[, .(
    #      race, cohort, birthplace, age, foi_mean
    #    )]
    #  }
  }
  return(df_estimates_final)
}


start.time <- Sys.time()  

df_foi_linear_birth_cohort_all <- generate_foi_linear_birth_cohort_alt(df_alpha_up, df_gamma_up)

end.time <- Sys.time()  # End the timer


print(end.time - start.time)
