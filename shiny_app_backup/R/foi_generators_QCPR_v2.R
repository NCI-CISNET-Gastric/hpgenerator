#### Function Force of Infection ####

#' \code{generate_foi_v2} get the Force of Infection (FOI) of the the Helicobacter pylori
#'  of the United States of America. This FOI is based in the surveys reported in 
#'  the National Health and Nutrition Examination (NHANES) of 1988-1994 
#'  and 1999-2000 
#'
#' @param age Numeric vector specifying the age of the person with
#' Helicobacter pylori
#' @param race Character vector specifying the race of the person 
#' (Non-Hispanic White, Non-Hispanic Black, Other Hispanic, Mexican-American, Other)
#' @param cohort Character vector specifying the period of interest of the FOI. The default period is the year 1991
#' @import tidyverse
#' @return A dataframe with the distributions of the alpha and gamma parameters according to the specified variables.
#'
#' @examples
#' generate_foi(age = c(1,5,80), race = "NH Asian", 
#' cohort = "1980")
#'
#' generate_foi(age = 90, race = "NH Black", 
#' cohort = "1995")
#'
#' @export
#' 
#' 
#' 
#' 


load(file="data/df_alpha_QCPR_v2.rda")
load(file="data/df_gamma_QCPR_v2.rda")


generate_foi_v2 <- function(age = c(0:90), 
                         race = c("Hispanic", "NH American Indian", 
                                  "NH Asian", "NH Black", "NH White"), 
                         cohort = 1908:1998) {
  
  if (!all(age %in% c(0:90))) {
    stop("age must be a number or numeric vector between 0 and 90 years")
  }
  
  
  if (!all(race %in% c("Hispanic", "NH American Indian", 
                       "NH Asian", "NH Black", "NH White"))) {
    stop("Race must be a character or vector with the next races: Hispanic, 
         NH American Indian, NH Asian, NH Black, NH White")
  }
  
  options(dplyr.summarise.inform = FALSE)
  
  levels_race <- c("Hispanic", "NH American Indian", 
                   "NH Asian", "NH Black", "NH White")
  
  race <- factor(race,levels = levels_race)
  
  race <- sort(race)
  
  n_cohort <- length(cohort)
  
  cohort <- as.character(cohort)
  
  cohort <- factor(as.numeric(cohort), ordered = T)
  
  cohort <- as.character(sort(cohort))
  
  df_alpha <- df_alpha_QCPR_v2 %>%
    dplyr::filter(v_race %in% race)
  
  v_alpha0 = df_alpha$v_alpha0
  v_alpha1 = df_alpha$v_alpha1
  v_alpha2 = df_alpha$v_alpha2
  
  df_gamma <- df_gamma_QCPR_v2 %>%
    dplyr::filter(v_race %in% race)
  v_gamma0 = df_gamma$v_gamma0
  v_gamma1 = df_gamma$v_gamma1
  v_gamma2 = df_gamma$v_gamma2
  
  v_num_cohort <- as.numeric(cohort)-1908
  
  #v_names_races <- c("Non-Hispanic White", "Non-Hispanic Black", 
  #                   "Other Hispanic", "Mexican-American", "Other")
  
  #c(outer(sex, period, FUN = "paste0"))
  
  #m_estimates <- c()
  df_estimates <- data.frame(NULL)

    for (cohort_i in seq_along(v_num_cohort)){ #period_i = 1
      num_cohort = v_num_cohort[cohort_i]
      v_alpha_est <- ilogit(v_alpha0 + v_alpha1*num_cohort + v_alpha2*num_cohort^2) 
      v_gamma_est <- exp(v_gamma0 + v_gamma1*num_cohort + v_gamma2*num_cohort^2)
      v_age <- matrix(age, nrow = 1)
      
      m_foi_hat <- ((v_alpha_est*v_gamma_est) * exp(-(v_gamma_est %*% v_age)))/(1-v_alpha_est*(1-exp(-(v_gamma_est %*% v_age))))
      
      colnames(m_foi_hat) <- paste0("age_",as.character(age))
      
      df_foi_hat <- data.frame(sim = 1:2000,
                               race = rep(race, each = 2000),
                               cohort = as.numeric(cohort[cohort_i]),
                               m_foi_hat,
                               check.names = F)
      
      #m_estimates <- rbind(m_estimates, m_foi_hat)
      df_estimates <- rbind.data.frame(df_estimates, df_foi_hat)
  }
  
  return(df_estimates)
}



#### Summary Force of Infection ####

#' \code{summary_foi_v2} summarise the Force of Infection (FOI) in a dataframe
#' according to the variables specified by the user. 
#' 
#'
#' @param age Numeric vector specifying the age of the person with
#' Helicobacter pylori
#' @param race Character vector specifying the race of the person 
#' (Non-Hispanic White, Non-Hispanic Black, Other Hispanic, Mexican-American, Other)
#' @param cohort Character vector specifying the cohort of interest of the FOI. The default cohort is the year 1908
#' @param percs Logical statement specifying the Confidence Intervals (Upper Bound (UB) and Lower Bound (LB)).
#' TRUE to get Confidence Intervals.
#' @import tidyverse
#' @return A dataframe with the Force of Infection (FOI) according to the specified variables by the user.
#'
#' @examples
#' summary_foi(age = c(1,5,80), race = "Hispanic", 
#' period = "2000", percs = c(LB, UB))
#'
#' summary_foi(age = 90, race = "NH Black", 
#' cohort = "1995", percs = c(LB, UB))
#'
#' @export
#' 
#' 

summary_foi_v2 <- function(age = c(0:90), 
                        race = c("Hispanic", "NH American Indian", 
                                 "NH Asian", "NH Black", "NH White"),
                        cohort = 1908:1998,
                        percs = T) {
  
  options(dplyr.summarise.inform = FALSE)
  
  df_foi_hat <- generate_foi_v2(age = age, race = race, cohort = cohort)
  
  LB <- 0.025
  
  UB <- 0.975
  
  df_foi_long <- df_foi_hat %>% tidyr::pivot_longer(names_to = "age",
                                             values_to = "value",
                                             cols = starts_with("age_"),
                                             names_prefix = "age_")
  
  df_foi_long$age <- as.numeric(df_foi_long$age)
  
  
  df_foi_final <- df_foi_long %>% group_by(race, cohort, age) %>% 
    summarise(foi_mean = mean(value),
              foi_cri_lb = quantile(value, probs = LB, na.rm= TRUE),
              foi_cri_ub = quantile(value, probs = UB, na.rm= TRUE)) %>% ungroup()
  
  
  if(percs==F){
    df_foi_final <- df_foi_final %>% dplyr::select(-foi_cri_lb, -foi_cri_ub)  
  }
  
  return(df_foi_final)
  
}



