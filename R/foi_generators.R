#### Function Force of Infection ####

#' \code{generate_foi} get the Force of Infection (FOI) of the the Helicobacter pylori
#'  of the United States of America. This FOI is based in the surveys reported in 
#'  the National Health and Nutrition Examination (NHANES) of 1988-1994 
#'  and 1999-2000 
#'
#' @param age Numeric vector specifying the age of the person with
#' Helicobacter pylori
#' @param sex Character vector specifying the sex of the person (Male, Female)
#' @param race Character vector specifying the race of the person 
#' (Non-Hispanic White, Non-Hispanic Black, Other Hispanic, Mexican-American, Other)
#' @param period Character vector specifying the period of interest of the FOI. The default period is the year 1991
#' @import dplyr
#' @return A dataframe with the distributions of the alpha and gamma parameters according to the specified variables.
#'
#' @examples
#' generate_foi(age = c(1,5,80), sex = c("Male", "Female"), race = "Non-Hispanic White", 
#' period = "2000")
#'
#' generate_foi(age = 90, sex = "Male", race = "Other", 
#' period = "1995")
#'
#' @export
#' 
#' 

generate_foi <- function(age = c(0:100), 
                         sex = c("Male", "Female"), 
                         race = c("Non-Hispanic White", "Non-Hispanic Black", 
                                  "Other Hispanic", "Mexican-American", "Other"), 
                         period = 1991) {
  
  if (!all(age %in% c(0:100))) {
    stop("age must be a number or numeric vector between 0 and 100 years")
  }
  
  if (!all(sex %in% c("Male", "Female"))){
    stop("Sex must be a character or vector with the next years: Male, Female")
    
  }
  
  if (is.null(sex)){
    sex <- c("Male", "Female")
  }
  n_sex <- length(sex)
  
  if (n_sex > 2){
    stop("Sex must be a character of Male or Female or a vector with both")
  }
  if (n_sex == 2 & sex[1]==sex[2]){
    stop("Sex must be a character of Male or Female or a vector with both")
  }
  
  levels_sex <- c("Female", "Male")
  sex <- factor(sex,levels = levels_sex)
  sex <- sort(sex)
  
  if (!all(race %in% c("Non-Hispanic White", "Non-Hispanic Black", 
                       "Other Hispanic", "Mexican-American", "Other"))) {
    stop("Race must be a character or vector with the next races: Non-Hispanic White, 
         Non-Hispanic Black, Other Hispanic, Mexican-American, Other")
  }
  
  options(dplyr.summarise.inform = FALSE)
  
  levels_race <- c("Non-Hispanic White","Non-Hispanic Black",
                   "Other Hispanic","Mexican-American", "Other")
  
  race <- factor(race,levels = levels_race)
  
  race <- sort(race)
  
  n_period <- length(period)
  
  period <- as.character(period)
  
  period <- factor(as.numeric(period), ordered = T)
  
  period <- as.character(sort(period))
  
  df_alpha <- df_alpha_race %>%
    filter(v_race %in% race)
  v_alpha0 = df_alpha$v_alpha0
  v_alpha1 = df_alpha$v_alpha1
  v_alpha2 = df_alpha$v_alpha2
  
  df_gamma <- df_gamma_race %>%
    filter(v_race %in% race)
  v_gamma0 = df_gamma$v_gamma0
  v_gamma1 = df_gamma$v_gamma1
  v_gamma2 = df_gamma$v_gamma2
  
  v_num_period <- as.numeric(period)-1991
  
  #v_names_races <- c("Non-Hispanic White", "Non-Hispanic Black", 
  #                   "Other Hispanic", "Mexican-American", "Other")
  
  #c(outer(sex, period, FUN = "paste0"))
  
  #m_estimates <- c()
  df_estimates <- data.frame(NULL)
  for (sex_i in sex){#sex_i = "Female"
    if(sex_i == "Male"){
      num_sex <- 0
    } else if (sex_i == "Female"){
      num_sex <- 1
    }
    for (period_i in seq_along(v_num_period)){ #period_i = 1
      num_period = v_num_period[period_i]
      v_alpha_est <- ilogit(v_alpha0 + v_alpha1*num_period + v_alpha2*num_sex) 
      v_gamma_est <- exp(v_gamma0 + v_gamma1*num_period + v_gamma2*num_sex)
      
      v_age <- matrix(age, nrow = 1)
      
      m_foi_hat <- ((v_alpha_est*v_gamma_est) * exp(-(v_gamma_est %*% v_age)))/(1-v_alpha_est*(1-exp(-(v_gamma_est %*% v_age))))
      
      colnames(m_foi_hat) <- paste0("age_",as.character(age))
      
      df_foi_hat <- data.frame(sim = 1:10000,
                               race = rep(race, each = 10000),
                               period = as.numeric(period[period_i]),
                               sex = sex_i,
                               country = "USA",
                               m_foi_hat,
                               check.names = F)
      
      #m_estimates <- rbind(m_estimates, m_foi_hat)
      df_estimates <- rbind.data.frame(df_estimates, df_foi_hat)
    }
  }
  
  return(df_estimates)
}

#### Summary Force of Infection ####

#' \code{summary_foi} summarise the Force of Infection (FOI) in a dataframe
#' according to the variables specified by the user. 
#' 
#'
#' @param age Numeric vector specifying the age of the person with
#' Helicobacter pylori
#' @param sex Character vector specifying the sex of the person (Male, Female)
#' @param race Character vector specifying the race of the person 
#' (Non-Hispanic White, Non-Hispanic Black, Other Hispanic, Mexican-American, Other)
#' @param period Character vector specifying the period of interest of the FOI. The default period is the year 1991
#' @param percs Logical statement specifying the Confidence Intervals (Upper Bound (UB) and Lower Bound (LB)).
#' TRUE to get Confidence Intervals.
#' @import dplyr
#' @return A dataframe with the Force of Infection (FOI) according to the specified variables by the user.
#'
#' @examples
#' summary_foi(age = c(1,5,80), sex = c("Male", "Female"), race = "Non-Hispanic White", 
#' period = "2000", percs = c(LB, UB))
#'
#' summary_foi(age = 90, sex = "Male", race = "Other", 
#' period = "1995", percs = c(LB, UB))
#'
#' @export
#' 
#' 

summary_foi <- function(age = c(0:100), 
                        sex = c("Male", "Female"), 
                        race = c("Non-Hispanic White", "Non-Hispanic Black", 
                                 "Other Hispanic", "Mexican-American", 
                                 "Other"), 
                        period = 1991, 
                        percs = T) {
  
  options(dplyr.summarise.inform = FALSE)
  
  df_foi_hat <- generate_foi(age = age, sex = sex, race = race, period = period)
  
  LB <- 0.025
  
  UB <- 0.975
  
  df_foi_long <- df_foi_hat %>% pivot_longer(names_to = "age",
                                             values_to = "value",
                                             cols = starts_with("age_"),
                                             names_prefix = "age_")
  
  df_foi_long$age <- as.numeric(df_foi_long$age)
  
  
  df_foi_final <- df_foi_long %>% group_by(race, period, sex, age) %>% 
    summarise(foi_mean = mean(value),
              foi_cri_lb = quantile(value, probs = LB),
              foi_cri_ub = quantile(value, probs = UB)) %>% ungroup()
  
  
  if(percs==F){
    df_foi_final <- df_foi_final %>% dplyr::select(-foi_cri_lb, -foi_cri_ub)  
  }
  
  return(df_foi_final)
  
}