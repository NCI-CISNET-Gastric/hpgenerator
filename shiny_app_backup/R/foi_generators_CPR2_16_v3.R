#### Function Force of Infection ####

#' \code{generate_foi_v3} get the Force of Infection (FOI) of the the Helicobacter pylori
#'  of the United States of America. 
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

generate_foi_v3 <- function(age = c(0:90), 
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
  
  df_alpha <- df_alpha_CPR2_16_v3 %>%
    dplyr::filter(v_race %in% race)
  
  v_alpha0 = df_alpha$v_alpha0
  v_alpha1 = df_alpha$v_alpha1
  
  df_gamma <- df_gamma_CPR2_16_v3 %>%
    dplyr::filter(v_race %in% race)
  v_gamma0 = df_gamma$v_gamma0
  v_gamma1 = df_gamma$v_gamma1
  
  v_num_cohort <- as.numeric(cohort)-1908
  
  #v_names_races <- c("Non-Hispanic White", "Non-Hispanic Black", 
  #                   "Other Hispanic", "Mexican-American", "Other")
  
  #c(outer(sex, period, FUN = "paste0"))
  
  #m_estimates <- c()
  df_estimates <- data.frame(NULL)
  
  for (cohort_i in seq_along(v_num_cohort)){ #period_i = 1
    num_cohort = v_num_cohort[cohort_i]
    v_alpha_est <- ilogit(v_alpha0 + v_alpha1*num_cohort) 
    v_gamma_est <- exp(v_gamma0 + v_gamma1*num_cohort)
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

#' \code{summary_foi_v3} summarise the Force of Infection (FOI) in a dataframe
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


summary_foi_v3 <- function(age = c(0:90), 
                           race = c("Hispanic", "NH American Indian", 
                                    "NH Asian", "NH Black", "NH White"),
                           cohort = 1908:1998,
                           percs = T) {
  
  options(dplyr.summarise.inform = FALSE)
  
  df_foi_hat <- generate_foi_v3(age = age, race = race, cohort = cohort)
  
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


library(tidyverse)


start.time <- Sys.time()

prueba <- generate_foi_v3()

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

prueba2 <- summary_foi_v3()

jet.colors <- colorRampPalette(c("black", "#00007F", "blue", "#007FFF",
                                        "cyan", "#7FFF7F", "yellow", "#FF7F00",
                                        "red", "#7F0000"))
                                        
color_map  <-  jet.colors(100)



df_hp_hat_final_prev_cohort_all <- prueba2 %>% group_by(age, cohort) %>% 
  summarise(across(everything(), mean)) %>% 
  mutate(cohort_f = as.character(cohort)) %>% 
  arrange(cohort)


ggplot(df_hp_hat_final_prev_cohort_all %>% filter(age!=0), aes(y = foi_mean, x = age, group = cohort, color = cohort)) + 
  geom_line(size = 1) +   
  xlab("Age") + 
  ylab(expression(paste("Force of infection ", (lambda)))) +
  labs(color='Birth cohort') +
  scale_colour_gradientn(colours = rev(jet.colors(100)), breaks = c(seq(1908, 1998, 30)))+
  scale_x_continuous(breaks = seq(0, 90, by = 10), 
                     labels = seq(0, 90, by = 10), 
                     limits = c(0, 90)) +
  #scale_y_continuous(breaks = seq(0, 100, by = 20), 
  #                   labels = c("0%", "20%", "40%", "60%", "80%", "100%"),
  #                   limits = c(0, 100)) +
  theme_bw(base_size = 13) + 
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.title = element_text(vjust = .8),
        legend.key.size = unit(.7, 'cm'), 
        legend.text = element_text(size = 10))+
  guides(fill=guide_legend(title="New Legend Title"))





ggplot(prueba2 %>% filter(age!=0, cohort>=1940), aes(y = foi_mean, x = age, group = cohort, color = cohort)) + 
  geom_line() +   
  #geom_point(data = df_geom_point_cohorts, aes(y = prev, x = age, color = cohort, size = total), shape = 1, stroke = 1) +
  facet_wrap(~ race) +
  xlab("Age") + 
  ylab(expression(paste("Force of infection ", (lambda)))) +
  labs(color='Birth cohort') +
  scale_colour_gradientn(colours = rev(jet.colors(100)), breaks = c(seq(1908, 1998, 30)))+
  scale_x_continuous(breaks = seq(0, 90, by = 10), 
                     labels = seq(0, 90, by = 10), 
                     limits = c(0, 90)) +
  #scale_y_continuous(breaks = seq(0, 100, by = 20), 
  #                   labels = c("0%", "20%", "40%", "60%", "80%", "100%"),
  #                   limits = c(0, 100)) +
  theme_bw(base_size = 13) + 
  theme(legend.position = c(.88, 0.2),
        #legend.box = "vertical",
        legend.title = element_text(vjust = .8),
        legend.key.size = unit(.7, 'cm'), 
        legend.spacing.x = unit(.1, 'cm'),
        legend.text = element_text(size = 10),
        strip.background = element_rect(fill = "transparent", color = "transparent")) +
  #edit legends
  guides(
    #reverse color order (higher value on top)
    color = guide_colorbar(reverse = TRUE))
