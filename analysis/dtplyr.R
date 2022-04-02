
generate_foi <- function(age = c(0:100), 
                         sex = c("Male", "Female"), 
                         race = c("Non-Hispanic White", "Non-Hispanic Black", 
                                  "Other Hispanic", "Mexican-American", "Other"), 
                         period = 1991:2010) {
  
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


library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)

summary_foi <- function(age = c(0:100), 
                        sex = c("Male", "Female"), 
                        race = c("Non-Hispanic White", "Non-Hispanic Black", 
                                 "Other Hispanic", "Mexican-American", 
                                 "Other"), 
                        period = 1991:2000, 
                        percs = T) {
  
  options(dplyr.summarise.inform = FALSE)
  
  df_foi_hat <- generate_foi(age = age, sex = sex, race = race, period = period)
  
  dt_foi_hat <- lazy_dt(df_foi_hat)
  
  LB <- 0.025
  
  UB <- 0.975


  # df_foi_long <- df_foi_hat %>% tidyr::pivot_longer(names_to = "age",
  #                                                   values_to = "value",
  #                                                   cols = starts_with("age_"),
  #                                                   names_prefix = "age_")
  
  dt_foi_long <- dt_foi_hat %>% tidyr::pivot_longer(names_to = "age",
                                                    values_to = "value",
                                                    cols = starts_with("age_"),
                                                    names_prefix = "age_")
  

  dt_foi_long$age <- as.numeric(dt_foi_long$age)
  

  #df_foi_final <- df_foi_long %>% group_by(race, period, sex, age) %>% 
  #  summarise(foi_mean = mean(value),
  #            foi_cri_lb = quantile(value, probs = LB),
  #            foi_cri_ub = quantile(value, probs = UB)) %>% ungroup()
  

  dt_foi_final <- dt_foi_long %>% group_by(race, period, sex, age) %>% 
    summarise(foi_mean = mean(value),
              foi_cri_lb = quantile(value, probs = LB),
              foi_cri_ub = quantile(value, probs = UB)) %>% ungroup() %>% as.data.frame()
  
  
  
  dt_foi_final <- dt_foi_final$parent$parent$parent$parent$parent$parent$parent
  
  if(percs==F){
  #  df_foi_final <- df_foi_final %>% dplyr::select(-foi_cri_lb, -foi_cri_ub)  
    dt_foi_final <- dt_foi_final %>% dplyr::select(-foi_cri_lb, -foi_cri_ub) 
  }
  
  return(dt_foi_final)
  
}

library(hpgenerator)


plot_foi <- function(age = c(0:100), 
                     sex = c("Male", "Female"), 
                     race = c("Non-Hispanic White", "Non-Hispanic Black", 
                              "Other Hispanic", "Mexican-American", 
                              "Other"), 
                     period = 1991, 
                     percs = T,
                     age_bracket = 10,
                     save_data = F,
                     save_plot = F,
                     plot_name = "FOI",
                     data_name = "df_foi",
                     width = 10,
                     height = 8) {
  
  jet.colors <- colorRampPalette(c("black", "#00007F", "blue", "#007FFF",
                                   "cyan", "#7FFF7F", "yellow", "#FF7F00",
                                   "red", "#7F0000"))
  
  color_map  <-  jet.colors(100)
  
  options(dplyr.summarise.inform = FALSE)
  
  df_foi_plot <- summary_foi(age = age, sex = sex, race=race, period=period, percs = T)
  
  n_sex <- length(sex)
  
  n_race <- length(race)
  
  n_period <- length(period)
  
  levels_sex <- c("Female", "Male")
  
  df_foi_plot$sex <- factor(dt_foi_plot$sex, levels = levels_sex)
  
  df_foi_plot$age <- as.numeric(dt_foi_plot$age)
  
  
  plot_temp <-ggplot(df_foi_plot, aes(y = foi_mean, x = age, group = period, 
                                      color = period)) + 
    geom_line(aes(x = age, y = foi_mean))+
    xlab("Age") + ylab(expression(paste("Force of infection ", (lambda)))) +
    scale_x_continuous(breaks = seq(0, last(age), by = age_bracket), 
                       labels = seq(0, last(age), by = age_bracket), 
                       limits = c(0, last(age))) +
    scale_colour_gradientn(colours = jet.colors(100), breaks = c(unique(df_foi_plot$period))) +
    scale_fill_gradientn(colours = jet.colors(100), breaks = c(unique(df_foi_plot$period))) +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom",
          legend.box = "vertical",
          legend.title = element_blank(),
          strip.background = element_rect(fill = "white",
                                          color = "white"),
          strip.text = element_text(size = 14, face = "bold"))+
    scale_size(guide = "none") +
    guides(fill = "none", color = guide_legend(override.aes = list(size = 3,
                                                                   color = jet.colors(n_period),
                                                                   fill = jet.colors(n_period))))
  
  if(percs){
    plot_temp <- plot_temp +
      geom_ribbon(aes(ymin = foi_cri_lb, ymax = foi_cri_ub, fill = period), 
                  colour = "transparent", alpha = 0.3, na.rm = F)
  }  
  
  if (n_sex==1 & n_race==1){
    plot_final <- plot_temp + labs(title = sex, subtitle = race)
  } else if (n_sex==2 & n_race==1){
    plot_final <- plot_temp + facet_grid(race~sex)
  } else {
    plot_final <- plot_temp + facet_grid(sex~race)
  }
  if(save_plot){
    
    ggsave(paste0("figs/", plot_name, ".jpg"), plot = plot_final, width = width, height = height)
    
  }
  
  if(save_data){
    
    save(df_foi_plot, file = paste0("data/", data_name, ".Rdata"))
    write.csv(df_foi_plot, file = paste0("data/", data_name, ".csv"))
    
  }   
  return(plot_final)
  
  
}
