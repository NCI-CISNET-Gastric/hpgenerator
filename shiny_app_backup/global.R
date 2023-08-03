load("data/df_alpha_race.rda")
load("data/df_gamma_race.rda")
#load("data/df_parameters.Rdata")


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
#' @import tidyverse
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

generate_foi <- function(age = c(0:1000), 
                         sex = c("Male", "Female"), 
                         race = c("Non-Hispanic White", "Non-Hispanic Black", 
                                  "Other Hispanic", "Mexican-American", "Other"), 
                         period = 1991:2010) {
  
  if (!all(age %in% c(0:1000))) {
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
#' @import tidyverse
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

summary_foi <- function(age = c(0:1000), 
                        sex = c("Male", "Female"), 
                        race = c("Non-Hispanic White", "Non-Hispanic Black", 
                                 "Other Hispanic", "Mexican-American", 
                                 "Other"), 
                        period = 1991:2010, 
                        percs = T) {
  
  options(dplyr.summarise.inform = FALSE)
  
  df_foi_hat <- generate_foi(age = age, sex = sex, race = race, period = period)
  
  LB <- 0.025
  
  UB <- 0.975
  
  df_foi_long <- df_foi_hat %>% tidyr::pivot_longer(names_to = "age",
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



#### Plot the Force of Infection ####

#' \code{plot_foi} Plot the Force of Infection (FOI)
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
#' @param age_bracket Set the number of brakes for the age in the x axis in the plot.
#' @param save_data Logical statement. TRUE to save the dataframe of the plot.
#' @param save_plot Logical statement. TRUE to save the plot as a jpg file.
#' @param plot_name Name in string that will have the saved plot.
#' @param data_name Name in string that will have the saved dataframe.
#' @param width width of the saved plot.
#' @param height height of the saved plot. 
#' @import tidyverse
#' @import ggplot2
#' @import dplyr
#' @return A dataframe with the Force of Infection (FOI) according to the specified variables by the user.
#'
#' @examples
#' plot_foi(age = c(1,5,80), sex = c("Male", "Female"), race = "Non-Hispanic White", 
#' period = "2000", percs = F, age_bracket = 5, save_data = F, save_plot = T,
#' plot_name = "Force of Infection", data_name = "df_force_infection", 
#' width = 14, height = 10)
#'
#' plot_foi(age = 90, sex = "Male", race = c("Non-Hispanic White", "Non-Hispanic Black", 
#' "Other Hispanic", "Mexican-American", "Other"), 
#' period = c(1995, 2000, 2010))
#'
#' @export
#' 

plot_foi <- function(age = c(0:1000), 
                     sex = c("Male", "Female"), 
                     race = c("Non-Hispanic White", "Non-Hispanic Black", 
                              "Other Hispanic", "Mexican-American", 
                              "Other"), 
                     period = 1991:2010, 
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
  
  df_foi_plot$sex <- factor(df_foi_plot$sex, levels = levels_sex)
  
  
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


#### Plot the Force of Infection (Shiny Specifications) ####

#' \code{plot_foi_shiny} Plot the Force of Infection (FOI)
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
#' @param age_bracket Set the number of brakes for the age in the x axis in the plot.
#' @param save_data Logical statement. TRUE to save the dataframe of the plot.
#' @param save_plot Logical statement. TRUE to save the plot as a jpg file.
#' @param plot_name Name in string that will have the saved plot.
#' @param data_name Name in string that will have the saved dataframe.
#' @param width width of the saved plot.
#' @param height height of the saved plot. 
#' @import tidyverse
#' @import ggplot2
#' @import dplyr
#' @return A dataframe with the Force of Infection (FOI) according to the specified variables by the user.
#'
#' @examples
#' plot_foi(age = c(1,5,80), sex = c("Male", "Female"), race = "Non-Hispanic White", 
#' period = "2000", percs = F, age_bracket = 5, save_data = F, save_plot = T,
#' plot_name = "Force of Infection", data_name = "df_force_infection", 
#' width = 14, height = 10)
#'
#' plot_foi(age = 90, sex = "Male", race = c("Non-Hispanic White", "Non-Hispanic Black", 
#' "Other Hispanic", "Mexican-American", "Other"), 
#' period = c(1995, 2000, 2010))
#'
#' @export
#' 


plot_foi_shiny <- function(age = c(0:1000), 
                           sex = c(),
                           race = c("Non-Hispanic White", "Non-Hispanic Black", 
                                    "Other Hispanic", "Mexican-American", 
                                    "Other"), 
                           period = 1991:2010, 
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
  
  df_foi_plot$sex <- factor(df_foi_plot$sex, levels = levels_sex)
  
  df_foi_plot$age <- as.numeric(df_foi_plot$age)
  
  
  plot_temp <-ggplot(df_foi_plot, aes(y = foi_mean, x = age, group = period, 
                                      color = period)) + 
    geom_line(aes(x = age, y = foi_mean))+
    xlab("Age") + ylab(expression(paste("Force of infection ", (lambda)))) +
    scale_x_continuous(breaks = seq(0, last(age), by = age_bracket), 
                       labels = seq(0, last(age), by = age_bracket), 
                       limits = c(0, last(age))) +
    scale_colour_gradientn(colours = jet.colors(100), breaks = c(unique(df_foi_plot$period))) +
    scale_fill_gradientn(colours = jet.colors(100), breaks = c(unique(df_foi_plot$period))) +
    theme_bw(base_size = 13) +
    theme(axis.title.y = element_text(size = 19),
          axis.title.x = element_text(size = 19),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.text = element_text(size=16),
          legend.title = element_blank(),
          strip.background = element_rect(fill = "white",
                                          color = "white"),
          strip.text = element_text(size = 17, face = "bold"))+
    scale_size(guide = "none") +
    guides(fill = "none", color = guide_legend(override.aes = list(size = 1.5,
                                                                   color = jet.colors(n_period),
                                                                   fill = jet.colors(n_period)),
                                               direction = "horizontal", keyheight = 1))
  
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



#### Plot State-Specific Cancer Incidence  (Relative Difference) ####

#' \code{plot_incidence_rel_diff} Plot the State-Specific Cancer Incidence relative difference across scenarios.
#' 
#' 
#'
#' @param state Character specifying the abbreviation of the state of interest. 
#' @param period Numeric vector specifying the period of interest. The default period is the year 2006-2100.
#@param percs Logical statement specifying the Confidence Intervals (Upper Bound (UB) and Lower Bound (LB)).
#TRUE to get Confidence Intervals.
#' @param scenarios Character vector specifying different scenarios of interest. 
#' @param save_data Logical statement. TRUE to save the dataframe of the plot.
#' @param type Character specifying the type of plot generated (Line, Heatmap and AgeSpecific)
#' @param age_specific Logical statement. TRUE to relative differences presented by age.
#' @import tidyverse
#' @import ggplot2
#' @import dplyr
#' 
#' 
#' 
#' @return
#'
#' @examples
#
#'
#' @export
#' 


plot_incidence_rel_diff <- function(state = "CA",
                                    period = c(2006:2100),
                                    scenarios = c("Covid-19 Backslide",
                                                  "Healthy Initiative 5 yrs",
                                                  "Healthy Initiative 10 yrs"),
                                    save_data = FALSE,
                                    type = "Line"){
  
  
  if(state == "US" & type %in% c("Line", "Heatmap")){
    df_plot <- df_proj_scn_RelRed_sum %>%
      ungroup() %>%
      filter(subtype == "Aggreg" & abbrev_ste == "" &
               Outcome == "Cervical Cancer Incidence" &
               abbrev_scn != "scn0")
  }else if (state != "US" & type %in% c("Line", "Heatmap")){
    df_plot <- df_proj_scn_RelRed_sum %>%
      ungroup() %>%
      filter(subtype == "Aggreg" & abbrev_ste == state &
               Outcome == "Cervical Cancer Incidence" &
               abbrev_scn != "scn0")
  }else if(state == "US" & type %in% c("AgeSpecific")){
    
    df_plot <- df_proj_scn_RelRed_sum %>%
      ungroup() %>%
      filter(subtype == "YrlAges" & abbrev_ste == "" &
               Outcome == "Cervical Cancer Incidence" &
               abbrev_scn != "scn0")
    
  } else if (state != "US" & type %in% c("AgeSpecific")){
    
    df_plot <- df_proj_scn_RelRed_sum %>%
      ungroup() %>%
      filter(subtype == "YrlAges" & abbrev_ste == state &
               Outcome == "Cervical Cancer Incidence" &
               abbrev_scn != "scn0")
  }
  
  df_plot$year <- as.numeric(df_plot$year)
  
  df_plot <- df_plot %>% filter(year %in% period,
                                scenario %in% scenarios)
  
  if(state=="US"){
    
    plot_title <- "United States"
    
  }else{
    
    plot_title <- state.name[which(state == state.abb)]
    
  }
  # Plot subtitle
  plot_subtitle <- "Relative difference"
  
  # Plot
  
  if(type=="Line"){
    
    plot_final <- ggplot(df_plot,
                         aes(x = year, y = diff*100, color = scenario,
                             fill = scenario, ymin = lb*100, ymax = ub*100)) +
      geom_line(size = 1) +
      geom_ribbon(alpha = 0.5) +
      scale_linetype_manual("", values = 1:4) +
      scale_color_manual("", values = scales::viridis_pal()(5)) +
      scale_fill_manual("", values = scales::viridis_pal()(5)) +
      scale_x_continuous("",
                         breaks = number_ticks(15)) +
      geom_vline(xintercept = c(2008, 2019), linetype = 2:3) +
      geom_hline(yintercept = 0, linetype = 4) +
      scale_y_continuous("Percentage difference (%)",
                         breaks = number_ticks(10)) +
      labs(title = plot_title,
           subtitle = plot_subtitle) +
      theme_ipsum(base_size = 18,
                  plot_title_size = 24,
                  subtitle_size = 16,
                  grid = FALSE) +
      theme(text = element_text(size = text_size + 8),
            axis.text.x = element_text(angle = 90,
                                       hjust = 0.5,
                                       vjust = 0.5,
                                       size = 16),
            axis.text.y = element_text(angle = 0,
                                       hjust = 0.5,
                                       vjust = 0.5,
                                       size = 16),
            axis.title.y = element_text(size = 16,
                                        hjust = 0.5,
                                        vjust = 0.5),
            panel.background = element_rect(fill = "white",
                                            colour = "gray",
                                            size = 0.15,
                                            linetype = "solid"),
            legend.position = "right",
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.key = element_rect(fill   = "transparent",
                                      colour = "transparent",
                                      size   = unit(7, "cm")))
    
    
    
  } else if (type=="Heatmap"){
    
    v_scenarios <- c(backslide = "Covid-19 Backslide",
                     HI5yrs    = "Healthy Initiative 5 yrs",
                     HI10yrs   = "Healthy Initiative 10 yrs")
    
    df_plot$scenario <- factor(df_plot$scenario, levels = rev(v_scenarios))
    
    plot_final <- ggplot(df_plot,
                         aes(x = year, y = scenario, fill = diff*100)) +
      geom_raster() +
      labs(x = "Year", y = "", title = plot_title, subtitle = plot_subtitle,
           fill = "Percentage\ndifference"
      ) +
      scale_x_continuous(breaks = number_ticks(20)) +
      # scale_y_continuous(breaks = number_ticks(10)) +
      # scale_fill_gradientn(colours = color_map_mod) + # Hawre's jet map
      scale_fill_viridis(option="mako", direction = -1) +
      theme_ipsum(base_size = 18,
                  plot_title_size = 24,
                  subtitle_size = 16,
                  grid = FALSE) + #facet_wrap( ~ state, nrow = 1) +
      theme(text = element_text(size = text_size + 8),
            axis.text.x = element_text(angle = 90,
                                       hjust = 0.5,
                                       vjust = 0.5,
                                       size = 16),
            axis.title.x = element_text(size = 16,
                                        hjust = 0.5,
                                        vjust = 0.5),
            axis.text.y = element_text(angle = 0,
                                       hjust = 0.5,
                                       vjust = 0.5,
                                       size = 16),
            axis.title.y = element_text(size = 16,
                                        hjust = 0.5,
                                        vjust = 0.5),
            panel.background = element_rect(fill = "white",
                                            colour = "gray",
                                            size = 0.15,
                                            linetype = "solid"),
            legend.position = "right",
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.key = element_rect(fill   = "transparent",
                                      colour = "transparent",
                                      size   = unit(7, "cm")))
    
    
  } else if (type=="AgeSpecific"){
    
    
    df_plot$age_grps <- as.numeric(df_plot$age_grps)
    
    plot_final <- ggplot(subset(df_plot, age_grps >= 15), #& age_grps <=5),
                         aes(x = year, y = age_grps, fill = diff*100)) +
      geom_raster() +
      labs(x = "Year", y = "Age", title = plot_title, subtitle = plot_subtitle,
           fill = "Percentage\ndifference"
      ) +
      scale_x_discrete(breaks = number_ticks(20)) +
      scale_y_continuous(breaks = number_ticks(10)) +
      # scale_fill_gradientn(colours = color_map_mod) + # Hawre's jet map
      scale_fill_viridis(option="mako", direction = -1) +
      theme_ipsum(base_size = 18,
                  plot_title_size = 24,
                  subtitle_size = 16,
                  grid = FALSE) +
      facet_wrap( ~ scenario, nrow = 1, scales = "free") +
      theme(text = element_text(size = text_size + 8),
            axis.text.x = element_text(angle = 90,
                                       hjust = 0.5,
                                       vjust = 0.5,
                                       size = 16),
            axis.title.x = element_text(size = 16,
                                        hjust = 0.5,
                                        vjust = 0.5),
            axis.text.y = element_text(angle = 0,
                                       hjust = 0.5,
                                       vjust = 0.5,
                                       size = 16),
            axis.title.y = element_text(size = 16,
                                        hjust = 0.5,
                                        vjust = 0.5),
            panel.background = element_rect(fill = "white",
                                            colour = "gray",
                                            size = 0.15,
                                            linetype = "solid"),
            legend.position = "right",
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.key = element_rect(fill   = "transparent",
                                      colour = "transparent",
                                      size   = unit(7, "cm")))
    
    
  }
  
  return(plot_final)
  
}



#### Plot State-Specific Cancer Incidence  ####

#' \code{plot_incidence} Plot the State-Specific Cancer Incidence across scenarios.
#' 
#' 
#'
#' @param state Character specifying the abbreviation of the state of interest. 
#' @param period Numeric vector specifying the period of interest. The default period is the year 2006-2100.
#@param percs Logical statement specifying the Confidence Intervals (Upper Bound (UB) and Lower Bound (LB)).
#TRUE to get Confidence Intervals.
#' @param scenarios Character vector specifying different scenarios of interest. 
#' @param save_data Logical statement. TRUE to save the dataframe of the plot.
#' 
#' @import tidyverse
#' @import ggplot2
#' @import dplyr
#' 
#' 
#' 
#' @return
#'
#' @examples
#
#'
#' @export
#' 


plot_incidence <- function(state = "CA",
                           period = c(2006:2100),
                           scenarios = c("Pre-pandemic",
                                         "Covid-19 Backslide",
                                         "Healthy Initiative 5 yrs",
                                         "Healthy Initiative 10 yrs",
                                         "No vaccination"),
                           percs = TRUE,
                           save_data = FALSE){
  
  
  if(state == "US"){
    df_plot <- df_proj_prob_scn_all_sum %>% filter(subtype == "Aggreg") %>%
      filter(abbrev_ste == "" & Outcome == "Cervical Cancer Incidence")
  }else{
    df_plot <- df_proj_prob_scn_all_sum %>% filter(subtype == "Aggreg") %>%
      filter(abbrev_ste == state & Outcome == "Cervical Cancer Incidence")
  }
  
  df_plot$year <- as.numeric(df_plot$year)
  
  df_plot <- df_plot %>% filter(year %in% period,
                                scenario %in% scenarios)
  
  if(state=="US"){
    
    plot_title <- "United States"
    
  }else{
    
    plot_title <- state.name[which(state == state.abb)]
    
  }
  # Plot subtitle
  plot_subtitle <- "Cervical Cancer Incidence"
  
  # Plot
  
  
  plot_final <- ggplot(df_plot,
                       aes(x = year, y = value, color = scenario, group = scenario,
                       )) +
    geom_line(size = 1) +
    # scale_linetype_manual("", values = 1:5) +
    scale_color_manual("", values = scales::viridis_pal()(5)) +
    scale_fill_manual("", values = scales::viridis_pal()(5)) +
    scale_x_continuous("",
                       breaks = number_ticks(15)) +
    scale_y_continuous("Incidence per 100,000",
                       breaks = number_ticks(10)) +
    geom_vline(xintercept = c(2008, 2019), linetype = 2:3) +
    labs(title = plot_title,
         subtitle = plot_subtitle) +
    theme_ipsum(base_size = 18,
                plot_title_size = 24,
                subtitle_size = 16,
                grid = FALSE) + #facet_wrap( ~ state, nrow = 1) +
    theme(text = element_text(size = text_size + 8),
          axis.text.x = element_text(angle = 90,
                                     hjust = 0.5,
                                     vjust = 0.5,
                                     size = 16),
          axis.text.y = element_text(angle = 0,
                                     hjust = 0.5,
                                     vjust = 0.5,
                                     size = 16),
          axis.title.y = element_text(size = 16,
                                      hjust = 0.5,
                                      vjust = 0.5),
          panel.background = element_rect(fill = "white",
                                          colour = "gray",
                                          size = 0.15,
                                          linetype = "solid"),
          legend.position = "right",
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16),
          legend.key = element_rect(fill   = "transparent",
                                    colour = "transparent",
                                    size   = unit(7, "cm")))
  
  if(percs){
    
    plot_final <- plot_final + geom_ribbon(aes(fill = scenario, ymin = lb, ymax = ub),alpha = 0.5)
  }
  
  
  return(plot_final)
  
}


