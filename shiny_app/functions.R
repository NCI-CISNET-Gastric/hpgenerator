load(file = "df_foi_birth_cohort_all.Rda")
load(file = "19.01_df_parameters_birthcohort_FOI_HP.RData")
load(file = "19.01_df_p_hat_race_final_cohort.Rdata")

library(data.table)
library(tidyverse)
library(ggiraph)


summary_foi_bc <- function(ages = c(0:90), 
                           races = c("Hispanic", "NH American Indian", "NH Asian", "NH Black", "NH White"), 
                           birthcohort = 1908:1998) {
  
  options(dplyr.summarise.inform = FALSE)
  
  dt_estimates_final <- df_foi_birth_cohort_all[race %in% races & cohort %in% birthcohort & age %in% ages, ]
  
  return(dt_estimates_final)
}







#### Plot the Force of Infection (Shiny Specifications) ####

#' \code{plot_foi_shiny_v2} Plot the Force of Infection (FOI)
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



plot_foi_shiny_v2 <- function(age = c(0:90), 
                              race =  c("Hispanic", "NH American Indian","NH Asian", "NH Black", "NH White"), 
                              cohort = 1940:1998, 
                              age_bracket = 20,
                              save_data = F,
                              save_plot = F,
                              plot_name = "FOI",
                              data_name = "df_foi",
                              width = 8,
                              height = 7) {
  
  jet.colors <- colorRampPalette(c("black", "#00007F", "blue", "#007FFF",
                                   "cyan", "#7FFF7F", "yellow", "#FF7F00",
                                   "red", "#7F0000"))
  
  color_map  <-  jet.colors(100)
  
  options(dplyr.summarise.inform = FALSE)
  
  df_foi_plot <- summary_foi_bc(ages = age, races = race, birthcohort = cohort)
  
  n_race <- length(race)
  
  n_cohort <- length(cohort)
  
  df_foi_plot$age <- as.numeric(df_foi_plot$age)
  
  v_breaks <- unique(df_foi_plot$cohort)
  
  selected_indices <- seq(1, n_cohort, length.out = 4)
  
  if (n_cohort<=6) {
    selected_indices <- seq(1, n_cohort, length.out = 2)
  }
  
  selected_values <- v_breaks[selected_indices]
  
  all_cohorts <- 1940:1998
  all_colors <- rev(jet.colors(length(all_cohorts))) # Reverse the colors here
  color_df <- data.frame(cohort = all_cohorts, color = all_colors)
  df_foi_plot <- merge(df_foi_plot, color_df, by = "cohort", all.x = TRUE)
  final_colors <- unique(df_foi_plot$color)
  
  plot_temp <- ggplot(df_foi_plot %>% filter(age!=0), aes(y = foi_mean, x = age, group = cohort, color = cohort)) + 
    geom_line_interactive(aes(data_id = cohort, color =cohort, tooltip = paste0("Birth cohort:  ",cohort)), linewidth = .5) +   
    #geom_point(data = df_geom_point_cohorts, aes(y = prev, x = age, color = cohort, size = total), shape = 1, stroke = 1) +
    xlab("Age") + 
    ylab(expression(paste("Force of infection ", (lambda)))) +
    labs(color='Birth cohort') +
    scale_color_gradientn(colors = final_colors, breaks = selected_values) +
    scale_x_continuous(breaks = seq(0, last(age), by = age_bracket), 
                       labels = seq(0, last(age), by = age_bracket), 
                       limits = c(0, last(age))) +
    scale_y_continuous(breaks = seq(0, .15, by = .05), 
                       #labels = c("0%", "20%", "40%", "60%", "80%", "100%"),
                       limits = c(0, .15)) +
    theme_bw(base_size = 21)
  
  
  if (n_race<=1){
    plot_final <- plot_temp + labs(title = race) +
      theme(legend.position = "bottom",
            #legend.box = "vertical",
            legend.title = element_text(vjust = .8),
            legend.key.size = unit(1, 'cm'), 
            legend.spacing.x = unit(.5, 'cm'),
            #legend.spacing.y = unit(.5, 'cm'),
            legend.text = element_text(size = 16),
            strip.background = element_rect(fill = "transparent", color = "transparent"),
            strip.text = element_text(size = 30, face = "bold")) +
      #edit legends
      guides(
        #reverse color order (higher value on top)
        color = guide_colorbar(reverse = FALSE))
  } 
  
  else if (n_race<=4){
    plot_final <- plot_temp + facet_wrap(~race) +
      theme(legend.position = "bottom",
            #legend.box = "vertical",
            legend.title = element_text(vjust = .8),
            legend.key.size = unit(1, 'cm'), 
            legend.spacing.x = unit(.5, 'cm'),
            #legend.spacing.y = unit(.5, 'cm'),
            legend.text = element_text(size = 16),
            strip.background = element_rect(fill = "transparent", color = "transparent"),
            strip.text = element_text(size = 30, face = "bold")) +
      #edit legends
      guides(
        #reverse color order (higher value on top)
        color = guide_colorbar(reverse = FALSE))
  }
  
  else {
    plot_final <- plot_temp + facet_wrap(~race) +
      theme(legend.position = c(.88, 0.2),
            #legend.box = "vertical",
            legend.title = element_text(vjust = .8),
            legend.key.size = unit(1, 'cm'), 
            legend.spacing.x = unit(.5, 'cm'),
            legend.text = element_text(size = 16),
            strip.background = element_rect(fill = "transparent", color = "transparent"),
            strip.text = element_text(size = 30, face = "bold")) +
      #edit legends
      guides(
        #reverse color order (higher value on top)
        color = guide_colorbar(reverse = TRUE))
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


#' Logit function
#'
#' \code{logit} computes the log odds of a probability \code{p}.
#'
#' @param p probability
#' @return
#' The log-odds of \code{p}.
#' @export
logit <- function(p){
  if(p < 0 | p > 1){
    stop("The probability parameter `p` should be [0, 1]")
  }
  
  x <- log(p/(1- p))
  
  return(x)
}

#' Inverse logit unction
#'
#' \code{ilogit} computes the probability \code{p} from a log-odds.
#'
#' @param logodds log-odds
#' @return
#' The probability of log-odds.
#' @export
ilogit <- function(logodds){
  p <- 1/(1 + exp(-logodds))
  
  return(p)
}


alpha0 <- df_parameters_birthcohort_19_01[1:5,"Mean"]
alpha1 <- df_parameters_birthcohort_19_01[6:10,"Mean"]
gamma0 <- df_parameters_birthcohort_19_01[11:15,"Mean"]
gamma1 <- df_parameters_birthcohort_19_01[16:20,"Mean"]





#' Get parameters for constrained exponential function
#'
#' \code{get_parameters_constrained_exponential} implements the inverse
#' constrained exponential function.
#'
#' @param cohort Year of birth cohort.
#' @param race Race in numeric categories.
#' @param alpha0
#' @param alpha1
#' @param gamma0
#' @param gamma1
#' @return
#' The time at which the event happens.
#' 
#' 
#' @export

get_parameters_constrained_exponential <- function(birth_cohort, race,
                                                   alpha0, alpha1,
                                                   gamma0, gamma1){
  l_parameters <- list()
  
  for(i in seq_along(birth_cohort)) {
    for(j in seq_along(race)) {
      bc <- birth_cohort[i] - 1908
      par_alpha <- ilogit(alpha0[race[j]] + alpha1[race[j]]*bc)
      par_gamma <- exp(gamma0[race[j]] + gamma1[race[j]]*bc)
      l_parameters[[paste0("Cohort_", birth_cohort[i], "_Race_", race[j])]] <- list(par_alpha = par_alpha, par_gamma = par_gamma)
    }
  }
  return(l_parameters)
}




#' Get the age to infection function
#'
#' \code{age_to_infection} use PDF of the force of infection and function get_parameters_constrained_exponential to construct the age to infection.
#'
#' @param cohort Year of birth cohort.
#' @param race Race in numeric categories.
#' @param alpha0 Vector of alpha0 parameters according to the race.
#' @param alpha1 Vector of alpha1 parameters according to the race.
#' @param gamma0 Vector of gamma0 parameters according to the race.
#' @param gamma1 Vector of gamma1 parameters according to the race.
#' @param size Numeric vector with the size of the population.
#' @return A dataframe with the Age to Infection according to the specified variables by the user.
#' 
#' 
#' @examples
#' age_to_infection(df = df_p_hat_race_final_cohort, v_cohort = c(1940), v_race = c(1:5), alpha0 = alpha0, alpha1 = alpha1, gamma0 = gamma0, gamma1 = gamma1, size = 100000)
#' 
#' age_to_infection(df = df_p_hat_race_final_cohort, v_cohort = c(1940:1998), v_race = c(1), alpha0 = alpha0, alpha1 = alpha1, gamma0 = gamma0, gamma1 = gamma1, size = 100000)
#'
#'
#'
#' @export
#' 



age_to_infection <- function(df, cohort = 1940, race = 5, alpha0, alpha1, gamma0, gamma1, size) {
  dt <- as.data.table(df)
  dt_pop <- data.table()
  
  v_parameters <- get_parameters_constrained_exponential(birth_cohort = cohort, race = race, alpha0 = alpha0, alpha1 = alpha1, gamma0 = gamma0, gamma1 = gamma1)
  
  for(birthcohort in cohort) {
    for(races in race) {
      dt_races <- dt[cohort == birthcohort & as.numeric(race_fac) %in% races, .(age, Mean)]
      dt_races[, PDF := Mean - shift(Mean, fill = 0)]
      
      par_alpha <- v_parameters[[paste0("Cohort_", birthcohort, "_Race_", races)]]$par_alpha
      
      has_event <- rbinom(n = size, size = 1, prob = par_alpha)
      
      time_event <- rep(NaN, length = size)
      
      time_event[has_event == 1] <- sample(x = c(0:90), size = sum(has_event), replace = T, prob = dt_races[, PDF])
      
      dt_pop_temp <- data.table(cohort = birthcohort, race = races, id = 1:length(time_event), has_event, age_to_infection = time_event)
      dt_pop <- rbind(dt_pop, dt_pop_temp)
    }
  }
  
  dt_pop[, race_fac := fifelse(race == 1, "Hispanic",
                               fifelse(race == 2, "NH American Indian",
                                       fifelse(race == 3, "NH Asian",
                                               fifelse(race == 4, "NH Black", 
                                                       fifelse(race == 5, "NH White", NA_character_)))))] 
  
  setcolorder(dt_pop, c("id", "cohort","race_fac", "race", "has_event", "age_to_infection"))
  
  return(dt_pop)
}




plot_age_to_infection <- function(size = 100, 
                                  race =  c(1:5), 
                                  cohort = c(1940:1998)) {
  
  race_titles <- c("Hispanic", "NH American Indian", "NH Asian", "NH Black", "NH White")
  names(race_titles) <- c(1:5)
  
  title_race <- paste(race_titles[as.character(race)], collapse = ", ")
  
  
  jet.colors <- colorRampPalette(c("black", "#00007F", "blue", "#007FFF",
                                          "cyan", "#7FFF7F", "yellow", "#FF7F00",
                                          "red", "#7F0000"))
                                          
  color_map  <-  jet.colors(100)
  
  options(dplyr.summarise.inform = FALSE)
  
  dt_ati <- age_to_infection(df = df_p_hat_race_final_cohort, cohort = cohort, race = race, alpha0 = alpha0, alpha1 = alpha1, gamma0 = gamma0, gamma1 = gamma1, size = size)
  
  
  # Transform the data
  dt_races_all_transformed <- dt_ati[
    , age := ifelse(is.na(age_to_infection), 0, age_to_infection)
  ][
    , .(infected_count = sum(has_event)), by = .(cohort, race, age)
  ][
    , proportion_infected := infected_count / size
  ][
    , race_fac := fifelse(race == 1, "Hispanic",
                          fifelse(race == 2, "NH American Indian",
                                  fifelse(race == 3, "NH Asian",
                                          fifelse(race == 4, "NH Black", 
                                                  fifelse(race == 5, "NH White", NA_character_)))))
  ]
  
  
  n_race <- length(race)
  
  n_age <- length(unique(dt_races_all_transformed$age))
  
  #df_foi_plot$age <- as.numeric(df_foi_plot$age)
  
  v_breaks <- unique(dt_races_all_transformed$age_to_infection)
  
  selected_indices <- seq(1, n_age, length.out = 4)
  
  selected_values <- v_breaks[selected_indices]
  
  
  
  # Creating the plot
  plot_temp <- ggplot(dt_races_all_transformed, aes(x = cohort, y = proportion_infected, fill = age, group = race_fac)) +
    geom_bar_interactive(stat = "identity", position = "dodge", aes(fill = age, data_id = age, tooltip = age)) +
    #geom_bar() +
    scale_fill_gradientn(name = "Age \n to \n infection", colours = rev(jet.colors(100))) +
    #scale_y_continuous(breaks = seq(0, .4, by = .1), 
    #                   labels = seq(0, .4, by = .1)) +
    scale_x_continuous(breaks = seq(first(cohort), last(cohort), by = 10), 
                       labels = seq(first(cohort), last(cohort), by = 10), 
                       limits = c(first(cohort), last(cohort))) +
    labs(title = "Proportion of the population that eventually gets infected ",
         x = "Birth Cohort",
         y = "Proportion") + 
    theme_bw(base_size = 20)
  
  
  if (n_race==1){
    plot_final <- plot_temp + labs(subtitle = title_race) +
      theme(legend.position = "bottom",
            #legend.box = "vertical",
            legend.title = element_text(vjust = .8, hjust = 0.5, size = 18), # Updated this line        legend.key.size = unit(.7, 'cm'), 
            legend.spacing.y = unit(.5, 'cm'),
            legend.key.size = unit(1, 'cm'), # Adjust key size
            #panel.spacing.y = unit(1, "cm"),
            #panel.spacing.x = unit(.5, "cm"),
            legend.text = element_text(size = 20),
            strip.background = element_rect(fill = "transparent", color = "transparent"),
            axis.text.x = element_text(angle = 45, margin = margin(t = 10)),
            strip.text = element_text(size = 20, face = "bold")) +
      
      #edit legends
      guides(
        #reverse color order (higher value on top)
        fill = guide_colorbar(reverse = FALSE))
    
  } else if (n_race>1 & n_race<=4){
    
    plot_final <- plot_temp + facet_wrap(~race_fac, scales = "free_x") +
      theme(legend.position = "bottom",
            #legend.box = "vertical",
            legend.title = element_text(vjust = .8, hjust = 0.5, size = 18), # Updated this line        legend.key.size = unit(.7, 'cm'), 
            legend.spacing.y = unit(.5, 'cm'),
            #panel.spacing.y = unit(.1, "cm"),
            #panel.spacing.x = unit(.5, "cm"),
            legend.key.size = unit(1, 'cm'), # Adjust key size
            legend.text = element_text(size = 20),
            strip.background = element_rect(fill = "transparent", color = "transparent"),
            axis.text.x = element_text(angle = 45, margin = margin(t = 10)),
            strip.text = element_text(size = 20, face = "bold")) +
      
      #edit legends
      guides(
        #reverse color order (higher value on top)
        fill = guide_colorbar(reverse = FALSE))
  } else{
    
    plot_final <- plot_temp + facet_wrap(~race_fac, scales = "free_x") +
      theme(legend.position = c(.88, 0.2),
            #legend.box = "vertical",
            legend.title = element_text(vjust = .8, hjust = 0.5, size = 18), # Updated this line        legend.key.size = unit(.7, 'cm'), 
            legend.spacing.y = unit(.5, 'cm'),
            legend.key.size = unit(1, 'cm'), # Adjust key size
            #panel.spacing.y = unit(1, "cm"),
            #panel.spacing.x = unit(.5, "cm"),
            legend.text = element_text(size = 16),
            strip.background = element_rect(fill = "transparent", color = "transparent"),
            axis.text.x = element_text(angle = 45, margin = margin(t = 10)),
            strip.text = element_text(size = 20, face = "bold")) +
      
      #edit legends
      guides(
        #reverse color order (higher value on top)
        fill = guide_colorbar(reverse = TRUE))
    
  }
  
  
  return(plot_final)
  
  
}




plot_age_to_infection_grid <- function(size = 100, 
                                  race =  c(1:5), 
                                  cohort = c(1940:1998)) {
  
  race_titles <- c("Hispanic", "NH American Indian", "NH Asian", "NH Black", "NH White")
  names(race_titles) <- c(1:5)
  
  title_race <- paste(race_titles[as.character(race)], collapse = ", ")
  
  
  jet.colors <- colorRampPalette(c("black", "#00007F", "blue", "#007FFF",
                                   "cyan", "#7FFF7F", "yellow", "#FF7F00",
                                   "red", "#7F0000"))
  
  color_map  <-  jet.colors(100)
  
  options(dplyr.summarise.inform = FALSE)
  
  dt_ati <- age_to_infection(df = df_p_hat_race_final_cohort, cohort = cohort, race = race, alpha0 = alpha0, alpha1 = alpha1, gamma0 = gamma0, gamma1 = gamma1, size = size)
  
  
  # Preparing the data: filtering out NA values in age_to_infection
  dt_ati_hm <- dt_ati[!is.na(age_to_infection)]
  
  # Creating a new data.table for the heatmap
  # Counting the occurrences of age_to_infection for each cohort and race_fac combination
  dt_hm <- dt_ati_hm[, .(people = .N), by = .(cohort, race_fac, age_to_infection)]
  
  n_cohort <- length(cohort)
  
  n_race <- length(race)
  
  n_age <- length(unique(dt_hm$age))
  
  #df_foi_plot$age <- as.numeric(df_foi_plot$age)
  
  v_breaks <- unique(dt_hm$age_to_infection)
  
  selected_indices <- seq(1, n_age, length.out = 4)
  
  selected_values <- v_breaks[selected_indices]
  
  
  
  # Creating the plot
  plot_final <- ggplot(dt_hm, aes(x = factor(cohort), y = age_to_infection, fill = people)) + 
    geom_tile_interactive(aes(tooltip = paste0("Birth cohort:  ",cohort, "<br>Age at infection:  ",age_to_infection, "<br>People:  ",people)), size = 1) +
    #geom_bar() +
    scale_fill_gradientn(name = "People", colours = jet.colors(100)) +
    #scale_x_continuous(breaks = cohort, 
    #                   labels = cohort) +
    #scale_y_continuous(breaks = seq(0, .4, by = .1), 
    #                   labels = seq(0, .4, by = .1)) +
    #scale_x_continuous(breaks = seq(first(cohort), last(cohort), by = 10), 
    #                   labels = seq(first(cohort), last(cohort), by = 10), 
    #                   limits = c(first(cohort), last(cohort))) +
    labs(title = "Population that eventually gets infected ",
         x = "Birth Cohort",
         y = "Age") + 
    theme_bw(base_size = 20)
  
 if (n_cohort>7 & n_cohort<=40){
    
    plot_final <- plot_final + scale_x_discrete(breaks = seq(min(dt_hm$cohort), max(dt_hm$cohort), by = 5), 
                                                labels = seq(min(dt_hm$cohort), max(dt_hm$cohort), by = 5))  +
      theme(axis.text.x = element_text(angle = 45, margin = margin(t = 10)))
  
 } else{
    
    plot_final <- plot_final + scale_x_discrete(breaks = seq(min(dt_hm$cohort), max(dt_hm$cohort), by = 10), 
                                                labels = seq(min(dt_hm$cohort), max(dt_hm$cohort), by = 10))  +
      theme(axis.text.x = element_text(angle = 45, margin = margin(t = 10)))
  }
  
  if (n_race==1){
    plot_final <- plot_final + labs(subtitle = title_race) +
      theme(legend.position = "bottom",
            #legend.box = "vertical",
            legend.title = element_text(vjust = .8, hjust = 0.5, size = 18), # Updated this line        legend.key.size = unit(.7, 'cm'), 
            legend.spacing.y = unit(.5, 'cm'),
            legend.key.size = unit(1, 'cm'), # Adjust key size
            legend.key.width = unit(2, 'cm'), # Adjust key width
            #panel.spacing.y = unit(1, "cm"),
            #panel.spacing.x = unit(.5, "cm"),
            legend.text = element_text(size = 20),
            strip.background = element_rect(fill = "transparent", color = "transparent"),
            axis.text.x = element_text(angle = 45, margin = margin(t = 10)),
            strip.text = element_text(size = 20, face = "bold")) +
      
      #edit legends
      guides(
        #reverse color order (higher value on top)
        fill = guide_colorbar(reverse = FALSE))
    
  } else if (n_race>1 & n_race<=4){
    
    plot_final <- plot_final + facet_wrap(~race_fac, scales = "free_x") +
      theme(legend.position = "bottom",
            #legend.box = "vertical",
            legend.title = element_text(vjust = .8, hjust = 0.5, size = 18), # Updated this line        legend.key.size = unit(.7, 'cm'), 
            legend.spacing.y = unit(.5, 'cm'),
            #panel.spacing.y = unit(.1, "cm"),
            #panel.spacing.x = unit(.5, "cm"),
            legend.key.size = unit(1, 'cm'), # Adjust key size
            legend.key.width = unit(2, 'cm'), # Adjust key width
            legend.text = element_text(size = 20),
            strip.background = element_rect(fill = "transparent", color = "transparent"),
            axis.text.x = element_text(angle = 45, margin = margin(t = 10)),
            strip.text = element_text(size = 20, face = "bold")) +
      
      #edit legends
      guides(
        #reverse color order (higher value on top)
        fill = guide_colorbar(reverse = FALSE))
  } else{
    
    plot_final <- plot_final + facet_wrap(~race_fac, scales = "free_x") +
      theme(legend.position = c(.88, 0.2),
            #legend.box = "vertical",
            legend.title = element_text(vjust = .8, hjust = 0.5, size = 18), # Updated this line        legend.key.size = unit(.7, 'cm'), 
            legend.spacing.y = unit(.5, 'cm'),
            legend.key.size = unit(1, 'cm'), # Adjust key size
            #panel.spacing.y = unit(1, "cm"),
            #panel.spacing.x = unit(.5, "cm"),
            legend.text = element_text(size = 16),
            strip.background = element_rect(fill = "transparent", color = "transparent"),
            axis.text.x = element_text(angle = 45, margin = margin(t = 10)),
            strip.text = element_text(size = 20, face = "bold")) +
      
      #edit legends
      guides(
        #reverse color order (higher value on top)
        fill = guide_colorbar(reverse = FALSE))
    
  }
  
  
  return(plot_final)
  
  
}

