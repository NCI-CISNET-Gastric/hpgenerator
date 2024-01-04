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
                              race =  c("Hispanic", "NH American Indian", "NH Asian", "NH Black", "NH White"), 
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
  
  plot_temp <- ggplot(df_foi_plot %>% filter(age!=0), aes(y = foi_mean, x = age, group = cohort, color = as.numeric(cohort))) + 
    geom_line_interactive(aes(data_id = cohort, tooltip = cohort), linewidth = .5) +   
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
    theme_bw(base_size = 21) + 
    theme(legend.position = c(.88, 0.2),
          #legend.box = "vertical",
          legend.title = element_text(vjust = .8),
          legend.key.size = unit(.7, 'cm'), 
          legend.spacing.x = unit(.1, 'cm'),
          legend.text = element_text(size = 10),
          strip.background = element_rect(fill = "transparent", color = "transparent"),
          strip.text = element_text(size = 15, face = "bold")) +
    #edit legends
    guides(
      #reverse color order (higher value on top)
      color = guide_colorbar(reverse = TRUE))
  
  
  if (n_race==1){
    plot_final <- plot_temp + labs(title = race)
  } else {
    plot_final <- plot_temp + facet_wrap(~race)
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
