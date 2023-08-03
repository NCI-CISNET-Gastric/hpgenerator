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

plot_foi <- function(age = c(0:100), 
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


plot_foi_shiny_v2 <- function(age = c(0:90), 
                           birthplace = c("US", "Foreign"), 
                           race = c("Hispanic", "NH Black", "NH White", "Other"), 
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
  
  df_foi_plot <- summary_foi_bc(ages = age, races=race,
                                status = birthplace, 
                                birthcohort = cohort)
  
  n_birthplace <- length(birthplace)
  
  n_race <- length(race)
  
  n_cohort <- length(cohort)
  
  levels_birthplace <- c("US", "Foreign")
  
  df_foi_plot$birthplace <- factor(df_foi_plot$birthplace, levels = levels_birthplace)
  
  df_foi_plot$age <- as.numeric(df_foi_plot$age)
  
  v_breaks <- unique(df_foi_plot$cohort)
  
  selected_indices <- seq(1, vector_length, length.out = 4)
  
  selected_values <- v_breaks[selected_indices]
  
  
  plot_temp <- ggplot(df_foi_plot %>% filter(age!=0), aes(y = foi_mean, x = age, group = cohort, color = cohort)) + 
    geom_line_interactive(aes(fill = cohort, data_id = cohort, tooltip = cohort), linewidth = 1) +   
    #geom_point(data = df_geom_point_cohorts, aes(y = prev, x = age, color = cohort, size = total), shape = 1, stroke = 1) +
    xlab("Age") + 
    ylab(expression(paste("Force of infection ", (lambda)))) +
    labs(color='Birth cohort') +
    scale_colour_gradientn(colours = jet.colors(100), breaks = selected_values) +
    scale_x_continuous(breaks = seq(0, last(age), by = age_bracket), 
                       labels = seq(0, last(age), by = age_bracket), 
                       limits = c(0, last(age))) +
    #scale_y_continuous(breaks = seq(0, 100, by = 20), 
    #                   labels = c("0%", "20%", "40%", "60%", "80%", "100%"),
    #                   limits = c(0, 100)) +
    theme_bw(base_size = 21) + 
    theme(legend.position = "bottom",
          #legend.box = "vertical",
          legend.title = element_text(vjust = .8),
          legend.key.size = unit(.7, 'cm'), 
          legend.spacing.x = unit(.1, 'cm'),
          legend.text = element_text(size = 10),
          strip.background = element_rect(fill = "transparent", color = "transparent"),
          strip.text = element_text(size = 25, face = "bold")) +
    #edit legends
    guides(
      #reverse color order (higher value on top)
      color = guide_colorbar(reverse = FALSE))

  
  if (n_birthplace==1 & n_race==1){
    plot_final <- plot_temp + labs(title = birthplace, subtitle = race)
  } else if (n_birthplace==2 & n_race==1){
    plot_final <- plot_temp + facet_grid(race~birthplace)
  } else {
    plot_final <- plot_temp + facet_grid(race~birthplace)
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

plot_foi_shiny_v2()





