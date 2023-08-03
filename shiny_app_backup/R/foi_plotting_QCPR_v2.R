#### Plot the Force of Infection ####

#' \code{plot_foi} Plot the Force of Infection (FOI)
#' according to the variables specified by the user. 
#' 
#'
#' @param age Numeric vector specifying the age of the person with
#' Helicobacter pylori
#' @param race Character vector specifying the race of the person 
#' (Non-Hispanic White, Non-Hispanic Black, Other Hispanic, Mexican-American, Other)
#' @param cohort Character vector specifying the period of interest of the FOI. The default period is the year 1908
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
#' plot_foi(age = c(1,5,80), race = "Hispanics", 
#' cohort = "1940", percs = F, age_bracket = 5, save_data = F, save_plot = T,
#' plot_name = "Force of Infection", data_name = "df_force_infection", 
#' width = 14, height = 10)
#'
#' plot_foi(age = 90, race = c("Hispanic", "NH American Indian", "NH Asian", "NH Black", "NH White"), 
#' cohort = c(1950, 1978, 1990))
#'
#' @export
#' 

plot_foi_v2 <- function(age = c(0:90), 
                     race = c("Hispanic", "NH American Indian", 
                              "NH Asian", "NH Black", "NH White"), 
                     cohort = 1908:1998, 
                     age_bracket = 20,
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
  
  df_foi_plot <- summary_foi_v2(age = age, race=race, cohort = cohort, percs = T)
  
  
  n_race <- length(race)
  
  n_cohort <- length(cohort)
  

  plot_temp <-ggplot(df_foi_plot %>% filter(age!=0), aes(y = foi_mean, x = age, group = cohort, 
                                       color = cohort)) + 
    geom_line()+
    xlab("Age") + ylab(expression(paste("Force of infection ", (lambda)))) +
    scale_x_continuous(breaks = seq(0, last(age), by = age_bracket), 
                       labels = seq(0, last(age), by = age_bracket), 
                       limits = c(0, last(age))) +
    scale_colour_gradientn(colours = jet.colors(100), breaks = seq(1908,1998, 18)) +
    scale_fill_gradientn(colours = jet.colors(100), breaks = seq(1908,1998, 18)) +
    theme_bw(base_size = 16)
  if (n_race<=4){
    plot_final <- plot_temp + 
      theme(legend.position = "bottom",
            legend.key.width= unit(1.2, 'cm'),
            legend.box = "vertical",
            legend.title = element_blank(),
            strip.background = element_rect(fill = "white",
                                            color = "white"),
            strip.text = element_text(size = 15, face = "bold"))+
      scale_size(guide = "none") 
  } else {
    plot_final <- plot_temp + labs(title = race) + 
                 theme(legend.position = c(.88, 0.2),
                         legend.title = element_text(vjust = .8),
                         legend.key.size = unit(.7, 'cm'), 
                         legend.spacing.x = unit(.1, 'cm'),
                         legend.text = element_text(size = 10),
                         strip.background = element_rect(fill = "transparent", color = "transparent"),
                         strip.text = element_text(size = 15, face = "bold")) +
          guides(color = guide_colorbar(reverse = TRUE)) + facet_wrap(~race)
    
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


plot_foi_v2()




