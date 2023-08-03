#### Function Force of Infection ####

#' \code{generate_foi_linear_birth_cohort} get the Force of Infection (FOI) of the the Helicobacter pylori
#'  of the United States of America. 
#' @param age Numeric vector specifying the age of the person with
#' Helicobacter pylori
#' @param race Character vector specifying the race of the person 
#' (Non-Hispanic White, Non-Hispanic Black, Other Hispanic,, Other)
#' @param cohort Character vector specifying the period of interest of the FOI. The default period is the year 1991
#' @import tidyverse
#' @import data.table
#' @return A data.frame with the distributions of the alpha and gamma parameters according to the specified variables.
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


library(tidyverse)
library(data.table)
library(doParallel)
library(foreach)


source("R/anxiliary_functions.R")


load("shiny/19_HP_TwoLevel_Linear_output_MVNPrior_updatedData.RData")



param_prefixes <- c("alpha0", "alpha1", "gamma0", "gamma1")

for (x in 1:length(param_prefixes)) {
  for (y in 1:3) {
    var_name <- paste0(param_prefixes[x], "_", y)
    assign(var_name, t(fit_model[["par"]][, x, , y]))
  }
}

df_alpha0 <- data.frame(as.matrix(rbind(alpha0_1, alpha0_2, alpha0_3)))
names(df_alpha0) <- c("alpha0[1]", "alpha0[2]", "alpha0[3]", "alpha0[4]", "alpha0[5]")

df_alpha1 <- data.frame(as.matrix(rbind(alpha1_1, alpha1_2, alpha1_3)))
names(df_alpha1) <- c("alpha1[1]", "alpha1[2]", "alpha1[3]", "alpha1[4]", "alpha1[5]")

df_gamma0 <- data.frame(as.matrix(rbind(gamma0_1, gamma0_2, gamma0_3)))
names(df_gamma0) <- c("gamma0[1]", "gamma0[2]", "gamma0[3]", "gamma0[4]", "gamma0[5]")

df_gamma1 <- data.frame(as.matrix(rbind(gamma1_1, gamma1_2, gamma1_3)))
names(df_gamma1) <- c("gamma1[1]", "gamma1[2]", "gamma1[3]", "gamma1[4]", "gamma1[5]")



# Define a function to perform the repeated operations
convert_and_reshape <- function(mat, prefix) {
  df <- data.frame(as.matrix(mat))
  names(df) <- paste0(prefix, "[", 1:ncol(mat), "]")
  
  df_long <- df %>%
    tibble::rownames_to_column(var = "Row") %>%
    pivot_longer(cols = -Row, names_to = "Parameters", values_to = paste0("v_", prefix)) %>% 
    mutate(v_race = case_when(stringr::str_detect(Parameters, paste(c( "5]"), collapse = '|')) ~ "NH White", 
                              stringr::str_detect(Parameters, paste(c( "4]"), collapse = '|')) ~ "NH Black", 
                              stringr::str_detect(Parameters, paste(c( "3]"), collapse = '|')) ~ "NH Asian",
                              stringr::str_detect(Parameters, paste(c( "2]"), collapse = '|')) ~ "NH American Indian",
                              stringr::str_detect(Parameters, paste(c( "1]"), collapse = '|')) ~ "Hispanic"))%>% 
    mutate(v_race = factor(v_race, levels = c("Hispanic", "NH American Indian", "NH Asian", "NH Black", "NH White")))  %>% 
    arrange(v_race)
  
  return(df_long)
}

# Now call this function for each matrix
df_alpha0_long <- convert_and_reshape(rbind(alpha0_1, alpha0_2, alpha0_3), "alpha0")
df_alpha1_long <- convert_and_reshape(rbind(alpha1_1, alpha1_2, alpha1_3), "alpha1") %>% select(v_alpha1)
df_gamma0_long <- convert_and_reshape(rbind(gamma0_1, gamma0_2, gamma0_3), "gamma0")
df_gamma1_long <- convert_and_reshape(rbind(gamma1_1, gamma1_2, gamma1_3), "gamma1") %>% select(v_gamma1)



df_alpha_up <- bind_cols(df_alpha0_long, df_alpha1_long) %>% select(v_race, v_alpha0,v_alpha1)
df_gamma_up <- bind_cols(df_gamma0_long, df_gamma1_long) %>% select(v_race, v_gamma0,v_gamma1)


options(max.print=1000)

generate_foi_linear_birth_cohort <- function(df_alpha_up,
                                             df_gamma_up,
                                             age = c(0:90), 
                                             race = c("Hispanic", "NH American Indian", "NH Asian", "NH Black", "NH White"), 
                                             cohort = 1908:1998) {
  
  if (!all(age %in% c(0:90))) {
    stop("age must be a number or numeric vector between 0 and 90 years")
  }
  
  match.arg(race)
  if (!all(race %in% c("Hispanic", "NH American Indian", "NH Asian", "NH Black", "NH White"))) {
    stop("Race must be a character or vector with the next races: Hispanic, 
         NH American Indian, NH Asian, NH Black, NH White")
  }
  
  options(dplyr.summarise.inform = FALSE)
  
  levels_race <- c("Hispanic", "NH American Indian", "NH Asian", "NH Black", "NH White")
  
  race <- factor(race,levels = levels_race)
  
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
  
  df_gamma <- df_gamma_up %>%
    dplyr::filter(v_race %in% race)
  
  v_gamma0 = df_gamma$v_gamma0
  v_gamma1 = df_gamma$v_gamma1
  
  v_num_cohort <- as.numeric(cohort)-1908
  
  df_estimates <- data.frame(NULL)
  
    no_cores <- parallel::detectCores() - 7   # detect number of cores
    cl <- parallel::makeCluster(no_cores)      # initialize cluster object
    doParallel::registerDoParallel(cl)
    
    df_final <- foreach::foreach (cohort_i = seq_along(v_num_cohort), .combine = rbind) %do% { #period_i = 1
      num_cohort = v_num_cohort[cohort_i]
      v_alpha_est <- ilogit(v_alpha0 + v_alpha1*num_cohort) 
      v_gamma_est <- exp(v_gamma0 + v_gamma1*num_cohort)
      v_age <- matrix(age, nrow = 1)
      
      m_foi_hat <- ((v_alpha_est*v_gamma_est) * exp(-(v_gamma_est %*% v_age)))/(1-v_alpha_est*(1-exp(-(v_gamma_est %*% v_age))))
      
      colnames(m_foi_hat) <- paste0("age_",as.character(age))
      
      df_foi_hat <- data.frame(sim = 1:3000,
                               race = rep(race, each = 3000),
                               cohort = as.numeric(cohort[cohort_i]),
                               m_foi_hat,
                               check.names = F)# %>% melt(id.vars = c("sim", "race", "cohort", "birthplace"))
      
      #m_estimates <- rbind(m_estimates, m_foi_hat)
      #df_estimates <- rbind.data.frame(df_estimates, df_foi_hat)
      
      df_foi_hat
    }
    parallel::stopCluster(cl)
    
    df_estimates <- rbind.data.frame(df_estimates, df_final)
    
  return(df_estimates)
}

start.time <- Sys.time()

prueba_shiny <- generate_foi_linear_birth_cohort(df_alpha_up = df_alpha_up, df_gamma_up = df_gamma_up)

end.time <- Sys.time()  # End the timer

# Print the time taken
print(end.time - start.time)



summary_foi_v3 <- function(age = c(0:90), 
                           race = c("Hispanic", "NH American Indian", "NH Asian", "NH Black", "NH White"), 
                           cohort = 1908:1998){
  
  options(dplyr.summarise.inform = FALSE)
  
  df_foi_hat <- generate_foi_linear_birth_cohort(df_alpha_up, df_gamma_up, age = age, race = race, cohort = cohort)
  
  LB <- 0.025
  UB <- 0.975
  
  df_foi_long <- melt(as.data.table(df_foi_hat), id.vars = c("sim", "race", "cohort"))
  
  df_foi_final <- df_foi_long[, .(
    foi_mean = mean(value),
    foi_cri_lb = quantile(value, probs = LB, na.rm = TRUE),
    foi_cri_ub = quantile(value, probs = UB, na.rm = TRUE)
  ), by = .(race, cohort, variable)]
  
  df_foi_final <- df_foi_final[, age := as.numeric(str_remove(variable, "age_"))]
  
  df_foi_final[, variable := NULL]
  return(df_foi_final)
}

start.time <- Sys.time()

dt_foi_birth_cohort_all <- summary_foi_v3()


end.time <- Sys.time()  # End the timer

# Print the time taken
print(end.time - start.time)


jet.colors <- colorRampPalette(c("black", "#00007F", "blue", "#007FFF",
                                        "cyan", "#7FFF7F", "yellow", "#FF7F00",
                                        "red", "#7F0000"))
                                        
color_map  <-  jet.colors(17)

colors_5 <- RColorBrewer::brewer.pal(name = "Set1", n = 5)



ggplot(prueba_summary %>% filter(age!=0, cohort >= 1940), aes(y = foi_mean, x = age, group = cohort, color = cohort)) + 
  geom_line() +   
  #geom_point(data = df_geom_point_cohorts, aes(y = prev, x = age, color = cohort, size = total), shape = 1, stroke = 1) +
  facet_wrap(~ race) +
  xlab("Age") + 
  ylab("Force of infection") +
  labs(color='Birth cohort') +
  scale_colour_gradientn(colours = rev(jet.colors(100)), breaks = c(1940, 1960, 1980, 1998))+
  scale_x_continuous(breaks = seq(0, 90, by = 20), 
                     labels = seq(0, 90, by = 20), 
                     limits = c(0, 90)) +
  scale_y_continuous(breaks = seq(0, .15, by = .05), 
                     #labels = c("0%", "20%", "40%", "60%", "80%", "100%"),
                     limits = c(0, .15)) +
  theme_bw(base_size = 16) + 
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


ggsave(file = paste0(path,"19.01_TwoLevel_MVNPrior_FOI_USA_by_race_UD.png"),
       width = 8, height = 7, bg = "transparent", limitsize = FALSE)



save(df_foi_birth_cohort_all, file="df_foi_birth_cohort_all.Rda")


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
  
  selected_values <- v_breaks[selected_indices]
  
  
  plot_temp <- ggplot(df_foi_plot %>% filter(age!=0), aes(y = foi_mean, x = age, group = cohort, color = cohort)) + 
    geom_line_interactive(aes(data_id = cohort, tooltip = cohort), linewidth = .5) +   
    #geom_point(data = df_geom_point_cohorts, aes(y = prev, x = age, color = cohort, size = total), shape = 1, stroke = 1) +
    xlab("Age") + 
    ylab(expression(paste("Force of infection ", (lambda)))) +
    labs(color='Birth cohort') +
    scale_colour_gradientn(colours = rev(jet.colors(100)), breaks = selected_values) +
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













