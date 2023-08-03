


plot_age_to_infection <- function(size = 100, 
                                  race =  c(1), 
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
    , proportion_infected := infected_count / 10000
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









plot_age_to_infection(size = 100, race = c(1:2), cohort = c(1940:1998))






df_whites_1940 <- age_to_infection(df = df_p_hat_race_final_cohort, cohort = c(1940), race = c(5), alpha0 = alpha0, alpha1 = alpha1, gamma0 = gamma0, gamma1 = gamma1, size = 100000)



jet.colors <- colorRampPalette(c("black", "#00007F", "blue", "#007FFF",
                                        "cyan", "#7FFF7F", "yellow", "#FF7F00",
                                        "red", "#7F0000"))
                                        
color_map  <-  jet.colors(17)


library(dplyr)

df_races_all_transformed <- df_races_all %>%
  mutate(age = ifelse(is.na(age_to_infection), 0, age_to_infection)) %>% 
  group_by(cohort, race, age) %>%
  summarise(infected_count = sum(has_event)) %>%
  mutate(proportion_infected = infected_count / 10000) %>%
  ungroup() %>% 
  mutate(race_fac = factor(case_when(race == 1 ~ "Hispanic",
                                     race == 2 ~ "NH American Indian",
                                     race == 3 ~ "NH Asian",
                                     race == 4 ~ "NH Black", 
                                     race == 5 ~ "NH White")))




library(ggplot2)
library(RColorBrewer)
library(ggiraph)



# Creating the plot
p <- ggplot(df_races_all_transformed, aes(x = cohort, y = proportion_infected, fill = age)) +
  geom_bar_interactive(stat = "identity", position = "dodge", aes(fill = age, data_id = age, tooltip = age)) +
  #geom_bar() +
  facet_wrap(~ race_fac, scales = "free_x") +
  scale_fill_gradientn(name = "Age \n to \n infection", colours = rev(jet.colors(100))) +
  #scale_x_continuous(breaks = seq(0, 90, by = 10), 
  #                   labels = seq(0, 90, by = 10), 
  #                   limits = c(0, 90)) +
  labs(title = "Proportion of the population that eventually gets infected ",
       x = "Age",
       y = "Proportion") + 
  theme_bw(base_size = 16) +
  theme(legend.position = c(.88, 0.2),
        #legend.box = "vertical",
        legend.title = element_text(vjust = .8, hjust = 0.5), # Updated this line        legend.key.size = unit(.7, 'cm'), 
        legend.spacing.x = unit(.1, 'cm'),
        panel.spacing.y = unit(1, "cm"),
        panel.spacing.x = unit(.5, "cm"),
        legend.text = element_text(size = 20),
        strip.background = element_rect(fill = "transparent", color = "transparent"),
        strip.text = element_text(size = 15, face = "bold")) +
  #edit legends
  guides(
    #reverse color order (higher value on top)
    fill = guide_colorbar(reverse = TRUE))


ggiraph(code = print(p))

girafe(
  ggobj = p,
  options = list(
    opts_hover(css = ''),
    opts_sizing(rescale = FALSE),
    opts_hover_inv(css = "opacity:0.1;"),
    opts_tooltip(opacity = .7, use_fill = TRUE, use_stroke = TRUE),
    opts_zoom(min = .5, max = 4)
    #sizingPolicy(defaultWidth = "100%", defaultHeight = "100%")
  ),
  width_svg = 8,
  height_svg = 9
) 





load(file = "shiny/19.01_df_parameters_birthcohort_FOI_HP.RData")

load(file = "shiny/19.01_df_p_hat_race_final_cohort.Rdata")


save(df_p_hat_race_final_cohort, file= "data/19.01_df_p_hat_race_final_cohort.Rdata")


library(data.table)

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

hist(df_whites_1940$age_to_infection)

df_whites_1940 <- age_to_infection(df = df_p_hat_race_final_cohort, cohort = c(1940), race = c(1:5), alpha0 = alpha0, alpha1 = alpha1, gamma0 = gamma0, gamma1 = gamma1, size = 100000)


hist(df_whites_1940$age_to_infection)

ecdf_whites_1940 <- ecdf(df_whites_1940$age_to_infection)

l_alpha_Whites_1940 <- get_parameters_constrained_exponential(birth_cohort = 1940, race = 5, alpha0 = alpha0, alpha1 = alpha1, gamma0 = gamma0, gamma1 = gamma1)

plot(l_alpha_Whites_1940[["Cohort_1940_Race_5"]][["par_alpha"]]*ecdf_whites_1940(0:90), main="Cumulative Distribution Function Infection",
     xlab="Age",
     ylab="Proportion infected", ylim = c(0, 1), type = "l")




df_races_all <- age_to_infection(df = df_p_hat_race_final_cohort, 
                                 cohort = c(1940:1998), 
                                 race = c(1:5), 
                                 alpha0 = alpha0, 
                                 alpha1 = alpha1, 
                                 gamma0 = gamma0, 
                                 gamma1 = gamma1, 
                                 size = 10000)




