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

load("priors/df_alpha_CPR2_16_v3.rda")
load("priors/df_gamma_CPR2_16_v3.rda")
load("data/df_alphas_MVN_04.rda")
load("data/df_gammas_MVN_04.rda")


source("R/anxiliary_functions.R")

race <- c("Hispanic")

load("shiny/04_hp_foi_nhanes_MVNprior_usborn_restricted_output.RData")



param_prefixes <- c("alpha0", "alpha1", "alpha2", "gamma0", "gamma1", "gamma2")

for (x in 1:length(param_prefixes)) {
  for (y in 1:3) {
    var_name <- paste0(param_prefixes[x], "_", y)
    assign(var_name, t(fit_model[["par"]][, x, , y]))
  }
}

df_alpha0 <- data.frame(as.matrix(rbind(alpha0_1, alpha0_2, alpha0_3)))
names(df_alpha0) <- c("alpha0[1]", "alpha0[2]", "alpha0[3]", "alpha0[4]")


df_alpha1 <- data.frame(as.matrix(rbind(alpha1_1, alpha1_2, alpha1_3)))
names(df_alpha1) <- c("alpha1[1]", "alpha1[2]", "alpha1[3]", "alpha1[4]")


df_alpha2 <- data.frame(as.matrix(rbind(alpha2_1, alpha2_2, alpha2_3)))
names(df_alpha2) <- c("alpha2[1]", "alpha2[2]", "alpha2[3]", "alpha2[4]")

df_gamma0 <- data.frame(as.matrix(rbind(gamma0_1, gamma0_2, gamma0_3)))
names(df_gamma0) <- c("gamma0[1]", "gamma0[2]", "gamma0[3]", "gamma0[4]")


df_gamma1 <- data.frame(as.matrix(rbind(gamma1_1, gamma1_2, gamma1_3)))
names(df_gamma1) <- c("gamma1[1]", "gamma1[2]", "gamma1[3]", "gamma1[4]")

df_gamma2 <- data.frame(as.matrix(rbind(gamma2_1, gamma2_2, gamma2_3)))
names(df_gamma2) <- c("gamma2[1]", "gamma2[2]", "gamma2[3]", "gamma2[4]")

# Define a function to perform the repeated operations
convert_and_reshape <- function(mat, prefix) {
  df <- data.frame(as.matrix(mat))
  names(df) <- paste0(prefix, "[", 1:ncol(mat), "]")
  
  df_long <- df %>%
    tibble::rownames_to_column(var = "Row") %>%
    pivot_longer(cols = -Row, names_to = "Parameters", values_to = paste0("v_", prefix)) %>% 
    mutate(v_race = case_when(stringr::str_detect(Parameters, paste(c( "4]"), collapse = '|')) ~ "Other", 
                            stringr::str_detect(Parameters, paste(c( "3]"), collapse = '|')) ~ "NH White",
                            stringr::str_detect(Parameters, paste(c( "2]"), collapse = '|')) ~ "NH Black",
                            stringr::str_detect(Parameters, paste(c( "1]"), collapse = '|')) ~ "Hispanic"))%>% 
    mutate(v_race = factor(v_race, levels = c("Hispanic", "NH Black", "NH White", "Other")))  %>% 
    arrange(v_race)
  
  return(df_long)
}

# Now call this function for each matrix
df_alpha0_long <- convert_and_reshape(rbind(alpha0_1, alpha0_2, alpha0_3), "alpha0")
df_alpha1_long <- convert_and_reshape(rbind(alpha1_1, alpha1_2, alpha1_3), "alpha1") %>% select(v_alpha1)
df_alpha2_long <- convert_and_reshape(rbind(alpha2_1, alpha2_2, alpha2_3), "alpha2") %>% select(v_alpha2)
df_gamma0_long <- convert_and_reshape(rbind(gamma0_1, gamma0_2, gamma0_3), "gamma0")
df_gamma1_long <- convert_and_reshape(rbind(gamma1_1, gamma1_2, gamma1_3), "gamma1") %>% select(v_gamma1)
df_gamma2_long <- convert_and_reshape(rbind(gamma2_1, gamma2_2, gamma2_3), "gamma2") %>% select(v_gamma2)



df_alpha_up <- bind_cols(df_alpha0_long, df_alpha1_long, df_alpha2_long) %>% select(v_race, v_alpha0,v_alpha1, v_alpha2)
df_gamma_up <- bind_cols(df_gamma0_long, df_gamma1_long, df_gamma2_long) %>% select(v_race, v_gamma0,v_gamma1, v_gamma2)


options(max.print=1000)

generate_foi_linear_birth_cohort <- function(df_alpha_up,
                                             df_gamma_up,
                                             age = c(0:90), 
                                             race = c("Hispanic", "NH Black", "NH White", "Other"), 
                                             cohort = 1908:1998,
                                             birthplace = c("US", "Foreign")) {
  
  if (!all(age %in% c(0:90))) {
    stop("age must be a number or numeric vector between 0 and 90 years")
  }
  
  match.arg(race)
  if (!all(race %in% c("Hispanic", "NH Black", "NH White", "Other"))) {
    stop("Race must be a character or vector with the next races: Hispanic, 
         NH American Indian, NH Asian, NH Black, NH White")
  }
  
  options(dplyr.summarise.inform = FALSE)
  
  
  levels_birthplace <- c("US", "Foreign")
  birthplace <- factor(birthplace,levels = levels_birthplace)
  birthplace <- sort(birthplace)
  
  
  levels_race <- c("Hispanic", "NH Black", "NH White", "Other")
  
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
  v_alpha2 = df_alpha$v_alpha2
  
  df_gamma <- df_gamma_up %>%
    dplyr::filter(v_race %in% race)
  
  v_gamma0 = df_gamma$v_gamma0
  v_gamma1 = df_gamma$v_gamma1
  v_gamma2 = df_gamma$v_gamma2
  
  v_num_cohort <- as.numeric(cohort)-1908
  
  df_estimates <- data.frame(NULL)
  
  for (bp_i in birthplace){#sex_i = "Female"
    if(bp_i == "US"){
      num_bp <- 0
    } else if (bp_i == "Foreign"){
      num_bp <- 1
    } 

    no_cores <- parallel::detectCores() - 7   # detect number of cores
    cl <- parallel::makeCluster(no_cores)      # initialize cluster object
    doParallel::registerDoParallel(cl)
    
  df_final <- foreach::foreach (cohort_i = seq_along(v_num_cohort), .combine = rbind) %do% { #period_i = 1
    num_cohort = v_num_cohort[cohort_i]
    v_alpha_est <- ilogit(v_alpha0 + v_alpha1*num_bp + v_alpha2*num_cohort) 
    v_gamma_est <- exp(v_gamma0 + v_gamma1*num_bp + v_gamma2*num_cohort)
    v_age <- matrix(age, nrow = 1)
    
    m_foi_hat <- ((v_alpha_est*v_gamma_est) * exp(-(v_gamma_est %*% v_age)))/(1-v_alpha_est*(1-exp(-(v_gamma_est %*% v_age))))
    
    colnames(m_foi_hat) <- paste0("age_",as.character(age))
    
    df_foi_hat <- data.frame(sim = 1:3000,
                             race = rep(race, each = 3000),
                             cohort = as.numeric(cohort[cohort_i]),
                             birthplace = bp_i,
                             m_foi_hat,
                             check.names = F)# %>% melt(id.vars = c("sim", "race", "cohort", "birthplace"))
    
    #m_estimates <- rbind(m_estimates, m_foi_hat)
    #df_estimates <- rbind.data.frame(df_estimates, df_foi_hat)
  
    df_foi_hat
  }
  parallel::stopCluster(cl)
  
  df_estimates <- rbind.data.frame(df_estimates, df_final)
 
  }
  return(df_estimates)
}

start.time <- Sys.time()  

prueba_final_2 <- generate_foi_linear_birth_cohort(df_alpha_up = df_alpha_up,
                                                 df_gamma_up = df_gamma_up)

prueba_final_2_1 <- prueba_final_2 %>% melt(id.vars = c("sim", "race", "cohort", "birthplace"))

end.time <- Sys.time()  # End the timer

# Print the time taken
print(end.time - start.time)


df_foi_linear_birth_cohort_all <- generate_foi_linear_birth_cohort(df_alpha_up = df_alpha_up,
                                                                  df_gamma_up = df_gamma_up)


df_foi_linear_birth_cohort_all



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
                           race = c("Hispanic", "NH Black", "NH White", "Other"), 
                           cohort = 1908:1998,
                           birthplace = c("US", "Foreign"),
                           percs = TRUE) {
  
  options(dplyr.summarise.inform = FALSE)
  
  df_foi_hat <- generate_foi_linear_birth_cohort(df_alpha_up, df_gamma_up, age = age, race = race, cohort = cohort, birthplace = birthplace)
  
  LB <- 0.025
  UB <- 0.975
  
  df_foi_long <- melt(as.data.table(df_foi_hat), id.vars = c("sim", "race", "cohort", "birthplace"))
  
  df_foi_final <- df_foi_long[, .(
    foi_mean = mean(value),
    foi_cri_lb = quantile(value, probs = LB, na.rm = TRUE),
    foi_cri_ub = quantile(value, probs = UB, na.rm = TRUE)
  ), by = .(race, cohort, birthplace, variable)]
  
  df_foi_final <- df_foi_final[, age := as.numeric(str_remove(variable, "age_"))]
  
  
  if (!percs) {
    df_foi_final <- df_foi_final[, .(
      race, cohort, birthplace, age, foi_mean
    )]
  }
  
  return(df_foi_final)
}


start.time <- Sys.time()  

df_all <- summary_foi_v3()

end.time <- Sys.time()  # End the timer

# Print the time taken
print(end.time - start.time)


df_all_hispanic <- summary_foi_v3(race = "Hispanic")

jet.colors <- colorRampPalette(c("black", "#00007F", "blue", "#007FFF",
                                        "cyan", "#7FFF7F", "yellow", "#FF7F00",
                                        "red", "#7F0000"))
                                        

color_map  <-  jet.colors(100)

unique(prueba_sum$birthplace)

ggplot(df_all_alt %>% filter(age!=0, cohort >= 1940, birthplace == "Foreign"), aes(y = foi_mean, x = age, group = cohort, color = cohort)) + 
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
  #scale_y_continuous(breaks = seq(0, 100, by = 20), 
  #                   labels = c("0%", "20%", "40%", "60%", "80%", "100%"),
  #                   limits = c(0, 100)) +
  theme_bw(base_size = 16) + 
  theme(legend.position = "bottom",
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
    color = guide_colorbar(reverse = FALSE))



