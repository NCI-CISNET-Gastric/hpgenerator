# *****************************************************************************
#
# Script: 
#
# Purpose: Get parameters alpha, gamma 
#
# Author: Jorge Roa
#
# Email: jorge.roa@cide.edu / J.Roa@students.hertie-school.org
#
# Date Created: 5-May-2023
#
# *****************************************************************************
#
# Notes:
#   
#
# *****************************************************************************

# *****************************************************************************
# *****************************************************************************

rm(list = ls()) # to clean the workspace

# *****************************************************************************
#### 01 Load packages ####
# *****************************************************************************

library(tidyverse)

# *****************************************************************************
#### 02 Load data ####
# *****************************************************************************

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

save(df_alpha_up, file = "data/df_alpha_up.rda")

save(df_gamma_up, file = "data/df_gamma_up.rda")



