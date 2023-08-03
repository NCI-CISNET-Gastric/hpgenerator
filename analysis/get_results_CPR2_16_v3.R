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

# *****************************************************************************
#### 02 Load data ####
# *****************************************************************************

load("results/16_HP_TwoLevel_Linear_output_CauchyPriors_UniformRestricted8.Rdata")



v_alpha0 <- c()

for (i in c(1,2,3,4,5)) {                
  alpha0 <- c(fit_model$alpha0[i,,1], fit_model$alpha0[i,,2])        
  v_alpha0 <- c(v_alpha0, alpha0)    
}

v_alpha1 <- c()

for (i in c(1,2,3,4,5)) {                
  alpha1 <- c(fit_model$alpha1[i,,1], fit_model$alpha1[i,,2])        
  v_alpha1 <- c(v_alpha1, alpha1)    
}

v_gamma0 <- c()

for (i in c(1,2,3,4,5)) {                
  gamma0 <- c(fit_model$gamma0[i,,1], fit_model$gamma0[i,,2])        
  v_gamma0 <- c(v_gamma0, gamma0)    
}

v_gamma1 <- c()

for (i in c(1,2,3,4,5)) {                
  gamma1 <- c(fit_model$gamma1[i,,1], fit_model$gamma1[i,,2])        
  v_gamma1 <- c(v_gamma1, gamma1)    
}




v_race <- rep(c("Hispanic", "NH American Indian", 
                "NH Asian", "NH Black", "NH White"), each = 2000)

df_alpha_CPR2_16_v3 <- data.frame(v_race, v_alpha0, v_alpha1)


df_gamma_CPR2_16_v3 <- data.frame(v_race, v_gamma0, v_gamma1)


save(df_alpha_CPR2_16_v3, file = "data/df_alpha_CPR2_16_v3.rda")

save(df_gamma_CPR2_16_v3, file = "data/df_gamma_CPR2_16_v3.rda")


