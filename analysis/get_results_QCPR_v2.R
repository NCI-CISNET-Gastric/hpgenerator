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
# Date Created: 07-Mar-2023
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

load("results/09_HP_TwoLevel_Quadratic_output_new_data_CauchyPriors_Restricted.RData")


prueba <- fit_model$alpha0

prueba1 <- fit_model$alpha0[1,,1]
prueba2 <- fit_model$alpha0[2,,1]
prueba3 <- fit_model$alpha0[3,,1]
prueba4 <- fit_model$alpha0[4,,1]
prueba5 <- fit_model$alpha0[5,,1]

bind

prueba_t <- t(prueba)

prueba_r2 <- t(prueba)

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



v_alpha2 <- c()

for (i in c(1,2,3,4,5)) {                
  alpha2 <- c(fit_model$alpha2[i,,1], fit_model$alpha2[i,,2])        
  v_alpha2 <- c(v_alpha2, alpha2)    
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

v_gamma2 <- c()

for (i in c(1,2,3,4,5)) {                
  gamma2 <- c(fit_model$gamma2[i,,1], fit_model$gamma2[i,,2])        
  v_gamma2 <- c(v_gamma2, gamma2)    
}



v_race <- rep(c("Hispanic", "NH American Indian", 
                "NH Asian", "NH Black", "NH White"), each = 2000)

df_alpha_QCPR_v2 <- data.frame(v_race, v_alpha0, v_alpha1, v_alpha2)


df_gamma_QCPR_v2 <- data.frame(v_race, v_gamma0, v_gamma1, v_gamma2)


save(df_alpha_QCPR_v2, file = "data/df_alpha_QCPR_v2.rda")

save(df_gamma_QCPR_v2, file = "data/df_gamma_QCPR_v2.rda")
