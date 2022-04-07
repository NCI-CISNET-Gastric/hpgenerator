age = c(0:100) 
sex = c("Male", "Female")
race = c("Non-Hispanic White", "Non-Hispanic Black", 
         "Other Hispanic", "Mexican-American", "Other") 
period = 1991:2010

levels_sex <- c("Female", "Male")
sex <- factor(sex,levels = levels_sex)
sex <- sort(sex)

df_alpha <- df_alpha_race %>%
  filter(v_race %in% race)
v_alpha0 = df_alpha$v_alpha0
v_alpha1 = df_alpha$v_alpha1
v_alpha2 = df_alpha$v_alpha2

df_gamma <- df_gamma_race %>%
  filter(v_race %in% race)
v_gamma0 = df_gamma$v_gamma0
v_gamma1 = df_gamma$v_gamma1
v_gamma2 = df_gamma$v_gamma2


df_parameters <- data.frame(NULL)

v_num_period <- as.numeric(period) - 1991

for (sex_i in sex){#sex_i = "Female"
  if(sex_i == "Male"){
    num_sex <- 0
  } else if (sex_i == "Female"){
    num_sex <- 1
  }
  for (period_i in seq_along(v_num_period)){ #period_i = 1
    num_period = v_num_period[period_i]
    v_alpha_est <- ilogit(v_alpha0 + v_alpha1*num_period + v_alpha2*num_sex) 
    v_gamma_est <- exp(v_gamma0 + v_gamma1*num_period + v_gamma2*num_sex)
    
    df_temp <- data.frame(sim = 1:10000,
                          race = rep(race, each = 10000),
                          period = as.numeric(period[period_i]),
                          sex = sex_i,
                          country = "USA",
                          v_alpha0 = v_alpha0,
                          v_alpha1 = v_alpha1,
                          v_alpha2 = v_alpha2,
                          v_alpha_est = v_alpha_est,
                          v_gamma0 = v_gamma0,
                          v_gamma1 = v_gamma1,
                          v_gamma2 = v_gamma2,
                          v_gamma_est = v_gamma_est,
                          check.names = F)
    df_parameters <- rbind(df_parameters, df_temp)
  }
}


save(df_parameters, file = "data/df_parameters.Rdata")



v_age <- matrix(age, nrow = 1)

m_foi_hat <- ((v_alpha_est*v_gamma_est) * exp(-(v_gamma_est %*% v_age)))/(1-v_alpha_est*(1-exp(-(v_gamma_est %*% v_age))))

colnames(m_foi_hat) <- paste0("age_",as.character(age))


df_foi_hat <- cbind(df_alpha_gamma_est, m_foi_hat)




df_foi_long_filter <- df_foi_long %>% filter(age %in% age,
                                             race %in% race,
                                             period %in% period,
                                             sex %in% sex)



save(df_foi_long, file = "data/df_foi_long.Rdata")

