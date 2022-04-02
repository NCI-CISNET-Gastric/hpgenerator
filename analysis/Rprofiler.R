library(tidyverse)

Rprof(filename = "Prof_generate_foi.out")

test_rprof_generate_foi <- generate_foi()

Rprof(NULL)

summaryRprof(filename = "Prof_generate_foi.out")


summaryRprof(
  filename = "Prof_generate_foi.out",
  memory = c("both")
)[["by.self"]]

library(hpgenerator)

library(profvis)

library(ggplot2)

profvis({ generate_foi()}, interval = .1)

profvis({ summary_foi()}, interval = .1)

profvis({ plot_foi()}, interval = .1)


test_g_f <- profr::profr(generate_foi())

summaryRprof()

profr::profr()

ggplot(test_g_f)


library(tictoc)
# generate_foi ------------------------------------------------------------

tic("total")

tic("levels variables")
  levels_race <- c("Non-Hispanic White","Non-Hispanic Black",
                   "Other Hispanic","Mexican-American", "Other")
  
  race <- factor(race,levels = levels_race)
  
  race <- sort(race)
  
  n_period <- length(period)
  
  period <- as.character(period)
  
  period <- factor(as.numeric(period), ordered = T)
  
  period <- as.character(sort(period))
  
toc()

tic("filter alphas and gammas")
  
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
  
  v_num_period <- as.numeric(period)-1991
  
  toc()
  
  #v_names_races <- c("Non-Hispanic White", "Non-Hispanic Black", 
  #                   "Other Hispanic", "Mexican-American", "Other")
  
  #c(outer(sex, period, FUN = "paste0"))
  
  #m_estimates <- c()
  
  tic("df_estimates")
  df_estimates <- data.frame(NULL)
  
  toc()
  
  tic("for")
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

      v_age <- matrix(age, nrow = 1)


      m_foi_hat <- ((v_alpha_est*v_gamma_est) * exp(-(v_gamma_est %*% v_age)))/(1-v_alpha_est*(1-exp(-(v_gamma_est %*% v_age))))


      colnames(m_foi_hat) <- paste0("age_",as.character(age))

      df_foi_hat <- data.frame(sim = 1:10000,
                               race = rep(race, each = 10000),
                               period = as.numeric(period[period_i]),
                               sex = sex_i,
                               country = "USA",
                               m_foi_hat,
                               check.names = F)

      #m_estimates <- rbind(m_estimates, m_foi_hat)
      
      
      df_estimates <- rbind.data.frame(df_estimates, df_foi_hat)

      
    }
  }
  toc()


# rbenckmark generate foi -------------------------------------------------


benchmark("levels variables" = {
levels_race <- c("Non-Hispanic White","Non-Hispanic Black",
                 "Other Hispanic","Mexican-American", "Other")

race <- factor(race,levels = levels_race)

race <- sort(race)

n_period <- length(period)

period <- as.character(period)

period <- factor(as.numeric(period), ordered = T)

period <- as.character(sort(period))},

"filter alphas and gammas" = {

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

v_num_period <- as.numeric(period)-1991

},

"df_estimates" = {
df_estimates <- data.frame(NULL)
},


"sex_i" = {
for (sex_i in sex){#sex_i = "Female"
  if(sex_i == "Male"){

    num_sex <- 0

  } else if (sex_i == "Female"){
    

    num_sex <- 1

  },
  "num_period" = {
  for (period_i in seq_along(v_num_period)){ #period_i = 1
    
    num_period = v_num_period[period_i]
  
  
  "v_alpha_est" = {
    v_alpha_est <- ilogit(v_alpha0 + v_alpha1*num_period + v_alpha2*num_sex) 
  }
    "v_gamma_est") = {
    v_gamma_est <- exp(v_gamma0 + v_gamma1*num_period + v_gamma2*num_sex)
    }
    "v_age" = {
    v_age <- matrix(age, nrow = 1)
    }
   "m_foi_hat" = {
    m_foi_hat <- ((v_alpha_est*v_gamma_est) * exp(-(v_gamma_est %*% v_age)))/(1-v_alpha_est*(1-exp(-(v_gamma_est %*% v_age))))
   }
    "col_m_foi_hat" = {
    colnames(m_foi_hat) <- paste0("age_",as.character(age))
    }
   "df_foi_hat" = {
    df_foi_hat <- data.frame(sim = 1:10000,
                             race = rep(race, each = 10000),
                             period = as.numeric(period[period_i]),
                             sex = sex_i,
                             country = "USA",
                             m_foi_hat,
                             check.names = F)
   }
    "df_estimates" = {
    #m_estimates <- rbind(m_estimates, m_foi_hat)
    
    
    df_estimates <- rbind.data.frame(df_estimates, df_foi_hat)
    }
  }
  }

replications = 1000,
columns = c("test", "replications", "elapsed",
            "relative", "user.self", "sys.self"))
