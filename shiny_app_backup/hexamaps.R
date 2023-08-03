
source("R/hex_apc_2020_mod.R")

library(hpgenerator)
library(dplyr)
library(tidyr)


races <- c("Non-Hispanic White", "Non-Hispanic Black", 
           "Other Hispanic", "Mexican-American", 
           "Other")

sex <- c("Male", "Female")

df_summary_foi_total <- summary_foi(age = 0:100, 
            sex = sex,
            race = races,
            period = c(1991:2005), percs = F)


for (i in races) {
  for (x in sex){
       foi_list <- list(summary_foi(age = 0:100, 
                         sex = x,
                         race = i,
                         period = c(1991:2005), percs = F))
  }
  }


df_summary_foi <- summary_foi(age = 0:100, 
             sex = c("Male"),
             race = c("Non-Hispanic White"),
             period = c(1991:2005), percs = F)





df_summary_foi_f <- summary_foi(age = 0:100, 
                              sex = c("Female"),
                              race = c("Non-Hispanic White"),
                              period = c(1991:2001), percs = F)


df_summary_foi_NHW_male <- df_summary_foi %>% 
                        pivot_wider(names_from = period, values_from = foi_mean)

df_summary_foi_NHW_female <- df_summary_foi_f %>% 
  pivot_wider(names_from = period, values_from = foi_mean)


m_foi_NHW_m <- as.matrix(df_summary_foi_NHW_male[,-c(1:3)])

m_foi_NHW_f <- as.matrix(df_summary_foi_NHW_female[,-c(1:3)])




pdf("figs/NHW_fm.pdf", width= 5, height = 8) #output the result to a pdf file
par(mfrow=c(1,2))

create_hexamap(data = m_foi_NHW_m,  #matrix: age as rows, period as columns
               first_age = 0,
               first_period = 1991,
               interval = 1,
               first_age_isoline = 0,
               first_period_isoline = 1991,
               isoline_interval = 2,
               scale_units = "Force\nof\ninfection",
               wrap_cohort_labels = T,
               title_main = "Non-Hispanic White",
               title_sub = "Male")

create_hexamap(data = m_foi_NHW_f,  #matrix: age as rows, period as columns
               first_age = 0,
               first_period = 1991,
               interval = 1,
               first_age_isoline = 0,
               first_period_isoline = 1991,
               isoline_interval = 2,
               scale_units = "Force\nof\ninfection",
               wrap_cohort_labels = T,
               title_main = "",
               title_sub = "Female")
dev.off()


