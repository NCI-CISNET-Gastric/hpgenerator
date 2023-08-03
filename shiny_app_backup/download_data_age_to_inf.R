
download_data_age_to_inf <- function(input, output, session, buttons_age_to_inf) {

data_dwn_age_to_inf <- reactive({
  
  age_to_infection(df = df_p_hat_race_final_cohort, 
                   cohort = buttons_age_to_inf$years(), 
                   race = buttons_age_to_inf$races(), 
                   alpha0 = alpha0, alpha1 = alpha1, gamma0 = gamma0, 
                   gamma1 = gamma1, size = buttons_age_to_inf$size())
  
})


output$Download_csv_ati <- downloadHandler(
  
  filename = function() { 
    paste("df_foi_birthcohort_", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(data_dwn_age_to_inf(), file)
  })

output$Download_r_ati <- downloadHandler(
  
  filename = function() { 
    paste("df_foi_birthcohort_", Sys.Date(), ".Rdata", sep="")
  },
  content = function(file) {
    data_r_ati <- data_dwn_age_to_inf()
    save(data_r_ati, file=file)
  })

}
