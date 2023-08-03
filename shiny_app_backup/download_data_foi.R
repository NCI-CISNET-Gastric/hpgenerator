
download_data_foi <- function(input, output, session, buttons) {

data_dwn <- reactive({summary_foi_bc(ages =buttons$age(), 
                                     races =  buttons$races(), 
                                     birthcohort = buttons$years())
})


output$Download_csv <- downloadHandler(
  
  filename = function() { 
    paste("df_foi_birthcohort_", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(data_dwn(), file)
  })

output$Download_r <- downloadHandler(
  
  filename = function() { 
    paste("df_foi_birthcohort_", Sys.Date(), ".Rdata", sep="")
  },
  content = function(file) {
    data_r <- data_dwn()
    save(data_r, file=file)
  })

}
