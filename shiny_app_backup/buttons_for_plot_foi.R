
  buttons_for_plot_foi <- function(input, output, session) {
    
    age <- reactive({
      seq(input$deslizador[1], input$deslizador[2], by = 1)
    })
    
    years <- reactive({
      seq(input$cohort[1], input$cohort[2], by = 1)
    })
    
    races <- reactive({
      race_selected <- c()
      if(input$race1) race_selected <- c(race_selected, "Hispanic")
      if(input$race2) race_selected <- c(race_selected, "NH American Indian")
      if(input$race3) race_selected <- c(race_selected, "NH Asian")
      if(input$race4) race_selected <- c(race_selected, "NH Black")
      if(input$race5) race_selected <- c(race_selected, "NH White")
      race_selected
    })
    
    return(list(age = age, years = years, races = races))
  }
  



