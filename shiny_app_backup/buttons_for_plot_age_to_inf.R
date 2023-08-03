
  buttons_for_plot_age_to_inf <- function(input, output, session) {
    
    numeric_pop_ati <- reactive({
      input$numeric_pop
    })
    
    years_ati <- reactive({
      seq(input$cohort_ati[1], input$cohort_ati[2], by = 1)
    })
    
    races_ati <- reactive({
      race_selected_ati <- c()
      if(input$race1_ati) race_selected_ati <- c(race_selected_ati, 1)
      if(input$race2_ati) race_selected_ati <- c(race_selected_ati, 2)
      if(input$race3_ati) race_selected_ati <- c(race_selected_ati, 3)
      if(input$race4_ati) race_selected_ati <- c(race_selected_ati, 4)
      if(input$race5_ati) race_selected_ati <- c(race_selected_ati, 5)
      race_selected_ati
    })
    
    return(list(size = numeric_pop_ati, years = years_ati, races = races_ati))
  }

    

  buttons_for_plot_age_to_inf()

