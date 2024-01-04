


plots_server_age_to_inf <- function(input, output, session, buttons_age_to_inf) {

  plt_foi_act_age_to_inf <- eventReactive(input$goButton_age_to_inf, {
    
    
    plot_age_to_infection_grid(size = buttons_age_to_inf$size(), 
                          race = buttons_age_to_inf$races(), 
                          cohort = buttons_age_to_inf$years())
    
    #save_data = input$download)
    
  })
  output$plt_age_to_inf <- renderGirafe({age_to_inf_plot <- plt_foi_act_age_to_inf()
  
  
  girafe(
    ggobj = age_to_inf_plot,
    options = list(
      opts_sizing(rescale = FALSE),
      opts_hover_inv(css = "opacity:0.1;"),
      opts_tooltip(opacity = .9, use_fill = TRUE, use_stroke = TRUE),
      opts_zoom(min = .5, max = 4)
      #sizingPolicy(defaultWidth = "100%", defaultHeight = "100%")
    ),
    width_svg = (.8*input$pltChange$width/input$pltChange$dpi),
    height_svg = (1*input$pltChange$height/input$pltChange$dpi)
  )
  })
  
}
