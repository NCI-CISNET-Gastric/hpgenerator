


plots_server_foi <- function(input, output, session, buttons) {

  plt_foi_act <- eventReactive(input$goButton, {
    
    plot_foi_shiny_v2(age = buttons$age(), 
                      race = buttons$races(), 
                      cohort = buttons$years(), age_bracket = input$bracket)
    #save_data = input$download)
    
  })
  output$plt_foi <- renderGirafe({box_plot <- plt_foi_act()
  
  girafe(
    ggobj = box_plot,
    options = list(
      opts_hover(css = ''),
      opts_sizing(rescale = FALSE),
      opts_hover_inv(css = "opacity:0.1;"),
      opts_tooltip(
        opacity = 0.8, use_fill = TRUE,
        use_stroke = FALSE),
      opts_zoom(min = .5, max = 4)
      #sizingPolicy(defaultWidth = "100%", defaultHeight = "100%")
    ),
    width_svg = (.8*input$pltChange$width/input$pltChange$dpi),
    height_svg = (1*input$pltChange$height/input$pltChange$dpi)
  ) 
  })
  
  plt_foi_act_glide <- eventReactive(input$goButton_obs, {
    
    plot_foi_shiny_v2(age = buttons$age(), 
                      race = buttons$races(), 
                      cohort = buttons$years(), age_bracket = input$bracket)
    #save_data = input$download)
    
  })
  output$plt_foiglide <- renderGirafe({box_plot <- plt_foi_act_glide()
  
  girafe(
    ggobj = box_plot,
    options = list(
      opts_hover(css = ''),
      opts_sizing(rescale = FALSE),
      opts_hover_inv(css = "opacity:0.1;"),
      opts_tooltip(opacity = .7, use_fill = TRUE, use_stroke = TRUE),
      opts_zoom(min = .5, max = 4)
      #sizingPolicy(defaultWidth = "100%", defaultHeight = "100%")
    ),
    width_svg = (.8*input$pltChange$width/input$pltChange$dpi),
    height_svg = (1*input$pltChange$height/input$pltChange$dpi)
  ) 
  })  
  
}

