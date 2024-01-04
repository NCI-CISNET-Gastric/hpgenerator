main_panel_age_to_inf <- mainPanel(id = "main_panel_age_to_inf",
          style = "margin-top: 100px;
                   margin-left: -80px;
                   width:60%;", 
          #h1(textOutput("txt_foi"), align = "center"),
          #h2("subtítulo central"),
          # Irán los outputs
          girafeOutput("plt_age_to_inf")  %>% withSpinner(color="#E32500", type =6, size = 2.5))