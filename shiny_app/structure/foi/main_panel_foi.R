main_panel_foi <- mainPanel(id = "custom_main_panel",
          style = "margin-top: 100px;
                   margin-left: -80px;
                   width:60%;", 
          #h1(textOutput("txt_foi"), align = "center"),
          #h2("subtítulo central"),
          # Irán los outputs
          girafeOutput("plt_foi") %>% withSpinner(color="#E32500", type =6, size = 2.5),
          girafeOutput("plt_foiglide") %>% withSpinner(color="#E32500", type =6, size = 2.5),
          
)