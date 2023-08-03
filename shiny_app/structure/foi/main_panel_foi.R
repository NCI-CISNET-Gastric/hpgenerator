main_panel_foi <- mainPanel(id = "custom_main_panel",
          style = "margin-top: 100px;
                   margin-left: -80px;
                   width:60%;", 
          #h1(textOutput("txt_foi"), align = "center"),
          #h2("subtítulo central"),
          # Irán los outputs
          girafeOutput("plt_foi") %>% withSpinner(image = "https://thumbs.gfycat.com/AdorableMajesticJellyfish-size_restricted.gif"),
          girafeOutput("plt_foiglide") %>% withSpinner(image = "https://thumbs.gfycat.com/AdorableMajesticJellyfish-size_restricted.gif"),
          
)