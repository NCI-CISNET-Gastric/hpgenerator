about_tab <- tabPanel(strong("About"), icon = icon("circle-info", "fa-lg"),
                      fluidPage(
                        titlePanel(title = h1("Cancer Intervention and Surveillance Modeling Network", align = "center")),
                        br(),
                        br(),
                        br(),
                        mainPanel(
                          div(
                            img(src = "cisnet.jpeg", height = "160vh", width = "140vh"), 
                            style= 'position:absolute; right:160px;'
                          ),
                          div(
                            style = "text-align: center; margin-left: 520px;",
                            div(
                              p("Go to the repository and add your feedback as an issue", style = "font-size: 20px;"), 
                              style = "margin-top:280px;" # Text div
                            ),
                            div(
                              a(id = "github_but", href="https://github.com/feralaes/hpgenerator", target="_blank", class="btn btn-default", icon("github"), "GitHub Repository", style = "font-size: 20px;"),
                              style = "margin-top:20px;" # Button div
                            )
                          )
                        ),
                        div(style = "margin-bottom:260px;")
                      ))
