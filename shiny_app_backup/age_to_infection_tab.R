age_to_infection_tab <- tabPanel(strong("Age to infection"), icon = icon("person-walking-arrow-loop-left", "fa-lg"),              
                    fluidPage(
                      sidebarLayout(sidebarPanel = sidebar_age_to_inf,
                                    mainPanel = main_panel_age_to_inf)))