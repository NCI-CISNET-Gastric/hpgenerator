foi_tab <- tabPanel(strong("Force of Infection"), icon = icon("chart-line", "fa-lg"),              
         fluidPage(
           sidebarLayout(sidebarPanel = sidebar_foi,
                         mainPanel = main_panel_foi)))