library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(shinyalert)
library(shinyglide)
library(ggiraph)
library(htmlwidgets)
library(fontawesome)
library(shinyjs)
library(rintrojs)
library(bslib)




source(file = "global.R")
source(file = "sidebar_foi.R")
source(file = "main_panel_foi.R")
source(file = "foi_tab.R")
source(file = "prevalence_tab.R")
source(file = "age_to_infection_tab.R")
source(file = "sidebar_age_to_inf.R")
source(file = "main_panel_age_to_inf.R")
source(file = "hexamaps_tab.R")
source(file = "about_tab.R")
source(file = "glide-modal-introduction_foi.R")
source(file = "glide-modal-introduction_ati.R")
source(file = "buttons_for_plot_foi.R")
source(file = "buttons_for_plot_age_to_inf.R")
source(file = "plots_server_foi.R")
source(file = "plots_server_age_to_inf.R")
source(file = "download_data_foi.R")
source(file = "download_data_age_to_inf.R")


source(file = "global.R")
source(file = "sidebar_foi.R")
source(file = "main_panel_foi.R")
source(file = "foi_tab.R")
source(file = "prevalence_tab.R")
source(file = "age_to_infection_tab.R")
source(file = "sidebar_age_to_inf.R")
source(file = "main_panel_age_to_inf.R")
source(file = "hexamaps_tab.R")
source(file = "about_tab.R")
source(file = "glide-modal-introduction_foi.R")
source(file = "glide-modal-introduction_ati.R")
source(file = "buttons_for_plot_foi.R")
source(file = "buttons_for_plot_age_to_inf.R")
source(file = "plots_server_foi.R")
source(file = "plots_server_age_to_inf.R")
source(file = "download_data_foi.R")
source(file = "download_data_age_to_inf.R")





# User Interface ------------------------------------------------------ 
ui <- shinyUI( fluidPage(includeCSS("www/css/custom.css"),
                         includeScript("www/js/custom.js"), #for ggiraph
                         tags$body(tags$div(id="ppitest", style="width:1in;")),
                         introjsUI(), 
# Title of navbar ------------------------------------------------------                   
                    div(class = "container-fluid",
                        navbarPage(em(icon("bacterium"), "Helicobacter Pylori"), selected = strong("Force of Infeºction"), collapsible = TRUE, inverse = TRUE, 
# Prevalence tab ------------------------------------------------------     
                           prevalence_tab,
# Foi tab ------------------------------------------------------     
                           foi_tab,
# Age to infection tab ------------------------------------------------------     
              age_to_infection_tab,
# Hexamaps tab ------------------------------------------------------ 
                      hexamaps_tab,
# About tab ------------------------------------------------------ 
                         about_tab 
)
)
)
)




# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  #Introduction screen
  glide_modal_introduction_foi(input, output, session)
  
  
  buttons <- buttons_for_plot_foi(input, output, session)
  
  plots_server_foi(input, output, session, buttons)
  
  download_data_foi(input, output, session, buttons)
  
  buttons_age_to_inf <- buttons_for_plot_age_to_inf(input, output, session)
  
  plots_server_age_to_inf(input, output, session, buttons_age_to_inf)
  
  download_data_age_to_inf(input, output, session, buttons_age_to_inf)
}

shinyApp(ui, server)



