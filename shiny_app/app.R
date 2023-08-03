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
library(gdtools)


register_gfont("Open Sans")



source(file = "global.R")
source(file = "functions.R")
source(file = "structure/foi/glide-modal-introduction_foi.R")
source(file = "structure/foi/plots_server_foi.R")
source(file = "structure/foi/buttons_for_plot_foi.R")
source(file = "structure/foi/download_data_foi.R")
source(file = "structure/foi/sidebar_foi.R")
source(file = "structure/foi/main_panel_foi.R")
source(file = "structure/foi/foi_tab.R")
source(file = "structure/prevalence/prevalence_tab.R")
source(file = "structure/age_to_infection/buttons_for_plot_age_to_inf.R")
source(file = "structure/age_to_infection/plots_server_age_to_inf.R")
source(file = "structure/age_to_infection/download_data_age_to_inf.R")
source(file = "structure/age_to_infection/sidebar_age_to_inf.R")
source(file = "structure/age_to_infection/main_panel_age_to_inf.R")
source(file = "structure/age_to_infection/age_to_infection_tab.R")
source(file = "structure/hexamaps/hexamaps_tab.R")
source(file = "structure/about/about_tab.R")









# User Interface ------------------------------------------------------ 
ui <- shinyUI( fluidPage(includeCSS("www/css/custom.css"),
                         tags$head( tags$style(type="text/css", "text {@import url('https://fonts.googleapis.com/css2?family=Roboto:wght@300&display=swap'); font-family: 'Roboto', sans-serif;
                          font-weight: 300;}")),
                         includeScript("www/js/custom.js"), #for ggiraph
                         tags$body(tags$div(id="ppitest", style="width:1in;")),
                         introjsUI(), 
                         # Title of navbar ------------------------------------------------------                   
                         div(class = "container-fluid",
                             navbarPage(em(icon("bacterium"), "Helicobacter Pylori"), selected = strong("Force of Infection"), collapsible = TRUE, inverse = TRUE, 
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

