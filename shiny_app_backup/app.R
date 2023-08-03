####
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(lubridate)
library(stringr)
library(withr)
library(treemap)
library(DT)
library(shinyBS)
library(shinyjs)
library(WDI)
library(geosphere)
library(magrittr)
library(shinycssloaders)
options(spinner.color="#006272")
library(timevis)
library(leaflet)
library(tidyverse)
library(faraway)
library(devtools)
library(lme4)
library(plotly)
library(dampack)
library(hrbrthemes)


source(file = "global.R")

#load("df_alpha_race.rda")
#load("df_gamma_race.rda")
#load("df_parameters.Rdata")
#



header <- 
  dashboardHeader( title = tags$a(href='https://www.starwars.com/',
                                  tags$img(src='c3po.png', height = "35px",width="35px"),
                                  'C3POMOD'), 
                   disable = FALSE,
                   titleWidth  = 167,
                   tags$li(class = "dropdown",
                           tags$style(".sidebar-toggle {height: 40px}"),
                           tags$style(".navbar {height:20px !important}")),
                   dropdownMenuCustom( type = 'message',
                                       customSentence = customSentence,
                                       messageItem(
                                         from = "jorge.roa@cide.edu",#'Feedback and suggestions',
                                         message =  "",
                                         icon = icon("mailchimp"),
                                         href = "mailto:jorge.roa@cide.edu"
                                       ),
                                       icon = icon('code')
                   ),
                   dropdownMenuCustom( type = 'message',
                                       customSentence = customSentence_share,
                                       icon = icon("bullhorn"),
                                       messageItem(
                                         from = 'Twitter',
                                         message = "",
                                         icon = icon("twitter"),
                                         href = "https://twitter.com/"
                                       )
                   )
                   
  )
#anchor <- tags$a(href='http://www.mbie.govt.nz',
#                 tags$img(src='c3po.png'),
#                 'C3POMOD')
#
#header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
#header$children[[2]]$children[[1]] <-  tags$li(tags$a(href='http://www.mbie.govt.nz',
#                                             tags$img(src='c3po.png'),
#                                             'C3POMOD'), class= "dropdown") #,height='67',width='228.6', align = 'left'
#
#header$children[[2]]$children <- tags$div(
#  tags$head(tags$style(HTML(".name { background-color: #063970 }"))),
#  anchor,
#  class = 'name')
## 2. siderbar ------------------------------
siderbar <- 
  dashboardSidebar( 
    width = 200,
    sidebarMenu(
      id = 'sidebar',
      style = "position: relative; overflow: visible;",
      #style = "position: relative; overflow: visible; overflow-y:scroll",
      #style = 'height: 90vh; overflow-y: auto;',
      ## 1st tab show the Main dashboard -----------
      menuItem( "Prevalence", tabName = 'dashboard', icon = icon('bar-chart'),
                badgeLabel = "New", badgeColor = "red" ),
      
      ## add conditional panel to show more
      # conditionalPanel( "input.sidebar === 'dashboard'",
      #                   actionButton("btn_show_more",
      #                                paste0(' Show more details'),
      #                                icon = icon('chevron-circle-down'),
      #                                style='padding-top:0px; padding-bottom:0px;padding-left:3px;padding-right:3px; '
      #                                ) 
      #                   ),
      
      ## 2nd Second tab shows the country/region level tab --------------
      menuItem("Force of Infection", tabName = 'country_intel', icon = icon('chart-line')),
      
      
      
      ## 3rd tab shows commodity intel ----------
      menuItem("Cervical Cancer Incid.", tabName = "commodity_intel", icon = icon('layer-group'), startExpanded = F,
               menuSubItem('Across scenarios', tabName = "ci_exports", icon = icon('flag-usa')),
               menuSubItem('Relative Difference', tabName = "ci_imports", icon = icon('euro')),
               menuSubItem('Hexamaps', tabName = "ci_intel_by_hs", icon = icon("globe") )
      ),
      
      
      ## 4th tab HS finder -------------------------
      #menuItem("HS code finder", tabName = 'hs_finder', icon = icon('search') ),
      
      ## 5th tab Data source, definition , i.e., help ---------------
      menuItem( em(strong("H. Pylori Worldwide")), tabName = 'help', icon = icon('virus') ),
      
      ## 6th tab monthly update ----------------------
      menuItem( "About", tabName = 'monthly_update', icon = icon('question'),
                badgeLabel = "Update", badgeColor = "green" )
      
      
    )
  )

## 3. body --------------------------------
body <- dashboardBody( 
  ## 3.0. CSS styles in header ----------------------------
  tags$head(
    
    
    ### Styles 
    
    ###Icon
    tags$style(HTML(".small-box {height: 95px}")),
    tags$style(HTML(".fa { font-size: 35px; }")),
    tags$style(HTML(".glyphicon { font-size: 33px; }")),  ## use glyphicon package
    tags$style(HTML(".fa-bar-chart { font-size: 20px; }")), 
    tags$style(HTML(".fa-chart-line { font-size: 20px; }")),
    tags$style(HTML(".fa-layer-group { font-size: 20px; }")),
    tags$style(HTML(".fa-virus { font-size: 20px; }")),
    tags$style(HTML(".fa-file-csv { font-size: 20px; }")),
    tags$style(HTML(".fa-question { font-size: 20px; }")),
    tags$style(HTML(".fa-code { font-size: 20px; }")),
    tags$style(HTML(".fa-bullhorn { font-size: 20px; }")),
    tags$style(HTML(".tab-content { padding-left: 20px; padding-right: 30px; }")) ,
    tags$style(HTML(".fa-wrench { font-size: 15px; }")),
    tags$style(HTML(".fa-refresh { font-size: 15px; }")),
    tags$style(HTML(".fa-search { font-size: 15px; }")),
    tags$style(HTML(".fa-comment { font-size: 20px; }")),
    tags$style(HTML(".fa-share-alt { font-size: 20px; }")),
    tags$style(HTML(".fa-envelope { font-size: 10px; }")),
    tags$style(HTML(".fa-question-circle { font-size: 20px; }")),
    tags$style(HTML(".fa-chevron-circle-down { font-size: 15px; }")),
    tags$style(HTML(".fa-bell { font-size: 17px; }")),
    tags$style(HTML(".fa-check { font-size: 14px; }")),
    tags$style(HTML(".fa-times { font-size: 14px; }")),
    tags$style(HTML(".fa-circle { font-size: 10px; }")),
    tags$style(HTML(".fa-circle-o { font-size: 10px; }")),
    tags$style(HTML(".btn-group-container-sw {
    width:70%;
  }")),
    tags$style(HTML(".btn-group{
 width:70%;
  }")),
    tags$style(HTML(" .btn{
    background-color:#2C498C;
    color: white;
     width: 100%;
     margin-bottom: 10px;
    border-radius:10px;
}
.btn.active{
background-color:#8091ba;
border-color: #27417e;
color: white;
}
.btn:hover {
      background-color:#566da3;
      border-color: #27417e;
      cursor: pointer;
      color: white;
      transition: 0.7s;}

")),
    
    

    
    
    ## modify the dashboard's skin color
    tags$style(HTML('
                       /* logo */
                       .skin-blue .main-header .logo {
                       background-color: #063970;
                       }

                       /* logo when hovered */
                       .skin-blue .main-header .logo:hover {
                       background-color: #063970;
                       }

                       /* navbar (rest of the header) */
                       .skin-blue .main-header .navbar {
                       background-color: #063970;
                       }

                       /* active selected tab in the sidebarmenu */
                       .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                       background-color: #063970;
                                 }
                       ')
    ),
    
    ## modify icon size in the sub side bar menu
    tags$style(HTML('
                       /* change size of icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview-menu>li>a>.fa {
                      font-size: 15px;
                      }

                      .sidebar .sidebar-menu .treeview-menu>li>a>.glyphicon {
                      font-size: 13px;
                      }

                      /* Hide icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview>a>.fa-angle-left {
                      display: none;
                      } 
                      '
    )),
    
    tags$style( HTML("hr {border-top: 1px solid #000000;}") ),
    
    ## to not show error message in shiny
    tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
    tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
    
    ## heand dropdown menu size
    #tags$style(HTML('.navbar-custom-menu>.navbar-nav>li>.dropdown-menu { width:100px;}'))
    tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.dropdown-menu { width:10px; font-size:10px; padding:1px; margin:1px;}')),
    tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > h4 {width:0px; font-size:0px; padding:0px; margin:0px;}')),
    tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > p {width:0px; font-size:0px; padding:0px; margin:0px;}'))
  ),
  
  ## 3.1 Dashboard body --------------
  tabItems(
    ## 3.1 Main dashboard ----------------------------------------------------------
    tabItem( tabName = 'dashboard',
             ## contents for the dashboard tab
             
             tags$img(
               src = "https://r4.wallpaperflare.com/wallpaper/1005/822/563/star-wars-death-star-at-at-space-wallpaper-abe6bced53f1bfb9a5847bf93dac5c90.jpg",
               style = 'position: absolute; opacity: 1;',
               height = "91.5%",width="98%"),
             
    ),
    
    tabItem( tabName = 'country_intel',
             ## contents for the dashboard tab
             
             tags$img(
               src = "https://r4.wallpaperflare.com/wallpaper/1005/822/563/star-wars-death-star-at-at-space-wallpaper-abe6bced53f1bfb9a5847bf93dac5c90.jpg",
               style = 'position: absolute; opacity: 1;',
               height = "91.5%",width="98%"),
             
    ),
    
    ## 3.2.1 Export/import commodities/services intelligence ------------------------
    tabItem( tabName = 'ci_exports',
             
             ## 3.3.1 Help text first -------------- 
             tags$img(
               src = "curvecancer.png",
               style = 'position: absolute; opacity: 0.1;',
               height = "30%",width="100%"),
             
             
             sidebarLayout(
               ##### Imagen barra lateral ---------------------------------------
               sidebarPanel(div(img(src = "curvecancer.png", height = 90, width = "100%", align = "center")),
                            width = 3,
                            tags$style(HTML('#goButton {margin-top: 10px}')),
                            #Título                
                            div(h5("Cervical Cancer Incidence", align = "center")),
                            ##### Rango de edad ---------------------------------------                 
                            
                            setSliderColor(c("#0cc1ef", "#f39c13", "#dd513e"),c(1,2,3)),
                            
                            #Formato línea gris
                            tags$hr(style="height:2px;border-width:0;color:gray;background-color:gray"),
                            ##### Raza ---------------------------------------        
                            h5("Select a scenario:", align = "center"),
                            materialSwitch(inputId = "SQ",
                                           label =  "Pre-pandemic", 
                                           status = "success", right = T,
                                           value = T),
                            materialSwitch(inputId = "backslide",
                                           label =  "Covid-19 Backslide", 
                                           status = "success", right = T),
                            materialSwitch(inputId = "HI5yrs",
                                           label = "Healthy Initiative 5 yrs", 
                                           status = "success", right = T),
                            materialSwitch(inputId = "HI10yrs",
                                           label = "Healthy Initiative 10 yrs", 
                                           status = "success", right = T),
                            materialSwitch(inputId = "scn0",
                                           label = "No vaccination", 
                                           status = "success", right = T),
                            #Formato línea gris
                            tags$hr(style="height:2px;border-width:0;color:gray;background-color:gray"),
                            fluidRow(
                              align="center",
                              radioGroupButtons(
                                inputId = "rb_state",
                                label = "Select a state:",
                                choices = list(`<img src="usa-white.png",width="25", 
                                                height="25"></img> US`  = "US", 
                                                `<img src="california-white.png",width="25", 
                                                height="25"></img> California`  = "California",
                                                `<img src="newyork-white.png",width="25", 
                                                height="25"></img> New York`  = "New York", 
                                                `<img src="texas-white.png",width="25", 
                                                height="25"></img> Texas`  = "Texas"),
                                status = "btn",
                                individual = TRUE)),
                            #Formato línea gris
                            tags$hr(style="height:2px;border-width:0;color:gray;background-color:gray"),
                            ##### Periodo ---------------------------------------
                            h5("Select a period:", align = "center"),
                            sliderTextInput(inputId = "period",
                                            label = "",
                                            choices = c(2006:2100),
                                            selected = c(2006:2100)),
                            #Formato línea gris
                            tags$hr(style="height:2px;border-width:0;color:gray;background-color:gray"),
                            #Formato línea gris
                            h4("Plot settings", align = "center", ),
                            ##### Intervalos de confianza ---------------------------------------
                            tags$hr(style="color:white;background-color:white"),
                            materialSwitch(inputId = "percs",
                                           label = "Confidence Intervals", 
                                           status = "primary", value = T),
                            ##### Botón para graficar ---------------------------------------
                            actionButton(inputId = "goButton", 
                                         label =  " Plot Incidence",
                                         width = "100%", 
                                         icon = icon("chart-line", "fa-lg")),
                            #Formato línea gris
                            tags$hr(style="color:white;background-color:white"),
                            ##### Descargar datos ---------------------------------------
                            h5("Download data:", align = "center", ),
                            downloadButton(outputId = "Download_csv", label = "csv", 
                                           icon = icon("file-csv"), 
                                           class = "butt1",
                                           tags$head(tags$style(".butt1{background:#cdcdcb;} 
                                                          .butt1{color: #217346 !important;} 
                                                          .butt1{border-color: #cdcdcb;}
                                                          .butt1{height:4vh;}
                                                          .butt1{width:7vw;}
                                                          .butt1{float:right;};"))),
                            downloadButton(outputId = "Download_r", label = "data", 
                                           icon = icon("r-project", "fa-lg"), 
                                           class = "butt2",
                                           tags$head(tags$style(".butt2{background:#cdcdcb;} 
                                                         .butt2{color: #2165b7 !important;} 
                                                         .butt2{border-color: #cdcdcb;}
                                                         .butt2{height:4vh;}
                                                         .butt2{width:7vw;};"))),
                            tags$hr(style="color:white;background-color:white")), #sidebarPanel final
               #### Panel principal ---------------------------------------  
               mainPanel(
                 ##### Título---------------------------------------     
                 titlePanel(title = h1("Cervical Cancer Incidence", align = "center")),
                 br(),
                 br(),
                 ##### Gráfica---------------------------------------
                 plotlyOutput("plt_foi")%>% withSpinner(color="#0dc5c1", image = "https://c.tenor.com/1_5w5vXEH5gAAAAj/mandalorian-star-wars.gif"))#mainPanel final
             )#sidebarLayout final
             
    ),
    tabItem( tabName = 'ci_imports',
             
             tags$img(
               src = "https://r4.wallpaperflare.com/wallpaper/1005/822/563/star-wars-death-star-at-at-space-wallpaper-abe6bced53f1bfb9a5847bf93dac5c90.jpg",
               style = 'position: absolute; opacity: 1;',
               height = "91.5%",width="98%"),     
             
    ),
    
    
    
    ## 3.2.2 Export/import commodities/services intelligence ------------------------
    
    ## 3.2.3 Quick Intel by HS codes ---------------
    tabItem( tabName = 'ci_intel_by_hs',
             
             tags$img(
               src = "https://r4.wallpaperflare.com/wallpaper/1005/822/563/star-wars-death-star-at-at-space-wallpaper-abe6bced53f1bfb9a5847bf93dac5c90.jpg",
               style = 'position: absolute; opacity: 1;',
               height = "91.5%",width="98%"),
             
    ),
    
    ## 3.3 country intellgence -----------------------------------------------------
    
    ## 3.4 HS code finder ------------------------------
    # tabItem( tabName = 'hs_finder',
    #          div( id = 'hs_code_finder_table' ,
    #               fluidRow( h1( "Level 2, 4 and 6 HS code table" ),
    #                         h3( "How to:"),
    #                         howto_hs_finder(),
    #                         dataTableOutput("HSCodeTable")
    #                         ) 
    #               )
    #          ),
    
    ## 3.5 Help and info -------------------------------
    tabItem( tabName = 'help',
             
             leaflet() %>% #leaflet
               addTiles() %>%
               addProviderTiles('Hydda.Full') %>% 
               setView(lat = -37.8, lng = 144.8, zoom = 5)
             ## 3.5.1 Data sources ---------------
    ),
    
    ## 3.6 Monthly update from Stats NZ --------------
    tabItem( tabName = 'monthly_update',
             tags$img(
               src = "https://c4.wallpaperflare.com/wallpaper/390/39/806/red-white-star-wars-darth-maul-wallpaper-preview.jpg",
               style = 'position: absolute; opacity: 1;',
               height = "91.5%",width="98%"),
             
             
    )
  )
)




ui <- 
  dashboardPage(header, siderbar, body,
                tags$head(tags$style(HTML('* {
  font-family: Trebuchet MS;
}
'))))


server <- function(input, output, session) {
  
  
  #btn_foi
  buttonplot <- eventReactive(input$goButton, {
  })
  years <- reactive({
    seq(input$period[1], input$period[2], by = 1)
  })
  
  
  SQ <- reactive({
    if(input$SQ == T){
      SQ <- "Pre-pandemic"
    }
  })
  backslide <- reactive({
    if(input$backslide == T){
      backslide <- "Covid-19 Backslide"
    }
  })
  HI5yrs <- reactive({
    if(input$HI5yrs == T){
      HI5yrs <- "Healthy Initiative 5 yrs"
    }
  })
  HI10yrs <- reactive({
    if(input$HI10yrs == T){
      HI10yrs <- "Healthy Initiative 10 yrs"
    }
  })
  
  scn0 <- reactive({
    if(input$scn0 == T){
      scn0 <- "No vaccination"
    }
  })
  
  US <- reactive({
    if(input$rb_state == "US"){
      US <- "US"
    }
  })
  CA <- reactive({
    if(input$rb_state == "California"){
      CA <- "CA"
    }
  })
  NY <- reactive({
    if(input$rb_state == "New York"){
      NY <- "NY"
    }
  })
  
  TX <- reactive({
    if(input$rb_state == "Texas"){
      TX <- "TX"
    }
  })
  
  
  
  plt_foi_act <- eventReactive(input$goButton, {
    
    
    plot_incidence(state = c(US(), CA(), NY(), TX()), period = years(),
                            scenarios = c(SQ(), backslide(), HI5yrs(), HI10yrs(), scn0()),
                            percs = input$percs)
  })
  
  
  output$plt_foi <- renderPlotly(plt_foi_act())
  
  
  data_dwn <- reactive({summary_foi(age = age(), sex = c(male(), female()), 
                                    race = c(race1(), race2(), race3(), race4(), race5()), 
                                    period = years(), percs = input$percs)
  })
  
  #  
  #data_dwn_r <- eventReactive(input$Download_r, {
  #    summary_foi(age = input$deslizador, sex = c(male(), female()), 
  #                race = c(race1(), race2(), race3(), race4(), race5()), 
  #                period = years(), percs = input$percs) 
  #                          
  #})
  #
  ##output$df_final <- data_dwn()
  # 
  #
  output$Download_csv <- downloadHandler(
    
    filename = function() { 
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_dwn(), file)
    })
  
  output$Download_r <- downloadHandler(
    
    filename = function() { 
      paste("df_foi_final_", Sys.Date(), ".Rdata", sep="")
    },
    content = function(file) {
      data_r <- data_dwn()
      save(data_r, file=file)
    })
  
  #  observeEvent({
  #   if(input$female == F & input$male == F){
  #     
  #     shinyalert("Oops!", "Something went wrong.", type = "error")
  #   }
  #  })
  
  
  
  #  output$df_final <- renderTable({
  #    
  #    data_dwn()
  #  })
  
  
  #  output$distPlot <- renderPlot({
  #    # generate bins based on input$bins from ui.R
  #    buttonplot()
  #    plot_foi(age = input$deslizador, sex = input$sex, 
  #             race = input$race, 
  #             period = years(), percs = input$percs,
  #             save_data = input$download)
  #  },
  #)
  
  
}







shinyApp(ui, server)