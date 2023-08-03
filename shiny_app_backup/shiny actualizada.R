library(shiny)
#library(hpgenerator)
library(shinyWidgets)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(shinyalert)
library(shinyglide)

source(file = "global.R")

library(rintrojs)

# User Interface ------------------------------------------------------ 
ui <- shinyUI(fluidPage(includeCSS("www/css/custom.css"),
                        introjsUI(), 
                        
# Title of navbar ------------------------------------------------------                       
                        navbarPage(em("Helicobacter Pylori"), selected = strong("Force of Infection"), collapsible = TRUE, inverse = TRUE, theme = shinytheme("yeti"),
# Prevalence tab ------------------------------------------------------     
                           tabPanel(strong("Prevalence"), icon = icon("chart-area", "fa-lg"), 
                                    fluidPage(titlePanel(title = h1("Under construction :)", align = "center")))),
# Prevalence tab ------------------------------------------------------     
                           tabPanel(strong("Force of Infection"), icon = icon("chart-line", "fa-lg"),              
                                    fluidPage(
                                      introBox(
                                        titlePanel(title = h1("Force of Infection", align = "center")),
                                        data.step = 1,
                                        data.intro = "This is the title panel"
                                      ),
                                      sidebarLayout(
                                        # La barra lateral
                                        sidebarPanel(div(img(src = "foi.png", height = 80, width = "100%", align = "right")),#, style="text-align: center;"),
                                                     width = 3,
                                                     tags$style(HTML('#goButton {margin-top: 10px}')),
                                                     div(h5("Force of infection", align = "center")),
                                                     tags$hr(style="height:2px;border-width:0;color:gray;background-color:gray"),
                                                     h5("Select a Age interval:", align = "center"),
                                                     setSliderColor(c("#0cc1ef", "#f39c13", "#dd513e"),c(1,2,3)),
                                                     introBox(
                                                     sliderInput(inputId = "deslizador", label =  "", 
                                                                 min = 0, max = 100, value = c(0,100), step = 1),
                                                     
                                                     data.step = 1,
                                                     data.intro = "This is a slider",
                                                     data.hint = "You can slide me"),
                                                     tags$hr(style="height:2px;border-width:0;color:gray;background-color:gray"),
                                                     h5("Select a race:", align = "center"),
                                                     materialSwitch(inputId = "race1",
                                                                    label = "Non-Hispanic White", 
                                                                    status = "success", right = T,
                                                                    value = T),
                                                     materialSwitch(inputId = "race2",
                                                                    label = "Non-Hispanic Black", 
                                                                    status = "success", right = T),
                                                     materialSwitch(inputId = "race3",
                                                                    label = "Other Hispanic", 
                                                                    status = "success", right = T),
                                                     materialSwitch(inputId = "race4",
                                                                    label = "Mexican-American", 
                                                                    status = "success", right = T),
                                                     materialSwitch(inputId = "race5",
                                                                    label = "Other", 
                                                                    status = "success", right = T),
                                                     tags$hr(style="height:2px;border-width:0;color:gray;background-color:gray"),
                                                     #switchInput(inputId = "race1",
                                                     #            label = "Non-Hispanic White",
                                                     #            onLabel = "ON",
                                                     #            offLabel = "OFF",
                                                     #            onStatus = "danger",
                                                     #            offStatus = "info", value = F),
                                                     #selectInput(inputId = "race", #checkbox
                                                     #            label = "Choose a race:",
                                                     #            choices = c("Non-Hispanic White", 
                                                     #                        "Non-Hispanic Black", 
                                                     #                        "Other Hispanic", 
                                                     #                        "Mexican-American", 
                                                     #                        "Other"),
                                                     #            multiple = T, selected = "Non-Hispanic White"),
                                                     h5("Select a sex:", align = "center"),
                                                     materialSwitch(inputId = "male",
                                                                    label = "Male", 
                                                                    status = "warning", right = T),
                                                     materialSwitch(inputId = "female",
                                                                    label = "Female", 
                                                                    status = "warning", right = T),
                                                     tags$hr(style="height:2px;border-width:0;color:gray;background-color:gray"),
                                                     h5("Select a period:", align = "center"),
                                                     sliderTextInput(inputId = "period",
                                                                     label = "",
                                                                     choices = c(1991:2010),
                                                                     selected = c(1991:2010)),
                                                     tags$hr(style="height:2px;border-width:0;color:gray;background-color:gray"),
                                                     h4("Plot settings", align = "center", ),
                                                     tags$hr(style="color:white;background-color:white"),
                                                     materialSwitch(inputId = "percs",
                                                                    label = "Confidence Intervals", 
                                                                    status = "primary", value = T),
                                                     sliderInput(inputId = "bracket", 
                                                                 label = "", 
                                                                 min = 0, 
                                                                 max = 50, value = 10, ),
                                                     h5("Age brackets", align = "center"),
                                                     actionButton(inputId = "goButton", 
                                                                  label =  " Plot Force of Infection",
                                                                  width = "100%", 
                                                                  icon = icon("chart-line", "fa-lg")),
                                                     tags$hr(style="color:white;background-color:white"),
                                                     h5("Download data:", align = "center", ),
                                                     downloadButton(outputId = "Download_csv", label = "csv", 
                                                                    icon = icon("file-csv", "fa-lg"), 
                                                                    class = "butt1",
                                                                    tags$head(tags$style(".butt1{background:#cdcdcb;} 
                                                          .butt1{color: #217346 !important;} 
                                                          .butt1{border-color: #cdcdcb;}
                                                          .butt1{height:4vh;}
                                                          .butt1{width:9vw;}
                                                          .butt1{float:right;};"))),
                                                     downloadButton(outputId = "Download_r", label = "data", 
                                                                    icon = icon("r-project", "fa-lg"), 
                                                                    class = "butt2",
                                                                    tags$head(tags$style(".butt2{background:#cdcdcb;} 
                                                         .butt2{color: #2165b7 !important;} 
                                                         .butt2{border-color: #cdcdcb;}
                                                         .butt2{height:4vh;}
                                                         .butt2{width:9vw;};"))),
                                                     tags$hr(style="color:white;background-color:white"),
                                        ),
                                        mainPanel(
                                          #h1(textOutput("txt_foi"), align = "center"),
                                          #h2("subtítulo central"),
                                          # Irán los outputs
                                          plotOutput("plt_foi", width = "70vw",
                                                     height = "70vh")%>% withSpinner(color="#0dc5c1"),
                                          
                                        )
                                        
                                      )
                                    )
                           ),
                           tabPanel(strong("Hexamaps"), icon = icon("map", "fa-lg"),
                                    fluidPage(titlePanel(title = h1("Under construction :)", align = "center")))),
                           tabPanel(strong("About"), icon = icon("info", "fa-lg"),
                                    fluidPage(titlePanel(title = h1("Cancer Intervention and Surveillance Modeling Network", align = "center")),
                                              br(),
                                              br(),
                                              br(),
                                              mainPanel(
                                                (div(img(src = "cisnet.png", height = "140vh", width = "140vh"), style= 'position:absolute; right:160px;')
                                                )),
                                              div(style = "margin-bottom:260px; "),
                                              h4("Shiny App: Jorge Roa", align = "center"),
                                              br(),
                                              h4("Creators: Fernando Alarid-Escudero", align = "center"),
                                              h4("Jorge Roa", align = "center")))
)
)
)



# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  
  modal_controls <- glideControls(
    list(
      prevButton(),
      firstButton(
        class = "btn btn-danger",
        `data-dismiss`="modal",
        "No, thanks !"
      )
    ),
    list(
      nextButton(),
      lastButton(
        class = "btn btn-success",
        `data-dismiss`="modal",
        "Done"
      )
    )
  )
  
  glide_modal <- modalDialog(
    title = "Startup assistant",
    easyClose = FALSE,
    footer = NULL,
    glide(
      custom_controls = modal_controls,
      screen(
        next_label = 'Yes, please ! <span class="glyphicon glyphicon-chevron-right" aria-hidden="true"></span>',
        p("Let's initialize some values, would you ?")
      ),
      screen(
        p("First, please select a mean value"),
        numericInput("mean_modal", "Mean", value = 0)
      ),
      screen(
        p("Next, please select a standard deviation value"),
        numericInput("sd_modal", "Standard deviation", value = 1, min = 0)
      ),
      screen(
        p("Thanks, we're all set !")
      )
    )
  )
  
  showModal(glide_modal)
  
  hintjs(session, options = list("hintButtonLabel"="Hope this hint was helpful"),
         events = list("onhintclose"=I('alert("Wasn\'t that hint helpful")')))
  
  #btn_foi
  buttonplot <- eventReactive(input$goButton, {
  })
  years <- reactive({
    seq(input$period[1], input$period[2], by = 1)
  })
  
  
  race1 <- reactive({
    if(input$race1 == T){
      race1 <- "Non-Hispanic White"
    }
  })
  race2 <- reactive({
    if(input$race2 == T){
      race2 <- "Non-Hispanic Black"
    }
  })
  race3 <- reactive({
    if(input$race3 == T){
      race3 <- "Other Hispanic"
    }
  })
  race4 <- reactive({
    if(input$race4 == T){
      race4 <- "Mexican-American"
    }
  })
  
  race5 <- reactive({
    if(input$race5 == T){
      race5 <- "Other"
    }
  })
  
  male <- reactive({
    if(input$male == T){
      male <- "Male"
    }
  })
  
  female <- reactive({
    if(input$female == T){
      female <- "Female"
    }
  })
  
  age <- reactive({
    seq(input$deslizador[1], input$deslizador[2], by = 1)
  })
  
  
  #txt_title_foi <- eventReactive(input$goButton, {
  #  
  #  if (input$sex==2){
  #    paste("FOI of", input$sex[1], "and", input$sex[2])
  #  } else {
  #    paste("FOI of", input$sex)
  #  }
  #  
  #})
  #output$txt_foi <- renderText(txt_title_foi())
  #
  plt_foi_act <- eventReactive(input$goButton, {
    
    plot_foi_shiny(age = input$deslizador, sex = c(male(), female()), 
                   race = c(race1(), race2(), race3(), race4(), race5()), 
                   period = years(), percs = input$percs, age_bracket = input$bracket)
    #save_data = input$download)
    
  })
  output$plt_foi <- renderPlot(plt_foi_act())
  
  
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
  
  # start introjs when button is pressed with custom options and events
  observeEvent(input$help,
               introjs(session, options = list("nextLabel"="Onwards and Upwards",
                                               "prevLabel"="Did you forget something?",
                                               "skipLabel"="Don't be a quitter"),
                       events = list("oncomplete"=I('alert("Glad that is over")')))
  )
  
  #observeEvent(input$plt_foi, {
  #  
  #  if(input$female == F & input$male == F){
  #    
  #    shinyalert("Oops!", "Something went wrong.", type = "error")
  #  }
  #  
  #})
  
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



