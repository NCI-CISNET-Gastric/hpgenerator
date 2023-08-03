library(shiny)
#library(hpgenerator)
library(shinyWidgets)
library(shinythemes)
library(shinycssloaders)
library(tidyverse)

# User Interface ------------------------------------------------------ 
ui <- fluidPage(#theme = themeSelector(),
#  tags$head(
#    tags$style(
#      HTML(
#        "
#          .form-group {
#            margin-bottom: 0 !important;
#          }
#        "
#      )
#    )
#  ),
  
#tags$head(
#  tags$style(type="text/css", ºqs
#             "label.control-label, 
#                                           .selectize-control.single { 
#                                              text-align: center; 
#                                              vertical-align: middle;} 
#                                            label.control-label {
#                                            padding-left: 5vw;}
#                                          .selectize-control.single div.item {
#                                            padding-left: 60vw;}")),
#
  
                theme = shinytheme(theme = "spacelab"), 
                titlePanel(title = h1("Force of Infection", align = "center")),

                 sidebarLayout(
                   # La barra lateral
                   sidebarPanel(width = 3,
                                tags$style(HTML('#goButton {margin-top: 10px}')),
                                div(HTML("Force of Infection")),
                                br(),
                     #h1("titulo lateral"),
                     #h2("subtítulo lateral"),
                     tags$hr(style="height:2px;border-width:0;color:gray;background-color:gray"),
                     h5("Select a Age interval:", align = "center"),
                     setSliderColor(c("#0cc1ef", "#f39c13", "#dd513e"),c(1,2,3)),
                     sliderInput(inputId = "deslizador",
                                 label =  "",
                                 min = 0,
                               max = 100,
                                 value = c(0,100),
                                 step = 1),
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
                     #checkboxGroupInput(inputId = "sex",
                     #            label = "Select a sex:",
                     #            choices = c("Male", "Female"),inline = T),
                     h5("Select a period:", align = "center"),
                     sliderTextInput(inputId = "period",
                                 label = "",
                                 choices = c(1991:2010),
                                selected = c(1991:2010)),
                     tags$hr(style="height:2px;border-width:0;color:gray;background-color:gray"),
                     h4("Plot Specifications", align = "center", ),
                     tags$hr(style="color:white;background-color:white"),
                     materialSwitch(inputId = "percs",
                                    label = "Confidence Intervals", 
                                    status = "primary", value = T),
                     #radioGroupButtons(inputId = "bracket",
                     #                  label = "Age brackets:", 
                     #                  size = "xs",
                     #                  justified = T,
                     #                  choices = list("1" = 1, "5" = 5, 
                     #                                 "10" = 10, "25" = 25,
                     #                                 "50" = 50),
                     #                  status = "primary"),
                     sliderInput(inputId = "bracket", 
                                 label = "", 
                                 min = 0, 
                                 max = 50, value = 10, ),
                     h5("Age brackets", align = "center"),
                     #selectInput(inputId = "download",
                     #            label = "Download Data",
                     #            choices = list("Yes" = T,
                     #                           "No" = F)),
                     actionButton(inputId = "goButton", 
                                  label =  " Plot Force of Infection",
                                  width = "100%", 
                                  icon = icon("chart-line", "fa-lg")),
                     tags$hr(style="color:white;background-color:white"),
                     h5("Download data:", align = "center", ),
                     downloadButton(outputId = "Download_csv", label = "csv", icon = icon("file-csv", "fa-lg"), class = "butt1",
                                    tags$head(tags$style(".butt1{background:#cdcdcb;} 
                                                          .butt1{color: #217346 !important;} 
                                                          .butt1{border-color: #cdcdcb;}
                                                          .butt1{height:4vh;}
                                                          .butt1{width:9vw;}
                                                          .butt1{float:right;};"))),
                     downloadButton(outputId = "Download_r", label = "data", icon = icon("r-project", "fa-lg"), class = "butt2",
                                    tags$head(tags$style(".butt2{background:#cdcdcb;} 
                                                         .butt2{color: #2165b7 !important;} 
                                                         .butt2{border-color: #cdcdcb;}
                                                         .butt2{height:4vh;}
                                                         .butt2{width:9vw;};"))),
                     tags$hr(style="color:white;background-color:white"),
                     ),
                   # El panel central
                   mainPanel(
                    #h1(textOutput("txt_foi"), align = "center"),
                    #h2("subtítulo central"),
                     # Irán los outputs
                     plotOutput("plt_foi", width = "70vw",
                                height = "70vh")%>% withSpinner(color="#0dc5c1"),
                     
                   )
                 )  
                )



# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
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



