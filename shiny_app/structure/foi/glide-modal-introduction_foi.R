glide_modal_introduction_foi <- function(input, output, session) {
  
observe({
  if (input$toggleSidebar) {
    shinyjs::show(id = "Sidebar")
  } else {
    shinyjs::hide(id = "Sidebar")
  }
})

modal_controls <- glideControls(
  list(
    prevButton(),
    firstButton(
      class = "btn btn-danger",
      `data-dismiss`="modal",
      "No, Thanks."
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
  title = p(icon("bacterium"), em("Helicobacter Pylori"), ("Infection Generator")),
  easyClose = FALSE,
  footer = NULL,
  glide(
    custom_controls = modal_controls,
    screen(
      next_label = 'Start the introduction <span class="glyphicon glyphicon-chevron-right" aria-hidden="true"></span>',
      HTML('<center> <img src="https://studikard.com/wp-content/uploads/2022/11/Helicobacter-pylori.png" width="20%" height="20%"> </center>'),
      p( em("Helicobacter pylori (H. pylori)"), ("is a type of bacteria that can live in the digestive tract. This bacteria can lead to serious diseases including peptic ulcers and stomach (gastric) cancer."), 
         hr(),("Our Shiny app brings"), (em("H. pylori ")), (" data to life, offering an interactive platform to analyze the force of infection among different race groups including Hispanics, Non-Hispanic American Indians, Non-Hispanic Asians, Non-Hispanic Blacks and Non-Hispanic Whites. It's an invaluable tool for public health officials, medical researchers, and anyone interested in gaining a comprehensive understanding of the impact and spread of"), (em("H. pylori ")), (" in the United States."),
         hr()
      )),
    screen(
      tags$div(style = "text-align:center;", 
               p("First, please select an age interval"),
               sliderInput(inputId = "deslizador_obs", label =  "", 
                           min = 0, max = 90, value = c(0,90))
               
      )),
    screen(tags$div(style = "text-align:center;", p("Next, please select a race")),
           tags$div(
             materialSwitch(inputId = "race1_obs",
                            label = "Hispanic", 
                            status = "success", right = T,
                            value = T),
             materialSwitch(inputId = "race2_obs",
                            label = "Non-Hispanic American Indian", 
                            status = "success", right = T),
             materialSwitch(inputId = "race3_obs",
                            label = "Non-Hispanic Asian", 
                            status = "success", right = T),
             materialSwitch(inputId = "race4_obs",
                            label = "Non-Hispanic Black", 
                            status = "success", right = T),
             materialSwitch(inputId = "race5_obs",
                            label = "Non-Hispanic White", 
                            status = "success", right = T)
           )),
    screen(
      tags$div(style = "text-align:center;", 
               p("Next, please select a cohort"),
               sliderTextInput(inputId = "cohort_obs",
                               label = "",
                               choices = c(1940:1998),
                               selected = c(1940:1998))
               
      )),
    screen(
      p("Thanks, we're all set ! \n Remember to plot the force of infection")
    )
  )
)

showModal(glide_modal)



observe({
  updateSliderInput(session, "deslizador", value = input$deslizador_obs)
})

observe({
  updateMaterialSwitch(session, "race1", value = input$race1_obs)
})

observe({
  updateMaterialSwitch(session, "race2", value = input$race2_obs)
})

observe({
  updateMaterialSwitch(session, "race3", value = input$race3_obs)
})

observe({
  updateMaterialSwitch(session, "race4", value = input$race4_obs)
})

observe({
  updateMaterialSwitch(session, "us", value = input$us_obs)
})

observe({
  updateMaterialSwitch(session, "foreign", value = input$foreign_obs)
})

observe({
  updateSliderTextInput(session, "cohort",choices = c(1940:1998),
                        selected = input$cohort_obs)
})

observe({
  updateActionButton(session, "goButton_obs")
})

}