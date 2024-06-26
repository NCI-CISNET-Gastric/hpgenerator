sidebar_age_to_inf <- sidebarPanel(id = "sidebar_age_to_inf",
             useShinyjs(),
             style = "margin-top: -15px;
                      margin-left: 10px;
                      background: #AB3126 !important; 
                      width:60%;
                      border-radius: 15px;
                      border-color: #F0C0BC;
                      box-shadow: 8px 8px 22px 2px rgba(240,192,188,1);", 
             div(style = "text-align: center;", 
                 icon("people-line", class = "fa-2x")),
             width = 4,
             tags$style(HTML('#goButton_age_to_inf {margin-top: 10px}')),
             div(h5("Age to Infection", align = "center")),
             tags$hr(style = "background-color: white !important;"),
             h5("Choose a population size:", align = "center"),
             numericInput(inputId = "numeric_pop", label = "", min=1, max = 20000, value=10000),
             tags$hr(style = "background-color: white !important;"),
             h5("Select a race:", align = "center"),
             materialSwitch(inputId = "race1_ati", label = "Hispanic", status = "success", right = T, value = T),
             materialSwitch(inputId = "race2_ati", label = "Non-Hispanic American Indian", status = "success", right = T),
             materialSwitch(inputId = "race3_ati", label = "Non-Hispanic Asian", status = "success", right = T),
             materialSwitch(inputId = "race4_ati", label = "Non-Hispanic Black", status = "success", right = T),
             materialSwitch(inputId = "race5_ati", label = "Non-Hispanic White", status = "success", right = T),
             tags$hr(style = "background-color: white !important;"),
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
             h5("Select a cohort:", align = "center"),
             sliderTextInput(inputId = "cohort_ati",
                             label = "",
                             choices = c(1940:1998),
                             selected = c(1940:1998)),
             tags$hr(style = "background-color: white !important;"),
             actionButton(inputId = "goButton_age_to_inf", 
                          label =  " Plot Age to Infection",
                          width = "100%", 
                          icon = icon("disease", "fa-lg")),
             tags$hr(style = "background-color: white !important;"),
             h5("Download data", align = "center", ),
             div(class = "button-row-age-to-inf",
                 downloadButton(outputId = "Download_csv_ati", label = "csv", 
                                icon = icon("file-csv", "fa-lg"), 
                                class = "butt1_ati"),
                 downloadButton(outputId = "Download_r_ati", label = "data", 
                                icon = icon("r-project", "fa-lg"), 
                                class = "butt2_ati")),
)