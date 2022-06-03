library(shiny)
library(shinysurveys)
library(ggplot2)

library(googledrive)
library(googlesheets4)

gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)


# Define the fields we want to save from the form
fields <- c("favorite_food")


#functions

saveData <- function(data, ip) {
        # The data must be a dataframe rather than a named vector
        data <- data %>% as.list() %>% data.frame()
        data$ip <- ip
        # Add the data as a new row
        sheet_append("1saXE8aAt0ymhsNooGFIJ7Qjd9x3lSv-rmhIQyAkg38Q", data)
}

loadData <- function() {
        # Read the data
        read_sheet("1saXE8aAt0ymhsNooGFIJ7Qjd9x3lSv-rmhIQyAkg38Q")
}


df <- data.frame(question = "What is your favorite food?",
                 option = "Your Answer",
                 input_type = "text",
                 input_id = "favorite_food",
                 dependence = NA,
                 dependence_value = NA,
                 required = F)

ui <- fluidPage(
        # get IP user using Json's function
        tags$script('$(document).on("shiny:sessioninitialized",function(){$.get("https://api.ipify.org", function(response) {Shiny.setInputValue("getIP", response);});})'),
        
        
        surveyOutput(df = df,
                     survey_title = "Hello, World!",
                     survey_description = "Welcome! This is a demo survey showing off the {shinysurveys} package."),
        
)

server <- function(input, output, session) {
        renderSurvey()
        
        #get IP from user
        ip_user <- reactive(input$getIP)
        
        # Whenever a field is filled, aggregate all form data
        formData <- reactive({
                data <- sapply(fields, function(x) input[[x]])
                data
        })
        
        
        # When the Submit button is clicked, save the form data
        observeEvent(input$submit, {
                saveData(formData(), ip_user() )
                
        })

        observeEvent(input$submit, {
                showModal(modalDialog(
                        title = "Congrats, you completed your first shinysurvey!",
                        "You can customize what actions happen when a user finishes a survey using input$submit."
                ))
        })
}


shinyApp(ui, server)