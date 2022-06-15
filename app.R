library(shiny)

login1 <- c("user1", "pw1")
login2 <- c("user2", "pw2")


# Define UI for application that draws a histogram
ui <- fluidPage(
        
        # Application title
        uiOutput("ui")
        
        # Sidebar with a slider input for number of bins 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        
        logged <- reactiveValues(logged = FALSE, user = NULL)
        
        observeEvent(input$signin, {
                if(input$name == "user1" & input$pw == "pw1") {
                        logged$logged <- TRUE
                        logged$user <- "user1"
                } else if (input$name == "user2" & input$pw == "pw2") {
                        logged$logged <- TRUE
                        logged$user <- "user2"
                } else {}
        })
        
        
        output$ui <- renderUI({
                
                
                if(logged$logged == FALSE) {
                        return(
                                tagList(
                                        textInput("name", "Name"),
                                        passwordInput("pw", "Password"),
                                        actionButton("signin", "Sign In")
                                )
                        )
                } else if(logged$logged == TRUE & logged$user == "user1") {
                        return(
                                tagList(
                                        titlePanel("This is user 1 Panel"),
                                        tags$h1("User 1 is only able to see text, but no plots")
                                )
                        )
                } else if(logged$logged == TRUE & logged$user == "user2") {
                        return(
                                tagList(
                                        titlePanel("This is user 2 Panel for Executetives"),
                                        sidebarLayout(
                                                sidebarPanel(
                                                        sliderInput("bins",
                                                                    "Number of bins:",
                                                                    min = 1,
                                                                    max = 50,
                                                                    value = 30)
                                                ),
                                                
                                                
                                                # Show a plot of the generated distribution
                                                mainPanel(
                                                        plotOutput("distPlot")
                                                )
                                        )
                                )
                        )
                } else {}
        })
        
        
        
        output$distPlot <- renderPlot({
                x    <- faithful[, 2]
                bins <- seq(min(x), max(x), length.out = input$bins + 1)
                hist(x, breaks = bins, col = 'darkgray', border = 'white')
        })
}

# Run the application 
shinyApp(ui = ui, server = server)