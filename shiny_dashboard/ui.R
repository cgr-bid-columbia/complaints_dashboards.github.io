library(shiny)
library(shinydashboard)



ui <- dashboardPage(
        dashboardHeader(title = "Prueba"),
        dashboardSidebar(),
        dashboardBody(
                
                sidebarLayout(
                        sidebarPanel(
                                sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100)
                        ),
                        mainPanel(plotOutput("distPlot"))
                )
                
                
                
        )
)

