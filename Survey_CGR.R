library(shiny)
library(shinydashboard)
library(ggplot2)
library(googledrive)
library(googlesheets4)

gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)


# Define the fields we want to save from the form
fields <- c("nacionalidad", "residencia")


#functions

saveData <- function(data, ip, time) {
        # The data must be a dataframe rather than a named vector
        data <- data %>% as.list() %>% data.frame()
        data$ip <- ip 
        data$time <- Sys.time()
        # Add the data as a new row
        sheet_append("1saXE8aAt0ymhsNooGFIJ7Qjd9x3lSv-rmhIQyAkg38Q", data)
}


#HEADER
header <- dashboardHeader(title = "Denuncias CGR")

#SIDEBAR
sidebar <- dashboardSidebar(width = 280,
                            disable = FALSE, #don't show sidebar
                            sidebarMenu(
                                    menuItem("Preguntas generales", tabName = "info_survey"),
                                    menuItem("Información general", tabName = "info", icon=icon("home")),
                                    menuItem("Denuncias 2021", tabName = "claims2021", icon = icon("th")),
                                    menuItem("Denuncias Históricas", tabName = "claims_hist", icon = icon("th"))
                            ) 
)

#BODY
body <- dashboardBody(
        tags$head(tags$style(HTML('
                      .main-header .logo {
                        font-family: "Georgia", Times, "Times New Roman", serif;
                        font-weight: bold;
                        font-size: 24px;
                      }
                    '))),
        
        #Estilo de los tabBox
        tags$style(".nav-tabs {
                                background-color: transparent;   
                                font-family:Georgia;
                                }
                                                
                                .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
                                background-color: #F2F1F1;
                                border-color: transparent;
                                }
                                                
                                .nav-tabs-custom .nav-tabs li.active {
                                border-top-color: red;
                                }"),
        
        #Estilo de los Box
        tags$style(HTML("

                    .box.box-solid.box-primary{
                    border-bottom-color:transparent;
                    border-left-color:transparent;
                    border-right-color:transparent;
                    border-top-color:transparent;
                    background:#F2F1F1
                    }

                    ")),
        
        # get IP user using Json's function
        tags$script('$(document).on("shiny:sessioninitialized",function(){$.get("https://api.ipify.org", function(response) {Shiny.setInputValue("getIP", response);});})'),
        
        
        tabItems(
                ###################
                #Información general
                ###################
                tabItem(tabName = "info_survey",
                
                        radioButtons("nacionalidad", label = "¿Cuál es su nacionalidad",
                                     choices = c("peruana", "otra") ),
                        
                        conditionalPanel(
                                condition = "input.nacionalidad == 'otra'",
                                
                                
                                radioButtons("residencia", label = "¿Cuenta con residencia permanente peruana?",
                                             choices = c("sí", "no") )
                                

                        ),
                        
                        actionButton("submit", "Enviar")
                        
                
                        
                ),

        
                tabItem(tabName = "info", 
                        
                        fluidRow(
                                
                                box( title = "Información General",
                                     status = "danger", solidHeader = FALSE,
                                     height = "300px",  width = 12, 
                                     
                                     p(br(),"¡¡Bienvenidos!!", br(),br(),
                                       "La idea de esta aplicación web es presentar de manera ordenada y fácil de entender el progreso en la codificación de denuncias, 
                                              así como estadísticos que permitan comprender los tipos de casos que han sido identificados y el progreso de la Contraloría General de la República (CGR)
                                              en la evaluación de las denuncias recibidas. Si bien el desarrollo inicial ha estado en manos del equipo académico, se espera que mediante una estrecha coordinación
                                              con la CGR, parte de los estadísticos que incorporará el Dashboard sean recomendados por especialistas internos de la mencionada institución"
                                       , style = "text-align:justify; color: #14505B ; font-size: 18px")
                                )
                                
                        ),
                        
                        fluidRow(
                                
                                
                                box( title = "Sobre la CGR",
                                     status = "danger", solidHeader = FALSE,
                                     height = "300px",  width = 6, 
                                     
                                     p(br(),"La CGR verifica la correcta aplicación de las políticas públicas y el uso de los recursos y bienes del Estado, 
                                              a través de gerencias regionales de control, los Órganos de Control Institucional (OCI) y las Sociedades de Auditorías (SOA)"
                                       , style = "text-align:justify; color: #14505B ; font-size: 18px")
                                ),
                                
                                
                                box( title = "Sobre la Gerencia de Control Social y Denuncia",
                                     status = "danger", solidHeader = FALSE,
                                     height = "300px",  width = 6, 
                                     
                                     p(br(),"La Gerencia de Control Social y Denuncias busca promover el control social y el fortalecimiento del Sistema Nacional de Control
                                              mediante normas que regulen la atención de la demanda imprevisible de control proveniente de denuncias recibidas y/o autogeneradas,
                                              mecanismos de participación ciudadana, mecanismos de análisis, así como de los servicios de control resultantes"
                                       , style = "text-align:justify; color: #14505B ; font-size: 18px")
                                )
                                
                                
                                
                        )
                        
                        
                        
                ),
                ####################
                # Denuncias 2021
                ####################
                tabItem(tabName = "claims2021",
                        
                        
                ),
                #####################
                # Denuncias históricas
                ####################
                tabItem(tabName = "claims_hist",
                        
                        
                        
                )
                
                
                
        )
)




shinyApp(
        ui = dashboardPage(skin = "red",
                           header, sidebar, body),
        
        server = function(input, output) {
                
                
                #get IP from user
                ip_user <- reactive(input$getIP)
                
                # Whenever a field is filled, aggregate all form data
                formData <- reactive({
                        data <- sapply(fields, function(x) input[[x]])
                        
                        #TODO_ CREATE A CONDITION 
                        #if input[[x]] == ""  then data$field == ""
                        
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
)