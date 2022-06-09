library(shiny)
library(shinydashboard)
library(ggplot2)
library(googledrive)
library(googlesheets4)
library(shinyWidgets) #todo: install on AWS
library(shinyjs) #todo: install on AWS
library(dplyr)
library(plotly)
library(reactable)
library(htmltools)
library(ggplot2)


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

# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
        bar <- div(style = list(background = fill, width = width, height = height))
        chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
        div(style = list(display = "flex", alignItems = "center"), label, chart)
}


gaugeSectors <- function(success = NULL, warning = NULL, danger = NULL,
                         colors = c("success", "warning", "danger")) {
        list(success = success,
             warning = warning,
             danger = danger,
             colors = colors)
}


#upload data
dep_prov_dist <- read.csv("dep_prov_dist.csv", encoding = "UTF-8")
colnames(dep_prov_dist) <- c("DEPARTAMENTO", "PROVINCIA", "DISTRITO")

raw_historical_claims <- read.csv('raw_historical_claims_dash.csv')
corruption_claims_count_historical <- read.csv('corruption_claims_count_claims_historical.csv')


#HEADER
header <- dashboardHeader(title = "Denuncias CGR")

#SIDEBAR
sidebar <- dashboardSidebar(width = 280,
                            disable = TRUE, #don't show sidebar
                            sidebarMenu(id = "tabs",
                                    menuItem("Cuestionario Inicial", tabName = "info_survey"),
                                    menuItem("Estadísticas", tabName = "statistics"),
                                    menuItem("Cuestionario Percepción", tabName = "perception_survey")
                            ) 
)

#BODY
body <- dashboardBody(
        
        useShinyjs(), #it allows to use java script
        
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
                #Cuestionario inicial
                ###################
                tabItem(tabName = "info_survey",
                        
                        fluidRow(
                                
                                column(5, align="center",
                                       
                                       
                                       p(br(),"¡¡Bienvenidos!!", br(),br(),
                                         "La idea de esta aplicación web es presentar de manera ordenada y fácil de entender el progreso en la codificación de denuncias, 
                                              así como estadísticos que permitan comprender los tipos de casos que han sido identificados y el progreso de la Contraloría General de la República (CGR)
                                              en la evaluación de las denuncias recibidas. Si bien el desarrollo inicial ha estado en manos del equipo académico, se espera que mediante una estrecha coordinación
                                              con la CGR, parte de los estadísticos que incorporará el Dashboard sean recomendados por especialistas internos de la mencionada institución"
                                         , style = "text-align:justify;color:black;background-color:#fcebeb;padding:15px;border-radius:10px"),
                                       
                                       
                                       br(),
                                       
                                       p("Instrucciones:
                                       Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum"
                                       ,
                                         style="text-align:justify;color:black;background-color:#fcebeb;padding:15px;border-radius:10px")
        
                                       
                                       ),
                                
                                column(7, align="left",
                                       
                                       tabBox(
                                               id = "questions_1", width = 12, title = "Cuestionario inicial",
                                               
                                               # PREGUNTAS GENERALES
                                               tabPanel("Generales",

                                                        #1.
                                                        br(),
                                                        radioGroupButtons(
                                                                inputId = "nacionality",
                                                                label = "1. Nacionalidad (*)",
                                                                choices = c("", 
                                                                            "peruana", "otra"),
                                                                individual = TRUE,
                                                                checkIcon = list(
                                                                        yes = tags$i(class = "fa fa-circle", 
                                                                                     style = "color: steelblue"),
                                                                        no = tags$i(class = "fa fa-circle-o", 
                                                                                    style = "color: steelblue"))
                                                        ),
                                                        
                                                        
                                                        conditionalPanel(
                                                                condition = "input.nacionality == 'otra'",
                                                                
                                                                fluidRow(
                                                                        
                                                                        column(6,
                                                                               br(),
                                                                               radioGroupButtons(
                                                                                       inputId = "nacionality_2",
                                                                                       label = "1.1 ¿Cuenta con residencia permanente peruana? (*)",
                                                                                       choices = c("sí", "no"),
                                                                                       individual = TRUE,
                                                                                       checkIcon = list(
                                                                                               yes = tags$i(class = "fa fa-circle", 
                                                                                                            style = "color: steelblue"),
                                                                                               no = tags$i(class = "fa fa-circle-o", 
                                                                                                           style = "color: steelblue"))
                                                                               )
                                                                               
                                                                        ),
                                                                        
                                                                        column(6,
                                                                               br(),
                                                                               textInput(
                                                                                       inputId = "nacionality_3",
                                                                                       label = "1.2 ¿Cuál es su nacionalidad?",
                                                                                       value = "Opcional",
                                                                                       
                                                                               )
                                                                        )
                                                                        
                                                                )
 
                                                        ),
                                                        
                                                        #2.
                                                        br(),
                                                        radioGroupButtons(
                                                                inputId = "age",
                                                                label = "2. Rango de edad (*)",
                                                                choices = c("", 
                                                                            "18 a 34 años", "35 a 54 años", "55 a 74 años", "75 años a más"),
                                                                individual = TRUE,
                                                                checkIcon = list(
                                                                        yes = tags$i(class = "fa fa-circle", 
                                                                                     style = "color: steelblue"),
                                                                        no = tags$i(class = "fa fa-circle-o", 
                                                                                    style = "color: steelblue"))
                                                        ),
                                                        
                                                        #3.
                                                        br(),
                                                        radioGroupButtons(
                                                                inputId = "gender",
                                                                label = "3. Género (*)",
                                                                choices = c("", 
                                                                            "mujer", "hombre", "otro"),
                                                                individual = TRUE,
                                                                checkIcon = list(
                                                                        yes = tags$i(class = "fa fa-circle", 
                                                                                     style = "color: steelblue"),
                                                                        no = tags$i(class = "fa fa-circle-o", 
                                                                                    style = "color: steelblue"))
                                                        ),
                                                        
                                                        #4.
                                                        br(),
                                                        radioGroupButtons(
                                                                inputId = "education",
                                                                label = "4. Nivel educativo (*)",
                                                                choices = c("", 
                                                                            "ninguna", "primaria", "secundaria", "superior técnica", "universitaria"),
                                                                individual = TRUE,
                                                                checkIcon = list(
                                                                        yes = tags$i(class = "fa fa-circle", 
                                                                                     style = "color: steelblue"),
                                                                        no = tags$i(class = "fa fa-circle-o", 
                                                                                    style = "color: steelblue"))
                                                        ),
                                                        
                                                        conditionalPanel(
                                                                condition = "input.education!= 'ninguna' & input.education!= '' ",
                                                                
                                                                br(),
                                                                p("Desmarque el botón si es que el nivel educativo seleccionado está incompleto."),

                                                                materialSwitch(
                                                                        inputId = "education_2",
                                                                        label = "Completo", 
                                                                        value = TRUE,
                                                                        status = "primary"
                                                                )
                                                                
                                                        ),
 
                                                        #5
                                                        br(),
                                                        radioGroupButtons(
                                                                inputId = "work",
                                                                label = "5. Dedicación principal (*)",
                                                                choices = c("", 
                                                                            "trabajador", "estudiante", "cuidado del hogar", "jubilado/pensionista/incapacitado", "otro"),
                                                                individual = TRUE,
                                                                checkIcon = list(
                                                                        yes = tags$i(class = "fa fa-circle", 
                                                                                     style = "color: steelblue"),
                                                                        no = tags$i(class = "fa fa-circle-o", 
                                                                                    style = "color: steelblue"))
                                                        ),
                                                        
                                                        conditionalPanel(
                                                                condition = "input.work== 'otro'",
                                                                
                                                                br(),
                                                                textInput(
                                                                        inputId = "work_2",
                                                                        label = "5.1 Especifique a qué otra actividad se dedica principalmente",
                                                                        value = "Opcional"
                                                                )
                                                                
                                                        ),
                                                        
                                                        
                                                        
                                                        
                                               ),
                                               
                                               #PREGUNTAS SOBRE UBICACIÓN
                                               tabPanel("Ubicación", 
                                                        
                                                        #6
                                                        br(),
                                                        radioGroupButtons(
                                                                inputId = "residence",
                                                                label = "6. ¿Reside en el Perú? (*)",
                                                                choices = c("", 
                                                                            "sí", "no"),
                                                                individual = TRUE,
                                                                checkIcon = list(
                                                                        yes = tags$i(class = "fa fa-circle", 
                                                                                     style = "color: steelblue"),
                                                                        no = tags$i(class = "fa fa-circle-o", 
                                                                                    style = "color: steelblue"))
                                                        ),
                                                        
                                                        conditionalPanel(
                                                                condition = "input.residence== 'sí'",
                                                                
                                                                fluidRow(
                                                                        
                                                                        br(),
                                                                        column(3,
                                                                               
                                                                               pickerInput(
                                                                                       inputId = "dep",
                                                                                       label = "Departamento (*)", 
                                                                                       choices = c(unique(dep_prov_dist$DEPARTAMENTO)),
                                                                                       options = list(
                                                                                               title = "Selecciona")
                                                                               )  
                                                                               
                                                                               # selectizeInput('dep', 'Departamento: (*)', choices = c(unique(dep_prov_dist$DEPARTAMENTO))),
                                                                        ),
                                                                        
                                                                        column(3,
                                                                               
                                                                               pickerInput(
                                                                                       inputId = "prov",
                                                                                       label = "Provincia (*)", 
                                                                                       choices = NULL,
                                                                                       options = list(
                                                                                               title = "Selecciona")
                                                                               )
                                                                               
                                                                               #selectizeInput('prov', 'Provincia: (*)', choices = NULL),
                                                                        ),
                                                                        
                                                                        column(3,
                                                                               
                                                                               pickerInput(
                                                                                       inputId = "dist",
                                                                                       label = "Distrito (*)", 
                                                                                       choices = NULL,
                                                                                       options = list(
                                                                                               title = "Selecciona")
                                                                               )
                                                                               #selectizeInput('dist', 'Distrito: (*)', choices = NULL),
                                                                        )
                                                                        
                                                                )
  
                                                                
                                                        ),
                                                        
                                                        conditionalPanel(
                                                                condition = "input.residence== 'no'",
                                                                
                                                                
                                                                br(),
                                                                textInput(
                                                                        inputId = "residence_2",
                                                                        label = "6.1 Especifique en qué país reside",
                                                                        value = "Opcional"
                                                                )
                                                                
                                                        ),
                                                        
                                                        #7 
                                                        br(),
                                                        radioGroupButtons(
                                                                inputId = "urban_rural",
                                                                label = "7. Usted vive en (*)",
                                                                choices = c("", 
                                                                            "una ciudad", "la periferia de la ciudad/asentamiento humano", "un pueblo cercano a un área rural", "un área rural", "no sabe"),
                                                                individual = TRUE,
                                                                checkIcon = list(
                                                                        yes = tags$i(class = "fa fa-circle", 
                                                                                     style = "color: steelblue"),
                                                                        no = tags$i(class = "fa fa-circle-o", 
                                                                                    style = "color: steelblue"))
                                                        ),
                                                        
                                               ),
                                               
                                               #PREGUNTAS SOBRE CGR
                                               tabPanel("Sobre la CGR",
                                                        
                                                        #8
                                                        br(),
                                                        radioGroupButtons(
                                                                inputId = "cgr_1",
                                                                label = "8. ¿Tiene conocimientos sobre la labor de la Contraloría General de la República del Perú (CGR)? (*)",
                                                                choices = c("", 
                                                                            "sí", "no"),
                                                                individual = TRUE,
                                                                checkIcon = list(
                                                                        yes = tags$i(class = "fa fa-circle", 
                                                                                     style = "color: steelblue"),
                                                                        no = tags$i(class = "fa fa-circle-o", 
                                                                                    style = "color: steelblue"))
                                                        ),
                                                        
                                                        conditionalPanel(
                                                                condition = "input.cgr_1 == 'sí'",
                                                                
                                                                br(),
                                                                textAreaInput(
                                                                        inputId = "cgr_2",
                                                                        label = "8.1 Describa brevemente qué conocimientos tiene sobre la labor de la CGR",
                                                                        value = "Opcional",
                                                                        
                                                                )
                                                        ),
       
                                               ),
                                               
                                               tags$script("
                                                           $('body').mouseover(function() {
                                                           list_tabs=[];
                                                           $('#questions_1 li a').each(function(){
                                                           list_tabs.push($(this).html())
                                                           });
                                                           Shiny.onInputChange('List_of_tab', list_tabs);})
                                                           "
                                               )

                                       ),
                                       # The uiOutput will contain the Next and Previous button
                                       uiOutput("Next_Previous")
                                       

                                )
                                
                        ),
                        
                        fluidRow(
                                
                                column(8),
                                
                                column(4,
                                       actionButton("submit", "Enviar")
                                       )
                        )
                        
                ),

                ###################
                #Estadísticos
                ###################
                tabItem(tabName = "statistics", 
                        
                        fluidRow(
                                
                                selectInput("year_selection", label = "Selecciona el año",
                                            choices = c("Todos los años", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
                                ),
                                
                                tabBox(
                                        id = "claims_hist_tabs", width = 12, height = "650px",
                                        
                                        #Informacion General       
                                        tabPanel("Panorama General", icon = icon("briefcase-medical"),
                                                 
                                                 
                                                 fluidRow(
                                                         valueBoxOutput("historical_total", width = 12)
                                                 ),
                                                 
                                                 fluidRow(
                                                         valueBoxOutput("fur_historical", width = 3),
                                                         
                                                         valueBoxOutput("fud_historical", width = 3),
                                                         
                                                         valueBoxOutput("pde_historical", width = 3),
                                                         
                                                         valueBoxOutput("cad_historical", width = 3)
                                                         
                                                 )
                                                 
                                        ),
                                        
                                        #Hechos Estadísticos 
                                        tabPanel("Estadísticos Generales", icon = icon("briefcase-medical"),
                                                 
                                                 fluidRow(
                                                         column(6,
                                                                br(),
                                                                p("Hechos por departamento - estadísticos",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_hechos_dep_historical")
                                                         )
                                                 ),
                                                 
                                        ),
                                        
                                        # Estadísticos corrupción
                                        tabPanel("Estadísticos Corrupción", icon = icon("briefcase-medical"),
                                                 
                                                 fluidRow(
                                                         
                                                         box(strong("Hechos de corrupción (Total)"), width = 6, status = "primary", solidHeader = TRUE, 
                                                             style = "font-family: Georgia;text-align:center;color:#14505B ;font-size: 18px; padding-top: 20px",
                                                             
                                                             flexdashboard::gaugeOutput("corruption_count_historical", width = "90%", height = "100px")
                                                         ),
                                                         
                                                         box(strong("Hechos de corrupción (%)"), width = 6, status = "primary", solidHeader = TRUE, 
                                                             style = "font-family: Georgia;text-align:center;color:#14505B ;font-size: 18px; padding-top: 20px",
                                                             
                                                             flexdashboard::gaugeOutput("corruption_perc_historical", width = "90%", height = "100px")
                                                         )
                                                         
                                                 ),
                                                 
                                                 fluidRow(
                                                         column(6,
                                                                br(),
                                                                p("Hechos codificados: hechos de corrupción por departamento - estadísticos",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_corruption_historical")
                                                                
                                                         )
                                                 )
                                        )
                                        
                                        
                                        
                                        
                                        
                                        
                                )
                                
                                
                                
                                
                        )
                        
         
                ),
                ###################
                #Cuestionario Percepción
                ###################
                tabItem(tabName = "perception_survey",
                        
                        
                        fluidRow(
                                
                                column(5, align="center",
                                       
                                       
                                       p(br(),"¡¡Bienvenidos!!", br(),br(),
                                         "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum"
                                         , style = "text-align:justify;color:black;background-color:#fcebeb;padding:15px;border-radius:10px"),
                                       
                                       
                                       br(),
                                       
                                       p("Instrucciones:
                                       Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum"
                                         ,
                                         style="text-align:justify;color:black;background-color:#fcebeb;padding:15px;border-radius:10px")
                                       
                                       
                                ),
                                
                                column(7, align="left",
                                       
                                       tabBox(
                                               id = "questions_2", width = 12, title = "Cuestionario sobre Percepciones",
                                               
                                               # PREGUNTAS SOBRE CORRUPCIÓN
                                               tabPanel("Corrupción",
                                                        
                                                        #1.
                                                        br(),
                                                        radioGroupButtons(
                                                                inputId = "corruption_1",
                                                                label = "1. ¿Cree que en los últimos 5 años la corrupción en el Perú ha aumentado, sigue igual o ha disminuido? (*)",
                                                                choices = c("", 
                                                                            "Ha aumentado", "Sigue igual", "Ha disminuido", "No sabe"),
                                                                individual = TRUE,
                                                                checkIcon = list(
                                                                        yes = tags$i(class = "fa fa-circle", 
                                                                                     style = "color: steelblue"),
                                                                        no = tags$i(class = "fa fa-circle-o", 
                                                                                    style = "color: steelblue"))
                                                        ),
                                                        

                                                        #2.
                                                        br(),
                                                        radioGroupButtons(
                                                                inputId = "corruption_2",
                                                                label = "2. ¿Cree que en los próximos 5 años la corrupción en el Perú habrá aumentado, seguirá igual o habrá disminuido? (*)",
                                                                choices = c("", 
                                                                            "Habrá aumentado", "Seguirá igual", "Habrá disminuido", "No sabe"),
                                                                individual = TRUE,
                                                                checkIcon = list(
                                                                        yes = tags$i(class = "fa fa-circle", 
                                                                                     style = "color: steelblue"),
                                                                        no = tags$i(class = "fa fa-circle-o", 
                                                                                    style = "color: steelblue"))
                                                        ),
                                                        
                                                        #3.
                                                        br(),
                                                        radioGroupButtons(
                                                                inputId = "corruption_3",
                                                                label = "3. Teniendo en cuenta su experiencia o lo que ha oído mencionar, ¿la corrupción de los funcionarios públicos en el país está: (*)",
                                                                choices = c("", 
                                                                            "Muy generalizada", "Algo generalizada", "Poco generalizada", "Nada generalizada", "No sabe"),
                                                                individual = TRUE,
                                                                checkIcon = list(
                                                                        yes = tags$i(class = "fa fa-circle", 
                                                                                     style = "color: steelblue"),
                                                                        no = tags$i(class = "fa fa-circle-o", 
                                                                                    style = "color: steelblue"))
                                                        ),
                                                        
                                                        #4.
                                                        br(),
                                                        radioGroupButtons(
                                                                inputId = "corruption_4",
                                                                label = "4. Pensando en los funcionarios de la CGR, ¿cuántos de ellos cree que están involucrados en corrupción? (*)",
                                                                choices = c("", 
                                                                            "Ninguno", "Menos de la mitad", "La mitada de ellos", "Más de la mitad", "Todos", "No sabe"),
                                                                individual = TRUE,
                                                                checkIcon = list(
                                                                        yes = tags$i(class = "fa fa-circle", 
                                                                                     style = "color: steelblue"),
                                                                        no = tags$i(class = "fa fa-circle-o", 
                                                                                    style = "color: steelblue"))
                                                        ),

                                                        #5
                                                        br(),
                                                        sliderTextInput(
                                                                inputId = "corruption_5",
                                                                label = "5. Usando una escala que va de 1, que significa “muy mala” al 6, que significa “muy buena”, ¿Cómo calificaría la gestión de la CGR en la lucha contra la corrupción? (*)",
                                                                choices = c(1,2,3,4,5,6),
                                                                grid = TRUE
                                                        )
      
                                               ),
                                               
                                               #PREGUNTAS SOBRE TRANSPARENCIA
                                               tabPanel("Transparencia", 
                                                        
                                                        #6
                                                        br(),
                                                        radioGroupButtons(
                                                                inputId = "transparency_1",
                                                                label = "6. ¿Considera que las entidades gubernamentes son transparentes en cuanto a provisión de información? (*)",
                                                                choices = c("", 
                                                                            "sí", "no", "no sabe"),
                                                                individual = TRUE,
                                                                checkIcon = list(
                                                                        yes = tags$i(class = "fa fa-circle", 
                                                                                     style = "color: steelblue"),
                                                                        no = tags$i(class = "fa fa-circle-o", 
                                                                                    style = "color: steelblue"))
                                                        ),

                                                        #7 
                                                        br(),
                                                        radioGroupButtons(
                                                                inputId = "transparency_2",
                                                                label = "7. Actualmente, ¿tiene usted confianza en institucionales tales como la Contraloría General de la República?",
                                                                choices = c("", 
                                                                            "nada", "poca", "suficiente", "bastante", "no sabe"),
                                                                individual = TRUE,
                                                                checkIcon = list(
                                                                        yes = tags$i(class = "fa fa-circle", 
                                                                                     style = "color: steelblue"),
                                                                        no = tags$i(class = "fa fa-circle-o", 
                                                                                    style = "color: steelblue"))
                                                        ),
                                                        
                                                        #8
                                                        br(),
                                                        sliderTextInput(
                                                                inputId = "transparency_3",
                                                                label = "8. Usando una escala que va de 1, que significa “muy sencillo” al 6, que significa “muy díficil”, ¿Que tan sencillo considera que es conseguir información pública sobre la CGR? (*)",
                                                                choices = c(1,2,3,4,5,6),
                                                                grid = TRUE
                                                        ),
                                                        
                                                        #9
                                                        br(),
                                                        sliderTextInput(
                                                                inputId = "transparency_4",
                                                                label = "9. Usando una escala que va de 1, que significa “muy sencillo” al 6, que significa “muy díficil”, ¿Que tan sencillo considera que es solicitar información a la CGR? (*)",
                                                                choices = c(1,2,3,4,5,6),
                                                                grid = TRUE
                                                        ),
                                                        
                                                        #10
                                                        br(),
                                                        sliderTextInput(
                                                                inputId = "transparency_5",
                                                                label = "10. Usando una escala que va de 1, que significa “nada transparente” al 6, que significa “muy transparentel”, ¿Que tan transparente considera que son los procesos de elección de los funcionarios de la CGR? (*)",
                                                                choices = c(1,2,3,4,5,6),
                                                                grid = TRUE
                                                        )
                                                        
                                               ),
                                               
                                               tags$script("
                                                           $('body').mouseover(function() {
                                                           list_tabs_2=[];
                                                           $('#questions_2 li a').each(function(){
                                                           list_tabs_2.push($(this).html())
                                                           });
                                                           Shiny.onInputChange('List_of_tab_2', list_tabs_2);})
                                                           "
                                               )
                                               
                                       ),
                                       # The uiOutput will contain the Next and Previous button
                                       uiOutput("Next_Previous_2")
                                       
                                       
                                )
                                
                        ),
                        
                        fluidRow(
                                
                                column(8),
                                
                                column(4,
                                       actionButton("submit_2", "Enviar")
                                )
                        )
                        

                )
                
        ),
        
        #previous/next button for tab items
        hidden(actionButton(inputId ="Next", label = icon("arrow-right")))
        
        

)





shinyApp(
        ui = dashboardPage(skin = "red",
                           header, sidebar, body),
        
        server = function(input, output, session) {
                
                #previous/next button for tab items / https://stackoverflow.com/questions/44309328/generic-button-for-go-to-next-and-previous-tabitem-shiny
                
                tab_id <- c("info_survey","statistics","perception_survey")
                
                observe({
                        lapply(c("Next"),
                               toggle,
                               condition = input[["tabs"]] != "perception_survey")
                })
                
                Current <- reactiveValues(
                        Tab = "info_survey"
                )
                
                observeEvent(
                        input[["tabs"]],
                        {
                                Current$Tab <- input[["tabs"]]
                        }
                )
                
                
                observeEvent(
                        input[["Next"]],
                        {
                                tab_id_position <- match(Current$Tab, tab_id) + 1
                                if (tab_id_position > length(tab_id)) tab_id_position <- 1
                                Current$Tab <- tab_id[tab_id_position]
                                updateTabItems(session, "tabs", tab_id[tab_id_position]) 
                        }
                )
                
                
                
                #PREVIOUS/NEXT BUTTON for Cuestionario Inicial
                Previous_Button=tags$div(actionButton("Prev_Tab",HTML('<div class="col-sm-4"><i class="fa fa-angle-double-left fa-2x"></i></div>
                                                                  ')))
                
                Next_Button=div(actionButton("Next_Tab",HTML('<div class="col-sm-4"><i class="fa fa-angle-double-right fa-2x"></i></div>')))
                

                output$Next_Previous=renderUI({
                        tab_list=input$List_of_tab[-length(input$List_of_tab)]
                        nb_tab=length(tab_list)
                        if (which(tab_list==input$questions_1)==nb_tab)
                                column(1,offset=1,Previous_Button)
                        else if (which(tab_list==input$questions_1)==1)
                                column(1,offset = 10,Next_Button)
                        else
                                div(column(1,offset=1,Previous_Button),column(1,offset=8,Next_Button))
                        
                })
                observeEvent(input$Prev_Tab,
                             {
                                     tab_list=input$List_of_tab
                                     current_tab=which(tab_list==input$questions_1)
                                     updateTabsetPanel(session,"questions_1",selected=tab_list[current_tab-1])
                             }
                )
                observeEvent(input$Next_Tab,
                             {
                                     tab_list=input$List_of_tab
                                     current_tab=which(tab_list==input$questions_1)
                                     updateTabsetPanel(session,"questions_1",selected=tab_list[current_tab+1])
                             }
                )
                
                
                #PREVIOUS/NEXT BUTTON for Cuestionario Percepciones
                Previous_Button_2=tags$div(actionButton("Prev_Tab_2",HTML('<div class="col-sm-4"><i class="fa fa-angle-double-left fa-2x"></i></div>
                                                                  ')))
                
                Next_Button_2=div(actionButton("Next_Tab_2",HTML('<div class="col-sm-4"><i class="fa fa-angle-double-right fa-2x"></i></div>')))
                
                
                output$Next_Previous_2=renderUI({
                        tab_list_2=input$List_of_tab_2[-length(input$List_of_tab_2)]
                        nb_tab_2=length(tab_list_2)
                        if (which(tab_list_2==input$questions_2)==nb_tab_2)
                                column(1,offset=1,Previous_Button_2)
                        else if (which(tab_list_2==input$questions_2)==1)
                                column(1,offset = 10,Next_Button_2)
                        else
                                div(column(1,offset=1,Previous_Button_2),column(1,offset=8,Next_Button_2))
                        
                })
                observeEvent(input$Prev_Tab_2,
                             {
                                     tab_list_2=input$List_of_tab_2
                                     current_tab_2=which(tab_list_2==input$questions_2)
                                     updateTabsetPanel(session,"questions_2",selected=tab_list_2[current_tab_2-1])
                             }
                )
                observeEvent(input$Next_Tab_2,
                             {
                                     tab_list_2=input$List_of_tab_2
                                     current_tab_2=which(tab_list_2==input$questions_2)
                                     updateTabsetPanel(session,"questions_2",selected=tab_list_2[current_tab_2+1])
                             }
                )

                
                #Add character limit to a text box
                shinyjs::runjs("$('#nacionality_3').attr('maxlength',15)")  #15 characters
                shinyjs::runjs("$('#work_2').attr('maxlength',15)")  #15 characters
                shinyjs::runjs("$('#residence_2').attr('maxlength',15)")  #15 characters
                shinyjs::runjs("$('#cgr_2').attr('maxlength',150)")
                

                #get IP from user
                ip_user <- reactive(input$getIP)
                
                # Whenever a field is filled, aggregate all form data
                formData <- reactive({
                        data <- sapply(fields, function(x) input[[x]])
                        
                        #ifelse(input[[x]] != "", input[[x]], "NA")
                        #ifelse
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
                
                
                # Update dep, prov and dist options
                observe({
                        x <- input$dep
                        
                        updatePickerInput(session, "prov",
                                             label = 'Provincia (*)',
                                             choices = c( unique(dep_prov_dist$PROVINCIA[dep_prov_dist$DEPARTAMENTO == x ]) )
                                             
                                             
                        )
                })
                
                observe({
                        y <- input$prov
                        
                        updatePickerInput(session, "dist",
                                             label = 'Distrito (*)',
                                             choices = c( dep_prov_dist$DISTRITO[dep_prov_dist$DEPARTAMENTO == input$dep & dep_prov_dist$PROVINCIA == y ])
                        )
                })
                
                
                ################
                # ESTADISTICOS
                ###############
                
                #CLAIMS TOTAL
                output$historical_total <- renderValueBox({
                        
                        
                        #creating n_claims_historical variable
                        if (input$year_selection == "Todos los años") {
                                n_claims_historical <- nrow(raw_historical_claims)
                                
                        } else {
                                n_claims_historical <- nrow(subset(raw_historical_claims, year== input$year_selection ))
                                
                        }
                        
                        valueBox(value = paste(format(n_claims_historical, big.mark = ","), "", sep = " "),
                                 "Hechos totales",
                                 icon = icon("fa-solid fa-database"),             #https://fontawesome.com/v4/icons/
                                 color = "purple"
                        )
                })
                
                #FUR 
                output$fur_historical <- renderValueBox({
                        
                        if (input$year_selection == "Todos los años") {
                                fur_claims <- sum(raw_historical_claims$FUR_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_claims)
                                percent_fur <-  paste( round(fur_claims/n_claims_historical,2)*100, "%")
                        } else {
                                #dataframe with specific year
                                raw_historical_year <- subset(raw_historical_claims, year== input$year_selection)
                                
                                fur_claims <- sum(raw_historical_year$FUR_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_year)
                                percent_fur <-  paste( round(fur_claims/n_claims_historical,2)*100, "%")
                        }
                        
                        valueBox(value = paste(format(fur_claims, big.mark = ","), "/", percent_fur  , sep = " "),
                                 "Hechos FUR (respecto a hechos totales)",
                                 icon = icon("fa-solid fa-folder-open"),            
                                 color = 'aqua')
                })
                
                #FUD
                output$fud_historical <- renderValueBox({
                        
                        if (input$year_selection == "Todos los años") {
                                fud_claims <- sum(raw_historical_claims$FUD_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_claims)
                                percent_fud <-  paste( round(fud_claims/n_claims_historical,2)*100, "%")
                        } else {
                                #dataframe with specific year
                                raw_historical_year <- subset(raw_historical_claims, year== input$year_selection)
                                
                                fud_claims <- sum(raw_historical_year$FUD_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_year)
                                percent_fud <-  paste( round(fud_claims/n_claims_historical,2)*100, "%")
                        }
                        
                        valueBox(value = paste(format(fud_claims, big.mark = ","), "/", percent_fud  , sep = " "),
                                 "Hechos FUD (respecto a hechos totales)",
                                 icon = icon("fa-solid fa-folder-open"),            
                                 color = 'aqua')
                })
                
                #PDE
                output$pde_historical <- renderValueBox({
                        
                        if (input$year_selection == "Todos los años") {
                                pde_claims <- sum(raw_historical_claims$PDE_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_claims)
                                percent_pde <-  paste( round(pde_claims/n_claims_historical,2)*100, "%")
                        } else {
                                raw_historical_year <- subset(raw_historical_claims, year== input$year_selection)
                                
                                pde_claims <- sum(raw_historical_year$PDE_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_year)
                                percent_pde <-  paste( round(pde_claims/n_claims_historical,2)*100, "%")
                        }
                        valueBox(value = paste(format(pde_claims, big.mark = ","), "/", percent_pde  , sep = " "),
                                 "Hechos PDE (respecto a hechos totales)",
                                 icon = icon("fa-solid fa-folder-open"),            
                                 color = 'aqua')
                })
                
                #CAD
                output$cad_historical <- renderValueBox({
                        
                        if (input$year_selection == "Todos los años") {
                                cad_claims <- sum(raw_historical_claims$CAD_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_claims)
                                percent_cad <-  paste( round(cad_claims/n_claims_historical,2)*100, "%")
                        } else {
                                raw_historical_year <- subset(raw_historical_claims, year== input$year_selection)
                                
                                cad_claims <- sum(raw_historical_year$CAD_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_year)
                                percent_cad <-  paste( round(cad_claims/n_claims_historical,2)*100, "%")
                        }
                        valueBox(value = paste(format(cad_claims, big.mark = ","), "/", percent_cad , sep = " "),
                                 "Hechos CAD (respecto a hechos totales)",
                                 icon = icon("fa-solid fa-folder-open"),            
                                 color = 'aqua')
                })
                
                #Hechos por departamento - estadísticos
                output$table_hechos_dep_historical <- renderReactable({
                        
                        hechos_dep_census <- read.csv('out/hechos_dep_census_claims_historical.csv')
                        
                        if (input$year_selection == "Todos los años") {
                                
                                hechos_dep_census <- hechos_dep_census %>% 
                                        group_by(departamento) %>% 
                                        mutate( n = sum(n_year),
                                                pop17 = as.integer(min(pop17)) ) %>% 
                                        filter(row_number()==1) %>% 
                                        select(departamento, pop17, n) %>% 
                                        mutate( N_dpt_claims100Kpop = round( n/(pop17/100000) ,1) ) %>% 
                                        ungroup() %>% 
                                        mutate(percentage = round( (n/sum(n) )*100, 1))  
                                
                        } else {
                                
                                hechos_dep_census <- hechos_dep_census %>%
                                        filter(year== input$year_selection) %>%
                                        select(departamento, n_year, pop17) %>% 
                                        mutate( N_dpt_claims100Kpop = round( n_year/(pop17/100000) ,1),
                                                percentage = round( (n_year/sum(n_year) )*100, 1))
                                
                                colnames(hechos_dep_census) <- c("departamento", "n", "pop17", "N_dpt_claims100Kpop", "percentage")
                        }
                        
                        
                        reactable(
                                hechos_dep_census[,c("departamento","n","N_dpt_claims100Kpop", "percentage")],
                                pagination = TRUE,
                                defaultSorted = "n",
                                defaultColDef = colDef(headerClass = "header", align = "left"),
                                columns = list(
                                        departamento = colDef(name = "departamento"
                                                              
                                        ),
                                        n = colDef(
                                                name = "N° de hechos",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(hechos_dep_census$n), "%")
                                                        # Add thousands separators
                                                        value <- format(value, big.mark = ",")
                                                        bar_chart(value, width = width, fill = "#3fc1c9")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        percentage = colDef(
                                                name = "Porcentaje",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                        bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        N_dpt_claims100Kpop = colDef(
                                                name = "N° de hechos por 100 mil habitantes",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(hechos_dep_census$N_dpt_claims100Kpop), "%")
                                                        # Add thousands separators
                                                        value <- format(value, big.mark = ",")
                                                        bar_chart(value, width = width, fill = "#00aa7f")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        )
                                )
                        )
                        
                })

                #Hechos de corrupción (Total)
                output$corruption_count_historical <- flexdashboard::renderGauge({
                        
                        
                        if (input$year_selection == "Todos los años") {
                                
                                corruption_claims_count <- corruption_claims_count_historical %>% 
                                        group_by(corruption_denunc) %>% 
                                        summarise( n = sum(n) )
                                
                                max = sum(corruption_claims_count$n)
                                
                        } else {
                                
                                corruption_claims_count <- corruption_claims_count_historical %>% 
                                        filter(year== input$year_selection) 
                                
                                max = sum(corruption_claims_count$n)
                                
                        }
                        
                        
                        flexdashboard::gauge(corruption_claims_count$n[corruption_claims_count$corruption_denunc == 1], min = 0, max = max , gaugeSectors(
                                success = c(round((2/3)*max) + 1 , max ), warning = c(round(max/3) + 1, round((2/3)*max) ), danger = c(0, round(max/3) ))
                                , abbreviate = FALSE, abbreviateDecimals = 1)
                })
                
                #Hechos de corrupción (%)
                output$corruption_perc_historical <- flexdashboard::renderGauge({
                        
                        if (input$year_selection == "Todos los años") {
                                
                                corruption_claims_count <- corruption_claims_count_historical %>% 
                                        group_by(corruption_denunc) %>% 
                                        summarise( n = sum(n) )
                                
                                corruption_claims_count$percentage <- round( (corruption_claims_count$n/sum(corruption_claims_count$n) )*100, 1)
                                
                        } else {
                                
                                corruption_claims_count <- corruption_claims_count_historical %>% 
                                        filter(year== input$year_selection) 
                                
                                corruption_claims_count$percentage <- round( (corruption_claims_count$n/sum(corruption_claims_count$n) )*100, 1)
                                
                        }
                        
                        
                        flexdashboard::gauge(corruption_claims_count$percentage[corruption_claims_count$corruption_denunc == 1], min = 0, max = 100, symbol = '%', gaugeSectors(
                                success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
                        ))
                }) 
                
                #Hechos codificados: hechos de corrupción por departamento - estadísticos
                output$table_corruption_historical <- renderReactable({
                        
                        corruption_dep_census <- read.csv('out/corruption_dep_census_claims_historical.csv')
                        
                        
                        if (input$year_selection == "Todos los años") {
                                
                                corruption_dep_census <- corruption_dep_census %>% 
                                        group_by(departamento) %>% 
                                        mutate( n = sum(n_year),
                                                pop17 = as.integer(min(pop17)) ) %>% 
                                        filter(row_number()==1) %>% 
                                        select(departamento, pop17, n) %>% 
                                        mutate( N_dpt_claims100Kpop_corruption = round( n/(pop17/100000) ,1) ) %>% 
                                        ungroup() %>% 
                                        mutate(percentage = round( (n/sum(n) )*100, 1))  
                                
                        } else {
                                
                                corruption_dep_census <- corruption_dep_census %>%
                                        filter(year== input$year_selection) %>%
                                        select(departamento, n_year, pop17) %>% 
                                        mutate( N_dpt_claims100Kpop_corruption = round( n_year/(pop17/100000) ,1),
                                                percentage = round( (n_year/sum(n_year) )*100, 1))
                                
                                colnames(corruption_dep_census) <- c("departamento", "n", "pop17", "N_dpt_claims100Kpop_corruption", "percentage")
                        }
                        
                        
                        
                        
                        reactable(
                                corruption_dep_census[,c("departamento", "n", "percentage", "N_dpt_claims100Kpop_corruption")],
                                pagination = TRUE,
                                defaultSorted = "n",
                                defaultColDef = colDef(headerClass = "header", align = "left"),
                                columns = list(
                                        departamento = colDef(name = "departamento"
                                                              
                                        ),
                                        n = colDef(
                                                name = "N° de hechos",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(corruption_dep_census$n), "%")
                                                        # Add thousands separators
                                                        value <- format(value, big.mark = ",")
                                                        bar_chart(value, width = width, fill = "#3fc1c9")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        percentage = colDef(
                                                name = "Porcentaje",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                        bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        N_dpt_claims100Kpop_corruption = colDef(
                                                name = "N° de hechos por 100 mil habitantes",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(corruption_dep_census$N_dpt_claims100Kpop_corruption), "%")
                                                        # Add thousands separators
                                                        value <- format(value, big.mark = ",")
                                                        bar_chart(value, width = width, fill = "#00aa7f")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        )
                                )
                        )
                        
                })
                
                

        }
)

#runApp("C:/Users/Yoseph/Documents/GitHub/complaints_dashboards/Survey_CGR.R")