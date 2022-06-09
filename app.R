library(shiny)
library(shinydashboard)
library(ggplot2)
library(googledrive)
library(googlesheets4)
library(shinyWidgets) #todo: install on AWS
library(shinyjs) #todo: install on AWS

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

#upload data
dep_prov_dist <- read.csv("dep_prov_dist.csv", encoding = "UTF-8")
colnames(dep_prov_dist) <- c("DEPARTAMENTO", "PROVINCIA", "DISTRITO")



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
                        
                        p("hola1")
                        
         
                ),
                ###################
                #Cuestionario Percepción
                ###################
                tabItem(tabName = "perception_survey",
                        
                        p("hola2")
                        
                        
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

        }
)

#runApp("C:/Users/Yoseph/Documents/GitHub/complaints_dashboards/Survey_CGR.R")