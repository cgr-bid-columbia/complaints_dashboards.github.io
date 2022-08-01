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
library(viridis)
library(mapproj)


# USERS AND PASSWORDS
#user1 <- Sys.getenv("USER_CGR")
#pw1 <- Sys.getenv("PASS_CGR")

user1 <- "cgr"
pw1 <- "pass_cgr"

#user2 <- Sys.getenv("USER_CITIZEN")
#pw2 <- Sys.getenv("PASS_CITIZEN")

user2 <- "citizen"
pw2 <- "pass_citizen"


gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)


# Define the fields we want to save from the form
fields <- c("nacionality", "nacionality_2", "nacionality_3", "age", "gender", "education", "education_2",
            "ethnicity", "work", "work_2", "residence", "dep", "prov", "dist","residence_2",
            "urban_rural", "cgr_1", "cgr_2",
            "corruption_1", "corruption_2", "corruption_3", "corruption_4", "corruption_5",
            "transparency_1", "transparency_2", "transparency_3", "transparency_4", "transparency_5")


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

clean_claims_21 <- read.csv('clean_claims_21_dash.csv')
raw_claims_feb21 <- read.csv('raw_claims_feb21_dash.csv')
raw_historical_claims <- read.csv('raw_historical_claims_dash.csv')

corruption_claims_count_historical <- read.csv('corruption_claims_count_claims_historical.csv')


##################
## DENUNCIAS 2021
##################

n_claims <- nrow(raw_claims_feb21)
n_encoded <- nrow(clean_claims_21)
percent_encoded <- paste( round(n_encoded/n_claims,2)*100, "%")



##Aprobado, rechazado, pendiente categories
#TODO_ hacer más eficiente esta parte del codigo
#FUR categories
FUR_categories_df <- mutate(subset(raw_claims_feb21, FUR_decision == "Yes"), FUR_decision_categories =
                                    case_when( FUD_decision == "Yes" ~ "Aprobado",
                                               FUD_decision == "No" & (estado_fur == "No admitido" | estado_fur == "No aceptado a trámite") ~ "Rechazado",
                                               TRUE  ~ "Pendiente") )

#FUD categories
FUD_categories_df <- mutate(subset(raw_claims_feb21, FUD_decision == "Yes"), FUD_decision_categories =
                                    case_when( PDE_decision == "Yes" ~ "Aprobado",
                                               PDE_decision == "No" & (estado_fud == "Sin atender" | estado_fud == "Devuelto")  ~ "Pendiente",
                                               TRUE ~ "Rechazado") )

#PDE categories
PDE_categories_df  <- mutate(subset(raw_claims_feb21, PDE_decision == "Yes"), PDE_decision_categories =
                                     case_when( CAD_decision == "Yes" ~ "Aprobado",
                                                CAD_decision == "No" & producto_aprobado == "En proceso" ~ "Pendiente",
                                                TRUE ~ "Rechazado") )

#FUD/FUD/PDE/CAD
fur_claims <- sum(raw_claims_feb21$FUR_decision == "Yes")
percent_fur <-  paste( round(fur_claims/n_claims,2)*100, "%")

fud_claims <- sum(raw_claims_feb21$FUD_decision == "Yes")
percent_fud <-  paste( round(fud_claims/n_claims,2)*100, "%")

pde_claims <- sum(raw_claims_feb21$PDE_decision == "Yes")
percent_pde <-  paste( round(pde_claims/n_claims,2)*100, "%")

cad_claims <- sum(raw_claims_feb21$CAD_decision == "Yes")
percent_cad <-  paste( round(cad_claims/n_claims,2)*100, "%")



departamentos_fortified_ <- read.csv('departamentos_fortified_claims_21.csv')


corruption_claims_count <- read.csv('corruption_claims_count_claims_21.csv')





#HEADER
header <- dashboardHeader(title = "Denuncias CGR")

#SIDEBAR
sidebar <- dashboardSidebar(width = 280,
                            disable = TRUE, #don't show sidebar
                            sidebarMenu(id = "tabs",
                                        menuItem("Usuarios", tabName = "users"),  
                                        menuItem("Cuestionario Inicial", tabName = "info_survey"),   #CITIZEN
                                        menuItem("Estadísticas", tabName = "statistics"),            #CITIZEN
                                        menuItem("Cuestionario Percepción", tabName = "perception_survey"), #CITIZEN
                                        menuItem("Denuncias 2021", tabName = "claims2021"), #CGR
                                        menuItem("Denuncias Históricas", tabName = "claims_hist") #CGR
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
                #Users
                ###################
                tabItem(tabName = "users",
                        
                        
                        uiOutput("ui")
                        
    
                ),
                
                ###################
                #Cuestionario inicial
                ###################
                tabItem(tabName = "info_survey",
                        
                        fluidRow(
                                
                                column(5, align="center",
                                       
                                       
                                       p(br(),"¡¡Bienvenidos!!", br(),br(),
                                         "La idea de esta aplicación web es poder recoger información sobre la opinión ciudadana en cuanto a su percepción de corrupción y transparencia ante la Contraloría General de la República.
                                         El aplicativo consta de 3 partes. Primero, hay un cuestionario inicial, que busca recoger un poco sobre su información personal. Segundo, hay una sección con algunos estadísticos sobre el proceso
                                         de denuncias de la CGR y sobre denuncias de corrupción. Finalmente, hay un cuestionario sobre percepción de corrupción y transparencia."
                                         , style = "text-align:justify;color:black;background-color:#fcebeb;padding:15px;border-radius:10px"),
                                       
                                       
                                       br(),
                                       
                                       p("Instrucciones:
                                       Para poder pasar a la siguiente sección del aplicativo, es necesario que responda todas las preguntas. No se preocupe que el cuestionario es anónimo"
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
                                                                inputId = "ethnicity",
                                                                label = "5. Por sus antepasados y de acuerdo a sus costumbres, ¿usted se considera: (*)",
                                                                choices = c("", 
                                                                            "quechua", "aymara", "nativo/indígena", "negro/afroperuano", "blanco", "mestizo", "otro"),
                                                                individual = TRUE,
                                                                checkIcon = list(
                                                                        yes = tags$i(class = "fa fa-circle", 
                                                                                     style = "color: steelblue"),
                                                                        no = tags$i(class = "fa fa-circle-o", 
                                                                                    style = "color: steelblue"))
                                                        ),
                                                        
                                                        #6
                                                        br(),
                                                        radioGroupButtons(
                                                                inputId = "work",
                                                                label = "6. Dedicación principal (*)",
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
                                                                        label = "6.1 Especifique a qué otra actividad se dedica principalmente",
                                                                        value = "Opcional"
                                                                )
                                                                
                                                        ),
                                                        
                                                        
                                                        
                                                        
                                               ),
                                               
                                               #PREGUNTAS SOBRE UBICACIÓN
                                               tabPanel("Ubicación", 
                                                        
                                                        #7
                                                        br(),
                                                        radioGroupButtons(
                                                                inputId = "residence",
                                                                label = "7. ¿Reside en el Perú? (*)",
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
                                                                        label = "7.1 Especifique en qué país reside",
                                                                        value = "Opcional"
                                                                )
                                                                
                                                        ),
                                                        
                                                        #8
                                                        br(),
                                                        radioGroupButtons(
                                                                inputId = "urban_rural",
                                                                label = "8. Usted vive en (*)",
                                                                choices = c("", 
                                                                            "una ciudad", "la periferia de la ciudad/asentamiento humano", "un pueblo cercano a un área rural", "un área rural"),
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
                                                        
                                                        #9
                                                        br(),
                                                        radioGroupButtons(
                                                                inputId = "cgr_1",
                                                                label = "9. ¿Tiene conocimientos sobre la labor de la Contraloría General de la República del Perú (CGR)? (*)",
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
                                                                        label = "9.1 Describa brevemente qué conocimientos tiene sobre la labor de la CGR",
                                                                        value = "Opcional",
                                                                        
                                                                )
                                                        ),
                                                        
                                                        #if all the mandatory questions are answered, the submit button appears
                                                        conditionalPanel(
                                                                condition = "(input.nacionality != '' && 
                                                                             input.age != '' &&
                                                                             input.gender != '' &&
                                                                             input.education != '' &&
                                                                             input.ethnicity != '' &&
                                                                             input.work != '' &&
                                                                             input.residence == 'no' &&
                                                                             input.urban_rural != '' &&
                                                                             input.cgr_1 != '')||
                                                                             (input.nacionality != '' && 
                                                                             input.age != '' &&
                                                                             input.gender != '' &&
                                                                             input.education != '' &&
                                                                             input.ethnicity != '' &&
                                                                             input.work != '' &&
                                                                             input.residence == 'sí' &&
                                                                             input.dep != '' &&
                                                                             input.prov != '' &&
                                                                             input.dist != '' &&
                                                                             input.urban_rural != '' &&
                                                                             input.cgr_1 != '')",

                                                                fluidRow(
                                                                        
                                                                        column(6),
                                                                        
                                                                        column(6,
                                                                               actionButton("submit", "Enviar")
                                                                        )
                                                                )
                                                                
                                                        )
 
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
                        
                       
                ),
                
                
                ###################
                #Estadísticos
                ###################
                tabItem(tabName = "statistics", 
                        
                        fluidRow(
                                
                                selectInput("year_selection", label = "Selecciona el año",
                                            choices = c("Todos: 2013-2021", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013")
                                ),
                                
                                tabBox(
                                        id = "claims_hist_tabs", width = 12, height = "650px",
                                        
                                        
                                        # Link denuncias online
                                        tabPanel("Envía tu denuncia",
                                        
                                                 fluidRow(
                                        
                                                     column(12,
                                        
                                                               p("La emisión de denuncias ciudadanas forma parte del ejercicio de la participación ciudadana en el control social. 
                                                      A través del formulario virtual, la ciudadanía podrá denunciar los hechos presuntamente irregulares en las entidades
                                                     del Estado y convertirse en aliada de la Contraloría."),
                                        
                                                            p("Formulario Virtual de Denuncias", strong(tags$a(href="https://denunciaweb.contraloria.gob.pe/SAD_WEB/#/AtencionDenuncias", "aquí"
                                                                                                              , target="_blank", style="color:black; text-decoration: underline")))
                                        
                                                    )
                                        
                                                  ),
                                        
                                        
                                                fluidRow(
                                                       column(12,
                                                             htmlOutput("frame")
                                                    )
                                                )
                                        
                                        ),
                                        
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
                                                         
                                                 ),
                                                 
                                                 conditionalPanel(
                                                         condition = "input.year_selection== 'Todos: 2013-2021'",
                                                         
                                                         br(),
                                                         selectInput(
                                                                 inputId = "select_hechos", label = "Selecciona:",
                                                                 choices = c("Hechos totales", "Hechos FUR", "Hechos FUD", "Hechos PDE", "Hechos CAD")
                                                         ),
                                                         
                                                         plotlyOutput("hechos_evolucion")

                                                 ),
                                                 
                                                 conditionalPanel(
                                                         condition = "input.year_selection!= 'Todos: 2013-2021'",
                                                         
                                                         fluidRow(
                                                                 column(6,
                                                                        br(),
                                                                        p("Hechos por departamento",
                                                                          style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                        
                                                                        reactableOutput("table_hechos_dep_historical")
                                                                 ),
                                                                 
                                                                 column(6,
                                                                        br(),
                                                                        
                                                                        p("Denuncias de corrupción por departamento",
                                                                          style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                        
                                                                        reactableOutput("table_corruption_historical")
                                                                        
                                                                 )
                                                         )
                                                 ),
                                                 
                                                 
                                                 fluidRow(
                                                         br(),br(),br(),br(),br(),
                                                         p(tags$ul("Descripción:", style = "text-align:left; color: black ; font-size: 17px; font-weight: bold"),
                                                           
                                                         ),
                                                         
                                                         p(tags$ul(
                                                                 tags$li("Hechos FUR: Primer filtro del proceso de denuncia (cuando su denuncia haya calificado para ser una Ficha única de Registro)"), 
                                                                 tags$li("Hechos FUD: Segundo filtro del proceso de denuncia (cuando su denuncia haya calificado para ser una Ficha única de Denuncia)"), 
                                                                 tags$li("Hechos PDE: Tercer filtro del proceso de denuncia (cuando su denuncia haya calificado para ser un Plan de Atención de Denuncia)"),
                                                                 tags$li("Hechos CAD: Cuarto filtro del proceso de denuncia (cuando su denuncia haya calificado para ser una Carpeta de Atención de Denuncias)"),
                                                                 tags$li("Su denuncia será atendida si es que pasa por los 4 filtros"),
                                                                 style = "text-align:left; color: black ; font-size: 16px"
                                                         )
                                                         
                                                         ),
                                                         
                                                 )
                                                 
                                        ),
                                        
                                        #Hechos Estadísticos : Tu region
                                        tabPanel("Tu región", icon = icon("briefcase-medical"),
                                                 
                                                 fluidRow(
                                                         column(6,
                                                                br(),
                                                                
                                                                plotlyOutput("hechos_dep_hist")
                                                                
                                                         ),
                                                         
                                                         column(6,
                                                                br(),
                                                                
                                                                plotlyOutput("hechos_corrup_dep_hist")
                                                                

                                                         )
                                                 ),
                                                 
                                                 fluidRow(
                                                         
                                                         column(6,
                                                                br(),br(),br(),
                                                                actionButton("next_cgr2", label = icon("arrow-right"))
                                                         )
                                                         

                                                         
                                                 )
                                                 
                                        ),
                                        
                                       
                                        

                                        
                                )
                                
                                
                                
                                
                        )
                        
                        
                ),
                ###################
                #Cuestionario Percepción
                ###################
                tabItem(tabName = "perception_survey",
                        
                        
                        fluidRow(
                                
                                column(5, align="center",
                                       
                                       
                                       #p(br(),"¡¡Bienvenidos!!", br(),br(),
                                        # "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum"
                                         #, style = "text-align:justify;color:black;background-color:#fcebeb;padding:15px;border-radius:10px"),
                                       
                                       
                                      # br(),
                                       
                                       p("Instrucciones:
                                          Para poder enviar sus respuestas, debe responder todas las preguntas. No se preocupe que el cuestionario es anónimo. Muchas gracias por su participación."
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
                                                                            "Ha aumentado", "Sigue igual", "Ha disminuido"),
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
                                                                label = "2. ¿Cree que en los próximos 5 años la corrupción en el Perú habra aumentado, seguirá igual o habra disminuido? (*)",
                                                                choices = c("", 
                                                                            "Habra aumentado", "Seguirá igual", "Habra disminuido"),
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
                                                                            "Muy generalizada", "Algo generalizada", "Poco generalizada", "Nada generalizada"),
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
                                                                            "Ninguno", "Menos de la mitad", "La mitada de ellos", "Más de la mitad", "Todos"),
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
                                                                choices = c(0,1,2,3,4,5,6),
                                                                grid = TRUE
                                                        ),
                                                        
                                                        p("El 0 no es una respuesta válida")
                                                        
                                               ),
                                               
                                               #PREGUNTAS SOBRE TRANSPARENCIA
                                               tabPanel("Transparencia", 
                                                        
                                                        #6
                                                        br(),
                                                        radioGroupButtons(
                                                                inputId = "transparency_1",
                                                                label = "6. ¿Considera que las entidades gubernamentes son transparentes en cuanto a provisión de información? (*)",
                                                                choices = c("", 
                                                                            "sí", "no"),
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
                                                                label = "7. Actualmente, ¿tiene usted confianza en institucionales tales como la Contraloría General de la República? (*)",
                                                                choices = c("", 
                                                                            "nada", "poca", "suficiente", "bastante"),
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
                                                                choices = c(0,1,2,3,4,5,6),
                                                                grid = TRUE
                                                        ),
                                                        
                                                        p("El 0 no es una respuesta válida"),
                                                        
                                                        #9
                                                        br(),
                                                        sliderTextInput(
                                                                inputId = "transparency_4",
                                                                label = "9. Usando una escala que va de 1, que significa “muy sencillo” al 6, que significa “muy díficil”, ¿Que tan sencillo considera que es solicitar información a la CGR? (*)",
                                                                choices = c(0,1,2,3,4,5,6),
                                                                grid = TRUE
                                                        ),
                                                        
                                                        p("El 0 no es una respuesta válida"),
                                                        
                                                        #10
                                                        br(),
                                                        sliderTextInput(
                                                                inputId = "transparency_5",
                                                                label = "10. Usando una escala que va de 1, que significa “nada transparente” al 6, que significa “muy transparente”, ¿Que tan transparente considera que son los procesos de elección de los funcionarios de la CGR? (*)",
                                                                choices = c(0,1,2,3,4,5,6),
                                                                grid = TRUE
                                                        ),
                                                        
                                                        p("El 0 no es una respuesta válida"),
                                                                
                                                        #if all the mandatory questions are answered, the submit button appears
                                                        conditionalPanel(
                                                                condition = "(input.corruption_1 != '0' && 
                                                                             input.corruption_2 != '0' &&
                                                                             input.corruption_3 != '0' &&
                                                                             input.corruption_4 != '0' &&
                                                                             input.corruption_5 != '0' &&
                                                                             input.transparency_1 != '0' &&
                                                                             input.transparency_2 != '0' &&
                                                                             input.transparency_3 != '0' &&
                                                                             input.transparency_4 != '0' &&
                                                                             input.transparency_5 != '0'
                                                                             )",
                                                                
                                                                fluidRow(
                                                                        
                                                                        column(6),
                                                                        
                                                                        column(6,
                                                                               actionButton("submit_2", "Enviar")
                                                                        )
                                                                )
                                                                
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
                                
                        )
                        
                ),
                
                ####################
                # Denuncias 2021
                ####################
                tabItem(tabName = "claims2021",
                        
                        fluidRow(
                                
                                
                                p("Denuncias 2021", style = "text-align:right;color:black;font-size: 20px;font-weight: bold;background-color:white;padding:11px;border-radius:10px"),

                                tabBox(
                                        id = "claims2021_tabs", width = 12, height = "800px",
                                        
                                        
                                        #Informacion General       
                                        tabPanel("Panorama General", icon = icon("briefcase-medical"),
                                                 
                                                 fluidRow(
                                                         
                                                         valueBox(value = paste(format(n_claims, big.mark = ","), "", sep = " "), 
                                                                  "Hechos totales",
                                                                  icon = icon("fa-solid fa-database"),             #https://fontawesome.com/v4/icons/
                                                                  color = "purple",
                                                                  width = 6
                                                         ),
                                                         
                                                         valueBox(value = paste(format(n_encoded, big.mark = ","), "/", percent_encoded  , sep = " "), 
                                                                  "Hechos codificados",
                                                                  icon = icon("fa-solid fa-arrow-circle-up"),
                                                                  color = "purple",
                                                                  width = 6
                                                         )
                                                         
                                                 ),
                                                 
                                                 
                                                 fluidRow(
                                                         
                                                         valueBox(value = paste(format(fur_claims, big.mark = ","), "/", percent_fur  , sep = " "),
                                                                  "Hechos FUR (respecto a hechos totales)",
                                                                  icon = icon("fa-solid fa-folder-open"),            
                                                                  color = 'aqua',
                                                                  width = 3),
                                                         
                                                         valueBox(value = paste(format(fud_claims, big.mark = ","), "/", percent_fud  , sep = " "),
                                                                  "Hechos FUD (respecto a hechos totales)",
                                                                  icon = icon("fa-solid fa-folder-open"),             
                                                                  color = 'aqua',
                                                                  width = 3),
                                                         
                                                         valueBox(value = paste(format(pde_claims, big.mark = ","), "/", percent_pde  , sep = " "),
                                                                  "Hechos PDE (respecto a hechos totales)",
                                                                  icon = icon("fa-solid fa-folder-open"),             
                                                                  color = 'aqua',
                                                                  width = 3),
                                                         
                                                         valueBox(value = paste(format(cad_claims, big.mark = ","), "/", percent_cad , sep = " "),
                                                                  "Hechos CAD (respecto a hechos totales)",
                                                                  icon = icon("fa-solid fa-folder-open"),             
                                                                  color = 'aqua',
                                                                  width = 3)
                                                         
                                                 ),
                                                 
                                                 
                                                 fluidRow(
                                                         column(4,
                                                                
                                                                p("Estado FUR",
                                                                  style = "text-align:center; color: black ; font-size: 16px; font-weight: bold"),
                                                                
                                                                plotlyOutput("fur_status_2021")
                                                         ),
                                                         column(4,
                                                                
                                                                p("Estado FUD",
                                                                  style = "text-align:center; color: black ; font-size: 16px; font-weight: bold"),
                                                                
                                                                plotlyOutput("fud_status_2021")
                                                         ),
                                                         column(4,
                                                                
                                                                p("Estado PDE",
                                                                  style = "text-align:center; color: black ; font-size: 16px; font-weight: bold"),
                                                                
                                                                plotlyOutput("pde_status_2021")
                                                         )
                                                 ),
                                                 
                                                 
                                                 fluidRow(
                                                         br(),br(),br(),br(),br(),
                                                         p(tags$ul("Descripción:", style = "text-align:left; color: black ; font-size: 17px; font-weight: bold"),
                                                           
                                                         ),
                                                         
                                                         p(tags$ul(
                                                                 tags$li("Hechos FUR: Conformado por los hechos que, en el estado HEC, son Calificados o Concluidos"), 
                                                                 tags$li("Hechos FUD: Conformado por los hechos que son FUR y que, además, poseen numeración FUD"), 
                                                                 tags$li("Hechos PDE: Conformado por los hechos que poseen numeración HE y que, en producto aprobado, aparecen como CAD, Desestimados o En proceso"),
                                                                 tags$li("Hechos CAD: Conformado por los hechos que poseen numeración HE y que, en producto aprobado, aparecen únicamente como CAD"),
                                                                 style = "text-align:left; color: black ; font-size: 16px"
                                                         )
                                                         
                                                         ),
                                                         
                                                 )
                                                 
                                                 
                                        ),
                                        
                                        #Hechos codificados: Monitoreo   
                                        tabPanel("Monitoreo", icon = icon("briefcase-medical"),    
                                                 
                                                 fluidRow(
                                                         column(6,
                                                                
                                                                br(),
                                                                p("Hechos codificados: valores faltantes",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_missings")
                                                         ),
                                                         column(6,
                                                                
                                                                br(),
                                                                p("Hechos codificados: tasa de error por codificador",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                plotlyOutput("plot_tasa_error")
                                                                
                                                         )
                                                 )
                                                 
                                                 
                                        ),
                                        
                                        #Hechos codificados: Estadísticos 
                                        tabPanel("Estadísticos Generales", icon = icon("briefcase-medical"),
                                                 
                                                 
                                                 fluidRow(
                                                         column(6,
                                                                br(),
                                                                p("Hechos codificados: hechos por departamento - estadísticos",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_hechos_dep_2021")
                                                         ),
                                                         column(6,
                                                                br(),
                                                                p("Hechos codificados: hechos por 100 mil habitantes por departamento - mapa",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                plotOutput("map_dep_2021")
                                                                
                                                         )
                                                 ),
                                                 
                                                 fluidRow(
                                                         column(6,
                                                                br(),
                                                                p("Hechos por tipología primaria",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_tipologia_2021")
                                                                
                                                         ),
                                                         column(6,
                                                                br(),
                                                                p("Hechos por entidad",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_entidad_2021")
                                                         )
                                                 ),
                                                 
                                                 fluidRow(
                                                         column(6,
                                                                br(),
                                                                p("Hechos por unidad orgánica del analista",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_uo_ara_2021")
                                                         )
                                                 )
                                                 
                                                 
                                        ),
                                        
                                        #Hechos codificados: Estadísticos corrupción
                                        tabPanel("Estadísticos Corrupción", icon = icon("briefcase-medical"),
                                                 
                                                 fluidRow(
                                                         
                                                         box(strong("Denuncias de corrupción (Total)"), width = 6, status = "primary", solidHeader = TRUE, 
                                                             style = "font-family: Georgia;text-align:center;color:#14505B ;font-size: 18px; padding-top: 20px",
                                                             
                                                             flexdashboard::gaugeOutput("corruption_count", width = "90%", height = "100px")
                                                         ),
                                                         
                                                         box(strong("Denuncias de corrupción (%)"), width = 6, status = "primary", solidHeader = TRUE, 
                                                             style = "font-family: Georgia;text-align:center;color:#14505B ;font-size: 18px; padding-top: 20px",
                                                             
                                                             flexdashboard::gaugeOutput("corruption_perc", width = "90%", height = "100px")
                                                         )
                                                         
                                                 ),
                                                 
                                                 fluidRow(
                                                         column(6,
                                                                br(),
                                                                p("Hechos codificados: Denuncias de corrupción por departamento - estadísticos",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_corruption_2021")
                                                                
                                                         ),
                                                         column(6,
                                                                br(),
                                                                p("Hechos codificados: Denuncias de corrupción por 100 mil habitantes por departamento - mapa",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                plotOutput("map_corruption_2021")
                                                         )
                                                 )
                                        )
                                )
                        )
                ),
                
                #####################
                # Denuncias históricas
                ####################
                tabItem(tabName = "claims_hist",
                        
                        fluidRow(
                                
                                p("Denuncias históricas", style = "text-align:right;color:black;font-size: 20px;font-weight: bold;background-color:white;padding:11px;border-radius:10px"),
                                
                                selectInput("year_selection_cgr", label = "Selecciona el año",
                                            choices = c("Todos: 2013-2020", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013")
                                ),
                                
                                tabBox(
                                        id = "claims_hist_tabs_cgr", width = 12, height = "650px",
                                        
                                        #Informacion General       
                                        tabPanel("Panorama General", icon = icon("briefcase-medical"),
                                                 
                                                 
                                                 fluidRow(
                                                         valueBoxOutput("historical_total_cgr", width = 12)
                                                 ),
                                                 
                                                 fluidRow(
                                                         valueBoxOutput("fur_historical_cgr", width = 3),
                                                         
                                                         valueBoxOutput("fud_historical_cgr", width = 3),
                                                         
                                                         valueBoxOutput("pde_historical_cgr", width = 3),
                                                         
                                                         valueBoxOutput("cad_historical_cgr", width = 3)
                                                         
                                                 ),
                                                 
                                                 fluidRow(
                                                         br(),br(),br(),br(),br(),
                                                         p(tags$ul("Descripción:", style = "text-align:left; color: black ; font-size: 17px; font-weight: bold"),
                                                           
                                                         ),
                                                         
                                                         p(tags$ul(
                                                                 tags$li("Hechos FUR: Conformado por los hechos que en Estado RAC son distintos de Anulado o Derivado"), 
                                                                 tags$li("Hechos FUD: Conformado por los hechos que son FUR y que en NUMEROFCERO* tienen valor de Asociado"), 
                                                                 tags$li("Hechos PDE: Conformado por los hechos que son FUD y que en producto propuesto son CAD, Control Simultáneo o Alerta Control"),
                                                                 tags$li("Hechos CAD: Conformado por los hechos que son FUD y que en producto aprobado son CAD, Control Simultáneo o Alerta Control"),
                                                                 style = "text-align:left; color: black ; font-size: 16px"
                                                           )
                                                           
                                                         ),
                                                         
                                                         p(br(), br(),
                                                           tags$ul("NUMEROFCERO: Documento que procede a la evaluación preparatoria", style = "text-align:left; color: black ; font-size: 14px")
                                                           
                                                         ),
     
                                                 )
                                                 
                                                 #fluidRow(
                                                 #        column(4,
                                                 #        ),
                                                         
                                                  #       column(4,
                                                  #              
                                                  #              p("Estado PDE",
                                                  #                style = "text-align:center; color: black ; font-size: 16px; font-weight: bold"),
                                                  #              
                                                  #              plotlyOutput("pde_status_historical_cgr")
                                                  #       )
                                                 #)
                                                 
                                        ),
                                        
                                        #Hechos Estadísticos 
                                        tabPanel("Estadísticos Generales", icon = icon("briefcase-medical"),
                                                 
                                                 fluidRow(
                                                         column(6,
                                                                br(),
                                                                p("Hechos por departamento - estadísticos",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_hechos_dep_historical_cgr")
                                                         ),
                                                         column(6,
                                                                br(),
                                                                p("Hechos por tipología primaria",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_tipologia_historical_cgr")
                                                                
                                                                #plotOutput("map_dep_historical_cgr")
                                                         )
                                                 ),
                                                 fluidRow(
                                                         column(6,
                                                                br(),
                                                                p("Hechos por entidad",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_entidad_historical_cgr")
                                                                
                                                         ),
                                                         column(6,
                                                                br(),
                                                                p("Hechos por unidad orgánica del analista",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_uo_ara_historical_cgr")
                                                         )
                                                 )
                                                 
                                        ),
                                        
                                        # Estadísticos corrupción
                                        tabPanel("Estadísticos Corrupción", icon = icon("briefcase-medical"),
                                                 
                                                 fluidRow(
                                                         
                                                         box(strong("Denuncias de corrupción (Total)"), width = 6, status = "primary", solidHeader = TRUE, 
                                                             style = "font-family: Georgia;text-align:center;color:#14505B ;font-size: 18px; padding-top: 20px",
                                                             
                                                             flexdashboard::gaugeOutput("corruption_count_historical_cgr", width = "90%", height = "100px")
                                                         ),
                                                         
                                                         box(strong("Denuncias de corrupción (%)"), width = 6, status = "primary", solidHeader = TRUE, 
                                                             style = "font-family: Georgia;text-align:center;color:#14505B ;font-size: 18px; padding-top: 20px",
                                                             
                                                             flexdashboard::gaugeOutput("corruption_perc_historical_cgr", width = "90%", height = "100px")
                                                         )
                                                         
                                                 ),
                                                 
                                                 fluidRow(
                                                         column(6,
                                                                br(),
                                                                p("Hechos codificados: Denuncias de corrupción por departamento - estadísticos",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_corruption_historical_cgr")
                                                                
                                                         )
                                                 )
                                        )
                                        
                                        
                                        
                                        
                                        
                                        
                                )
                                
                                
                                
                                
                        )
                        
                )
                
        ),
        
        fluidRow(

                #next button for tab items (CITIZEN)
                hidden(actionButton(inputId ="Next", label = icon("arrow-right"))),
                
                #previous/next button for tab items (CGR)
                hidden(actionButton(inputId ="Previous_cgr", label = icon("arrow-left"))),
                hidden(actionButton(inputId ="Next_cgr", label = icon("arrow-right")))
                
        )
        
   
)

shinyApp(
        ui = dashboardPage(skin = "red",
                           header, sidebar, body),
        
        server = function(input, output, session) {
                
                #INITIAL LOGGING
                logged <- reactiveValues(logged = FALSE, user = NULL)
                
                observeEvent(input$signin, {
                        if(input$name == user1 & input$pw == pw1) {
                                logged$logged <- TRUE
                                logged$user <- user1
                        } else if (input$name == user2 & input$pw == pw2) {
                                logged$logged <- TRUE
                                logged$user <- user2
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
                                
                        # CGR
                        } else if(logged$logged == TRUE & logged$user == user1) {
                                return(
                                        updateTabItems(session, "tabs", selected = "claims2021")
                                )
                                
                                
                        #CITIZEN
                        } else if(logged$logged == TRUE & logged$user == user2) {
                                return(
                                        updateTabItems(session, "tabs", selected = "info_survey")
                                )
                        } else {}
                })
                
                ############################
                #### CITIZEN
                ###########################
                
                #next button for tab items / https://stackoverflow.com/questions/44309328/generic-button-for-go-to-next-and-previous-tabitem-shiny
                
                tab_id <- c("info_survey", "statistics", "perception_survey")
                
                
                #Once the user clicks on submit, the next button appears on info survey and statistics page
                observeEvent(
                        input$submit,
                        {
                                observe({
                                        lapply(c("Next"),
                                               toggle,
                                               condition = input[["tabs"]] != "users" &  input[["tabs"]] != "perception_survey"  &  input[["tabs"]] != "statistics" &
                                                       input[["tabs"]] != "claims2021" &  input[["tabs"]] != "claims_hist") 
                                })
                        }
                )
                
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
                
                
                observeEvent(
                        input[["next_cgr2"]],
                        {
                                updateTabItems(session, "tabs", "perception_survey" )
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
                
                
                observeEvent(input$submit, {
                        showModal(modalDialog(
                                title = "Respuestas enviadas",
                                "Puede pasar a la siguiente sección. Se ha activado el botón para ir a la siguiente sección en la parte inferior izquierda."
                        ))
                })
                
                
                observeEvent(input$submit_2, {
                        showModal(modalDialog(
                                title = "Respuestas enviadas",
                                "Muchas gracias por participar de la encuesta"
                        ))
                })
                
                #hide submit_2 button once its clicked
                observeEvent(input$submit_2, { 
                        hide('submit_2')
                })
                
                
                # When the Submit button is clicked, save the form data
                observeEvent(input$submit_2, {
                        saveData(formData(), ip_user() )
                        
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
                        if (input$year_selection == "Todos: 2013-2021") {
                                n_claims_historical <- nrow(raw_historical_claims) + nrow(raw_claims_feb21)
                                
                        } else if (input$year_selection == "2021") {
                                n_claims_historical <- nrow(raw_claims_feb21)
                                
                                
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
                        
                        if (input$year_selection == "Todos: 2013-2021") {
                                fur_claims <- sum(raw_historical_claims$FUR_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_claims)
                                percent_fur <-  paste( round(fur_claims/n_claims_historical,2)*100, "%")
                                
                        } else if (input$year_selection == "2021") {
                                fur_claims <- sum(raw_claims_feb21$FUR_decision == "Yes")
                                percent_fur <-  paste( round(fur_claims/nrow(raw_claims_feb21),2)*100, "%")
                                
                                
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
                        
                        if (input$year_selection == "Todos: 2013-2021") {
                                fud_claims <- sum(raw_historical_claims$FUD_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_claims)
                                percent_fud <-  paste( round(fud_claims/n_claims_historical,2)*100, "%")
                                
                        } else if (input$year_selection == "2021") {
                                fud_claims <- sum(raw_claims_feb21$FUD_decision == "Yes")
                                percent_fud <-  paste( round(fud_claims/nrow(raw_claims_feb21),2)*100, "%")
                                
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
                        
                        if (input$year_selection == "Todos: 2013-2021") {
                                pde_claims <- sum(raw_historical_claims$PDE_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_claims)
                                percent_pde <-  paste( round(pde_claims/n_claims_historical,2)*100, "%")
                                
                        } else if (input$year_selection == "2021") {
                                pde_claims <- sum(raw_claims_feb21$PDE_decision == "Yes")
                                percent_pde <-  paste( round(pde_claims/nrow(raw_claims_feb21),2)*100, "%")
                                
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
                        
                        if (input$year_selection == "Todos: 2013-2021") {
                                cad_claims <- sum(raw_historical_claims$CAD_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_claims)
                                percent_cad <-  paste( round(cad_claims/n_claims_historical,2)*100, "%")
                                
                        } else if (input$year_selection == "2021") {
                                cad_claims <- sum(raw_claims_feb21$CAD_decision == "Yes")
                                percent_cad <-  paste( round(cad_claims/nrow(raw_claims_feb21),2)*100, "%")
                                
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
                
                
                #Evolución de los hechos 
                output$hechos_evolucion <- renderPlotly({
                        
                        #número de hechos totales por año
                        df_hechos <- as.data.frame(table(raw_historical_claims$year))
                        colnames(df_hechos) <- c("year", "n")
                        df_hechos$year <- as.character(df_hechos$year)
                        
                        df_hechos[nrow(df_hechos) + 1,] = c( "2021" ,nrow(raw_claims_feb21))
                        
                        #número de fur/fud/pde/cad
                        df_hechos_etapa <- raw_historical_claims %>% 
                                group_by(year) %>% 
                                summarise(n_fur = sum(FUR_decision == "Yes"),
                                          n_fud = sum(FUD_decision == "Yes"),
                                          n_pde = sum(PDE_decision == "Yes"),
                                          n_cad = sum(CAD_decision == "Yes")) %>% 
                                ungroup()
                        
                        
                        df_hechos_etapa[nrow(df_hechos_etapa) + 1,] = list(2021,
                                                                        sum(raw_claims_feb21$FUR_decision == "Yes"),
                                                                        sum(raw_claims_feb21$FUD_decision == "Yes"),
                                                                        sum(raw_claims_feb21$PDE_decision == "Yes"),
                                                                        sum(raw_claims_feb21$CAD_decision == "Yes"))
                        
                        df_hechos_etapa$year <- as.character(df_hechos_etapa$year)
                        
                        #merge
                        df_hechos <- df_hechos %>% 
                                left_join(df_hechos_etapa, by="year")
                        
                
                        #barplot
                        if (input$select_hechos == "Hechos totales"){
                                plot_ly(df_hechos, x = ~year, y = ~as.integer(n), type = 'bar',
                                        marker = list(color = '#c587ff') ) %>% 
                                        layout(title = "<b>Evolución de los hechos totales</b>" ,
                                               xaxis = list(title = "Año",
                                                            zeroline = FALSE),
                                               yaxis = list(title = "Cantidad de hechos",
                                                            zeroline = FALSE))
                                
                        }else if (input$select_hechos == "Hechos FUR"){
                                plot_ly(df_hechos, x = ~year, y = ~n_fur, type = 'bar',
                                        marker = list(color = '#c587ff') ) %>% 
                                        layout(title = "<b>Evolución de los hechos FUR</b>" ,
                                               xaxis = list(title = "Año",
                                                            zeroline = FALSE),
                                               yaxis = list(title = "Cantidad de hechos FUR",
                                                            zeroline = FALSE))
                                
                        }else if (input$select_hechos == "Hechos FUD"){
                                plot_ly(df_hechos, x = ~year, y = ~n_fud, type = 'bar',
                                        marker = list(color = '#c587ff') ) %>% 
                                        layout(title = "<b>Evolución de los hechos FUD</b>" ,
                                               xaxis = list(title = "Año",
                                                            zeroline = FALSE),
                                               yaxis = list(title = "Cantidad de hechos FUD",
                                                            zeroline = FALSE))
                                
                        }else if (input$select_hechos == "Hechos PDE"){
                                plot_ly(df_hechos, x = ~year, y = ~n_pde, type = 'bar',
                                        marker = list(color = '#c587ff') ) %>% 
                                        layout(title = "<b>Evolución de los hechos PDE</b>" ,
                                               xaxis = list(title = "Año",
                                                            zeroline = FALSE),
                                               yaxis = list(title = "Cantidad de hechos PDE",
                                                            zeroline = FALSE))
                                
                        }else if (input$select_hechos == "Hechos CAD"){
                                plot_ly(df_hechos, x = ~year, y = ~n_cad, type = 'bar',
                                        marker = list(color = '#c587ff') ) %>% 
                                        layout(title = "<b>Evolución de los hechos CAD</b>" ,
                                               xaxis = list(title = "Año",
                                                            zeroline = FALSE),
                                               yaxis = list(title = "Cantidad de hechos CAD",
                                                            zeroline = FALSE))
                        }
                                
                })
                
                
                #Hechos por departamento - estadísticos
                output$table_hechos_dep_historical <- renderReactable({
                        
                        hechos_dep_census <- read.csv('hechos_dep_census_claims_historical.csv')
                        
                        hechos_dep_census_2021 <- read.csv('hechos_dep_census_claims_21.csv')
                        
                        #claims 2021
                        hechos_dep_census_2021 <- hechos_dep_census_2021 %>% 
                                select(departamento, n, pop17) %>% 
                                mutate(year=2021)
                        
                        hechos_dep_census_2021 <- hechos_dep_census_2021[, c("departamento", "year", "n", "pop17")]
                        colnames(hechos_dep_census_2021) <- c("departamento", "year", "n_year", "pop17")
                        
                        #append claims 2021 + historical
                        
                        hechos_dep_census <- rbind(hechos_dep_census_2021,  hechos_dep_census)
                        
                        
                        if (input$year_selection == "Todos: 2013-2021") {
                                
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
                                hechos_dep_census[,c("departamento","n", "percentage","N_dpt_claims100Kpop")],
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
                

                
                #Evolución de denuncias por departamento
                output$hechos_dep_hist <- renderPlotly({ 
                        
                        hechos_dep_census <- read.csv('hechos_dep_census_claims_historical.csv')
                        
                        hechos_dep_census_2021 <- read.csv('hechos_dep_census_claims_21.csv')
                        
                        #claims 2021
                        hechos_dep_census_2021 <- hechos_dep_census_2021 %>% 
                                select(departamento, n, pop17) %>% 
                                mutate(year=2021)
                        
                        hechos_dep_census_2021 <- hechos_dep_census_2021[, c("departamento", "year", "n", "pop17")]
                        colnames(hechos_dep_census_2021) <- c("departamento", "year", "n_year", "pop17")
                        
                        #append claims 2021 + historical  (select just one department)
                        
                        hechos_dep_evolucion <- rbind(hechos_dep_census_2021,  hechos_dep_census) %>%  
                                filter(departamento == input$dep) 
                                
                        
                        #barplot
                        plot_ly(hechos_dep_evolucion, x = ~year, y = ~n_year, type = 'bar',
                                marker = list(color = '#c587ff') ) %>% 
                                layout(title = paste("<b>Evolución de los hechos totales en</b>", input$dep ) ,
                                       xaxis = list(title = "Año",
                                                    zeroline = FALSE),
                                       yaxis = list(title = "Cantidad de hechos",
                                                    zeroline = FALSE))
                        
                })
                
                #Evolución de denuncias corruptas por departamento
                output$hechos_corrup_dep_hist <- renderPlotly({ 
                        
                        corruption_dep_census <- read.csv('corruption_dep_census_claims_historical.csv')
                        
                        corruption_dep_census_2021 <- read.csv('corruption_dep_census_claims_21.csv')
                        
                        #claims 2021
                        corruption_dep_census_2021 <- corruption_dep_census_2021 %>% 
                                select(departamento, n, pop17) %>% 
                                mutate(year=2021)
                        
                        corruption_dep_census_2021 <- corruption_dep_census_2021[, c("departamento", "year", "n", "pop17")]
                        colnames(corruption_dep_census_2021) <- c("departamento", "year", "n_year", "pop17")
                        
                        #append claims 2021 + historical (select just one department)
                        
                        corruption_dep_census <- rbind(corruption_dep_census_2021,  corruption_dep_census)%>%  
                                filter(departamento == input$dep) 
  
                        #barplot
                        plot_ly(corruption_dep_census, x = ~year, y = ~n_year, type = 'bar',
                                marker = list(color = '#3fc1c9') ) %>% 
                                layout(title = paste("<b>Evolución de los Denuncias de corrupción en</b>", input$dep ) ,
                                       xaxis = list(title = "Año",
                                                    zeroline = FALSE),
                                       yaxis = list(title = "Cantidad de hechos corruptos",
                                                    zeroline = FALSE))
                        
                })
                

                #Hechos codificados: Denuncias de corrupción por departamento - estadísticos
                output$table_corruption_historical <- renderReactable({
                        
                        corruption_dep_census <- read.csv('corruption_dep_census_claims_historical.csv')
                        
                        corruption_dep_census_2021 <- read.csv('corruption_dep_census_claims_21.csv')
                        
                        #claims 2021
                        corruption_dep_census_2021 <- corruption_dep_census_2021 %>% 
                                select(departamento, n, pop17) %>% 
                                mutate(year=2021)
                        
                        corruption_dep_census_2021 <- corruption_dep_census_2021[, c("departamento", "year", "n", "pop17")]
                        colnames(corruption_dep_census_2021) <- c("departamento", "year", "n_year", "pop17")
                        
                        #append claims 2021 + historical
                        
                        corruption_dep_census <- rbind(corruption_dep_census_2021,  corruption_dep_census)
                        
                        
                        
                        if (input$year_selection == "Todos: 2013-2021") {
                                
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
                
                
                
                ############################
                #### CGR
                ###########################
                
                #previous/next button for tab items
                
                tab_id_cgr <- c("claims2021", "claims_hist")
                
                observe({
                        lapply(c("Next_cgr", "Previous_cgr"),
                               toggle,
                               condition = input[["tabs"]] != "users" &  input[["tabs"]] != "info_survey" &
                                           input[["tabs"]] != "statistics" & input[["tabs"]] != "perception_survey" )
                })
                
                Current_cgr <- reactiveValues(
                        Tab = "claims2021"
                )
                
                observeEvent(
                        input[["tabs"]],
                        {
                                Current_cgr$Tab <- input[["tabs"]]
                        }
                )
                
                observeEvent(
                        input[["Previous_cgr"]],
                        {
                                tab_id_position <- match(Current_cgr$Tab, tab_id_cgr) - 1
                                if (tab_id_position == 0) tab_id_position <- length(tab_id_cgr)
                                Current_cgr$Tab <- tab_id_cgr[tab_id_position]
                                updateTabItems(session, "tabs", tab_id_cgr[tab_id_position]) 
                        }
                )
                
                observeEvent(
                        input[["Next_cgr"]],
                        {
                                tab_id_position <- match(Current_cgr$Tab, tab_id_cgr) + 1
                                if (tab_id_position > length(tab_id_cgr)) tab_id_position <- 1
                                Current_cgr$Tab <- tab_id_cgr[tab_id_position]
                                updateTabItems(session, "tabs", tab_id_cgr[tab_id_position]) 
                        }
                )
                
                
                
                ###################
                # Denuncias 2021
                ##################
                
                output$fur_status_2021 <- renderPlotly({
                        
                        fur_status_df <- as.data.frame(table(FUR_categories_df$FUR_decision_categories))
                        
                        colnames(fur_status_df) <- c("fur_status", "n")
                        
                        fur_status_df %>% 
                                plot_ly(labels = ~fur_status, values = ~n, marker = list(colors = c('#00aa7f', '#ccccc7', '#ff0000'))) %>% 
                                add_pie(hole = 0.55, textfont = list(size = 13)) %>% 
                                layout(
                                        #title = "Estado FUR",
                                        showlegend = T,
                                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) 
                })
                
                
                
                output$fud_status_2021 <- renderPlotly({ 
                        #Donut chart: fud status  - categories
                        fud_status_df <- as.data.frame(table(FUD_categories_df$FUD_decision_categories))
                        
                        colnames(fud_status_df) <- c("fud_status", "n")
                        
                        fud_status_df %>% 
                                plot_ly(labels = ~fud_status, values = ~n, marker = list(colors = c('#00aa7f', '#ccccc7', '#ff0000'))) %>% 
                                add_pie(hole = 0.55, textfont = list(size = 13)) %>% 
                                layout(
                                        #title = "Estado FUD",
                                        showlegend = T,
                                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                        
                })
                
                output$pde_status_2021 <- renderPlotly({ 
                        #Donut chart: PDE status - categories
                        pde_status_df <- as.data.frame(table(PDE_categories_df$PDE_decision_categories))
                        
                        colnames(pde_status_df) <- c("pde_status", "n")
                        
                        pde_status_df %>% 
                                plot_ly(labels = ~pde_status, values = ~n, marker = list(colors = c('#00aa7f', '#ccccc7', '#ff0000'))) %>%
                                add_pie(hole = 0.55, textfont = list(size = 13)) %>% 
                                layout(
                                        #title = "Estado PDE",
                                        showlegend = T,
                                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                        
                })
                
                output$table_missings <- renderReactable({
                        
                        #dataframe with two columns: variable and number of missings
                        df_missings_claims_21 <- read.csv('df_missings_claims_21.csv')
                        
                        reactable(
                                df_missings_claims_21,
                                pagination = TRUE,
                                defaultSorted = "percentage_missing",
                                defaultColDef = colDef(headerClass = "header", align = "left"),
                                columns = list(
                                        variable = colDef(name = "Variable codificada"
                                                          
                                        ),
                                        n_missings = colDef(
                                                name = "N° de valores faltantes",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(df_missings_claims_21$n_missing), "%")
                                                        # Add thousands separators
                                                        value <- format(value, big.mark = ",")
                                                        bar_chart(value, width = width, fill = "#39a7ad")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        percentage_missing = colDef(
                                                name = "% de valores faltantes",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                        bar_chart(value, width = value, fill = "#ff0000", background = "#e1e1e1")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        )
                                )
                        )
                        
                })
                
                
                output$plot_tasa_error <- renderPlotly({ 
                        
                        ### Create a plot with error rate per encoder (Andrea/Edwar) (x axis is for number of batch, y for the error rate)
                        
                        data_plot <- read.csv('data_plot_claims_21.csv')
                        
                        #barplot: a plot with error rate per encoder (Andrea/Edwar)
                        plot_ly(data_plot, x = ~date_batch, y = ~error_rate_andrea, type = 'bar',  name = 'Andrea',
                                marker = list(color = '#ccccc7') ) %>% 
                                add_trace(y = ~error_rate_edwar, name = 'Edwar',  marker = list(color = '#00aa7f') ) %>% 
                                add_trace(y = ~error_rate_gannen, name = 'Gannen',  marker = list(color = '#cca1ed') ) %>% 
                                layout(title = "",
                                       xaxis = list(title = "Fecha del batch",
                                                    zeroline = FALSE),
                                       yaxis = list(title = "Tasa de error (%)",
                                                    zeroline = FALSE))
                        
                })
                
                
                output$table_hechos_dep_2021 <- renderReactable({
                        
                        hechos_dep_census <- read.csv('hechos_dep_census_claims_21.csv')
                        
                        reactable(
                                hechos_dep_census[,c("departamento", "n", "percentage", "N_dpt_claims100Kpop")],
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
                
                
                output$map_dep_2021 <- renderPlot({
                        
                        ggplot() +
                                geom_polygon(data = departamentos_fortified_, aes(fill = N_dpt_claims100Kpop, x = long, y = lat, group = group), colour= "gray", size=0.05, alpha=0.9) +
                                theme_void() +
                                scale_fill_viridis(option = 'D',  direction = -1, trans = "log2",  name="N° de hechos (en 100 miles)") +
                                #scale_fill_viridis(option = 'D', trans = "log2",  breaks = trans_breaks("log2", function(x) 2^x), name="N° de hechos") +
                                theme(  
                                        text = element_text(size= 13, color = "#22211d"),
                                        
                                        plot.title = element_text(size= 14, hjust=0.01, color = "black", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
                                        plot.subtitle = element_text(size= 12, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
                                        plot.caption = element_text( size=10, color = "black", margin = margin(b = 0.3, r=-99, unit = "cm") ),
                                ) +
                                coord_map()
                })
                
                
                output$table_entidad_2021 <- renderReactable({
                        
                        type_entity_count <- read.csv('type_entity_count_claims_21.csv')
                        
                        reactable(
                                type_entity_count,
                                pagination = TRUE,
                                defaultSorted = "n",
                                defaultColDef = colDef(headerClass = "header", align = "left"),
                                columns = list(
                                        tipo_de_entidad = colDef(name = "Tipo de entidad", width = 180),
                                        n = colDef(
                                                name = "N° de hechos",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(type_entity_count$n), "%")
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
                                        n_hec = colDef(
                                                name = "HEC", width = 60,
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                }),
                                        n_fur = colDef(
                                                name = "FUR", width = 60,
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                }),
                                        n_fud = colDef(
                                                name = "FUD", width = 60,
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                }),
                                        n_pde = colDef(
                                                name = "PDE", width = 60,
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                }),
                                        n_cad = colDef(
                                                name = "CAD", width = 60,
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                })
                                )
                        )
                })       
                
                
                output$table_tipologia_2021 <- renderReactable({
                        
                        primary_class_count <- read.csv("primary_class_count_claims_21.csv")
                        
                        reactable(
                                primary_class_count,
                                pagination = TRUE,
                                defaultSorted = "n",
                                defaultColDef = colDef(headerClass = "header", align = "left"),
                                columns = list(
                                        tipo_denuncia_primario_homologado = colDef(name = "Tipo de denuncia"
                                                                                   
                                        ),
                                        n = colDef(
                                                name = "N° de hechos",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(primary_class_count$n), "%")
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
                                        )
                                )
                        )
                        
                })
                
                output$table_uo_ara_2021 <- renderReactable({
                        
                        uo_ara_count <- read.csv("uo_ara_count_claims_21.csv")
                        
                        reactable(
                                uo_ara_count,
                                pagination = TRUE,
                                defaultSorted = "n",
                                defaultColDef = colDef(headerClass = "header", align = "left"),
                                columns = list(
                                        uo_ara = colDef(name = "Unidad Orgánica", width = 180),
                                        n = colDef(
                                                name = "N° de hechos",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(uo_ara_count$n), "%")
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
                                        n_hec = colDef(
                                                name = "HEC", width = 60,
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                }),
                                        n_fur = colDef(
                                                name = "FUR", width = 60,
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                }),
                                        n_fud = colDef(
                                                name = "FUD", width = 60,
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                }),
                                        n_pde = colDef(
                                                name = "PDE", width = 60,
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                }),
                                        n_cad = colDef(
                                                name = "CAD", width = 60,
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                })
                                )
                        )
                        
                })
                
                
                output$corruption_count <- flexdashboard::renderGauge({
                        
                        max = sum(corruption_claims_count$n)
                        
                        flexdashboard::gauge(corruption_claims_count$n[corruption_claims_count$corruption_denunc == 1], min = 0, max = max , gaugeSectors(
                                success = c(round((2/3)*max) + 1 , max ), warning = c(round(max/3) + 1, round((2/3)*max) ), danger = c(0, round(max/3) ))
                                , abbreviate = FALSE, abbreviateDecimals = 1)
                })
                
                output$corruption_perc <- flexdashboard::renderGauge({
                        
                        flexdashboard::gauge(corruption_claims_count$percentage[corruption_claims_count$corruption_denunc == 1], min = 0, max = 100, symbol = '%', gaugeSectors(
                                success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
                        ))
                }) 
                
                output$table_corruption_2021 <- renderReactable({
                        
                        corruption_dep_census <- read.csv('corruption_dep_census_claims_21.csv')
                        
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
                                                        width <- paste0(value * 100 / max(corruption_dep_census$N_dpt_claims100Kpop), "%")
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
                
                output$map_corruption_2021 <- renderPlot({
                        
                        ggplot() +
                                geom_polygon(data = departamentos_fortified_, aes(fill = N_dpt_claims100Kpop_corruption, x = long, y = lat, group = group), colour= "gray", size=0.05, alpha=0.9) +
                                theme_void() +
                                scale_fill_viridis(option = 'D',  direction = -1, trans = "log2",  name="N° de hechos (en 100 miles)") +
                                #scale_fill_viridis(option = 'D', trans = "log2",  breaks = trans_breaks("log2", function(x) 2^x), name="N° de hechos") +
                                theme(  
                                        text = element_text(size= 13, color = "#22211d"),
                                        
                                        plot.title = element_text(size= 14, hjust=0.01, color = "black", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
                                        plot.subtitle = element_text(size= 12, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
                                        plot.caption = element_text( size=10, color = "black", margin = margin(b = 0.3, r=-99, unit = "cm") ),
                                ) +
                                coord_map()
                })
                
                
                ###################
                # Denuncias Históricas
                ##################
                
                # total claims
                output$historical_total_cgr <- renderValueBox({
                        
                        
                        #creating n_claims_historical variable
                        if (input$year_selection_cgr == "Todos: 2013-2020") {
                                n_claims_historical <- nrow(raw_historical_claims)
                                
                        } else {
                                n_claims_historical <- nrow(subset(raw_historical_claims, year== input$year_selection_cgr ))
                                
                        }
                        
                        valueBox(value = paste(format(n_claims_historical, big.mark = ","), "", sep = " "),
                                 "Hechos totales",
                                 icon = icon("fa-solid fa-database"),             #https://fontawesome.com/v4/icons/
                                 color = "purple"
                        )
                })
                
                # fur claims
                output$fur_historical_cgr <- renderValueBox({
                        
                        if (input$year_selection_cgr == "Todos: 2013-2020") {
                                fur_claims <- sum(raw_historical_claims$FUR_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_claims)
                                percent_fur <-  paste( round(fur_claims/n_claims_historical,2)*100, "%")
                        } else {
                                #dataframe with specific year
                                raw_historical_year <- subset(raw_historical_claims, year== input$year_selection_cgr)
                                
                                fur_claims <- sum(raw_historical_year$FUR_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_year)
                                percent_fur <-  paste( round(fur_claims/n_claims_historical,2)*100, "%")
                        }
                        
                        valueBox(value = paste(format(fur_claims, big.mark = ","), "/", percent_fur  , sep = " "),
                                 "Hechos FUR (respecto a hechos totales)",
                                 icon = icon("fa-solid fa-folder-open"),            
                                 color = 'aqua')
                })
                
                # fud claims
                output$fud_historical_cgr <- renderValueBox({
                        
                        if (input$year_selection_cgr == "Todos: 2013-2020") {
                                fud_claims <- sum(raw_historical_claims$FUD_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_claims)
                                percent_fud <-  paste( round(fud_claims/n_claims_historical,2)*100, "%")
                        } else {
                                #dataframe with specific year
                                raw_historical_year <- subset(raw_historical_claims, year== input$year_selection_cgr)
                                
                                fud_claims <- sum(raw_historical_year$FUD_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_year)
                                percent_fud <-  paste( round(fud_claims/n_claims_historical,2)*100, "%")
                        }
                        
                        valueBox(value = paste(format(fud_claims, big.mark = ","), "/", percent_fud , sep = " "),
                                 "Hechos FUD (respecto a hechos totales)",
                                 icon = icon("fa-solid fa-folder-open"),            
                                 color = 'aqua')
                })
                
                # pde claims
                output$pde_historical_cgr <- renderValueBox({
                        
                        if (input$year_selection_cgr == "Todos: 2013-2020") {
                                pde_claims <- sum(raw_historical_claims$PDE_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_claims)
                                percent_pde <-  paste( round(pde_claims/n_claims_historical,2)*100, "%")
                        } else {
                                raw_historical_year <- subset(raw_historical_claims, year== input$year_selection_cgr)
                                
                                pde_claims <- sum(raw_historical_year$PDE_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_year)
                                percent_pde <-  paste( round(pde_claims/n_claims_historical,2)*100, "%")
                        }
                        valueBox(value = paste(format(pde_claims, big.mark = ","), "/", percent_pde  , sep = " "),
                                 "Hechos PDE (respecto a hechos totales)",
                                 icon = icon("fa-solid fa-folder-open"),            
                                 color = 'aqua')
                })
                
                #cad claims
                output$cad_historical_cgr <- renderValueBox({
                        
                        if (input$year_selection_cgr == "Todos: 2013-2020") {
                                cad_claims <- sum(raw_historical_claims$CAD_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_claims)
                                percent_cad <-  paste( round(cad_claims/n_claims_historical,2)*100, "%")
                        } else {
                                raw_historical_year <- subset(raw_historical_claims, year== input$year_selection_cgr)
                                
                                cad_claims <- sum(raw_historical_year$CAD_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_year)
                                percent_cad <-  paste( round(cad_claims/n_claims_historical,2)*100, "%")
                        }
                        valueBox(value = paste(format(cad_claims, big.mark = ","), "/", percent_cad , sep = " "),
                                 "Hechos CAD (respecto a hechos totales)",
                                 icon = icon("fa-solid fa-folder-open"),            
                                 color = 'aqua')
                })
                
                # estado pde
                output$pde_status_historical_cgr <- renderPlotly({ 
                        #Donut chart: PDE status - categories
                        PDE_categories_df <- read.csv("PDE_categories_df_historical.csv")
                        
                        if (input$year_selection_cgr == "Todos: 2013-2020") {
                                
                                pde_status_df <- as.data.frame(table(PDE_categories_df$PDE_decision_categories))
                                colnames(pde_status_df) <- c("pde_status", "n")
                        } else {
                                
                                PDE_categories_df <- subset(PDE_categories_df, year== input$year_selection_cgr)
                                pde_status_df <- as.data.frame(table(PDE_categories_df$PDE_decision_categories))
                                colnames(pde_status_df) <- c("pde_status", "n")
                        }
                        
                        pde_status_df %>% 
                                plot_ly(labels = ~pde_status, values = ~n, marker = list(colors = c('#00aa7f', '#ccccc7', '#ff0000'))) %>%
                                add_pie(hole = 0.55, textfont = list(size = 13)) %>% 
                                layout(
                                        #title = "Estado PDE",
                                        showlegend = T,
                                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                        
                })
                
                #Hechos por departamento - estadísticos
                output$table_hechos_dep_historical_cgr <- renderReactable({
                        
                        hechos_dep_census <- read.csv('hechos_dep_census_claims_historical.csv')
                        
                        if (input$year_selection_cgr == "Todos: 2013-2020") {
                                
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
                                        filter(year== input$year_selection_cgr) %>%
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
                
                #output$map_dep_historical_cgr <- renderPlot({
                
                
                #    if (input$year_selection_cgr == "Todos: 2013-2021") {
                
                #departamentos_fortified_historical <- departamentos_fortified_historical %>% 
                #      group_by(id,long,lat,order,hole,piece,group) %>% 
                #     mutate( n_claims = sum(n_claims_year) ) %>% 
                #    filter(row_number()==1)  %>% 
                #   mutate( N_dpt_claims100Kpop = round( n_claims/(pop17/100000) ,1) ) 
                
                #   } else {
                
                
                
                #  }
                
                
                #})
                
                #Hechos por tipología primaria
                output$table_tipologia_historical_cgr <- renderReactable({
                        
                        if (input$year_selection_cgr == "Todos: 2013-2020") {
                                
                                primary_class_count <- read.csv('primary_class_count_historical.csv')
                                
                        } else {
                                
                                primary_class_count <- subset(raw_historical_claims, tipodedenuncia_primario!= ""  & year== input$year_selection_cgr ) %>% 
                                        count(tipodedenuncia_primario, sort = TRUE) %>% 
                                        mutate( percentage = round( (n/sum(n) )*100, 1))
                        }
                        
                        reactable(
                                primary_class_count,
                                pagination = TRUE,
                                defaultSorted = "n",
                                defaultColDef = colDef(headerClass = "header", align = "left"),
                                columns = list(
                                        tipodedenuncia_primario = colDef(name = "Tipo de denuncia"
                                                                         
                                        ),
                                        n = colDef(
                                                name = "N° de hechos",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(primary_class_count$n), "%")
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
                                        )
                                )
                        )
                        
                })
                
                #Hechos por entidad
                output$table_entidad_historical_cgr <- renderReactable({
                        
                        if (input$year_selection_cgr == "Todos: 2013-2020") {
                                
                                type_entity_count_historical <- subset(raw_historical_claims, tipo_de_entidad!= "" ) %>%
                                        group_by(tipo_de_entidad) %>% 
                                        summarise( n= n() ) %>% 
                                        mutate( percentage = round( (n/sum(n))*100, 1 ) )
                                
                                
                        } else {
                                
                                type_entity_count_historical <- subset(raw_historical_claims, tipo_de_entidad!= "" & year== input$year_selection_cgr  ) %>%
                                        group_by(tipo_de_entidad) %>% 
                                        summarise( n= n() ) %>% 
                                        mutate( percentage = round( (n/sum(n))*100, 1 ) )
                                
                        }
                        
                        reactable(
                                type_entity_count_historical,
                                pagination = TRUE,
                                defaultSorted = "n",
                                defaultColDef = colDef(headerClass = "header", align = "left"),
                                columns = list(
                                        tipo_de_entidad = colDef(name = "Tipo de entidad", width = 180),
                                        n = colDef(
                                                name = "N° de hechos",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(type_entity_count_historical$n), "%")
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
                                        )
                                )
                        )
                        
                }) 
                
                #Hechos por unidad orgánica del analista
                output$table_uo_ara_historical_cgr <- renderReactable({
                        
                        if (input$year_selection_cgr == "Todos: 2013-2020") {
                                
                                uo_ara_count <- read.csv("uo_ara_count_historical.csv")
                                
                        } else {
                                
                                
                                uo_ara_count <- raw_historical_claims %>% subset(uo_ara!= "" & year== input$year_selection_cgr) %>%
                                        group_by(uo_ara) %>%
                                        summarise(
                                                n = n(),
                                                percentage = NaN
                                        )  %>%
                                        mutate( percentage = round( (n/sum(n) )*100, 1)
                                        )
                                
                        }
                        
                        reactable(
                                uo_ara_count,
                                pagination = TRUE,
                                defaultSorted = "n",
                                defaultColDef = colDef(headerClass = "header", align = "left"),
                                columns = list(
                                        uo_ara = colDef(name = "Unidad Orgánica", width = 180),
                                        n = colDef(
                                                name = "N° de hechos",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(uo_ara_count$n), "%")
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
                                        )
                                )
                        )
                        
                })
                
                #Denuncias de corrupción (Total)
                output$corruption_count_historical_cgr <- flexdashboard::renderGauge({
                        
                        
                        if (input$year_selection_cgr == "Todos: 2013-2020") {
                                
                                corruption_claims_count <- corruption_claims_count_historical %>% 
                                        group_by(corruption_denunc) %>% 
                                        summarise( n = sum(n) )
                                
                                max = sum(corruption_claims_count$n)
                                
                        } else {
                                
                                corruption_claims_count <- corruption_claims_count_historical %>% 
                                        filter(year== input$year_selection_cgr) 
                                
                                max = sum(corruption_claims_count$n)
                                
                        }
                        
                        
                        flexdashboard::gauge(corruption_claims_count$n[corruption_claims_count$corruption_denunc == 1], min = 0, max = max , gaugeSectors(
                                success = c(round((2/3)*max) + 1 , max ), warning = c(round(max/3) + 1, round((2/3)*max) ), danger = c(0, round(max/3) ))
                                , abbreviate = FALSE, abbreviateDecimals = 1)
                })
                
                #Denuncias de corrupción (%)
                output$corruption_perc_historical_cgr <- flexdashboard::renderGauge({
                        
                        if (input$year_selection_cgr == "Todos: 2013-2020") {
                                
                                corruption_claims_count <- corruption_claims_count_historical %>% 
                                        group_by(corruption_denunc) %>% 
                                        summarise( n = sum(n) )
                                
                                corruption_claims_count$percentage <- round( (corruption_claims_count$n/sum(corruption_claims_count$n) )*100, 1)
                                
                        } else {
                                
                                corruption_claims_count <- corruption_claims_count_historical %>% 
                                        filter(year== input$year_selection_cgr) 
                                
                                corruption_claims_count$percentage <- round( (corruption_claims_count$n/sum(corruption_claims_count$n) )*100, 1)
                                
                        }
                        
                        
                        flexdashboard::gauge(corruption_claims_count$percentage[corruption_claims_count$corruption_denunc == 1], min = 0, max = 100, symbol = '%', gaugeSectors(
                                success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
                        ))
                }) 
                
                
                #Hechos codificados: Denuncias de corrupción por departamento - estadísticos
                output$table_corruption_historical_cgr <- renderReactable({
                        
                        corruption_dep_census <- read.csv('corruption_dep_census_claims_historical.csv')
                        
                        
                        if (input$year_selection == "Todos: 2013-2020") {
                                
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
                                        filter(year== input$year_selection_cgr) %>%
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
                
                
                
                #link denuncias virtuales
                output$frame <- renderUI({
                        my_test <- tags$iframe(src="https://www.gob.pe/institucion/contraloria/campa%C3%B1as/6643-denuncias-ciudadanas-a-traves-del-formulario-virtual", height=500, width=1400)
                        print(my_test)
                        my_test
                })
                
                
                
                
        }
)

