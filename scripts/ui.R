library(shiny)
library(DBI)
library(dplyr)


#runApp("C:/Users/Yoseph/Documents/GitHub/complaints_dashboards/scripts")

# Obtaining the values of the environment variables

db <- Sys.getenv("db_algorithm")  #database name
db_host <- Sys.getenv("db_host") #localhost
db_port <- Sys.getenv("db_port") #port
db_user <- Sys.getenv("db_user") #credentials: user name
db_pass <- Sys.getenv("db_pass") #credentials: user password


#Connecting to the Postgres database

conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = db,
  host = db_host,
  port = db_port,
  user = db_user,
  password = db_pass
)


#TEMPORAL: (delete after last commit)
#setwd("C:/BID_Columbia")
#raw_claims_feb21 <-import("raw_claims_feb21.dta")

raw_claims_feb21 <- dbReadTable(conn, "raw_claims_feb21")
clean_claims_21 <- dbReadTable(conn, "clean_claims_21")



# Number of claims
n_claims <- nrow(raw_claims_feb21)


# Number of encoded claims
n_encoded <- nrow(clean_claims_21)
percent_encoded <- paste( round(n_encoded/n_claims,2)*100, "%")


# Recategorization for fur status (dummy)
raw_claims_feb21 <- mutate(raw_claims_feb21, FUR_decision =
                                   case_when(estado_hec == "Calificado" | estado_hec == "Concluido" ~ "Yes",
                                             estado_hec == "Anulado" | estado_hec == "En Proceso" | estado_hec == "Pendiente" ~ "No" ) )

# Recategorization for fud status (dummy)
raw_claims_feb21$FUD_decision<- ifelse(raw_claims_feb21$FUR_decision == "Yes" & raw_claims_feb21$numero_fud != "", "Yes", "No")


# Recategorization for PDE status (dummy)
raw_claims_feb21$PDE_decision<- ifelse(raw_claims_feb21$numero_he != "" & (raw_claims_feb21$producto_aprobado == "Carpeta Atención de Denuncia" | raw_claims_feb21$producto_aprobado == "Desestimado" | raw_claims_feb21$producto_aprobado == "En proceso") , "Yes", "No")


# Recategorization for CAD status (dummy)
raw_claims_feb21$CAD_decision<- ifelse(raw_claims_feb21$numero_he != "" &  raw_claims_feb21$producto_aprobado == "Carpeta Atención de Denuncia" , "Yes", "No")


##Aprobado, rechazado, pendiente categories

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


#FUR CLAIMS
fur_claims <- sum(raw_claims_feb21$FUR_decision == "Yes")
percent_fur <-  paste( round(fur_claims/n_claims,2)*100, "%")

#FUD CLAIMS
fud_claims <- sum(raw_claims_feb21$FUD_decision == "Yes")
percent_fud <-  paste( round(fud_claims/n_claims,2)*100, "%")


#PDE CLAIMS
pde_claims <- sum(raw_claims_feb21$PDE_decision == "Yes")
percent_pde <-  paste( round(pde_claims/n_claims,2)*100, "%")


#CAD CLAIMS
cad_claims <- sum(raw_claims_feb21$CAD_decision == "Yes")
percent_cad <-  paste( round(cad_claims/n_claims,2)*100, "%")




ui <- navbarPage(
        
        title="Dashboard de Denuncias",
        
        
        tabPanel("Denuncias 2021",
                
                 
                 fluidRow(
                         
                         column(width = 6, align = "center",
                                 
                                 p("Hechos Totales", HTML('&nbsp;'), strong(n_claims) , HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), icon("fa-solid fa-database")  
                                   ,  style="text-align:center;font-size: 20px;color:white;background-color:#7636b3;padding:15px")
                                 
                         ),
                         
                         
                         column(width = 6, align = "center",
                                
                                p("Hechos codificados:", HTML('&nbsp;'), strong(percent_encoded) , HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), icon("fa-solid fa-database")  
                                  ,  style="text-align:center;font-size: 20px;color:white;background-color:#a249f5;padding:15px")                                
                                
                         )
                         
                 ),
                 
                 
                 fluidRow(
                         
                         column(width = 3, align = "center",
                                
                                p("Hechos FUR", HTML('&nbsp;'), strong(fur_claims) , HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), icon("fa-solid fa-database")  
                                  ,  style="text-align:center;font-size: 20px;color:white;background-color:#7636b3;padding:15px")
                                
                         ),
                         
                         
                         column(width = 3, align = "center",
                                
                                p("Hechos FUD:", HTML('&nbsp;'), strong(fud_claims) , HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), icon("fa-solid fa-database")  
                                  ,  style="text-align:center;font-size: 20px;color:white;background-color:#a249f5;padding:15px")                                
                                
                         ),
                         
                         column(width = 3, align = "center",
                                
                                p("Hechos PDE", HTML('&nbsp;'), strong(pde_claims) , HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), icon("fa-solid fa-database")  
                                  ,  style="text-align:center;font-size: 20px;color:white;background-color:#7636b3;padding:15px")
                                
                         ),
                         
                         
                         column(width = 3, align = "center",
                                
                                p("Hechos CAD:", HTML('&nbsp;'), strong(cad_claims) , HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), icon("fa-solid fa-database")  
                                  ,  style="text-align:center;font-size: 20px;color:white;background-color:#a249f5;padding:15px")                                
                                
                         )                         
                         
                 )
                 
                
                 
        ),
        
        tabPanel("Denuncias Históricas")
        
        
)


