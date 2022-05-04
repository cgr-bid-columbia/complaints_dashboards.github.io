library(DBI)
library ('plyr')
library(dplyr)
library(tidyverse)
library(haven)


setwd("C:/Users/Yoseph/Documents/GitHub/CGR_dashboard")

#------------------------------
# Output: clean claims table
#------------------------------

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

## --- INPUT ---- 
# import clean claims 21 table 
clean_claims_21 = dbReadTable(conn, "clean_claims_21")


#keep the last version
clean_claims_21 <- clean_claims_21[ clean_claims_21$version == max(clean_claims_21$version), ]


#Cleaning data
clean_claims_21[ clean_claims_21 == "NAN" | clean_claims_21 == "nan" |  clean_claims_21 == "." |  clean_claims_21 == "," ] <- ""


## --- OUTPUT ---- 


#------------------------------
# Output: df missings
#------------------------------

df_missings <- ldply(clean_claims_21, function(var_value) sum(var_value =="")) %>% 
colnames(df_missings) <- c("variable", "n_missings")

#adding a new column: percentage of missings obs 
df_missings$percentage_missing = round( (df_missings$n_missings/ nrow(clean_claims_21))*100 , 1)

## --- OUTPUT ---- 


#------------------------------
# Output: df plot error encoders
#------------------------------

#keeping just claims reviewed by Jaime
tie_breaking_append <- clean_claims_21[ clean_claims_21$tiebraker_name == "Jaime", ] %>%
                       select(chosen_encoder, date_batch)


names_encoders <- c("andrea", "edwar")
#names_encoders <- c("andrea", "edwar", "gannen")

#Encoders' number of errors
for(name in names_encoders) {                                                              
        
        #creates columns with zeros 
        n_error <- rep(0, nrow(tie_breaking_append))                                       # Create new column
        tie_breaking_append[ , ncol(tie_breaking_append) + 1] <- n_error                   # Append new column
        colnames(tie_breaking_append)[ncol(tie_breaking_append)] <- paste0("error_", name) # Rename column name
        
        #fills colums with one when there's an error
        if(name == "andrea"){
                
                tie_breaking_append$error_andrea[tie_breaking_append$chosen_encoder != "Andrea" & tie_breaking_append$chosen_encoder != "Andrea/Edwar"] <- 1
                
        } else if(name == "edwar"){
                
                tie_breaking_append$error_edwar[tie_breaking_append$chosen_encoder != "Edwar" & tie_breaking_append$chosen_encoder != "Andrea/Edwar"] <- 1
          
          # for gannen     
        } #else{ 
        
               #tie_breaking_append$error_gannen[tie_breaking_append$chosen_encoder != "Gannen" & tie_breaking_append$chosen_encoder != "Gannen/Edwar"] <- 1

        #}
}


#Number of observations per batches
df2<- tie_breaking_append %>%
        group_by(date_batch) %>%
        summarise(obs = n()) 


#Number of errors per encoder and batch
for(name in names_encoders) {
        
        df <- aggregate(tie_breaking_append %>% select( paste0("error_", name) ), by=list(date_batch=tie_breaking_append$date_batch), FUN=sum)
        assign( paste0("df_", name) , df)

        
}


#Error rate per encoder and batch  
df_list <- list(df2, df_andrea, df_edwar)        #put all data frames into list
#df_list <- list(df2, df_andrea, df_gannen) 

data_plot <- df_list %>%
        reduce(full_join, by='date_batch') %>%  #merge all data frames in list
        `colnames<-`(c("date_batch", "obs", "error_andrea", "error_edwar")) %>% 
        mutate(
                error_rate_andrea <- round( (error_andrea/obs)*100 , 1),
                error_rate_edwar <- round( (error_edwar/obs)*100 , 1)#,
                #error_rate_gannen <- round( (error_gannen/obs)*100 , 1)
        ) 

colnames(data_plot) <- c("date_batch", "obs", "error_andrea", "error_edwar", "error_rate_andrea", "error_rate_edwar" )

## --- OUTPUT ---- 


#------------------------------
# Output: df claims per region
#------------------------------

hechos_dep <- subset(mutate_each(clean_claims_21, funs(toupper)), DEPARTAMENTO!= "" )

#delete special characters from DEPARTAMENTO
hechos_dep$DEPARTAMENTO <- iconv(hechos_dep$DEPARTAMENTO, from="UTF-8",to="ASCII//TRANSLIT")

#Solving some issues with DEPARTAMENTO (temporal)
hechos_dep$DEPARTAMENTO <- gsub('AREQUIPA, LIMA', 'AREQUIPA', hechos_dep$DEPARTAMENTO)
hechos_dep$DEPARTAMENTO <- gsub('LIMA, AYACUCHO', 'LIMA', hechos_dep$DEPARTAMENTO)
hechos_dep$DEPARTAMENTO <- gsub('MINISTERIO DE DEFENSA', 'LIMA', hechos_dep$DEPARTAMENTO)
hechos_dep$DEPARTAMENTO <- gsub('PIURA, LIMA', 'LIMA', hechos_dep$DEPARTAMENTO)
hechos_dep$DEPARTAMENTO <- gsub('TUMBES, PIURA, LAMBAYEQUE, CALLAO, TACNA, AREQUIPA', 'TUMBES', hechos_dep$DEPARTAMENTO)
hechos_dep$DEPARTAMENTO <- gsub('MONQUEGUA', 'MOQUEGUA', hechos_dep$DEPARTAMENTO)
hechos_dep$DEPARTAMENTO <- gsub('TRUJILLO', 'LA LIBERTAD', hechos_dep$DEPARTAMENTO)
hechos_dep$DEPARTAMENTO <- gsub('URUBAMBA', 'CUSCO', hechos_dep$DEPARTAMENTO)
hechos_dep$DEPARTAMENTO <- gsub('SAN AGUSTIN', 'AREQUIPA', hechos_dep$DEPARTAMENTO)
hechos_dep$DEPARTAMENTO <- gsub('CELENDIN', 'CAJAMARCA', hechos_dep$DEPARTAMENTO)
hechos_dep$DEPARTAMENTO <- gsub('CARHUAZ', 'ANCASH', hechos_dep$DEPARTAMENTO)
hechos_dep$DEPARTAMENTO <- gsub('AOURIMAC', 'APURIMAC', hechos_dep$DEPARTAMENTO)


hechos_dep <- hechos_dep %>%
        group_by(DEPARTAMENTO) %>%
        summarise( n = n() ) 

hechos_dep$percentage <- round( (hechos_dep$n/sum(hechos_dep$n) )*100, 1)


## --- INPUT ---- 
census_dep <-read_stata("data/departments_census2017.dta")


#claims per department, per 100k inhabitants
hechos_dep_census <- merge(x = hechos_dep, y = census_dep, by.x = "DEPARTAMENTO", by.y = "dep" )
hechos_dep_census$N_dpt_claims100Kpop <- round( hechos_dep_census$n/(hechos_dep_census$pop17/100000) ,1)


## --- OUTPUT ---- 


#------------------------------
# Output: 
#-----------------------------



