library(shiny)
library(rio)
library ('plyr')
library(dplyr)
library(tidyverse)
library(plotly)
library(reactable)
library(htmltools)
library(ggplot2)
library(ggiraph)
library(DBI)


#TEMPORAL: (delete after last commit)
setwd("C:/BID_Columbia/claims_assigment/outputs_temp")

#clean_claims_21 <-import("stacking_encoded.dta")
clean_claims_21 <-import("clean_claims_v4.dta")

#keep the last version
last_version <- max(clean_claims_21$version)

clean_claims_21 <- clean_claims_21[ clean_claims_21$version == last_version, ]


#TEMPORAL: (delete after last commit)
setwd("C:/BID_Columbia")

raw_claims_feb21 <-import("raw_claims_feb21.dta")

raw_historical_claims <-import("raw_historical_claims.dta")




ui <- fluidPage(
        
        sidebarLayout(
                sidebarPanel(
                        sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100)
                ),
                mainPanel(plotOutput("distPlot"))
        )
        
)







