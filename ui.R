#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library("red")
library("RJSONIO")
library("ritis")
library("plotly")
library("lubridate")
library("reshape")
library("dplyr")
library("plotly")
library("tidyverse")
library("tidyr")

# Define UI for application that draws a histogram
ui <- navbarPage("ESI-Congo",
                 tabPanel("Dataviewer"),
                 tabPanel("Spatial viewer"),
                 tabPanel("Protocole"),
                 tabPanel("Import dataset"),
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("var1",label = "Taxonomic rank",choices = list("choice 1" = 1, "choice 2" = 2), selected = 1),
                     selectInput("var2",label = "Taxa (single or multiple)",choices = list("single" = 1, "multipe" = 2),selected = 1),
                     selectInput("var3",label= "Market (single or multiple)",choices = list("Marche Central" = "Marche Central", "Marche Faubourg" = "Marche Faubourg","Marche Kouikou" = "Marche Kouikou","Marche Liberte" = "Marche Liberte","Marche Mayaka" = "Marche Mayaka","Marche Mvoumvou" = "Marche Mvoumvou","Marche Nkouikou" = "Marche Nkouikou", "Marche Tie-Tie" = "Marche Tie-Tie", "Tous les marchés" = "Tous les marchés"),selected = "Tous les marchés"),
                     checkboxInput("checkbox", label = "Display weighted data : Number of bush meat occurences divided by number of visits", value = FALSE),
                     dateRangeInput("dates", label = "Date range",start = "2008-01-01", end = "2018-12-31", min = "2008-01-01",max = "2018-12-31", format = 'yyyy', startview = "decade"),
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel( 
                     plotlyOutput("plotly"),
                   )
                 ),
                 DT::dataTableOutput("DT")
)
