#####Useful library#####

#library("red")
#library("RJSONIO")
#library("ritis")
#library("lubridate")
library("plotly")
library("reshape")
library("dplyr")
library("tidyverse")
library("tidyr")
library(shiny)

######################

# Define server logic 
server <- function(input, output) {
  ######Plotly#####
  output$plotly <- renderPlotly({
    #####Checking date#####
    dates_2 <- as.numeric(substring(input$dates,1,4))
    if (dates_2[1] == 2010){
      dates_2[1] <- 2011
    }
    i <- dates_2[1]
    dates <- as.character(i)
    i <- i+1
    while (i != dates_2[2]+1) {
      if (i != 2010) {
        dates <- c(dates,as.character(i))
        i <- i+1
      } else {
        i <- i+1
      }
    }
    #####################
    #####Checking checkbox#####
    if (input$checkbox) {
      yaxis <- list(
        title = 'Fréquences cumulées',
        cex.axis =0.5,
        cex.lab = 0.5
      )
      #####checking marché#####
      if (input$var3 != 'Tous les marchés') {
        nb_esp_annee_lieu <- nb_esp_annee_lieu[which(nb_esp_annee_lieu$marche == input$var3),]
        nb_esp_annee_lieu <- nb_esp_annee_lieu[,-3]
        ####Création du graphique####
        py_nb_esp_annee_lieu <- plot_ly(type = 'bar') %>%
          layout(yaxis = yaxis, margin = m,xaxis = xaxis, barmode = "stack")
        for (i in dates) {
          py_nb_esp_annee_lieu<- add_trace(py_nb_esp_annee_lieu,x = ~unique(nb_esp_annee_lieu[,"especes"]),y = nb_esp_annee_lieu[which(nb_esp_annee_lieu[,"annee"] == i),"Freq"], name = i)
        }
        py_nb_esp_annee_lieu
        ############################
      } else {
        ####Création du graphique####
        py <- plot_ly(type = 'bar') %>%
          layout(yaxis = yaxis, margin = m,xaxis = xaxis, barmode = "stack")
        for (i in dates) {
          py <- add_trace(py,x = ~unique(tolower(b_anl[,"especes"])), y = b_anl[which(b_anl[,"annee"] == i),"Freq"], name = i)
        }
        py
        ############################
      }
      #######################
    }
    else{
      yaxis <- list(
        title = 'Fréquences (occurances/nbre visites) cumulées',
        cex.axis =0.5,
        cex.lab = 0.5
      )
      if (input$var3 != 'Tous les marchés') {
        nb_esp_annee_lieu_brute <- nb_esp_annee_lieu_brute[which(nb_esp_annee_lieu_brute$marche == input$var3),]
        nb_esp_annee_lieu_brute <- nb_esp_annee_lieu_brute[,-3]
        
        py_nb_esp_annee_lieu_brute <- plot_ly(type = 'bar') %>%
          layout(yaxis = yaxis, margin = m,xaxis = xaxis, barmode = "stack")
        for (i in dates) {
          py_nb_esp_annee_lieu_brute <- add_trace(py_nb_esp_annee_lieu_brute,x = ~unique(nb_esp_annee_lieu_brute[,"especes"]),y = nb_esp_annee_lieu_brute[which(nb_esp_annee_lieu_brute[,"annee"] == i),"Freq"], name = i)
        }
        py_nb_esp_annee_lieu_brute
      } else {
        py_brute <- plot_ly(type = 'bar') %>%
          layout(yaxis = yaxis, margin = m,xaxis = xaxis, barmode = "stack")
        for (i in dates) {
          py_brute <- add_trace(py_brute,x = ~unique(tolower(b_anl_brute[,"especes"])), y = b_anl_brute[which(b_anl_brute[,"annee"] == i),"Freq"], name = i)
        }
        py_brute
      }
    }
    ########################
  })
  ###############
  ######Tableau######
  output$DT <- DT::renderDataTable({
    #####Checking checkbox#####
    if (input$checkbox) {
      #####Checking Marché#####
      if (input$var3 != 'Tous les marchés') {
        data_p <- data_p[which(data_p$LIEU == input$var3),]
      }
      #######################
      #####Récupération fréquence/visites######
      annee_p<-substring(data_p[,"DATE"],7,10) 
      jours_visite_annee_p<-table(substring(unique(data_p[,"DATE"]),7,10))
      b_an_p <- data.frame(table(data_p$ESPECE.OBSERVEE,annee_p))
      for (i in names(jours_visite_annee_p)){
        b_an_p[which(as.character(b_an_p[,"annee_p"]) == i), "Freq"] <- round(b_an_p[which(as.character(b_an_p[,"annee_p"]) == i), "Freq"]/jours_visite_annee_p[i],2)
      }
      ######################################
      ######Création table#####
      names(b_an_p)<-c("especes","annee","Freq")
      b_an_p<-cast(b_an_p,formula = especes~annee,value.var = "Freq")
      DT::datatable(b_an_p)
      #######################
    }
    else{
      #####Checking marché#####
      if (input$var3 != 'Tous les marchés') {
        data_p <- data_p[which(data_p$LIEU == input$var3),]
      }
      #######################
      ######Création table######
      annee_p<-substring(data_p[,"DATE"],7,10)
      b_an_p <- data.frame(table(data_p$ESPECE.OBSERVEE,annee_p))
      names(b_an_p)<-c("especes","annee","Freq")
      b_an_p<-cast(b_an_p,formula = especes~annee,value.var = "Freq")
      DT::datatable(b_an_p)
      #######################
    }
    ######################
  })
  ###################
}

