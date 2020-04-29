##################################
######Traitement des données######
##################################
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
library(markdown)

######################

######Initialisation######

setwd("C:/Users/Utilisateur/Desktop/Stage/DonneesTravaillees")
data<- read.csv2("data_nettoye.csv", header = TRUE, encoding = "ANVI")
data_p <- data

################################################

######Création de l'axe x marge des graphiques######
m <- list(
  l = 50,
  r = 50,
  b = 200,
  t = 10,
  pad = 4
)

xaxis <- list(
  title = 'espèces',
  cex.axis =0.5,
  cex.lab = 0.5
)

########################################################

