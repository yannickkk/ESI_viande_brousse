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
library(shinyjs)
library(shinyauthr)
library(googledrive)
library(leaflet)
library(leaflet.minicharts)
library(geojsonio)
library(units)
library(maptools)
#library(mapview)
library(leafsync)
library(sodium)

source("user_base.R")
source("access_drive.R")


######Initialisation######
###################Si les fichiers protocoles et data_final n existent pas sous la racine de l'app ils sont telecharges depuis le drive
##############ATTENTION le fichier global.R ne va être lu qu'au lancement de l'appli
# if (!exists("protocole.html")) {drive_download(as_id(drive_find(pattern = "protocole.html")$id), overwrite = TRUE)}
# if (!exists("data_final.csv")) {drive_download(as_id(drive_find(pattern = "data_final.csv")$id), overwrite = TRUE)}
# if (!exists("district.csv")) {drive_download(as_id(drive_find(pattern = "district.csv")$id), overwrite = TRUE)}

data$QUANTITE <- replace(data$QUANTITE,is.na(data$QUANTITE),1)
##Transformation de NA en 1 pour compenser le manque d'informations


distircts_geo<- geojsonio::geojson_read("districts_v2.geojson", what = "sp")
protected_geo<-geojsonio::geojson_read("protected_area.geojson", what = "sp")
cent_dist_geo<-geojsonio::geojson_read("districts_centroids_v2.geojson", what = "sp")
#setwd("C:/Users/Utilisateur/Desktop/Stage/Outputs")
#data<- read.csv2("data_final.csv", header = TRUE, encoding = "ANVI")
district <- read.csv2("district.csv",header=TRUE, encoding ="ANVI")
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
  title = 'species',
  cex.axis =0.5,
  cex.lab = 0.5
)

########################################################

