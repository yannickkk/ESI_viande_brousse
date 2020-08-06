##################################################################
######Variables globales de l'application (lu une seule fois)######
##################################################################
#####chargement des libraries#####

library(plotly)
library(reshape)
library(dplyr)
library(tidyverse)
library(tidyr)
library(shiny)
library(markdown)
library(shinyjs)
library(shinyauthr)
library(leaflet)
library(leaflet.minicharts)
library(geojsonio)
library(units)
library(maptools)
library(leafsync)
library(sodium)
library(rdrop2)
library(data.table)


########lien vers les scripts de telechargement des donnees et d'autorisation d'acces
source("scripts/user_base.R")
source("scripts/access_drive.R")

dat<-as.data.frame(apply(dat,2,as.factor)) ###typage des colonnes en facteur et retour au dataframe
########si des quantites manquent on les remplace par 1 car lespece est presente
dat$QUANTITE <- replace(dat$QUANTITE,is.na(dat$QUANTITE),1)

#####################################################################################################################COMMENTER POURQUOI ON FAIT CELA
data_p <- dat

######CrC)ation de l'axe x marge des graphiques######
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

