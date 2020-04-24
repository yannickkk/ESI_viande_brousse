##################################
######Traitement des données######
##################################

######Initialisation######

setwd("C:/Users/Utilisateur/Desktop/Stage/DonneesTravaillees")
data<- read.csv2("data_nettoye.csv", header = TRUE, encoding = "UTF-8")
data_p <- data
#Récupère annee, mois et jour séparés
annee<-substring(data[,"DATE"],7,10) 
mois<-substring(data[,"DATE"],4,5)
jour<-substring(data[,"DATE"],1,2)

##################################

######Fréquence de chaque espèce par année######

#Crée un dataframe espece observee par année
b_an <- table(data$ESPECE.OBSERVEE,annee)
b_an <- data.frame(b_an)

b_an_brute <- b_an
#Récupère le nombre de jour par année
jours_visite_annee<-table(substring(unique(data[,"DATE"]),7,10))
#Pour chaque espèce, divise la Frequence par le nombre de jour visité dans l'année (Permet d'avoir une fréquence plus réaliste)
for (i in names(jours_visite_annee))
{
  b_an[which(as.character(b_an[,"annee"]) == i), "Freq"] <- round(b_an[which(as.character(b_an[,"annee"]) == i), "Freq"]/jours_visite_annee[i],2)
}

names(b_an)<-c("especes","annee","Freq")

b_anl <- b_an
#Permet de visualiser sous forme de graphique b_an
# x = chaque espece (unique), y = fréquence en fonction de l'année
#add_trace rajoute les annéeq 1 par 1

names(b_an_brute) <- c("especes","annee","Freq")
b_anl_brute <- b_an_brute
b_an_brute <-cast(b_an_brute,formula = especes~annee,value.var = "Freq")
p_brute <-DT::datatable(b_an_brute)

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


######Nombre de chaque espèce par année par marché######

nb_esp_annee_lieu <- data.frame(table(data$ESPECE.OBSERVEE,annee,data$LIEU))
nb_esp_annee_lieu_brute <- nb_esp_annee_lieu
#Pour chaque espèce, divise la Frequence par le nombre de jour visité dans l'année (Permet d'avoir une fréquence plus réaliste)
for (i in names(jours_visite_annee))
{
  nb_esp_annee_lieu[which(as.character(nb_esp_annee_lieu[,"annee"]) == i), "Freq"] <- round(nb_esp_annee_lieu[which(as.character(nb_esp_annee_lieu[,"annee"]) == i), "Freq"]/jours_visite_annee[i],2)
}

names(nb_esp_annee_lieu) <- c('especes','annee','marche','Freq')
names(nb_esp_annee_lieu_brute) <- c('especes','annee','marche','Freq')

########################################################
