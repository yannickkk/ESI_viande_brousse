#Mise en place de l'espace de travail
setwd("C:/Users/Utilisateur/Desktop/Stage/DonneesTravaillees")
setwd("C:/Users/ychaval/Documents/Collegues/BLASE_Vincent/DonneesTravaillees")
library("red")
library("RJSONIO")
library("ritis")
library("plotly")
library("lubridate")
library("reshape")
library("dplyr")
library("plotly")
#library("tidyverse")
#library("tidyr")
data<- read.csv2("data_total.csv", header = TRUE, encoding = "UTF-8")

#Traitement d'une erreur de chargement sur les "é"
data$LIEU <- str_replace(data$LIEU,"<e9>","e")
data$LIEU <- as.factor(data$LIEU)
data$FORMAT.VENTE <- str_replace(data$FORMAT.VENTE,"<e9>","e")
data$FORMAT.VENTE <- as.factor(data$FORMAT.VENTE)
data$ESPECE.OBSERVEE <- str_replace(data$ESPECE.OBSERVEE,"<e9>","e")
data$ESPECE.OBSERVEE <- as.factor(data$ESPECE.OBSERVEE)
data$STATUT.DE.PROTECTION <- str_replace(data$STATUT.DE.PROTECTION,"<e9>","e")
data$STATUT.DE.PROTECTION <- as.factor(data$STATUT.DE.PROTECTION)
data$NOM.LATIN <- str_replace(data$NOM.LATIN,"<e9>","e")
data$NOM.LATIN <- as.factor(data$NOM.LATIN)
data$STATUT.CONGO <- str_replace(data$STATUT.CONGO,"<e9>","e")
data$STATUT.CONGO <- as.factor(data$STATUT.CONGO)
data$STATUT.UICN <- str_replace(data$STATUT.UICN,"<e9>","e")
data$STATUT.UICN <- as.factor(data$STATUT.UICN)
data$ORIGINE.1 <- str_replace(data$ORIGINE.1,"<e9>","e")
data$ORIGINE.1 <- as.factor(data$ORIGINE.1)
data$ORIGINE.2 <- str_replace(data$ORIGINE.2,"<e9>","e")
data$ORIGINE.2 <- as.factor(data$ORIGINE.2)
data$ORIGINE.3 <- str_replace(data$ORIGINE.3,"<e9>","e")
data$ORIGINE.3 <- as.factor(data$ORIGINE.3)
data$ORIGINE.4 <- str_replace(data$ORIGINE.4,"<e9>","e")
data$ORIGINE.4 <- as.factor(data$ORIGINE.4)
data$DISTRICT.1 <- str_replace(data$DISTRICT.1,"<e9>","e")
data$DISTRICT.1 <- as.factor(data$DISTRICT.1)
data$DISTRICT.2 <- str_replace(data$DISTRICT.2,"<e9>","e")
data$DISTRICT.2 <- as.factor(data$DISTRICT.2)
data$DISTRICT.3 <- str_replace(data$DISTRICT.3,"<e9>","e")
data$DISTRICT.3 <- as.factor(data$DISTRICT.3)
data$DISTRICT.5 <- str_replace(data$DISTRICT.5,"<e9>","e")
data$DISTRICT.5 <- as.factor(data$DISTRICT.5)
data$DISTRICTS <- str_replace(data$DISTRICTS,"<e9>","e")
data$DISTRICTS <- as.factor(data$DISTRICTS)
data$ENTIER...MORCEAUX <- str_replace(data$ENTIER...MORCEAUX,"<e9>","e")
data$ENTIER...MORCEAUX <- as.factor(data$ENTIER...MORCEAUX)
data$REGION <- str_replace(data$REGION,"<e9>","e")
data$REGION <- as.factor(data$REGION)
data$ville.ou.village <- str_replace(data$ville.ou.village,"<e9>","e")
data$ville.ou.village <- as.factor(data$ville.ou.village)
data$ACHETEUR <- str_replace(data$ACHETEUR,"<e9>","e")
data$ACHETEUR <- as.factor(data$ACHETEUR)
data$REMARQUE <- str_replace(data$REMARQUE,"<e9>","e")
data$REMARQUE <- as.factor(data$REMARQUE)
data$FUMES...FRAIS <- str_replace(data$FUMES...FRAIS,"<e9>","e")
data$FUMES...FRAIS <- as.factor(data$FUMES...FRAIS)

data_2 <- data

summary(data_2)
#Suppression dans l'ordre de Heure, Taille.Du.Marche, Remarque, 
#ville.ou.village,Acheteur,Region car trop peu de données
data_2 <- data_2[,-2]
data_2 <- data_2[,-28]
data_2 <- data_2[,-32]
data_2 <- data_2[,-30]
data_2 <- data_2[,-30]
data_2 <- data_2[,-29]

#Sauvegarde
data_3 <- data_2

#Regroupement de Origine et District
data_3$ORIGINE <- NA
for (i in 1:nrow(data_3))
{
  f <- data_3[i,c(10,12,14,16)]
  f <- f[f != 0]
  f <- na.omit(f)
  data_3$ORIGINE[i] <- paste(f,collapse = "")
}
data_3$ORIGINE <- as.factor(data_3$ORIGINE)
data_3 <- data_3[,-c(10,12,14,16)]

data_3$DISTRICT <- NA
for (i in 1:nrow(data_3))
{
  f <- data_3[i,c(10,11,12,13,14)]
  f <- f[f != 0]
  f <- na.omit(f)
  data_3$DISTRICT[i] <- paste(f,collapse = "")
}
data_3$DISTRICT <- as.factor(data_3$DISTRICT)
data_3 <- data_3[,-c(10,11,12,13,14)]
summary(data_3)

#Sauvegarde
data_4 <- data_3

#Regroupement de prix_part et prix_entier
data_4$PRIX_PART <- NA
for (i in 1:nrow(data_4))
{
  f <- data_4[i,c(14,18,21)]
  f <- f[f != 0]
  f <- na.omit(f)
  data_4$PRIX_PART[i] <- paste(f, collapse = "")
}
data_4 <- data_4[,-c(14,18,21)]
data_4$PRIX_PART <- as.numeric(data_4$PRIX_PART)
summary(data_4)


data_4$PRIX_ENTIER <- NA
for (i in 1:nrow(data_4))
{
  f <- data_4[i,c(13,16,18)]
  f <- f[f != 0]
  f <- na.omit(f)
  data_4$PRIX_ENTIER[i] <- paste(f, collapse = "")
}
data_4 <- data_4[,-c(13,16,18)]
data_4$PRIX_ENTIER <- as.numeric(data_4$PRIX_ENTIER)
summary(data_4$PRIX_ENTIER)

#Sauvegarde
data_5 <- data_4

#Correction sur des doublons sur les Districts
data_5$DISTRICT <- str_replace(data_5$DISTRICT,"Madingo-KayesMadingo-Kayes","Madingo-Kayes")
data_5$DISTRICT <- str_replace(data_5$DISTRICT,"KakamoekaKakamoeka","Kakamoeka")
data_5$DISTRICT <- str_replace(data_5$DISTRICT,"ConkouatiConkouati","Conkouati")
data_5$DISTRICT <- str_replace(data_5$DISTRICT,"NzassiNzassi","Nzassi")
data_5$DISTRICT <- str_replace(data_5$DISTRICT,"MvoutiMvouti","Mvouti")
data_5$DISTRICT <- str_replace(data_5$DISTRICT,"HindaHinda","Hinda")
data_5$DISTRICT <- as.factor(data_5$DISTRICT)

#Correction sur les Statuts, 0 devient NA, regroupement de protegee et protege
data_5$STATUT.CONGO <- replace(data_5$STATUT.CONGO,data_5$STATUT.CONGO=="0",NA)
data_5$STATUT.UICN <- replace(data_5$STATUT.UICN,data_5$STATUT.UICN=="0",NA)
data_5$STATUT.DE.PROTECTION <- str_replace(data_5$STATUT.DE.PROTECTION,"protegee","protege")
data_5$STATUT.DE.PROTECTION <- str_replace(data_5$STATUT.DE.PROTECTION,"Protegee","Protege")
data_5$STATUT.DE.PROTECTION <- str_replace(data_5$STATUT.DE.PROTECTION,"non","Non")
data_5$STATUT.DE.PROTECTION <- replace(data_5$STATUT.DE.PROTECTION,data_5$STATUT.DE.PROTECTION=="Non protege","Non protege au Congo")
data_5$STATUT.DE.PROTECTION <- as.factor(data_5$STATUT.DE.PROTECTION)
summary(data_5)

data_6 <- data_5
#Suppression de FORMAT.VENTE
data_6 <- data_6[,-14]


#Sauvegarde
data_7 <- data_6

#Correction Statut de protection
data_7$STATUT.DE.PROTECTION <- replace(data_7$STATUT.DE.PROTECTION,data_7$STATUT.DE.PROTECTION == "P",NA)
data_7$STATUT.DE.PROTECTION <- replace(data_7$STATUT.DE.PROTECTION,data_7$STATUT.DE.PROTECTION == "Non protege ","Non protege au Congo")

#Correction Nom espèces

data_7$ESPECE.OBSERVEE <- replace(data_7$ESPECE.OBSERVEE,data_7$ESPECE.OBSERVEE == "PAGOLAIN GEANT","PANGOLIN GEANT")
levels(data_7$ESPECE.OBSERVEE)

#Correction nom districte
levels(data_7$DISTRICT) <- c(levels(data_7$DISTRICT),"Multiple")
levels(data_7$DISTRICT) <- c(levels(data_7$DISTRICT),"Pointe-Noire")
levels(data_7$DISTRICT) <- c(levels(data_7$DISTRICT),"Zassi")
levels(data_7$DISTRICT) <- c(levels(data_7$DISTRICT),"Dimonika")
levels(data_7$DISTRICT) <- c(levels(data_7$DISTRICT),"Bas-Kouilou")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "AutreAutre","Autre")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "AutreKakamoekaAutre Kakamoeka","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "","Autre")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "Bas-Kouilou ","Bas-Kouilou")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "Bas-KouilouBas-Kouilou","Bas-Kouilou")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "CabindaCabinda","Cabinda")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "ConkouatiMadingo-KayesConkouati Madingo-Kayes","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "DimonikaDimonika","Dimonika")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "hinda","Hinda")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "HindaKakamoekaHinda Kakamoeka","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "HindaMvoutiHinda Mvouti","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "Kakamoeka Kakamoeka","Kakamoeka")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "KakamoekaHinda","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "KakamoekaHindaKakamoeka Hinda","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "KakamoekaMadingo-KayesKakamoeka Madingo-Kayes","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "KakamoekaMvoutiKakamoeka Mvouti","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "KakamoekaNzassiKakamoeka Nzassi","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "Madingo-KayesConkouatiMadingo-Kayes Conkouati","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "Madingo-KayesHindaMadingo-Kayes Hinda","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "Madingo-KayesKakamoekaMadingo-Kayes Kakamoeka","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "Madingo-KayesMvoutiMadingo-Kayes Mvouti","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "Madingo-KayesPointe-NoireKakamoekaMadingo-Kayes Pointe-Noire","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "Mvouti Mvouti","Mvouti")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "Mvouti0 Mvouti","Mvouti")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "MvoutiKakamoekaMvouti Kakamoeka","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "MvoutiMadingo-KayesMvouti Madingo-Kayes","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "NzassiHindaNzassi Hinda","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "NzassiPointe-NoireNzassi Pointe-Noire","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "Pointe-NoireMadingo-KayesPointe-Noire Madingo-Kayes","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "Pointe-NoireNzassiPointe-Noire Nzassi","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "Pointe-NoirePointe-Noire","Pointe-Noire")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "Tchiamba Nzassi","Multiple")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "TransfrontalierTransfrontalier","Transfrontalier")
data_7$DISTRICT <- replace(data_7$DISTRICT,data_7$DISTRICT == "ZassiZassi","Zassi")

#Sauvegarde
data_8 <- data_7

#Suppression origine
data_8 <- data_8[,-15]
#Correction de Entier...Morceaux
data_8$ENTIER...MORCEAUX <- replace(data_8$ENTIER...MORCEAUX,data_8$ENTIER...MORCEAUX =="morceaux","Morceaux")
data_8$ENTIER...MORCEAUX <- replace(data_8$ENTIER...MORCEAUX,data_8$ENTIER...MORCEAUX =="",NA)

#Correction de FUMES...FRAIS
data_8$FUMES...FRAIS<- replace(data_8$FUMES...FRAIS,data_8$FUMES...FRAIS =="",NA)

#Correction de lieu
data_8$LIEU<- replace(data_8$LIEU,data_8$LIEU =="MARCHE CENTRAL","Marche Central")
data_8$LIEU<- replace(data_8$LIEU,data_8$LIEU =="MARCHE FAUBOURG","Marche Faubourg")
data_8$LIEU<- replace(data_8$LIEU,data_8$LIEU =="MARCHE KOUILOU","Marche Nkouikou")
data_8$LIEU<- replace(data_8$LIEU,data_8$LIEU =="MARCHE LIBERTE","Marche Liberte")
data_8$LIEU<- replace(data_8$LIEU,data_8$LIEU =="MARCHE MAYAKA","Marche Mayaka")
data_8$LIEU<- replace(data_8$LIEU,data_8$LIEU =="MARCHE TIE-TIE","Marche Tie-Tie")
data_8$LIEU<- replace(data_8$LIEU,data_8$LIEU =="MARCHE MVOUMVOU","Marche Mvoumvou")
data_8$LIEU<- replace(data_8$LIEU,data_8$LIEU =="Marche kouikou","Marche Nkouikou")


#Correction de nom Latin
data_8$NOM.LATIN<- replace(data_8$NOM.LATIN,data_8$NOM.LATIN =="Pelusios casteneus","Pelusios castaneus")
data_8$TAILLE.ENTIER <- replace(data_8$TAILLE.ENTIER,data_8$TAILLE.ENTIER=="",NA)
data_8$TAILLE.PART <- replace(data_8$TAILLE.PART,data_8$TAILLE.PART=="",NA)
summary(data_8)

data_final <- data_8
summary(data_final)
write.csv2(data_final,"data_nettoye.csv", row.names = FALSE)





