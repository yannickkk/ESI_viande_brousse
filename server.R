
# Define server logic 
server <- function(input, output, session) {
  
  observe({
    rank <- input$rank
    if (rank == "species") {
      data$species <- as.factor(paste(data$ESPECE.OBSERVEE,data$species,sep =" - "))
    }
    updateSelectInput(session,"taxa",label = paste('Select ',rank),choices = c("whole taxa",levels(data[,rank])),selected ="whole taxa")
  })
  
  ######Plotly#####
  output$plotly <- renderPlotly({
    shiny::validate(
      need(input$taxa != "", "Please select a taxa")
    )
    shiny::validate(
      need(input$var3 != "", "Please select a market")
    )
    
    #####Checking taxa####
    taxa <- input$taxa
    if ("whole taxa"%in%taxa & length(taxa) > 1) {
      observe({
        updateSelectInput(session,"taxa",label = paste('Select ',rank),choices = c("whole taxa",levels(data[,rank])),selected = taxa[2])
      })
    }
    
    rank <- input$rank
    if (rank == "species") {
      data$species <- as.factor(paste(data$ESPECE.OBSERVEE,data$species,sep =" - "))
    }
    data_cut_taxa <- data
    if ("whole taxa"%in%taxa){
      data_cut_taxa <- data
    } else {
      data_cut_taxa <- data[which(data[,rank] == taxa[1]),]
      if (length(taxa)>1){
        for (i in 2:length(taxa)){
          data_cut_taxa <- rbind(data_cut_taxa,data[which(data[,rank] == taxa[i]),])
        }
      }
      data_cut_taxa$ESPECE.OBSERVEE <- factor(data_cut_taxa$ESPECE.OBSERVEE,exclude=NULL)
    }
    ######################
    #####Checking marche#####
    marche <- input$var3
    if ("whole markets"%in%marche & length(marche) > 1) {
      observe({
        updateSelectInput(session,"var3",label= "Choice market (single or multiple)",choices = c("whole markets", levels(data$LIEU)),selected = marche[2])
      })
    }
    data_cut <- data_cut_taxa
    if ("whole markets"%in%marche){
      data_cut <- data_cut_taxa
    } else {
      data_cut <- data_cut_taxa[which(data_cut_taxa[,"LIEU"] == marche[1]),]
      if (length(marche) > 1){
        for (i in 2:length(marche)){
          data_cut <- rbind(data_cut,data_cut_taxa[which(data_cut_taxa[,"LIEU"] == marche[i]),])
        }
      }
      data_cut$LIEU <- factor(data_cut$LIEU,exclude=NULL)
    }
    ########################
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
    annee_cut<-substring(data_cut[,"DATE"],7,10)
    b_an <- data.frame(table(tolower(data_cut$ESPECE.OBSERVEE),annee_cut))
    names(b_an) <- c('especes','annee','Freq')
    #####Checking checkbox#####
    if (input$checkbox) {
      
      jours_visite_annee<-table(substring(unique(data_cut[,"DATE"]),7,10))
      for (i in names(jours_visite_annee)) {
        b_an[which(as.character(b_an[,"annee"]) == i), "Freq"] <- round(b_an[which(as.character(b_an[,"annee"]) == i), "Freq"]/jours_visite_annee[i],2)
      }
      #####Checking log#####
      if(input$checkboxlog){
        for (i in 1:nrow(b_an)) {
          b_an[i,3] <- log(1+b_an[i,3])
        }
        yaxis <- list(
          title = 'log(meat occurrences/nber of visits)',
          cex.axis =0.5,
          cex.lab = 0.5
        )
      } else {
        yaxis <- list(
          title = 'meat occurrences/number of visits',
          cex.axis =0.5,
          cex.lab = 0.5
        )
      }
      ####################
      ####Création du graphique####
      py_b_an <- plot_ly(type = 'bar') %>%
      layout(yaxis = yaxis, margin = m,xaxis = xaxis, barmode = "stack")
      for (i in dates) {
        if (i%in%unique(annee_cut)){
          py_b_an<- add_trace(py_b_an,x = ~unique(b_an[,"especes"]),y = b_an[which(b_an[,"annee"] == i),"Freq"], name = i)
        }
      }
      py_b_an
      ############################
    } else {
      #####Checking log#####
      if(input$checkboxlog){
        for (i in 1:nrow(b_an)){
          b_an[i,3] <- log(1+b_an[i,3])
          
        }
        yaxis <- list(
          title = 'log(meat occurrences)',
          cex.axis =0.5,
          cex.lab = 0.5
        )
      } else {
        yaxis <- list(
          title = 'meat occurrences',
          cex.axis =0.5,
          cex.lab = 0.5
        )
      }
      ####################
      ####Création du graphique####
      py_b_an <- plot_ly(type = 'bar') %>%
        layout(yaxis = yaxis, margin = m,xaxis = xaxis, barmode = "stack")
      for (i in dates) {
        if (i%in%unique(annee_cut)){
          py_b_an<- add_trace(py_b_an,x = ~unique(b_an[,"especes"]),y = b_an[which(b_an[,"annee"] == i),"Freq"], name = i)
        }
      }
      py_b_an
      ############################
    }
  })
    
  ######Tableau######
  output$DT <- DT::renderDataTable({
    #####Checking checkbox#####
    if (input$checkbox) {
      #####Checking Marché#####
      if (input$var3 != 'whole markets') {
        data_p <- data_p[which(data_p$LIEU == input$var3),]
      }
      #######################
      #####Récupération fréquence/visites######
      annee_p<-substring(data_p[,"DATE"],7,10) 
      jours_visite_annee_p<-table(substring(unique(data_p[,"DATE"]),7,10))
      b_an_p <- data.frame(table(paste(data_p$ESPECE.OBSERVEE,data_p$species,sep=', '),annee_p))
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
      if (input$var3 != 'whole markets') {
        data_p <- data_p[which(data_p$LIEU == input$var3),]
      }
      #######################
      ######Création table######
      annee_p<-substring(data_p[,"DATE"],7,10)
      b_an_p <- data.frame(table(paste(data_p$ESPECE.OBSERVEE,data_p$species,sep=', '),annee_p))
      names(b_an_p)<-c("especes","annee","Freq")
      b_an_p<-cast(b_an_p,formula = especes~annee,value.var = "Freq")
      DT::datatable(b_an_p)
      #######################
    }
    ######################
  })
  
  #####iframe######
  
  
  observeEvent(input$species,{
    shiny::validate(
      need(input$species != "", "Please select a specie")
    )
    name <- as.character(input$species)
    specie <- str_split(name, " - ", simplify = TRUE)[1,2]
    link <<- paste0("https://species.wikimedia.org/wiki/",specie,"")
    output$frame <- renderUI({
      tags$iframe(src=link, height=1200, width=1600,frameborder = "no")
    })
    url <- a(tags$h3("More informations about this species"), href = as.character(unique(data[which(data[,"species"]== specie),"URL"])),target ="_blank")
    output$`More informations` <- renderUI({
      tagList(url)
    })
  })
  ##############
  
  #####Login#####
  logout_init <- callModule(shinyauthr::logout, 
                            id = "logout", 
                            active = reactive(credentials()$user_auth))
  
  credentials <- callModule(shinyauthr::login, 
                            id = "login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password,
                            log_out = reactive(logout_init()))
  
  #############
  
  #####Import#####
  
  output$`Import data` <- renderUI({
    req(credentials()$user_auth)
    h3("Import data")
  })
  
  output$import_data <- renderUI({
    req(credentials()$user_auth)
    actionButton("button1", label = "Click for import data")
  })
  
  observeEvent(input$button1,{
    drive_download(as_id(drive_find(pattern = "data_final.csv")$id), overwrite = TRUE)
    data<-read.csv2("data_final.csv", header = TRUE, encoding = "ANVI")
  })
  
  output$`Import protocol` <- renderUI({
    req(credentials()$user_auth)
    h3("Import protocol")
  })
    
  output$import_protocol <- renderUI({
    req(credentials()$user_auth)
    actionButton("button2", label = "Click for import protocol")
  })
  
  observeEvent(input$button2,{
    drive_download(as_id(drive_find(pattern = "protocole.html")$id), overwrite = TRUE)
  })
  
  output$`Import district` <- renderUI({
    req(credentials()$user_auth)
    h3("Import district")
  })
  
  output$import_district <- renderUI({
    req(credentials()$user_auth)
    actionButton("button3", label = "Click for import district")
  })
  
  observeEvent(input$button3,{
    drive_download(as_id(drive_find(pattern = "district.csv")$id), overwrite = TRUE)
  })
  ##############
  
  
  #####Spatial viewer######
  
  observe({
    rank2 <- input$rank2
    if (rank2 == "species") {
      data$species <- as.factor(paste(data$ESPECE.OBSERVEE,data$species,sep =" - "))
    }
    updateSelectInput(session,"taxa2",label = paste('Select ',rank2),choices = c("whole taxa",levels(data[,rank2])),selected ="whole taxa")
  })
  
  
  output$map <- renderLeaflet({
    shiny::validate(
      need(input$taxa2 != "", "Please select a taxa")
    )
    shiny::validate(
      need(input$market2 != "", "Please select a market")
    )
    
    #####Checking taxa####
    taxa2 <- input$taxa2
    if ("whole taxa"%in%taxa2 & length(taxa2) > 1) {
    observe({
       updateSelectInput(session,"taxa2",label = paste('Select ',rank2),choices = c("whole taxa",levels(data[,rank2])),selected = taxa2[2])
      })
    }
    
    rank2 <- input$rank2
    if (rank2 == "species") {
      data$species <- as.factor(paste(data$ESPECE.OBSERVEE,data$species,sep =" - "))
    }
    data_cut_taxa2 <- data
    if ("whole taxa"%in%taxa2){
      data_cut_taxa2 <- data
    } else {
      data_cut_taxa2 <- data[which(data[,rank2] == taxa2[1]),]
      if (length(taxa2)>1){
        for (i in 2:length(taxa2)){
          data_cut_taxa2 <- rbind(data_cut_taxa2,data[which(data[,rank2] == taxa2[i]),])
        }
      }
      data_cut_taxa2$ESPECE.OBSERVEE <- factor(data_cut_taxa2$ESPECE.OBSERVEE,exclude=NULL)
    }
    ######################
    
    #####Checking marche#####
    marche2 <- input$market2
    if ("whole markets"%in%marche2 & length(marche2) > 1) {
      observe({
        updateSelectInput(session,"market2",label= "Choice market (single or multiple)",choices = c("whole markets", levels(data$LIEU)),selected = marche2[2])
      })
    }
    data_cut2 <- data_cut_taxa2
    if ("whole markets"%in%marche2){
      data_cut2 <- data_cut_taxa2
    } else {
      data_cut2 <- data_cut_taxa2[which(data_cut_taxa2[,"LIEU"] == marche2[1]),]
      if (length(marche2) > 1){
        for (i in 2:length(marche2)){
          data_cut2 <- rbind(data_cut2,data_cut_taxa2[which(data_cut_taxa2[,"LIEU"] == marche2[i]),])
        }
      }
      data_cut2$LIEU <- factor(data_cut2$LIEU,exclude=NULL)
    }
    ########################
    
    #####Checking statut#####
    statut <- input$statut
    if ("IUCN statut"%in%statut){
      df <- data.frame(table(data_cut2$DISTRICT,data_cut2$iucn_cat))
    }else{
      df <- data.frame(table(data_cut2$DISTRICT,data_cut2$STATUT.CONGO))
    }
    names(df) <- c("District","Statut","Freq")
    
    #####Création du DataFrame####
    df$DISTRICT <- NA
    df$lng <- NA
    df$lat <- NA
    for (i in unique(df[,c("District")])){
      if(length(district[which(district[,"sous_district"]== i),"district"] != 0)){
        df[which(df[,c("District")]==i),"DISTRICT"] <- strrep(district[which(district[,"sous_district"]==i),"district"],1)
        df[which(df[,c("District")]==i),"lng"] <- rep(district[which(district[,"sous_district"]==i),"lng"],length(df[which(df[,c("District")] == i),"lng"]))
        df[which(df[,c("District")]==i),"lat"] <- rep(district[which(district[,"sous_district"]==i),"lat"],length(df[which(df[,c("District")] == i),"lat"]))
      }
    } 
    names(df) <- c("sous_district","Statut","Freq","District","lng","lat")
    
    #####################
    ####Création des données pour chaque district#####
    ######Kakamoéka#####
    df_Kakamoéka <- df[which(df[,c("District")]=="Kakamoéka"), c("Statut","Freq","District","lng","lat")]
    df_Kakamoéka %>%
      group_by(Statut) %>%
      mutate(Freq = sum(Freq))
    df_Kakamoéka <- df_Kakamoéka[!duplicated(df_Kakamoéka[,c('Statut')]),]
    
    #####################
    #####Madingo-Kayes#####
    df_Madingo_Kayes <- df[which(df[,c("District")]=="Madingo-Kayes"), c("Statut","Freq","District","lng","lat")]
    df_Madingo_Kayes %>%
      group_by(Statut) %>%
      mutate(Freq = sum(Freq))
    df_Madingo_Kayes <- df_Madingo_Kayes[!duplicated(df_Madingo_Kayes[,c('Statut')]),]
    ######Cabinda########
    df_Cabinda <- df[which(df[,c("District")]=="Cabinda"), c("Statut","Freq","District","lng","lat")]
    df_Cabinda %>%
      group_by(Statut) %>%
      mutate(Freq = sum(Freq))
    df_Cabinda <- df_Cabinda[!duplicated(df_Cabinda[,c('Statut')]),]
    ####################
    ######Mvouti########
    df_Mvouti <- df[which(df[,c("District")]=="Mvouti"), c("Statut","Freq","District","lng","lat")]
    df_Mvouti %>%
      group_by(Statut) %>%
      mutate(Freq = sum(Freq))
    df_Mvouti <- df_Mvouti[!duplicated(df_Mvouti[,c('Statut')]),]
    ###################
    #####Djambala######
    df_Djambala <- df[which(df[,c("District")]=="Djambala"), c("Statut","Freq","District","lng","lat")]
    df_Djambala %>%
      group_by(Statut) %>%
      mutate(Freq = sum(Freq))
    df_Djambala <- df_Djambala[!duplicated(df_Djambala[,c('Statut')]),]
    ##################
    ######Louvakou######
    df_Louvakou<- df[which(df[,c("District")]=="Louvakou (Loubomo)"), c("Statut","Freq","District","lng","lat")]
    df_Louvakou%>%
      group_by(Statut) %>%
      mutate(Freq = sum(Freq))
    df_Louvakou<- df_Louvakou[!duplicated(df_Louvakou[,c('Statut')]),]
    ###################
    #####Loandjili#####
    df_Loandjili <- df[which(df[,c("District")]=="Loandjili (Pointe Noire)"), c("Statut","Freq","District","lng","lat")]
    df_Loandjili %>%
      group_by(Statut) %>%
      mutate(Freq = sum(Freq))
    df_Loandjili <- df_Loandjili[!duplicated(df_Loandjili[,c('Statut')]),]
    ###################
    #####Mossendjo#####
    df_Mossendjo <- df[which(df[,c("District")]=="Mossendjo"), c("Statut","Freq","District","lng","lat")]
    df_Mossendjo %>%
      group_by(Statut) %>%
      mutate(Freq = sum(Freq))
    df_Mossendjo <- df_Mossendjo[!duplicated(df_Mossendjo[,c('Statut')]),]
    #################
    leaflet(mymap) %>%
      addTiles()  %>%# Add default OpenStreetMap map tiles
      setView(lng=12.330, lat=-4.029,zoom = 8) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.6, fillOpacity = 0.3,color = rainbow(6)) %>%
      addMinicharts(df_Kakamoéka$lng[1],df_Kakamoéka$lat[1],type = "pie", chartdata = df_Kakamoéka$Freq,width = 75, showLabels = TRUE, colorPalette=rainbow(length(levels(df$Statut))),opacity =0.7,labelMinSize = 1,labelMaxSize = 16) %>%
      addMinicharts(df_Madingo_Kayes$lng[1],df_Madingo_Kayes$lat[1],type = "pie", chartdata = df_Madingo_Kayes$Freq,width = 75, showLabels = TRUE, colorPalette=rainbow(length(levels(df$Statut))),opacity =0.7,labelMinSize = 1,labelMaxSize = 16) %>%
      addMinicharts(df_Cabinda$lng[1],df_Cabinda$lat[1],type = "pie", chartdata = df_Cabinda$Freq,width = 75, showLabels = TRUE, colorPalette=rainbow(length(levels(df$Statut))),opacity =0.7,labelMinSize = 1,labelMaxSize = 16) %>%
      addMinicharts(df_Mvouti$lng[1],df_Mvouti$lat[1],type = "pie", chartdata = df_Mvouti$Freq,width = 75, showLabels = TRUE, colorPalette=rainbow(length(levels(df$Statut))),opacity =0.7,labelMinSize = 1,labelMaxSize = 16) %>%
      addMinicharts(df_Djambala$lng[1],df_Djambala$lat[1],type = "pie", chartdata = df_Djambala$Freq,width = 75, showLabels = TRUE, colorPalette=rainbow(length(levels(df$Statut))),opacity =0.7,labelMinSize = 1,labelMaxSize = 16) %>%
      addMinicharts(df_Louvakou$lng[1],df_Louvakou$lat[1],type = "pie", chartdata = df_Louvakou$Freq,width = 75, showLabels = TRUE, colorPalette=rainbow(length(levels(df$Statut))),opacity =0.7,labelMinSize = 1,labelMaxSize = 16) %>%
      addMinicharts(df_Loandjili$lng[1],df_Loandjili$lat[1],type = "pie", chartdata = df_Loandjili$Freq,width = 75, showLabels = TRUE, colorPalette=rainbow(length(levels(df$Statut))),opacity =0.7,labelMinSize = 1,labelMaxSize = 16) %>%
      addMinicharts(df_Mossendjo$lng[1],df_Mossendjo$lat[1],type = "pie", chartdata = df_Mossendjo$Freq,width = 75, showLabels = TRUE, colorPalette=rainbow(length(levels(df$Statut))),opacity =0.7,labelMinSize = 1,labelMaxSize = 16) %>%
      addLegend("bottomright",title = "Statuts", colors = rainbow(length(levels(df$Statut))),labels = levels(df$Statut),opacity = 0.7)
  })
}

