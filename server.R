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
      if(taxa[1] == "whole taxa") {
        observe({
          updateSelectInput(session,"taxa",label = paste('Select ',rank),choices = c("whole taxa",levels(data[,rank])),selected = taxa[2])
        })
      } else {
        observe({
          updateSelectInput(session,"taxa",label = paste('Select ',rank),choices = c("whole taxa",levels(data[,rank])),selected = taxa[length(taxa)])
        })
      }
      
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
      if(taxa2[1] == "whole taxa"){
        observe({
          updateSelectInput(session,"taxa2",label = paste('Select ',rank2),choices = c("whole taxa",levels(data[,rank2])),selected = taxa2[2])
        })
      } else {
        observe({
          updateSelectInput(session,"taxa2",label = paste('Select ',rank2),choices = c("whole taxa",levels(data[,rank2])),selected = taxa2[length(taxa2)])
        })
      }
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
    data_cut2_market <- data_cut_taxa2
    if ("whole markets"%in%marche2){
      data_cut2_market <- data_cut_taxa2
    } else {
      data_cut2_market <- data_cut_taxa2[which(data_cut_taxa2[,"LIEU"] == marche2[1]),]
      if (length(marche2) > 1){
        for (i in 2:length(marche2)){
          data_cut2_market <- rbind(data_cut2_market,data_cut_taxa2[which(data_cut_taxa2[,"LIEU"] == marche2[i]),])
        }
      }
      data_cut2_market$LIEU <- factor(data_cut2_market$LIEU,exclude=NULL)
    }
    ########################
    
    ########################
    #####Checking date#####
    dates_2 <- as.numeric(substring(input$dates2,1,4))
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
    
    data_cut2 <- data_cut2_market[which(substring(data_cut2_market[,"DATE"],7,10) == dates[1]),]
    if (length(dates) > 1){
      for (i in 2:length(dates)){
        data_cut2 <- rbind(data_cut2,data_cut2_market[which(substring(data_cut2_market[,"DATE"],7,10) == dates[i]),])
      }
    }
    data_cut2$DATE <- factor(data_cut2$DATE,exclude=NULL)
    #####################
    
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
    # for (i in unique(df[,c("District")])){
    #   if(length(district[which(district[,"sous_district"]== i),"district"] != 0)){
    #     df[which(df[,c("District")]==i),"DISTRICT"] <- strrep(district[which(district[,"sous_district"]==i),"district"],1)
    #     df[which(df[,c("District")]==i),"lng"] <- rep(district[which(district[,"sous_district"]==i),"lng"],length(df[which(df[,c("District")] == i),"lng"]))
    #     df[which(df[,c("District")]==i),"lat"] <- rep(district[which(district[,"sous_district"]==i),"lat"],length(df[which(df[,c("District")] == i),"lat"]))
    #   }
    # } 
    for (i in unique(df[,c("District")])){
      if(length(which(cent_dist_geo[,"ADM2_FR"]$ADM2_FR== i) != 0)){
      df[which(df[,c("District")]==i),"DISTRICT"] <- strrep(i,1)
      df[which(df[,c("District")]==i),"lng"] <- rep(coordinates(cent_dist_geo[which(cent_dist_geo[,"ADM2_FR"]$ADM2_FR==i),])[,"coords.x1"],length(df[which(df[,c("District")] == i),"lng"]))
      df[which(df[,c("District")]==i),"lat"] <- rep(coordinates(cent_dist_geo[which(cent_dist_geo[,"ADM2_FR"]$ADM2_FR==i),])[,"coords.x2"],length(df[which(df[,c("District")] == i),"lat"]))
    }}
    names(df) <- c("sous_district","Statut","Freq","District","lng","lat")
    
    
    if ("IUCN statut"%in%statut){
      df$Statut <- factor(df$Statut, levels = c("LC","NT","VU","EN","CR","DD"))
      pal <- c("#1AC227","#D2FF05","#FBD610","#FB8010","#FF0C00","#666968")
    } else {
      #df$Statut <- factor(df$Statut, levels = c(""))
      pal <- c("#1AC227","#FFAE05","#FF0C00","#666968")
    }
    #####################
    
    df <- arrange(df,Statut)
    ####Création des données pour chaque district#####
    df$District <- as.factor(df$District)
    name_df <- c()
    for (i in levels(df$District)){
      name <- paste("df",i,sep ="_")
      df_district <- df[which(df[,c("District")]== i), c("Statut","Freq","District","lng","lat")]
      df_district %>%
        group_by(Statut) %>%
        mutate(Freq = sum(Freq))
      
      df_district <- df_district[!duplicated(df_district[,c('Statut')]),]
      assign(name,df_district)
      name_df <- c(name_df,name)
    }
    
    
    #################
    #################
    tilesURL <-"https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png"
    
    prot_geo <- leaflet(data=c(protected_geo,distircts_geo)) %>%
      addTiles(tilesURL)  %>%# Add default OpenStreetMap map tiles
      setView(lng=12.330, lat=-4.029,zoom = 8) %>%
      addPolygons(data = distircts_geo, stroke = TRUE, smoothFactor = 0.6, fill = TRUE, label  = distircts_geo$ADM2_FR, color = "grey", dashArray = "3", weight= 3.95, fillOpacity = 0,labelOptions = labelOptions(textsize = "13px")) %>%
      addPolygons(data =protected_geo, stroke = TRUE, smoothFactor = 0.6, fill = TRUE, label  = protected_geo$NAME,  color = "black", weight= 2, fillColor= "#FFFFCC", fillOpacity = 0.4,labelOptions = labelOptions(textsize = "13px")) %>%
      addLegend("topright",title = "Statuts", colors = pal,labels = levels(df$Statut),opacity = 0.7)
      
      for (i in name_df){
        d <- get(i)
        Freq <- d %>% select(Freq)
        rownames(Freq) <- d$Statut
        Freq <- t(Freq)
        if (substring(i,4)=="Autre"){
          prot_geo <- addMinicharts(map = prot_geo, lng = d$lng[1],lat = d$lat[1],type = "pie", chartdata = Freq,width = 75, showLabels = TRUE, colorPalette=pal ,opacity =0.7,labelMinSize = 1,labelMaxSize = 32,layerId = "Origin unknown",popup = popupArgs(),legend = FALSE)
        } else {
          print(Freq)
          prot_geo <- addMinicharts(map = prot_geo, lng = d$lng[1],lat = d$lat[1],type = "pie", chartdata = Freq,width = 75, showLabels = TRUE, colorPalette=pal ,opacity =0.7,labelMinSize = 1,labelMaxSize = 32, layerId = substring(i,4),popup = popupArgs(labels = levels(df$Statut)),legend = FALSE)
        }
      }
      
      prot_geo
  })
}
