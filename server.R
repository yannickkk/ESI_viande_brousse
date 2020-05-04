
# Define server logic 
server <- function(input, output, session) {
  observe({
    rank <- input$rank
    if (rank == "species") {
      data$species <- as.factor(paste(data$ESPECE.OBSERVEE,data$species,sep =" - "))
    }
    updateSelectInput(session,"taxa",label = paste('Select ',rank),choices = c("whole taxa",levels(data[,rank])))
  })
  
  
  
  ######Plotly#####
  output$plotly <- renderPlotly({
    validate(
      need(input$taxa != "", "Please select a taxa")
    )
    
    #####Checking taxa####
    taxa <- input$taxa
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
  ###################
  observe({
    specie <- as.character(input$species)
    link <<- paste0('http://apiv3.iucnredlist.org/api/v3/website/',specie)
  })
  output$frame <- renderUI({
    tags$iframe(src=link, height=600, width=535,frameborder = "no")
  })
}

