
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
    validate(
      need(input$taxa != "", "Please select a taxa")
    )
    validate(
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
  output$import_data <- renderUI({
    req(credentials()$user_auth)
    fileInput("file1", "Download dataset as data_final.csv",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/Semicolon-separated-values,text/plain",
                         ".csv"))
  })
  output$import_protocol <- renderUI({
    req(credentials()$user_auth)
    fileInput("file2","Dowload protocol as protocole.html",
              multiple = FALSE,
              accept = c("text/html",
                         ".html"))
  })
  output$import_account <- renderUI({
    req(credentials()$user_auth)
    fileInput("file3", "Download account as account.csv",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/Semicolon-separated-values,text/plain",
                         ".csv"))
  })
  ##############
  
  #####Affichage import#####
  output$contents_csv <- renderTable({
    req(credentials()$user_auth)
    req(input$file1)
    
    tryCatch(
      {
        data <- read.csv2(input$file1$datapath,
                       header = TRUE)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  output$text <- renderUI({
    req(credentials()$user_auth)
    tags$h5("Column needed :")
  })
  
  output$head_data <- renderUI({
    req(credentials()$user_auth)
    colonnes <- tolower(names(data)[1])
    for (i in 2:length(names(data))){
      colonnes <- tolower(paste(colonnes,names(data)[i],sep = ", "))
    }
    return(colonnes)
  })
  
  output$text_2 <- renderUI({
    req(credentials()$user_auth)
    tags$h5("Column needed : username, password")
  })
}

