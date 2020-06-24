

# Define UI for application 
ui <- navbarPage(windowTitle = "ESI Pointe Noire bushmeat survey", title=div(tags$a(img(src="esi.gif", style="margin-top: -15px",height ="50" ,width ="179" ), href="https://www.facebook.com/esicongo", target ="_blank")),
                 tabPanel("Dataviewer",
                          ######Sidebar Layout######
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("rank",label = "Choice taxonomic rank",choices = list("species" = "species", "class" = "class", "order"="order","family"="family","genus"="genus")),
                              selectInput("taxa","Choice taxa (single or multiple)",paste("whole taxa",data$species),multiple = TRUE,selected ="whole taxa"),
                              selectInput("var3",label= "Choice market (single or multiple)",choices = c("whole markets", levels(data$LIEU)),selected = "whole markets",multiple=TRUE),
                              checkboxInput("checkbox", label = "Display weighted data : Number of bush meat occurences divided by number of visits", value = FALSE),
                              checkboxInput("checkboxlog", label = "Display logarithmic scale for y axis", value = FALSE),
                              dateRangeInput("dates", label = "Date range",start = "2008-01-01", end = "2019-12-31", min = "2008-01-01",max = "2019-12-31", format = 'yyyy', startview = "decade"),
                              width = 2),
                            ##########################
                            ######Main Panel######
                            mainPanel( 
                              plotlyOutput("plotly",height="600px"),
                              h6("Certaines données ont été réajusté pour compenser le manque d'informations"),
                              width = 10)
                            ###################
                            ),
                          DT::dataTableOutput("DT")
                          ),
                 tabPanel("Spatial viewer",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("rank2",label = "Choice taxonomic rank",choices = list("species" = "species", "class" = "class", "order"="order","family"="family","genus"="genus")),
                              selectInput("taxa2","Choice taxa (single or multiple",choices = c("whole taxa",data$species),multiple = TRUE,selected ="whole taxa"),
                              selectInput("market2",label= "Choice market (single or multiple)",choices = c("whole markets", levels(data$LIEU)),selected = "whole markets",multiple=TRUE),
                              selectInput("statut",label = "Statut", choices =list("IUCN statut"="IUCN statut","Republic of Congo statut"="Republic of Congo statut"), selected = "Republic of Congo statut"),
                              dateRangeInput("dates2", label = "Date range",start = "2008-01-01", end = "2019-12-31", min = "2008-01-01",max = "2019-12-31", format = 'yyyy', startview = "decade"),
                              width = 2
                            ),
                            mainPanel(leafletOutput("map", height = 850),
                                      width = 10
                            )
                          )
                        ),
                 tabPanel("Species information",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("species",label = "Species",choices = c("",levels(data$name)),selected = ""),
                              htmlOutput("More informations"),
                              width = 2
                              ),
                            mainPanel(
                              htmlOutput("frame")
                            )
                          )
                        ),
                 tabPanel("Protocol",
                          includeHTML("protocole.html")
                 ),
                 tabPanel("Import dataset",
                          shinyjs::useShinyjs(),
                          div(class = 'pull-right', logoutUI(id = 'logout')),
                          loginUI(id='login'),
                          htmlOutput("Import data"),
                          uiOutput("import_data"),
                          htmlOutput("head_data"),
                          htmlOutput("Import protocol"),
                          uiOutput("import_protocol"),
                          htmlOutput("Import district"),
                          uiOutput("import_district")
                 )
)

