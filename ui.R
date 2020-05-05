

# Define UI for application 
ui <- navbarPage(windowTitle = "ESI Pointe Noire bushmeat survey", title=div(tags$a(img(src="esi.gif", style="margin-top: -15px",height ="50" ,width ="179" ), href="https://www.facebook.com/esicongo", target ="_blank")),
                 tabPanel("Dataviewer",
                          ######Sidebar Layout######
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("rank",label = "Choice a taxonomic rank",choices = list("species" = "species", "class" = "class", "order"="order","family"="family","genus"="genus")),
                              selectInput("taxa","Choice taxa (single or multiple",paste("whole taxa",data$species),multiple = TRUE,selected ="whole taxa"),
                              selectInput("var3",label= "Choice market (single or multiple)",choices = c("whole markets", levels(data$LIEU)),selected = "whole markets",multiple=TRUE),
                              checkboxInput("checkbox", label = "Display weighted data : Number of bush meat occurences divided by number of visits", value = FALSE),
                              checkboxInput("checkboxlog", label = "Display logarithmic scale for y axis", value = FALSE),
                              dateRangeInput("dates", label = "Date range",start = "2008-01-01", end = "2019-12-31", min = "2008-01-01",max = "2019-12-31", format = 'yyyy', startview = "decade"),
                              width = 2),
                            ##########################
                            ######Main Panel######
                            mainPanel( 
                              plotlyOutput("plotly",height="600px"),
                              width = 10,)
                            ###################
                            ),
                          DT::dataTableOutput("DT")
                          ),
                 tabPanel("Species information",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("species",label = "Species",choices = levels(data$species)),
                              htmlOutput("More informations"),
                              width = 2
                              ),
                            mainPanel(
                              htmlOutput("frame")
                            )
                          )
                        ),
                 tabPanel("Protocol",
                          includeHTML("C:/Users/Utilisateur/Desktop/Stage/Programmes/R/App_Shiny_ESI_viande_brousse/App_Shiny_ESI_viande_brousse/ESI_viande_brousse/templates/protocole.html")
                 ),
                 tabPanel("Import dataset",
                          shinyjs::useShinyjs(),
                          div(class = 'pull-right', logoutUI(id = 'logout')),
                          loginUI(id='login'),
                          uiOutput("import_data")
                 )
)

