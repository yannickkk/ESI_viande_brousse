

# Define UI for application 
ui <- navbarPage(windowTitle = "ESI Pointe Noire bushmeat survey", title=div(tags$a(img(src="esi.gif", style="margin-top: -15px",height ="50" ,width ="179" ), href="https://www.facebook.com/esicongo")),
                 tabPanel("Dataviewer",
                          ######Sidebar Layout######
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("rank",label = "Choice a taxonomic rank",choices = list("species" = "species", "class" = "class", "order"="order","family"="family","genus"="genus")),
                              selectInput("taxa","Choice taxa (single or multiple",paste("whole taxa",data$Scientific_name),multiple = TRUE,selected ="whole taxa"),
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
                              selectInput("species",label = "Species",choices = list("Gorilla gorilla" = "Gorilla gorilla", "Hyemoschus aquaticus" = "Hyemoschus aquaticus", "Varanus niloticus" = "Varanus niloticus"), selected = "Gorilla gorilla"),
                              HTML("More informations")
                              ),
                            mainPanel(
                              htmlOutput("frame")
                            )
                          )
                        )
)

