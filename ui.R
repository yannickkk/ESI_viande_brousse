

# Define UI for application 
ui <- navbarPage("ESI-Congo",
                 tabPanel("Dataviewer",
                          ######Sidebar Layout######
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("rank",label = "Taxonomic rank",choices = list("Nom latin" = "NOM.LATIN", "class" = "class", "order"="order","family"="family","genus"="genus")),
                              selectInput("taxa","Choose species",data$NOM.LATIN,multiple = TRUE),
                              selectInput("var3",label= "Market (single or multiple)",choices = c("Tous les marchés", levels(data$LIEU)),selected = "Tous les marchés",multiple=TRUE),
                              checkboxInput("checkbox", label = "Display weighted data : Number of bush meat occurences divided by number of visits", value = FALSE),
                              checkboxInput("checkboxlog", label = "Display logarithmic scale for y axis", value = FALSE),
                              dateRangeInput("dates", label = "Date range",start = "2008-01-01", end = "2019-12-31", min = "2008-01-01",max = "2019-12-31", format = 'yyyy', startview = "decade")
                              ),
                            ##########################
                            ######Main Panel######
                            mainPanel( 
                              plotlyOutput("plotly")
                              )
                            ###################
                            ),
                          DT::dataTableOutput("DT")
                          ),
                 tabPanel("Species presentation",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("species",label = "Species",choices = list("Gorilla gorilla" = "Gorilla gorilla", "Hyemoschus aquaticus" = "Hyemoschus aquaticus", "Varanus niloticus" = "Varanus niloticus"), selected = "Gorilla gorilla")
                              ),
                            mainPanel(
                              htmlOutput("frame")
                            )
                          )
                        )
)

