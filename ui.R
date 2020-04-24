

# Define UI for application 
ui <- navbarPage("ESI-Congo",
                 tabPanel("Dataviewer"),
                 tabPanel("Spatial viewer"),
                 tabPanel("Protocole"),
                 tabPanel("Import dataset"),
                 
                 ######Sidebar Layout######
        
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("var1",label = "Taxonomic rank",choices = list("choice 1" = 1, "choice 2" = 2), selected = 1),
                     selectInput("var2",label = "Taxa (single or multiple)",choices = list("single" = 1, "multipe" = 2),selected = 1),
                     selectInput("var3",label= "Market (single or multiple)",choices = list("Marche Central" = "Marche Central", "Marche Faubourg" = "Marche Faubourg","Marche Kouikou" = "Marche Kouikou","Marche Liberte" = "Marche Liberte","Marche Mayaka" = "Marche Mayaka","Marche Mvoumvou" = "Marche Mvoumvou","Marche Nkouikou" = "Marche Nkouikou", "Marche Tie-Tie" = "Marche Tie-Tie", "Tous les marchés" = "Tous les marchés"),selected = "Tous les marchés"),
                     checkboxInput("checkbox", label = "Display weighted data : Number of bush meat occurences divided by number of visits", value = FALSE),
                     dateRangeInput("dates", label = "Date range",start = "2008-01-01", end = "2018-12-31", min = "2008-01-01",max = "2018-12-31", format = 'yyyy', startview = "decade"),
                   ),
                   ##########################
                   
                   ######Main Panel######
                   mainPanel( 
                     plotlyOutput("plotly"),
                   )
                   ######################
                 ),
                 ####Bas de page####
                 DT::dataTableOutput("DT")
                 ###################
)

