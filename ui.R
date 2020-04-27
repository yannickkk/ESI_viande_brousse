

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
                     selectInput("var3",label= "Market (single or multiple)",choices = list("Marché Central" = "Marché Central", "Marché Faubourg" = "Marché Faubourg","Marché Kouikou" = "Marché Kouikou","Marché Liberté" = "Marché Liberté","Marché Mayaka" = "Marché Mayaka","Marché Mvoumvou" = "Marché Mvoumvou","Marché Nkouikou" = "Marché Nkouikou", "Marché Tié-Tié" = "Marché Tié-Tié", "Tous les marchés" = "Tous les marchés"),selected = "Tous les marchés"),
                     checkboxInput("checkbox", label = "Display weighted data : Number of bush meat occurences divided by number of visits", value = FALSE),
                     dateRangeInput("dates", label = "Date range",start = "2008-01-01", end = "2019-12-31", min = "2008-01-01",max = "2019-12-31", format = 'yyyy', startview = "decade"),
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

