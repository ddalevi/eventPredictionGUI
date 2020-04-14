library( shiny )
library( shinythemes )
library( anytime )
library( eventPrediction )


source("params.R")
source("about.R")

VERSION <- 0.1

navbarPage( paste0( "Event Prediction GUI (v. ", VERSION, ")" ), 
            theme = shinytheme( "yeti" ), selected = "Parametric", fluid=TRUE, 
  
   tags$head(
     tags$link( rel = "stylesheet", type = "text/css", href = "stylesheet.css")
   ),
           
   # Application title
   tabPanel( "Parametric",
         # Sidebar with a slider input for number of bins 
         sidebarLayout(  
            sidebarPanel(
              selectInput( "eventParam", "Parameters:",
                           c( "Trial parameters" = "trialParams",
                              "Event rates" = "eventParams",
                              "Prediction" = "predictParams",
                              "Dropouts" = "dropoutParams",
                              "Critical No events" = "criticalEvents"
                           ) ),
              uiOutput( "showParams" ),
               width = 2 ),
            # Show a plot of the generated distribution
            mainPanel(
                htmlOutput( "epText" ),
                plotOutput( "epPlot" ),
                HTML( "<br><b>Display options:</b><br>" ),
                uiOutput( "displayOptions" ), 
                HTML( "<b>Export options:</b><br>" ),
                downloadButton( 'downloadEPPlot' ), 
                selectInput( 'format', "Format: ", c( "pdf", "bmp" ) ),
                checkboxInput( "includeTitle", "Include title text", FALSE )
            )
     ) ),
     tabPanel( "About", 
               htmlOutput( "aboutTxt" ) )
)



