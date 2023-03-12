#
# Circadian Rhythm Using Mealtime Behavior (CRUMB) 
# by Sergio Hidalgo Sotelo  
# 2023
#
#

## Loading required packages
require(shiny) 
source('functions/Packages.R')


## UI with source on ui folder

ui <- fluidPage(   
  theme = shinytheme("darkly"),
  source('Misc/Style.R')[1],
  
  ## Application title and credit
  fluidRow(column(width=2,img(src = "Logo.png", height = 200, width = 200)),
           column(width=7,h1("CRUMB",align='center'), 
                  tags$head(tags$style('h1 {color:#bf0d0d;
        font-weight: bold; font-size:80px}')),
                  h3("Circadian Rhythm Using Mealtime Behavior",align='center'),
                  h5("Sergio Hidalgo & Joanna Chiu. University of California, Davis. 2023",align='center'),
                  h5("Version 1.0 'Muffin Monster'", align='center'))
  ),
  
  tabsetPanel(
    ## Load files and setting dates
    tabPanel('A. File loading & QC', source("ui/Load_QC_ui.R")[1]),
    ## Defining parameters for event calculations & initial graphs
    tabPanel('B. Initial Survey', source('ui/Ini_ui.R')[1]),
    ## Getting in depth info about events
    tabPanel('C. Feeding Analysis', source('ui/Feed_ui.R')[1]),
    ## Running Circadian analysis 
    tabPanel('D. Circadian Analysis', source('ui/Circa_ui.R')[1])
  ))

## Server with source in server folder
server <- function(input, output,session) {
  ## Allowing big FLIC files to be loaded without shiny default cap
  options(shiny.maxRequestSize = 50*1024^2)   
  
  source('functions/PrivateFunctions.R',local = T)
  source("server/Load_QC_server.R", local = T)
  source("server/Ini_server.R", local = T)
  source("server/Feed_server.R",local=T)
  source("server/Circadian_server.R",local = T)
}


# Run the application 
shinyApp(ui = ui, server = server)




