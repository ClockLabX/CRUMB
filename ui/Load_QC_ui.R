h4(fluidPage
  (conditionalPanel(
    condition = "$('html').hasClass('shiny-busy')",
    wellPanel("Loading... Please wait...", id = "loadmessage")
  ),
    wellPanel(
    h3('1. Select files and populate dates'),
    fileInput("files", "Select files", multiple = T, width = '30%',accept = ".csv"),
    actionButton('populate', 'Cache data & Populate dates'),
  ),
  wellPanel(
    h3('2. Select parameters'),
    flowLayout(
      selectInput(
        'baselineopt',
        'Baseline:',
        choices = c('Running Median', 'Asymmetric Least Squares'),
        selected = 'Running Median'
      ),
      conditionalPanel(
        condition = 'input.baselineopt=="Running Median"',
        numericInput('window', 'Minutes for baseline:', 
                     value =5)
      ),
      selectInput(
        inputId = "timeframe",
        label = "Binning interval:",
        choices = c("1 min", "5 min", "30 min", "1 h"),
        selected = "1 h"
      ),
      selectInput('Bin_fun', 'Binning function:', choices = c("Sum", "Mean"))
    ),
    fluidRow(column(
      width = 6,
      dateRangeInput(
        'dates',
        'Select start and end date:',
        start = 0,
        end = 0
      )
    )),
    actionButton('goini', 'Baseline data!')
  ),
  conditionalPanel(
    condition = "input.goini!=0",
    wellPanel(h3('3. Baselined data'),
    splitLayout(plotlyOutput('plot')),
    br(),
    downloadButton('download.traces', 'Download Daily Baselined Signal'),
    downloadButton('download.avgday', 'Download Average Baselined Signal')
  )))
)
