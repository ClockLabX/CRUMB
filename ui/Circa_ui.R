h4(fluidPage(conditionalPanel(
  condition = "$('html').hasClass('shiny-busy')",
  wellPanel("Loading... Please wait...", id = "loadmessage")
),
  sidebarLayout(
  sidebarPanel(
    h3('Select Parameters'),
    dateRangeInput('date_act', 'Select days on DD:'),
    sliderInput('perrange','Period range', min = 10, max=30, value = c(18,28), step=1),
    actionButton("gocirc",'Get Circadian Analysis!'),
    width = 3
  ),
conditionalPanel(condition = "input.gocirc != 0",mainPanel(
    wellPanel(splitLayout(plotlyOutput('period.graph'),
                plotlyOutput('acto.compare')),
    DTOutput('period'),
    downloadButton('circ.table', "Download Circadian information"))
  ,width = 9))
)))