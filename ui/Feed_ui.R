h4(fluidPage(conditionalPanel(
  condition = "$('html').hasClass('shiny-busy')",
  wellPanel("Loading... Please wait...", id = "loadmessage")
),

sidebarLayout(
sidebarPanel(
  h3('Select Parameters'),
  radioGroupButtons('disperr','Display Error bars?',choices=c("SEM","SD","None"),selected = "SEM"),
  selectInput('binfeedv2','Binning interval', choices = c("1 min","30 min","1 hour"),selected = "1 hour", width = "50%"),
  selectInput('binfunv2','Binning function', choices = c("Sum","Mean"),selected = "Sum",width = "50%"),
  sliderTextInput('LD','Light Transition',choices = c("00:00","24:00"),selected = c("00:00","24:00")),
  actionButton('goplot','Plot!'),width = 3),
conditionalPanel(condition = "input.goplot !=0",
mainPanel(
  wellPanel(
  plotlyOutput('dailygraph'),
  downloadButton('download.feed','Daily Feeding Events'),
  br(),
  br(),
  plotlyOutput('graph.event.bin'),
  downloadButton('download.avg.feed',label='Download Avg Daily Events'),
  splitLayout(wellPanel(plotlyOutput('graphlight'),
                        downloadButton('daytime.events', label='Download Day Events')),
  wellPanel(plotlyOutput('graphnight'),
            downloadButton('nighttime.events', label='Download Night Events'))),
  br(),
  splitLayout(DTOutput('dtlight'),DTOutput('dtnight'))
  ),width = 9)
))))