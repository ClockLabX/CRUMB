h4(fluidPage(
  conditionalPanel(
    condition = "$('html').hasClass('shiny-busy')",
    wellPanel("Loading... Please wait...", id =
                "loadmessage")
  ),
    wellPanel(
      h3('1. Annotate conditions'),
      wellPanel(
        numericInput(
          'n',
          'Number of conditions',
          value = 1,
          min = 1,
          step = 1,
          width = '10%'
        ),
        fluidRow(uiOutput('conditions',inline=T))
      )
    ),
    wellPanel(
      h3('2. Select threshold for feeding events'),
     splitLayout(cellWidths = c("45%", "55%"),
       wellPanel(
       sliderInput(
        'Feeding.Threshold',
        '(A) Feeding min and threshold.',
        min = 1,
        max = 50,
        value = c(15, 20),
        width = "80%"
      ),
      sliderInput(
        'Feeding.Minevents',
        '(B) Number of consecutive licks to constitute a unique feeding event.',
        min = 1,
        max = 10,
        value = 2,
        width = "80%"
      ),
      sliderInput(
        'Feeding.Event.Link.Gap',
        '(C) Time between sporadic events to link them into a single event.',
        min = 1,
        max = 10,
        value = 5,
        width = "80%"
      ),
      br(),
      br(),
      br(),
      actionButton('buttonfed', 'Get Events!')),
      img(
        src = "Thresholds.png",
        height = 392,
        width = 789
      )
      )
      ),
  conditionalPanel(
    condition = "input.buttonfed != 0",
   wellPanel(h3('3. Summary'),
             
  splitLayout(DTOutput('sum.table'),
              DTOutput('sum.table.all')),
  br(),
  splitLayout(wellPanel(h4('Normalization'),
                        switchInput(
                          inputId = "sumshow",
                          onStatus = "success", 
                          offStatus = "danger")),br()),
  br(),
  splitLayout (
     plotlyOutput('graphdays'),
     plotlyOutput('graphtotal'))
  
  )))
)
