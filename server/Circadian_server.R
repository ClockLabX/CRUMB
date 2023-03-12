observe({
  d <- event.collap()
  start <- as.Date(na.omit(unique(d$Date))[1],'%Y-%m-%d')
  end <- as.Date(na.omit(rev(unique(d$Date)))[1],'%Y-%m-%d')
  updateDateRangeInput(
    getDefaultReactiveDomain(),
    "date_act",
    label = 'Select start and end date:',
    start = start,
    end = end,
    min = start,
    max = end
  )
})

acto <- eventReactive(input$gocirc, {
  req(fillcollap())
  datest <- input$date_act[1]
  dateen <- input$date_act[2]
  e <- fillcollap()
  
  # Get sample freq from user input in Set Parameters
  sample_freq <-
    as.numeric(lubridate::seconds(lubridate::as.period(plotinputs()[['window']])))
  
  #Set dates for analysis 
  
  e <- e[which(e$Date==datest)[1]:nrow(e),]
  e <- e[1:last(which(e$Date==dateen)),]
  
  # Get the period of circadian oscillation using Autocorrelation
  period <- Per.stat(e,sample_freq,
                     range=input$perrange)
  period[is.na(period)] <- 0
  period <- period %>% group_by(Fly, period) %>% filter(power > 0)
  period
})

acto.graph <- reactive({
  acto <- acto()
  Color <- as.character(condition_table()[,c('Color')])
  Condition <- condition_table()[,c('Condition')]
  names(Color) <- Condition
  for (i in 1:length(Condition)) {
    acto$Fly <- gsub(paste0(Condition[[i]], ".*", sep = ""),
                     paste(Condition[[i]]),
                     acto$'Fly')
  }
  mean <-
    acto %>% group_by(Fly,period) %>% summarise(meanpower = mean(power), sd =
                                                   sd(power), sem = sd(power) / sqrt(n()))
  gp <- ggplot(mean, aes(x = period, y = meanpower, color = Fly)) +
    ylab('Power') +
    xlab('Period') +
    labs(title = "Periodogram")+
    theme_classic() +
    geom_line() +
    geom_hline(yintercept = acto$signif_threshold[1],
               linetype = 2) +
    geom_point() +
    geom_errorbar(aes(ymin = meanpower - sem, ymax = meanpower + sem))+
    scale_color_manual(values = Color)
  
  gp
})

acto.comp <- reactive({
  period <- acto()
  Color <- as.character(condition_table()[,c('Color')])
  Condition <- condition_table()[,c('Condition')]
  names(Color) <- Condition
  per <- period %>% group_by(Fly) %>% filter(power == max(power),power > 0)
  
  for (i in 1:length(Condition)) {
    per$Fly <- gsub(paste0(Condition[[i]], ".*", sep = ""),
                    paste(Condition[[i]]),
                    per$'Fly')
  }
  a <- ggplot(per, aes(Fly, power, fill = Fly)) +
    labs(title = "Power")+
    ylab('Power')+
    geom_boxplot(alpha = 0.5) +
    geom_jitter() +
    geom_hline(yintercept = per$signif_threshold[1],
               linetype = 2) +
    scale_fill_manual(values = Color) +
    theme_classic()
    
  
  if (length(Condition) > 1) {
    a <- a + stat_compare_means()
  }
  a
})

per.table <- reactive({
  req(acto())
  period <- acto()
  
  per <- period %>% group_by(Fly) %>% 
    filter(power == max(power)) %>% 
    mutate(across(everything(),~round(.,3)))   
    
  per
})

output$period <- renderDT(
  per.table(),
  caption =
    htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black;  font-size:150%;font-weight;bold;','Circadian Stats'),
  options = list(lengthChange = FALSE,
                 pageLength =  12,
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().container()).css({'background-color':'white', 'color': 'black'});",
                   "}")), rownames = F
)

output$circ.table <- downloadHandler(
  filename = function() {
    paste0("Circadian Table", ".csv")
  },
  content = function(file) {
    write.csv(per.table(), file, row.names = F)
  }
)

output$period.graph <- renderPlotly({
  plotly::ggplotly(acto.graph())
})

output$acto.compare <- renderPlotly({
  plotly::ggplotly(acto.comp())
})
