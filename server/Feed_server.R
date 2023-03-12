#### Access input ####

time_labels <- reactive({
  a <- expand.grid(
    hours = 0:24,
    minutes = minutes()
  ) %>%
    # add leading 0's
    mutate_at(1:2, ~ifelse(nchar(.) == 1, paste0('0', .), .))%>%
    # create label
    mutate(time_label = paste0(hours, ':', minutes)) %>%
    arrange(time_label)%>%
    # remove anything over 60:00
    filter(time_label <= '24:00') %>%
    pull(time_label)
  a
})

observe({
  updateSliderTextInput(
    session = session,
    inputId = 'LD',
    choices = time_labels(),
    selected=c("06:00","18:00")
  )
})

plotinputs <- eventReactive(input$goplot,{
  window <- input$binfeedv2
  fun <- input$binfunv2
  error <- input$disperr
  times <- input$LD
  inplot <- list(window=window,fun=fun,error=error,times=times)
  inplot
})


#### Working dataframes ####

fillcollap <- reactive({
  req(filterdf)
  df <- filterdf()
  l <- plotinputs()
  
  #Normalization to max
  if(input$sumshow){
    for (i in 4:ncol(df)) {
      df[, i] = (df[, i] / sum(df[, i]))
      df[is.na(df[,i]),i] <- 0
    }}
  #Binned the data the designated window size
  df <- Collap.time(df, collapse_interval=l[['window']], bin_fun=l[['fun']])
  
  df 
})

event.bin <- eventReactive(input$goplot,{
  req(fillcollap())
  dfm <- fillcollap()
  
  dfm <- dfm[,-c(1,3)]
  dfm <-
    dfm %>% group_by(Time) %>% arrange(desc(Time)) %>% summarise(across(everything(), mean))
  dfm$Time <- strptime(dfm$Time, "%I:%M:%S %p", tz="UTC")
  dfm <- arrange(dfm, Time)
  dfm$Time <-
    format(strptime(dfm$Time, format = '%Y-%m-%d %H:%M:%S',tz="UTC"),
           '%I:%M:%S %p')
  dfm
})

daybin <- eventReactive(input$goplot,{
  req(filterdf())
  df <- filterdf()
  l <- plotinputs()
  
 
  # Close the progress when this reactive exits (even if there's an error)
  
  # if(any(strptime(df$Time,format="%I:%M:%S %p") %in% strptime(l[['times']][1],format="%H:%M")) & any(strptime(df$Time,format="%I:%M:%S %p") %in% strptime(l[['times']][2],format="%H:%M"))){
    showNotification('Getting Day Events',duration=NULL, type="default",id='id')
    day <- df[strptime(df$Time,format="%I:%M:%S %p",tz="UTC") >= strptime(l[['times']][1],format="%H:%M",tz="UTC") & strptime(df$Time,format="%I:%M:%S %p",tz="UTC") <= strptime(l[['times']][2],format="%H:%M",tz="UTC"),]
    day <-   day %>%  group_by(Date) %>% select(!Time & !TimeDate) %>% summarise_all(sum) %>% select(!Date) %>% summarise_all(sum)
    day <- reshape2::melt(day,variable.name='Fly',value.name = "Event")  
    day$Fly <- gsub("\\..*",'',day$Fly)
    removeNotification(id = 'id', session = getDefaultReactiveDomain())
    
    showNotification('Getting Night Events',duration=NULL, type="default",id='id2')
    night <- df[!(strptime(df$Time,format="%I:%M:%S %p",tz="UTC") >= strptime(l[['times']][1],format="%H:%M",tz="UTC") & strptime(df$Time,format="%I:%M:%S %p",tz="UTC") <= strptime(l[['times']][2],format="%H:%M",tz="UTC")),]
    night <-   night %>%  group_by(Date) %>% select(!Time & !TimeDate) %>% summarise_all(sum) %>% select(!Date) %>% summarise_all(sum)
    night <- reshape2::melt(night,variable.name='Fly',value.name = "Event")  
    night$Fly <- gsub("\\..*",'',night$Fly)
    
    daynight <- list(day=day,night=night)
    removeNotification(id = 'id2', session = getDefaultReactiveDomain())
  # }else(
  #   sendSweetAlert(
  #     session = session,
  #     title = "Warning !!!",
  #     text = 'Probably not a full day of recording. Showing entire dataframe.',
  #     type = "warning"
  #   )
  # )
  if(!exists('daynight')){
    ret <- df
  }else(
    ret <- daynight
  )
  ret
})

minutes <- reactive({
  a <- input$binfeedv2
  
  if(a == '1 hour'){
    minutes <- 0
  }
  else if(a == '30 min'){
    minutes <- c(0,30)
  }
  else if(a == '1 min'){
    minutes <- 0:59
  }
  minutes
})


#### Output GRAPHS ####

output$dailygraph <- plotly::renderPlotly({
  req(fillcollap())
  req(input$goplot)
  e <- fillcollap()
  l <- plotinputs()
  c <- as.character(condition_table()[,c('Color')])
  cond <- condition_table()[,c('Condition')]
  
  #Graph considering color and condition name
  d <- Graph.flic(e,Conditions=cond,Color=c,mean=T,error=l[['error']])+
    labs(title = "Daily Feeding Events")
  
  plotly::ggplotly(d)
})


output$graph.event.bin <- plotly::renderPlotly({
  dfm <- event.bin()
  l <- plotinputs()
  c <- as.character(condition_table()[,c('Color')])
  cond <- condition_table()[,c('Condition')]
  
  dfm$Time <- strptime(dfm$Time, "%I:%M:%S %p",tz="UTC")
  dfm <- cbind(dfm$Time, dfm$Time, dfm$Time, dfm[, 2:ncol(dfm)])
  colnames(dfm)[3] <- "TimeDate"
  d <- Graph.flic(dfm,Conditions = cond,
                  Color = c, ld = F,mean=T, error=l[['error']]) +
    scale_x_datetime(date_labels = "%H:%M") +
    labs(title = "Average Daily Feeding Events")+
    Geom_ld(dfm,on=l[['times']][1],off = l[['times']][2])
    
  
  
  
  plotly::ggplotly(d)
})

output$graphlight <- plotly::renderPlotly({
  req(daybin())
  df <- daybin()[['day']]
  Color <- as.character(condition_table()[,c('Color')])
  Condition <- condition_table()[,c('Condition')]
  names(Color) <- Condition
  l <- plotinputs()
  
  a <- ggplot(df, aes(Fly, Event, fill = Fly)) +
    geom_boxplot(alpha = 0.5) +
    geom_jitter() +
    scale_fill_manual(values = Color) +
    theme_classic()+
    ylab('Events/Day')+
    labs(title = "Average daytime events")
  
  if (length(Condition) > 1) {
    a <- a + stat_compare_means()
  }
  
  ggplotly(a)
})

output$graphnight <- plotly::renderPlotly({
  req(daybin())
  df <- daybin()[['night']]
  Color <- as.character(condition_table()[,c('Color')])
  Condition <- condition_table()[,c('Condition')]
  names(Color) <- Condition
  l <- plotinputs()
  
  a <- ggplot(df, aes(Fly, Event, fill = Fly)) +
    geom_boxplot(alpha = 0.5) +
    geom_jitter() +
    scale_fill_manual(values = Color) +
    theme_classic()+
    ylab('Events/Day')+
    labs(title = "Average nighttime events")
  
  if (length(Condition) > 1) {
    a <- a + stat_compare_means()
  }
  
  ggplotly(a)
})

#### Output DT ####

output$dtlight <- renderDT(
  daybin()[['day']] %>% group_by(Fly) %>% summarise(Events=round(mean(Event),3),SD=round(sd(Event),3),SEM=round((sd(Event)/n()),3)),
    caption =
      htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black;  font-size:150%;font-weight;bold;','Daytime Event Stats'),
    options = list(lengthChange = FALSE,
                   pageLength =  12,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().container()).css({'background-color':'white', 'color': 'black'});",
                     "}")), rownames = F
    
)
 
output$dtnight <- renderDT(
  daybin()[['night']] %>% group_by(Fly) %>% summarise(Events=round(mean(Event),3),SD=round(sd(Event),3),SEM=round((sd(Event)/n()),3)),
  caption =
    htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black;  font-size:150%;font-weight;bold;','Nighttime Event Stats'),
  options = list(lengthChange = FALSE,
                 pageLength =  12,
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().container()).css({'background-color':'white', 'color': 'black'});",
                   "}")), rownames = F
  
)


#### Output download ####

output$download.feed <- downloadHandler(
  filename=function() {
    paste0("Daily Feeding Events", ".csv")
  },
  content=function(file) {
    write.csv(fillcollap(), file, row.names = F)
  }
)

output$download.avg.feed <- downloadHandler(
  filename=function() {
    paste0("Avg Feeding Events", ".csv")
  },
  content=function(file) {
    write.csv(event.bin(), file, row.names = F)
  }
)
output$daytime.events <- downloadHandler(
  filename=function() {
    paste0("Daytime events", ".csv")
  },
  content=function(file) {
    write.csv(daybin()[['day']], file, row.names = F)
  }
)

output$nighttime.events <- downloadHandler(
  filename=function() {
    paste0("Nighttime events", ".csv")
  },
  content=function(file) {
    write.csv(daybin()[['night']], file, row.names = F)
  }
)