##Gets monitor numbers from file names 
a <-  eventReactive(input$goini,{
                      namescsv <- rbind(input$files$name)
                      monitors = as.numeric(gsub("([0-9]+).*$", "\\1", (substr(namescsv, 5, 6))))
                      un = unique(monitors)
                      a = c(unlist(un))
                      a
                    })

###### Condition setting #######
num_condition <- reactive({
  if(is.na(input$n)){
    a <- 1
  }else(
    a <- input$n
  )
  a
})

col <- reactive({
  (paste0("Cond", seq_len(num_condition())))
})
col2 <- reactive({
  (paste0("Condi", seq_len(num_condition())))
})
col3 <- reactive({
  (paste0("Condit", seq_len(num_condition())))
})
col4 <- reactive({
  (paste0("Conditi", seq_len(num_condition())))
})

output$conditions = renderUI({
  req(data())

  a <-  a()
  Condition <-  map(col(),  ~ textInput(.x, 'Condition', value = isolate(input[[.x]])))
  DFMo <- map(col4(),
              ~ selectInput(
                .x,
                'Monitor',
                choices = a,
                selected = isolate(input[[.x]])
              ))
  Colour <- map(col3(),  ~ colourInput(.x, "Colour" , "#BF0D0D"))
  Wells <- map(
    col2(),
    ~ checkboxGroupInput(
      .x,
      'Wells',
      inline = T,
      selected = paste0("W", seq_len(12)),
      choices = paste0("W", seq_len(12))
    )
  )
  list(fluidRow(
    column(3, Condition),
    column(2, Colour),
    column(2, DFMo),
    column(5, Wells)
  ))
})


conditions_out <- eventReactive(input$buttonfed, {
  map(col(),  ~ input[[.x]]) %>% map_chr(toString)
})
Wells_out <- eventReactive(input$buttonfed,{
  map(col2(),  ~ input[[.x]]) %>% map_chr(toString)
})
colors_out <- reactive({
  map(col3(),  ~ input[[.x]]) %>% map_chr(toString)
})
monitors_out <- eventReactive(input$buttonfed, {
  map(col4(),  ~ input[[.x]]) %>% map_chr(toString)
})

##### Access Descriptors #####
condition_table <- reactive({
  Condition <- conditions_out()
  if(any(Condition == "")){
    Condition <- cbind(paste("Condition_",1:input$n,sep=""))
  }
  Well <- Wells_out()
  Color <- colors_out()
  DFM <- monitors_out()
  d <- cbind(Condition, Well, Color, DFM)
  colnames(d)[1] <- 'Condition'
  d
})

observe({
  req(data())
  req(colors_out())
  
  Wells=(map(col2(),  ~ input[[.x]]) %>% map_chr(toString) %>% strsplit(.,", ") )[[1]]
  
  if (length(Wells) <= 1){
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = "Please select at least two or more wells.",
      type = "error"
    )
  }
})

asig.data <- eventReactive(input$buttonfed,{
  req(basedfm())
  b <- basedfm()
  con.table <- condition_table()
  b.fil <- select(b, paste0(unlist(strsplit(con.table[1, 4], ", ")), "_", unlist(strsplit(con.table[1, 2], ", "))))
  colnames(b.fil) <- rep.int(con.table[1, 1], ncol(b.fil))
  if (nrow(con.table) > 1) {
    for (i in 2:nrow(con.table)) {
      tmp <- select(b, paste0(unlist(strsplit(
        con.table[i, 4], ", "
      )), "_", unlist(strsplit(
        con.table[i, 2], ", "
      ))))
      colnames(tmp) <- rep.int(con.table[i, 1], ncol(tmp))
      b.fil <- cbind(b.fil, tmp)
    }
  }
  df <- cbind(b[, 1:3], b.fil)
  df
})

###### Feeding Event calculation #######

setfed <- eventReactive(input$buttonfed, {
  req(asig.data())
  asig <-  asig.data()
  coln <-  colnames(asig)
  max <- max(input$Feeding.Threshold)
  min <- min(input$Feeding.Threshold)
  fmin <- input$Feeding.Minevents
  gap <- input$Feeding.Event.Link.Gap

  # Create a Progress object
  progress <- shiny::Progress$new()
  progress$set(message = "Getting events", value = 0)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())

  # Create a callback function to update progress.
  # Each time this is called:
  # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
  #   distance. If non-NULL, it will set the progress to that value.
  # - It also accepts optional detail text.

  updateProgress <- function(value = NULL, detail = NULL, message=NULL) {
    if (is.null(message)) {
      progress$set(value = value, detail = detail)
    }
    else(progress$set(value = value, detail = detail, message = message))
    
  }

  # Gets the feeding events (adapted from FLIC R Code Master)
  asigfed <- Set.Lick.Data(asig,
                           Feed.min = min,
                           Feed.max = max,
                           Feeding.Minevents = fmin,
                           Feeding.Event.Link.Gap = gap,
                           updateProgress)

  colnames(asigfed) <- coln
  
  #Get the events from licks
  asigfed <- Get.Event.Data(asigfed)
  asigfed
})

event.collap <- reactive({
  req(setfed())
  window <- input$timeframe
  e <- setfed()
  
  #Binned the data the designated window size
  e <- Collap.time(e, collapse_interval=window, bin_fun="Sum")
  
  e
})


m2 <- reactive({
  req(event.collap())
  e <- event.collap()

  e <- e[, -c(1, 2)]
  m2 <-
    reshape2::melt(
      e,
      id.vars = "TimeDate",
      variable.name = "Fly",
      value.name = "Signal"
    )
  m2 <- m2 %>% group_by(Fly) %>% summarise(Total_Events = sum(Signal))
  
  m2
})

m3 <- reactive({
  req(m2())
  s=input$sum.table_rows_selected
  m <- as.data.frame(m2(),row.names = F)

  if(length(s)){
    m <- m[-s,]
  }

  m$Fly <- gsub('\\..*', "", m$Fly)

    m <-
      m %>% group_by(Fly) %>% 
        summarise(
        Events = round(mean(Total_Events),3),
        SD = round(sd(Total_Events),3),
        SEM = round(sd(Total_Events) / sqrt(n()),3)
        )
  m
})
###### Outputs #######

output$sum.table <- renderDT(
  m2() , extension = 'Buttons', caption =
    htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black;  font-size:150%; font-weight;bold;','Individual Stats'),
  options = list(lengthChange = FALSE,
                 pageLength =  12,
                 buttons = c('copy','csv','excel'),
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().container()).css({'background-color':'white', 'color': 'black'});",
                   "}")), rownames = F
)

output$sum.table.all <- renderDT(
  m3() , caption =
    htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black;  font-size:150%;font-weight;bold;','Group Stats'),
  options = list(lengthChange = FALSE,
                        pageLength =  12,
                        initComplete = JS(
                          "function(settings, json) {",
                          "$(this.api().table().container()).css({'background-color':'white', 'color': 'black'});",
                          "}")), rownames = F
)

output$graphtotal <- renderPlotly({
  req(m2())
  s=input$sum.table_rows_selected
  Color <- as.character(condition_table()[,c('Color')])
  Condition <- condition_table()[,c('Condition')]
  names(Color) <- Condition

  m <- as.data.frame(m2(),row.names = F)

  if(length(s)){
    m <- m[-s,]
  }

  m$Fly <- gsub('\\..*', "", m$Fly)


    a <- ggplot(m, aes(Fly, m[,2] , fill = Fly)) +
      geom_boxplot(alpha = 0.5) +
      geom_jitter() +
      scale_fill_manual(values = Color) +
      theme_classic()+
      ylab(colnames(m)[2])+
      labs(title = "Number of Events")
    
    if (length(Condition) > 1) {
      a <- a + stat_compare_means()
    }

  ggplotly(a)

})

observe({
  req(m2())
  insertUI(selector = "#graphtotal",
            where = "afterEnd",
            # beep.wav should be in /www of the shiny app
            ui = tags$audio(src = "listen.mp3", type = "audio/mp3", autoplay = T, controls = NA, style="display:none;")
  )
})


output$graphdays <- renderPlotly({
  req(filterdf())
  df <- filterdf()
  
  if(input$sumshow){
    for (i in 4:ncol(df)) {
      df[, i] = (df[, i] / sum(df[, i]))
      df[is.na(df[,i]),i] <- 0
    }}
  e <- Collap.time(df, collapse_interval="daily", bin_fun="Sum")
  gp <- Graph.flic(e,
                   ld = F,
                   mean = FALSE) +
    labs(title = "Events per day")

  ggplotly(gp)
})


#### FILTERED #####

filterdf <- reactive({
  req(m2())
  req(setfed())
  s=input$sum.table_rows_selected
  df <- setfed()
  df <- df[,]
  m <- as.data.frame(m2(),row.names = F)
  
  if(length(s)){
    colselect <- m[-s,1]
    tmp <- df[,which(colnames(df) %in% colselect)]
    df <- cbind(df[,1:3],tmp)
  }
  
  df
})


