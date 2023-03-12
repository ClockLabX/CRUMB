####### Load and Populate #######
data <- eventReactive(input$populate, {
  req(input$files)
  
  #Getting file names to extract monitor numbers
  mycsvs <- rbind(input$files$datapath)
  namescsv <- rbind(input$files$name)
  
  monitors <-
    as.numeric(gsub("([0-9]+).*$", "\\1", (substr(namescsv, 5, 6))))
  un <- unique(monitors)
  
  mycsvs1 <- t(mycsvs[which(monitors == un[[1]])])
  namescsv1 <- t(namescsv[which(monitors == un[[1]])])
  
  # Create a Progress object
  progress <- shiny::Progress$new()
  progress$set(message = "Loading", value = 0)
  
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())
  
  # Create a callback function to update progress.
  # Each time this is called:
  # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
  #   distance. If non-NULL, it will set the progress to that value.
  # - It also accepts optional detail text.
  updateProgress <- function(value = NULL, detail = NULL) {
    if (is.null(value)) {
      value <- progress$getValue()
      value <- value + (progress$getMax() - value) / 5
    }
    progress$set(value = value, detail = detail)
  }
  
  # Loads file
  dfm <- Get.flic.batch(mycsvs1, namescsv1, un[[1]], updateProgress)
  
  # Append files from different monitors
  if (length(un) > 1) {
    for (i in 2:length(un)) {
      mycsvtmp <- t(mycsvs[which(monitors == un[[i]])])
      namescsvtmp <- t(namescsv[which(monitors == un[[i]])])
      tmp <- Get.flic.batch(mycsvtmp, namescsvtmp, i, updateProgress)
      dfm <- merge.data.frame(
        dfm,
        tmp,
        by = c('Date', 'Time'),
        sort = F,
        suffixes = c("", "")
      )
    }
  }
  dfm
})

observeEvent(data(), {
  d <- data()
  start <- as.Date(na.omit(unique(d$Date))[1],'%m/%d/%Y')
  end <- as.Date(na.omit(rev(unique(d$Date)))[1],'%m/%d/%Y')
  updateDateRangeInput(
    getDefaultReactiveDomain(),
    "dates",
    label = 'Select start and end date:',
    start = start,
    end = end,
    min = start,
    max = end
  )
})

###### Working Dataframes #######

basedfm <- eventReactive(input$goini, {
  req(data())
  req(input$dates)
  baseline.mod <- input$baselineopt
  win.baseline <- input$window
  datest <- input$dates[1]
  dateen <- input$dates[2]
  
  # Create a Progress object
  progress <- shiny::Progress$new()
  progress$set(message = "Loading", value = 0)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())
  
  # Create a callback function to update progress.
  # Each time this is called:
  # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
  #   distance. If non-NULL, it will set the progress to that value.
  # - It also accepts optional detail text.
  updateProgress <- function(value = NULL, detail = NULL) {
    if (is.null(value)) {
      value <- progress$getValue()
      value <- value + (progress$getMax() - value) / 5
    }
    progress$set(value = value, detail = detail)
  }
  
  d <- data()
  f <- Set.date(d, datest, dateen)
  p <- Calculate.base(f, win.baseline, baseline.mod, updateProgress)
  p
})

binned <- eventReactive(input$goini, {
  req(basedfm())
  window <- input$timeframe
  fun <- input$Bin_fun
  
  p <- basedfm()
  e <- Collap.time(p, window, fun)
  e
})

dfm_bin <- eventReactive(input$goini, {
  dfm <- binned()
  dfm <-
    dfm %>% group_by(Time) %>% arrange(desc(Time)) %>% summarise(across(everything(), mean))
  dfm$Time <- strptime(dfm$Time, "%I:%M:%S %p",tz="UTC")
  dfm <- arrange(dfm, Time)
  dfm$Time <-
    format(strptime(dfm$Time, format = '%Y-%m-%d %H:%M:%S',tz="UTC"),
           '%I:%M:%S %p')
  dfm <- select(dfm,-c('Date','TimeDate'))
  dfm
})

###### Output #######

output$plot <- plotly::renderPlotly({
  dfm <- binned()
  d <- Graph.flic(dfm) +
    labs(title = "Daily Baselined signal")
  plotly::ggplotly(d)
})


output$download.traces <- downloadHandler(
  filename = function() {
    paste0("Daily Baselined Activity", ".csv")
  },
  content = function(file) {
    write.csv(binned(), file, row.names = F)
  }
)

output$download.avgday <- downloadHandler(
  filename = function() {
    paste0("Average Daily Baselined Activity ", ".csv")
  },
  content = function(file) {
    write.csv(dfm_bin(), file, row.names = F)
  }
  
)

