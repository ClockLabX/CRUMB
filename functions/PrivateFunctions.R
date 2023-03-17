#' Load batch DFM
#'
#' Batch loading subsequent DFM files from a folder. It produce
#' a compiled bind dataframe with the information from all the files loaded.
#' @param x Path
#' @param names Filenames
#' @param id Monitor ID
#' @export

Get.flic.batch <- function(x, names, id, updateProgress = NULL) {
  text <- print(paste("Reading", names[1]))
  updateProgress(detail=text,value=0)
  dfm <- as.data.frame(fread(x[1], header = TRUE))
  if (length(names) > 1) {
    for (i in 2:ncol(names)) {
      # If we were passed a progress update function, call it
      if (is.function(updateProgress)) {
        text <- print(paste("Reading", names[i]))
        updateProgress(detail = text)
        updateProgress(value = i / ncol(names))
      }
      tmp <- as.data.frame(fread(x[i], header = TRUE))
      dfm <- rbind(dfm, tmp)
    }
  }
  dfm <- cbind(Date=dfm$Date, Time= dfm$Time, dfm[,which(colnames(dfm)=="W1"):which(colnames(dfm)=="W12")])
  dfm$TimeDate <- paste(dfm$Date, dfm$Time, sep = " ")
  dfm$TimeDate <-
    parse_date_time(dfm$TimeDate, '%m-%d-%Y %I:%M:%S %p')
  dfm <- dfm[order(dfm$TimeDate), ]
  dfm <- dfm[,-which(colnames(dfm)=='TimeDate')]
  colnames(dfm) <- gsub("W", paste0(id, "_", "W"), colnames(dfm))
  dfm
}

#' Set start/end date
#'
#'These functions return a trimmed dfm dataframe defined by the start and end date
#' @param x A dfm dataframe.
#' @param date_ini The start date as MM/DD/YYYY.
#' @param date_end The end date as MM/DD/YYYY.
#' @name set.date
#' @export

Set.date <- function(x, date_ini, date_end) {
  df <- x[, -c(1,2)]
  df <- as.data.frame(df)
  col <- colnames(df)
  df$TimeDate <- paste(x$Date, x$Time, sep = " ")
  df$TimeDate <-
    parse_date_time(df$TimeDate, '%m-%d-%Y %I:%M:%S %p')
  Date <- as.Date(df$TimeDate)
  Time <- format(df$TimeDate, "%I:%M:%S %p")
  df <- df[, c('TimeDate', col)]
  df <- cbind(Date, Time, df)
  
  if (date_ini != date_end) {
    df <- df[(which(df$Date == date_ini)[1]):nrow(df), ]
    df <-
      df[1:(which(df$Date == date_end)[length(which(df$Date == date_end))]), ]
  }
  df
}

#' Calculate baseline
#'
#'Get baseline traces using Running Medians.
#' @param dfm A dfm dataframe.
#' @param window.interval Interval (in minutes) for the running median.
#' @export

Calculate.base <- function(dfm,
                           window.interval,
                           baseline.mod = c('Running Median', 'Asymmetric Least Squares'),
                           updateProgress = NULL) {
  window <- window.interval * 60 * 5
  newData <- dfm[, -c(1:3)]
  
  if (baseline.mod == 'Running Median') {
    if (window %% 2 == 0)
      window = window + 1
    
    for (i in 1:ncol(newData)) {
      tmp <- runmed(newData[, i], window)
      newData[, i] <- newData[, i] - tmp
      # If we were passed a progress update function, call it
      if (is.function(updateProgress)) {
        text <- print(paste("Calculating Baseline", colnames(newData)[i]))
        updateProgress(detail = text)
        updateProgress(value = i / ncol(newData))
      }
    }
    newData <- cbind(dfm[, 1:3], newData)
  }
  
  if (baseline.mod == 'Asymmetric Least Squares') {
    pldf <-  NULL
    for (i in 1:ncol(newData)) {
      # If we were passed a progress update function, call it
      if (is.function(updateProgress)) {
        text <- print(paste("Calculating Baseline", colnames(newData)[i]))
        updateProgress(detail = text)
        updateProgress(value = i / ncol(newData))
      }
      x <- newData[i]
      y <- t(x)
      p <- baseline.als(y, maxit = 5)
      pl <- p$corrected
      pl <- as.data.frame(t(pl))
      pldf <- c(pldf, pl)
    }
    newData <- cbind(dfm[, 1:3], pldf)
  }
  
  newData
}

#' Collapse time
#'
#'Get a dataframe with binned information in a desire time interval.
#' @param x A dfm file.
#' @param collapse_interval Time interval to use for binning
#' (e.g. "1 hour","30 min", etc.).
#' @export

Collap.time <-
  function(x,
           collapse_interval,
           bin_fun = c("Mean", "Sum")) {
    df <- x[,-(1:2)]
    df$TimeDate <- as_datetime(df$TimeDate)
    if (bin_fun == "Mean") {
      df <- df %>% as_tbl_time(index = TimeDate) %>%
        collapse_by(collapse_interval, side = "start", clean = T) %>%
        group_by(TimeDate) %>% summarise(across(everything(), mean))
    }
    else if (bin_fun == "Sum") {
      df <- df %>% as_tbl_time(index = TimeDate) %>%
        collapse_by(collapse_interval, side = "start", clean = T) %>%
        group_by(TimeDate) %>% summarise(across(everything(), sum))
    }
    Date <- as.Date(df$TimeDate)
    Time <- format(df$TimeDate, "%I:%M:%S %p")
    df <- cbind(Date, Time, df)
    df
    
  }



#' Graph FLIC data
#'
#'These functions return an interactive graph of FLIC data.
#' @param dfm A dfm dataframe or baselined dfm
#' @param Conditions String with Conditions info
#' @param Color String with Color info
#' @param ld Vertical line for days limits
#' @param mean If mean needs to be calculated FALSE by default
#' @export

Graph.flic <-
  function(dfm,
           Conditions = NULL,
           Color = NULL,
           ld = T,
           mean = FALSE,
           error=NULL) {
    e <- dfm[, -c(1, 2)]
    m2 <-
      reshape2::melt(
        e,
        id.vars = "TimeDate",
        variable.name = "Fly",
        value.name = "Signal"
      )
    if (mean == TRUE) {
      m2$Fly <- gsub('\\..*', "", m2$Fly)
      colnames(m2)[3] <- 'Sig'
      m2 <-
        m2 %>% group_by(TimeDate, Fly) %>% summarise(
          Signal = mean(Sig),
          sd = sd(Sig),
          sem = sd(Sig) / sqrt(n())
        )
    }
    if ((length(unique(m2$Fly)) > 1) & (is.null(Color) == T)) {
      gp <- ggplot(data = m2, aes(
        x = TimeDate,
        y = Signal,
        group = Fly,
        color = Fly
      )) +
        xlab("Time") +
        ylab("Signal") +
        theme_classic() +
        geom_line() +
        geom_point()
    }
    else{
      gp <- ggplot(data = m2, aes(x = TimeDate,
                                  y = Signal,
                                  color = Fly)) +
        xlab("Time") +
        ylab("Signal") +
        theme_classic() +
        geom_line() +
        geom_point() +
        scale_color_manual(values = Color, breaks = Conditions)
    }
    if (length(unique(dfm$Date)) > 1 & ld == T) {
      ld <- dfm[grepl('12:00:00 AM', dfm$Time), 'TimeDate']
      ld <- as.data.frame(ld)
      gp <- gp +
        geom_vline(data = ld,
                   aes(xintercept = as.numeric(ld)),
                   linetype = 'solid')
    }
    if ("sem" %in% colnames(m2) == T) {
      if(is.null(error)){
        gp <- gp + geom_errorbar(aes(ymin = Signal - sem, ymax = Signal + sem))
      }
      if(error == "SEM"){
        gp <- gp + geom_errorbar(aes(ymin = Signal - sem, ymax = Signal + sem))
      }
      else if(error == "SD"){
      gp <- gp + geom_errorbar(aes(ymin = Signal - sd, ymax = Signal + sd))
      }
      else if(error == "None"){
        gp <- gp 
      }
    }
    gp
  }


#' Graph LD boxes
#'
#'These function add a geom_layer showing dark perios
#' @param df A full day timebin 
#' @param on Lights ON
#' @param off lights OFF
#' @export

Geom_ld <- function(dfm,on,off){
  e <- dfm[, -c(1, 2)]
  m2 <-
    reshape2::melt(
      e,
      id.vars = "TimeDate",
      variable.name = "Fly",
      value.name = "Event"
    ) %>% mutate(Fly=gsub("\\..*","",Fly)) %>% 
    group_by(TimeDate,Fly) %>% 
    summarise(Signal=mean(Event),sd=sd(Event))
  
  onset = as.POSIXct(on,format='%H:%M',tz="UTC")
  offset = as.POSIXct(off,format='%H:%M',tz="UTC")
  midnight = as.POSIXct("00:00 AM",format='%H:%M',tz="UTC")
  
  
  df_h <- data.frame(xmin=c(midnight,offset),xmax=c(onset,last(m2$TimeDate)))
  
  y_min = floor(m2$Signal %>% min) 
  y_max = m2$Signal %>% max +  2*(m2$sd %>% max)
  
  
  
  
  gp=geom_rect(data=df_h, 
               aes(NULL,NULL,xmin = xmin, xmax = xmax), ymin = y_min, ymax = y_max,
               alpha=0.15,inherit.aes = F)
  
  gp
}


#' Lick events
#'
#' 'Set.Lick.Data' is a function that output licks in true feeding events
#' @param dfm A baselined unbinned dfm dataframe
#' @param Feed.min Lower threshold for feeding event
#' @param Feed.max Upper threshold for feeding event
#' @param Feeding.Minevents Minimum size of feeding event
#' @param Feeding.Event.Link.Gap Max gap between licks to count them as
#' part of one event
#' @export

Set.Lick.Data <-
  function(dfm,
           Feed.min = NULL,
           Feed.max = NULL,
           Feeding.Minevents = NULL,
           Feeding.Event.Link.Gap = NULL,
           updateProgress = NULL) {
    ## Get all possible feeding Licks
    data <- dfm[, 4:ncol(dfm)]
    
    Feeding.Licks.Min <- (data > Feed.min)
    
    Feeding.Licks.Max <- (data > Feed.max)
    
    Events <- Get.Surviving.Events(Feeding.Licks.Min,
                                   Feeding.Licks.Max,
                                   updateProgress)
    
    ## Now remove events that are too short
    Events[Events < Feeding.Minevents] <- 0
    
    ## Now expand the licks to TRUE/FALSE entries
    FeedingLicks <- Expand.Events(Events)
    
    ## Now Bridge sporadic lick events into single events.
    tmp <- Link.Events(FeedingLicks, Feeding.Event.Link.Gap)
    
    if (is.function(updateProgress)) {
      text <- 'Linking Events'
      updateProgress(message=text)
    }
    
    Events <- Get.Events(tmp,updateProgress)
    
    data.frame(dfm[1:3], Events)
    
  }


#' #' Feeding event,
#' #'
#' #' 'Get.Event.Data' is a function that gives the number of feeding events.
#' #' @param dfm A dfm with lick information
#' #' @export
#'
Get.Event.Data <-
  function(dfm) {
    data <- dfm[, 4:ncol(dfm)]
    coln <- colnames(dfm)
    rest <- NULL
    
    for (i in 1:ncol(data)) {
      tmp <- data[, i]
      res <- as.integer(tmp > 0)
      rest <- cbind(rest, res)
    }
    
    d <- data.frame(dfm[1:3], rest)
    colnames(d) <- coln
    d
  }


## This function takes 2 vectors, one with the events
## above a minimal threshold (minvec) and one that
## specifies events that pass a more stringent threshold (maxvec).
## Contiguous events are only kept if at least one
## value in the event, which is defined by minvec, is above
## the higher threshold, which is defined by max vec
## z <- c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE)
## zz <- c(FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE)

## Get.Surviving.Events(z,zz) -> (2 0 0 0 0 0 3 0 0)

Get.Surviving.Events <-
  function(minvec, maxvec, updateProgress = NULL) {
    tmp <- Get.Events(minvec, updateProgress)
    tmp <- as.data.frame(tmp)
    maxvec <- as.data.frame(maxvec)
    for (j in 1:ncol(tmp)) {
      result <- tmp[, j]
      m <- maxvec[, j]
      indices <- (1:length(result))[result > 0]
      for (i in indices) {
        tmp2 <- m[i:(i + (result[i] - 1))]
        if (sum(tmp2) == 0)
          result[i] <- 0
      }
      tmp[, j] <- result
    }
    tmp
  }

## These functions are helper functions for the basic calculations
# This function replaces continuing events with zero and make the first event of that
# episode equal to its duration.
## c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE) -> (2 0 0 0 1 0 3 0 0)
Get.Events <- function(z, updateProgress = NULL) {
  z <- as.data.frame(z)
  
  for (i in 1:ncol(z)) {
    if (is.function(updateProgress)) {
      text <- print(paste(i, "wells out of", ncol(z)))
      updateProgress(detail = text)
      updateProgress(value = i / ncol(z))
    }
    tmp <- as.data.frame(rle2(z[, i]))
    result <- c(-1)
    for (f in 1:length(tmp$length)) {
      if (tmp$value[f]) {
        tmp2 <- c(tmp$length[f], rep(0, tmp$length[f] - 1))
        result <- c(result, tmp2)
      }
      else {
        tmp2 <- c(rep(0, tmp$length[f]))
        result <- c(result, tmp2)
      }
    }
    z[, i] <- result[-1]
  }
  z
}

## This function is the reverse of Get.Events
## (2 0 0 0 1 0 3 0 0) -> c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE)

Expand.Events <- function(eventframe) {
  a <- eventframe
  for (j in 1:ncol(a)) {
    eventvec <- a[, j]
    result <- rep(FALSE, length(eventvec))
    indices <- (1:length(eventvec))[eventvec > 0]
    for (i in indices) {
      result[i:(i + eventvec[i] - 1)] <- TRUE
    }
    a[, j] <- result
  }
  a
}

## This function will take a TRUE/FALSE vector, assumed to be minthresholded lick data
## and it will "bridge" runs of FALSE of less than 'thresh' entries with TRUE
## e.g. c(TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE) -> c(TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE)
## with thresh <= 3

Link.Events <- function(a, thresh) {
  p <- a
  for (j in 1:ncol(p)) {
    z <- p[, j]
    tmp <- rle(z)
    result <- c(FALSE)
    for (i in 1:length(tmp$lengths)) {
      if (tmp$values[i]) {
        tmp2 <- rep(TRUE, tmp$lengths[i])
        result <- c(result, tmp2)
      }
      else {
        if (tmp$lengths[i] > thresh) {
          tmp2 <- rep(FALSE, tmp$lengths[i])
          result <- c(result, tmp2)
        }
        else {
          tmp2 <- rep(TRUE, tmp$lengths[i])
          result <- c(result, tmp2)
        }
      }
    }
    p[, j] <- result[-1]
  }
  p
}

#' Period statistics
#'
#' 'per.stat' is a function that uses 'ac_periodogram' from the
#' zeitgebr package to calculate the period and power for each individual.
#' @param x A dfm dataframe
#' @param bin_freq Frequency of data acquisition indicated by time.
#' By default '0.005 seconds'.
#' @export

Per.stat <- function(data,samp_freq, range) {
  data <- data[, -c(1:3)]
  period <- NULL
  for (i in 1:ncol(data)) {
    cn <- colnames(data[i])
    act <- ac_periodogram(data[,i], period_range = behavr::hours(range),
                          sampling_rate = 1 / samp_freq)
    res <- as.data.frame(c(cn, act))
    colnames(res)[1] <- "Fly"
    period <- rbind2(period, res)
    
  }
  
  period <- period[-1, ]
  period$period <- (period$period / 3600)
  period
}

