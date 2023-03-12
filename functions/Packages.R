lib <- .libPaths()[1]
required.packages <- c(
  "shinyTime",
  "shinythemes",
  "shinyWidgets",
  "shinyjs",
  "data.table",
  "dplyr",
  "reshape2",
  "ggplot2",
  "plotly",
  "zeitgebr",
  "purrr",
  "lubridate",
  "tibbletime",
  "colourpicker",
  "ggpubr",
  "baseline",
  "DT",
  "accelerometry"
)

i1 <- !(required.packages %in% row.names(installed.packages()))
if (any(i1)) {
  Ans <-
    readline(
      prompt = paste(
        "The packages",
        print(list(required.packages[i1])),
        "are missing. Do you want to install them? (y/n)",
        sep = " "
      )
    )
  if (Ans == 'y' || Ans == 'yes') {
    install.packages(required.packages[i1],
                     dependencies = TRUE,
                     lib = lib)
    
  }
  else if (Ans == 'n' || Ans == 'no') {
    stop(paste(
      "To use CRUMB, please install",
      list(required.packages[i1]),
      sep = " "
    ))
  }
  else
    (
      stop("Please enter a valid (yes or no) answer.")
    )
}
lapply(required.packages, require, character.only = TRUE)
if (exists('Ans')) {
  rm('Ans')
}
if(sum(i1)==0){
  rm(list=ls())
}
