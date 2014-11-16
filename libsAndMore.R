library(dplyr)
library(knitr)
library(ggplot2)
library(gridExtra)
library(png)
library(boot)
library(scales)

sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

source('parameters.R')
sourceDir('R/')

themedurspeed(p)
