extrapolSpeedMatch <- function(d, x, y, dSpeed) {
  library(dplyr)
  dSub <- dSpeed %>% 
    filter(subject == as.character(unique(d$subject)),
           number == as.character(unique(d$number)))
  extr <- approx(d[[x]],d[[y]],dSub[[x]])
  extr <- data.frame(extr)
  extr <- extr[complete.cases(extr$y),]
  names(extr) <- c(x,y)
  extr
  
}