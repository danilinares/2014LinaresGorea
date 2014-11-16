#' bootstrapping
#' @export
bootstrapping <- function(d, x = 'x', B = 1000 , name = 'x') {
  library('Hmisc')
  b <- smean.cl.boot(d[[x]], B = B)
  d <- data.frame(x = b[[1]], xmin = b[[2]], xmax = b[[3]])
  names(d) <- c(name, paste0(name,'Min'), paste0(name,'Max'))
  d
}

