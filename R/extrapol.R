#' extrapol
#' @export
extrapol <- function(d, x, y, xout) {
  extr <- approx(d[[x]],d[[y]],xout)
  extr <- data.frame(extr)
  extr <- extr[complete.cases(extr$y),]
  names(extr) <- c(x,y)
  extr
}