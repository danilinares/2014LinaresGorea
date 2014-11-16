#' bootstrappinggeom
#' @export

bootstrappinggeom <- function(d, x = 'x', B = 1000 , name = 'x') {
  geomean <- function(f,i,x) {
    z <- f[[x]]
    exp(mean(log(z[i])))
  }
  bt <- boot(d, geomean,B, x=x)
  g <- data.frame(x = bt$t0, 
                  xmin = quantile(bt$t,.025), 
                  xmax = quantile(bt$t,0.975))
  names(g) <- c(name, paste0(name,'Min'), paste0(name,'Max'))
  g
}



