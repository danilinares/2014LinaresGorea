#' curvepsy
#' @export
curvepsy <- function(d, x, pini,  m=Inf, fun = cumnorm,
                     xmin = min(d[[x]]), xmax = max(d[[x]])) { 
  
  cumnorm <- function(x, p, m) (1 / m)+(1 - 1 / m) * pnorm(x, p[1], p[2])
  
  neglikelihood <- function(p, d, x, fun, m){
    pr <- fun(d[[x]], p, m)
    -sum(d$nyes * log(pr) + d$nno * log(1-pr))
  }
  
  p <- optim(pini, neglikelihood, d = d, x = x, fun = cumnorm, m = m)$par
  xseq <- seq(xmin, xmax, len = 100)
  yseq <- fun(xseq, p, m)
  
  data.frame(x = xseq, y = yseq)
}