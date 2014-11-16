#' fitpsy
#' @export

fitpsy <- function(d, x, pini, m = Inf, fun = cumnorm, invfun = cumnormq, 
                   ythre = (1 / m) + .5 * (1 - 1 / m), B = 1000) { 
  library(boot)
  
  cumnorm <- function(x,p,m) (1/m)+(1-1/m)*pnorm(x,p[1],p[2])
  cumnormq <- function(y,p,m) qnorm((y-(1/m))/(1-1/m),p[1],p[2])
  
  
  neglikelihood <- function(p, d, x, fun, m){
    pr <- fun(d[[x]], p, m)
    -sum(d$nyes * log(pr) + d$nno * log(1-pr))
  }
  
  
  # parameters
  estimpar <- function(d) {
    optim(pini, neglikelihood, d = d, x = x, fun = fun, m = m)$par
  }
  p <- estimpar(d)
  
  # thresholds
  estimthre <- function(d) {
    invfun(ythre, estimpar(d), m)
  }
  thre <- estimthre(d)
  # boot
  mle<- fun(d[[x]], p, m)
  sim_d <- function(f, mle) {
    g <- d
    g$nyes <- rbinom(nrow(d), d$n, mle)
    g$nno <- g$n - g$nyes
    g$y <- g$nyes / g$n
    g
  }
  bootstr <- boot(d, estimthre, R = B, sim = 'parametric', 
                  ran.gen = sim_d, mle = mle)
  ci <- boot.ci(bootstr, type = 'perc')$percent
  
  data.frame(x = thre, xmin = ci[[4]], xmax = ci[[5]])
}
