#' fitpsyboot
#' @export
fitpsyboot <- function(d, x, fun = cumnorm, invfun = cumnormq, 
                       ythre, m = 2, B=1000, p) { 
  library(boot)
  
  cumnorm <- function(x,p,m) (1/m)+(1-1/m)*pnorm(x,p[1],p[2])
  cumnormq<-function(y,p,m) qnorm((y-(1/m))/(1-1/m),p[1],p[2])
  
  
  neglikelihood <- function(p, d, x, fun, m){
    pr <- fun(d[[x]], p, m)
    -sum(d$nyes * log(pr) + d$nno * log(1-pr))
  }
  
  
  # parameters
  estimpar <- function(d) {
    optim(p, neglikelihood, d = d, x = x, fun = fun, m = m)$par
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
  xsample <- bootstr$t
  
  data.frame(x = thre, sample = 1:length(xsample), xsample)
}


