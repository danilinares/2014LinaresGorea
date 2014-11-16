#'overlaping
#'@export
overlaping <- function(d, v, var){
  d$v <- d[[v]]  
  dRange <- d %>% regroup(lapply(var, as.symbol)) %>%
    summarise(vmin = min(v), vmax = max(v))
  
  d <- d %>%
    filter(v >= max(dRange$vmin), v <= min(dRange$vmax))
  d$v <- NULL
  d
}