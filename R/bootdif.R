#' bootdif
#' @export
#'
bootdif <- function(d, y, sample, prob, onlySigni = FALSE) {
  library(dplyr)
  library(stringr)

  d$y <- d[[y]]
  d$sample <- d[[sample]]

  d <- d %>%
    regroup(lapply(sample, as.symbol)) %>%
    summarise(dif = diff(y) ) %>%
    summarise(ymin = quantile(dif, prob),
              ymax = quantile(dif, 1-prob),
              signi = sign(ymin)*sign(ymax))

  if (onlySigni) {
    d <- d %>% filter(signi ==1)
  }

  names(d)[names(d)=='y'] <- y
  names(d)[names(d)=='ymin'] <- paste0(y,'min')
  names(d)[names(d)=='ymax'] <- paste0(y,'max')
  d
}
