#' themedurspeed
#' @export
themedurspeed <- function(p) {
  library('RColorBrewer')

  theme_set(theme_bw(8))
  theme_update(panel.grid=element_blank(),
               panel.border=element_blank(),
               legend.key.size = unit(.5, units = 'line'),
               strip.background=element_blank(),
               plot.title = element_text(size=6),
               axis.ticks=element_line(size=p$sizeLine),
               axis.line=element_line(size=p$sizeLineAxis))
  
}