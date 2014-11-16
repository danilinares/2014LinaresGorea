plottingDurAll <- function(d1, d2, p) {
  av_l3 <- .15
  av_l1 <- .46 #.44
  av_l2 <- 1-av_l3-av_l1
  
  p_dif_extr_all_fig_a <- d1 +
    theme(strip.text.y=element_blank(),
          plot.margin=unit(c(p$av_marg_sup,0.5,.5, p$av_marg_left), 'line'),
          plot.title=element_blank(),
          legend.direction='vertical')
  
  p_dif_freq_extr_all_fig_a <- d2 +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.line.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin=unit(c(p$av_marg_sup,.5,.5,-1), 'line'),
          plot.title=element_blank(),
          legend.direction='vertical')
  
  legend_speed_freq <- g_legend(p_dif_extr_all_fig_a)
  p_dif_extr_all_no_legend <- p_dif_extr_all_fig_a + theme(legend.position='none')
  p_dif_freq_extr_all_no_legend <- p_dif_freq_extr_all_fig_a + 
    theme(legend.position='none')
  
  p_dif_extr_all_no_legend <- arrangeGrob(p_dif_extr_all_no_legend,main=tG('          A           B'))
  p_dif_freq_extr_all_no_legend <- arrangeGrob(p_dif_freq_extr_all_no_legend,main=tG('C           D'))
  
  pa<-arrangeGrob(p_dif_extr_all_no_legend, 
                  p_dif_freq_extr_all_no_legend,
                  legend_speed_freq,
                  ncol=3,widths=c(av_l1,av_l2,av_l3))
  
  
  pa
  
}