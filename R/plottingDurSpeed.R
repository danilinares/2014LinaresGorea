plottingDurSpeed <- function(d1, d2, d3, p) {
  
  av_l4 <- .15
  av_l1 <- .35
  av_l2 <- .25
  av_l3 <- 1 - av_l4 - av_l1 - av_l2
  
  p_dif_extr_all_fig_a <- d1 +# ggtitle (' ')+
    theme(strip.text.y=element_blank(),
          plot.margin=unit(c(p$av_marg_sup,0.4,.5, 0), 'line'),
          legend.direction='vertical')
  
  p_dif_freq_extr_all_fig_a <- d2 + # ggtitle (' ')+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.line.y=element_blank(),
          axis.ticks.y=element_blank(),
          strip.text.y=element_blank(),
          plot.margin=unit(c(p$av_marg_sup,0.4,.5, -1), 'line'),
          legend.direction='vertical')
  
  p_dif_speed_match_extr_all_fig_a <- d3 + 
    xlab(p$freqLabel2) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.line.y=element_blank(),
          axis.ticks.y=element_blank(),
          
          plot.margin=unit(c(p$av_marg_sup,-.2,.5,-1), 'line'),
          legend.direction='vertical')
  
  legend_speed_freq <- g_legend(p_dif_extr_all_fig_a)
  p_dif_extr_all_no_legend <- p_dif_extr_all_fig_a + theme(legend.position='none')
  p_dif_freq_extr_all_no_legend <- p_dif_freq_extr_all_fig_a + 
    theme(legend.position='none')
  p_dif_speed_match_extr_all_fig_a_no_legend <- p_dif_speed_match_extr_all_fig_a + 
    theme(legend.position=c(.6,.5),
          legend.direction='horizontal',
          legend.background = element_rect(fill=alpha('black', 0)),
          legend.title=element_blank(),
          legend.key=element_rect(size=.1)) 
  
  
  p_dif_extr_all_no_legend <- arrangeGrob(p_dif_extr_all_no_legend,main=tG('          A'))
  p_dif_freq_extr_all_no_legend <- arrangeGrob(p_dif_freq_extr_all_no_legend,main=tG('B'))
  p_dif_speed_match_extr_all_fig_a_no_legend <- arrangeGrob(p_dif_speed_match_extr_all_fig_a_no_legend,main=tG('C'))
  
  
  
  pa<-arrangeGrob(p_dif_extr_all_no_legend, 
                  p_dif_freq_extr_all_no_legend,
                  p_dif_speed_match_extr_all_fig_a_no_legend,
                  ncol=3,widths=c(av_l1,av_l2,av_l3))
  pa
  
}
