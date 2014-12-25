source('libsAndMore.R')


### Figure  radius ####
plotingSpeedFreqAll <- plottingDurAll(plotThresholdsSpeedRadiusAll$plot[[1]],
                                      plotThresholdsFreqRadiusAll$plot[[1]], p)
ggsave('figures/Fig2.pdf', plotingSpeedFreqAll, 
       width = p$sizeFig1column, height = p$heightFig2)

### Figure speed match ####
pSpeedMatchSpeedRadiusDifLegend <- g_legend(pSpeedMatchSpeedRadiusDif)
pSpeedMatchSpeedRadiusDifNoLegend <- pSpeedMatchSpeedRadiusDif +
  theme(legend.position = 'none',
        plot.margin=unit(c(p$av_marg_sup,p$av_marg_right,
                           p$av_marg_bottom, p$av_marg_left), 'line'))

pSpeedMatchSpeedNumberDifLegend <-g_legend(pSpeedMatchSpeedNumberDif)
pSpeedMatchSpeedNumberDifNoLegend <- pSpeedMatchSpeedNumberDif +
  theme(legend.position = 'none',
        plot.margin=unit(c(p$av_marg_sup,p$av_marg_right,
                           p$av_marg_bottom, p$av_marg_left), 'line'))

pSpeedMatch <- arrangeGrob(
  arrangeGrob(pSpeedMatchSpeedRadiusDifNoLegend,
              pSpeedMatchSpeedRadiusDifLegend, 
              main = tG2('A'),ncol =2, widths = c(.8,.2)),
  arrangeGrob(pSpeedMatchSpeedNumberDifNoLegend,
              pSpeedMatchSpeedNumberDifLegend, 
              main = tG2('B'), ncol =2, widths = c(.8,.2))
)

ggsave('figures/Fig3.pdf', pSpeedMatch, 
       width = p$sizeFig1column, height = p$heightFigSpeedMatch)

### Figure perceived duration speed match ####
pDurSpeedMatchAD <- plottingDurSpeed(plotThresholdsSpeedRadiusSpeedMatch$plot[[1]],
                                     plotThresholdsSpeedMatchRadiusSpeedMatch$plot[[1]],
                                     plotThresholdsFreqRadiusSpeedMatch$plot[[1]], p)

pDurSpeedMatchMA <- plottingDurSpeed(plotThresholdsSpeedRadiusSpeedMatch$plot[[2]],
                                     plotThresholdsSpeedMatchRadiusSpeedMatch$plot[[2]],
                                     plotThresholdsFreqRadiusSpeedMatch$plot[[2]], p)

pDurSpeedMatchMP <- plottingDurSpeed(plotThresholdsSpeedRadiusSpeedMatch$plot[[3]],
                                     plotThresholdsSpeedMatchRadiusSpeedMatch$plot[[3]],
                                     plotThresholdsFreqRadiusSpeedMatch$plot[[3]], p)

pDurSpeedMatch <- arrangeGrob(
  pDurSpeedMatchAD,
  pDurSpeedMatchMA,
  pDurSpeedMatchMP, ncol=1)

ggsave('figures/Fig4.pdf', pDurSpeedMatch, 
       width = p$sizeFig1column, height = p$heightFig3)

### Figure freq match ####
pFreqMatchFreqRadiusDifLegend <- g_legend(pFreqMatchFreqRadiusDif)
pFreqMatchFreqRadiusDifNoLegend <- pFreqMatchFreqRadiusDif +
  theme(legend.position = 'none',
        plot.margin=unit(c(p$av_marg_sup,p$av_marg_right,
                           p$av_marg_bottom, p$av_marg_left), 'line'))

pFreqMatchFreqNumberDifLegend <-g_legend(pFreqMatchFreqNumberDif)
pFreqMatchFreqNumberDifNoLegend <- pFreqMatchFreqNumberDif +
  theme(legend.position = 'none',
        plot.margin=unit(c(p$av_marg_sup,p$av_marg_right,
                           p$av_marg_bottom, p$av_marg_left), 'line'))

pFreqMatch <- arrangeGrob(
  arrangeGrob(pFreqMatchFreqRadiusDifNoLegend,
              pFreqMatchFreqRadiusDifLegend, 
              main = tG2('A'),ncol =2, widths = c(.8,.2)),
  arrangeGrob(pFreqMatchFreqNumberDifNoLegend,
              pFreqMatchFreqNumberDifLegend, 
              main = tG2('B'), ncol =2, widths = c(.8,.2))
)

ggsave('figures/Fig5.pdf', pFreqMatch, 
       width = p$sizeFig1column, height = p$heightFigSpeedMatch)

### Figure perceived duration freq match ####
pDurFreqMatchAB <- plottingDurSpeed(plotThresholdsSpeedRadiusFreqFreqMatch$plot[[1]],
                                    plotThresholdsFreqMatchRadiusFreqFreqMatch$plot[[1]],
                                     plotThresholdsFreqRadiusFreqFreqMatch$plot[[1]], p)

pDurFreqMatchFE <- plottingDurSpeed(plotThresholdsSpeedRadiusFreqFreqMatch$plot[[2]],
                                    plotThresholdsFreqMatchRadiusFreqFreqMatch$plot[[2]],
                                    plotThresholdsFreqRadiusFreqFreqMatch$plot[[2]], p)

pDurFreqMatchMC <- plottingDurSpeed(plotThresholdsSpeedRadiusFreqFreqMatch$plot[[3]],
                                    plotThresholdsFreqMatchRadiusFreqFreqMatch$plot[[3]],
                                    plotThresholdsFreqRadiusFreqFreqMatch$plot[[3]], p)

pDurFreqMatchSY <- plottingDurSpeed(plotThresholdsSpeedRadiusFreqFreqMatch$plot[[4]],
                                    plotThresholdsFreqMatchRadiusFreqFreqMatch$plot[[4]],
                                    plotThresholdsFreqRadiusFreqFreqMatch$plot[[4]], p)

pDurFreqMatch <- arrangeGrob(
  pDurFreqMatchAB,
  pDurFreqMatchFE,
  pDurFreqMatchMC, 
  pDurFreqMatchSY,ncol=1)

ggsave('figures/Fig6.pdf', pDurFreqMatch, 
       width = p$sizeFig1column, height = p$heightFig4)


### Figure number ####
plotingFreqTfAll <- plottingDurAll(plotThresholdsFreqNumberAll$plot[[1]],
                                   plotThresholdsTfNumberAll$plot[[1]], p)

ggsave('figures/Fig7.pdf', plotingFreqTfAll, 
       width = p$sizeFig1column, height = p$heightFig2)
