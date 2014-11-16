#' plotThreshold
#' @export
plotThreshold <- function(d , dDif = NULL, dDif2 = NULL, 
                          x, y, ymin, ymax, cond, p, all = T) {
  library(ggplot2)
  library(gridExtra)
  library(dplyr)
  
  d$durationTestFig <- d$durationTest
  d$durationTestFig <- factor(d$durationTestFig,
                              levels = c(56, 112),
                              labels = c('Short', 'Long'))
  
  d$numberFig <- d$number
  d$numberFig <- factor(d$numberFig,
                        levels = c(1, 2),
                        labels = c(p$oneObjectLabel, p$twoObjectsLabel))
  
  d$radiusFig <- d$radius
  d$radiusFig <- factor(d$radiusFig,
                        levels = c(2, 4),
                        labels = c('2 deg', '4 deg'))
  
  subject <- unique(d$subject)

  d$x <- d[[x]]
  d$y <- d[[y]]
  d$ymin <- d[[ymin]]
  d$ymax <- d[[ymax]]
  

  
  
  subj <- as.character(unique(d$subject))
  
  if (!is.null(dDif)) {
    dDif <- dDif %>% filter(subject == subj)
    dDif$x <- dDif[[x]]
    
    dDif$durationTestFig <- dDif$durationTest
    dDif$durationTestFig <- factor(dDif$durationTestFig,
                                   levels = c(56, 112),
                                   labels = c('Short', 'Long'))
    if(cond == 'radius') {
      dDif$numberFig <- dDif$number
      dDif$numberFig <- factor(dDif$numberFig,
                            levels = c(1, 2),
                            labels = c(p$oneObjectLabel, p$twoObjectsLabel))
    }
    if(cond == 'number') {
      dDif$radiusFig <- dDif$radius
      dDif$radiusFig <- factor(dDif$radiusFig,
                            levels = c(2, 4),
                            labels = c('2 deg', '4 deg'))
    }  
    
  }
  
  if (!is.null(dDif2)) {
    dDif2 <- dDif2 %>% filter(subject == subj)
    dDif2$x <- dDif2[[x]]
    
    dDif2$durationTestFig <- dDif2$durationTest
    dDif2$durationTestFig <- factor(dDif2$durationTestFig,
                                    levels = c(56, 112),
                                    labels = c('Short', 'Long'))
    
    if(cond == 'radius') {
      dDif2$numberFig <- dDif2$number
      dDif2$numberFig <- factor(dDif2$numberFig,
                               levels = c(1, 2),
                               labels = c(p$oneObjectLabel, p$twoObjectsLabel))
    }
    
    if(cond == 'number') {
      dDif2$radiusFig <- dDif2$radius
      dDif2$radiusFig <- factor(dDif2$radiusFig,
                               levels = c(2, 4),
                               labels = c('2 deg', '4 deg'))
    }
  }
  
  breakingY <- c(.8,1,1.2)
  breakingYLabels <- c(.8,1,1.2)
  if (x == 'speed') {
    xlabel <- p$speedLabel
    if (all) {
      breaking <- p$breaksSpeed
      breakingLabels <- p$breaksSpeedLabel
    }
    else {
      breaking <- p$breaksSpeedInd
      breakingLabels <- p$breaksSpeedLabelInd
    }
  }
  if (x == 'freq') {
    xlabel <- p$freqLabel
    if (all) {
      breaking <- p$breaksFreq
      breakingLabels <- p$breaksFreqLabel
    }
    else {
      breaking <- p$breaksFreqInd
      breakingLabels <- p$breaksFreqLabelInd
    }
  }
  if (x == 'tf') {
    xlabel <- p$tfLabel
    breaking <- p$breaksFreq
    breakingLabels <- p$breaksFreqLabel
  }
  if (x == 'angle') {
    xlabel <- p$angleLabel
    breaking <- p$breaksFreq
    breakingLabels <- p$breaksFreqLabel
  }
  if (x == 'speedMatch') {
    xlabel <- p$speedMatchLabel
    if (all) {
      breaking <- p$breaksSpeed
      breakingLabels <- p$breaksSpeedLabel
    }
    else {
      breaking <- p$breaksSpeedInd
      breakingLabels <- p$breaksSpeedLabelInd
    }
  }
  
  if (x == 'freqMatch') {
    xlabel <- p$freqMatchLabel2
    if (all) {
      breaking <- p$breaksFreq
      breakingLabels <- p$breaksFreqLabel
    }
    else {
      breaking <- p$breaksFreqInd
      breakingLabels <- p$breaksFreqLabelInd
    }
  }
  ylabel <- p$biasLabel
  
  if (all) {
    ylimits <- c(.7, 1.3)
    posSigni1 <- 1.25
    posSigni2 <- posSigni1 + .03
  }
  else {
    ylimits <- c(.55, 1.5)
    posSigni1 <- 1.35
    posSigni2 <- posSigni1 + .05
  }
  
  observer <- paste('Obsever',unique(d$subject))
  plot <- ggplot(data=d) + 
    scale_x_log10(breaks = breaking, labels = breakingLabels) +
    scale_y_continuous(breaks = breakingY, labels = breakingYLabels) +
    coord_cartesian(ylim=ylimits) +
    xlab(xlabel)+ylab(ylabel)+
    ggtitle(observer) +
    theme(legend.position = 'top',
          legend.key = element_blank())
      
  if (cond == 'numberradius') {
    plot <- plot +
      facet_grid(durationTestFig~.)+
      geom_linerange(aes(x=x,ymin=ymin,ymax=ymax,color=factor(radiusFig))) +
      geom_point(size = p$sizePoint, aes(x=x,y=y,color=factor(radiusFig),
                                         shape=factor(radiusFig))) +
      geom_line(size=p$sizeLine, aes(x=x,y=y,color=factor(radiusFig),
                                lty=factor(number)))+
      labs(color = p$radiusLabel, lty = p$numberLabel)
  }
  if (cond == 'radius') {
    if (all) {
      plot <- plot +
        geom_ribbon(alpha = p$alphaRib, 
                    aes(x=x,ymin=ymin,ymax=ymax, fill=factor(radiusFig))) 
    }
    plot <- plot +
      facet_grid(durationTestFig~numberFig)+
      geom_point(size = p$sizePoint, aes(x=x,y=y,color=factor(radiusFig),
                                         shape=factor(radiusFig))) +
      geom_line(size=p$sizeLine, aes(x=x,y=y,color=factor(radiusFig)))+
      labs(color = p$radiusLabel, fill = p$radiusLabel, shape = p$radiusLabel)
    
    if (!is.null(dDif) & length(dDif$x != 0)) {
      plot <- plot + 
        geom_point(data = dDif, size = p$sizeSigni, y = posSigni1,
                   aes(x = x), color = 'black',shape='*')
    }
    
    if (!is.null(dDif2)& length(dDif2$x != 0)) {
      plot <- plot + 
        geom_point(data = dDif2, size = p$sizeSigni, y = posSigni2,
                   aes(x = x), color = 'black',shape='*')
    }
    
 }   

  if (cond == 'number') {
    if (all) {
      plot <- plot +
        geom_ribbon(alpha = p$alphaRib, 
                    aes(x=x,ymin=ymin,ymax=ymax, fill=factor(numberFig)))
    }
    plot <- plot +
      facet_grid(durationTestFig~radiusFig)+
      geom_point(size = p$sizePoint, aes(x=x,y=y,color=factor(numberFig),
                                         shape=factor(numberFig))) +
      geom_line(size=p$sizeLine, aes(x=x,y=y,color=factor(numberFig)))+
      scale_color_manual(values=c(3,4)) +
      scale_fill_manual(values=c(3,4)) +
      labs(color = p$numberLabel, fill = p$numberLabel, shape = p$numberLabel)
    
    if (!is.null(dDif)) {
      plot <- plot + 
        geom_point(data = dDif, size = p$sizeSigni, y = posSigni1,
                   aes(x = x), color = 'black',shape='*') 
    }
    
    if (!is.null(dDif2)) {
      plot <- plot + 
        geom_point(data = dDif2, size = p$sizeSigni, y = posSigni2,
                   aes(x = x), color = 'black',shape='*')
    }
  } 


  plot
}

