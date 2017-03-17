# Time series - annual
tsPlot <- function(type, limits){
  if(preventPlot()) return()
  pos <- .getPosition(input$jitter, colorby())
  grp <- c("RCP", "Model", "Region", "Var", "Vegetation")
  grp <- grp[grp %in% names(keep())]
  x <- "Year"
  statname <- stat()
  lhs <- paste0("italic(hat(y)[", strsplit(variable(), " ")[[1]][2], "])~`=`~")
  rhs <- "~italic(Year)"
  alpha <- input$alpha
  reg <- input$addReg
  regpos <- strsplit(tolower(input$regpos), " ")[[1]]
  
  if(type=="cumulative"){
    d_keep <- group_by_(keep(), .dots=grp) %>%
      mutate_(Cumulative_total=lazyeval::interp(~cumsum(x), x=as.name(stat())))
    statname <- "Cumulative_total"
    ylab <- axislab2()
  } else {
    d_keep <- keep()
    ylab <- axislab()
  }
  
  g <- ggplot(data=d(), aes_string(x, statname, colour=colorby()))
  if(input$addLines) g <- g + geom_line(data=d_keep, aes_string(group=plotInteraction()), alpha=alpha)
  if(type=="raw") ts_brush <- input$plot1_brush else ts_brush <- input$plot2_brush
  g2 <- g
  g2 <- g2 + geom_point(data=d_keep, size=3, alpha=alpha, position=pos)
  if(type=="raw") g2 <- g2 + geom_point(data=exclude(), size=3, shape=21, fill=NA, color="black", alpha=alpha/2)
  if(reg){
    g2 <- if(is.null(colorby())) g2 + geom_smooth(data=d_keep, method='lm', se=FALSE, colour="black") else
      g2 + geom_smooth(data=d_keep, method='lm', se=FALSE)
    g2 <- g2 + stat_poly_eq(data=d_keep, formula=y ~ x, eq.with.lhs=lhs, eq.x.rhs=rhs,
      label.x.npc=regpos[2], label.y.npc=regpos[1],
      aes(label=paste(..eq.label.., ..rr.label.., sep = "~~~")), parse=TRUE, size=5)
  }
  if(!is.null(ts_brush)){
    d_keep2 <- brushedPoints(d_keep, ts_brush)
    if(nrow(d_keep2)!=0){
      d_keep <- setdiff(d_keep, d_keep2)
    } else {
      d_keep2 <- d_keep
    }
    if(nrow(d_keep)!=0) g <- g + geom_point(data=d_keep, size=3, alpha=alpha/2, position=pos)
    g <- g + geom_point(data=d_keep2, size=3, alpha=alpha, position=pos)
    if(type=="raw") g <- g + geom_point(data=exclude(), size=3, shape=21, fill=NA, color="black", alpha=alpha/2)
    if(reg){
      g <- if(is.null(colorby())) g + geom_smooth(data=d_keep2, method='lm', se=FALSE, colour="black") else
        g + geom_smooth(data=d_keep2, method='lm', se=FALSE)
      g <- g + stat_poly_eq(data=d_keep2, formula=y ~ x, eq.with.lhs=lhs, eq.x.rhs=rhs,
        label.x.npc=regpos[2], label.y.npc=regpos[1],
        aes(label=paste(..eq.label.., ..rr.label.., sep = "~~~")), parse=TRUE, size=5)
    }
  } else {
    g <- g2
  }
  g <- g + coord_cartesian(xlim=limits[[1]], ylim=limits[[2]]) + theme_bw(base_size=14)
  g <- .colorFacet(g, d(), colorby(), colorvec(), input$facetby, input$facet_scales)
  g + plottheme + labs(y=ylab)
}
