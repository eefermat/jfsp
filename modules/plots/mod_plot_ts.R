# Time series - annual
tsPlot <- function(type, limits){
  if(preventPlot()) return()
  if(input$jitter) pos <- position_jitter(width=0.2, height=0) else pos <- "identity"
  
  grp <- c("GBM", "RCP", "Model", "Region", "Var", "Vegetation")
  grp <- grp[grp %in% names(keep())]
  x <- "Year"
  statname <- stat()
  
  if(type=="annual-cumulative"){
    d_keep <- group_by_(keep(), .dots=grp) %>% mutate_(Cumulative_total=lazyeval::interp(~cumsum(x), x=as.name(stat())))
    statname <- "Cumulative_total"
  } else {
    d_keep <- keep()
  }
  
  d_source <- d()
  clrby <- colorby()
  pInteract <- plotInteraction()
  clrvec <- colorvec()
  fctby <- input$facetby
  fctscales <- input$facet_scales
  alpha <- input$alpha
  alphaHalf <- alpha/2
  
  g <- ggplot(data=d_source, aes_string(x, statname, colour=clrby))
  
  if(input$addLines) g <- g + geom_line(data=d_keep, aes_string(group=pInteract), alpha=alpha)
  
  if(type=="annual-raw") ts_brush <- input$plot1_brush else ts_brush <- input$plot2_brush
  g2 <- g
  g2 <- g2 + geom_point(data=d_keep, size=3, alpha=alpha, position=pos)
  if(type=="annual-raw") g2 <- g2 + geom_point(data=exclude(), size=3, shape=21, fill=NA, color="black", alpha=0.3)
  if(!is.null(ts_brush)){
    d_keep2 <- brushedPoints(d_keep, ts_brush)
    if(nrow(d_keep2)!=0){
      d_keep <- setdiff(d_keep, d_keep2)
    } else {
      d_keep2 <- d_keep
    }
    if(nrow(d_keep)!=0) g <- g + geom_point(data=d_keep, size=3, alpha=alphaHalf, position=pos)
    g <- g + geom_point(data=d_keep2, size=3, alpha=alpha, position=pos)
    if(type=="annual-raw") g <- g + geom_point(data=exclude(), size=3, shape=21, fill=NA, color="black", alpha=0.3)
  } else {
    g <- g2
  }
  
  g <- g + coord_cartesian(xlim=limits[[1]], ylim=limits[[2]]) + theme_bw(base_size=14)
  
  if(!is.null(clrvec)) g <- g + scale_colour_manual(values=clrvec, limits=levels(d_source[[clrby]]))
  if(fctby!="") g <- g + facet_wrap(as.formula(paste0("~", fctby)), scales=fctscales)
  g + plottheme
}
