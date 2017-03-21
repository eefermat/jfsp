# Distributions - densities and histograms
distPlot <- function(type, limits){
  if(is.null(colorby())) clr <- "white" else clr <- "black"
  ylab <- paste(yrs()[1], "-", yrs()[2], "density")
  
  div_data <- function(x, y, stat, area.vars=c("Burn Area", "Fire Size", "Vegetated Area")){
    mutate_(x, Converted=lazyeval::interp(
      ~ifelse(Var %in% area.vars, x/y, x), x=as.name(stat))) %>%
      select_(.dots=paste0("-", stat)) %>% rename_(.dots=setNames("Converted", stat))
  }
  
  d1 <- d()
  if(axis_scale()!=1) d1 <- div_data(d1, axis_scale(), stat())
  
  g <- ggplot(data=d1, aes_string(stat(), group=plotInteraction()))
  if(type=="density"){
    g <- g + geom_line(aes_string(colour=colorby()), stat="density", alpha=input$alpha)
  } else {
    g <- g + geom_histogram(aes_string(fill=colorby()), colour=clr, position="dodge", alpha=input$alpha, bins=input$bins)
  }
  if(!is.null(limits[[1]]) & !is.null(limits[[2]])){
    if(input$zoom=="Zoom only") g <- g + coord_cartesian(xlim=limits[[1]])
    if(input$zoom=="Subset data") g <- g + xlim(limits[[1]])
  }
  g <- g + theme_bw(base_size=14) #+ scale_y_continuous(expand = c(0, 0.5))
  g <- .colorFacet(g, d1, colorby(), colorvec(), input$facetby, input$facet_scales)
  g + plottheme + labs(x=axislab(), y=ylab)
}
