# Distributions - densities and histograms
distPlot <- function(type, limits){
  if(is.null(colorby())) clr <- "white" else clr <- "black"
  g <- ggplot(data=d(), aes_string(stat(), group=plotInteraction()))
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
  g <- .colorFacet(g, d(), colorby(), colorvec(), input$facetby, input$facet_scales)
  g + plottheme
}
