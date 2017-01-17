# Distributions - densities and histograms
distPlot <- function(type, limits){
  if(is.null(den_colorby())) clr <- "white" else clr <- "black"
  g <- ggplot(data=d_den(), aes_string(stat(), group=den_plotInteraction()))
  if(type=="density"){
    g <- g + geom_line(aes_string(colour=den_colorby()), stat="density", alpha=input$den_alpha)
  } else {
    g <- g + geom_histogram(aes_string(fill=den_colorby()), colour=clr, position="dodge", alpha=input$den_alpha, bins=input$den_bins)
  }
  if(!is.null(limits[[1]]) & !is.null(limits[[2]])){
    if(input$den_zoom=="Zoom only") g <- g + coord_cartesian(xlim=limits[[1]])
    if(input$den_zoom=="Subset data") g <- g + xlim(limits[[1]])
  }
  g <- g + theme_bw(base_size=14) #+ scale_y_continuous(expand = c(0, 0.5))
  
  if(!is.null(den_colorvec()))
    g <- g + scale_colour_manual(values=den_colorvec(), limits=levels(d_den()[[den_colorby()]])) +
    scale_fill_manual(values=den_colorvec(), limits=levels(d_den()[[den_colorby()]]))
  if(input$den_facetby!="")
    g <- g + facet_wrap(as.formula(paste0("~", input$den_facetby)), scales=input$den_facet_scales)
  g + plottheme
}
