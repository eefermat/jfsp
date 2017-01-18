# Time series - decadal
decPlot <- function(type, limits){
  if(preventPlot()) return()
  pos <- .getPosition(input$jitter, colorby(), dodgeable=TRUE)
  grp <- c("GBM", "RCP", "Model", "Region", "Var", "Vegetation")
  statname <- stat()
  
  if(type=="barplot"){
    d_keep <- keep_dec()
    statname <- "Decadal_mean"
  } else {
    d_keep <- keep()
  }
  x <- "Decade"

  d_source <- d_keep
  clrby <- colorby()
  pInteract <- plotInteraction()
  clrvec <- colorvec()
  fctby <- input$facetby
  fctscales <- input$facet_scales
  alpha <- input$alpha
  alphaHalf <- alpha/2
  barpos <- input$barpos
  
  g2 <- g <- ggplot(data=d_source, aes_string(x, statname, colour=clrby, fill=clrby))
  
  if(type=="boxplot"){
    doBox <- input$bptype %in% c("Box plot", "Overlay")
    doStrip <- input$bptype %in% c("Strip chart", "Overlay")
    if(doStrip) shp.out <- NA else shp.out <- 21
    if(doBox){
      if(is.null(clrby)){
        g2 <- g2 + geom_boxplot(fill="gray", colour="black", alpha=alpha, outlier.shape=shp.out)
      } else {
        g2 <- g2 + geom_boxplot(colour="black", alpha=alpha, outlier.shape=shp.out)
      }
    }
    if(doStrip){
      if(is.null(clrby)){
        g2 <- g2 + geom_point(shape=21, fill="black", colour="black", position=pos, alpha=alpha)
      } else {
        g2 <- g2 + geom_point(shape=21, colour="black", position=pos, alpha=alpha)
      }
    }
    if(nrow(keep())==length(rv_plots$keeprows)){
      if(!is.null(rv_plots$holdBrush) | !is.null(rv_plots$holdClick)){
        if(any(rv_plots$keeprows)){
          d_keep2 <- d_keep[rv_plots$keeprows,]
          d_keep <- setdiff(d_keep, d_keep2)
        } else {
          d_keep2 <- d_keep
        }
        if(nrow(d_keep)!=0){
          if(doBox){
            if(is.null(clrby)){
              g <- g + geom_boxplot(data=d_keep, fill="gray", colour="black", alpha=alphaHalf, outlier.shape=shp.out)
            } else {
              g <- g + geom_boxplot(data=d_keep, colour="black", alpha=alphaHalf, outlier.shape=shp.out)
            }
          }
          if(doStrip){
            if(is.null(clrby)){
              g <- g + geom_point(data=d_keep, shape=21, fill="black", colour="black", position=pos, alpha=alphaHalf)
            } else {
              g <- g + geom_point(data=d_keep, shape=21, colour="black", position=pos, alpha=alphaHalf)
            }
          }
        }
        if(doBox){
          if(is.null(clrby)){
            g <- g + geom_boxplot(data=d_keep2, fill="gray", colour="black", alpha=alpha, outlier.shape=shp.out)
          } else {
            g <- g + geom_boxplot(data=d_keep2, colour="black", alpha=alpha, outlier.shape=shp.out)
          }
        }
        if(doStrip){
          if(is.null(clrby)){
            g <- g + geom_point(data=d_keep2, shape=21, fill="black", colour="black", position=pos, alpha=alpha)
          } else {
            g <- g + geom_point(data=d_keep2, shape=21, colour="black", position=pos, alpha=alpha)
          }
        }
      } else {
        g <- g2
      }
    } else {
      g <- g2
    }
  }
  
  if(type=="barplot"){
    g2 <- g2 + geom_bar(stat="identity", colour="white", alpha=alpha, position=barpos)
    if(nrow(d_keep)==length(rv_plots$keeprows2)){
      if(!is.null(rv_plots$holdBrush2) | !is.null(rv_plots$holdClick2)){
        if(any(rv_plots$keeprows2)){
          d_keep2 <- d_keep[rv_plots$keeprows2,]
          d_keep <- setdiff(d_keep, d_keep2)
        } else {
          d_keep2 <- d_keep
        }
        if(nrow(d_keep)!=0){
          g <- g + geom_bar(data=d_keep, stat="identity", colour="white", alpha=alphaHalf, position=barpos)
        }
        g <- g + geom_bar(data=d_keep2, stat="identity", colour="white", alpha=alpha, position=barpos)
      } else {
        g <- g2
      }
    } else {
      g <- g2
    }
  }
  
  g <- g + coord_cartesian(xlim=limits[[1]], ylim=limits[[2]]) + theme_bw(base_size=14)
  g <- .colorFacet(g, d_source, clrby, clrvec, fctby, fctscales)
  g + plottheme
}
