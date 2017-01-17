# Time series - decadal
decPlot <- function(type, limits){
  if(preventPlot()) return()
  if(input$dec_jitter) pos <- position_jitter(width=0.2, height=0) else pos <- "identity"
  if(!is.null(dec_colorby())){
    if(input$dec_jitter){
      pos <- position_jitterdodge(jitter.width=0.2, jitter.height=0)
    } else {
      pos <- position_dodge(width=0.75)
    }
  }
  
  grp <- c("GBM", "RCP", "Model", "Region", "Var", "Vegetation")
  statname <- stat()
  d_keep <- keep_dec()
  x <- "Decade"
  if(type=="barplot"){
    d_keep <- summarise_(d_keep, Decadal_mean=lazyeval::interp(~mean(x), x=as.name(stat())))
    statname <- "Decadal_mean"
  }
  d_source <- d_keep
  clrby <- dec_colorby()
  pInteract <- dec_plotInteraction()
  clrvec <- dec_colorvec()
  fctby <- input$dec_facetby
  fctscales <- input$dec_facet_scales
  alpha <- input$dec_alpha
  alphaHalf <- alpha/2
  
  g <- ggplot(data=d_source, aes_string(x, statname, colour=clrby, fill=clrby))
  
  if(type=="boxplot"){
    doBox <- input$dec_boxplot_type %in% c("Box plot", "Overlay")
    doStrip <- input$dec_boxplot_type %in% c("Strip chart", "Overlay")
    if(doStrip) shp.out <- NA else shp.out <- 21
    g2 <- g
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
    if(nrow(keep_dec())==length(rv_plots$dec_keeprows)){
      if(!is.null(rv_plots$dec_holdBrush) | !is.null(rv_plots$dec_holdClick)){
        if(any(rv_plots$dec_keeprows)){
          d_keep2 <- d_keep[rv_plots$dec_keeprows,]
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
    if(is.null(clrby)){
      g <- g + geom_bar(stat="identity", colour="white", alpha=alpha)
    } else {
      g <- g + geom_bar(stat="identity", alpha=alpha)
    }
  }
  
  g <- g + coord_cartesian(xlim=limits[[1]], ylim=limits[[2]]) + theme_bw(base_size=14)
  
  if(!is.null(clrvec)){
    g <- g + scale_fill_manual(values=clrvec, limits=levels(d_source[[clrby]])) +
      scale_colour_manual(values=clrvec, limits=levels(d_source[[clrby]]))
  }
  if(fctby!="") g <- g + facet_wrap(as.formula(paste0("~", fctby)), scales=fctscales)
  g + plottheme
}
