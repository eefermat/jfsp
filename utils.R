tolpal <- function(n){
  if(n==0) return()
  if(n >= 12) return(c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499"))
  switch(n,
         "1"=c("#4477AA"),
         "2"=c("#4477AA", "#CC6677"),
         "3"=c("#4477AA", "#DDCC77", "#CC6677"),
         "4"=c("#4477AA", "#117733", "#DDCC77", "#CC6677"),
         "5"=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677"),
         "6"=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499"),
         "7"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499"),
         "8"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499"),
         "9"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499"),
         "10"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
         "11"=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
  )
}

tableRowColors <- function(data, variable, colorvec, alpha_append=NULL){
  if(!"included_" %in% names(data)) stop("This function requires a special data table (DT package)  containing a 'included_' column.")
  if(ncol(data)==0 || !any(data$included_=="_TRUE")) return("#FFFFFF")
  if(!any(data$included_=="_FALSE")) return("#CCCCCC")
  x <- sort(unique(data[[variable]]))
  
  if(is.null(colorvec) || (length(x)==1 && x=="") || 
    (!is.null(colorvec) && length(x)!=length(colorvec))){ # no coloring
    x <- c("_TRUE", "_FALSE")
    colorvec <- c("#CCCCCC", "#FFFFFF")
  } else { # coloring
    x <- c(paste0(x, "_", TRUE), paste0(x, "_", FALSE))
    colorvec2 <- if(is.null(alpha_append)) colorvec else paste0(colorvec, alpha_append)
    colorvec <- c(colorvec, colorvec2)
  }
  
  styleEqual(x, colorvec)
}

plottheme <- theme(panel.grid.major = element_line(size = .5, color = "grey"),
        plot.title=element_text(hjust=0.5),
        axis.line=element_line(size=.7, color="black"),
        axis.ticks.length=unit(0.35,"cm"),
        legend.position="bottom",
        text = element_text(size=14),
        panel.spacing.x=unit(0.25,"cm"),
        plot.margin=unit(c(0.5, 1, 0.5, 0.5),"cm"),
        strip.text=element_text(size=14))
