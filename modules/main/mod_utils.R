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
  if(!"included_" %in% names(data))
    stop("This function requires a special data table (DT package) containing an 'included_' column.")
  x <- if(variable %in% names(data) && nrow(data)!=0) sort(unique(data[[variable]])) else ""

  if(is.null(colorvec) || (length(x)==1 && x=="")){ # no coloring
    x <- c("_TRUE", "_FALSE")
    colorvec <- c("#CCCCCC", "#FFFFFF")
  } else { # coloring
    colorvec <- colorvec[as.numeric(x)]
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

mouseInfo <- function(clk, dblclk, hov, brush){
  xy_str <- function(e) {
    if(is.null(e)) return("NULL\n")
    paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
  }
  xy_range_str <- function(e) {
    if(is.null(e)) return("NULL\n")
    paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
           " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
  }
  
  paste0(
    "click: ", xy_str(clk),
    "dblclick: ", xy_str(dblclk),
    "hover: ", xy_str(hov),
    "brush: ", xy_range_str(brush)
  )
}

plotDataPrep <- function(x, trans=NULL, pooled, col, facet, stat){
  if(pooled=="Average observations"){
    grp <- c("GBM", "RCP", "Model", "Region", "Vegetation")
    grp <- c(grp[grp %in% c(col, facet)], "Var", "Year")
    x <- group_by_(x, .dots=grp) %>% summarise_(Avg=lazyeval::interp(~round(mean(x)), x=as.name(stat))) %>% 
      rename_(.dots=setNames("Avg", stat)) %>% ungroup
  }
  
  if(!is.null(trans) && trans!=""){
    drp <- paste0("-", stat)
    grp <- c("GBM", "RCP", "Model", "Region", "Vegetation")
    transforms <- c("Baseline anomalies", "Period anomalies", "Prior decade anomalies")
    if(trans %in% transforms){
      if(trans==transforms[1]){
        y1 <- mutate(x, Decade=Year - Year %% 10)
        y1 <- filter(y1, Decade==y1$Decade[1]) %>% select(-Decade)
        grp <- c(grp[grp %in% c(col, facet)], "Var")
      } else if(trans==transforms[2]){
        y1 <- x
        grp <- c(grp[grp %in% c(col, facet)], "Var")
      } else if(trans==transforms[3]){
        y1 <- mutate(x, Decade=Year - Year %% 10)
        grp <- c(grp[grp %in% c(col, facet)], "Var", "Decade")
      }
      
      
      y2 <- group_by_(y1, .dots=grp) %>% summarise_(Avg=lazyeval::interp(~round(mean(x)), x=as.name(stat)))
      if(trans==transforms[3]) x <- mutate(y1, Decade=Decade - 10)
      x <- left_join(x, y2, by=grp) %>% filter(!is.na(Avg)) %>% 
        mutate_(.dots=list(Transformed=paste0(stat, "-Avg"))) %>% select_(.dots=list(drp, "-Avg"))
      if(trans==transforms[3]) x <- select(x, -Decade)
    } else {
      if(trans=="Log") x <- mutate_(x, Transformed=lazyeval::interp(~log(x+1), x=as.name(stat)))
      if(trans=="Square root") x <- mutate_(x, Transformed=lazyeval::interp(~sqrt(x), x=as.name(stat)))
      x <- select_(x, drp)
    }
    x <- rename_(x, .dots=setNames("Transformed", stat))
  }
  x
}

pTextSize <- function(x, value) tags$p(x, style=paste0("font-size: ", value, "%;"))

interact <- function(x){
  grp <- c("GBM", "RCP", "Model", "Region", "Vegetation")
  x <- grp[grp %in% x]
  if(!length(x)) return()
  paste0("interaction(", paste0(x, collapse=","), ")")
}

.getPosition <- function(jitter, cby, w=0.2, h=0, wd=0.75, dodgeable=FALSE){
  if(jitter) x <- position_jitter(width=w, height=h) else x <- "identity"
  if(!dodgeable) return(x)
  if(!is.null(cby)){
    if(jitter){
      x <- position_jitterdodge(jitter.width=w, jitter.height=h)
    } else {
      x <- position_dodge(width=wd)
    }
  }
  x
}

.colorFacet <- function(g, d, cby, clr, fby, scales){
  if(!is.null(clr)){
    g <- g + scale_fill_manual(values=clr, limits=levels(d[[cby]])) +
      scale_colour_manual(values=clr, limits=levels(d[[cby]]))
  }
  if(fby!="") g <- g + facet_wrap(as.formula(paste0("~", fby)), scales=scales)
  g
}

mouseLog <- function(x, ns, width){
  if(x) column(width,
         "Mouse feedback: plot 1", verbatimTextOutput(ns("info1")),
         "Mouse feedback: plot 2", verbatimTextOutput(ns("info2"))
  ) else NULL
}

inputsBox <- function(ns, grp, facet=grp, pooled, transforms=NULL, type, main="Tab box with inputs box", width){
  bpOpts <- c("Box plot", "Strip chart", "Overlay")
  barposOpts <- c("Dodge", "Stack", "Proportions"="fill")
  zoomOpts <- c("Zoom only", "Subset data")
  lab <- list(trans="Apply transform", lines="Connect points with lines", togPts="Toggle selected points",
    resPts="Reset points", selObs="Selected observations", jit="Jitter points", bpObs="Observations",
    bars="Grouped bars", zoom="Zoom behavior", bins="Histogram bins (approx.)", alpha="Semi-transparency")
  
  addLinesInput <- togInput <- togModal <- jitterInput <- binsInput <- 
    zoomInput <- bpInput <- barposInput <- deltaINput <- NULL
  
  transformsInput <- if(!is.null(transforms)) 
    selectizeInput(ns("transform"), lab$trans, c("", transforms), width="100%",
                   options=list(placeholder='Apply transform...')) else NULL
  
  if(type=="ts"){
    addLinesInput <- checkboxInput(ns("addLines"), lab$lines, width="100%")
    togInput <- tagList(
      actionButton(ns("exclude_toggle"), lab$togPts, class="btn-block"),
      actionButton(ns("exclude_reset"), lab$resPts, class="btn-block"),
      uiOutput(ns("btn_modal_table")))
    togModal <- bsModal(ns("modal_table"), lab$selObs, ns("btn_modal_table"),
      size="large", div(DT::dataTableOutput(ns('Selected_obs')), style="font-size: 100%"))
  }
  
  if(type %in% c("ts", "dec")){
    jitterInput <- checkboxInput(ns("jitter"), lab$jit, width="100%")
  }
  
  if(type=="dec"){
    bpInput <- selectInput(ns("bptype"), lab$bpObs, bpOpts, width="100%")
    barposInput <- selectInput(ns("barpos"), lab$bars, barposOpts, width="100%")
  }
  
  if(type=="den"){
    binsInput <- sliderInput(ns("bins"), lab$bins, 5, 30, 10, 5, sep="", width="100%")
    zoomInput <- selectInput(ns("zoom"), lab$zoom, zoomOpts, width="100%")
  }
  
  box(
    fluidRow(
      column(6,
        selectizeInput(ns("colorby"), "Color by", grp, width="100%",
                       options=list(placeholder='Color by...')),
        selectizeInput(ns("facetby"), "Facet by", facet, width="100%",
                       options=list(placeholder='Facet by...')),
        selectInput(ns("pooled_vars"), "Other variables", pooled, width="100%")
      ),
      column(6,
        transformsInput,
        bsModal(ns("settings"), paste(main, "additional settings"), ns("btn_settings"), size="large",
          fluidRow(
           column(3, sliderInput(ns("alpha"), lab$alpha, 0.1, 1, 1, 0.1, sep="", width="100%")),
           column(3, selectInput(ns("facet_scales"), "Axis scales", choices=axis_scales, width="100%")),
           column(3, binsInput, zoomInput, bpInput, barposInput, jitterInput),
           column(3, addLinesInput)
          )
        ),
        togInput,
        actionButton(ns("btn_settings"), "Additional settings", icon("gear"), class="btn-block")
      )
    ),
    togModal,
    title=main, status="primary", solidHeader=TRUE, width=width, collapsible=TRUE, collapsed=TRUE
  )
}
