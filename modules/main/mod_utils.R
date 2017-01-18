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
  if(!is.null(trans) && trans!=""){
    if(trans=="Log") x <- mutate_(x, Transformed=lazyeval::interp(~log(x+1), x=as.name(stat)))
    if(trans=="Square root") x <- mutate_(x, Transformed=lazyeval::interp(~sqrt(x), x=as.name(stat)))
    x <- select_(x, paste0("-", stat)) %>% rename_(.dots=setNames("Transformed", stat))
  }
  if(pooled=="Unique observations") return(x)
  grp <- c("GBM", "RCP", "Model", "Region", "Vegetation")
  grp <- c(grp[grp %in% c(col, facet)], "Var", "Year")
  group_by_(x, .dots=grp) %>% summarise_(Avg=lazyeval::interp(~round(mean(x)), x=as.name(stat))) %>% 
    rename_(.dots=setNames("Avg", stat)) %>% ungroup
}

pTextSize <- function(x, value) tags$p(x, style=paste0("font-size: ", value, "%;"))

interact <- function(x){
  grp <- c("GBM", "RCP", "Model", "Region", "Vegetation")
  x <- grp[grp %in% x]
  if(!length(x)) return()
  paste0("interaction(", paste0(x, collapse=","), ")")
}

inputsBox <- function(ns, grp, facet=grp, pooled, transforms=NULL, type, main="Tab box with inputs box", width){
  
  
  addLinesInput <- togInput <- togModal <- jitterInput <- binsInput <- zoomInput <- bpInput <- NULL
  transformsInput <- if(!is.null(transforms)) 
    selectizeInput(ns("transform"), label="Apply transform", choices=c("", transforms), selected="", width="100%") else NULL
  
  if(type=="ts"){
    addLinesInput <- checkboxInput(ns("addLines"), "Connect points with lines", FALSE, width="100%")
    togInput <- column(4,
      actionButton(ns("exclude_toggle"), "Toggle selected points", class="btn-block"),
      actionButton(ns("exclude_reset"), "Reset points", class="btn-block"),
      uiOutput(ns("btn_modal_table")))
    togModal <- bsModal(ns("modal_table"), "Selected observations", ns("btn_modal_table"), size = "large",
      div(DT::dataTableOutput(ns('Selected_obs')), style="font-size: 100%"))
  }
  
  if(type %in% c("ts", "dec")){
    bpInput <- selectInput(ns("bptype"), "Observations", choices=c("Box plot", "Strip chart", "Overlay"),
                selected="Box plot", width="100%")
    jitterInput <- checkboxInput(ns("jitter"), "Jitter points", FALSE, width="100%")
  }
  
  if(type=="den"){
    binsInput <- sliderInput(ns("bins"), "Histogram bins (approx.)", min=5, max=30, value=10, step=5, sep="", width="100%")
    zoomInput <- selectInput(ns("zoom"), "Zoom behavior", choices=c("Zoom only", "Subset data"), selected="Zoom only", width="100%")
  }
  
  box(
    fluidRow(
      column(4,
        selectizeInput(ns("colorby"), label="Color by", choices=grp, selected="", width="100%", options=list(placeholder='Color by...')),
        selectizeInput(ns("facetby"), label="Facet by", choices=facet, selected="", width="100%", options=list(placeholder='Facet by...')),
        selectizeInput(ns("pooled_vars"), label="Other variables", choices=pooled, selected=pooled[1], width="100%")
      ),
      column(4,
        transformsInput,
        bsModal(ns("settings"), paste(main, "additional settings"), ns("btn_settings"), size="large",
          fluidRow(
           column(3, sliderInput(ns("alpha"), "Semi-transparency", min=0.1, max=1, value=1, step=0.1, sep="", width="100%")),
           column(3, selectInput(ns("facet_scales"), "Axis scales", choices=axis_scales, selected="fixed", width="100%")),
           column(3, binsInput, zoomInput, bpInput, jitterInput),
           column(3, addLinesInput)
          )
        ),
        actionButton(ns("btn_settings"), "Additional settings", icon("gear"), class="btn-block")
      ),
      togInput
    ),
    togModal,
    title=main, status="primary", solidHeader=TRUE, width=width, collapsible=TRUE, collapsed=TRUE
  )
}
