tsModUI <- function(id, titles, values=titles, width=12){
  ns <- NS(id)

  sel <- values[1]
  plotId <- paste0("plot", seq_along(titles))
  tb <- map(rev(seq_along(titles)), ~tabPanel(titles[.x],
           plotOutput(ns(plotId[.x]), height="auto",
                      click=ns(paste0(plotId[.x], "_clk")), dblclick=ns(paste0(plotId[.x], "_dblclk")),
                      hover=ns(paste0(plotId[.x], "_hov")), brush=brushOpts(id=ns(paste0(plotId[.x], "_brush")))),
           value=ns(values[.x]))
           )
  
  inputsBox <- function(grp, facet=grp, pooled, transforms=NULL, addLines=TRUE, togglePoints=TRUE, width){
    transformsInput <- if(!is.null(transforms)) 
      selectizeInput(ns("transform"), label="Apply transform", choices=c("", transforms), selected="", width="100%") else NULL
    addLinesInput <- if(addLines) checkboxInput(ns("addLines"), "Connect points with lines", FALSE, width="100%") else NULL
    togInput <- if(togglePoints)
      column(4,
           actionButton(ns("exclude_toggle"), "Toggle selected points", class="btn-block"),
           actionButton(ns("exclude_reset"), "Reset points", class="btn-block"),
           uiOutput(ns("btn_modal_table"))) else NULL
    togModal <- if(togglePoints)
      bsModal(ns("modal_table"), "Selected observations", ns("btn_modal_table"), size = "large",
            div(DT::dataTableOutput(ns('Selected_obs')), style="font-size: 100%")) else NULL
            
    box(  # Annual time series inputs
      fluidRow(
        column(4,
               selectizeInput(ns("colorby"), label="Color by", choices=grp, selected="", width="100%", options=list(placeholder='Color by...')),
               selectizeInput(ns("facetby"), label="Facet by", choices=facet, selected="", width="100%", options=list(placeholder='Facet by...')),
               selectizeInput(ns("pooled_vars"), label="Other variables", choices=pooled, selected=pooled[1], width="100%")
        ),
        column(4,
               transformsInput,
               bsModal(ns("settings"), "Annual time series additional settings", ns("btn_settings"), size="large",
                       fluidRow(
                         column(3, selectInput(ns("facet_scales"), "Axis scales", choices=axis_scales, selected="fixed", width="100%")),
                         column(3, sliderInput(ns("alpha"), "Semi-transparency", min=0.1, max=1, value=1, step=0.1, sep="", width="100%")),
                         column(3, checkboxInput(ns("jitter"), "Jitter points", FALSE, width="100%")),
                         column(3, addLinesInput)
                       )
               ),
               actionButton(ns("btn_settings"), "Additional settings", icon("gear"), class="btn-block")
        ),
        togInput
      ),
      togModal,
      title="Time series", status="primary", solidHeader=TRUE, width=width, collapsible=TRUE, collapsed=TRUE
    )
  }
  
  tagList(
    do.call(tabBox, c(tb, ns(id="box_ts"), selected=ns(sel), title="Time series", width=width, side="right")),
    inputsBox(grp=groupby_vars, pooled=pooled_options, transforms=c("Log", "Square root"), width=width),
    column(width,
      "Mouse feedback for annual time series", verbatimTextOutput(ns("info1")),
      "Mouse feedback for cumulative time series", verbatimTextOutput(ns("info2"))
    )
  )
}

denModUI <- function(id, width=12){
  ns <- NS(id)
  tagList(
    tabBox( # Distributions tab box
      tabPanel("Histogram",
               plotOutput(ns("plot_den2"), height="auto", click=ns("plot_den2_clk"), dblclick=ns("plot_den2_dblclk"), hover=ns("plot_den2_hov"),
                          brush=brushOpts(id=ns("plot_den2_brush"), direction="x", resetOnNew=TRUE)), value=ns("histogram")
      ),
      tabPanel("Density",
               plotOutput(ns("plot_den1"), height="auto", click=ns("plot_den1_clk"), dblclick=ns("plot_den1_dblclk"), hover=ns("plot_den1_hov"),
                          brush=brushOpts(id=ns("plot_den1_brush"), direction="x", resetOnNew=TRUE)), value=ns("density")
      ),
      ns(id="box_den"), selected=ns("density"), title="Aggregate distribution", width=width, side="right"
    ),
    box( # Distributions inputs
      fluidRow(
        column(6,
               selectizeInput(ns("den_colorby"), label=NULL, choices=groupby_vars, selected="", width="100%", options=list(placeholder='Color by...')),
               selectizeInput(ns("den_pooled_vars"), label=NULL, choices=pooled_options, selected=pooled_options[1], width="100%"),
               selectizeInput(ns("den_transform"), label=NULL, choices=c("", "Log", "Square root"), selected="", width="100%")
        ),
        column(6,
               selectizeInput(ns("den_facetby"), label=NULL, choices=groupby_vars, selected="", width="100%", options=list(placeholder='Facet by...')),
               bsModal(ns("den_settings"), "Distribution plot additional settings", ns("btn_den_settings"), size="large",
                       fluidRow(
                         column(3, selectInput(ns("den_facet_scales"), "Axis scales", choices=axis_scales, selected="fixed", width="100%")),
                         column(3, sliderInput(ns("den_alpha"), "Semi-transparency", min=0.1, max=1, value=1, step=0.1, sep="", width="100%")),
                         column(3, sliderInput(ns("den_bins"), "Histogram bins (approx.)", min=5, max=30, value=10, step=5, sep="", width="100%")),
                         column(3, selectInput(ns("den_zoom"), "Zoom behavior", choices=c("Zoom only", "Subset data"), selected="Zoom only", width="100%"))
                       )
               ),
               actionButton(ns("btn_den_settings"), "Additional settings", icon("gear"), class="btn-block")
        )
      ),
      title="Distribution", status="primary", solidHeader=TRUE, width=width, collapsible=TRUE, collapsed=TRUE
    ),
    column(width,
      "Mouse feedback for period density plot", verbatimTextOutput(ns("info_den1")),
      "Mouse feedback for period histogram plot", verbatimTextOutput(ns("info_den2"))
    )
  )
}

decModUI <- function(id, width=12){
  ns <- NS(id)
  
  tagList(
    tabBox( # Decadal change tab box
      tabPanel("Averages",
               plotOutput(ns("plot_dec2"), height="auto", click=ns("plot_dec2_clk"), dblclick=ns("plot_dec2_dblclk"), hover=ns("plot_dec2_hov"),
                          brush=brushOpts(id=ns("plot_dec2_brush"), resetOnNew=TRUE)), value=ns("dec_barplot")
      ),
      tabPanel("Observations",
               fluidRow(
                 column(8,
                        plotOutput(ns("plot_dec1"), height="auto", click=ns("plot_dec1_clk"), dblclick=ns("plot_dec1_dblclk"), hover=ns("plot_dec1_hov"),
                                   brush=brushOpts(id=ns("plot_dec1_brush"), direction="x"))
                 ),
                 column(4, uiOutput(ns("decStatsBoxes")))
               ),
               value=ns("dec_boxplot")
      ),
      ns(id="box_dec"), selected=ns("dec_boxplot"), title="Decadal change", width=width, side="right"
    ),
    box( # Decadal change inputs
      fluidRow(
        column(6,
               selectizeInput(ns("dec_colorby"), label=NULL, choices=groupby_vars, selected="", width="100%", options=list(placeholder='Color by...')),
               selectizeInput(ns("dec_pooled_vars"), label=NULL, choices=pooled_options, selected=pooled_options[1], width="100%"),
               selectizeInput(ns("dec_transform"), label=NULL, choices=c("", "Log", "Square root"), selected="", width="100%")
        ),
        column(6,
               selectizeInput(ns("dec_facetby"), label=NULL, choices=groupby_vars, selected="", width="100%", options=list(placeholder='Facet by...')),
               bsModal(ns("dec_settings"), "Decadal change additional settings", ns("btn_dec_settings"), size="large",
                       fluidRow(
                         column(3, selectInput(ns("dec_facet_scales"), "Axis scales", choices=axis_scales, selected="fixed", width="100%")),
                         column(3, sliderInput(ns("dec_alpha"), "Semi-transparency", min=0.1, max=1, value=1, step=0.1, sep="", width="100%")),
                         column(3, selectInput(ns("dec_boxplot_type"), "Observations", choices=c("Box plot", "Strip chart", "Overlay"),
                                               selected="Box plot", width="100%")),
                         column(3, checkboxInput(ns("dec_jitter"), "Jitter points", FALSE, width="100%"))
                       )
               ),
               actionButton(ns("btn_dec_settings"), "Additional settings", icon("gear"), class="btn-block")
        )
      ),
      title="Decadal change", status="primary", solidHeader=TRUE, width=width, collapsible=TRUE, collapsed=TRUE
    ),
    column(width,
      "Mouse feedback for decadal box plot", verbatimTextOutput(ns("info_dec1")),
      "Mouse feedback for decadal bar plot", verbatimTextOutput(ns("info_dec2"))
    )
  )
}

denMod <- function(input, output, session, data){
  ns <- session$ns
  
  source("modules/plots/mod_plot_dist.R", local=TRUE)
  
  variable <- reactive({ as.character(data()$Var[1]) })
  stat <- reactive({ tail(names(data()), 1) })
  d_den <- reactive({ plotDataPrep(data(), input$den_transform, input$den_pooled_vars, input$den_colorby, input$den_facetby, stat()) })
  
  rv_plots <- reactiveValues(xden1=NULL, yden1=NULL, xden2=NULL, yden2=NULL)
  
  # Distribution - density plot
  
  # Doubleclk observation
  observeEvent(input$plot_den1_dblclk, {
    brush <- input$plot_den1_brush
    if (!is.null(brush)) {
      rv_plots$xden1 <- c(brush$xmin, brush$xmax)
      rv_plots$yden1 <- c(brush$ymin, brush$ymax)
    } else {
      rv_plots$xden1 <- NULL
      rv_plots$yden1 <- NULL
    }
  })
  
  # Distribution - histogram
  
  # Doubleclk observation
  observeEvent(input$plot_den2_dblclk, {
    brush <- input$plot_den2_brush
    if (!is.null(brush)) {
      rv_plots$xden2 <- c(brush$xmin, brush$xmax)
      rv_plots$yden2 <- c(brush$ymin, brush$ymax)
    } else {
      rv_plots$xden2 <- NULL
      rv_plots$yden2 <- NULL
    }
  })
  
  den_colorby <- reactive({ if(input$den_colorby=="") NULL else input$den_colorby })
  den_colorvec <- reactive({ if(is.null(den_colorby())) NULL else tolpal(length(unique(d_den()[[den_colorby()]]))) })
  preventPlot <- reactive({ nrow(d_den())==0 | d_den()$Var[1]!=variable() })
  plotHeight <- reactive({ if(preventPlot()) 0 else 400 })
  den_plotInteraction <- reactive({ interact(names(d_den())) })
  
  output$plot_den1 <- renderPlot({ distPlot("density", list(rv_plots$xden1, rv_plots$yden1)) }, height=function() plotHeight())
  output$plot_den2 <- renderPlot({ distPlot("histogram", list(rv_plots$xden2, rv_plots$yden2)) }, height=function() plotHeight())
  
  output$info_den1 <- renderText({ mouseInfo(input$plot_den1_clk, input$plot_den1_dblclk, input$plot_den1_hov, input$plot_den1_brush) })
  output$info_den2 <- renderText({ mouseInfo(input$plot_den2_clk, input$plot_den2_dblclk, input$plot_den2_hov, input$plot_den2_brush) })
  
  outputOptions(output, "plot_den1", suspendWhenHidden=FALSE)
  outputOptions(output, "plot_den2", suspendWhenHidden=FALSE)
}

tsMod <- function(input, output, session, data){
  ns <- session$ns
  
  source("modules/plots/mod_plot_ts.R", local=TRUE)
  
  variable <- reactive({ as.character(data()$Var[1]) })
  stat <- reactive({ tail(names(data()), 1) })
  d <- reactive({ plotDataPrep(data(), input$transform, input$pooled_vars, input$colorby, input$facetby, stat()) })
  
  rv_plots <- reactiveValues(x1=NULL, y1=NULL, x2=NULL, y2=NULL, keeprows=rep(TRUE, nrow(isolate(d()))))
  
  # Time series plot annual
  
  # Doubleclk observation
  observeEvent(input$plot1_dblclk, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      rv_plots$x1 <- c(brush$xmin, brush$xmax)
      rv_plots$y1 <- c(brush$ymin, brush$ymax)
    } else {
      rv_plots$x1 <- NULL
      rv_plots$y1 <- NULL
    }
  })
  
  observeEvent(d(), {
    rv_plots$keeprows <- rep(TRUE, nrow(d()))
  })
  
  # Toggle points that are clked
  observeEvent(input$plot1_clk, {
    res <- nearPoints(d(), input$plot1_clk, allRows=TRUE)
    rv_plots$keeprows <- xor(rv_plots$keeprows, res$selected_)
  })
  
  # Toggle points that are brushed, when button is clked
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(d(), input$plot1_brush, allRows=TRUE)
    rv_plots$keeprows <- xor(rv_plots$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    rv_plots$keeprows <- rep(TRUE, nrow(d()))
  })
  
  # Time series plot cumulative
  
  # Doubleclk observation
  observeEvent(input$plot2_dblclk, {
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      rv_plots$x2 <- c(brush$xmin, brush$xmax)
      rv_plots$y2 <- c(brush$ymin, brush$ymax)
    } else {
      rv_plots$x2 <- NULL
      rv_plots$y2 <- NULL
    }
  })
  
  colorby <- reactive({ if(input$colorby=="") NULL else input$colorby })
  colorvec <- reactive({ if(is.null(colorby())) NULL else tolpal(length(unique(d()[[colorby()]]))) })
  
  keep    <- reactive({
    req(rv_plots$keeprows)
    if(length(rv_plots$keeprows) != nrow(d())) return()
    filter(d(), rv_plots$keeprows)
  })
  exclude <- reactive({
    req(rv_plots$keeprows)
    if(length(rv_plots$keeprows) != nrow(d())) return()
    filter(d(), !rv_plots$keeprows)
  })
  
  preventPlot <- reactive({ nrow(d())==0 | nrow(d())!=length(rv_plots$keeprows) | d()$Var[1]!=variable() })
  plotHeight <- reactive({ if(preventPlot()) 0 else 400 })
  plotInteraction <- reactive({ interact(names(d())) })
  
  output$plot1 <- renderPlot({ tsPlot("annual-raw", list(rv_plots$x1, rv_plots$y1)) }, height=function() plotHeight())
  output$plot2 <- renderPlot({ tsPlot("annual-cumulative", list(rv_plots$x2, rv_plots$y2)) }, height=function() plotHeight())
  
  output$info1 <- renderText({ mouseInfo(input$plot1_clk, input$plot1_dblclk, input$plot1_hov, input$plot1_brush) })
  output$info2 <- renderText({ mouseInfo(input$plot2_clk, input$plot2_dblclk, input$plot2_hov, input$plot2_brush) })
  
  output$Selected_obs <- DT::renderDataTable({
    # ignore input$plot1_click for table updates; click obs-toggling removes all selection
    if(is.null(input$plot1_brush)){
      x <- slice(d(), 0)
    } else {
      x <- brushedPoints(d(), input$plot1_brush, allRows=TRUE)
    }
    if(preventPlot() || nrow(x)==0 || nrow(filter(x, selected_))==0) return()
    x <- mutate(x, included_=rv_plots$keeprows) %>% filter(selected_) %>% mutate(selected_=NULL)
    x <- mutate(x, included_=paste0(x[[input$colorby]], "_", x$included_))
    clrs <- tableRowColors(x, input$colorby, colorvec(), "35")
    
    DT::datatable(x, options=list(
      lengthMenu=list(c(5, 10, 25), c('5', '10', '25')), pageLength=5, searching=FALSE,
      columnDefs=list(list(visible=FALSE, targets=ncol(x))))) %>%
      formatStyle(columns="included_", backgroundColor=clrs, target='row')
  })
  
  output$btn_modal_table <- renderUI({
    if(is.null(input$plot1_brush)) return()
    actionButton(ns("btn_modal_table"), "Show selections", icon("list"), class="btn-block")
  })
  
  outputOptions(output, "plot1", suspendWhenHidden=FALSE)
  outputOptions(output, "plot2", suspendWhenHidden=FALSE)
  #outputOptions(output, "Selected_obs", suspendWhenHidden=FALSE) # something wrong with reactive behavior here
}

decMod <- function(input, output, session, data){
  ns <- session$ns
  
  source("modules/plots/mod_plot_dec.R", local=TRUE)
  
  variable <- reactive({ as.character(data()$Var[1]) })
  stat <- reactive({ tail(names(data()), 1) })
  d_dec <- reactive({ plotDataPrep(data(), input$dec_transform, input$dec_pooled_vars, input$dec_colorby, input$dec_facetby, stat()) })
  
  rv_plots <- reactiveValues(
    xdec1=NULL, ydec1=NULL, xdec2=NULL, ydec2=NULL,
    dec_keeprows=rep(TRUE, nrow(isolate(d_dec()))),
    dec_holdClick=NULL, dec_holdBrush=NULL)
  
  # Decadal series boxplot
  
  # Doubleclk observation
  observeEvent(input$plot_dec1_dblclk, {
    brush <- input$plot_dec1_brush
    if (!is.null(brush)) {
      rv_plots$xdec1 <- c(brush$xmin, brush$xmax)
      rv_plots$ydec1 <- c(brush$ymin, brush$ymax)
    } else {
      rv_plots$xdec1 <- NULL
      rv_plots$ydec1 <- NULL
    }
  })
  
  observeEvent(keep_dec(), {
    rv_plots$dec_keeprows <- rep(TRUE, nrow(keep_dec()))
  })
  
  # Toggle points that are clked
  observeEvent(input$plot_dec1_clk, {
    x <- input$plot_dec1_clk
    y <- rv_plots$dec_holdClick
    if(!is.null(x)){
      if(is.null(y) || y$x!=x$x) rv_plots$dec_holdClick <- x
    }
  })
  
  observeEvent(input$plot_dec1_clk, {
    x <- keep_dec()$Decade
    lvls <- levels(x)
    clk <- input$plot_dec1_clk
    if(is.null(clk)) y <- rv_plots$dec_holdClick else y <- clk
    keep_lvls <- lvls[round(y$x)]
    if(!length(keep_lvls) || is.na(keep_lvls)) keep_lvls <- lvls
    if(any(rv_plots$dec_keeprows!=(x==keep_lvls))) rv_plots$dec_keeprows <- x %in% keep_lvls
  })
  
  # Toggle points that are brushed in x axis direction (all y)
  observeEvent(input$plot_dec1_brush, {
    x <- input$plot_dec1_brush
    if(!is.null(x)){
      rv_plots$dec_holdBrush <- x
    }
  })
  
  observeEvent(input$plot_dec1_brush, {
    x <- keep_dec()$Decade
    lvls <- levels(x)
    brush <- input$plot_dec1_brush
    if(is.null(brush)) y <- rv_plots$dec_holdBrush else y <- brush
    intlvls <- round(y$xmin):round(y$xmax)
    rv_plots$dec_keeprows <- x %in% lvls[intlvls]
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    rv_plots$dec_keeprows <- rep(TRUE, nrow(keep_dec()))
  })
  
  # Decadal series barplot
  
  # Doubleclk observation
  observeEvent(input$plot_dec2_dblclk, {
    brush <- input$plot_dec2_brush
    if (!is.null(brush)) {
      rv_plots$xdec2 <- c(brush$xmin, brush$xmax)
      rv_plots$ydec2 <- c(brush$ymin, brush$ymax)
    } else {
      rv_plots$xdec2 <- NULL
      rv_plots$ydec2 <- NULL
    }
  })
  
  dec_colorby <- reactive({ if(input$dec_colorby=="") NULL else input$dec_colorby })
  dec_colorvec <- reactive({ if(is.null(dec_colorby())) NULL else tolpal(length(unique(d_dec()[[dec_colorby()]]))) })
  
  keep_dec <- reactive({
    if(is.null(d_dec())) return()
    grp <- c("GBM", "RCP", "Model", "Region", "Var", "Vegetation")
    grp <- grp[grp %in% names(d_dec())]
    mutate(d_dec(), Decade=factor(paste0(Year - Year %% 10, "s"))) %>% group_by_(.dots=c(grp, "Decade")) %>% select(-Year)
  })
  
  preventPlot <- reactive({ nrow(keep_dec())==0 | keep_dec()$Var[1]!=variable() })
  plotHeight <- reactive({ if(preventPlot()) 0 else 400 })
  dec_plotInteraction <- reactive({ interact(names(d_dec())) })
  
  output$plot_dec1 <- renderPlot({ decPlot("boxplot", list(rv_plots$xdec1, rv_plots$ydec1)) }, height=function() plotHeight())
  output$plot_dec2 <- renderPlot({ decPlot("barplot", list(rv_plots$xdec2, rv_plots$ydec2)) }, height=function() plotHeight())
  
  output$info_dec1 <- renderText({ mouseInfo(input$plot_dec1_clk, input$plot_dec1_dblclk, input$plot_dec1_hov, input$plot_dec1_brush) })
  output$info_dec2 <- renderText({ mouseInfo(input$plot_dec2_clk, input$plot_dec2_dblclk, input$plot_dec2_hov, input$plot_dec2_brush) })
  
  output$decStatsBoxes <- renderUI({
    if(is.null(rv_plots$dec_holdBrush) && is.null(rv_plots$dec_holdClick)){
      x <- keep_dec()
    } else if(is.null(rv_plots$dec_holdBrush)){
      x <- keep_dec()[rv_plots$dec_keeprows,]
    } else if(is.null(rv_plots$holdClick)){
      x <- keep_dec()[rv_plots$dec_keeprows,]
    }
    
    if(preventPlot() || nrow(x)==0 || any(is.na(x$Var))) return()
    x <- ungroup(x) %>% summarise_(.dots=list(
      Mean_=paste0("mean(", stat(), ")"),
      Min_=paste0("min(", stat(), ")"),
      Max_=paste0("max(", stat(), ")"),
      Median_=paste0("stats::median(", stat(), ")"),
      Pct25_=paste0("stats::quantile(", stat(), ", prob=0.25)"),
      Pct75_=paste0("stats::quantile(", stat(), ", prob=0.75)"),
      SD_=paste0("stats::sd(", stat(), ")")
    )) %>% round
    
    clrs <- c("yellow", "orange", "purple", "red", "blue", "navy")
    statval <- c(x$Mean_, x$Min_, x$Max_, x$Median_, paste(x$Pct25_, "-", x$Pct75_), x$SD_)
    statlab <- c("Mean", "Min", "Max", "Median", "IQR", "Std Dev")
    val <- map2(statval, c(rep(100, 4), 75, 100), ~pTextSize(.x, .y))
    text <- map2(statlab, rep(150, 6), ~pTextSize(.x, .y))
    y <- list(
      mean=valueBox(val[[1]], text[[1]], icon=icon(list(src="stat_icon_normal_mean_white.png", width="80px"), lib="local"), color=clrs[1], width=NULL),
      min=valueBox(val[[2]], text[[2]], icon=icon(list(src="stat_icon_normal_min_white.png", width="80px"), lib="local"), color=clrs[2], width=NULL),
      max=valueBox(val[[3]], text[[3]], icon=icon(list(src="stat_icon_normal_max_white.png", width="80px"), lib="local"), color=clrs[3], width=NULL),
      med=valueBox(val[[4]], text[[4]], icon=icon(list(src="stat_icon_normal_median_white.png", width="80px"), lib="local"), color=clrs[4], width=NULL),
      iqr=valueBox(val[[5]], text[[5]], icon=icon(list(src="stat_icon_boxplot_iqr_white.png", width="80px"), lib="local"), color=clrs[5], width=NULL),
      sd=valueBox(val[[6]], text[[6]], icon=icon(list(src="stat_icon_normal_sd_white.png", width="80px"), lib="local"), color=clrs[6], width=NULL)
    )
    
    fluidRow(
      tags$head(tags$style(HTML(".small-box {height: 100px}"))),
      column(6, y$mean, y$med, y$min), column(6, y$sd, y$iqr, y$max)
    )
  })
  
  outputOptions(output, "plot_dec1", suspendWhenHidden=FALSE)
  outputOptions(output, "plot_dec2", suspendWhenHidden=FALSE)
  outputOptions(output, "decStatsBoxes", suspendWhenHidden=FALSE)
  
}
