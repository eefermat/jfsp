tsModUI <- function(id){
  ns <- NS(id)
  
  tagList(
    tabBox(
     tabPanel("Cumulative",
              plotOutput(ns("plot_ts2"), height="auto", click=ns("plot_ts2_clk"), dblclick=ns("plot_ts2_dblclk"), hover=ns("plot_ts2_hov"),
                         brush=brushOpts(id=ns("plot_ts2_brush"))), value=ns("cumulative")
     ),
     tabPanel("Raw Observations",
              plotOutput(ns("plot_ts1"), height="auto", click=ns("plot_ts1_clk"), dblclick=ns("plot_ts1_dblclk"), hover=ns("plot_ts1_hov"),
                         brush=brushOpts(id=ns("plot_ts1_brush"))), value=ns("annual")
     ),
     ns(id="box_ts"), selected=ns("annual"), title="Time series", width=8, side="right"
    ),
    box( 
      fluidRow(
        column(4,
               selectizeInput(ns("colorby"), label=NULL, choices=groupby_vars, selected="", width="100%", options=list(placeholder='Color by...')),
               selectizeInput(ns("pooled_vars"), label=NULL, choices=pooled_options, selected=pooled_options[1], width="100%"),
               selectizeInput(ns("transform"), label=NULL, choices=c("", "Log", "Square root"), selected="", width="100%")
        ),
        column(4,
               selectizeInput(ns("facetby"), label=NULL, choices=groupby_vars, selected="", width="100%", options=list(placeholder='Facet by...')),
               bsModal(ns("ts_settings"), "Annual time series additional settings", ns("btn_ts_settings"), size="large",
                       fluidRow(
                         column(3, selectInput(ns("ts_facet_scales"), "Axis scales", choices=axis_scales, selected="fixed", width="100%")),
                         column(3, sliderInput(ns("ts_alpha"), "Semi-transparency", min=0.1, max=1, value=1, step=0.1, sep="", width="100%")),
                         column(3, checkboxInput(ns("ts_lines"), "Connect points with lines", FALSE, width="100%")),
                         column(3, checkboxInput(ns("ts_jitter"), "Jitter points", FALSE, width="100%"))
                       )
               ),
               actionButton(ns("btn_ts_settings"), "Additional settings", icon("gear"), class="btn-block")
        ),
        column(4,
               actionButton(ns("exclude_toggle"), "Toggle selected points", class="btn-block"),
               actionButton(ns("exclude_reset"), "Reset points", class="btn-block"),
               uiOutput(ns("btn_modal_table"))
        )
      ),
      bsModal(ns("modal_table"), "Selected observations", ns("btn_modal_table"), size = "large",
              div(DT::dataTableOutput(ns('Selected_obs')), style="font-size: 100%")
      ),
      title="Time series", status="primary", solidHeader=TRUE, width=8, collapsible=TRUE, collapsed=TRUE
    ),
    "Mouse feedback for annual time series", verbatimTextOutput(ns("info_ts1")),
    "Mouse feedback for cumulative time series", verbatimTextOutput(ns("info_ts2"))
  )
}

denModUI <- function(id){
  ns <- NS(id)
  
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
      title="Distribution", status="primary", solidHeader=TRUE, width=4, collapsible=TRUE, collapsed=TRUE
    )
}

decModUI <- function(id){
  ns <- NS(id)
  
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
      title="Decadal change", status="primary", solidHeader=TRUE, width=12, collapsible=TRUE, collapsed=TRUE
    )
}

tsMod <- function(input, output, session, data){
  ns <- session$ns
  
  source("mod_ts_plot.R", local=TRUE)
  
  variable <- reactive({ as.character(data()$Var[1]) })
  stat <- reactive({ tail(names(data()), 1) })
  d <- reactive({ plotDataPrep(data(), input$transform, input$pooled_vars, input$colorby, input$facetby, stat()) })
  
  rv_plots <- reactiveValues(xts1=NULL, yts1=NULL, xts2=NULL, yts2=NULL, keeprows=rep(TRUE, nrow(isolate(d()))))
  
  # Time series plot annual
  
  # Doubleclk observation
  observeEvent(input$plot_ts1_dblclk, {
    brush <- input$plot_ts1_brush
    if (!is.null(brush)) {
      rv_plots$xts1 <- c(brush$xmin, brush$xmax)
      rv_plots$yts1 <- c(brush$ymin, brush$ymax)
    } else {
      rv_plots$xts1 <- NULL
      rv_plots$yts1 <- NULL
    }
  })
  
  observeEvent(d(), {
    rv_plots$keeprows <- rep(TRUE, nrow(d()))
  })
  
  # Toggle points that are clked
  observeEvent(input$plot_ts1_clk, {
    res <- nearPoints(d(), input$plot_ts1_clk, allRows=TRUE)
    rv_plots$keeprows <- xor(rv_plots$keeprows, res$selected_)
  })
  
  # Toggle points that are brushed, when button is clked
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(d(), input$plot_ts1_brush, allRows=TRUE)
    rv_plots$keeprows <- xor(rv_plots$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    rv_plots$keeprows <- rep(TRUE, nrow(d()))
  })
  
  # Time series plot cumulative
  
  # Doubleclk observation
  observeEvent(input$plot_ts2_dblclk, {
    brush <- input$plot_ts2_brush
    if (!is.null(brush)) {
      rv_plots$xts2 <- c(brush$xmin, brush$xmax)
      rv_plots$yts2 <- c(brush$ymin, brush$ymax)
    } else {
      rv_plots$xts2 <- NULL
      rv_plots$yts2 <- NULL
    }
  })
  
  #source("mod_observers.R", local=TRUE)
  
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
  
  keep_dec <- reactive({
    if(is.null(d_dec())) return()
    grp <- c("GBM", "RCP", "Model", "Region", "Var", "Vegetation")
    grp <- grp[grp %in% names(d_dec())]
    mutate(d_dec(), Decade=factor(paste0(Year - Year %% 10, "s"))) %>% group_by_(.dots=c(grp, "Decade")) %>% select(-Year)
  })
  preventPlot_dec <- reactive({ nrow(keep_dec())==0 | keep_dec()$Var[1]!=variable() })
  
  #source("mod_plots.R", local=TRUE)  
  
  output$plot_ts1 <- renderPlot({ tsPlot("annual-raw", list(rv_plots$xts1, rv_plots$yts1)) }, height=function() plotHeight())
  output$plot_ts2 <- renderPlot({ tsPlot("annual-cumulative", list(rv_plots$xts2, rv_plots$yts2)) }, height=function() plotHeight())
  
  output$info_ts1 <- renderText({ mouseInfo(input$plot_ts1_clk, input$plot_ts1_dblclk, input$plot_ts1_hov, input$plot_ts1_brush) })
  output$info_ts2 <- renderText({ mouseInfo(input$plot_ts2_clk, input$plot_ts2_dblclk, input$plot_ts2_hov, input$plot_ts1_brush) })
  
  output$Selected_obs <- DT::renderDataTable({
    # ignore input$plot1_click for table updates; click obs-toggling removes all selection
    if(is.null(input$plot_ts1_brush)){
      x <- slice(d(), 0)
    } else {
      x <- brushedPoints(d(), input$plot_ts1_brush, allRows=TRUE)
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
    if(is.null(input$plot_ts1_brush)) return()
    actionButton(ns("btn_modal_table"), "Show selections", icon("list"), class="btn-block")
  })
}
