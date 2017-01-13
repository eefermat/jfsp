dbmodUI <- function(id, tab_name){
  ns <- NS(id)

    tabItem(tabName=tab_name,
      fluidRow( # Row 1
        tabBox( # Annual time series tab box
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
        tabBox( # Distributions tab box
          tabPanel("Histogram",
                   plotOutput(ns("plot_den2"), height="auto", click=ns("plot_den2_clk"), dblclick=ns("plot_den2_dblclk"), hover=ns("plot_den2_hov"),
                              brush=brushOpts(id=ns("plot_den2_brush"), direction="x", resetOnNew=TRUE)), value=ns("histogram")
          ),
          tabPanel("Density",
                   plotOutput(ns("plot_den1"), height="auto", click=ns("plot_den1_clk"), dblclick=ns("plot_den1_dblclk"), hover=ns("plot_den1_hov"),
                              brush=brushOpts(id=ns("plot_den1_brush"), direction="x", resetOnNew=TRUE)), value=ns("density")
          ),
          ns(id="box_den"), selected=ns("density"), title="Aggregate distribution", width=4, side="right"
        )
      ),
      fluidRow( # Row 2
        box( # Annual time series inputs
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
      ),
      fluidRow( # Row 3
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
          ns(id="box_dec"), selected=ns("dec_boxplot"), title="Decadal change", width=12, side="right"
        )
      ),
      fluidRow( # Row 4
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
      ),
      br(),
      fluidRow( # Row 5 - feedback for testing purposes - remove later
        box(
        column(8,
               "Mouse feedback for annual time series", verbatimTextOutput(ns("info_ts1")),
               "Mouse feedback for cumulative time series", verbatimTextOutput(ns("info_ts2"))
        ),
        column(4,
               "Mouse feedback for period density plot", verbatimTextOutput(ns("info_den1")),
               "Mouse feedback for period histogram plot", verbatimTextOutput(ns("info_den2")),
               "Mouse feedback for decadal box plot", verbatimTextOutput(ns("info_dec1")),
               "Mouse feedback for decadal bar plot", verbatimTextOutput(ns("info_dec2"))
        ), title="App diagnostics for interactive plots", width=12
      )
      )
    )
  
}

dbmod <- function(input, output, session, data){
  ns <- session$ns
  source("mod_utils.R", local=TRUE)
  
  variable <- reactive({ as.character(data()$Var[1]) })
  stat <- reactive({ tail(names(data()), 1) })
  d <- reactive({ plotDataPrep(input$transform, input$pooled_vars, input$colorby, input$facetby, stat()) })
  d_dec <- reactive({ plotDataPrep(input$dec_transform, input$dec_pooled_vars, input$dec_colorby, input$dec_facetby, stat()) })
  d_den <- reactive({ plotDataPrep(input$den_transform, input$den_pooled_vars, input$den_colorby, input$den_facetby, stat()) })
  
  rv_plots <- reactiveValues(xts1=NULL, yts1=NULL, xts2=NULL, yts2=NULL,
                             xden1=NULL, yden1=NULL, xden2=NULL, yden2=NULL,
                             keeprows=rep(TRUE, nrow(isolate(d()))),
                             xdec1=NULL, ydec1=NULL, xdec2=NULL, ydec2=NULL,
                             dec_keeprows=rep(TRUE, nrow(isolate(d()))),
                             dec_holdClick=NULL, dec_holdBrush=NULL)
  
  source("mod_observers.R", local=TRUE)
  
  colorby <- reactive({ if(input$colorby=="") NULL else input$colorby })
  den_colorby <- reactive({ if(input$den_colorby=="") NULL else input$den_colorby })
  dec_colorby <- reactive({ if(input$dec_colorby=="") NULL else input$dec_colorby })
  
  colorvec <- reactive({ if(is.null(colorby())) NULL else tolpal(length(unique(d()[[colorby()]]))) })
  den_colorvec <- reactive({ if(is.null(den_colorby())) NULL else tolpal(length(unique(d_den()[[den_colorby()]]))) })
  dec_colorvec <- reactive({ if(is.null(dec_colorby())) NULL else tolpal(length(unique(d_dec()[[dec_colorby()]]))) })
  
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
  den_plotInteraction <- reactive({ interact(names(d_den())) })
  dec_plotInteraction <- reactive({ interact(names(d_dec())) })
  
  keep_dec <- reactive({
    if(is.null(d_dec())) return()
    grp <- c("GBM", "RCP", "Model", "Region", "Var", "Vegetation")
    grp <- grp[grp %in% names(d_dec())]
    mutate(d_dec(), Decade=factor(paste0(Year - Year %% 10, "s"))) %>% group_by_(.dots=c(grp, "Decade")) %>% select(-Year)
  })
  preventPlot_dec <- reactive({ nrow(keep_dec())==0 | keep_dec()$Var[1]!=variable() })
  
  source("mod_plots.R", local=TRUE)  
  output$plot_den1 <- renderPlot({ distPlot("density", list(rv_plots$xden1, rv_plots$yden1)) }, height=function() plotHeight())
  output$plot_den2 <- renderPlot({ distPlot("histogram", list(rv_plots$xden2, rv_plots$yden2)) }, height=function() plotHeight())
  output$plot_ts1 <- renderPlot({ tsPlot("annual-raw", list(rv_plots$xts1, rv_plots$yts1)) }, height=function() plotHeight())
  output$plot_ts2 <- renderPlot({ tsPlot("annual-cumulative", list(rv_plots$xts2, rv_plots$yts2)) }, height=function() plotHeight())
  output$plot_dec1 <- renderPlot({ tsPlot("boxplot", list(rv_plots$xdec1, rv_plots$ydec1)) }, height=function() plotHeight())
  output$plot_dec2 <- renderPlot({ tsPlot("barplot", list(rv_plots$xdec2, rv_plots$ydec2)) }, height=function() plotHeight())
  
  output$info_ts1 <- renderText({ mouseInfo(input$plot_ts1_clk, input$plot_ts1_dblclk, input$plot_ts1_hov, input$plot_ts1_brush) })
  output$info_ts2 <- renderText({ mouseInfo(input$plot_ts2_clk, input$plot_ts2_dblclk, input$plot_ts2_hov, input$plot_ts1_brush) })
  output$info_den1 <- renderText({ mouseInfo(input$plot_den1_clk, input$plot_den1_dblclk, input$plot_den1_hov, input$plot_den1_brush) })
  output$info_den2 <- renderText({ mouseInfo(input$plot_den2_clk, input$plot_den2_dblclk, input$plot_den2_hov, input$plot_den2_brush) })
  output$info_dec1 <- renderText({ mouseInfo(input$plot_dec1_clk, input$plot_dec1_dblclk, input$plot_dec1_hov, input$plot_dec1_brush) })
  output$info_dec2 <- renderText({ mouseInfo(input$plot_dec2_clk, input$plot_dec2_dblclk, input$plot_dec2_hov, input$plot_dec2_brush) })
  
  # not suspending when hidden does not work within modules.
  # Desipite unique prepended module IDs, the ids still seem to conflict.
  
  #outputOptions(output, "plot_den2", suspendWhenHidden=FALSE)
  #outputOptions(output, "plot_ts1", suspendWhenHidden=FALSE)
  #outputOptions(output, "plot_ts2", suspendWhenHidden=FALSE)
  
  #outputOptions(output, "plot_den1", suspendWhenHidden=FALSE)
  #outputOptions(output, "info_ts1", suspendWhenHidden=FALSE)
  #outputOptions(output, "info_ts2", suspendWhenHidden=FALSE)
  #outputOptions(output, "info_den1", suspendWhenHidden=FALSE)
  #outputOptions(output, "info_den2", suspendWhenHidden=FALSE)
  
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
  
  output$decStatsBoxes <- renderUI({
    if(is.null(rv_plots$dec_holdBrush) && is.null(rv_plots$dec_holdClick)){
      x <- keep_dec()
    } else if(is.null(rv_plots$dec_holdBrush)){
      x <- keep_dec()[rv_plots$dec_keeprows,]
    } else if(is.null(rv_plots$holdClick)){
      x <- keep_dec()[rv_plots$dec_keeprows,]
    }
    
    if(preventPlot_dec() || nrow(x)==0 || any(is.na(x$Var))) return()
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
  
}
