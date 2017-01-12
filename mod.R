dbmodUI <- function(id, tab_name){
  ns <- NS(id)

    tabItem(tabName=tab_name,
      fluidRow(
        column(4),
        column(4, actionButton(ns("exclude_toggle"), "Toggle points", class="btn-block")),
        column(4, actionButton(ns("exclude_reset"), "Reset", class="btn-block"))
      ),
      fluidRow(
        tabBox(
          tabPanel("Histogram",
                   plotOutput(ns("plot_den2"), height="auto", click=ns("plot_den2_clk"), dblclick=ns("plot_den2_dblclk"), hover=ns("plot_den2_hov"),
                              brush=brushOpts(id=ns("plot_den2_brush"), resetOnNew=TRUE)), value=ns("histogram")
          ),
          tabPanel("Density",
                   plotOutput(ns("plot_den1"), height="auto", click=ns("plot_den1_clk"), dblclick=ns("plot_den1_dblclk"), hover=ns("plot_den1_hov"),
                              brush=brushOpts(id=ns("plot_den1_brush"), resetOnNew=TRUE)), value=ns("density")
          ),
          ns(id="box_den"), selected=ns("density"), title="Aggregate distribution", width=4, side="right"
        ),
        tabBox(
          tabPanel("Cumulative",
                   plotOutput(ns("plot_ts2"), height="auto", click=ns("plot_ts2_clk"), dblclick=ns("plot_ts2_dblclk"), hover=ns("plot_ts2_hov"),
                              brush=brushOpts(id=ns("plot_ts2_brush"), resetOnNew=TRUE)), value=ns("cumulative")
          ),
          tabPanel("Annual",
                   plotOutput(ns("plot_ts1"), height="auto", click=ns("plot_ts1_clk"), dblclick=ns("plot_ts1_dblclk"), hover=ns("plot_ts1_hov"),
                              brush=brushOpts(id=ns("plot_ts1_brush"), resetOnNew=TRUE)), value=ns("annual")
          ),
          ns(id="box_ts"), selected=ns("annual"), title="Time series", width=8, side="right"
        )
      ),
      fluidRow(
        box(
          fluidRow(
            column(6,
                   selectizeInput(ns("den_colorby"), label=NULL, choices=groupby_vars, selected="", width="100%", options=list(placeholder='Color by...')),
                   selectizeInput(ns("den_pooled_vars"), label=NULL, choices=pooled_options, selected=pooled_options[1], width="100%"),
                   selectizeInput(ns("den_transform"), label=NULL, choices=c("", "Log", "Square root"), selected="", width="100%")
            ),
            column(6, selectizeInput(ns("den_facetby"), label=NULL, choices=groupby_vars, selected="", width="100%", options=list(placeholder='Facet by...')))
          ),
          title="Distribution", status="primary", solidHeader=TRUE, width=4, collapsible=TRUE, collapsed=TRUE
        ),
        box(
          fluidRow(
            column(6,
                   selectizeInput(ns("colorby"), label=NULL, choices=groupby_vars, selected="", width="100%", options=list(placeholder='Color by...')),
                   selectizeInput(ns("pooled_vars"), label=NULL, choices=pooled_options, selected=pooled_options[1], width="100%")
            ),
            column(6, selectizeInput(ns("facetby"), label=NULL, choices=groupby_vars, selected="", width="100%", options=list(placeholder='Facet by...')))
          ),
          title="Time series", status="primary", solidHeader=TRUE, width=8, collapsible=TRUE, collapsed=TRUE
        )
      ),
      fluidRow(
        tabBox(
          tabPanel("Averages",
                   plotOutput(ns("plot_dec2"), height="auto", click=ns("plot_dec2_clk"), dblclick=ns("plot_dec2_dblclk"), hover=ns("plot_dec2_hov"),
                              brush=brushOpts(id=ns("plot_dec2_brush"), resetOnNew=TRUE)), value=ns("dec_barplot")
          ),
          tabPanel("Observations",
                   fluidRow(
                     column(4, uiOutput(ns("decStatsBoxes"))),
                     column(8,
                       plotOutput(ns("plot_dec1"), height="auto", click=ns("plot_dec1_clk"), dblclick=ns("plot_dec1_dblclk"), hover=ns("plot_dec1_hov"),
                                brush=brushOpts(id=ns("plot_dec1_brush"), direction="x"))
                     )
                   ),
                   value=ns("dec_boxplot")
          ),
          ns(id="box_dec"), selected=ns("dec_boxplot"), title="Decadal change", width=12, side="right"
        )
      ),
      box(
        fluidRow(
          column(6,
                 selectizeInput(ns("dec_colorby"), label=NULL, choices=groupby_vars, selected="", width="100%", options=list(placeholder='Color by...')),
                 selectizeInput(ns("dec_pooled_vars"), label=NULL, choices=pooled_options, selected=pooled_options[1], width="100%"),
                 selectizeInput(ns("dec_transform"), label=NULL, choices=c("", "Log", "Square root"), selected="", width="100%")
          ),
          column(6, selectizeInput(ns("dec_facetby"), label=NULL, choices=groupby_vars, selected="", width="100%", options=list(placeholder='Facet by...')))
        ),
        title="Decadal change", status="primary", solidHeader=TRUE, width=12, collapsible=TRUE, collapsed=TRUE
      ),
      br(),
      fluidRow(
        column(4),
        column(8, div(DT::dataTableOutput(ns('Selected_obs')), style="font-size: 75%"))
      ),
      br(),
      fluidRow(
        box(
        column(4,
               "Mouse feedback for period density plot", verbatimTextOutput(ns("info_den1")),
               "Mouse feedback for period histogram plot", verbatimTextOutput(ns("info_den2")),
               "Mouse feedback for decadal box plot", verbatimTextOutput(ns("info_dec1")),
               "Mouse feedback for decadal bar plot", verbatimTextOutput(ns("info_dec2"))
        ),
        column(8,
               "Mouse feedback for annual time series", verbatimTextOutput(ns("info_ts1")),
               "Mouse feedback for cumulative time series", verbatimTextOutput(ns("info_ts2"))
        ), title="App diagnostics for interactive plots", width=12
      )
      )
    )
  
}

dbmod <- function(input, output, session, data, variable, alpha, showLines, jitterPoints, facetScales){
  ns <- session$ns
  source("mod_utils.R", local=TRUE)
  
  stat <- reactive({ tail(names(data()), 1) })
  d <- reactive({ plotDataPrep(NULL, input$pooled_vars, input$colorby, input$facetby, stat()) })
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
  
  preventPlot <- reactive({ nrow(d())==0 | nrow(d())!=length(rv_plots$keeprows) | d()$Var[1]!=variable })
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
  preventPlot_dec <- reactive({ nrow(keep_dec())==0 | keep_dec()$Var[1]!=variable })
  
  distPlot <- function(type, limits){
    #if(preventPlot()) return()
    g <- ggplot(data=d_den(), aes_string(stat(), fill=den_colorby(), colour=den_colorby(), group=den_plotInteraction()))
    if(type=="density") g <- g + geom_line(stat="density", size=1) else g <- g + geom_histogram(size=1, position="dodge")
    if(!is.null(limits[[1]]) & !is.null(limits[[2]])) g <- g + xlim(limits[[1]]) + ylim(limits[[2]]) #+ scale_y_continuous(expand = c(0, 0.5))
    g <- g + theme_bw(base_size=14)
    
    if(!is.null(den_colorvec())) g <- g + scale_colour_manual(values=den_colorvec(), limits=levels(d_den()[[den_colorby()]])) +
      scale_fill_manual(values=den_colorvec(), limits=levels(d_den()[[den_colorby()]]))
    if(input$den_facetby!="") g <- g + facet_wrap(as.formula(paste0("~", input$den_facetby)), scales=facetScales())
    g + plottheme
  }
  
  #redraw <- reactive({
  #  input$plot_dec1_brush
  #  rv_plots$dec_holdClick
  #  isolate({
  #    TRUE
  #  })
  #})
  
  tsPlot <- function(type, limits){
    #redraw()
    #isolate({
    ann.types <- paste0("annual-", c("raw", "cumulative", "log", "sqrt"))
    decadal <- !type %in% ann.types
    if(!decadal && preventPlot()) return()
    if(decadal && preventPlot_dec()) return()
    if((decadal & type=="boxplot") || (!decadal & jitterPoints())) pos <- position_jitter(width=0.2, height=0) else pos <- "identity"
    if(decadal && !is.null(dec_colorby())) pos <- position_jitterdodge(jitter.width=0.2, jitter.height=0)
    
    grp <- c("GBM", "RCP", "Model", "Region", "Var", "Vegetation")
    if(!decadal) grp <- grp[grp %in% names(keep())]
    x <- "Year"
    statname <- stat()
    
    if(type=="annual-cumulative"){
      d_keep <- group_by_(keep(), .dots=grp) %>% mutate_(Cumulative_total=lazyeval::interp(~cumsum(x), x=as.name(stat())))
      statname <- "Cumulative_total"
    } else if(type=="annual-raw") {
      d_keep <- keep()
    } else {
      d_keep <- keep_dec()
      x <- "Decade"
      if(type=="barplot"){
        d_keep <- summarise_(d_keep, Decadal_mean=lazyeval::interp(~mean(x), x=as.name(stat())))
        statname <- "Decadal_mean"
      }
      d_source <- d_keep
    }
    
    if(type %in% ann.types){
      d_source <- d()
      clrby <- colorby()
      pInteract <- plotInteraction()
      clrvec <- colorvec()
      fctby <- input$facetby
    } else {
      d_source <- d_keep
      clrby <- dec_colorby()
      pInteract <- dec_plotInteraction()
      clrvec <- dec_colorvec()
      fctby <- input$dec_facetby
    }
      
    g <- ggplot(data=d_source, aes_string(x, statname, colour=clrby))
    g2 <- g + geom_boxplot() + geom_point(position=pos)  +
      geom_boxplot(size=1) + geom_point(position=pos, size=2)
    
    if(!decadal && showLines()) g <- g + geom_line(data=d_keep, aes_string(group=pInteract), alpha=alpha())
    
    if(!decadal){
      g <- g + geom_point(data=d_keep, size=3, alpha=alpha(), position=pos)
      if(type=="annual") g <- g + geom_point(data=exclude(), size=3, shape=21, fill=NA, color="black", alpha=0.25)
    } else {
      if(type=="boxplot"){
        if(nrow(keep_dec())==length(rv_plots$dec_keeprows)){
          if(!is.null(rv_plots$dec_holdBrush) | !is.null(rv_plots$dec_holdClick)){
            if(any(rv_plots$dec_keeprows)) d_keep2 <- d_keep[rv_plots$dec_keeprows,] else d_keep2 <- d_keep
            g <- g + geom_boxplot() + geom_point(position=pos) +
              geom_boxplot(data=d_keep2, size=1) + geom_point(data=d_keep2, position=pos, size=2)
          } else {
            g <- g2
          }
        } else {
          g <- g2
        }
      }
      if(type=="barplot") g <- g + geom_bar(stat="identity")
    }
    
    g <- g + coord_cartesian(xlim=limits[[1]], ylim=limits[[2]]) + theme_bw(base_size=14)
    
    if(!is.null(clrvec)) g <- g + scale_colour_manual(values=clrvec, limits=levels(d_source[[clrby]]))
    if(fctby!="") g <- g + facet_wrap(as.formula(paste0("~", fctby)), scales=facetScales())
    g + plottheme
    #})
  }
  
  output$plot_den1 <- renderPlot({ distPlot("density", list(rv_plots$xden1, rv_plots$yden1)) }, height=function() plotHeight())
  output$plot_den2 <- renderPlot({ distPlot("histogram", list(rv_plots$xden2, rv_plots$yden2)) }, height=function() plotHeight())
  output$plot_ts1 <- renderPlot({ tsPlot("annual-raw", list(rv_plots$xts1, rv_plots$yts1)) }, height=function() plotHeight())
  output$plot_ts2 <- renderPlot({ tsPlot("annual-cumulative", list(rv_plots$xts2, rv_plots$yts2)) }, height=function() plotHeight())
  output$plot_dec1 <- renderPlot({ tsPlot("boxplot", list(rv_plots$xdec1, rv_plots$ydec1)) }, height=function() plotHeight())
  output$plot_dec2 <- renderPlot({ tsPlot("barplot", list(rv_plots$xdec2, rv_plots$ydec2)) }, height=function() plotHeight())
  
  output$info_ts1 <- renderText({ mouseInfo(input$plot_ts1_clk, input$plot_ts1_dblclk, input$plot_ts1_hov, input$plot_ts1_brush) })
  output$info_ts2 <- renderText({ mouseInfo(input$plot_ts2_clk, input$plot_ts2_dblclk, input$plot_ts2_hov, input$plot_ts2_brush) })
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
