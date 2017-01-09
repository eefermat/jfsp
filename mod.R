dbmodUI <- function(id, tab_name){
  ns <- NS(id)

    tabItem(tabName=tab_name,
      fluidRow(
        column(2, selectizeInput(ns("colorby"), label=NULL, choices=groupby_vars, selected="", width="100%", options=list(placeholder='Color by...'))),
        column(2, selectizeInput(ns("facetby"), label=NULL, choices=groupby_vars, selected="", width="100%", options=list(placeholder='Facet by...'))),
        column(2, selectizeInput(ns("pooled_vars"), label=NULL, choices=pooled_options, selected=pooled_options[1], width="100%")),
        column(3, actionButton(ns("exclude_toggle"), "Toggle points", class="btn-block")),
        column(3, actionButton(ns("exclude_reset"), "Reset", class="btn-block"))
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
        tabBox(
          tabPanel("Averages",
                   plotOutput(ns("plot_dec2"), height="auto", click=ns("plot_dec2_clk"), dblclick=ns("plot_dec2_dblclk"), hover=ns("plot_dec2_hov"),
                              brush=brushOpts(id=ns("plot_dec2_brush"), resetOnNew=TRUE)), value=ns("dec_barplot")
          ),
          tabPanel("Observations",
                   plotOutput(ns("plot_dec1"), height="auto", click=ns("plot_dec1_clk"), dblclick=ns("plot_dec1_dblclk"), hover=ns("plot_dec1_hov"),
                              brush=brushOpts(id=ns("plot_dec1_brush"), resetOnNew=TRUE)), value=ns("dec_boxplot")
          ),
          ns(id="box_dec"), selected=ns("dec_boxplot"), title="Decadal change", width=4, side="right"
        )
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

dbmod <- function(input, output, session, data, variable, stat, alpha, showLines, jitterPoints, facetScales){
  ns <- session$ns
  
  d <- reactive({
    if(input$pooled_vars=="Unique observations") return(data())
    grp <- c("GBM", "RCP", "Model", "Region", "Vegetation")
    grp <- c(grp[grp %in% c(input$colorby, input$facetby)], "Var", "Year")
    group_by_(data(), .dots=grp) %>% summarise_(Avg=lazyeval::interp(~round(mean(x)), x=as.name(stat))) %>% 
      rename_(.dots=setNames("Avg", stat)) %>% ungroup
  })
  
  rv_plots <- reactiveValues(xts1=NULL, yts1=NULL, xts2=NULL, yts2=NULL, 
                             xden1=NULL, yden1=NULL, xden2=NULL, yden2=NULL,
                             keeprows=rep(TRUE, nrow(isolate(d()))),
                             xdec1=NULL, ydec1=NULL, xdec2=NULL, ydec2=NULL, dec_keeprows=rep(TRUE, nrow(isolate(d()))))
  
  source("mod_utils.R", local=TRUE)
  source("mod_observers.R", local=TRUE)
  
  colorby <- reactive({ if(input$colorby=="") NULL else input$colorby })
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
  colorvec <- reactive({ if(is.null(colorby())) NULL else tolpal(length(unique(d()[[colorby()]]))) })
  
  preventPlot <- reactive({ nrow(d())==0 | nrow(d())!=length(rv_plots$keeprows) | d()$Var[1]!=variable })
  plotHeight <- reactive({ if(preventPlot()) 0 else 400 })
  plotInteraction <- reactive({
    grp <- c("GBM", "RCP", "Model", "Region", "Vegetation")
    x <- grp[grp %in% names(d())]
    if(!length(x)) return()
    paste0("interaction(", paste0(x, collapse=","), ")")
  })
  
  keep_dec <- reactive({
    grp <- c("GBM", "RCP", "Model", "Region", "Var", "Vegetation")
    grp <- grp[grp %in% names(keep())]
    mutate(keep(), Decade=Year - Year %% 10) %>% group_by_(.dots=c(grp, "Decade")) %>% select(-Year)
  })
  
  distPlot <- function(type, limits){
    if(preventPlot()) return()
    g <- ggplot(data=d(), aes_string(stat, fill=colorby(), colour=colorby(), group=plotInteraction()))
    if(type=="density") g <- g + geom_line(stat="density", size=1) else g <- g + geom_histogram(size=1, position="dodge")
    if(!is.null(limits[[1]]) & !is.null(limits[[2]])) g <- g + xlim(limits[[1]]) + ylim(limits[[2]]) #+ scale_y_continuous(expand = c(0, 0.5))
    g <- g + theme_bw(base_size=14)
    
    if(!is.null(colorvec())) g <- g + scale_colour_manual(values=colorvec(), limits=levels(d()[[colorby()]])) +
      scale_fill_manual(values=colorvec(), limits=levels(d()[[colorby()]]))
    if(input$facetby!="") g <- g + facet_wrap(as.formula(paste0("~", input$facetby)), scales=facetScales())
    g + plottheme
  }
  
  tsPlot <- function(type, limits){
    if(preventPlot()) return()
    decadal <- !type %in% c("annual", "cumulative")
    if((decadal & type=="boxplot") || (!decadal & jitterPoints())) pos <- position_jitter(width=0.2, height=0) else pos <- "identity"
    if(decadal & !is.null(colorby())) pos <- position_jitterdodge(jitter.width=0.2, jitter.height=0)
    grp <- c("GBM", "RCP", "Model", "Region", "Var", "Vegetation")
    grp <- grp[grp %in% names(keep())]
    x <- "Year"
    statname <- stat
    d_source <- d()
    
    if(type=="cumulative"){
      d_keep <- group_by_(keep(), .dots=grp) %>% mutate_(Cumulative_total=lazyeval::interp(~cumsum(x), x=as.name(stat)))
      statname <- "Cumulative_total"
    } else if(type=="annual") {
      d_keep <- keep()
    } else {
      x <- "Decade"
      d_keep <- keep_dec()
      if(type=="barplot"){
        d_keep <- summarise_(d_keep, Decadal_mean=lazyeval::interp(~mean(x), x=as.name(stat)))
        statname <- "Decadal_mean"
      }
      d_source <- d_keep
    }
    
    g <- ggplot(data=d_source, aes_string(x, statname, colour=colorby()))
    
    if(!decadal && showLines()) g <- g + geom_line(data=d_keep, aes_string(group=plotInteraction()), alpha=alpha())
    x <<- d_source
    if(!decadal){
      g <- g + geom_point(data=d_keep, size=3, alpha=alpha(), position=pos)
      if(type=="annual") g <- g + geom_point(data=exclude(), size=3, shape=21, fill=NA, color="black", alpha=0.25)
    } else {
      if(type=="boxplot"){
        g <- g + geom_boxplot(aes(factor(Decade))) + geom_point(aes(factor(Decade)), position=pos)
      }
      if(type=="barplot") g <- g + geom_bar(stat="identity")
    }
    
    g <- g + coord_cartesian(xlim=limits[[1]], ylim=limits[[2]]) + theme_bw(base_size=14)
    
    if(!is.null(colorvec())) g <- g + scale_colour_manual(values=colorvec(), limits=levels(d()[[colorby()]]))
    if(input$facetby!="") g <- g + facet_wrap(as.formula(paste0("~", input$facetby)), scales=facetScales())
    g + plottheme
  }
  
  output$plot_den1 <- renderPlot({ distPlot("density", list(rv_plots$xden1, rv_plots$yden1)) }, height=function() plotHeight())
  output$plot_den2 <- renderPlot({ distPlot("histogram", list(rv_plots$xden2, rv_plots$yden2)) }, height=function() plotHeight())
  output$plot_ts1 <- renderPlot({ tsPlot("annual", list(rv_plots$xts1, rv_plots$yts1)) }, height=function() plotHeight())
  output$plot_ts2 <- renderPlot({ tsPlot("cumulative", list(rv_plots$xts2, rv_plots$yts2)) }, height=function() plotHeight())
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

}
