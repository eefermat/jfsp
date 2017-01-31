tsModUI <- function(...){
  trans <- c("Log", "Square root")
  modUIprep(trans=trans, ibox="ts", ...)
}

denModUI <- function(...){
  trans <- c("Log", "Square root")
  modUIprep(direction="x", resetOnNew=TRUE, trans=trans, ibox="den", ...)
}

decModUI <- function(...){
  trans <- c(paste(c("Baseline", "Period", "Prior decade"), "anomalies"), "Log", "Square root")
  modUIprep(direction="x", trans=trans, stats=TRUE, ibox="dec", ...)
}

denMod <- function(input, output, session, data){
  ns <- session$ns
  
  source("modules/plots/mod_plot_dist.R", local=TRUE)
  
  variable <- reactive({ as.character(data()$Var[1]) })
  stat <- reactive({ tail(names(data()), 1) })
  d <- reactive({ plotDataPrep(data(), input$transform, input$pooled_vars, input$colorby, input$facetby, stat()) })
  
  rv_plots <- reactiveValues(x1=NULL, y1=NULL, x2=NULL, y2=NULL)
  
  # Distribution - density plot
  
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
  
  # Distribution - histogram
  
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
  preventPlot <- reactive({ nrow(d())==0 | d()$Var[1]!=variable() })
  plotHeight <- reactive({ if(preventPlot()) 0 else 400 })
  plotInteraction <- reactive({ interact(names(d())) })
  
  output$plot1 <- renderPlot({ distPlot("density", list(rv_plots$x1, rv_plots$y1)) }, height=function() plotHeight())
  output$plot2 <- renderPlot({ distPlot("histogram", list(rv_plots$x2, rv_plots$y2)) }, height=function() plotHeight())
  
  output$info1 <- renderText({ mouseInfo(input$plot1_clk, input$plot1_dblclk, input$plot1_hov, input$plot1_brush) })
  output$info2 <- renderText({ mouseInfo(input$plot2_clk, input$plot2_dblclk, input$plot2_hov, input$plot2_brush) })
  
  #outputOptions(output, "plot1", suspendWhenHidden=FALSE)
  #outputOptions(output, "plot2", suspendWhenHidden=FALSE)
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
  
  output$plot1 <- renderPlot({ tsPlot("raw", list(rv_plots$x1, rv_plots$y1)) }, height=function() plotHeight())
  output$plot2 <- renderPlot({ tsPlot("cumulative", list(rv_plots$x2, rv_plots$y2)) }, height=function() plotHeight())
  
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
  
  #outputOptions(output, "plot1", suspendWhenHidden=FALSE)
  #outputOptions(output, "plot2", suspendWhenHidden=FALSE)
  #outputOptions(output, "Selected_obs", suspendWhenHidden=FALSE) # something wrong with reactive behavior here
}

decMod <- function(input, output, session, data){
  ns <- session$ns
  
  source("modules/plots/mod_plot_dec.R", local=TRUE)
  
  variable <- reactive({ as.character(data()$Var[1]) })
  stat <- reactive({ tail(names(data()), 1) })
  d <- reactive({ plotDataPrep(data(), input$transform, input$pooled_vars, input$colorby, input$facetby, stat()) })
  
  keep <- reactive({
    if(is.null(d())) return()
    grp <- c("GBM", "RCP", "Model", "Region", "Var", "Vegetation")
    grp <- grp[grp %in% names(d())]
    mutate(d(), Decade=factor(paste0(Year - Year %% 10, "s"))) %>% group_by_(.dots=c(grp, "Decade")) %>% select(-Year)
  })
  
  keep_dec <- reactive({
    if(is.null(keep())) return()
    summarise_(keep(), Decadal_mean=lazyeval::interp(~mean(x), x=as.name(stat())))
  })
  
  rv_plots <- reactiveValues(
    x1=NULL, y1=NULL, x2=NULL, y2=NULL,
    keeprows=rep(TRUE, nrow(isolate(keep()))),
    keeprows2=rep(TRUE, nrow(isolate(keep_dec()))),
    holdClick=NULL, holdBrush=NULL, holdClick2=NULL, holdBrush2=NULL)
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    rv_plots$keeprows <- rep(TRUE, nrow(keep()))
    rv_plots$keeprows2 <- rep(TRUE, nrow(keep_dec()))
  })
  
  # Decadal series boxplot
  
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
  
  # Toggle points that are clked
  observeEvent(input$plot1_clk, {
    x <- input$plot1_clk
    y <- rv_plots$holdClick
    if(!is.null(x)){
      if(is.null(y) || y$x!=x$x) rv_plots$holdClick <- x
    }
  })
  
  observeEvent(input$plot1_clk, {
    x <- keep()$Decade
    lvls <- levels(x)
    clk <- input$plot1_clk
    if(is.null(clk)) y <- rv_plots$holdClick else y <- clk
    keep_lvls <- lvls[round(y$x)]
    if(!length(keep_lvls) || is.na(keep_lvls)) keep_lvls <- lvls
    if(any(rv_plots$keeprows!=(x==keep_lvls))) rv_plots$keeprows <- x %in% keep_lvls
  })
  
  # Toggle points that are brushed in x axis direction (all y)
  observeEvent(input$plot1_brush, {
    x <- input$plot1_brush
    if(!is.null(x)){
      rv_plots$holdBrush <- x
    }
  })
  
  observeEvent(input$plot1_brush, {
    x <- keep()$Decade
    lvls <- levels(x)
    brush <- input$plot1_brush
    if(is.null(brush)) y <- rv_plots$holdBrush else y <- brush
    intlvls <- round(y$xmin):round(y$xmax)
    rv_plots$keeprows <- x %in% lvls[intlvls]
  })
  
  observeEvent(keep(), {
    rv_plots$keeprows <- rep(TRUE, nrow(keep()))
  })
  
  # Decadal series barplot
  
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
  
  # Toggle points that are clked
  observeEvent(input$plot2_clk, {
    x <- input$plot2_clk
    y <- rv_plots$holdClick2
    if(!is.null(x)){
      if(is.null(y) || y$x!=x$x) rv_plots$holdClick2 <- x
    }
  })
  
  observeEvent(input$plot2_clk, {
    x <- keep_dec()$Decade
    lvls <- levels(x)
    clk <- input$plot2_clk
    if(is.null(clk)) y <- rv_plots$holdClick2 else y <- clk
    keep_lvls <- lvls[round(y$x)]
    if(!length(keep_lvls) || is.na(keep_lvls)) keep_lvls <- lvls
    if(any(rv_plots$keeprows2!=(x==keep_lvls))) rv_plots$keeprows2 <- x %in% keep_lvls
  })
  
  # Toggle points that are brushed in x axis direction (all y)
  observeEvent(input$plot2_brush, {
    x <- input$plot2_brush
    if(!is.null(x)){
      rv_plots$holdBrush2 <- x
    }
  })
  
  observeEvent(input$plot2_brush, {
    x <- keep_dec()$Decade
    lvls <- levels(x)
    brush <- input$plot2_brush
    if(is.null(brush)) y <- rv_plots$holdBrush2 else y <- brush
    intlvls <- round(y$xmin):round(y$xmax)
    rv_plots$keeprows2 <- x %in% lvls[intlvls]
  })
  
  observeEvent(keep_dec(), {
    rv_plots$keeprows2 <- rep(TRUE, nrow(keep_dec()))
  })
  
  colorby <- reactive({ if(input$colorby=="") NULL else input$colorby })
  colorvec <- reactive({ if(is.null(colorby())) NULL else tolpal(length(unique(d()[[colorby()]]))) })
  
  preventPlot <- reactive({ nrow(keep_dec())==0 | keep_dec()$Var[1]!=variable() })
  plotHeight <- reactive({ if(preventPlot()) 0 else 400 })
  plotInteraction <- reactive({ interact(names(d())) })
  
  output$plot1 <- renderPlot({ decPlot("boxplot", list(rv_plots$xdec1, rv_plots$ydec1)) }, height=function() plotHeight())
  output$plot2 <- renderPlot({ decPlot("barplot", list(rv_plots$xdec2, rv_plots$ydec2)) }, height=function() plotHeight())
  
  output$info1 <- renderText({ mouseInfo(input$plot1_clk, input$plot1_dblclk, input$plot1_hov, input$plot1_brush) })
  output$info2 <- renderText({ mouseInfo(input$plot2_clk, input$plot2_dblclk, input$plot2_hov, input$plot2_brush) })
  
  output$statBoxes1 <- renderUI({
    if(nrow(keep())==length(rv_plots$keeprows)){
      x <- keep()[rv_plots$keeprows,]
    } else{
      x <- keep()
    }
    dec <- as.character(sort(unique(x$Decade)))
    if(length(dec) > 1) dec <- paste(dec[1], dec[length(dec)], sep=" - ")
    
    if(preventPlot() || nrow(x)==0) return()
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
    statlab <- list(
      c("Mean", dec),
      c("Min", dec),
      c("Max", dec),
      c("Median", dec),
      c("IQR", dec),
      c("Std Dev", dec)
    )
    val <- map2(statval, 75, ~pTextSize(.x, .y))
    text <- map2(statlab, rep(150, 6), ~pTextSize(.x, .y, margin=0))
    y <- list(
      mean=valueBox(val[[1]], text[[1]], icon=icon(list(src="stat_icon_normal_mean_white.png", width="90px"), lib="local"), color=clrs[1], width=NULL),
      min=valueBox(val[[2]], text[[2]], icon=icon(list(src="stat_icon_normal_min_white.png", width="90px"), lib="local"), color=clrs[2], width=NULL),
      max=valueBox(val[[3]], text[[3]], icon=icon(list(src="stat_icon_normal_max_white.png", width="90px"), lib="local"), color=clrs[3], width=NULL),
      med=valueBox(val[[4]], text[[4]], icon=icon(list(src="stat_icon_normal_median_white.png", width="90px"), lib="local"), color=clrs[4], width=NULL),
      iqr=valueBox(val[[5]], text[[5]], icon=icon(list(src="stat_icon_boxplot_iqr_white.png", width="90px"), lib="local"), color=clrs[5], width=NULL),
      sd=valueBox(val[[6]], text[[6]], icon=icon(list(src="stat_icon_normal_sd_white.png", width="90px"), lib="local"), color=clrs[6], width=NULL)
    )
    
    fluidRow(
      tags$head(tags$style(HTML(".small-box {height: 110px}"))),
      column(6, y$mean, y$med, y$min), column(6, y$sd, y$iqr, y$max)
    )
  })
  
  output$statBoxes2 <- renderUI({
    if(nrow(keep_dec())==length(rv_plots$keeprows2)){
      x <- keep_dec()[rv_plots$keeprows2,]
    } else{
      x <- keep_dec()
    }
    y <- "Decadal_mean"
    
    if(preventPlot() || nrow(x)==0) return()
    x <- group_by(x, Decade) %>% summarise(Decadal_mean=mean(Decadal_mean))
    idx.mn <- which.min(x$Decadal_mean)
    idx.mx <- which.max(x$Decadal_mean)
    idx.dn <- if(nrow(x)==1) NA else seq(which.min(diff(x$Decadal_mean)), length.out=2)
    idx.up <- if(nrow(x)==1) NA else seq(which.max(diff(x$Decadal_mean)), length.out=2)
    tot <- tail(x$Decadal_mean, 1) - x$Decadal_mean[1]
    tot2 <- ifelse(tot < 1 & tot > 0, 1, ifelse(tot < 0 & tot > -1, -1, round(tot)))
    pct <- paste0(round(100*(tail(x$Decadal_mean, 1) / x$Decadal_mean[1] - 1)), "%")
    
    
    if(input$transform=="Baseline anomalies"){
      pct <- NA
    } else if(input$transform=="Period anomalies"){
      pct <- NA
    } else if(input$transform=="Prior decade anomalies"){
      tot2 <- NA
      pct <- NA
    }
    
    clrs <- c("yellow", "orange", "purple", "red", "blue", "navy")
    statval <- list(
      mn=round(x$Decadal_mean[idx.mn]),
      mx=round(x$Decadal_mean[idx.mx]),
      dn=if(is.na(idx.dn[1])) NA else round(diff(x$Decadal_mean)[idx.dn[1]]),
      up=if(is.na(idx.up[1])) NA else round(diff(x$Decadal_mean)[idx.up[1]]),
      totdif=tot2,
      totpct=pct
    )
    
    src.dnup <- c("stat_icon_bar_deltaNeg_white.png", "stat_icon_bar_deltaPos_white.png")
    if(!is.na(statval$dn[1]) && statval$dn > 0) src.dnup[1] <- src.dnup[2]
    if(!is.na(statval$up[1]) && statval$up < 0) src.dnup[2] <- src.dnup[1]
    if(tot < 0){
      src.totals <- c("stat_icon_ts_deltaDec_white.png", "stat_icon_ts_deltaPctDec_white.png")
    } else {
      src.totals <- c("stat_icon_ts_deltaInc_white.png", "stat_icon_ts_deltaPctInc_white.png")
    }
    dec <- if(nrow(x)==1) paste(x$Decade[1]) else paste(x$Decade[c(1, nrow(x))], collapse=" - ")
    
    statlab <- list(
      c("Min", paste(x$Decade[idx.mn])),
      c("Max", paste(x$Decade[idx.mx])),
      c("Min growth", paste(x$Decade[idx.dn], collapse=" - ")),
      c("Max growth", paste(x$Decade[idx.up], collapse=" - ")),
      c("Total change", dec),
      c("% change", dec)
    )
    val <- map2(statval, 75, ~pTextSize(.x, .y))
    text <- map2(statlab, rep(150, 6), ~pTextSize(.x, .y, margin=0))
    y <- list(
      mn=valueBox(val[[1]], text[[1]], icon=icon(list(src="stat_icon_normal_min_white.png", width="90px"), lib="local"), color=clrs[1], width=NULL),
      mx=valueBox(val[[2]], text[[2]], icon=icon(list(src="stat_icon_normal_max_white.png", width="90px"), lib="local"), color=clrs[2], width=NULL),
      dn=valueBox(val[[3]], text[[3]], icon=icon(list(src=src.dnup[1], width="90px"), lib="local"), color=clrs[3], width=NULL),
      up=valueBox(val[[4]], text[[4]], icon=icon(list(src=src.dnup[2], width="90px"), lib="local"), color=clrs[4], width=NULL),
      totdif=valueBox(val[[5]], text[[5]], icon=icon(list(src=src.totals[1], width="90px"), lib="local"), color=clrs[5], width=NULL),
      totpct=valueBox(val[[6]], text[[6]], icon=icon(list(src=src.totals[2], width="90px"), lib="local"), color=clrs[6], width=NULL)
    )
    
    fluidRow(
      tags$head(tags$style(HTML(".small-box {height: 110px}"))),
      column(6, y$totdif, y$dn, y$mn), column(6, y$totpct, y$up, y$mx)
    )
  })
  
  #outputOptions(output, "plot1", suspendWhenHidden=FALSE)
  #outputOptions(output, "plot2", suspendWhenHidden=FALSE)
  #outputOptions(output, "statBoxes1", suspendWhenHidden=FALSE)
  #outputOptions(output, "statBoxes2", suspendWhenHidden=FALSE)
}
