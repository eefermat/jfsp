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

denMod <- function(input, output, session, data, metric, axis_scale){
  ns <- session$ns
  source("modules/plots/mod_plot_dist.R", local=TRUE)
  
  variable <- reactive({ as.character(data()$Var[1]) })
  stat <- reactive({ tail(names(data()), 1) })
  d <- reactive({ plotDataPrep(data(), input$transform, input$pooled_vars, input$colorby, input$facetby, stat()) })
  
  rv_plots <- reactiveValues(x1=NULL, y1=NULL, x2=NULL, y2=NULL)
  source("modules/plots/mod_plot_general_observers.R", local=TRUE)
  
  yrs <- reactive({ range(d()$Year) })
  axislab <- reactive({ 
    primeAxis(stat(), variable(), transform=input$transform, metric=metric(), axis_scale=axis_scale())
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
  
  outputOptions(output, "plot1", suspendWhenHidden=FALSE)
  outputOptions(output, "plot2", suspendWhenHidden=FALSE)
}

tsMod <- function(input, output, session, data, metric, axis_scale){
  ns <- session$ns
  source("modules/plots/mod_plot_ts.R", local=TRUE)
  
  variable <- reactive({ as.character(data()$Var[1]) })
  stat <- reactive({ tail(names(data()), 1) })
  d <- reactive({ plotDataPrep(data(), input$transform, input$pooled_vars, input$colorby, input$facetby, stat()) })
  
  rv_plots <- reactiveValues(x1=NULL, y1=NULL, x2=NULL, y2=NULL, keeprows=rep(TRUE, nrow(isolate(d()))))
  source("modules/plots/mod_plot_general_observers.R", local=TRUE)
  source("modules/plots/mod_plot_ts_observers.R", local=TRUE)
  
  yrs <- reactive({ range(d()$Year) })
  axislab <- reactive({
    primeAxis(stat(), variable(), transform=input$transform, metric=metric(), axis_scale=axis_scale()) 
  })
  axislab2 <- reactive({ 
    primeAxis(stat(), variable(), prefix="Cumulative total", transform=input$transform, metric=metric(), axis_scale=axis_scale()) 
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
  
  outputOptions(output, "plot1", suspendWhenHidden=FALSE)
  outputOptions(output, "plot2", suspendWhenHidden=FALSE)
  #outputOptions(output, "Selected_obs", suspendWhenHidden=FALSE) # something wrong with reactive behavior here
}

decMod <- function(input, output, session, data, metric, axis_scale){
  ns <- session$ns
  source("modules/plots/mod_plot_dec.R", local=TRUE)
  
  kilo_mega <- function(x){
    if(abs(x) < 1e4) paste0(x) else 
      if(abs(x) < 1e5) paste0(round(x/1000, 1), "K") else 
        if(abs(x) < 1e6) paste0(round(x/1000), "K") else
          paste0(round(x/1e6, 2), "M") 
  }
  
  variable <- reactive({ as.character(data()$Var[1]) })
  stat <- reactive({ tail(names(data()), 1) })
  d <- reactive({ plotDataPrep(data(), input$transform, input$pooled_vars, input$colorby, input$facetby, stat()) })
  
  keep <- reactive({
    if(is.null(d())) return()
    grp <- c("RCP", "Model", "Region", "Var", "Vegetation")
    grp <- grp[grp %in% names(d())]
    mutate(d(), Decade=factor(paste0(Year - Year %% 10, "s"))) %>% group_by_(.dots=c(grp, "Decade")) %>% select(-Year)
  })
  
  keep_dec <- reactive({
    if(is.null(keep())) return()
    x <- setNames("Decadal_mean", stat())
    summarise_(keep(), Decadal_mean=lazyeval::interp(~mean(x), x=as.name(stat()))) %>%
      rename_(.dots=x)
  })
  
  rv_plots <- reactiveValues(
    x1=NULL, y1=NULL, x2=NULL, y2=NULL,
    keeprows=rep(TRUE, nrow(isolate(keep()))),
    keeprows2=rep(TRUE, nrow(isolate(keep_dec()))),
    holdClick=NULL, holdBrush=NULL, holdClick2=NULL, holdBrush2=NULL)
  source("modules/plots/mod_plot_general_observers.R", local=TRUE)
  source("modules/plots/mod_plot_dec_observers.R", local=TRUE)
  
  yrs <- reactive({ range(d()$Year) })
  axislab <- reactive({ 
    primeAxis(stat(), variable(), transform=input$transform, metric=metric(), axis_scale=axis_scale()) 
  })
  axislab2 <- reactive({ 
    primeAxis(stat(), variable(), suffix="decadal mean", transform=input$transform, metric=metric(), axis_scale=axis_scale())
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
    )) %>% round %>% unlist %>% map_chr(~kilo_mega(.x))

    clrs <- c("yellow", "orange", "purple", "red", "blue", "navy")
    statval <- c(x[1:4], paste(x[5], "-", x[6]), x[7])
    statlab <- list(
      c("Mean", dec),
      c("Min", dec),
      c("Max", dec),
      c("Median", dec),
      c("IQR", dec),
      c("Std Dev", dec)
    )
    val <- map2(statval, c(rep(75, 4), 50, 75), ~pTextSize(.x, .y))
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
    
    if(preventPlot() || nrow(x)==0) return()
    dots <- paste0("mean(", stat(), ")")
    x <- group_by(x, Decade) %>% summarise_(.dots=list(Decadal_mean=dots)) %>%
      rename_(.dots=setNames("Decadal_mean", stat()))
    idx.mn <- which.min(x[[stat()]])
    idx.mx <- which.max(x[[stat()]])
    idx.dn <- if(nrow(x)==1) NA else seq(which.min(diff(x[[stat()]])), length.out=2)
    idx.up <- if(nrow(x)==1) NA else seq(which.max(diff(x[[stat()]])), length.out=2)
    tot <- tail(x[[stat()]], 1) - x[[stat()]][1]
    tot2 <- ifelse(tot < 1 & tot > 0, 1, ifelse(tot < 0 & tot > -1, -1, round(tot)))
    pct <- paste0(round(100*(tail(x[[stat()]], 1) / x[[stat()]][1] - 1)), "%")
    
    
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
      mn=kilo_mega(round(x[[stat()]][idx.mn])),
      mx=kilo_mega(round(x[[stat()]][idx.mx])),
      dn=if(is.na(idx.dn[1])) NA else kilo_mega(round(diff(x[[stat()]])[idx.dn[1]])),
      up=if(is.na(idx.up[1])) NA else kilo_mega(round(diff(x[[stat()]])[idx.up[1]])),
      totdif=kilo_mega(tot2),
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
  
  outputOptions(output, "plot1", suspendWhenHidden=FALSE)
  outputOptions(output, "plot2", suspendWhenHidden=FALSE)
  outputOptions(output, "statBoxes1", suspendWhenHidden=FALSE)
  outputOptions(output, "statBoxes2", suspendWhenHidden=FALSE)
}
