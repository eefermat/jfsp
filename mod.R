dbmodUI <- function(id, tab_name){
  ns <- NS(id)

    tabItem(tabName=tab_name,
      fluidRow(
        box(plotOutput(ns("plot_density1"), height="auto",
                       click=ns("plot_density1_click"), dblclick=ns("plot_density1_dblclick"), hover=ns("plot_density1_hover"),
                       brush=brushOpts(id=ns("plot_density1_brush"), resetOnNew=TRUE)), title="Period density", width=4),
        box(
          plotOutput(ns("plot_ts1"), height="auto", click=ns("plot1_click"), dblclick=ns("plot1_dblclick"), hover=ns("plot1_hover"),
                     brush=brushOpts(id=ns("plot1_brush"), resetOnNew=TRUE)),
               #conditionalPanel(condition=sprintf("input['%s'] == true", ns("cumulative")), plotOutput(ns("plot_ts2")))
          title="Annual time series", width=8
        )
      ),
      br(),
      fluidRow(
        column(2,
               selectizeInput(ns("colorby"), label=NULL, choices=groupby_vars, selected="", width="100%", options=list(placeholder='Color by...'))
               #,checkboxInput(ns("cumulative"), "Cumulative total", FALSE, width="100%")
        ),
        column(2, selectizeInput(ns("facetby"), label=NULL, choices=groupby_vars, selected="", width="100%", options=list(placeholder='Facet by...'))),
        column(2, selectizeInput(ns("pooled_vars"), label=NULL, choices=pooled_vars, selected=pooled_vars[1], width="100%", options=list(placeholder='Pooled variables...'))),
        column(3, actionButton(ns("exclude_toggle"), "Toggle points", class="btn-block")),
        column(3, actionButton(ns("exclude_reset"), "Reset", class="btn-block"))
      ),
      br(),
      fluidRow(
        column(4, verbatimTextOutput(ns("info_density"))),
        column(8, verbatimTextOutput(ns("info")))
      ),
      fluidRow(
        column(4),
        column(8, div(DT::dataTableOutput(ns('Selected_obs')), style = "font-size: 75%"))
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
  
  rv_plots <- reactiveValues(x=NULL, y=NULL, xden=NULL, yden=NULL, keeprows=rep(TRUE, nrow(isolate(d()))))
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
  
  output$plot_density1 <- renderPlot({
    if(preventPlot()) return()
    g <- ggplot(data=d(), aes_string(stat, colour=colorby(), group=plotInteraction()))
    g <- g + geom_line(stat="density", size=1)
    if(!is.null(rv_plots$xden) & !is.null(rv_plots$yden)) g <- g + xlim(rv_plots$xden) + ylim(rv_plots$yden) #+ scale_y_continuous(expand = c(0, 0.5))
    g <- g + theme_bw(base_size=14)
    
    if(!is.null(colorvec())) g <- g + scale_colour_manual(values=colorvec(), limits=levels(d()[[colorby()]]))
    if(input$facetby!="") g <- g + facet_wrap(as.formula(paste0("~", input$facetby)), scales=facetScales())
    g + plottheme
  }, height=function() plotHeight())
  
  output$plot_ts1 <- renderPlot({
    if(preventPlot()) return()
    if(jitterPoints()) pos <- position_jitter(w=0.2, h=0) else pos <- "identity"
    
    g <- ggplot(data=d(), aes_string("Year", stat, colour=colorby()))
    if(showLines()) g <- g + geom_line(data=keep(), aes_string(group=plotInteraction()), alpha=alpha())
    
    g <- g + geom_point(data=keep(), size=3, alpha=alpha(), position=pos) +
      geom_point(data=exclude(), size=3, shape=21, fill=NA, color="black", alpha=0.25) +
      coord_cartesian(xlim=rv_plots$x, ylim=rv_plots$y) + theme_bw(base_size=14)
    
    if(!is.null(colorvec())) g <- g + scale_colour_manual(values=colorvec(), limits=levels(d()[[colorby()]]))
    if(input$facetby!="") g <- g + facet_wrap(as.formula(paste0("~", input$facetby)), scales=facetScales())
    g + plottheme
  }, height=function() plotHeight())
  #outputOptions(output, "plot_ts1", suspendWhenHidden=FALSE)
  
  output$plot_ts2 <- renderPlot({
    return()
    if(nrow(d())==0 | nrow(d())!=length(rv_plots$keeprows) | d()$Var[1]!=variable) return()
    if(jitterPoints()) pos <- position_jitter(w=0.2, h=0) else pos <- "identity"
    
    grp <- c("GBM", "RCP", "Model", "Region", "Var", "Vegetation")
    d_keep <- group_by_(keep(), .dots=grp) %>% mutate_(`Cumulative_total`=lazyeval::interp(~cumsum(x), x=as.name(stat)))
    statname <- "Cumulative_total"
    
    g <- ggplot(data=d_keep, aes_string("Year", statname, colour=colorby()))
    if(showLines()) g <- g + geom_line(aes_string(group=plotInteraction()), alpha=alpha())
    
    g <- g + geom_point(size=3, alpha=alpha(), position=pos) +
      coord_cartesian(xlim=rv_plots$x, ylim=rv_plots$y) + theme_bw(base_size=14)
    
    if(!is.null(colorvec())) g <- g + scale_colour_manual(values=colorvec(), limits=levels(d()[[colorby()]]))
    if(input$facetby!="") g <- g + facet_wrap(as.formula(paste0("~", input$facetby)), scales=facetScales())
    g + plottheme
  }, height=0)
  
  output$info <- renderText({
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
      "click: ", xy_str(input$plot1_click),
      "dblclick: ", xy_str(input$plot1_dblclick),
      "hover: ", xy_str(input$plot1_hover),
      "brush: ", xy_range_str(input$plot1_brush)
    )
  })
  
  output$info_density <- renderText({
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
      "click: ", xy_str(input$plot_density1_click),
      "dblclick: ", xy_str(input$plot_density1_dblclick),
      "hover: ", xy_str(input$plot_density1_hover),
      "brush: ", xy_range_str(input$plot_density1_brush)
    )
  })
  
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

}
