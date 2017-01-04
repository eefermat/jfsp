dbmodUI <- function(id, tab_name){
  ns <- NS(id)

    tabItem(tabName=tab_name,
      fluidRow(
        column(4, leafletOutput(ns("Map"), width="100%")),
        column(8, plotOutput(ns("plot1"), click=ns("plot1_click"), dblclick=ns("plot1_dblclick"), hover=ns("plot1_hover"),
                             brush=brushOpts(id=ns("plot1_brush"), resetOnNew=TRUE)))
      ),
      br(),
      fluidRow(
        column(4,
               h4("Selected observations")
        ),
        column(2,
               selectizeInput(ns("colorby"), label=NULL, choices=groupby_vars, selected="", width="100%", options=list(placeholder='Color by...')),
               checkboxInput("cumulative", "Cumulative total", FALSE, width="100%")
        ),
        column(2, selectizeInput(ns("facetby"), label=NULL, choices=groupby_vars, selected="", width="100%", options=list(placeholder='Facet by...'))),
        column(2, actionButton(ns("exclude_toggle"), "Toggle points", class="btn-block")),
        column(2, actionButton(ns("exclude_reset"), "Reset", class="btn-block"))
      ),
      br(),
      fluidRow(
        column(6, verbatimTextOutput(ns("info"))),
        column(6, div(DT::dataTableOutput(ns('Selected_obs')), style = "font-size: 75%"))
      )
    )
  
}

dbmod <- function(input, output, session, xdata, variable, stat, allRows, clickExclude, alpha, showLines, jitterPoints){
  ns <- session$ns
  
  source("utils.R", local=TRUE)
  
  rv_plot1 <- reactiveValues(x=NULL, y=NULL, keeprows=rep(TRUE, nrow(isolate(xdata()))))
  
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      rv_plot1$x <- c(brush$xmin, brush$xmax)
      rv_plot1$y <- c(brush$ymin, brush$ymax)
    } else {
      rv_plot1$x <- NULL
      rv_plot1$y <- NULL
    }
  })
  
  observeEvent(xdata(), {
    rv_plot1$keeprows <- rep(TRUE, nrow(xdata()))
  })
  
  # Toggle points that are clicked
  observeEvent(input$plot1_click, {
    if(clickExclude()){
      res <- nearPoints(xdata(), input$plot1_click, allRows=TRUE)
      rv_plot1$keeprows <- xor(rv_plot1$keeprows, res$selected_)
    } else {
      rv_plot1$keeprows <- rep(TRUE, nrow(xdata()))
    }
  })
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(xdata(), input$plot1_brush, allRows=TRUE)
    rv_plot1$keeprows <- xor(rv_plot1$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    rv_plot1$keeprows <- rep(TRUE, nrow(xdata()))
  })
  
  colorby <- reactive({ if(input$colorby=="") NULL else input$colorby })
  keep    <- reactive({
    req(rv_plot1$keeprows)
    if(length(rv_plot1$keeprows) != nrow(xdata())) return()
    filter(xdata(), rv_plot1$keeprows)
  })
  exclude <- reactive({ filter(xdata(), !rv_plot1$keeprows) })
  colorvec <- reactive({ if(is.null(colorby())) NULL else tolpal(length(unique(keep()[[colorby()]]))) })
  
  output$plot1 <- renderPlot({
    if(nrow(xdata())==0 | nrow(xdata())!=length(rv_plot1$keeprows) | xdata()$Var[1]!=variable) return()
    if(jitterPoints()) pos <- position_jitter(w=0.2, h=0) else pos <- "identity"
    
    g <- ggplot(data=keep(), aes_string("Year", stat, colour=colorby())) 
    if(showLines()) g <- g + geom_line(aes(group=interaction(GBM, RCP, Model, Region, Var, Vegetation)), alpha=alpha())
    
    g <- g + geom_point(size=3, alpha=alpha(), position=pos) +
      geom_point(data=exclude(), shape=21, fill=NA, color="black", alpha=0.25) +
      coord_cartesian(xlim=rv_plot1$x, ylim=rv_plot1$y) + theme_bw(base_size = 14)
    
    if(!is.null(colorvec())) g <- g + scale_colour_manual(values=colorvec())
    if(input$facetby!="") g <- g + facet_wrap(as.formula(paste0("~", input$facetby)), scales=input$facet_scales)
    g + plottheme
  })
  
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
  
  output$Selected_obs <- DT::renderDataTable({
    x <- if(is.null(input$plot1_brush)) nearPoints(xdata(), input$plot1_click, allRows=allRows()) else
      brushedPoints(xdata(), input$plot1_brush, allRows=allRows())
    
    if(nrow(x)==0) return(
      DT::datatable(x, options=list(
      lengthMenu=list(c(5, 10, 25), c('5', '10', '25')), pageLength=5, emptyTable="No observations selected in plot", searching=FALSE)) %>%
      formatStyle(columns="Var", backgroundColor="white", target='row')
    )
    
    if(!"selected_" %in% names(x)) x <- mutate(x, selected_=TRUE)
    x <- mutate(x, selected_=paste0(x[[input$colorby]], "_", x$selected_))
    clrs <- tableRowColors(x, input$colorby, colorvec(), "25")
    
    DT::datatable(x, options=list(
      lengthMenu=list(c(5, 10, 25), c('5', '10', '25')), pageLength=5, searching=FALSE,
      columnDefs=list(list(visible=FALSE, targets=ncol(x))))) %>%
      formatStyle(columns="selected_", backgroundColor=clrs, target='row')
  })

}
