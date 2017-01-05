dbmodUI <- function(id, tab_name){
  ns <- NS(id)

    tabItem(tabName=tab_name,
      fluidRow(
        column(4, leafletOutput(ns("Map"), width="100%")),
        column(8,
               plotOutput(ns("plot_ts1"), click=ns("plot1_click"), dblclick=ns("plot1_dblclick"), hover=ns("plot1_hover"),
                             brush=brushOpts(id=ns("plot1_brush"), resetOnNew=TRUE))#,
               #conditionalPanel(condition=sprintf("input['%s'] == true", ns("cumulative")), plotOutput(ns("plot_ts2")))
        )
      ),
      br(),
      fluidRow(
        column(4,
               h4("Selected observations")
        ),
        column(2,
               selectizeInput(ns("colorby"), label=NULL, choices=groupby_vars, selected="", width="100%", options=list(placeholder='Color by...')),
               checkboxInput(ns("cumulative"), "Cumulative total", FALSE, width="100%")
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

dbmod <- function(input, output, session, xdata, variable, stat, alpha, showLines, jitterPoints){
  ns <- session$ns
  rv_plot1 <- reactiveValues(x=NULL, y=NULL, keeprows=rep(TRUE, nrow(isolate(xdata()))))
  source("mod_utils.R", local=TRUE)
  source("mod_observers.R", local=TRUE)
  
  colorby <- reactive({ if(input$colorby=="") NULL else input$colorby })
  keep    <- reactive({
    req(rv_plot1$keeprows)
    if(length(rv_plot1$keeprows) != nrow(xdata())) return()
    filter(xdata(), rv_plot1$keeprows)
  })
  exclude <- reactive({
    req(rv_plot1$keeprows)
    if(length(rv_plot1$keeprows) != nrow(xdata())) return()
    filter(xdata(), !rv_plot1$keeprows)
  })
  colorvec <- reactive({ if(is.null(colorby())) NULL else tolpal(length(unique(xdata()[[colorby()]]))) })
  
  output$plot_ts1 <- renderPlot({
    if(nrow(xdata())==0 | nrow(xdata())!=length(rv_plot1$keeprows) | xdata()$Var[1]!=variable) return()
    if(jitterPoints()) pos <- position_jitter(w=0.2, h=0) else pos <- "identity"
    
    g <- ggplot(data=xdata(), aes_string("Year", stat, colour=colorby()))
    if(showLines()) g <- g + geom_line(data=keep(), aes(group=interaction(GBM, RCP, Model, Region, Var, Vegetation)), alpha=alpha())
    
    g <- g + geom_point(data=keep(), size=5, alpha=alpha(), position=pos) +
      geom_point(data=exclude(), size=3, shape=21, fill=NA, color="black", alpha=0.25) +
      coord_cartesian(xlim=rv_plot1$x, ylim=rv_plot1$y) + theme_bw(base_size = 14)
    
    if(!is.null(colorvec())) g <- g + scale_colour_manual(values=colorvec(), limits=levels(xdata()[[colorby()]]))
    if(input$facetby!="") g <- g + facet_wrap(as.formula(paste0("~", input$facetby)), scales=input$facet_scales)
    g + plottheme
  })
  
  output$plot_ts2 <- renderPlot({
    return()
    if(nrow(xdata())==0 | nrow(xdata())!=length(rv_plot1$keeprows) | xdata()$Var[1]!=variable) return()
    if(jitterPoints()) pos <- position_jitter(w=0.2, h=0) else pos <- "identity"
    
    grp <- c("GBM", "RCP", "Model", "Region", "Var", "Vegetation")
    d_keep <- group_by_(keep(), .dots=grp) %>% mutate_(`Cumulative_total`=lazyeval::interp(~cumsum(x), x=as.name(stat)))
    statname <- "Cumulative_total"
    
    g <- ggplot(data=d_keep, aes_string("Year", statname, colour=colorby()))
    if(showLines()) g <- g + geom_line(aes(group=interaction(GBM, RCP, Model, Region, Var, Vegetation)), alpha=alpha())
    
    g <- g + geom_point(size=3, alpha=alpha(), position=pos) +
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
    # ignore input$plot1_click for table updates; click obs-toggling removes all selection
    if(is.null(input$plot1_brush)){
      x <- slice(xdata(), 0)
    } else {
      x <- brushedPoints(xdata(), input$plot1_brush, allRows=TRUE)
    }
    if(nrow(x)==0 || nrow(filter(x, selected_))==0) return()
    x <- mutate(x, included_=rv_plot1$keeprows) %>% filter(selected_) %>% mutate(selected_=NULL)
    x <- mutate(x, included_=paste0(x[[input$colorby]], "_", x$included_))
    clrs <- tableRowColors(x, input$colorby, colorvec(), "35")
    
    DT::datatable(x, options=list(
      lengthMenu=list(c(5, 10, 25), c('5', '10', '25')), pageLength=5, searching=FALSE,
      columnDefs=list(list(visible=FALSE, targets=ncol(x))))) %>%
      formatStyle(columns="included_", backgroundColor=clrs, target='row')
  })

}
