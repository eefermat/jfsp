yrs <- reactive({ seq(inp()$yrs[1], inp()$yrs[2]) })
regions <- reactive({ 
  paste(inp()$reg.names, "fire management zone") })
variable <- reactive({ as.character(data()$Var[1]) })
uni <- reactive({
  x <- if(metric()) "km^2" else "acres" 
  if(variable()=="Fire Count") x <- "fires"
  if(variable()=="Vegetation Age") x <- "years"
  x
})

output$report <- downloadHandler(
  filename = "report.pdf",
  content = function(file){
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report.Rmd", tempReport, overwrite=TRUE)
    params <- list(
      years=yrs(),
      n=length(yrs()),
      variable=variable(),
      x=ts_list()$x,
      units=uni(),
      regions=regions(),
      plot_ts=ts_list()$p,
      plot_den=den_list()$p,
      plot_dec=dec_list()$p,
      rcps=inp()$rcps,
      gcms=inp()$gcms,
      veg=inp()$veg
    )
    rmarkdown::render(tempReport, output_file=file, params=params, envir=new.env(parent=globalenv())
    )
  }
)
