
shinyUI(fluidPage(
  titlePanel('Download project data'),
    mainPanel(
      h3("Download data for project:"),
      uiOutput("projectselector"),
      downloadButton('downloadData', 'Download'),
      h3("Download aggregated project data for period:"),
      dateRangeInput('dateRange', "Date range:"),
      downloadButton("downloadAggregatedData","Download aggregated data"),
      h3("Download yearly report data for period:"),
      textInput('year', "Year:"),
      downloadButton("downloadYearReportData","Download yearly report data")
    )
  
))
