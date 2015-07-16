source("data.R")

shinyUI(fluidPage(
  titlePanel('Download project data'),
    mainPanel(
      h3("Download data for project:"),
      selectizeInput(inputId = "project", label = "Choose a dataset:", choices = project_names, selected = NULL, multiple = FALSE,
                     options = list(
                       placeholder = 'Please select an option below',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      downloadButton('downloadData', 'Download'),
      h3("Download aggregated project data for period:"),
      dateRangeInput('dateRange', "Date range:"),
      downloadButton("downloadAggregatedData","Download aggregated data"),
      h3("Download yearly report data for period:"),
      textInput('year', "Year:"),
      downloadButton("downloadYearReportData","Download yearly report data")
    )
  
))
