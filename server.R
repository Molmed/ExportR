library(dplyr)
library(projmanr)

shinyServer(function(input, output) {
  
  connection <- function(){
    RSQLServer::src_sqlserver("MS_SQL", file='sql.yaml', database = 'ProjectMan')
  }
  
  project_names <- reactive({
    projects <- 
      sample_table(connection()) %>%
      select(project_id) %>%
      distinct() %>%
      collect()
    return(projects$project_id)
  })

  # Since we want dynamical generate the choices for the project selector
  # this construction is used to render a `selectizeInput` ui component
  # onto the 'projectselector' ui element.
  output$projectselector <- renderUI(
    selectizeInput(inputId = "project",
                   label = "Choose a dataset:",
                   choices = project_names(), 
                   selected = NULL, multiple = FALSE,
                   options = list(
                     placeholder = 'Please select an option below',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
  )
  
  datasetInput <- reactive({
    projmanr::fetch_information_for_project(connection(), input$project)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(input$project, '.csv', sep='') 
    },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )


  output$downloadYearReportData <- downloadHandler(
   filename = function() {
     paste("projectdata", "_", input$year, ".csv", sep="")
   },
   content = function(file) {
    connection <- RSQLServer::src_sqlserver("MS_SQL", file='sql.yaml', database = 'ProjectMan')
    start_date <- paste(input$year, "-01-01")
    end_date <- paste(input$year, "-12-31")
    report.data <- projmanr::fetch_aggregated_data_per_instrument_for_period(connection, start_date, end_date)
    write.csv(report.data, file = file, row.names=FALSE)
    }
  )

  output$downloadAggregatedData <- downloadHandler(
    filename = function() {
      paste("projectdata", "_", as.Date(input$dateRange[1]), "_", as.Date(input$dateRange[2]), ".csv", sep="")
      },
    content = function(file) {
      connection <- RSQLServer::src_sqlserver("MS_SQL", file='sql.yaml', database = 'ProjectMan')
      start_date <- input$dateRange[1]
      end_date <- input$dateRange[2]
      aggregated_data <- projmanr::fetch_project_summaries_for_date_range(connection, start_date, end_date)
      write.csv(aggregated_data, file = file, row.names=FALSE)
    }

  )
})
