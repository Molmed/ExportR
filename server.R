
library(reshape2)

source("data.R")

# If you are reading this, please forgive this extremly hack code.
# My you never need to refactor it, and pray that a read LIMS
# will one day come to set you free from this crapy solution.
# /JD 2013-10-09

# On 2015-01-21 I sinned again adding things to this code.
# Please forgive me. /JD

shinyServer(function(input, output) {
  datasetInput <- reactive({    
    project_info[project_info$project_id == input$project, ]
  })
  
  output$table <- renderTable({
    datasetInput()
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
     paste("projectdata", "_", input$year, 
           ".csv", sep="")
   },
   content = function(file) {

     cumulativeGBPerInstrumentData <- function() {
     # A helper function to return the cumulative data per instrument
     #
     # Args:
     #
     # Returns: The cumulative data in the following format: 
     #                   Year Month Instrument         GB cumulativeGB
     #   2012.HiSeq 1.1  2012     1    HiSeq 1 1190.16157     1190.162
     #   2012.HiSeq 1.4  2012     2    HiSeq 1  369.35514     1559.517
     #
  
      current.year <- as.numeric(format(Sys.time(), "%Y"))
      giga.bases.per.month.m <- melt(queryGigaBasesPerMonth(2012, current.year), id.vars=c("Instrument","Month","Year"), measure.vars=c("GB"))
      giga.bases.per.month.m$Year <- as.factor(giga.bases.per.month.m$Year)
      aggregated.data <- aggregate(as.numeric(giga.bases.per.month.m$value),
                                   by=list(giga.bases.per.month.m$Year,
                                           giga.bases.per.month.m$Month,
                                           giga.bases.per.month.m$Instrument),
                                    sum)
      colnames(aggregated.data) <- c("Year", "Month", "Instrument", "GB")
    
      split.by.year.and.instrument <- split(aggregated.data,
                                            list(aggregated.data$Year,aggregated.data$Instrument))
    
      cumulative.results <-
        do.call(rbind,
                lapply(split.by.year.and.instrument, function(x) {
                                     x$cumulativeGB <- cumsum(x$GB)
                                             x
                }))
    
      cumulative.results
    }
  
  
    cum.per.instrument <- cumulativeGBPerInstrumentData()
    cum.per.instrument.2014 <- cum.per.instrument[cum.per.instrument$Year == input$year,]
    
    quality.metrics <- queryQualityValues(paste(input$year,"-01-01"),paste(input$year,"-12-31"))[,c(1:3,5,7)]
    
    q30.per.month.and.instrument <- 
      aggregate(as.numeric(quality.metrics$PercentQ30),
                by=list(quality.metrics$Month, quality.metrics$Instrument),
                mean)
    
    colnames(q30.per.month.and.instrument) <- c("Month", "Instrument", "Mean %Q30")
    
    final.report.data <- 
      merge(
            cum.per.instrument.2014, 
            q30.per.month.and.instrument,
            by=c("Month", "Instrument"))
    
    final.report.data <- final.report.data[order( final.report.data$Instrument, final.report.data$Month),]
  
  
    write.csv(final.report.data, file = file, row.names=FALSE)
     
    }
  )

  # If you are reading this, please forgive this extremly hack code.
  # My you never need to refactor it, and pray that a read LIMS
  # will one day come to set you free from this crapy solution.
  # /JD 2013-10-09
  output$downloadAggregatedData <- downloadHandler(
    filename = function() {
      paste("projectdata", "_", as.Date(input$dateRange[1]), "_", as.Date(input$dateRange[2]), ".csv", sep="")
      },
    content = function(file) {
      project_db_dump <- project_info
      project_db_dump$run_date <- as.Date(project_db_dump$run_date)
      data_for_period <- project_db_dump[project_db_dump$run_date > as.Date(input$dateRange[1]) & project_db_dump$run_date < as.Date(input$dateRange[2]),]

      #Samples per project
      relevant_info <- data_for_period[data_for_period$read_num == 1, c("run_date","project_id", "sample_name", "flowcell_id", "lane_num")]
      relevant_info$counts <- 1
      samples_per_project_in_period <- aggregate(relevant_info$counts, by=list(relevant_info$project_id), sum)
      
      colnames(samples_per_project_in_period) <- c("project", "samples")
      head(samples_per_project_in_period)
      
      #Lane per project
      project_and_lanes <- unique(relevant_info[,c("run_date","project_id", "flowcell_id", "lane_num")])
      project_and_lanes$counts <- 1
      
      info_on_lanes <-
        aggregate(
          project_and_lanes$counts,
          by=list(project_and_lanes$project_id),
          sum)
      
      colnames(info_on_lanes) <- c("project", "lanes")
      
      # GB per project
      data_for_period$GB <- as.numeric((as.numeric(data_for_period$cycles) * as.numeric(data_for_period$pf_clusters)) / 10^9)
      
      data_relevant_for_gb_calc <- data_for_period[,c("run_date","project_id", "sample_name", "flowcell_id", "lane_num", "GB")]
      
      gb_per_project <-
        aggregate(
          data_relevant_for_gb_calc$GB,
          by=list(data_relevant_for_gb_calc$project_id),
          sum)
      
      colnames(gb_per_project) <- c("project", "GB")
      
      first_two <- merge(samples_per_project_in_period, info_on_lanes, by =c("project"))
      final_data <- merge(first_two, gb_per_project, by =c("project"))
      
      write.csv(final_data, file = file, row.names=FALSE)

    }

  )
})
