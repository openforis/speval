######################################################################
# Species Validation Tool                                        #####
# SPECIES NAMES VALIDATION TOOL                                        
# (c) Lauri Vesa, Javier Garcia-Perez, Elisee Tchana, Gael Sola FAO    
# server.R file                                                        
# updated 17.8.2021
######################################################################





################
# SERVER LOGIC #
################

shinyServer(function(input, output) {
  
   
  output$contents <- renderTable({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read.csv(file$datapath, header = TRUE) #  input$header)

  })
  
  output$text <- renderText("Before you can import a species list, you need to prepare a table and save it as a CSV format file. Then upload the file into this app.")
  

  # observe({ 
  #   file.copy(input$file1$datapath,
  #             input$file1$name, overwrite = TRUE)
  # })
  
  disable("run_analyse")
  
  observeEvent(input$file1, {
    file     <- input$file1
    stable   <- read.csv(file$datapath, header = TRUE)
    enable("run_analyse")
    shinyjs::html(id = "console_text", "")
    shinyjs::html(id = "console_text", html = paste0("Number of names: ", nrow(stable), '<br>'), add = TRUE)
    output$result_statistics  <- renderTable(NULL)
    output$result_statistic_2 <- renderTable(NULL)
    output$result_contents    <- renderTable(NULL)
  })
  
  
  observeEvent(input$run_analyse, {
    withCallingHandlers({
      shinyjs::html("console_text", "")
      source("Species_search_17_August_2021.R", local = TRUE)
    },
    message = function(m) {
      shinyjs::html(id = "console_text", html = paste0(m$message, '<br>'), add = TRUE)
    })
    
    if(file.exists(outFile1)){
      output$result_contents <- renderTable({
        
        read.csv(outFile1)
      }, na = "" )
      
      output$downloadData <- downloadHandler(
        filename <- function() {
          paste(outFile1, sep='')
        },
        content <- function(file) {
          file.copy(outFile1, file)
        }
      )
    }
    
    if(file.exists(outFile2)){
      output$result_statistics <- renderTable({
        
        
        read.csv(outFile2)
      })
      
      output$downloadData2 <- downloadHandler(
        filename <- function() {
          paste(outFile2, sep='')
        },
        content <- function(file) {
          file.copy(outFile2, file)
        }
      )
    }
    
    if(file.exists(outFile3)){
      output$result_statistics_2 <- renderTable({
        
        read.csv(outFile3, header = TRUE)
      })
      
      output$downloadData3 <- downloadHandler(
        filename <- function() {
          paste(outFile3, sep='')
        },
        content <- function(file) {
          file.copy(outFile3, file)
        }
      )
    }
    
  })

})