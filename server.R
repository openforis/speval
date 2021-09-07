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

shinyServer(function(input, output, session) {
  
  ## Initial Disclaimer ###################################################
  sendSweetAlert(
    session = session,
    title =  NULL,
    text = div(
      h3(icon("info-circle"), HTML("&nbsp;"), strong("Disclaimer"), 
         style = "text-align: center; color: #f13c1f;"),
      br(),
      h5("This application is experimental and may use outdated version of taxonomic webservices"),
      h5(strong("The information provided in this demo may be incomplete and contain errors.")),
      class = "disclaimer"
    ),
    #type = "error", 
    html = TRUE,
    closeOnClickOutside = FALSE
  )
  
  
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
    # file     <- input$file1
    # stable   <- read.csv(file$datapath, header = TRUE)
    enable("run_analyse")
    shinyjs::html(id = "console_text", "")
    # shinyjs::html(id = "console_text", html = paste0("Number of names: ", nrow(stable), '<br>'), add = TRUE)
    output$stat1           <- renderTable(NULL)
    output$stat2           <- renderTable(NULL)
    output$stat3           <- renderTable(NULL)
    output$result_contents <- renderTable(NULL)
    
  })
  
  
  ## Run the solving meta-function
  species_solved <- eventReactive(input$run_analyse, {
    
      withCallingHandlers({
        shinyjs::html("console_text", "")
        #source("Species_search_17_August_2021.R", local = TRUE)
        species_solve(
          .path       = input$file1$datapath, 
          .how_to     = "compare", 
          .with_lcvp  = FALSE, 
          .save_table = NULL, 
          .multicore  = TRUE, 
          .ref_lcvp   = wfo_backbone_lcvp,
          .ref_wfo    = wfo_file,
          .tx_src     = src_tropicos,
          .ref_ncbi   = wfo_backbone_ncbi, 
          .ref_gbif   = wfo_backbone_gbif, 
          .ref_gts    = gts_file,
          .ref_iucn   = iucn_checklist
          )
      },
      message = function(m) {
        shinyjs::html(id = "console_text", html = paste0(m$message, '<br>'), add = TRUE)
      })
    
  }) ## End eventReactive
  
  
  ## Show outputs
  observeEvent(input$run_analyse, {
    
    ## Species solved table
    output$result_contents <- renderTable({
      species_solved()$species_final
    })
    
    output$downloadData <- downloadHandler(
      filename <- function() { paste0(input$file1$name %>% str_remove(".csv"), "-species.csv") },
      content  <- function(file) { readr::write_csv(species_solved()$species_final, file) }
      )
    
    ## Stat1
    output$stat1 <- renderTable({
      species_solved()$stat1
    })
    
    output$downloadStat1 <- downloadHandler(
      filename <- function() { paste0(input$file1$name %>% str_remove(".csv"),"-stat1.csv") },
      content  <- function(file) { readr::write_csv(species_solved()$stat1, file) }
    )
    
    ## Stat2
    output$stat2 <- renderTable({
      species_solved()$stat2
    })
    
    output$downloadStat2 <- downloadHandler(
      filename <- function() { paste0(input$file1$name %>% str_remove(".csv"), "-stat2.csv") },
      content  <- function(file) { readr::write_csv(species_solved()$stat2, file) }
    )
    
    ## Stat3
    output$stat3 <- renderTable({
      species_solved()$stat3
    })
    
    output$downloadStat3 <- downloadHandler(
      filename <- function() { paste0(input$file1$name %>% str_remove(".csv"), "-stat3.csv") },
      content  <- function(file) { readr::write_csv(species_solved()$stat3, file) }
    )
    
  }) ## End observeEvent
  
  
  # observeEvent(input$run_analyse, {
  #   withCallingHandlers({
  #     shinyjs::html("console_text", "")
  #     #source("Species_search_17_August_2021.R", local = TRUE)
  #     
  #   },
  #   message = function(m) {
  #     shinyjs::html(id = "console_text", html = paste0(m$message, '<br>'), add = TRUE)
  #   })
  #   
  #   if(file.exists(outFile1)){
  #     output$result_contents <- renderTable({
  #       
  #       read.csv(outFile1)
  #     }, na = "" )
  #     
  #     output$downloadData <- downloadHandler(
  #       filename <- function() {
  #         paste(outFile1, sep='')
  #       },
  #       content <- function(file) {
  #         file.copy(outFile1, file)
  #       }
  #     )
  #   }
  #   
  #   if(file.exists(outFile2)){
  #     output$result_statistics <- renderTable({
  #       
  #       
  #       read.csv(outFile2)
  #     })
  #     
  #     output$downloadData2 <- downloadHandler(
  #       filename <- function() {
  #         paste(outFile2, sep='')
  #       },
  #       content <- function(file) {
  #         file.copy(outFile2, file)
  #       }
  #     )
  #   }
  #   
  #   if(file.exists(outFile3)){
  #     output$result_statistics_2 <- renderTable({
  #       
  #       read.csv(outFile3, header = TRUE)
  #     })
  #     
  #     output$downloadData3 <- downloadHandler(
  #       filename <- function() {
  #         paste(outFile3, sep='')
  #       },
  #       content <- function(file) {
  #         file.copy(outFile3, file)
  #       }
  #     )
  #   }
  #   
  # })

})