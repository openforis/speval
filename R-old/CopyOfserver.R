# ***************************************************************************
# Species Validation Tool                                               #####
# SPECIES NAMES VALIDATION TOOL                                        
# (c) Lauri Vesa, Javier Garcia-Perez, Elisee Tchana, Gael Sola. FAO    
# server.R file                                                        
# updated 8.9.2021
# ***************************************************************************


# ************ #
# SERVER LOGIC #
# ************ #

shinyServer(function(input, output, session) {
  
  
  
  ## Disclaimer #############################################################
  
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
  
  
  
  ## Initiate reactives #####################################################
  
  ## Initiate conditions to show the validation and results panels
  output$file_valid <- reactive({ FALSE })
  outputOptions(output, "file_valid", suspendWhenHidden = FALSE)
  
  output$validation_done <- reactive({ FALSE })
  outputOptions(output, "validation_done", suspendWhenHidden = FALSE)
  
  ## Initiate reactive for conditions to start the master function
  species_cond <- reactiveValues(run = FALSE, valid = FALSE)
  
  ## Initae reactive for data outputs
  species_results <- reactiveValues(output = NULL)
  
  
  ## Read and show species list #############################################
  
  check_table <- reactive({
    req(input$file1)
    
    ext <- tools::file_ext(input$file1$name)
    
    validate(need(ext == "csv", "Please upload a csv file"))
    read.csv(input$file1$datapath, header = TRUE) #  input$header)
  })
  
  output$contents <- renderTable( check_table() )
  #output$contents <- renderTable(input$file1)
  
  
  
  ## Activate validation Panel ##############################################
  
  # If file is valid show indicator panels and buttons
  observeEvent(input$file1, {
    if (tools::file_ext(input$file1$name) == "csv") {
      output$file_valid <- reactive({ TRUE })
    } else {
      output$file_valid <- reactive({ FALSE })
    }
  }) ## End observeEvent
  
  
  
  ## Confirm run the validation #############################################
  
  observeEvent(input$run_analyse, {
    
    ## Re-initialize console and button
    shinyjs::html(id = "console_text", "")
    shinyjs::hide("after_valid")
    
    ## Update Run condition to start the master function
    # if (is.null(species_results$output)) {
    #   
    #   species_cond$run <- TRUE
    #   
    # } else {
      
      shinyalert::shinyalert(
        inputId = "shinyalert", 
        title = "Confirmation",
        text  = "Run the species validation (Erase previous results)?",
        callbackR = function(x) {
          species_cond$run <- x
        },
        showCancelButton = T
      )
      
    # } ## End if
    
  }) ## End observeEvent
  
  
  
  ## Run the validation master function #####################################
  
  observeEvent(species_cond$run, {
    
    if (species_cond$run) {
      
      if ("scientific_name" %in% names(check_table())) {
        
        shinyjs::html(id = "console_text", "")
        
        species_results$output <- withCallingHandlers({
          species_solve(
            .path       = input$file1$datapath,
            .how_to     = "wfo_ncbi", #"compare",
            .with_lcvp  = FALSE,
            .save_table = NULL,
            .multicore  = input$opt_multicore,
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
        
        species_cond$valid <- TRUE
        
      } else {
        
        shinyjs::html(id = "console_text", "")
        shinyjs::html(id = "console_text", html = "The input file doesn't have 'scientific_name' column. <br>")
        shinyjs::html(id = "console_text", html = "Please re-upload a CSV file with the species list under a column named: 'scientific_name'. <br>", add = TRUE)
        
        species_cond$valid <- FALSE
        
      } ## End if
      
      species_cond$run <- FALSE
      
    } ## End if
    
    
    if (species_cond$valid) {
      output$validation_done <- reactive({ TRUE })
      shinyjs::show("after_valid")
    } else {
      output$validation_done <- reactive({ FALSE })
      shinyjs::hide("after_valid")
    }
    
  })
   
  ## Run function
  # species_solved <- eventReactive(species_results$run, {
  #   
  #   if ("scientific_name" %in% names(check_table())) {
  #     
  #     shinyjs::html(id = "console_text", "")
  #     withCallingHandlers({
  #       species_solve(
  #         .path       = input$file1$datapath,
  #         .how_to     = "lcvp_wfo", #"compare",
  #         .with_lcvp  = FALSE,
  #         .save_table = NULL,
  #         .multicore  = input$opt_multicore,
  #         .ref_lcvp   = wfo_backbone_lcvp,
  #         .ref_wfo    = wfo_file,
  #         .tx_src     = src_tropicos,
  #         .ref_ncbi   = wfo_backbone_ncbi,
  #         .ref_gbif   = wfo_backbone_gbif,
  #         .ref_gts    = gts_file,
  #         .ref_iucn   = iucn_checklist
  #       )
  #     },
  #     message = function(m) {
  #       shinyjs::html(id = "console_text", html = paste0(m$message, '<br>'), add = TRUE)
  #     })
  #     
  #   } else {
  #     
  #     shinyjs::html(id = "console_text", "")
  #     shinyjs::html(id = "console_text", html = "The input file doesn't have 'scientific_name' column. <br>")
  #     shinyjs::html(id = "console_text", html = "Please re-upload a CSV file with the species list under a column named: 'scientific_name'. <br>", add = TRUE)
  #     list(valid = FALSE)
  #     
  #   } ## End if
  # 
  # }) ## End eventReactive
  
  
  
  ## Activate conditionalPanel if species validation done ###################
  
  # observeEvent(species_results$run, {
  #   
  #   if (species_solved()$valid) {
  #     output$validation_done <- reactive({ TRUE })
  #     shinyjs::show("after_valid")
  #     species_results$output <- TRUE
  #   } else {
  #     output$validation_done <- reactive({ FALSE })
  #     shinyjs::hide("after_valid")
  #     species_results$output <- FALSE
  #   }
  #   
  #   species_results$run <- FALSE
  #   
  # }) ## End observeEvent
    
  ## Change tab
  observeEvent(
    input$to_results,
    updateTabItems(session, "tabs", "results")
  )
  
  
  
  ## Prepare outputs ########################################################
  
  observeEvent(species_cond$valid, {

    ## Species solved table
    output$result_contents <- renderTable({
      species_results$output$species_final
    })

    output$downloadData <- downloadHandler(
      filename <- function() { paste0(input$file1$name %>% str_remove(".csv"), "-results.csv") },
      content  <- function(file) { readr::write_csv(species_results$output$species_final, file) }
      )

    ## Stat1
    output$stat1 <- renderTable({
      species_results$output$stat1
    })

    output$downloadStat1 <- downloadHandler(
      filename <- function() { paste0(input$file1$name %>% str_remove(".csv"),"-stat1.csv") },
      content  <- function(file) { readr::write_csv(species_results$output$stat1, file) }
    )

    ## Stat2
    output$stat2 <- renderTable({
      species_results$output$stat2
    })

    output$downloadStat2 <- downloadHandler(
      filename <- function() { paste0(input$file1$name %>% str_remove(".csv"), "-stat2.csv") },
      content  <- function(file) { readr::write_csv(species_results$output$stat2, file) }
    )

    ## Stat3
    output$stat3 <- renderTable({
      species_results$output$stat3
    })

    output$downloadStat3 <- downloadHandler(
      filename <- function() { paste0(input$file1$name %>% str_remove(".csv"), "-stat3.csv") },
      content  <- function(file) { readr::write_csv(species_results$output$stat3, file) }
    )

  }) ## End observeEvent
  
})