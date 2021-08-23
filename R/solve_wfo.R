
## Function to run a Taxonomic Name Resolution algorithm and output result harmonized with:
## 1. The whole input list of species is returned
## 2. Matches are returned with species and authors separated
## 3. Taxonomic status in lower case
## 4. Score or fuzzy matching indicator


## --- World Flora Online ---------------------------------------------------
## --- Offline
## --- Data source: accept WFO reference data "http://104.198.143.165/files/WFO_Backbone/_WFOCompleteBackbone/WFO_Backbone.zip"
## ---              or any dataset converted with WorldFlora::new.backbone()
## --- Algorithm: WFO.match()
## --- Performs better with parallel computing
## ---
## --- http://www.worldfloraonline.org/
## --- https://github.com/cran/WorldFlora
## ---
## --- function params: 
## ---    .data: vector of taxonomic names with or without authors. Genus are not evaluated if submitted alone. 
## ---           Preferably output of species_clean().
## ---    .save_table: NULL or path to export the results. if .path exists (function embedded 
## ---                 in a higher level function call) it is used in the file name 

solve_wfo <- function(.taxon, .ref_file, .ref_name, .multicore = TRUE, .save_table = NULL){
  
  ## !!! For testing only
  # .path    <- "demo/NFMA_species_mess.csv"
  # .taxon <- .path %>% species_clean() %>%
  #   filter(!is.na(input_ready)) %>%
  #   pull(input_ready) %>%
  #   unique()
  # .ref_file <- paste0(path_data, "/", wfo_backbone_lcvp)
  # .ref_name <- "Leipzig Catalogue of Vascular Plants"
  # .multicore <-  TRUE
  # .save_table <- path_res
  ## !!! 
  
  ## Check function inputs
  stopifnot(is.logical(.multicore))
  stopifnot(is.character(.taxon))
  stopifnot(is.character(.ref_file))
  stopifnot(is.character(.ref_name))
  stopifnot(is.null(.save_table)|is.character(.save_table))

  stopifnot("WorldFlora" %in% installed.packages())
  
  stopifnot(file.exists(.ref_file))
  
  if (.multicore) stopifnot("furrr" %in% installed.packages()) ## future and parallel are loaded from furrr
  if (!is.null(.save_table)) stopifnot(dir.exists(.save_table))
  
  ## Remove genus alone from the data
  input <- setdiff(.taxon, word(.taxon)) %>% unique() %>% sort()
  
  ## Find table name if .path exists
  filename     <- if_else(exists(".path"), get_filename(.path), "")
  ref_filename <- if_else(str_detect(.ref_file, "classification.txt"), "WFO_backbone", get_filename(.ref_file))
  
  
  ## --- RUN WFO ---
  message(paste0("...Running WFO with ", ref_filename, " dataset."))
  time1 <- Sys.time()
  
  if (.multicore) {
    
    ## Set nb workers
    n_cores <- if_else(future::availableCores() <= 2, 1, future::availableCores() - 2)
    
    ## Create function to avoid loading the whole environment to the workers
    crt_wfo <- carrier::crate(
      input     = input,
      .ref_file = .ref_file,
      function(.x){
        message("Processing sequence: ", min(.x), " to ", max(.x), ".")
        wfo_input = input[.x]
        WorldFlora::WFO.match(wfo_input, WFO.file = .ref_file)
      })
    
    ## Make chunks
    input_chunks <-furrr:::make_chunks(n_x = length(input), n_workers = n_cores)
    
    ## Run crated function 
    future::plan(multisession)
    solved_wfo <- furrr::future_map_dfr(.x = input_chunks, .f = crt_wfo, .options = furrr::furrr_options(globals = FALSE))
    future::plan(sequential)
  
  } else {
    
    solved_wfo <- WorldFlora::WFO.match(input, WFO.file = .ref_file)
    
  } ## End if .multicore
  
  time2 <- Sys.time()
  dt    <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...Taxons solved with WFO", " - ", dt, " sec."))
  ## --- END RUN WFO ---
  
  # table(solved_wfo$taxonomicStatus, useNA = "always")
  # table(solved_wfo$Fuzzy, useNA = "always")
  
  ## --- Harmonize ---
  solved_out <- tibble(submitted_name = .taxon) %>%
    left_join(solved_wfo, by = c("submitted_name" = "spec.name")) %>%
    mutate(
      fuzzy_dist      = if_else(is.na(Fuzzy.dist), 0, Fuzzy.dist),
      fuzzy           = Fuzzy,
      fuzzy_res       = if_else(Old.name == "", scientificName, Old.name),
      status          = if_else(taxonomicStatus == "", "noref", taxonomicStatus),
      accepted_id     = acceptedNameUsageID,
      refdata_id      = ref_filename,
      refdata         = .ref_name,
      matching_algo   = "WorldFlora::WFO.match()",
      accepted_name   = scientificName,
      accepted_author = scientificNameAuthorship,
    ) %>% 
    select(submitted_name, fuzzy, fuzzy_dist, status, accepted_id, accepted_name, accepted_author, refdata_id, refdata, matching_algo)
  ## ---
  
  ## output object to .GlobalEnv but just to be safe, also write csv back to demo file
  if (!is.null(.save_table)) {
    write_csv(solved_wfo, 
              paste0(.save_table, "/", filename, "-" , 
                     format(Sys.time(), format = "%Y-%m-%d-%H%M"), 
                     "-resWFO-with", ref_filename, ".csv"))
    write_csv(solved_out, 
              paste0(.save_table, "/", filename, "-" , 
                     format(Sys.time(), format = "%Y-%m-%d-%H%M"), 
                     "-resWFO-with", ref_filename, "-harmo.csv"))
    write_tsv(tibble(NULL), 
              paste0(.save_table, "/", filename, "-", 
                     format(Sys.time(), format = "%Y-%m-%d-%H%M"), 
                     "-resWFO-with", ref_filename, "-", dt,"-secs.txt"))
  }
  
  solved_out
    
}





