
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
## --- WFO.match() recommended parameters: Fuzzy = 2 to compare with LCVP(), ## needed params for genus only: First.dist = TRUE, Fuzzy.one = T, Fuzzy.max = Inf
## ---
## --- http://www.worldfloraonline.org/
## --- https://github.com/cran/WorldFlora
## ---
## --- function parameters: 
## ---    .taxon     : vector of taxonomic names with or without authors. Genus are not evaluated if submitted alone. 
## ---                 Preferably output of species_clean().
## ---    .ref_file  : path to reference table used for WFO.match()
## ---    .ref_name  : Name to record in the harmonized output table
## ---    .save_table: NULL or path to export the results.
## ---    .filename  : default "". Input file name to add to saved outputs. 
## ---    .n_cores   : the number of cores to be used for parallel computing.

solve_wfo <- function(.taxon, .ref_file, .ref_name, .multicore = TRUE, .save_table = NULL, .filename = "", .n_cores = 1) {
  
  ## !!! For debbugging only
  # .taxon <- species_clean(.path = iFile) %>%
  #   filter(!is.na(input_ready)) %>%
  #   pull(input_ready) %>%
  #   unique()
  # .ref_file   = wfo_backbone_gbif
  # .ref_name   = "Leipzig Catalogue of Vascular Plants"
  # .multicore  = T
  # .save_table = path_res
  # .filename   = get_filename(.path = iFile)
  # .n_cores    = parallel::detectCores() - 1
  ## !!!
  
  
  ## Check function inputs
  stopifnot(is.character(.taxon))

  ## Remove genus alone from the data
  input <- setdiff(.taxon, word(.taxon)) %>% unique() %>% sort() ## WFO.match can handle genus alone with increased Fuzzy.max
  #input <- .taxon
  
  ## Find table name if .path exists
  ref_filename <- if_else(str_detect(.ref_file, "classification.txt"), "wfo", get_filename(.ref_file)) %>% str_remove("_conv")%>% str_to_lower()
  
  ## --- RUN WFO ---
  message(paste0("...Running WFO with ", ref_filename, " backbone."))
  time1 <- Sys.time()
  
  if (.multicore) {
    
    ## Create function to avoid loading the whole environment to the workers
    crt_wfo <- carrier::crate(
      input     = input,
      .ref_file = .ref_file,
      function(.x){
        message("Processing sequence: ", min(.x), " to ", max(.x), ".")
        wfo_input = input[.x]
        WorldFlora::WFO.match(wfo_input, WFO.file = .ref_file, Fuzzy = 2, verbose = F)
      })
    
    ## Make chunks
    input_chunks <-furrr:::make_chunks(n_x = length(input), n_workers = .n_cores)
    
    ## Run crated function 
    future::plan(multisession, workers = .n_cores)
    solved_wfo <- furrr::future_map_dfr(.x = input_chunks, .f = crt_wfo, .options = furrr::furrr_options(globals = FALSE))
    future::plan(sequential)
  
  } else {
    
    solved_wfo <- WorldFlora::WFO.match(input, WFO.file = .ref_file, Fuzzy = 2, Fuzzy.max = Inf, verbose = F)
    
  } ## End if .multicore
  
  time2 <- Sys.time()
  dt    <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...Taxons solved with WFO and ", ref_filename, " - ", dt, " sec."))
  ## --- END RUN WFO ---
  
  # table(solved_wfo$taxonomicStatus, useNA = "always")
  # table(solved_wfo$Fuzzy, useNA = "always")
  
  ## --- Harmonize ---
  solved_tmp <- tibble(name = .taxon) %>%
    left_join(solved_wfo, by = c("name" = "spec.name")) %>%
    rowwise() %>%
    mutate(fuzzy_recalc = as.numeric(utils::adist(name, scientificName, ignore.case = T))) %>%
    ungroup() %>%
    mutate(
      
      ## Calculate harmonized indicators
      fuzzy_dist      = if_else(is.na(Fuzzy.dist), 0, Fuzzy.dist),
      fuzzy           = Fuzzy,
      #fuzzy_res       = NA,
      status          = case_when(
        str_to_lower(taxonomicStatus) == "unchecked"                             ~ "unresolved",
        fuzzy_recalc == fuzzy_dist & str_to_lower(taxonomicStatus) == "accepted" ~ "accepted",
        fuzzy_recalc != fuzzy_dist & str_to_lower(taxonomicStatus) == "accepted" ~ "synonym",
        #taxonomicStatus == "" | is.na(taxonomicStatus)                           ~ "noref", ## Backbones can have mistakes in linking synonyms back to accepted name
        TRUE ~ "noref"
        ),
      score = case_when(
        is.na(taxonomicStatus) ~ "name not tested",
        taxonomicStatus == ""  ~ "name not found",
        fuzzy_dist == 0        ~ "matched",
        fuzzy_dist >  0        ~ "misspelled name",
        TRUE ~ NA_character_
      ),
      refdata_id      = ref_filename,
      refdata         = .ref_name,
      matching_algo   = "WorldFlora::WFO.match()",
      algo_reduced    = matching_algo %>% str_remove(".*::") %>% str_remove("\\(") %>% str_remove("\\)"),
      process         = paste0(refdata_id, "_", algo_reduced),
      
      ## Accepted names
      accepted_id     = taxonID,
      accepted_name   = scientificName,
      accepted_author = scientificNameAuthorship,
    ) %>% 
    select(name, fuzzy, fuzzy_dist, status, score, accepted_id, accepted_name, accepted_author, process, refdata_id, refdata, matching_algo) %>%
    distinct()
  
  ## If submitted name return several answers, keep only the accepted if exists or the synonym if exists.
  count_names <- solved_tmp %>%
    group_by(name) %>%
    summarise(count = n()) 
  
  has_accepted <- solved_tmp %>%
    filter(status == "accepted") %>%
    mutate(has_accepted = TRUE) %>%
    select(name, has_accepted)
  
  has_synonym <- solved_tmp %>%
    filter(status == "synonym") %>%
    mutate(has_synonym = TRUE) %>%
    select(name, has_synonym)
  
  solved_out <- solved_tmp %>%
    left_join(count_names , by = "name") %>%
    left_join(has_accepted, by = "name") %>%
    left_join(has_synonym , by = "name") %>%
    mutate(
      has_accepted = replace_na(has_accepted, FALSE),
      has_synonym  = replace_na(has_synonym , FALSE)
      ) %>%
    filter(
      !(count > 1 & has_accepted == TRUE & status != "accepted"),
      !(count > 1 & has_accepted == FALSE & has_synonym == TRUE & status != "synonym")
      ) %>%
    select(name, fuzzy, fuzzy_dist, status, accepted_id, accepted_name, accepted_author, process, refdata_id, refdata, matching_algo) %>%
    distinct()
  ## ---
  
  ## output object to .GlobalEnv but just to be safe, also write csv back to demo file
  if (!is.null(.save_table)) {
    write_csv(solved_wfo, 
              paste0(.save_table, "/", .filename, "-" , 
                     format(Sys.time(), format = "%Y-%m-%d-%H%M"), 
                     "-resWFO-with", ref_filename, ".csv"))
    write_csv(solved_out, 
              paste0(.save_table, "/", .filename, "-" , 
                     format(Sys.time(), format = "%Y-%m-%d-%H%M"), 
                     "-resWFO-with", ref_filename, "-harmo.csv"))
    write_tsv(tibble(NULL), 
              paste0(.save_table, "/", .filename, "-", 
                     format(Sys.time(), format = "%Y-%m-%d-%H%M"), 
                     "-resWFO-with", ref_filename, "-", dt,"-secs.txt"))
  }
  
  ## Output
  list(tab = solved_out, dt = tibble(process = unique(solved_out$process), duration_sec = dt))
    
}





