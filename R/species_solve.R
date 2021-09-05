
## Function to solve tree species taxonomic names against existing taxonomic databases
## Note: species_solve() is developed to run on a clean data output and may not 
## work if the species list has not been striped of special characters and common typos.
## Note: List of packages required: TBD
## Input a vector with species names
## Output a list with:
## - data frames with input and solved names
## - data frame with summary stats on the process

## Order for Species solve:
# 1) The Leipzig Catalogue of Vascular Plant [offline]
# 2) Tropicos - Missouri Botanical Garden [online]
# 3) Kew  - Plants of the World Online [online]
# 4) NCBI - National Center for Biotechnology Information, db="taxonomy" [online]
# 5) WFO  - WorldFlora Online [offline]
# 6) GBIF - Global Biodiversity Information Facility [online]
# additional information:
# 7) GBIF - IUCN Red List search [online], (no results for all species!)
# 8) GlobalTreeSearch [offline]. (Used to check names occur in this DB, not  actually to validate names)

## Description of services



## --- Function parameters:
## ---  .path: path to species list to solve
## ---  .how_to: how to combine or not the taxonomic name resolution services:
## ---    "compare"  : Run the input species list on all services. Doesn't use LCVP(), 
## ---                 its backbone table is used with WFO.match(), unless .with_lcvp == TRUE.
## ---    "integrate": Run the species list on all services but submit only the unmatch and unresolved species from service n to service n+1.
## ---                 (doesn't use LCVP(), backbone table used with WFO.match()).
## ---    "lcvp"     : Run LCVP.
## ---    "wfo_lcvp" : Run WFO algorithm with LCVP table as backbone.
## ---    "wfo"      : Run WFO.
## ---    "tropicos" : Run Tropicos.
## ---    "wfo_ncbi" : Run WFO algorithm with NCBI table as backbone.
## ---    "wfo_gbif" : Run WFO algorithm with GBIF table as backbone.
## ---  .with_lcvp   : Use LCVP() function. Default FALSE as LCVP backbone data is used with WFO.match().
## ---  .save_table  : NULL or path to save the function's outputs. Raw and harmonized 
## ---                 outputs are written while only the harmonized outputs are returned by the function. 
## ---  .multicore   : logical. if TRUE, relies on parallel, future, furrr and carrier packages. Use plan(multisession) 
## ---                 as compatible with all OS types.
## ---  .ref_lcvp    : NULL or path to file LCVP backbone for WFO.match(). Required when all services or "wfo_lcvp" are used.
## ---  .ref_wfo     : NULL or path to file WFO backbone for WFO.match(). Required when all services or "wfo" are used.
## ---  .ref_ncbi    : NULL or path to file NCBI backbone for WFO.match(). Required when all services or "wfo_ncbi" are used. 
## ---                 Data comes from taxadb, may not be the latest version.
## ---  .ref_gbif    : NULL or path to file GBIF backbone for WFO.match(). Required when all services or "wfo_gbif" are used. 
## ---                 Data comes from taxadb, may not be the latest version.

species_solve <- function(.path, 
                          .how_to     = "wfo_lcvp", 
                          .with_lcvp  = FALSE, 
                          .save_table = NULL, 
                          .multicore  = TRUE,
                          .ref_lcvp   = NULL, 
                          .ref_wfo    = NULL, 
                          .ref_gts    = NULL, 
                          .ref_ncbi   = NULL, 
                          .ref_gbif   = NULL, 
                          .gts        = NULL, 
                          .tx_src     = NULL) {
  
  time_start <- Sys.time()
  
  message("\n---\nInitiating Taxonomic Resolution.\n---\n")
  
  # write_file(x = "print(1+1)", file = "demo/test.R")
  # rstudioapi::jobRunScript("demo/test.R", "test", workingDir = getwd(), importEnv = T, exportEnv = "R_GlobalEnv")
  
  
  
  ##
  ## Check function inputs ##################################################
  ##
  
  stopifnot(is.character(.path))
  stopifnot(str_ends(.path, "csv"))
  stopifnot(.how_to %in% c("compare", "integrate", "lcvp", "wfo_lcvp", "wfo", "tropicos"))
  stopifnot(is.logical(.multicore))
  
  ## Check if packages installed (https://stackoverflow.com/questions/9341635/check-for-installed-packages-before-running-install-packages)
  stopifnot(nzchar(system.file(package = "furrr"))) ## future and parallel are loaded from furrr
  if (.how_to %in% c("compare", "integrate", "lcvp"))     stopifnot(nzchar(system.file(package = "lcvplants")))
  if (.how_to %in% c("compare", "integrate", "wfo_lcvp")) stopifnot(nzchar(system.file(package = "WorldFlora")))
  if (.how_to %in% c("compare", "integrate", "wfo"))      stopifnot(nzchar(system.file(package = "WorldFlora")))
  if (.how_to %in% c("compare", "integrate", "tropicos")) stopifnot(nzchar(system.file(package = "taxize")))
  
  ## Check if reference files for WFO.match() exist
  if (!is.null(.save_table) & .how_to %in% c("compare", "integrate", "wfo_lcvp")) stopifnot(file.exists(.ref_lcvp))
  if (!is.null(.save_table) & .how_to %in% c("compare", "integrate", "wfo"))      stopifnot(file.exists(.ref_wfo))
  
  ## Check if path to save tables exists
  if (!is.null(.save_table)) stopifnot(dir.exists(.save_table))
  
  
  
  ##
  ## Prepare data and parameters ############################################
  ##
  
  ## Get number of cores for multicore sub-functions
  n_cores <- parallel::detectCores() - 1
  #n_cores <- if_else(n_cores == 0, 1, n_cores)
  n_cores <- if_else(n_cores == 0, 1, ceiling(n_cores * 2 / 3))
  
  filename <- get_filename(.path)
  
  species_cleaned <- species_clean(.path)
  
  ## Split species (inc. intraspecies) from genus alone
  # species_notsolved <- setdiff(species_cleaned, word(species_cleaned)) %>% unique() %>% sort()
  # genus_notsolved   <- setdiff(species_cleaned, species_notsolved)     %>% unique() %>% sort()
  
  ## Keep all species and genus alone together
  species_notsolved <- species_cleaned %>%
    filter(!is.na(input_ready)) %>% 
    pull(input_ready) %>% 
    unique()
  
  
  ##
  ## Call services round 1 ##################################################
  ##
  
  ## --- 1a. LCVP() ---------------------------------------------------------
  if (.with_lcvp | .how_to == "lcvp") {
    
    if (.how_to %in% c("compare", "integrate", "lcvp")) {
      
      message("Start LCVP()...")
      
      ## Select data
      ## --- Data is species_notsolved as this is the first service
      
      ## Run service
      res_lcvp <- solve_lcvp(
        .taxon      = species_notsolved, 
        .save_table = .save_table, 
        .filename   = filename, 
        .n_cores    = n_cores
      )
      
      print(table(res_lcvp$tab$status, useNA = "always"))
      notsolved_lcvp <- res_lcvp$tab %>% filter(status %in% c("noref", "unresolved")) %>% pull(sc_name)
      
      ## Update species_notsolved
      if (.how_to == "integrate") species_notsolved <- setdiff(species_notsolved, notsolved_lcvp)
 
    } else {
      
      res_lcvp <- list(tab = NULL, dt = NULL)
      
    } ## End if LCVP
    
  } ## End if .with_lcvp
  
  
  
  ## --- 1b. WFO.match() with LCVP backbone ---------------------------------
  if (.how_to %in% c("compare", "integrate", "wfo_lcvp") & length(species_notsolved != 0)) {
    
    message("Start WFO.match() with LCVP backbone...")
    
    ## Run service
    res_wfo_lcvp <- solve_wfo(
      .taxon      = species_notsolved, 
      .ref_file   = .ref_lcvp,
      .ref_name   = "Leipzig Catalogue of Vascular Plants", 
      .multicore  = .multicore, 
      .save_table = .save_table,
      .filename   = filename,
      .n_cores    = n_cores
      )
    
    print(table(res_wfo_lcvp$tab$status, useNA = "always"))
    notsolved_wfo_lcvp <- res_wfo_lcvp$tab %>% filter(status %in% c("noref", "unresolved")) %>% pull(sc_name)
    
    ## Update species_notsolved
    if (.how_to == "integrate") species_notsolved <- setdiff(species_notsolved, notsolved_wfo_lcvp)
    
  } else {
    
    res_wfo_lcvp <- list(tab = NULL, dt = NULL)
    
  } ## End if WFO on LCVP
  
  
  
  ## --- 2. WFO.match() with WFO backbone -----------------------------------
  if (.how_to %in% c("compare", "integrate", "wfo") & length(species_notsolved != 0)) {
    
    message("Start WFO.match() with WFO backbone...")
    
    ## Run service
    res_wfo <- solve_wfo(
      .taxon      = species_notsolved, 
      .ref_file   = .ref_wfo,
      .ref_name   = "World Flora Online", 
      .multicore  = .multicore, 
      .save_table = .save_table,
      .filename   = filename,
      .n_cores    = n_cores
      )
    
    print(table(res_wfo$tab$status, useNA = "always"))
    notsolved_wfo <- res_wfo$tab %>% filter(status %in% c("noref", "unresolved")) %>% pull(sc_name)
    
    ## Update species_notsolved
    if (.how_to == "integrate") species_notsolved <- setdiff(species_notsolved, notsolved_wfo)
    
  } else {
    
    res_wfo <- list(tab = NULL, dt = NULL)
    
  } ## End if WFO
  
  
  
  ## --- 3. Tropicos --------------------------------------------------------
  if (.how_to %in% c("compare", "integrate", "tropicos") & length(species_notsolved != 0)) {
    
    message("Start gnr_resolve() with Tropicos backbone...")
    
    ## Run service
    res_tropicos <- solve_tropicos(
      .taxon      = species_notsolved, 
      .gnr_src    = .tx_src,
      .save_table = .save_table,
      .filename   = filename
    )
    
    print(table(res_tropicos$tab$status, useNA = "always"))
    notsolved_tropicos <- res_tropicos$tab %>% filter(status %in% c("noref", "unresolved")) %>% pull(sc_name)
    
    ## Update species_notsolved
    if (.how_to == "integrate") species_notsolved <- setdiff(species_notsolved, notsolved_tropicos)
    
  } else {
    
    res_tropicos <- list(tab = NULL, dt = NULL)
    
  } ## End if WFO
  
  
  
  ## --- 4. WFO.match() with NCBI backbone ----------------------------------
  if (.how_to %in% c("compare", "integrate", "wfo_ncbi") & length(species_notsolved != 0)) {
    
    message("Start WFO.match() with NCBI backbone...")
    
    ## Run service
    res_ncbi <- solve_wfo(
      .taxon      = species_notsolved, 
      .ref_file   = .ref_ncbi,
      .ref_name   = "National Center for Biotechnology Information", 
      .multicore  = .multicore, 
      .save_table = .save_table,
      .filename   = filename,
      .n_cores    = n_cores
    )
    
    print(table(res_ncbi$tab$status, useNA = "always"))
    notsolved_ncbi <- res_ncbi$tab %>% filter(status %in% c("noref", "unresolved")) %>% pull(sc_name)
    
    ## Update species_notsolved
    if (.how_to == "integrate") species_notsolved <- setdiff(species_notsolved, notsolved_ncbi)
    
  } else {
    
    res_ncbi <- list(tab = NULL, dt = NULL)
    
  } ## End if WFO on NCBI
  
  
  
  ## --- 5. WFO.match() with GBIF backbone ----------------------------------
  if (.how_to %in% c("compare", "integrate", "wfo_gbif")) {
    
    message("Start WFO.match() with GBIF backbone...")
    
    ## Run service
    res_gbif <- solve_wfo(
      .taxon      = species_notsolved, 
      .ref_file   = .ref_gbif,
      .ref_name   = "Global Biodiversity Information Facility", 
      .multicore  = .multicore, 
      .save_table = .save_table,
      .filename   = filename,
      .n_cores    = n_cores
    )
    
    print(table(res_gbif$tab$status, useNA = "always"))
    notsolved_gbif <- res_gbif$tab %>% filter(status %in% c("noref", "unresolved")) %>% pull(sc_name)
    
    ## Update species_notsolved
    if (.how_to == "integrate") species_notsolved <- setdiff(species_notsolved, notsolved_gbif)
    
  } else {
    
    res_gbif <- list(tab = NULL, dt = NULL)
    
  } ## End if WFO on GBIF
  
  
  
  ##
  ## Analyze results ########################################################
  ##
  
  message("\n---\nAnalyzing first results.\n---\n")
  
  time1 <- Sys.time()
  
  ## !!! For debugging analysis
  res_1 <- list(tab = read_csv("results/NFMA_species_clean100-2021-09-05-1955-resWFO-withlcvp-harmo.csv"), duration = tibble(process = "lcvp_WFO.match", duration_sec = 1000))
  res_2 <- list(tab = read_csv("results/NFMA_species_clean100-2021-09-05-1958-resTropicos-harmo.csv"    , col_types = cols(score = col_character())), duration = tibble(process = "tropicos_gnr_resolve", duration_sec = 1000))
  res_3 <- list(tab = read_csv("results/NFMA_species_clean100-2021-09-05-1958-resWFO-withwfo-harmo.csv" ), duration = tibble(process = "wfo_WFO.match" , duration_sec = 1000))
  res_4 <- list(tab = read_csv("results/NFMA_species_clean100-2021-09-05-1959-resWFO-withncbi-harmo.csv"), duration = tibble(process = "ncbi_WFO.match", duration_sec = 1000))
  res_5 <- list(tab = read_csv("results/NFMA_species_clean100-2021-09-05-2000-resWFO-withgbif-harmo.csv"), duration = tibble(process = "gbif_WFO.match", duration_sec = 1000))
  ## !!!
  
  
  ## -- Combine results -----------------------------------------------------
  tab         <- mget(ls(pattern = "res_")) %>% map_dfr(., 1)
  duration    <- mget(ls(pattern = "res_")) %>% map_dfr(., 2)
  num_process <- length(unique(tab$process))
  
  ## Add number of duplicates for each submitted taxon
  count_input <- tab %>%
    group_by(sc_name) %>%
    summarise(num_input = n())
    
  ## Add number of solutions for each submitted taxon
  count_taxon <- tab %>% 
    select(sc_name, accepted_name) %>%
    distinct() %>%
    group_by(sc_name) %>%
    summarise(num_taxon = n())
  
  ## Add number of solutions per service for each submitted taxon 
  count_dup <- tab %>% 
    select(sc_name, process, accepted_name) %>%
    distinct() %>%
    group_by(sc_name, process) %>%
    summarise(num_dup = n())
  
  ## Categorize solutions
  out_tab <- tab %>% 
    left_join(count_input, by = "sc_name") %>%
    left_join(count_taxon, by = "sc_name") %>%
    left_join(count_dup, by = c("sc_name", "process")) %>%
    mutate(result_type = case_when(
      word(sc_name) == sc_name                                    ~ "genus only",
      is.na(status)                                         ~ "backbone reference error",
      num_taxon == 1 & status %in% c("accepted", "synonym") ~ "all services",
      num_taxon == 1 & status %in% c("unresolved", "noref") ~ "no service",
      num_taxon > 1  & num_input == num_process             ~ "conflict between services",
      num_taxon > 1  & num_input  > num_process             ~ "conflict intra service",
      TRUE ~ NA_character_
    ))
  
  # table(out_tab$process, out_tab$status)
  # table(out_tab$process, out_tab$count_taxon)
  # table(out_tab$process, out_tab$count_dup)
  # table(out_tab$process, out_tab$result_type, useNA = "always")
  # table(out_tab$count_taxon, out_tab$count_dup)
  
  write_csv(out_tab, file.path(.save_table, paste0(filename, "-", format(Sys.time(), format = "%Y-%m-%d-%H%M"), "-results.csv")))
  
  
  ## -- STAT1: nb of records per status and process -------------------------
  service_order <- tibble(
    process = c("lcvp_LCVP", "lcvp_WFO.match", "wfo_WFO.match",  "tropicos_gnr_resolve", "ncbi_WFO.match", "gbif_WFO.match"), 
    order   = 1:6
  )
  
  stat1 <- out_tab %>%
    group_by(process, refdata,	matching_algo, status) %>%
    summarize(count = n()) %>%
    pivot_wider(names_from = status, values_from = count, values_fill = 0) %>%
    ungroup() %>%
    left_join(duration, by = "process") %>%
    left_join(service_order, by = "process") %>%
    bind_rows(
      list(order = 0.1, process = "Initial",  unresolved = length(species_cleaned$scientific_name)),
      list(order = 0.2, process = "Cleaned",  unresolved = length(unique(species_cleaned$input_ready))) 
      ) %>%
    arrange(order) %>%
    select(-order) %>%
    mutate(across(where(is.numeric), as.character)) %>%
    mutate(across(everything(), ~if_else(is.na(.x), "", .x)))
  
  write_csv(stat1, file.path(.save_table, paste0(filename, "-", format(Sys.time(), format = "%Y-%m-%d-%H%M"), "-stat1.csv")))  
  
  
  ## -- Make a table of unique solutions ------------------------------------ 
  ## First process: if LCVP() in the process use the service after to favor lcvp_WFO.match()
  first_process <- if_else(.with_lcvp, stat1$process[4], stat1$process[3])
  
  ## Unique solutions
  out1 <- out_tab %>%
    filter(result_type == "all services", num_input == num_process, process == first_process) %>%
    select(sc_name, accepted_name, accepted_author, status, fuzzy_dist) %>%
    distinct()

  ## Genus only: take from Tropicos
  out2 <- out_tab %>%
    filter(result_type == "genus only", process == "tropicos_gnr_resolve", status == "accepted") %>%
    select(sc_name, accepted_name, accepted_author, status, fuzzy_dist) %>%
    distinct()
  
  ## Conflicts between services: take first process as reference for accepted and synonyms (lcvp_WFO.match() unless specific service is called)
  unique_firstprocess <- out_tab %>%
    filter(
      result_type %in% c("conflict between services", "conflict intra service"), 
      process == first_process, 
      status %in% c("accepted", "synonym"),
      num_dup == 1
      ) %>%
    pull(sc_name)

  out3_tmp <- out_tab %>%
    filter(result_type  %in% c("conflict between services", "conflict intra service")) %>%
    mutate(unique_firstprocess = if_else(sc_name %in% unique_firstprocess, TRUE, FALSE))
  
  ## Check
  # table(out3_tmp$unique_lcvp)
  
  out3 <- out3_tmp %>%
    filter(unique_firstprocess, process == first_process) %>%
    select(sc_name, accepted_name, accepted_author, status, fuzzy_dist) %>%
    distinct()
  
  ## Combine unique solutions and unsolved
  solved1 <- bind_rows(out1, out2, out3)
  
  ## Check
  #solved1 %>% group_by(sc_name) %>% summarise(count = n()) %>% filter(count > 1) %>% pull(sc_name)
  
  
  
  ## -- Combine remaining unsolved ------------------------------------------
  nout1  <- out_tab %>% filter(result_type == "no service") %>% 
    pull(sc_name) %>% unique() %>% setdiff(., solved1$sc_name)
  nout1b <- out_tab %>% filter(result_type == "all services", num_input != num_process, process == first_process) %>% ## Author conflict in LCVP backbone
    pull(sc_name) %>% unique() 
  nout2  <- out_tab %>% filter(result_type == "genus only", process == "tropicos_gnr_resolve", status != "accepted") %>% 
    pull(sc_name) %>% unique()
  nout3  <- out3_tmp %>% filter(!unique_firstprocess) %>% 
    pull(sc_name) %>% unique()
  
  notsolved1 <- c(nout1, nout1b, nout2, nout3) %>% unique() %>% sort()
  
  ## Tests
  length(notsolved1) == length(unique(out_tab$sc_name)) - length(unique(solved1$sc_name))
  # tt  <- setdiff(unique(out_tab$sc_name), unique(solved1$sc_name))
  # tt2 <- setdiff(notsolved1, tt)
  
  
  ## -- STAT2: Numbers after first round of services ------------------------
  stat2 <- rbind(
    c("Initial number of inputs", length(species_cleaned$scientific_name)                           ),
    c("Unique number of inputs" , length(unique(species_cleaned$scientific_name))                   ),
    c("Number of cleaned inputs", length(unique(species_cleaned$input_ready))                       ),
    c("Number of matches"       , solved1 %>% nrow()                                                ),
    c(" - directly accepted"    , out1 %>% filter(status == "accepted" & fuzzy_dist == 0) %>% nrow()),
    c(" - accepted with typos"  , out1 %>% filter(status == "accepted" & fuzzy_dist != 0) %>% nrow()),
    c(" - synonyms"             , out1 %>% filter(status == "synonym") %>% nrow()                   ),
    c(" - genus only (Tropicos)", nrow(out2)                                                        ),
    c(" - conflicts (*)"        , nrow(out3)                                                        ),
    c("Remaining to solve"      , length(notsolved1)                                                ),
    c(""                                             , ""),
    c("(*) first process in stat1 used as reference.", "")
  ) 
  
  stat2 <- tibble(step = stat2[,1], count = stat2[,2])
  
  write_csv(stat2, file.path(.save_table, paste0(filename, "-", format(Sys.time(), format = "%Y-%m-%d-%H%M"), "-stat2.csv")))
  
  time2 <- Sys.time()
  dt    <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...Round 1 analysis: ", length(notsolved1), " inputs remaining to solve - ", dt, " sec."))
  
  
  
  ##
  ## Call services round 2 ##################################################
  ##
  
  ## --- pow_search() on remaining not solved -------------------------------
  if (length(notsolved1) != 0) {

    message("Send remaining issues to Kew Plants of the World Online...")

    ## Run service
    res_pow <- solve_pow(
      .taxon      = notsolved1,
      .save_table = .save_table,
      .filename   = filename
      )

    print(table(res_pow$tab$status, useNA = "always"))
    notsolved_pow <- res_pow$tab %>% filter(status %in% c("noref", "unresolved")) %>% pull(sc_name)

  }
  
  ## --- Make final output table --------------------------------------------
  out_tab2 <- res_pow$tab %>% 
    mutate(
      num_input   = 1,
      num_taxon   = 1,
      num_dup     = 1,
      result_type = "POW validation"
      ) %>%
    bind_rows(out_tab, .)
  
  species_final <- res_pow$tab %>%
    select(sc_name, accepted_name, accepted_author, status, fuzzy_dist) %>%
    bind_rows(solved1, .)
  
  nrow(species_final) == length(unique(species_cleaned$input_ready))
  
  
  
  ## --- Add Global Tree Search status --------------------------------------
  
  
  ## --- Add IUCN status ----------------------------------------------------
  
  ## Output #################################################################
  
  out <- list(tab = tab, duration = duration , stat1 = stat1, stat2 = stat2)
  
  
  out
  
} ## End function species_solve()



