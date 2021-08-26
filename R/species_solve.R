
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

## --- Function parameters:
## ---  .path: path to species list to solve
## ---  .how_to: how to combine or not the taxonomic name resolution services:
## ---    "compare"  : Run the input species list on all services
## ---    "integrate": Run the species list on all service but submit only the unmatch species from service n to service n=1 
## ---    "lcvp"     : Run on LCVP only
## ---    "wfo_lcvp" : Run on WFO algorithm with LCVP table backbone only
## ---    "wfo"      : Run on WFO only
## ---    "tropicos  : Run on Tropicos only
## ---  .save_table  : NULL or path to save the service outputs. Raw and harmonized 
## ---                 outputs are written while only the harmonized outputs are returned by the function 
## ---  .multicore   : logical. if TRUE, relies on parallel, future, furrr and carrier packages. Use plan(multisession) 
## ---                 as compatible with all OS types
## ---  .ref_lcvp    : NULL or path to file LCVP backbone for WFO.match(). Required when all services or "wfo_lcvp" are used.
## ---  .ref_wfo     : NULL or path to file WFO backbone for WFO.match(). Required when all services or "wfo" are used.



species_solve <- function(.path, .how_to = "wfo_lcvp", .save_table = NULL, 
                          .multicore = TRUE, .slices_threshold = 100,
                          .ref_lcvp = NULL, .ref_wfo = NULL, .ref_gts = NULL, 
                          .gts = NULL, .tx_src = NULL) {
  
  ## !!! For testing only
  # .path             <- "demo/NFMA_species_mess.csv"
  # .how_to           <- "compare"
  # .save_table       <- path_res
  # .multicore        <- TRUE
  # .slices_threshold <- 100
  # .ref_lcvp         <- wfo_backbone_lcvp
  # .ref_wfo          <- wfo_file
  # .ref_gts          <- "" ## TBD making WFO backbone from GTS
  # .gts              <- gts_file
  # .tx_src           <- src_tropicos
  ## !!!
  
  time_start <- Sys.time()
  
  message("\n---\nInitiating Taxonomic Resolution.\n---\n")
  
  # write_file(x = "print(1+1)", file = "demo/test.R")
  # rstudioapi::jobRunScript("demo/test.R", "test", workingDir = getwd(), importEnv = T, exportEnv = "R_GlobalEnv")
  
  
  
  ## Check function inputs ##################################################
  
  stopifnot(is.character(.path))
  stopifnot(str_ends(.path, "csv"))
  stopifnot(.how_to %in% c("compare", "integrate", "lcvp", "wfo_lcvp", "wfo", "tropicos"))
  stopifnot(is.logical(.multicore))
  
  ## Check if packages installed (https://stackoverflow.com/questions/9341635/check-for-installed-packages-before-running-install-packages)
  if (.multicore) stopifnot(nzchar(system.file(package = "furrr"))) ## future and parallel are loaded from furrr
  if (.how_to %in% c("compare", "integrate", "lcvp"))     stopifnot(nzchar(system.file(package = "lcvplants")))
  if (.how_to %in% c("compare", "integrate", "wfo_lcvp")) stopifnot(nzchar(system.file(package = "WorldFlora")))
  if (.how_to %in% c("compare", "integrate", "wfo"))      stopifnot(nzchar(system.file(package = "WorldFlora")))
  if (.how_to %in% c("compare", "integrate", "tropicos")) stopifnot(nzchar(system.file(package = "taxize")))
  
  
  ## Check if reference files for WFO.match() exist
  if (!is.null(.save_table) & .how_to %in% c("compare", "integrate", "wfo_lcvp")) stopifnot(file.exists(.ref_lcvp))
  if (!is.null(.save_table) & .how_to %in% c("compare", "integrate", "wfo"))      stopifnot(file.exists(.ref_wfo))
  
  ## Check if path to save tables exists
  if (!is.null(.save_table)) stopifnot(dir.exists(.save_table))
  
  
  
  ## Initiation #############################################################
  
  filename <- get_filename(.path)
  
  species_cleaned <- species_clean(.path) %>%
    filter(!is.na(input_ready)) %>% 
    pull(input_ready) %>% 
    unique()
  
  ## Split species (inc. intraspecies) for genus alone #####################
  # species_notsolved <- setdiff(species_cleaned, word(species_cleaned)) %>% unique() %>% sort()
  # genus_notsolved   <- setdiff(species_cleaned, species_notsolved) %>% unique() %>% sort()
  
  ## Keep all species and genus alone together
  species_notsolved <- species_cleaned
  
  ## !!!For testing only
  # set.seed(12)
  # species_notsolved <- sample(species_notsolved, 50)
  ## !!!
  
  ## Check
  #stopifnot(length(species_cleaned) == length(species_notsolved) + length(genus_notsolved))
  
  
  
  ## Implementation #########################################################
  
  ## --- 1. LCVP ------------------------------------------------------------
  if (.how_to %in% c("compare", "integrate", "lcvp")) {
    
    message("Start LCVP...")
    
    
    ## Select data
    ## --- Data is the same as first service
    
    ## Run service
    res_lcvp <- solve_lcvp(.taxon = species_notsolved, .save_table = .save_table, .filename = filename)
    
    print(table(res_lcvp$status, useNA = "always"))
    res_lcvp_notsolved <- res_lcvp %>% filter(status == "noref") %>% pull(submitted_name)
    
  } else {
    
    res_lcvp <- NULL
    
  }## End if LCVP
  
  
  
  ## --- 2. WFO on LCVP reference data --------------------------------------
  if (.how_to %in% c("compare", "integrate", "wfo_lcvp")) {
    
    message("Start WFO on LCVP reference data...")
    
    ## Select data
    if (.how_to == "integrate") species_notsolved <- setdiff(species_notsolved, res_lcvp_notsolved)
    
    ## Run service
    res_wfo_lcvp <- solve_wfo(
      .taxon      = species_notsolved, 
      .ref_file   = .ref_lcvp,
      .ref_name   = "Leipzig Catalogue of Vascular Plants", 
      .multicore  = .multicore, 
      .save_table = .save_table,
      .filename   = filename
      )
    
    print(table(res_wfo_lcvp$status, useNA = "always"))
    res_wfo_lcvp_notsolved <- res_wfo_lcvp %>% filter(status == "noref") %>% pull(submitted_name) 
    
  } else {
    
    res_wfo_lcvp <- NULL
    
  } ## End if WFO on LCVP
  
  
  
  ## --- 3. WFO on WFO reference data ---------------------------------------
  if (.how_to %in% c("compare", "integrate", "wfo")) {
    
    message("Start WFO...")
    
    ## Select data
    if (.how_to == "integrate") species_notsolved <- setdiff(species_notsolved, res_wfo_lcvp_notsolved)
    
    ## Run service
    res_wfo <- solve_wfo(
      .taxon      = species_notsolved, 
      .ref_file   = .ref_wfo,
      .ref_name   = "World Flora Online", 
      .multicore  = .multicore, 
      .save_table = .save_table,
      .filename   = filename
      )
    
    print(table(res_wfo_lcvp$status, useNA = "always"))
    res_wfo_lcvp_notsolved <- res_wfo_lcvp %>% filter(status == "noref") %>% pull(submitted_name) 
    
  } else {
    
    res_wfo <- NULL
    
  } ## End if WFO
  
  
  
  ## --- 4. Tropicos --------------------------------------------------------
  if (.how_to %in% c("compare", "integrate", "tropicos")) {
    
    message("Start Tropicos...")
    
    ## Select data
    if (.how_to == "integrate") species_notsolved <- setdiff(species_notsolved, res_wfo_lcvp_notsolved)
    
    ## Run service
    res_tropicos <- solve_tropicos(
      .taxon      = species_notsolved, 
      .gnr_src    = .tx_src,
      .save_table = .save_table,
      .filename   = filename
    )
    
    print(table(res_tropicos$status, useNA = "always"))
    res_tropicos_notsolved <- res_tropicos %>% filter(status == "noref") %>% pull(submitted_name) 

  } ## End if WFO
  
  
  
  
  ## Analysis results #######################################################
  
  
  
  ## Output #################################################################
  
  out <- mget(ls(pattern = "res_"))
  out
  
  
} ## End function species_solve()



