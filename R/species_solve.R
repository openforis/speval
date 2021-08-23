
## Function to solve tree species taxonomic names against existing taxonomic databases
## Note: species_solve() is developed to run on a clean data output of species_clean()
## and may not work if the species list has not been striped of special characters and common typos.
## Note: List of packages required: TBD
## Input a vector with species names
## Output a list with:
## - data frame with input and solved names
## - data frame with summary stats on the process

## Order for Genus solve - !!! ABORTED !!! Too many genus are almost identical making it 
## impossible to programmatically correct typos. EX: Danniella returns Daniella, Darniella, Dunniella
## 1. Tropicos
## 2. Kew
## 3. NCBI - National Center for Biotechnology Information, db="taxonomy" [online]
## 4. WFO  - WorldFlora Online [offline]
## 5. GBIF - Global Biodiversity Information Facility [online]

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




species_solve <- function(.path, .how_to, .save_table, .with_jobs = FALSE, 
                          .multicore = TRUE, .slices_threshold = 100,
                          .ref_lcvp = NULL, .ref_wfo = NULL, .ref_gts = NULL, 
                          .gts = NULL, .src_tropicos = NULL) {
  
  ## !!! For testing only
  .path               <- "demo/NFMA_species_mess.csv"
  .how_to             <- "comparative"
  .save_table         <- path_res
  .with_jobs          <- TRUE
  .multicore          <- TRUE
  .slices_threshold   <- 100
  .ref_lcvp           <- paste0(path_data, "/", wfo_backbone_lcvp)
  .ref_wfo            <- paste0(path_data, "/", wfo_file)
  .ref_gts            <- "" ## TBD making WFO backbone from GTS
  .gts                <- paste0(path_data, "/", gts_file)
  .src_tropicos       <- src_tropicos
  
  
  # .ref_name <- "Leipzig Catalogue of Vascular Plants"
  # .multicore <-  TRUE
  # 
  ## !!!
  
  time_start <- Sys.time()
  
  message("Initiating Taxonomic Resolution...")
  
  # write_file(x = "print(1+1)", file = "demo/test.R")
  # rstudioapi::jobRunScript("demo/test.R", "test", workingDir = getwd(), importEnv = T, exportEnv = "R_GlobalEnv")
  
  
  
  ## Check function inputs ##################################################
  
  stopifnot(is.character(.path))
  stopifnot(str_ends(.path, "csv"))
  
  
  
  ## Initiation #############################################################
  
  filename <- get_filename(.path)
  
  species_cleaned <- species_clean(.path) %>%
    filter(!is.na(input_ready)) %>% 
    pull(input_ready) %>% 
    unique()
  
  ## Split species (inc. intraspecies) for genus alone #####################
  species_notsolved <- setdiff(species_cleaned, word(species_cleaned)) %>% unique() %>% sort()
  genus_notsolved   <- setdiff(species_cleaned, species_notsolved) %>% unique() %>% sort()
  
  ## !!!For testing only
  # set.seed(12)
  # species_notsolved <- sample(species_notsolved, 50)
  ## !!!
  
  ## Check
  stopifnot(length(species_cleaned) == length(species_notsolved) + length(genus_notsolved))
  
  ## Create a temporary directory for saving function input for jobs
  if(.with_jobs) dir.create("tmp", showWarnings = F)
  
  
  
  ## Implementation #########################################################
  
  ## ************************************************************************
  ## --- 1. LCVP ------------------------------------------------------------
  ## ************************************************************************
  
  if (.how_to %in% c("comparative", "sequential", "lcvp")) {
    
    ## Select data
    ## --- Data is the same as first service
    
    ## Run service
    if (.with_jobs) {
      
      ## Save function inputs 
      save(species_notsolved, .save_table, file = paste0("tmp/lcvp_inputs.Rdata"))
      
      ## Make script
      job_script <- paste0(
        "source('global.R', local = T)\n",
        "load(file = paste0('tmp/lcvp_inputs.Rdata'))\n",
        ".path <- '", .path, "'\n",
        "res_lcvp <- solve_lcvp(.taxon = species_notsolved, .save_table = .save_table)"
      )
      
      ## Save script
      write_file(job_script, file = paste0("tmp/lcvp_job.R"))
      
      ## Run script
      rstudioapi::jobRunScript(paste0("tmp/lcvp_job.R"), name = "LCVP", workingDir = getwd(), exportEnv = "R_GlobalEnv")
      
      
    } else {
      
      res_lcvp <- solve_lcvp(.taxon = species_notsolved, .save_table = .save_table)
      
    } ## End if Run service
    
    res_lcvp_notsolved <- res_lcvp %>% filter(status == "noref") %>% pull(submitted_name)
    
  } ## End if LCVP
  
  
  ## ************************************************************************
  ## --- 2. WFO on LCVP reference data --------------------------------------
  ## ************************************************************************
  if (.how_to %in% c("comparative", "sequential", "wfo_lcvp")) {
    
    ## Select data
    if (.how_to == "sequential") species_notsolved <- setdiff(species_notsolved, res_lcvp_notsolved)
  
    ## Run service
    if (.with_jobs) {
      
      ## Save function inputs
      save(species_notsolved, .ref_lcvp, .multicore, .save_table, 
           file = paste0("tmp/wfo_lcvp_inputs.Rdata"))
      
      ## Make script
      job_script <- paste0(
        "source('global.R', local = T)\n",
        "load(file = paste0('tmp/wfo_lcvp_inputs.Rdata'))\n",
        ".path <- '", .path, "'\n",
        "res_wfo_lcvp <- solve_wfo(
        .taxon    = species_notsolved,
        .ref_file = .ref_lcvp, 
        .ref_name = 'Leipzig Catalogue of Vascular Plants',
        .multicore = .multicore,
        .save_table = .save_table
        )"
      )
      
      ## Save script
      write_file(job_script, file = paste0("tmp/wfo_lcvp_script.R"))
      
      ## Run script
      rstudioapi::jobRunScript(paste0("tmp/wfo_lcvp_script.R"), name = "WFO_LCVP", workingDir = getwd(), exportEnv = "R_GlobalEnv")
      
      
    } else {
      
      res_wfo_lcvp <- solve_wfo(
        .taxon      = species_notsolved, 
        .ref_file   = .ref_lcvp,
        .ref_name   = "Leipzig Catalogue of Vascular Plants", 
        .multicore  = .multicore, 
        .save_table = .save_table
        )
      
    } ## End if run service
    
    res_wfo_lcvp_notsolved <- res_wfo_lcvp %>% filter(status == "noref") %>% pull(submitted_name) 
    
  } ## End if WFO on LCVP
  
  
  
  
  
  if(.with_jobs) unlink("tmp", showWarnings = F)
  
} ## End function species_solve()



#   ## --- 1bis. Solve with LCVP data and WFO.match() algorythm ---------------
#   ## --- Offline
#   ## --- Data source: LCVP::tab_lcvp converted to WFO backbone
#   ## --- Algorithm: WorldFlora::WFO.match()
#   
#   ## Choosing data
#   input  <- species_init
#   
#   ## Create function to avoid loading the whole environment to the workers
#   crt_lcvp <- crate(
#     input     = input,
#     data_lcvp = data_lcvp,
#     function(.x){
#       message("Processing sequence: ", min(.x), " to ", max(.x), ".")
#       wfo_input = input[.x]
#       WorldFlora::WFO.match(wfo_input, WFO.data = data_lcvp)
#     }
#   )
#   
#   ## Make chunks
#   input_chunks <-furrr:::make_chunks(n_x = length(input), n_workers = n_cores, chunk_size = 200)
#   
#   
#   ## --- RUN WFO algo on LCVP data ---
#   message("...Running WFO.match() on LCVP data.")
#   
#   time1 <- Sys.time()
#   future::plan(multisession)
#   solved_lcvp_wfoalgo <- furrr::future_map_dfr(.x = input_chunks, .f = crt_lcvp, .options = furrr::furrr_options(globals = FALSE))
#   future::plan(sequential)
#   #future::plan(sequential, .cleanup = T)
#   time2 <- Sys.time()
#   dt    <- round(as.numeric(time2-time1, units = "secs"))
#   message(paste0("...Taxons solved with WFO", " - ", dt, " sec."))
#   ## ---
#   
#   ## output object to .GlobalEnv but just to be safe, also write csv back to demo file
#   write_csv(solved_lcvp_wfoalgo, "demo/NFMA_job_lcvp_wfoalgo.csv")
#   write_tsv(tibble(NULL), paste0("demo/NFMA_job_lcvp_wfoalgo-", dt,"-secs.txt"))
#   
#   # ## !!! For testing only: run tnrs on a job instead of the console 
#   # rstudioapi::jobRunScript("R-jobs/solve_lcvp_wfoalgo.R", "LCVP_WFOALGO", workingDir = getwd(), importEnv = T, exportEnv = "R_GlobalEnv")
#   # solved_lcvp_wfoalgo <- read_csv("demo/NFMA_job_lcvp_wfoalgo.csv", show_col_types = F)
#   # ## !!!
#   
#   # ## --- Arrange results ---
#   # ## Initiate output fom LVCP results as contain input species list and results.
#   # species_all <- as_tibble(solved_lcvp) %>%
#   #   mutate(
#   #     fuzzysum = Insertion + Deletion + Substitution,
#   #     service = "lcvp"
#   #     ) %>%
#   #   select(
#   #     input      = Submitted_Name, 
#   #     status     = Status, 
#   #     lcvp_taxon = LCVP_Accepted_Taxon, 
#   #     service,
#   #     pl_comparison  = PL_Comparison, 
#   #     pl_alternative = PL_Alternative, 
#   #     score          = Score, 
#   #     fuzzysum
#   #     )
#   # 
#   # ## Checks
#   # # table(species_all$status)
#   # table(species_all$score)
#   # # length(unique(species_lcvp2$input)) == length(species_input)
#   # 
#   # ## Extract unresolved
#   # species_notsolved <- species_all %>% 
#   #   filter(!(status %in% c("accepted", "synonym"))) %>%
#   #   pull(input) %>%
#   #   unique() %>%
#   #   sort()
#   # 
#   # ## In case genus alone slipped through:
#   # species_notsolved <- setdiff(species_notsolved, word(species_notsolved)) %>% unique() %>% sort()
#   # 
#   # ## Remove unresolved from out table
#   # species_solved <- species_all %>% 
#   #   filter(status %in% c("accepted", "synonym"))
#   # 
#   # ## END LCVP message 
#   # message(paste0("......Nb taxons remaining unsolved: ", length(species_notsolved)))
#   
#   
#   
#   ## --- 2. Solve with tropicos ---------------------------------------------
#   ## --- Online
#   ## --- Data source: taxize::gnr_datasources()
#   ## --- Algorithm: taxize::gnr_resolve()
#   ## --- Performs better with slices (larges batches have less hits)
#   ## ---
#   ## --- https://www.tropicos.org/home
#   ## --- http://services.tropicos.org/help?method=SearchNameXml
#   ## --- see taxize manual at https://cran.r-project.org/web/packages/taxize/taxize.pdf
#   ## --- and also this guidance: http://viktoriawagner.weebly.com/blog/cleaning-species-names-with-r-ii-taxize
#   
#   
#   # ## Choosing data and making slices
#   # input           <- species_init
#   # #set.seed(11)
#   # #input           <- input[sample(seq_along(input), 250, replace = F)]
#   # slices          <- c(0:trunc(length(input) / 200) * 200, length(input))
#   # slices
#   # 
#   # 
#   # ## --- RUN TROPICOS ---
#   # message("...Running Tropicos.")
#   # time1 <- Sys.time()
#   # 
#   # ## Getting tropicos id for taxize::gnr_resolve() is not supplied
#   # if(is.null(.src_tropicos)) {
#   #   .src_tropicos <- taxize::gnr_datasources() %>% 
#   #     filter(title == "Tropicos - Missouri Botanical Garden") %>% 
#   #     pull(id)
#   # }
#   # 
#   # ## !!! SLICING THE TABLE IS SLOWER BUT RESULTS IN MORE MATCHES
#   # # genus_tropicos <- taxize::gnr_resolve(sci = input_genus, data_source_ids = .src_tropicos, with_canonical_ranks = T)
#   # 
#   # ## Run Tropicos
#   # # ## map_dfr() should have increased performance over for loops and output directly a data frame
#   # # #input           <- species_notsolved
#   # # input           <- species_init
#   # # slices          <- c(0:trunc(length(input) / 100) * 100, length(input))
#   # # solved_tropicos <- purrr::map_dfr(.x = seq_along(slices[-length(slices)]), .f = function(x){
#   # #   
#   # #   message(paste0("Sequence: ", slices[x]+1, " to ", slices[x+1], "\n"))
#   # #   tmp_list <- input[slices[x]+1:slices[x+1]]
#   # #   taxize::gnr_resolve(sci = tmp_list, data_source_ids = src_tropicos, with_canonical_ranks = T)
#   # #   
#   # # }) ## End map_dfr()
#   # 
#   # ## Run with furrr - multicore version of purrr::map(), themselves tidyverse equivalent of apply()  
#   # future::plan(multisession)
#   # 
#   # solved_tropicos <- furrr::future_map_dfr(.x = seq_along(slices[-length(slices)]), .f = function(x){
#   #   
#   #   message(paste0("Sequence: ", slices[x]+1, " to ", slices[x+1], "\n"))
#   #   tmp_list <- input[slices[x]+1:slices[x+1]]
#   #   taxize::gnr_resolve(sci = tmp_list, data_source_ids = .src_tropicos, with_canonical_ranks = T)
#   #   Sys.sleep(0.5)
#   #   
#   # }) ## End map_dfr()
#   # 
#   # future::plan(sequential)
#   # 
#   # time2 <- Sys.time()
#   # dt    <- round(as.numeric(time2-time1, units = "secs"))
#   # message(paste0("...Taxons solved with Tropicos", " - ", dt, " sec."))
#   # ## --- END RUN TROPICOS ---
#   # 
#   # ## !!! For testing only: run tnrs on a job instead of the console 
#   # rstudioapi::jobRunScript("R-jobs/solve_tropicos.R", "TROPICOS", workingDir = getwd(), importEnv = T, exportEnv = "R_GlobalEnv")
#   # solved_tropicos <- read_csv("demo/NFMA_job_tropicos.csv", show_col_types = F)
#   # ## !!!
#   
#   
#   ## Checks
#   # length(unique(species_notsolved))
#   # length(solved_tropicos$user_supplied_name)
#   # length(unique(solved_tropicos$user_supplied_name))
#   
#   
#   ## --- Arrange results ---
#   ## --- Tropicos doesn't return the full set of inputs.
#   # species_solved2 <- solved_tropicos %>%
#   #   mutate(
#   #     status = ,
#   #     
#   #   ) %>%
#   #   select(
#   #     input      = Submitted_Name, 
#   #     status     = Status, 
#   #     lcvp_taxon = LCVP_Accepted_Taxon, 
#   #     service,
#   #     pl_comparison  = PL_Comparison, 
#   #     pl_alternative = PL_Alternative, 
#   #     score          = Score, 
#   #     fuzzysum
#   #   )
#   # message(paste0("......Nb genus remaining unsolved: ", length(genus_notsolved)))
#   
#   
#    
#   ## --- 3. Solve with World Flora Online -----------------------------------
#   ## --- Offline
#   ## --- Data source: "http://104.198.143.165/files/WFO_Backbone/_WFOCompleteBackbone/WFO_Backbone.zip"
#   ## --- Algorithm: WFO.match()
#   ## --- Performs better with parallel computing
#   ## ---
#   ## --- http://www.worldfloraonline.org/
#   ## --- https://github.com/cran/WorldFlora
#   
#   
#   ## Choosing data and making slices
#   input <- species_init
#   # set.seed(11)
#   # input <- input[sample(seq_along(input), 100, replace = F)] 
#   
#   ## Create function to avoid loading the whole environment to the workers
#   crt_wfo <- crate(
#     input    = input,
#     data_wfo = data_wfo,
#     function(.x){
#       message("Processing sequence: ", min(.x), " to ", max(.x), ".")
#       wfo_input = input[.x]
#       WorldFlora::WFO.match(wfo_input, WFO.data = data_wfo)
#       }
#   )
# 
#   ## Make chunks
#   input_chunks <-furrr:::make_chunks(n_x = length(input), n_workers = n_cores, chunk_size = 200)
#   
#   
#   ## --- RUN WFO ---
#   message("...Running WFO.")
#   
#   # time1 <- Sys.time()
#   # solved_wfo1 <- WorldFlora::WFO.match(input, WFO.data = wfo_data)
#   # time2 <- Sys.time()
#   # dt    <- round(as.numeric(time2-time1, units = "secs"))
#   # message(paste0("...Taxons solved with WFO", " - ", dt, " sec."))
#   # write_tsv(tibble(NULL), paste0("demo/NFMA_wfosinglecore-", dt,"-secs.txt"))
#   # 
#   # time1 <- Sys.time()
#   # solved_wfo2 <- WorldFlora::WFO.match(input, WFO.file = paste0(wfo_path, "/", wfo_class)) 
#   # time2 <- Sys.time()
#   # dt    <- round(as.numeric(time2-time1, units = "secs"))
#   # message(paste0("...Taxons solved with WFO", " - ", dt, " sec."))
#   
#   time1 <- Sys.time()
#   future::plan(multisession)
#   solved_wfo <- furrr::future_map_dfr(.x = input_chunks, .f = crt_wfo, .options = furrr::furrr_options(globals = FALSE))
#   future::plan(sequential)
#   time2 <- Sys.time()
#   dt    <- round(as.numeric(time2-time1, units = "secs"))
#   message(paste0("...Taxons solved with WFO", " - ", dt, " sec."))
#   ## ---
#   
#   ## output object to .GlobalEnv but just to be safe, also write csv back to demo file
#   write_csv(solved_wfo, "demo/NFMA_job_wfo.csv")
#   write_tsv(tibble(NULL), paste0("demo/NFMA_job_wfo-", dt,"-secs.txt"))
#   
#   
#   ## !!! For testing only: run tnrs on a job instead of the console 
#   # rstudioapi::jobRunScript("R-jobs/solve_wfo.R", "WFO", workingDir = getwd(), importEnv = T, exportEnv = "R_GlobalEnv")
#   # solved_wfo <- read_csv("demo/NFMA_job_wfo.csv", show_col_types = F)
#   ## !!!
#   
#   
#   # repo_df$species_count[5] <- n_left
#   # 
#   # if (n_left > 0) {
#   #   
#   #   bParallel= TRUE
#   #   # not parallel for less than n_cores
#   #   bParallel <- ifelse(nrow(sp.no_hit) < n_cores, FALSE, bParallel)
#   #   
#   #   if (bParallel==TRUE) {
#   #     if (os == "linux") {
#   #       #    required package: doMC
#   #       registerDoMC(n_cores)  #change the 2 to your number of CPU cores 
#   #       
#   #       cl <- parallel::makeCluster(n_cores, outfile= "")
#   #       ##END LINUX WAY
#   #     }
#   #     
#   #     if (os == "windows") {
#   #       #      memory.limit(size=56000)
#   #       cl <- parallel::makeCluster(n_cores, outfile= "")
#   #     }
#   #     registerDoParallel(cl)
#   #   }
#   #   
#   #   print(  paste0("V. WFO: Number of species to check: ", nrow(sp.no_hit)))
#   #   message(paste0("V. WFO: Number of species to check: ", nrow(sp.no_hit)))
#   #   
#   #   # https://stackoverflow.com/questions/17350867/split-data-set-and-pass-the-subsets-in-parallel-to-function-then-recombine-the-r
#   #   cuts <- cut(1:nrow(sp.no_hit), n_cores)
#   #   
#   #   if (bParallel == TRUE) {
#   #     sys.time<-system.time({ 
#   #       
#   #       sp.hit <- foreach(x=levels(cuts), .packages = "WorldFlora", .combine=rbind, .multicombine=TRUE) %dopar% { 
#   #         WFO.match(sp.no_hit[cuts==x ,]$scientific_name, WFO.file=WFO_file)
#   #       }
#   #     })  
#   #     stopCluster(cl); print("Cluster stopped.")
#   #     # insert serial backend, otherwise error in repetitive tasks, https://github.com/tobigithub/R-parallel/wiki/R-parallel-Errors
#   #     registerDoSEQ() 
#   #     
#   #     print(sys.time)
#   #     
#   #   } else {
#   #     sp.hit <- try( WFO.match(spec.data=sp.no_hit,spec.name="scientific_name", WFO.file=WFO_file, counter=10), silent = TRUE) 
#   #     # Note:  Fuzzy.min = TRUE, is the default setting, see e.g. https://www.biorxiv.org/content/10.1101/2020.02.02.930719v1.full.pdf
#   #   }
#   #   
#   #   if (exists("sp.hit")) {
#   #     if (!is.null(nrow(sp.hit))) {
#   #       sp.hit$row_name <- as.numeric(row.names(sp.hit))
#   #       # remove new rows in returned dataframe
#   #       sp.hit          <- subset(sp.hit,(row_name - trunc(row_name) == 0 ))
#   #       
#   #       sp.hit <- sp.hit %>%
#   #         dplyr::select(scientific_name=spec.name, Authors=scientificNameAuthorship, Status=taxonomicStatus,
#   #                       Accepted_Taxon=scientificName, PL_Comparison=Old.name, Alternative=Old.name, Matched, Fuzzy) %>% 
#   #         dplyr::mutate(Authors = ifelse( str_count(scientific_name, " ") > 0 & str_count(Accepted_Taxon, " ") == 0, "", Authors  ),
#   #                       Status         = ifelse( str_count(scientific_name, " ") > 0 & str_count(Accepted_Taxon, " ") == 0, "", Status        ),
#   #                       Matched        = ifelse( str_count(scientific_name, " ") > 0 & str_count(Accepted_Taxon, " ") == 0, "", Matched       ),
#   #                       Accepted_Taxon = ifelse( str_count(scientific_name, " ") > 0 & str_count(Accepted_Taxon, " ") == 0, "", Accepted_Taxon),
#   #                       Score          = ifelse(Matched=="TRUE" & Fuzzy=="TRUE", "matched (fuzzy)", ifelse(Matched=="TRUE", "matched", ""    )),
#   #                       Veri_source    = ifelse(Matched=="TRUE", "WFO", ""),
#   #                       Status         = tolower(Status),
#   #                       Alternative    = ifelse(Alternative==scientific_name, "", Alternative),
#   #                       PL_Comparison  = "") %>%
#   #         dplyr::select(-Matched, -Fuzzy)
#   #       
#   #       sp.accepted <- rbind(sp.accepted, sp.hit)
#   #       
#   #       sp.no_hit <- sp.hit %>%
#   #         dplyr::filter(Veri_source=="")
#   #       
#   #       n_left    <- nrow(sp.no_hit)
#   #       rm(sp.hit)
#   #     } # exists("sp.hit")
#   #   }
#   # }
#   
#   
#   
#    
#     ## -- 2. Solve genus with Kew -------------------------------------------
#     
#     # if (length(genus_notsolved) > 0 ) {
#     #   
#     #   time1 <- Sys.time()
#     #   message("...Running Kew on unsolved genus.")
#     #   
#     #   
#     #   #slices    <- c(0:trunc(length(genus_notsolved) / .slices_threshold) * .slices_threshold, length(genus_notsolved))
#     #   genus_pow <- map_dfr(.x = seq_along(genus_notsolved), .f = function(x, input = genus_notsolved){
#     #     
#     #     if (x == 1)                  message("genus ", x, " out of ", length(input), ".")
#     #     if (round(x / 10) * 10 == x) message("genus ", x, " out of ", length(input), ".")
#     #     if (x == length(input))      message("genus ", x, " out of ", length(input), ".")
#     #     
#     #     pow <- taxize::pow_search(input[x], limit = 1)
#     #     
#     #     out <- pow$data
#     #     
#     #     if (!is.null(out)) {
#     #       message(paste0("Hit for genus ", x, ": ", input[x])) 
#     #       out$input <- input[x]
#     #     }
#     #     
#     #     return(out)
#     #     
#     #   }) ## END map_dfr()
#     #   
#     #   genus_pow2 <- genus_pow %>% 
#     #     as_tibble() %>% 
#     #     filter(accepted == TRUE) %>%
#     #     mutate(
#     #       genus_pow = case_when(
#     #         rank == "Species" ~ word(name),
#     #         rank == "Genus"   ~ name, 
#     #         TRUE ~ NA_character_
#     #       )
#     #     ) %>%
#     #     select(input, genus_pow)
#     #   
#     #   genus_solved2 <- genus_solved %>% 
#     #     left_join(genus_pow2, by = c("genus_input" = "input"))
#     #   
#     #   genus_notsolved <- genus_solved %>% 
#     #     filter(is.na(genus_tropicos)) %>%
#     #     pull(genus_input)
#     #   
#     #   time2 <- Sys.time()
#     #   dt <- round(as.numeric(time2-time1, units = "secs"))
#     #   message(paste0("...Genus solved with Tropicos", " - ", dt, " sec."))
#     #   message(paste0("......Nb genus remaining unsolved: ", length(genus_notsolved)))
#     #   
#     # } 
#     # 
#     #   
#   
#   ## Solve genus
#   
#   
#   
#   
#   
#   time_end <- Sys.time()
#   
#   
# }



