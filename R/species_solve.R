
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


species_solve <- function(.vec, .tree_global_search, .src_tropicos = NULL, .slices_threshold = 100, .withjobs = F){
  
  ## !!! For testing only
  #set.seed(11)
  .vec <- species_clean(.path = "demo/NFMA_species_mess.csv") %>% 
    filter(!is.na(input_ready)) %>% 
    #slice_sample(n = 200) %>%
    pull(input_ready) %>% 
    unique()
  .tree_global_search <- global_tree_search
  .src_tropicos       <- src_tropicos
  .slices_threshold   <- 100
  .withjobs           <-  F
  ## !!!
  
  time_start <- Sys.time()
  
  message("Initiating Taxonomic Resolution...")
  
  
  ## Validation #############################################################
  stopifnot(is.character(.vec))
  
  
  ## Split species (innc. intraspecies) for genus alone #####################
  species_init <- setdiff(.vec, word(.vec))   %>% unique() %>% sort()
  genus_init   <- setdiff(.vec, species_init) %>% unique() %>% sort()
  
  #length(.vec) == length(species_input) + length(genus_input)
  
  
  ## Solve species excluding genus alone entries ############################
  
  ## --- 1. Solve with LCVP -------------------------------------------------
  ## --- Offline
  ## --- Data source: LCVP::tab_lcvp
  ## --- Algorithm: lcvplants::LCVP())
  ## --- Performs better with full list than slices. Multi-cores integrated. max.distance = 2 recommended 
  ## ---
  ## --- https://idiv-biodiversity.github.io/lcvplants/articles/taxonomic_resolution_using_lcplants.html#running-lcvplants

  ## Choosing data and slices
  input <- species_init
  slices          <- c(0:trunc(length(input) / 200) * 200, length(input))
  slices
  
  # ## --- RUN LCVP ---
  # message("...Running Leipzig Catalogue of Vascular Plants.")
  # time1 <- Sys.time()
  # 
  # ## Run directly the function, multicores integrated
  # solved_lcvp <- lcvplants::LCVP(input, max.distance = 2, synonyms = F)
  # 
  # time2 <- Sys.time()
  # dt    <- round(as.numeric(time2-time1, units = "secs"))
  # message(paste0("...Taxons solved with LCVP", " - ", dt, " sec."))
  # ## --- END RUN LCVP ---
  
  ## !!! For testing only: run tnrs on a job instead of the console 
  rstudioapi::jobRunScript("R-jobs/solve_lcvp.R", "LCVP", workingDir = getwd(), importEnv = T, exportEnv = "R_GlobalEnv")
  solved_lcvp <- read_csv("demo/NFMA_job_lcvp_dist2.csv", show_col_types = F)
  ## !!!
  
  
  
  # ## --- RUN LCVP with furrr::future_map_dfr() ---
  # message("...Running Leipzig Catalogue of Vascular Plants.")
  # time1 <- Sys.time()
  # 
  # ## Run with furrr::future_map* family
  # plan(multisession)
  # 
  # solved_lcvp <- furrr::future_map_dfr(.x = seq_along(slices[-length(slices)]), .f = function(x){
  #   
  #   message(paste0("Sequence: ", slices[x]+1, " to ", slices[x+1], "\n"))
  #   tmp_list <- input[slices[x]+1:slices[x+1]]
  #   lcvplants::LCVP(tmp_list, max.distance = 2, synonyms = F, max.cores = 1)
  #   #Sys.sleep(0.5)
  #   
  # }) ## End map_dfr()
  # 
  # plan(sequential)
  # 
  # time2 <- Sys.time()
  # dt    <- round(as.numeric(time2-time1, units = "secs"))
  # message(paste0("...Taxons solved with LCVP", " - ", dt, " sec."))
  # ## --- END RUN LCVP ---
  
  ## !!! For testing only: run tnrs on a job instead of the console 
  rstudioapi::jobRunScript("R-jobs/solve_lcvp_furrr.R", "LCVP_furrr", workingDir = getwd(), importEnv = T, exportEnv = "R_GlobalEnv")
  solved_lcvp <- read_csv("demo/NFMA_job_lcvp_dist2_furrr.csv", show_col_types = F)
  ## !!!
  
  
  
  # ## --- Arrange results ---
  # ## Initiate output fom LVCP results as contain input species list and results.
  # species_all <- as_tibble(solved_lcvp) %>%
  #   mutate(
  #     fuzzysum = Insertion + Deletion + Substitution,
  #     service = "lcvp"
  #     ) %>%
  #   select(
  #     input      = Submitted_Name, 
  #     status     = Status, 
  #     lcvp_taxon = LCVP_Accepted_Taxon, 
  #     service,
  #     pl_comparison  = PL_Comparison, 
  #     pl_alternative = PL_Alternative, 
  #     score          = Score, 
  #     fuzzysum
  #     )
  # 
  # ## Checks
  # # table(species_all$status)
  # table(species_all$score)
  # # length(unique(species_lcvp2$input)) == length(species_input)
  # 
  # ## Extract unresolved
  # species_notsolved <- species_all %>% 
  #   filter(!(status %in% c("accepted", "synonym"))) %>%
  #   pull(input) %>%
  #   unique() %>%
  #   sort()
  # 
  # ## In case genus alone slipped through:
  # species_notsolved <- setdiff(species_notsolved, word(species_notsolved)) %>% unique() %>% sort()
  # 
  # ## Remove unresolved from out table
  # species_solved <- species_all %>% 
  #   filter(status %in% c("accepted", "synonym"))
  # 
  # ## END LCVP message 
  # message(paste0("......Nb taxons remaining unsolved: ", length(species_notsolved)))
  
  
  
  ## --- 2. Solve with tropicos ---------------------------------------------
  ## --- Online
  ## --- Data source: taxize::gnr_datasources()
  ## --- Algorithm: taxize::gnr_resolve()
  ## --- Performs better with slices (larges batches have less hits)
  ## ---
  ## --- https://www.tropicos.org/home
  ## --- http://services.tropicos.org/help?method=SearchNameXml
  ## --- see taxize manual at https://cran.r-project.org/web/packages/taxize/taxize.pdf
  ## --- and also this guidance: http://viktoriawagner.weebly.com/blog/cleaning-species-names-with-r-ii-taxize
  
  
  ## Choosing data and making slices
  input           <- species_init
  #set.seed(11)
  #input           <- input[sample(seq_along(input), 250, replace = F)]
  slices          <- c(0:trunc(length(input) / 200) * 200, length(input))
  slices
  
  
  ## --- RUN TROPICOS ---
  message("...Running Tropicos.")
  time1 <- Sys.time()
  
  ## Getting tropicos id for taxize::gnr_resolve() is not supplied
  if(is.null(.src_tropicos)) {
    .src_tropicos <- taxize::gnr_datasources() %>% 
      filter(title == "Tropicos - Missouri Botanical Garden") %>% 
      pull(id)
  }
  
  ## !!! SLICING THE TABLE IS SLOWER BUT RESULTS IN MORE MATCHES
  # genus_tropicos <- taxize::gnr_resolve(sci = input_genus, data_source_ids = .src_tropicos, with_canonical_ranks = T)
  
  ## Run Tropicos
  # ## map_dfr() should have increased performance over for loops and output directly a data frame
  # #input           <- species_notsolved
  # input           <- species_init
  # slices          <- c(0:trunc(length(input) / 100) * 100, length(input))
  # solved_tropicos <- purrr::map_dfr(.x = seq_along(slices[-length(slices)]), .f = function(x){
  #   
  #   message(paste0("Sequence: ", slices[x]+1, " to ", slices[x+1], "\n"))
  #   tmp_list <- input[slices[x]+1:slices[x+1]]
  #   taxize::gnr_resolve(sci = tmp_list, data_source_ids = src_tropicos, with_canonical_ranks = T)
  #   
  # }) ## End map_dfr()

  ## Run with furrr - multicore version of purrr::map(), themselves tidyverse equivalent of apply()  
  future::plan(multisession)
  
  solved_tropicos <- furrr::future_map_dfr(.x = seq_along(slices[-length(slices)]), .f = function(x){
    
    message(paste0("Sequence: ", slices[x]+1, " to ", slices[x+1], "\n"))
    tmp_list <- input[slices[x]+1:slices[x+1]]
    taxize::gnr_resolve(sci = tmp_list, data_source_ids = .src_tropicos, with_canonical_ranks = T)
    Sys.sleep(0.5)
    
  }) ## End map_dfr()
  
  plan(sequential)
  
  time2 <- Sys.time()
  dt    <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...Genus solved with Tropicos", " - ", dt, " sec."))
  ## --- END RUN TROPICOS ---
  
  ## !!! For testing only: run tnrs on a job instead of the console 
  rstudioapi::jobRunScript("R-jobs/solve_tropicos.R", "TROPICOS", workingDir = getwd(), importEnv = T, exportEnv = "R_GlobalEnv")
  solved_tropicos <- read_csv("demo/NFMA_job_tropicos.csv", show_col_types = F)
  ## !!!
  
  
  ## Checks
  # length(unique(species_notsolved))
  # length(solved_tropicos$user_supplied_name)
  # length(unique(solved_tropicos$user_supplied_name))
  
  
  ## --- Arrange results ---
  ## --- Tropicos doesn't return the full set of inputs.
  # species_solved2 <- solved_tropicos %>%
  #   mutate(
  #     status = ,
  #     
  #   ) %>%
  #   select(
  #     input      = Submitted_Name, 
  #     status     = Status, 
  #     lcvp_taxon = LCVP_Accepted_Taxon, 
  #     service,
  #     pl_comparison  = PL_Comparison, 
  #     pl_alternative = PL_Alternative, 
  #     score          = Score, 
  #     fuzzysum
  #   )
  # message(paste0("......Nb genus remaining unsolved: ", length(genus_notsolved)))
  
  
   
  ## --- 3. Solve with World Flora Online -----------------------------------
  ## --- Offline
  ## --- Data source: "http://104.198.143.165/files/WFO_Backbone/_WFOCompleteBackbone/WFO_Backbone.zip"
  ## --- Algorithm: WFO.match()
  ## --- Performs better with parallel computing
  ## ---
  ## --- http://www.worldfloraonline.org/
  ## --- https://github.com/cran/WorldFlora
  
  
  ## Choosing data and making slices
  input           <- species_init
  #set.seed(11)
  #input           <- input[sample(seq_along(input), 250, replace = F)]
  slices          <- c(0:trunc(length(input) / 200) * 200, length(input))
  slices
  
  
  
  
  repo_df$species_count[5] <- n_left
  
  if (n_left > 0) {
    
    bParallel= TRUE
    # not parallel for less than n_cores
    bParallel <- ifelse(nrow(sp.no_hit) < n_cores, FALSE, bParallel)
    
    if (bParallel==TRUE) {
      if (os == "linux") {
        #    required package: doMC
        registerDoMC(n_cores)  #change the 2 to your number of CPU cores 
        
        cl <- parallel::makeCluster(n_cores, outfile= "")
        ##END LINUX WAY
      }
      
      if (os == "windows") {
        #      memory.limit(size=56000)
        cl <- parallel::makeCluster(n_cores, outfile= "")
      }
      registerDoParallel(cl)
    }
    
    print(  paste0("V. WFO: Number of species to check: ", nrow(sp.no_hit)))
    message(paste0("V. WFO: Number of species to check: ", nrow(sp.no_hit)))
    
    # https://stackoverflow.com/questions/17350867/split-data-set-and-pass-the-subsets-in-parallel-to-function-then-recombine-the-r
    cuts <- cut(1:nrow(sp.no_hit), n_cores)
    
    if (bParallel == TRUE) {
      sys.time<-system.time({ 
        
        sp.hit <- foreach(x=levels(cuts), .packages = "WorldFlora", .combine=rbind, .multicombine=TRUE) %dopar% { 
          WFO.match(sp.no_hit[cuts==x ,]$scientific_name, WFO.file=WFO_file)
        }
      })  
      stopCluster(cl); print("Cluster stopped.")
      # insert serial backend, otherwise error in repetitive tasks, https://github.com/tobigithub/R-parallel/wiki/R-parallel-Errors
      registerDoSEQ() 
      
      print(sys.time)
      
    } else {
      sp.hit <- try( WFO.match(spec.data=sp.no_hit,spec.name="scientific_name", WFO.file=WFO_file, counter=10), silent = TRUE) 
      # Note:  Fuzzy.min = TRUE, is the default setting, see e.g. https://www.biorxiv.org/content/10.1101/2020.02.02.930719v1.full.pdf
    }
    
    if (exists("sp.hit")) {
      if (!is.null(nrow(sp.hit))) {
        sp.hit$row_name <- as.numeric(row.names(sp.hit))
        # remove new rows in returned dataframe
        sp.hit          <- subset(sp.hit,(row_name - trunc(row_name) == 0 ))
        
        sp.hit <- sp.hit %>%
          dplyr::select(scientific_name=spec.name, Authors=scientificNameAuthorship, Status=taxonomicStatus,
                        Accepted_Taxon=scientificName, PL_Comparison=Old.name, Alternative=Old.name, Matched, Fuzzy) %>% 
          dplyr::mutate(Authors = ifelse( str_count(scientific_name, " ") > 0 & str_count(Accepted_Taxon, " ") == 0, "", Authors  ),
                        Status         = ifelse( str_count(scientific_name, " ") > 0 & str_count(Accepted_Taxon, " ") == 0, "", Status        ),
                        Matched        = ifelse( str_count(scientific_name, " ") > 0 & str_count(Accepted_Taxon, " ") == 0, "", Matched       ),
                        Accepted_Taxon = ifelse( str_count(scientific_name, " ") > 0 & str_count(Accepted_Taxon, " ") == 0, "", Accepted_Taxon),
                        Score          = ifelse(Matched=="TRUE" & Fuzzy=="TRUE", "matched (fuzzy)", ifelse(Matched=="TRUE", "matched", ""    )),
                        Veri_source    = ifelse(Matched=="TRUE", "WFO", ""),
                        Status         = tolower(Status),
                        Alternative    = ifelse(Alternative==scientific_name, "", Alternative),
                        PL_Comparison  = "") %>%
          dplyr::select(-Matched, -Fuzzy)
        
        sp.accepted <- rbind(sp.accepted, sp.hit)
        
        sp.no_hit <- sp.hit %>%
          dplyr::filter(Veri_source=="")
        
        n_left    <- nrow(sp.no_hit)
        rm(sp.hit)
      } # exists("sp.hit")
    }
  }
  
  
  
   
    ## -- 2. Solve genus with Kew -------------------------------------------
    
    if (length(genus_notsolved) > 0 ) {
      
      time1 <- Sys.time()
      message("...Running Kew on unsolved genus.")
      
      
      #slices    <- c(0:trunc(length(genus_notsolved) / .slices_threshold) * .slices_threshold, length(genus_notsolved))
      genus_pow <- map_dfr(.x = seq_along(genus_notsolved), .f = function(x, input = genus_notsolved){
        
        if (x == 1)                  message("genus ", x, " out of ", length(input), ".")
        if (round(x / 10) * 10 == x) message("genus ", x, " out of ", length(input), ".")
        if (x == length(input))      message("genus ", x, " out of ", length(input), ".")
        
        pow <- taxize::pow_search(input[x], limit = 1)
        
        out <- pow$data
        
        if (!is.null(out)) {
          message(paste0("Hit for genus ", x, ": ", input[x])) 
          out$input <- input[x]
        }
        
        return(out)
        
      }) ## END map_dfr()
      
      genus_pow2 <- genus_pow %>% 
        as_tibble() %>% 
        filter(accepted == TRUE) %>%
        mutate(
          genus_pow = case_when(
            rank == "Species" ~ word(name),
            rank == "Genus"   ~ name, 
            TRUE ~ NA_character_
          )
        ) %>%
        select(input, genus_pow)
      
      genus_solved2 <- genus_solved %>% 
        left_join(genus_pow2, by = c("genus_input" = "input"))
      
      genus_notsolved <- genus_solved %>% 
        filter(is.na(genus_tropicos)) %>%
        pull(genus_input)
      
      time2 <- Sys.time()
      dt <- round(as.numeric(time2-time1, units = "secs"))
      message(paste0("...Genus solved with Tropicos", " - ", dt, " sec."))
      message(paste0("......Nb genus remaining unsolved: ", length(genus_notsolved)))
      
    } 
    
      
  
  ## Solve genus
  
  
  
  
  
  time_end <- Sys.time()
  
  
}



