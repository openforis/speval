
## Function to run a Taxonomic Name Resolution algorithm and output result harmonized with:
## 1. The whole input list of species is returned
## 2. Matches are returned with species and authors separated
## 3. Taxonomic status in lower case
## 4. Score or fuzzy matching indicator

## --- LCVP -----------------------------------------------------------------
## --- Offline
## --- Data source: LCVP::tab_lcvp
## --- Algorithm: lcvplants::LCVP()
## --- Performs better with full list than slices. Multi-cores integrated. 
## --- LCVP() recommended parameters: max.distance = 2, genus_search = T, synonym = F
## ---
## --- https://idiv-biodiversity.github.io/lcvplants/articles/taxonomic_resolution_using_lcplants.html#running-lcvplants
## ---
## --- function parameters: 
## ---    .data      : Vector of taxonomic names with or without authors. Genus are not evaluated if submitted alone. 
## ---                 Preferably output of species_clean().
## ---    .save_table: NULL or path to export the results.
## ---    .filename  : default "". Input file name to add to saved outputs. 
## ---    .n_cores   : the number of cores to be used for parallel computing.

solve_lcvp <- function(.taxon, .save_table = NULL, .filename = "", .multicore = TRUE, .n_cores = 1) {
  
  ## Vector of infraspecies abbreviations
  infrasp_abb <- c("subsp.", "ssp.", "var.", "subvar.", "f.", "subf.", "forma")
  
  ## Check function inputs
  stopifnot(is.character(.taxon))
  
  ## Remove genus alone if in the data
  input <- setdiff(.taxon, word(.taxon)) %>% unique() %>% sort()
  
  ## --- RUN LCVP ---
  message("...Running Leipzig Catalogue of Vascular Plants.")
  time1 <- Sys.time()
  
  if (.multicore) {
    
    ## lcvplant 1.1.1
    # solved_lcvp <- lcvplants::LCVP(input, max.distance = 2, genus_search = T, synonyms = F, max.cores = .n_cores)
    
    ## lcvplant 2.0 NEED TO ADD MULTICORE
    solved_lcvp <- lcvplants::lcvp_search(input, max_distance = 2, genus_fuzzy = T, show_correct = T, grammar_check = T)
    #solved_lcvp <- lcvplants::lcvp_fuzzy_search(input, max_distance = 2, genus_fuzzy = T)
    
  } else {
    
    ## lcvplant 1.1.1
    #solved_lcvp <- lcvplants::LCVP(input, max.distance = 2, genus_search = T, synonyms = F, max.cores = 1)
    
    ## lcvplant2.0
    solved_lcvp <- lcvplants::lcvp_search(input, max_distance = 2, genus_fuzzy = T, show_correct = T, grammar_check = T)
    #solved_lcvp <- lcvplants::lcvp_fuzzy_search(input, max_distance = 2, genus_fuzzy = T)
  }
  
  time2 <- Sys.time()
  dt    <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...Taxa solved with LCVP", " - ", dt, " sec."))
  message("")
  ## --- END RUN LCVP ---
  
  # table(solved_lcvp$Status, useNA = "always")

  
  ## --- Harmonize ---
  solved_out <- tibble(sc_name = .taxon) %>%
    #left_join(solved_lcvp, by = c("sc_name" = "Submitted_Name")) %>% #changed v2.0
    left_join(solved_lcvp, by = c("sc_name" = "Search")) %>%
    mutate(
      #LCVP_Accepted_Taxon = if_else(Status == "unresolved", NA_character_, LCVP_Accepted_Taxon), #changed v2.0
      LCVP_Accepted_Taxon = if_else(Status == "unresolved", NA_character_, Output.Taxon),
      
      ## Calculate harmonized indicators
      #fuzzy_dist    = Insertion + Deletion + Substitution, #changed v2.0
      fuzzy_dist    = NA_real_,
      #fuzzy         = if_else(fuzzy_dist > 0, TRUE, FALSE), #changed v2.0
      fuzzy         = Correct,
      status        = if_else(Status == "" | is.na(Status), "noref", Status),
      #score         = Score,
      score         = NA_character_,
      accepted_id   = NA_character_,
      refdata_id    = "lcvp",
      refdata       = "Leipzig Catalogue of Vascular Plants",
      matching_algo = "lcvplants::LCVP()",
      algo_reduced  = matching_algo %>% str_remove(".*::") %>% str_remove("\\(") %>% str_remove("\\)"),
      process       = paste0(refdata_id, "_", algo_reduced),
      
      ## Split names based on space
      split_input  = LCVP_Accepted_Taxon %>% str_split(" ", n = 5),
      genus        = map_chr(split_input, 1, .default = ""),
      epithet      = map_chr(split_input, 2, .default = ""),
      infrasp      = map_chr(split_input, 3, .default = ""),
      infrasp_name = map_chr(split_input, 4, .default = ""),
      leftover     = map_chr(split_input, 5, .default = ""),
      
      ## Separate name from authors (!!! Doesn't handle sections, too rare)
      accepted_name = if_else(
        infrasp %in% infrasp_abb,
        paste(genus, epithet, infrasp, infrasp_name, sep = " "),
        paste(genus, epithet, sep = " ")
      ),
      accepted_author = if_else(
        infrasp %in% infrasp_abb,
        leftover,
        paste(infrasp, infrasp_name, leftover, sep = " ")
      )
    ) %>% 
    select(sc_name, fuzzy, fuzzy_dist, status, score, accepted_id, accepted_name, accepted_author, process, refdata_id, refdata, matching_algo) %>%
    distinct()
  ## ---
  
  ## output object to .GlobalEnv but just to be safe, also write csv back to demo file
  if (!is.null(.save_table)) {
    write_csv(solved_lcvp , paste0(.save_table, "/", .filename, "-", format(Sys.time(), format = "%Y-%m-%d-%H%M%S"), "-resLCVP.csv"))
    write_csv(solved_out  , paste0(.save_table, "/", .filename, "-", format(Sys.time(), format = "%Y-%m-%d-%H%M%S"), "-resLCVP-harmo.csv"))
    write_tsv(tibble(NULL), paste0(.save_table, "/", .filename, "-", format(Sys.time(), format = "%Y-%m-%d-%H%M%S"), "-resLCVP-", dt,"-secs.txt"))
  }
  
  ## Output
  out <- list(tab = solved_out, dt = tibble(process = unique(solved_out$process), duration_sec = dt))
    
}





