
## Function to run a Taxonomic Name Resolution algorithm and output result harmonized with:
## 1. The whole input list of species is returned
## 2. Matches are returned with species and authors separated
## 3. Taxonomic status in lower case
## 4. Score or fuzzy matching indicator

## --- Plant of the World ---------------------------------------------------
## --- Online
## --- Data source: https://wcsp.science.kew.org/
## --- Algorithm: taxize::pow_search()
## --- Send names 1 by 1
## --- pow_search() recommended parameters: limit = 1
## ---
## --- http://www.plantsoftheworldonline.org/
## --- https://github.com/cran/taxize
## ---
## --- function parameters: 
## ---    .taxon     : vector of taxonomic names with or without authors. Genus are not evaluated if submitted alone. 
## ---                 Preferably output of species_clean().
## ---    .save_table: NULL or path to export the results. 
## ---    .filename  : default "". Input file name to add to saved outputs. 


solve_pow <- function(.taxon, .save_table = NULL, .filename = "", .n_cores = 1) {
  
  ## Check function inputs
  stopifnot(is.character(.taxon))
  
  ## Remove genus alone from the data
  #input <- setdiff(.taxon, word(.taxon)) %>% unique() %>% sort()
  input <- .taxon
  
  # tt <- out_tab %>% filter(name %in% notsolved1)
  
  ## --- RUN POW ---
  message(paste0("...Sending to http://www.plantsoftheworldonline.org"))
  time1 <- Sys.time()
  
  # solved_pow <- vector(mode = "list", length = length(.taxon))
  # for (i in seq_along(solved_pow)) {
  #   Sys.sleep(2)
  #   x = .taxon[i]
  #   out <- taxize::pow_search(x)$data
  #   out$input <- x
  #   solved_pow[[i]] <- out
  # }

  solved_pow <- map_dfr(input, function(x){
    Sys.sleep(2)
    out <- taxize::pow_search(x, limit = 1)$data
    if(!is.null(out)) out$input <- x
    out
  })
  
  time2 <- Sys.time()
  dt    <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...Taxons solved with POW - ", dt, " sec."))
  ## --- END RUN POW ---
  
  # table(solved_wfo$taxonomicStatus, useNA = "always")
  # table(solved_wfo$Fuzzy, useNA = "always")
  
  ## Solve data.frame inside data.frame
  if ("synonymOf" %in% names(solved_pow)) {
    
    solved_tmp  <- solved_pow$synonymOf %>%
      rename_with(~ paste0("synonymOf.", .x)) %>% 
      add_column(input = solved_pow$input, .before = "synonymOf.accepted")
    solved_pow2 <- solved_pow %>% 
      as_tibble() %>% 
      select(-synonymOf) %>% 
      left_join(solved_tmp, by = "input")
    solved_pow <- solved_pow2
    rm(solved_tmp, solved_pow2)
  }
  
  if ("images" %in% names(solved_pow)) solved_pow <- solved_pow %>% select(-images)
  
  ## --- Harmonize ---
  solved_out <- tibble(sc_name = .taxon) %>%
    left_join(solved_pow, by = c("sc_name" = "input")) %>%
    rowwise() %>%
    mutate(fuzzy_dist = as.numeric(utils::adist(sc_name, name, ignore.case = T))) %>%
    ungroup() %>%
    mutate(
      
      ## Calculate harmonized indicators
      fuzzy         = if_else(fuzzy_dist > 0, TRUE, FALSE),
      #fuzzy_res    = NA,
      status        = case_when(
        accepted                       ~ "accepted",
        !accepted & synonymOf.accepted ~ "synonym",
        #is.na(accepted)                ~ "noref",
        TRUE ~ "not found"
      ),
      score         = if_else(is.na(accepted), "no hit", "matched"),
      refdata_id    = "pow",
      refdata       = "Plants of the World Online",
      matching_algo = "taxize::pow_search()",
      algo_reduced  = matching_algo %>% str_remove(".*::") %>% str_remove("\\(") %>% str_remove("\\)"),
      process       = paste0(refdata_id, "_", algo_reduced),
      
      ## Accepted names
      accepted_id     = if_else(status == "synonym", synonymOf.url   , url   ) %>% str_remove("/taxon/urn:lsid:ipni.org:names:"),
      accepted_name   = case_when(
        status == "accepted"  ~ name,
        status == "synonym"   ~ synonymOf.name, 
        status == "not found" ~ sc_name,
        TRUE ~ NA_character_
        ),
      accepted_author = if_else(status == "synonym", synonymOf.author, author),
    ) %>% 
    select(sc_name, fuzzy, fuzzy_dist, status, score, accepted_id, accepted_name, accepted_author, process, refdata_id, refdata, matching_algo) %>%
    distinct()
  
  ## !!! NOT NEEDED FOR POW_SEARCH() WITH LIMIT = 1
  # ## If submitted name return several answers, keep only the accepted if exists or the synonym if exists.
  # count_names <- solved_tmp %>%
  #   group_by(name) %>%
  #   summarise(count = n()) 
  # 
  # has_accepted <- solved_tmp %>%
  #   filter(status == "accepted") %>%
  #   mutate(has_accepted = TRUE) %>%
  #   select(name, has_accepted)
  # 
  # has_synonym <- solved_tmp %>%
  #   filter(status == "synonym") %>%
  #   mutate(has_synonym = TRUE) %>%
  #   select(name, has_synonym)
  # 
  # solved_out <- solved_tmp %>%
  #   left_join(count_names , by = "name") %>%
  #   left_join(has_accepted, by = "name") %>%
  #   left_join(has_synonym , by = "name") %>%
  #   mutate(
  #     has_accepted = replace_na(has_accepted, FALSE),
  #     has_synonym  = replace_na(has_synonym , FALSE)
  #   ) %>%
  #   filter(
  #     !(count > 1 & has_accepted == TRUE & status != "accepted"),
  #     !(count > 1 & has_accepted == FALSE & has_synonym == TRUE & status != "synonym")
  #   ) %>%
  #   select(name, fuzzy, fuzzy_dist, status, accepted_id, accepted_name, accepted_author, process, refdata_id, refdata, matching_algo) %>%
  #   distinct()
  # ## ---
  
  ## output object to .GlobalEnv but just to be safe, also write csv back to demo file
  if (!is.null(.save_table)) {
    write_csv(solved_pow  , paste0(.save_table, "/", .filename, "-", format(Sys.time(), format = "%Y-%m-%d-%H%M%S"), "-resPOW.csv"))
    write_csv(solved_out  , paste0(.save_table, "/", .filename, "-", format(Sys.time(), format = "%Y-%m-%d-%H%M%S"), "-resPOW-harmo.csv"))
    write_tsv(tibble(NULL), paste0(.save_table, "/", .filename, "-", format(Sys.time(), format = "%Y-%m-%d-%H%M%S"), "-resPOW-", dt,"-secs.txt"))
  }
  
  ## Output
  list(tab = solved_out, dt = tibble(process = unique(solved_out$process), duration_sec = dt))
  
}





