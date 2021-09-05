
## Function to run a Taxonomic Name Resolution algorithm and output result harmonized with:
## 1. The whole input list of species is returned
## 2. Matches are returned with species and authors separated
## 3. Taxonomic status in lower case
## 4. Score or fuzzy matching indicator



## --- Tropicos -------------------------------------------------------------
## --- Online
## --- Data source: https://www.tropicos.org/home
## --- Algorithm: taxize::gnr_resolve() which submit taxa to https://resolver.globalnames.org/
## --- Includes args to submit batches if number of species > 300: if (length(sci) > 300 && http == "get") http <- "post"
## --- Batches of 500 taxa: nms <- split(sci, ceiling(seq_along(sci)/500))
## --- Recommends to make small group of less than 300 to avoid function internal for loop
## --- gnr_resolve() recommended parameters: with_canonical_ranks = TRUE
## ---
## --- http://services.tropicos.org/help?method=SearchNameXml
## --- See taxize manual at https://cran.r-project.org/web/packages/taxize/taxize.pdf
## --- and also this guidance: http://viktoriawagner.weebly.com/blog/cleaning-species-names-with-r-ii-taxize
## --- https://github.com/cran/taxize
## ---
## --- function parameters: 
## ---    .taxon     : vector of taxonomic names with or without authors. Genus are not evaluated if submitted alone. 
## ---                 Preferably output of species_clean().
## ---    .gnr_src   : Numeric, the ID of Tropicos from the list of gnrs datasources. Can be obtain from taxize::gnr_datasources().
## ---    .save_table: NULL or path to export the results. 
## ---    .filename  : default "". Input file name to add to saved outputs. 


solve_tropicos <- function(.taxon, .gnr_src, .save_table = NULL, .filename = ""){
  
  ## Check function inputs
  stopifnot(is.character(.taxon))
  stopifnot(is.integer(.gnr_src))
  
  ## Remove genus alone from the data
  #input <- setdiff(.taxon, word(.taxon)) %>% unique() %>% sort()
  input <- .taxon
  
  ## --- RUN LCVP ---
  message("...Running Tropicos - Missouri Botanical Garden.")
  time1 <- Sys.time()
  
  # solved_tropicos <- taxize::gnr_resolve(input, data_source_ids = .gnr_src)
  
  ## Create function to avoid loading the whole environment to the workers
  crt_tropicos <- carrier::crate(
    input    = input,
    .gnr_src = .gnr_src,
    function(.x){
      message("Processing sequence: ", min(.x), " to ", max(.x), ".")
      tropicos_input = input[.x]
      taxize::gnr_resolve(tropicos_input, data_source_ids = .gnr_src, with_canonical_ranks = TRUE)
    })
  
  ## Make chunks
  input_chunks <-furrr:::make_chunks(n_x = length(input), chunk_size = 250)
  
  ## Run crated function 
  future::plan(sequential)
  solved_tropicos <- furrr::future_map_dfr(.x = input_chunks, .f = crt_tropicos, .options = furrr::furrr_options(globals = FALSE))
  #future::plan(sequential)
  
  time2 <- Sys.time()
  dt    <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...Taxons solved with Tropicos", " - ", dt, " sec."))
  ## --- END RUN LCVP ---
  
  ## --- Harmonize ---
  solved_out <- tibble(name = .taxon) %>%
    left_join(solved_tropicos, by = c("name" = "user_supplied_name")) %>%
    rowwise() %>%
    mutate(fuzzy_dist = if_else(is.na(matched_name2), 0, as.numeric(utils::adist(name, matched_name2, ignore.case = T)))) %>%
    ungroup() %>%
    mutate(
      fuzzy         = if_else(fuzzy_dist > 0, TRUE, FALSE),
      #fuzzy_res     = NA,
      status        = case_when(
        is.na(matched_name2) ~ "noref",
        matched_name2 == name & score >= 0.5    ~ "accepted",
        matched_name2 == name & score <  0.5    ~ "unresolved",
        matched_name2 != name & fuzzy_dist < 5  ~ "accepted",
        matched_name2 != name & fuzzy_dist >= 5 ~ "synonym",
        TRUE ~ NA_character_
        ),
      score         = as.character(score),
      accepted_id   = NA_character_,
      refdata_id    = "tropicos",
      refdata       = "Tropicos - Missouri Botanical Garden",
      matching_algo = "taxize::gnr_resolve()",
      algo_reduced    = matching_algo %>% str_remove(".*::") %>% str_remove("\\(") %>% str_remove("\\)"),
      process         = paste0(refdata_id, "_", algo_reduced),
      
      ## Accepted name
      accepted_name = matched_name2,
      accepted_author = NA_character_
    ) %>%
    select(name, fuzzy, fuzzy_dist, status, score, accepted_id, accepted_name, accepted_author, process, refdata_id, refdata, matching_algo) %>%
    distinct()
  ## ---
  
  ## output object to .GlobalEnv but just to be safe, also write csv back to demo file
  if (!is.null(.save_table)) {
    write_csv(solved_tropicos, paste0(.save_table, "/", .filename, "-", format(Sys.time(), format = "%Y-%m-%d-%H%M"), "-resTropicos.csv"))
    write_csv(solved_out     , paste0(.save_table, "/", .filename, "-", format(Sys.time(), format = "%Y-%m-%d-%H%M"), "-resTropicos-harmo.csv"))
    write_tsv(tibble(NULL)   , paste0(.save_table, "/", .filename, "-", format(Sys.time(), format = "%Y-%m-%d-%H%M"), "-resTropicos-", dt,"-secs.txt"))
  }
  
  ## Output
  list(tab = solved_out, dt = tibble(process = unique(solved_out$process), duration_sec = dt))
  
}











