
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
## ---    .save_table: NULL or path to export the results. if .path exists (function embedded 
## ---                 in a higher level function call) it is used in the file name 
## ---    .filename  : default "". Input file name to add to saved outputs. 
solve_lcvp <- function(.taxon, .save_table = NULL, .filename = "") {
  
  # ## !!! For testing only
  # .path    <- "demo/NFMA_species_mess.csv"
  # .taxon <- .path %>% species_clean() %>%
  #   filter(!is.na(input_ready)) %>% 
  #   pull(input_ready) %>% 
  #   unique()
  # .save_table <- path_res
  # .filename < - get_filename(.path)
  # ## !!! 
  
  ## Vector of infraspecies abbreviations
  infrasp_abb <- c("subsp.", "ssp.", "var.", "subvar.", "f.", "subf.", "forma")
  
  ## Check function inputs
  stopifnot("LCVP" %in% installed.packages())
  stopifnot("lcvplants" %in% installed.packages())
  if (!is.null(.save_table)) stopifnot(dir.exists(.save_table))
  
  ## Remove genus alone if in the data
  input <- setdiff(.taxon, word(.taxon)) %>% unique() %>% sort()
  
  ## --- RUN LCVP ---
  message("...Running Leipzig Catalogue of Vascular Plants.")
  time1 <- Sys.time()
  
  solved_lcvp <- lcvplants::LCVP(input, max.distance = 2, genus_search = T, synonyms = F)
  
  time2 <- Sys.time()
  dt    <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...Taxons solved with LCVP", " - ", dt, " sec."))
  ## --- END RUN LCVP ---
  
  # table(solved_lcvp$Status, useNA = "always")

  
  ## --- Harmonize ---
  solved_out <- tibble(name = .taxon) %>%
    left_join(solved_lcvp, by = c("name" = "Submitted_Name")) %>%
    mutate(
      LCVP_Accepted_Taxon = if_else(Status == "unresolved", NA_character_, LCVP_Accepted_Taxon),
      
      ## Calculate harmonized indicators
      fuzzy_dist    = Insertion + Deletion + Substitution,
      fuzzy         = if_else(fuzzy_dist > 0, TRUE, FALSE),
      # fuzzy_res     = if_else(
      #   Infrasp == "species", 
      #   paste(Genus, Species, sep = " "),
      #   paste(Genus, Species, Infrasp, Infraspecies, sep = " ")
      #   ),
      status        = if_else(Status == "" | is.na(Status), "noref", Status),
      accepted_id   = NA_character_,
      refdata_id    = "lcvp",
      refdata       = "Leipzig Catalogue of Vascular Plants",
      matching_algo = "lcvplants::LCVP()",
      
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
    select(name, fuzzy, fuzzy_dist, status, accepted_id, accepted_name, accepted_author, refdata_id, refdata, matching_algo)
  ## ---
  
  ## output object to .GlobalEnv but just to be safe, also write csv back to demo file
  if (!is.null(.save_table)) {
    write_csv(solved_lcvp , paste0(.save_table, "/", .filename, "-", format(Sys.time(), format = "%Y-%m-%d-%H%M"), "-resLCVP.csv"))
    write_csv(solved_out  , paste0(.save_table, "/", .filename, "-", format(Sys.time(), format = "%Y-%m-%d-%H%M"), "-resLCVP-harmo.csv"))
    write_tsv(tibble(NULL), paste0(.save_table, "/", .filename, "-", format(Sys.time(), format = "%Y-%m-%d-%H%M"), "-resLCVP-", dt,"-secs.txt"))
  }
  
  ## Output
  out <- list(tab = solved_out, dt = dt)
    
}





