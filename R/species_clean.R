


## Function to clean most common typos and entry errors on tree species taxonomic names
## Input file path to a species table with scientific_name and optionally code
## Output a data frame containing the input names uncleaned and cleaned:
## - genus
## - epithet
## - intraspecies abbreviation
## - intraspecies name
## - input_ready the cleaned species names.


species_clean <- function(.path){
  
  ## !!! For testing only 
  # .path <- "data/NFMA_species_mess100.csv"
  ##
  
  message("Initiating species name cleaning...")
  
  
  
  ## Validation #############################################################
  time1 <- Sys.time()
  
  stopifnot(is.character(.path))
  stopifnot(str_ends(.path, "csv"))
  
  ## !!! Read file 
  sp_init <- read_csv(.path, show_col_types = F)
  ## !!!

  stopifnot("scientific_name" %in% names(sp_init))
  
  nb_sp    <- sp_init %>% select(scientific_name) %>% distinct()  
  nb_genus <- nb_sp %>% mutate(genus = word(scientific_name)) %>% select(genus) %>% distinct()
  
  ## Messages and timing
  time2 <- Sys.time()
  dt <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...file successfully loaded", " - ", dt, " sec."))
  message("......Number of rows: "        , nrow(sp_init))
  message("......Number of unique names: ", nrow(nb_sp))
  message("......Number of unique genus: ", nrow(nb_genus))
  message("")
  
  
  ## Cleaning ###############################################################
  time1 <- Sys.time()
  
  ## Usual abbreviations for missing species and intraspecies type
  check_sp <- c("sp", "spp", "ssp")
  
  check_intrasp <- list(
    subsp   = c("subsp", "ssp", "spp", "sub"),
    var     = c("varietas", "variety", "var", "v"),
    subvar  = c("subvarietas", "subvariety", "subvar", "subv"),
    form    = c("form", "fo", "f"),
    subform = c("subforma", "subform", "subf", "subfo")
  )
  
  check_unknown <- c("unknown", "not known", "no listada", "zz", "others")
  
  lcvp_smallgenus <- LCVP::tab_lcvp %>% 
    pull(Input.Taxon) %>% 
    str_split(" ", n = 2, simplify=T) %>% 
    as_tibble() %>%
    select(genus = V1) %>%
    distinct() %>%
    mutate(genus_count = str_count(genus)) %>%
    filter(genus_count < 4) %>%
    pull(genus)
  
  ## Cleaning sequence
  sp_df <- sp_init %>%
    distinct() %>%
    mutate(
      input_cor = scientific_name %>% 
        
        ## Remove multiple spaces, leading and trailing spaces, coma, numbers
        ## https://stackoverflow.com/questions/25707647/merge-multiple-spaces-to-single-space-re$move-trailing-leading-spaces
        str_replace_all("\\s+", " ") %>% 
        str_trim() %>%
        str_replace_all(","    , "") %>%
        str_replace_all("[:digit:]", "") %>%
        
        ## Insert space when missing after a ".". ex. Blahblah blah var.blahblah
        ## https://stackoverflow.com/questions/64492572/regex-in-r-to-add-space-after-period-if-not-present
        str_replace_all("\\.(?=[A-Za-z])", ". ") %>%
        
        ## Remove dual names after / or \n separators
        str_replace("\\\n.*", "") %>%
        str_replace(" /.*", "") %>%
        str_replace("/.*", "") %>%
        
        ## Replace unknown species names coded using special characters or variations around unknown with NA
        ## Special characters are considered < "a" in R
        if_else(. < "a", NA_character_, .) %>%
        if_else(str_to_lower(.) %in% check_unknown, NA_character_, .),
      
      
      ## split in 5 to have genus / species / potential subsp. var. / potential subsp. name / left overs
      #split_input        = input_cor %>% str_to_lower() %>% str_remove("[:punct:]") %>% str_split(" ", n = 5),
      split_input        = input_cor %>% str_to_lower() %>% str_remove("\\.") %>% str_split(" ", n = 5), ## sOME OF THE PUNCTUATION IS USEFUL
      input_genus        = map_chr(split_input, 1, .default = NA_character_) %>% str_to_title(),
      input_epithet      = map_chr(split_input, 2, .default = NA_character_),
      input_intrasp      = map_chr(split_input, 3, .default = NA_character_),
      input_intrasp_name = map_chr(split_input, 4, .default = NA_character_),
      input_leftover     = map_chr(split_input, 5, .default = NA_character_),
      
      ## Remove family names from genus
      input_family = if_else(str_detect(input_genus, "aceae"), input_genus, NA_character_), 
      input_genus  = if_else(str_detect(input_genus, "aceae"), NA_character_, input_genus),
      
      ## Remove genus with less then 4 letters not in LCVP list
      input_genuscount = str_count(input_genus),
      input_genus      = if_else(input_genuscount < 4 & !(input_genus %in% lcvp_smallgenus), NA_character_, input_genus),
      
      ## Clean species epithet to remove unknown species coded with sp or derivatives
      input_epithet = input_epithet %>% if_else(. %in% check_sp, NA_character_, .),
      
      ## Clear non subsp. / var. / f. from input_var and harmonize.
      input_intrasp = case_when(
        is.na(input_intrasp_name) ~ NA_character_,
        input_intrasp %in% check_intrasp$subsp   ~ "subsp.",
        input_intrasp %in% check_intrasp$var     ~ "var.",
        input_intrasp %in% check_intrasp$subvar  ~ "subvar.",
        input_intrasp %in% check_intrasp$form    ~ "f.",
        input_intrasp %in% check_intrasp$subform ~ "subf.",
        input_intrasp == "forma"                 ~ "forma",
        TRUE ~ NA_character_
      ),
      input_intrasp_name = if_else(!is.na(input_intrasp), input_intrasp_name, NA_character_),
      
      ## Make species names ready for correction
      input_ready = case_when(
        is.na(input_genus)         ~ NA_character_,
        is.na(input_epithet)       ~ paste0(input_genus),
        is.na(input_intrasp)       ~ paste0(input_genus, " ", input_epithet),
        !is.na(input_intrasp_name) ~ paste0(input_genus, " ", input_epithet, " ", input_intrasp, " ", input_intrasp_name),
        TRUE ~ NA_character_
      )
    )
  
  
  ## Output #################################################################
  
  out_sp    <- sp_df %>% filter(!is.na(input_ready)) %>% pull(input_ready) %>% unique() %>% sort() 
  out_genus <- out_sp %>% word() %>% unique() %>% sort()
  
  time2 <- Sys.time()
  dt <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...input list cleaned", " - ", dt, " sec."))
  message("......Number of unique names: ", length(out_sp))
  message("......Number of unique genus: ", length(out_genus))
  message("")
  
  return(sp_df %>% select(scientific_name, input_cor, input_ready))
  
} ## END FUNCTION

## !!! For testing only
# check <- species_clean(.path = "data/NFMA_species_mess100.csv")
# check

