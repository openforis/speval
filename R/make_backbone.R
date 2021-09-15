
## Convert a reference data to backbone for WFO.match()

## 1. unique ID
## 2. scientific name separated from author
## 3. replace accepted name for synonyms with ID
## 4. Retroactively: Check that all accepted names are also in inputs and if not add them
## (For LCVP, filter output accepted name not in input to be only clean with no abbreviations or missing author name)



## --- 1. WFO backbone from LCVP --------------------------------------------

if (!(wfo_backbone_lcvp %in% list.files(recursive = T))) {
  
  message("Creating WFO backbone dataset from LCVP::tab_lcvp...")
  time1 <- Sys.time()
  
  ## Common abbreviation for intraspecies
  check_intrasp <- c("subsp.", "ssp.", "var.", "subvar.", "f.", "subf.", "forma")
  
  ## Correct few mistakes in LCVP table
  lcvp_cor <- LCVP::tab_lcvp %>% 
    as_tibble() %>%
    mutate(
      Output.Taxon = case_when(
        Output.Taxon == "Dalbergia nitidula Baker"             ~ "Dalbergia nitidula Welw. ex Baker",
        Output.Taxon == "Dolichandrone spathacea (L.f.) Seem." ~ "Dolichandrone spathacea (L.f.) K.Schum.",
        Output.Taxon == "Manilkara zapota"                     ~ "Manilkara zapota (L.) P.Royen",
        Output.Taxon == "Poeppigia procera C.Presl"            ~ "Poeppigia procera (Poepp. ex Spreng.) C.Presl",
        Output.Taxon == "Aniba canellila Mez"                  ~ "Aniba canellila (Kunth) Mez",
        TRUE ~ Output.Taxon
      ),
      Status = case_when(
        Input.Taxon == "Malus sieversii (Ledeb.) M.Roem." ~ "accepted",
        TRUE ~ Status
      )
    )
  
  ## Checks
  # lcvp_cor %>% filter(str_detect(Output.Taxon, "Dalbergia nitidula"))
  # lcvp_cor %>% filter(str_detect(Input.Taxon, "Malus sieversii \\(Ledeb.\\) M.Roem."))
  # lcvp_cor %>% filter(str_detect(Output.Taxon, "Dolichandrone spathacea"))
  # lcvp_cor %>% filter(str_detect(Output.Taxon, "Manilkara zapota"))
  # tt <- lcvp_cor %>% filter(str_detect(Output.Taxon, "Poeppigia procera"))
  
  ## Check missing Output.Taxon in Input.Taxon
  ## Need to remove unresolved and external status then check for incomplete genus name and missing author name. 
  ## Keep only taxa with author name to be conservative.
  lcvp_out <- lcvp_cor %>% filter(Status != "unresolved", Status != "external") %>% pull(Output.Taxon) %>% unique()
  lcvp_in  <- lcvp_cor %>% filter(Status != "unresolved", Status != "external") %>% pull(Input.Taxon) %>% unique()
  
  lcvp_add <- tibble(sp_add = lcvp_out[is.na(match(lcvp_out, lcvp_in))]) %>%
    mutate(
      ## Split names based on space
      split_input  = sp_add %>% enc2utf8() %>% str_split(" ", n = 5),
      genus        = map_chr(split_input, 1, .default = ""),
      epithet      = map_chr(split_input, 2, .default = ""),
      intrasp      = map_chr(split_input, 3, .default = ""),
      intrasp_name = map_chr(split_input, 4, .default = ""),
      leftover     = map_chr(split_input, 5, .default = ""),
      
      ## Separate name from authors (!!! Doesn't handle sections, too rare)
      sp_name = if_else(
        intrasp %in% check_intrasp,
        paste(genus, epithet, intrasp, intrasp_name, sep = " "),
        paste(genus, epithet, sep = " ")
      ),
      sp_author = if_else(
        intrasp %in% check_intrasp,
        leftover,
        paste(intrasp, intrasp_name, leftover, sep = " ")
      ),
      Input.Taxon = sp_add, 
      Status = "accepted",
      PL.comparison = "",
      PL.alternative = "",
      Output.Taxon = sp_add,
      Family = "", 
      Order = ""
    ) %>%
    filter(sp_author != "") %>%
    select(Input.Taxon, Status, PL.comparison, PL.alternative, Output.Taxon, Family, Order)
  
  ## Address 1. and 2.
  data_lcvp1 <- lcvp_cor %>% 
    bind_rows(lcvp_add) %>%
    arrange(Input.Taxon) %>%
    mutate(
      ## Make unique id
      id_num   = 1:nrow(.),
      id_order = trunc(log10(id_num)),
      id_num2  = str_pad(id_num, max(id_order) + 1, pad = "0"),
      taxonID  = paste0("lcvp-", id_num2),
      taxonomicStatus = if_else(Status == "unresolved", "Unchecked", str_to_title(Status)),
      
      ## Split names based on space
      split_input  = Input.Taxon %>% enc2utf8() %>% str_split(" ", n = 5),
      genus        = map_chr(split_input, 1, .default = ""),
      epithet      = map_chr(split_input, 2, .default = ""),
      intrasp      = map_chr(split_input, 3, .default = ""),
      intrasp_name = map_chr(split_input, 4, .default = ""),
      leftover     = map_chr(split_input, 5, .default = ""),
      
      ## Separate name from authors (!!! Doesn't handle sections, too rare)
      scientificName = if_else(
        intrasp %in% check_intrasp,
        paste(genus, epithet, intrasp, intrasp_name, sep = " "),
        paste(genus, epithet, sep = " ")
      ),
      scientificNameAuthorship = if_else(
        intrasp %in% check_intrasp,
        leftover,
        paste(intrasp, intrasp_name, leftover, sep = " ")
      )
    ) %>%
    select(taxonID, scientificName, scientificNameAuthorship, taxonomicStatus, family = Family, Input.Taxon, Output.Taxon)
  
  ## Create a subset with accepted names only for 3.
  data_lcvp_acc <- data_lcvp1 %>% 
    filter(taxonomicStatus == "Accepted") %>% 
    select(name_acc = Output.Taxon, acceptedNameUsageID = taxonID)
  
  ## Join the accepted name ID with the table
  data_lcvp2 <- data_lcvp1 %>%
    left_join(data_lcvp_acc, by = c("Output.Taxon" = "name_acc")) %>%
    mutate(
      acceptedNameUsageID = if_else(taxonomicStatus == "Accepted", "", acceptedNameUsageID),
      scientificNameAuthorship = str_trim(scientificNameAuthorship)
      ) %>%
    select(taxonID, scientificName, scientificNameAuthorship, acceptedNameUsageID, taxonomicStatus, family)
  
  # data_lcvp1 %>% filter(scientificName == "Aalius compressa") %>% pull(scientificNameAuthorship)
  # data_lcvp2 %>% filter(scientificName == "Aalius compressa") %>% pull(scientificNameAuthorship)
  
  # if (Sys.info()[["sysname"]] == "Windows") {
  #   data_lcvp2$scientificNameAuthorship <- enc2utf8(data_lcvp2$scientificNameAuthorship)
  #   Encoding(data_lcvp2$scientificNameAuthorship) <- "unknown"
  # }
  
  ## Make the WFO backbone
  data_lcvp3 <- WorldFlora::new.backbone(
    data_lcvp2, 
    taxonID = "taxonID", 
    scientificName = "scientificName", 
    scientificNameAuthorship = "scientificNameAuthorship", 
    acceptedNameUsageID =  "acceptedNameUsageID",
    taxonomicStatus = "taxonomicStatus"
  )
  
  names(data_lcvp3)[length(names(data_lcvp3))] <- "family"
  
  data.table::fwrite(data_lcvp3, file = wfo_backbone_lcvp, sep = "\t", bom = TRUE)
  
  ## !!! Remove tmp objects
  rm(check_intrasp, lcvp_cor, lcvp_in, lcvp_out, lcvp_add, data_lcvp1, data_lcvp2, data_lcvp3, data_lcvp_acc)
  ## !!!
  
  time2 <- Sys.time()
  dt <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...Done", " - ", dt, " sec."))
  
} ## End if wfo_backbone_lcvp



## --- 2. WFO backbone from NCBI --------------------------------------------

if (!(wfo_backbone_ncbi %in% list.files(recursive = T))) {
  
  message("Creating WFO backbone dataset from NCBI with taxadb::td_create()...")
  time1 <- Sys.time()
  
  ## Download a local copy of the data if necessary
  taxadb::td_create("ncbi", version = "2020", overwrite = F)
  
  ncbi_tab <- taxadb::taxa_tbl("ncbi", version = "2020") %>% 
    filter(taxonomicStatus %in% c("accepted", "synonym"), phylum == "Streptophyta Bremer, 1985") %>% 
    as_tibble() %>% 
    arrange() %>%
    mutate(scientificNameAuthorship = NA) %>%
    WorldFlora::new.backbone(
      taxonID = "taxonID", 
      scientificName = "scientificName", 
      scientificNameAuthorship = "scientificNameAuthorship", 
      acceptedNameUsageID =  "acceptedNameUsageID",
      taxonomicStatus = "taxonomicStatus"
    )
  
  data.table::fwrite(ncbi_tab, file = wfo_backbone_ncbi, sep = "\t")
  
  ## !!! Remove tmp objects
  rm(ncbi_tab)
  ## !!!
  
  time2 <- Sys.time()
  dt <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...Done", " - ", dt, " sec."))
  
} ## End if wfo_backbone_ncbi



## --- 3. WFO backbone from GBIF --------------------------------------------

if (!(wfo_backbone_gbif %in% list.files(recursive = T))) {
  
  message("Creating WFO backbone dataset from GBIF with taxadb::td_create()...")
  time1 <- Sys.time()
  
  ## Download a local copy of the data if necessary
  taxadb::td_create("gbif", version = "2020", overwrite = F)
  
  ## Check backbone reference errors 
  # taxa_tbl("gbif", version = "2020") %>% filter(scientificName == "Mimosa platycarpa")
  # taxa_tbl("gbif", version = "2020") %>% filter(taxonID == "GBIF:7628219")
  # taxa_tbl("gbif", version = "2020") %>% filter(taxonID == "GBIF:2969333")
  # taxa_tbl("gbif", version = "2020") %>% filter(scientificName == "Pausinystalia yohimba")
  # taxa_tbl("gbif", version = "2020") %>% filter(taxonID == "GBIF:3848388")
  # taxa_tbl("gbif", version = "2020") %>% filter(scientificName == "Pausinystalia yohimba")
  # taxa_tbl("gbif", version = "2020") %>% filter(taxonID == "GBIF:3848388")
  
  ## Make backbone
  gbif_tab <- taxadb::taxa_tbl("gbif", version = "2020") %>% 
    filter(phylum == "Tracheophyta") %>% 
    as_tibble() %>% 
    arrange() %>%
    select(-parentNameUsageID, -originalNameUsageID) %>%
    WorldFlora::new.backbone(
      taxonID = "taxonID", 
      scientificName = "scientificName", 
      scientificNameAuthorship = "scientificNameAuthorship", 
      acceptedNameUsageID =  "acceptedNameUsageID",
      taxonomicStatus = "taxonomicStatus"
    )
  
  data.table::fwrite(gbif_tab, file = wfo_backbone_gbif, sep = "\t")
  
  ## !!! Remove tmp objects
  rm(gbif_tab)
  ## !!!
  
  time2 <- Sys.time()
  dt <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...Done", " - ", dt, " sec."))
  
} ## End if wfo_backbone_gbif



## --- 4. IUCN check list ---------------------------------------------------

## IUCN is more restricted than other services. We might need to guide users to create an account and download manually:
## See cached data from taxadb: https://github.com/boettiger-lab/taxadb-cache/blob/master/R/iucn.R
## Directions:
## - Go to: https://www.iucnredlist.org/
## - Create an account/login
## - Go to advanced search
## - Filter by:
##    + Type: species
##    + Taxonomy: filter  Plantae > Tracheophyta
##    + include Species, Subspecies and varieties, Subpopulations
## - Download search summary. 
## - Find zip when file.choose() called


if (!(iucn_checklist %in% list.files(recursive = T))) {

  message("Creating IUCN checklist from manual download...")
  time1 <- Sys.time()

  ## Path to IUCN Manual download
  iucn_download <- file.choose() ## "C:\\Users\\Admin\\Downloads\\redlist_species_data_e73c2f06-3056-4897-9fb3-824f5b326757.zip"
  
  ## Unzip files to tempdir()
  unzip(iucn_download, exdir = file.path(tempdir(), "iucn"), files = c("taxonomy.csv", "synonyms.csv", "simple_summary.csv"))
  
  list.files(file.path(tempdir(), "iucn"))
  
  ## Read files
  iucn        <- list.files(file.path(tempdir(), "iucn"), full.names = TRUE) %>% map(read_csv)
  names(iucn) <- list.files(file.path(tempdir(), "iucn")) %>% str_remove(".csv")
  
  ## Checks 
  table(iucn$synonyms$infraType)
  table(iucn$simple_summary$redlistCategory)
  
  ## Red list codes: ND added by Lauri, no data (!= not evaluated)
  iucn_codes <- tibble(
    iucn_num   = 0:9,
    iucn_code  = c("ND", "NE", "DD", "LC", "NT", "VU", "EN", "CR", "EW", "EX"), 
    iucn_label = c("Not in Red List", "Not Evaluated", "Data Deficient", "Least Concern", 
                   "Near Threatened", "Vulnerable",  "Endangered", "Critically Endangered", 
                   "Extinct in the Wild", "Extinct")
    )
  
  iucn_conv <- tibble(
    iucn_label = c("Lower Risk/conservation dependent", "Lower Risk/least concern", "Lower Risk/near threatened"),
    iucn_code = c("NT", "LC", "NT")
  )
  
  ## Simplify summary
  iucn_redlist <- iucn$simple_summary %>%
    mutate(accepted_id = paste0("IUCN:", internalTaxonId)) %>%
    select(accepted_id, iucn_label = redlistCategory) %>%
    distinct()
  
  ## !!! Using synonyms aborted: many synonyms have more than one accepted name, 
  ##     possibly due to the data being constructed from accepted name back to synonyms 
  ##     instead of synonyms to accepted.  
  # iucn_syn <- iucn$synonyms %>%
  #   mutate(
  #     id_num          = 1:nrow(.),
  #     id_order        = trunc(log10(id_num)),
  #     id_num2         = str_pad(id_num, max(id_order) + 1, pad = "0"),
  #     id              = paste0("IUCN:", id_num2, "_syn),
  #     accepted_id     = paste0("IUCN:", internalTaxonId),
  #     name_cor        = name            %>% str_remove(" \\[orth\\. error\\]| \\{orth\\. error\\]"),
  #     author_cor      = speciesAuthor   %>% str_remove(" \\[orth\\. error\\]| \\{orth\\. error\\]"),
  #     infauthor_cor   = infrarankAuthor %>% str_remove(" \\[orth\\. error\\]| \\{orth\\. error\\]"),
  #     author_regex    = author_cor    %>% str_replace_all("\\.", "\\\\.") %>% str_replace_all("\\(", "\\\\(") %>% str_replace_all("\\)", "\\\\)"),
  #     infauthor_regex = infauthor_cor %>% str_replace_all("\\.", "\\\\.") %>% str_replace_all("\\(", "\\\\(") %>% str_replace_all("\\)", "\\\\)"),
  #     sc_name         = if_else(
  #       is.na(infauthor_cor), 
  #       name_cor %>% str_remove(paste0(" ", author_regex)),
  #       name_cor %>% str_remove(paste0(" ", author_regex)) %>% str_remove(paste0(" ", infauthor_regex))
  #       ),
  #     author          = if_else(is.na(infauthor_cor), author_cor, infauthor_cor),
  #     status          = "synonym"
  #     ) %>%
  #   select(id, sc_name, author, status, accepted_id) %>%
  #   distinct()
  
  ## Add IUCN red list category and synonyms to taxonomy
  iucn_taxo <- iucn$taxonomy %>%
    mutate(
      id          = paste0("IUCN:", internalTaxonId),
      accepted_id = id, 
      status      = "accepted"
      ) %>%
    select(id, sc_name = scientificName, author = authority, status, accepted_id) %>%
    #bind_rows(iucn_syn) %>% ## Aborted
    left_join(iucn_redlist, by = "accepted_id") %>%
    left_join(bind_rows(iucn_codes %>% select(-iucn_num), iucn_conv), by = "iucn_label") %>%
    left_join(iucn_codes, by = "iucn_code", suffix = c("", "_cor")) %>% ## Update Lower Risk to post 2001 categories
    select(-iucn_label, iucn_label = iucn_label_cor) %>%
    distinct() %>%
    arrange(sc_name)
  
  ## Check for duplicates
  num_taxon <- iucn_taxo %>%
    mutate(name = paste(sc_name, author, sep = "---")) %>%
    group_by(name) %>% summarise(count_taxon = n())

  table(num_taxon$count_taxon)
  
  ## Write file
  write_csv(iucn_taxo, file = iucn_checklist)
  
  ## !!! Remove tmp objects
  rm(iucn_download, iucn, iucn_codes, iucn_conv, iucn_redlist, iucn_taxo)
  unlink(file.path(tempdir(), "iucn"), recursive = T)
  ## !!!

  time2 <- Sys.time()
  dt <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...Done", " - ", dt, " sec."))

} ## End if wfo_backbone_uicn


