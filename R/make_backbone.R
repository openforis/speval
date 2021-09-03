
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
  
  ## Common abbrevaiton for intraspecies
  check_intrasp <- c("subsp.", "ssp.", "var.", "subvar.", "f.", "subf.", "forma")
  
  ## Check missing Output.Taxon in Input.Taxon
  ## Need to remove unresolved and external status then check for incomplete genus name and missing author name. 
  ## Keep only taxa with author name to be conservative.
  lcvp_out <- LCVP::tab_lcvp %>% filter(Status != "unresolved", Status != "external") %>% pull(Output.Taxon) %>% unique()
  lcvp_in  <- LCVP::tab_lcvp %>% filter(Status != "unresolved", Status != "external") %>% pull(Input.Taxon) %>% unique()
  
  lcvp_add <- tibble(sp_add = lcvp_out[is.na(match(lcvp_out, lcvp_in))]) %>%
    mutate(
      ## Split names based on space
      split_input  = sp_add %>% str_split(" ", n = 5),
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
  data_lcvp1 <- LCVP::tab_lcvp %>% 
    as_tibble() %>%
    bind_rows(lcvp_add) %>%
    arrange(Input.Taxon) %>%
    as_tibble() %>%
    mutate(
      ## Make unique id
      id_num   = 1:nrow(.),
      id_order = trunc(log10(id_num)),
      id_num2  = str_pad(id_num, max(id_order) + 1, pad = "0", ),
      taxonID  = paste0("lcvp-", id_num2),
      taxonomicStatus = if_else(Status == "unresolved", "Unchecked", str_to_title(Status)),
      
      ## Split names based on space
      split_input  = Input.Taxon %>% str_split(" ", n = 5),
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
    mutate(acceptedNameUsageID = if_else(taxonomicStatus == "Accepted", "", acceptedNameUsageID)) %>%
    select(taxonID, scientificName, scientificNameAuthorship, acceptedNameUsageID, taxonomicStatus)
  
  ## Make the WFO backbone
  data_lcvp3 <- WorldFlora::new.backbone(
    data_lcvp2, 
    taxonID = "taxonID", 
    scientificName = "scientificName", 
    scientificNameAuthorship = "scientificNameAuthorship", 
    acceptedNameUsageID =  "acceptedNameUsageID",
    taxonomicStatus = "taxonomicStatus"
  )
  
  data.table::fwrite(data_lcvp3, file = wfo_backbone_lcvp, sep = "\t")
  
  ## !!! Remove tmp objects
  rm(check_intrasp, lcvp_in, lcvp_out, lcvp_add, data_lcvp1, data_lcvp2, data_lcvp3, data_lcvp_acc)
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
  
  ncbi_tab <- taxa_tbl("ncbi", version = "2020") %>% 
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
  
  ## Check status
  taxa_tbl("gbif", version = "2020") %>% pull(taxonomicStatus) %>% unique()
  taxa_tbl("gbif", version = "2020") %>% filter(kingdom == "Plantae") %>% pull(phylum) %>% unique()
  taxa_tbl("gbif", version = "2020") %>% filter(scientificName == "Acacia mangium")
  
  ## Make backbone
  gbif_tab <- taxa_tbl("gbif", version = "2020") %>% 
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



## --- 4. WFO backbone from UICN --------------------------------------------

## UICN is more restricted than other services. We might need to guide users to create an account and download manualy:
## See cached data from taxadb: https://github.com/boettiger-lab/taxadb-cache/blob/master/R/iucn.R
## Directions:
## - Go to: https://www.iucnredlist.org/
## - Create an account/login
## - Go to advanced search
## - Filter by:
##    + Type: species
##    + Taxonomy: filter  Plantae > Tracheophyta
##    + include Species, Subspecies and varieties, Subpopulations
## - Download search summary. fill path in global.R Admin input section


# if (!(wfo_backbone_uicn %in% list.files(recursive = T))) {
# 
#   message("Creating WFO backbone dataset from UICN with taxadb::td_create()...")
#   time1 <- Sys.time()
# 
#   ## Download a local copy of the data if necessary
#   taxadb::td_create("uicn", version = "2020", overwrite = F)
# 
#   ## Check status
#   taxa_tbl("uicn", version = "2020") %>% pull(taxonomicStatus) %>% unique()
# 
#   ## Make backbone
#   gbif_tab <- taxa_tbl("gbif", version = "2020") %>%
#     filter(kingdom == "Plantae") %>%
#     as_tibble() %>%
#     arrange() %>%
#     WorldFlora::new.backbone(
#       taxonID = "taxonID",
#       scientificName = "scientificName",
#       scientificNameAuthorship = "scientificNameAuthorship",
#       acceptedNameUsageID =  "acceptedNameUsageID",
#       taxonomicStatus = "taxonomicStatus"
#     )
# 
#   data.table::fwrite(gbif_tab, file = wfo_backbone_gbif, sep = "\t")
# 
#   ## !!! Remove tmp objects
#   rm(gbif_tab)
#   ## !!!
# 
#   time2 <- Sys.time()
#   dt <- round(as.numeric(time2-time1, units = "secs"))
#   message(paste0("...Done", " - ", dt, " sec."))
# 
# } ## End if wfo_backbone_uicn


