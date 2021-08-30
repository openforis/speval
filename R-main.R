


## Main script to use the taxonomic name resolution services outside of the shiny app




## Source global to load packages, functions and download data if necessary
source("global.R")

## tests
# lcvplants::LCVP("Ziziphus", synonyms = F, genus_search = T, max.distance = 2, infra_specific = T)
# WorldFlora::WFO.match("Ziziphus", WFO.file = wfo_file, Fuzzy = 2)
# WorldFlora::WFO.match("Zizyphus", WFO.file = wfo_file, verbose = F, Fuzzy = 2)
# WorldFlora::WFO.match("Zizyphus", WFO.file = wfo_file, Fuzzy.max = 300)
# WorldFlora::WFO.match(c("Zizyphus", "Ziziphus", "Accacia", "Acacia"), WFO.file = wfo_file, First.dist = TRUE, Fuzzy.one = T, Fuzzy.max = Inf)
# WorldFlora::WFO.match(c("Zizyphus", "Ziziphus", "Accacia", "Acacia"), WFO.file = wfo_backbone_lcvp, First.dist = TRUE, Fuzzy.one = T, Fuzzy.max = Inf)

# 
# df <- tibble(sp_name = "Zizyphus", genus = "Zizyphus", sp = "")
# WorldFlora::WFO.match(df, spec.name = "sp_name", Genus = "genus", Species = "sp", WFO.file = wfo_file, Fuzzy = 2,  First.dist = TRUE, Fuzzy.one = T)
# 
# df2 <- tibble(sp_name = "Zizyphus cinnamomum", genus = "Zizyphus", sp = "cinnamomum")
# WorldFlora::WFO.match(df2, spec.name = "sp_name", Genus = "genus", Species = "sp", WFO.file = wfo_file, verbose = F, Fuzzy = 2)

# LCVP::tab_lcvp %>% as_tibble() %>% filter(str_detect(Input.Taxon, "Acer syriacum"))
# LCVP::tab_lcvp %>% as_tibble() %>% filter(str_detect(Input.Taxon, "Acer obtusifolium"))
# 
# LCVP::tab_lcvp %>% as_tibble() %>% filter(Output.Taxon == "")
# 
# lcvp_tab <- LCVP::tab_lcvp
# lcvp_out <- lcvp_tab %>% filter(Status != "unresolved", Status != "external") %>% pull(Output.Taxon) %>% unique() ## 406331 accepted names
# lcvp_in  <- lcvp_tab %>% filter(Status != "unresolved", Status != "external") %>% pull(Input.Taxon) %>% unique() ## 1315503 
# 
# lcvp_check <- lcvp_out[is.na(match(lcvp_out, lcvp_in))] %>% sort()
# 
# lcvp_check[2]
# LCVP::tab_lcvp %>% as_tibble() %>% filter(str_detect(Output.Taxon, lcvp_check[2]))
# 
# LCVP::tab_lcvp %>% as_tibble() %>% filter(str_detect(Input.Taxon, lcvp_check[1]))
# 
# tt <- LCVP::tab_lcvp %>% as_tibble() %>% filter(Output.Taxon %in% lcvp_check)
# 
# LCVP::tab_lcvp %>% as_tibble() %>% filter(str_detect(Input.Taxon, "Senegalia furcatispina"))

#LCVP::tab_lcvp %>% as_tibble() %>% filter(str_detect(Input.Taxon, "Malus communis"))



## Job setup
#iFile  <- "demo/NFMA_species_mess.csv"
iFile  <- "demo/NFMA_species_clean100.csv"
how_to <- "compare"

## --- Run species identification as job ---
dir.create("tmp", showWarnings = F)
## Make script
job_script <- paste0(
  "source('global.R', local = T)\n",
  "res_species <- species_solve(
  .path       = '", iFile, "', 
  .how_to     = '", how_to, "', 
  .save_table = path_res, 
  .multicore  = T, 
  .ref_lcvp   = wfo_backbone_lcvp, 
  .ref_wfo    = wfo_file,
  .tx_src     = src_tropicos
  )"
)

## Save script
write_file(job_script, file = "tmp/job_species_solve.R")
## Run script
rstudioapi::jobRunScript("tmp/job_species_solve.R", name = "species_solve", workingDir = getwd(), exportEnv = "R_GlobalEnv")
## ---



