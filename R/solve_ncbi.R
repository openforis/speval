
## Function to run a Taxonomic Name Resolution algorithm and output result harmonized with:
## 1. The whole input list of species is returned
## 2. Matches are returned with species and authors separated
## 3. Taxonomic status in lower case
## 4. Score or fuzzy matching indicator



## --- National Center for Biotechnology Information ------------------------
## --- Online (made offline through taxizedb)

## --> TBCompleted <--

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

#Sys.setenv("CONTENTID_REGISTRIES"  = "https://hash-archive.thelio.carlboettiger.info")
#Sys.unsetenv("CONTENTID_REGISTRIES")
td_create("ncbi", version = "2020")
# td_create("gbif")
td_create("iucn", version = "2019")

taxa_tbl("ncbi", version = "2019") %>% filter(!is.na(taxonID)) %>% pull(taxonomicStatus) %>% unique()
taxa_tbl("ncbi", version = "2019") %>% filter(taxonomicStatus== "authority")
taxa_tbl("ncbi", version = "2019") %>% filter(taxonomicStatus %in% c("accpeted", "synonym", "authority"))

filter_name("Bellucia costaricensis", provider = "ncbi", version = "2019") 
filter_name("Buchnera aphidicola", provider  = "ncbi", version = "2020")
filter_name("Acacia mangium", provider  = "ncbi", version = "2020")
filter_name("Acacia mangium", provider  = "gbif", version = "2020")
