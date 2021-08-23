

## Shiny has another default file in addition ui.R and server.R: global.R
## Recommend to put libraries, functions, paths and other setups here.
## UI elements can be stored in separate R scripts and sourced from here (with local=T)


## Libs #####################################################################

#source("R/libs.R", local = T)
## Shiny
library(shiny)
library(shinyjs)   # for hiding/showing objects
library(shinydashboard)
library(collapsibleTree)
library(shinycssloaders)
library(DT)
library(leaflet)
library(leaflet.extras)

## TNRS
library(taxize)
library(rentrez) # for NCBI search 
library(rgbif)
# remotes::install_github("idiv-biodiversity/LCVP")
# remotes::install_github("idiv-biodiversity/lcvplants")
library(LCVP) ## Contain lcvp data 
library(lcvplants)
library(WorldFlora)

## Data analysis
library(data.table) ## For WorldFlora.
library(furrr)
#> requires: future, parallel
library(carrier) ## Crate functions and their dependencies tables to avoid loading the whole .GlobalEnv in parallel computation workers
library(tidyverse)

## General purpose
#library(utils) ## Automatically loaded when R starts



## Functions ################################################################

# ## SUPERSEEDED: multicore setup replaced with future::plan(multiprocess), compatible with all OS 
# # get operating system info
# # https://www.r-bloggers.com/identifying-the-os-from-r/
# get_os <- function(){
#   sysinf <- Sys.info()
#   if (!is.null(sysinf)){
#     os <- sysinf['sysname']
#     if (os == 'Darwin')
#       os <- "osx"
#   } else { ## mystery machine
#     os <- .Platform$OS.type
#     if (grepl("^darwin", R.version$os))
#       os <- "osx"
#     if (grepl("linux-gnu", R.version$os))
#       os <- "linux"
#   }
#   tolower(os)
# }


# Create MODE function (https://www.tutorialspoint.com/r/r_mean_median_mode.htm)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# get filename from path
get_filename <- function(.path){
  filename <- .path %>% str_remove("\\..*") %>% str_remove(".*/")
}


## Source functions
source("R/species_clean.R", local = T)
source("R/solve_lcvp.R", local = T)
source("R/solve_wfo.R", local = T)
#source("R/species_solve", local = T)


## Paths ####################################################################

path_data <- "data"
dir.create(path_data, showWarnings = F)

path_res <- "results"
dir.create(path_res, showWarnings = F)



## Download and prepare datasets ############################################

## Download and prepare clean species lists from offline Taxonomic name resolution services
## No data is kept in the .GlobalEnv to avoid delays when running Rstudio jobs or multicore operations


## --- 1. Global Tree Seach ------------------------------------------------- 
## Get Global Tree Search File from internet if not already downloaded
gts_file <- "global_tree_search_trees_1_5.csv"

if (!(gts_file %in% list.files("data"))) {
  
  time1 <- Sys.time()
  
  message("Downloading Global Tree Search dataset...")
  
  download.file(
    destfile = paste0(path_data, "/", gts_file), 
    url      = paste0("https://tools.bgci.org/", gts_file)
  )
  
  time2 <- Sys.time()
  dt <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...Done", " - ", dt, " sec."))
  
} ## End if gts_file


## --- 2. World Flora Online ------------------------------------------------
## Get World Flora Online backbone dataset
wfo_file  <- "classification.txt"

if (!(wfo_file %in% list.files(path_data))) {
  
  message("Downloading World Flora Online backbone dataset...")
  
  time1 <- Sys.time()
  
  utils::download.file(
    url      = "http://104.198.143.165/files/WFO_Backbone/_WFOCompleteBackbone/WFO_Backbone.zip", 
    destfile = paste0(path_data, "/WFO_Backbone.zip")
  )
  
  utils::unzip(
    zipfile = paste0(path_data, "/WFO_Backbone.zip"),
    files   = wfo_file, 
    exdir   = path_data
    )
  
  unlink(paste0(path_data, "/WFO_Backbone.zip"))
  
  time2 <- Sys.time()
  dt <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...Done", " - ", dt, " sec."))
  
} ## End if wfo_class


## --- 3. Make a WFO backbone from LCVP -------------------------------------
wfo_backbone_lcvp <- "LCVP_conv.txt"

if (!(wfo_backbone_lcvp %in% list.files(path_data))) {
  
  message("Creating WFO backbone dataset from LCVP::tab_lcvp...")
  
  time1 <- Sys.time()
  
  check_intrasp <- c("subsp.", "ssp.", "var.", "subvar.", "f.", "subf.", "forma")
  
  ## Create dataset compatible with WFO backbone. Requires:
  ## 1. unique ID
  ## 2. scientific name separated from author
  ## 3. replace accepted name for synonyms with ID
  
  ## Address 1. and 2.
  data_lcvp1 <- LCVP::tab_lcvp %>%
    as_tibble() %>%
    mutate(
      ## Make unique id
      id_num   = 1:nrow(.),
      id_order = trunc(log10(id_num)),
      id_num2  = str_pad(id_num, max(id_order) + 1, pad = "0", ),
      taxonID  = paste0("lcvp-", id_num2),
      
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
    select(taxonID, scientificName, scientificNameAuthorship, taxonomicStatus = Status, family = Family, Input.Taxon, Output.Taxon)
  
  ## Create a subset with accepted names only for 3.
  data_lcvp_acc <- data_lcvp1 %>% 
    filter(taxonomicStatus == "accepted") %>% 
    select(name_acc = Output.Taxon, acceptedNameUsageID = taxonID)
  
  ## Join the accepted name ID with the table
  data_lcvp2 <- data_lcvp1 %>%
    left_join(data_lcvp_acc, by = c("Output.Taxon" = "name_acc")) %>%
    mutate(acceptedNameUsageID = if_else(taxonomicStatus == "accepted", "", acceptedNameUsageID)) %>%
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
  
  data.table::fwrite(data_lcvp3, file = paste0(path_data, "/", wfo_backbone_lcvp), sep = "\t")
  
  ## !!! Remove tmp objects
  rm(check_intrasp, data_lcvp1, data_lcvp2, data_lcvp3, data_lcvp_acc)
  ## !!!
  
  time2 <- Sys.time()
  dt <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...Done", " - ", dt, " sec."))
  
} ## Enf if wfo_backbone_lcvp


## Setup ####################################################################

## Get TROPICOS service ID
src_tropicos <- taxize::gnr_datasources() %>% 
  filter(title == "Tropicos - Missouri Botanical Garden") %>%
  pull(id)




# ## SUPERSEEDED: multicore setup replaced with future::plan(multiprocess), compatible with all OS  
# ## Add parallel computing library for Linux systems
# # os = "linux"
# os = get_os()
# if (os == "linux") library('doMC')



# ## Load data ################################################################
# 
# ## Global Tree Search
# global_tree_search <- read_csv(paste0(path_data, "/", gts_file), show_col_types = F) %>% select(TaxonName, Author)
# 
# 
# ## World Flora Online 
# ## Load WFO classification with readr::read_tsv directly from zip file and much faster than fread
# ## However classification.txt contains errors, requires data.table::fread()
# # wfo_data <- read_tsv(
# #   file = unz(description = paste0(wfo_path, "/", wfo_file), filename = wfo_classification), 
# #   col_types = cols(.default = col_character()), 
# #   )
# # data_wfo <- data.table::fread(paste0(wfo_path, "/", wfo_class), encoding="UTF-8")
# # 
# # head(data_wfo)
# 
# 
# ## Create backbone for WorldFlora::WFO.match() 
# ## based on LCVP::tab_lcvp from Leipzig Catalogue of Vascular Plants
# head(LCVP::tab_lcvp)





