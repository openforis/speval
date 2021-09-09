

## Shiny has another default file in addition ui.R and server.R: global.R
## Recommend to put libraries, functions, paths and other setups here.
## UI elements can be stored in separate R scripts and sourced from here (with local=T)


## Libs #####################################################################

## Shiny
library(shiny)
library(shinyjs)   # for hiding/showing objects
library(shinydashboard)
library(shinyWidgets)
# library(collapsibleTree)
# library(shinycssloaders)
# library(DT)
# library(leaflet)
# library(leaflet.extras)

## TNRS
library(taxize) ## For tropicos
library(taxadb) ## NCBI and GBIF

# remotes::install_github("idiv-biodiversity/LCVP")
# remotes::install_github("idiv-biodiversity/lcvplants")
library(LCVP) ## Contain lcvp data 
library(lcvplants)
library(WorldFlora)

## Data analysis
library(data.table) ## For WorldFlora
library(furrr) #> requires: future, parallel
library(carrier) ## Crate functions and their dependencies tables to avoid loading the whole .GlobalEnv in parallel computation workers
library(tidyverse)

## General purpose
#library(utils) ## Automatically loaded when R starts

## Options for dplyr::summarise()
options(dplyr.summarise.inform = FALSE)



## Admin input (not accessible for shiny users) #############################

## Path to data and results
path_data <- "data"
path_res  <- "results"

## Path to World Flora Online reference data 
wfo_file <- file.path(path_data, "classification.txt")

## Path to backbone from LCVP
wfo_backbone_lcvp <- file.path(path_data, "LCVP_conv.txt")

## Path to backbone from NCBI 2020 (2021 not working)
wfo_backbone_ncbi <- file.path(path_data, "NCBI_conv.txt")

## Path to backbone from GBIF 2020 (2021 not working)
wfo_backbone_gbif <- file.path(path_data, "GBIF_conv.txt")

## Path to backbone from GTS
gts_file <- file.path(path_data, "global_tree_search_trees_1_5.csv")

## Path to IUCN check list
## !!! When running this script for the first time, you will need to manually download the IUCN summary status for Plantae/Tracheophyta
## For more details, see make_backbone script, section 4: IUCN checklist
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
## !!!
iucn_checklist <- file.path(path_data, "iucn_checklist.csv")

## Code for Tropicos service in https://resolver.globalnames.org/
## Can be updated with:
# src_tropicos <- taxize::gnr_datasources() %>%
#   filter(title == "Tropicos - Missouri Botanical Garden") %>%
#   pull(id)
src_tropicos <- 165



## Create paths plus directories to data and results ########################

dir.create(path_data, showWarnings = F)
dir.create(path_res, showWarnings = F)



## Source functions #########################################################

source("R/other_functions.R", local = T)

source("R/species_clean.R", local = T)

source("R/solve_lcvp.R", local = T)

source("R/solve_wfo.R", local = T)

source("R/solve_tropicos.R", local = T)

source("R/solve_pow.R", local = T)

source("R/species_solve.R", local = T)



## Download and prepare datasets ############################################

## Download data for offline Taxonomic name resolution services: 
## 'gts_file', 'wfo_file'
## No data is kept in the .GlobalEnv to avoid delays when running Rstudio jobs or multicore operations
source("R/download_data.R", local = T)

## Make the backbones for WFO.match() with: 
## 'wfo_backbone_lcvp'
source("R/make_backbone.R", local = T)



## Setup ####################################################################

## Get TROPICOS service ID
# test_gnr <- tryCatch(taxize::gnr_datasources(opts = list(timeout_ms = 500)), error = function(e) e)
# error_gnr <- any(class(test_gnr) == "error")
# 
# if (error_gnr) {
#   message("Tropicos service NOT available.") 
# } else {
#   message("Tropicos service available.") 
# }

# test_pow <- tryCatch(taxize::pow_search("Acacia mangium"), error = function(e) e)
# error_pow <- any(class(test_pow) == "error")
# 
# if (error_pow) {
#   message("Plant of the World Online service NOT available.")
# } else {
#   message("Plant of the World Online service available.")
# }

# ## SUPERSEEDED: multicore setup replaced with future::plan(multiprocess), compatible with all OS  
# ## Add parallel computing library for Linux systems
# # os = "linux"
# os = get_os()
# if (os == "linux") library('doMC')






