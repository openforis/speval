

## Shiny has another default file in addition ui.R and server.R: global.R
## Recommend to put libraries, functions, paths and other setups here.
## UI elements can be stored in separate R scripts and sourced from here (with local=T)


## Libs #####################################################################

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
library(taxadb)
library(rentrez) # for NCBI search 
library(rgbif)
# remotes::install_github("idiv-biodiversity/LCVP")
# remotes::install_github("idiv-biodiversity/lcvplants")
library(LCVP) ## Contain lcvp data 
library(lcvplants)
library(WorldFlora)

## Data analysis
library(data.table) ## For WorldFlora.
library(furrr) #> requires: future, parallel
library(carrier) ## Crate functions and their dependencies tables to avoid loading the whole .GlobalEnv in parallel computation workers
library(tidyverse)

## General purpose
#library(utils) ## Automatically loaded when R starts



## Admin input (not accessible for shiny users) #############################

## Path to data and results
path_data <- "data"
path_res  <- "results"

## Path to Global Tree Search reference data
gts_file <- file.path(path_data, "global_tree_search_trees_1_5.csv")

## Path to World Flora Online reference data 
wfo_file <- file.path(path_data, "classification.txt")

## Path to backbone from LCVP
wfo_backbone_lcvp <- file.path(path_data, "LCVP_conv.txt")

## Path to backbone from GTS
## !!! To Be Done: GTS used to check in accepted names from other sources are included, not to correct submitted species lists.

## Path to backbone from NCBI 2020 (2021 not working)
wfo_backbone_ncbi <- file.path(path_data, "NCBI_conv.txt")

## Path to backbone from NCBI 2020 (2021 not working)
wfo_backbone_gbif <- file.path(path_data, "GBIF_conv.txt")



## Create paths plus directories to data and results ########################

dir.create(path_data, showWarnings = F)
dir.create(path_res, showWarnings = F)



## Source functions #########################################################

source("R/other_functions.R", local = T)

source("R/species_clean.R", local = T)

source("R/solve_lcvp.R", local = T)

source("R/solve_wfo.R", local = T)

source("R/solve_tropicos.R", local = T)

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
src_tropicos <- taxize::gnr_datasources() %>% 
  filter(title == "Tropicos - Missouri Botanical Garden") %>%
  pull(id)

# ## SUPERSEEDED: multicore setup replaced with future::plan(multiprocess), compatible with all OS  
# ## Add parallel computing library for Linux systems
# # os = "linux"
# os = get_os()
# if (os == "linux") library('doMC')






