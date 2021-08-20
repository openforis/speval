

## Load all libraries #######################################################

## Shiny
library(shiny)
library(shinyjs)   # for hiding/showing objects
library(shinydashboard)
library(collapsibleTree)
library(shinycssloaders)
library(DT)
library(leaflet)
library(leaflet.extras)

## General purpose
#library(utils) ## Automatically loaded when R starts

## Data wrangling
library(rvest)
library(textclean)
library(data.table) 
#library(plyr) # aleady in tidyverse
#library(dplyr) # already in tidyverse
#library(stringr)
library(tidyverse)


## US carto
library(tigris)


## Taxonomic related
library(taxize)
library(rentrez) # for NCBI search 
library(rgbif)


# remotes::install_github("idiv-biodiversity/LCVP")
# remotes::install_github("idiv-biodiversity/lcvplants")
library(LCVP) ## Contain lcvp data 
library(lcvplants) # for The Leipzig Catalogue of Vascular Plant
# see https://idiv-biodiversity.github.io/lcvplants/articles/taxonomic_resolution_using_lcplants.html

library(WorldFlora)

## Parallel computation
library(furrr)
# library(foreach)
# library(doParallel)
# library(parallel)
