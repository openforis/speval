

## Shiny has another default file in addition ui.R and server.R: global.R
## Recommend to put libraries, functions, paths and other setups here.
## UI elements can be stored in separate R scripts and sourced from here (with local=T)


## Libs #####################################################################

source("R/libs.R", local = T)



## Functions ################################################################

# get operating system info
# https://www.r-bloggers.com/identifying-the-os-from-r/
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}


# Create MODE function (https://www.tutorialspoint.com/r/r_mean_median_mode.htm)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


## submit taxonomic list to a resolution service and get results
## tnrs to choose from c("lcvp", "tropicos", "pow", "ncbi", "wfo", "gbif", "uicn", "gts")
## See vignette for description of the services

# ## STALLED: EASIER TO RUN ALL THROUGH THE MAIN FUNCTION
# tnrs_solve <- function(.input, .tnrs, .src_tropicos = NULL){
# 
#   ## !!! For testing only
#   # .input <- species_input
#   # .tnrs  <- "lcvp"
#   # .src_tropicos <- NULL
#   ## !!!
# 
#   stopifnot(.tnrs %in% c("lcvp", "tropicos", "pow", "ncbi", "wfo", "gbif", "uicn", "gts"))
#   stopifnot(is.character(.vec))
#   
#   message("Initiating resolution with: ", .tnrs, "...")
#   time1 <- Sys.time()
#   
#   ## --- 1. Solve with LCVP ------------------------------------------------- 
#   ## Full list at once
#   
#   if (.tnrs == "lcvp") {
#     lcvp <- lcvplants::LCVP(splist = tmp_list, max.distance = 2)
#     
#     out <- as_tibble(lcvp) %>% 
#       mutate(
#         fuzzysum = Insertion + Deletion + Substitution,
#         service  = .tnrs
#         ) %>%
#       select(
#         input      = Submitted_Name, 
#         status     = Status, 
#         lcvp_taxon = LCVP_Accepted_Taxon, 
#         service,
#         PL_Comparison, 
#         PL_Alternative, 
#         Score, 
#         fuzzysum
#       )
#   } ## End if "lcvp"    
#   
#   ## --- 2. Solve with tropicos ---------------------------------------------
#   ## By slices = 100
#   
#   if (.tnrs = "tropicos") {
#     if(is.null(.src_tropicos)) .src_tropicos <- taxize::gnr_datasources() %>% 
#         filter(title == "Tropicos - Missouri Botanical Garden") %>% 
#         pull(id)
#     
#     slices   <- c(0:trunc(length(.input) / 100) * 100, length(.input))
#     tropicos <- purrr::map_dfr(.x = seq_along(slices[-length(slices)]), .f = function(x, input = .input){
#       
#       message(paste0("Sequence: ", slices[x]+1, " to ", slices[x+1], "\n"))
#       tmp_list <- input[slices[x]+1:slices[x+1]]
#       taxize::gnr_resolve(sci = tmp_list, data_source_ids = src_tropicos)
#       
#     }) ## End map_dfr()
#     
#   } ## End if "tropics"
#   
#   
#   ## End function
#   time2 <- Sys.time()
#   dt <- round(as.numeric(time2-time1, units = "secs"))
#   message(paste0("...Resolution completed in ", dt, " sec."))
#   return(out)
#   
# }


## Paths ####################################################################

path_data <- "data"
dir.create(path_data, showWarnings = F)

path_res <- "results"
dir.create(path_res, showWarnings = F)



## Setup ####################################################################

## Add parallel computing library for Linux systems
# os = "linux"
os = get_os()
if (os == "linux") library('doMC')


## Get Global Tree Search File from internet if not already downloaded
gts_file <- "global_tree_search_trees_1_5.csv"

if (!(gts_file %in% list.files("data"))) {
  
  download.file(
    destfile = paste0(path_data, "/", gts_file), 
    url      = paste0("https://tools.bgci.org/", gts_file)
  )
  
}

## Get World Flora Online backbone dataset
wfo_classification <- "classification.txt"
wfo_file           <- "WFO_Backbone.zip"
wfo_path           <- "data/WFO"

if (!(wfo_file %in% list.files(wfo_path))) {
  
  message("Downloading World Flora Online backbone dataset...")
  
  time1 <- Sys.time()
  
  dir.create(wfo_path, showWarnings = F)
  
  utils::download.file(
    url      = "http://104.198.143.165/files/WFO_Backbone/_WFOCompleteBackbone/WFO_Backbone.zip", 
    destfile = paste0(wfo_path, "/", wfo_file)
  )
  
  # utils::unzip(
  #   zipfile = paste0(wfo_path, "/", wfo_file), 
  #   exdir = paste0(getwd(), "/", wfo_path)
  #   )
  
  time2 <- Sys.time()
  dt <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...WFO data sucessfully downloadd and extracted", " - ", dt, " sec."))
  
}

# ## Solution for WFO.downlaod() not handling relative pathing: utils::unzip(save.file, exdir = save.dir)
# dir.create("data/WFOdownload", showWarnings = F)
# 
# WFO.download <- function(
#   WFO.url="http://104.198.143.165/files/WFO_Backbone/_WFOCompleteBackbone/WFO_Backbone.zip", 
#   save.dir=getwd(), WFO.remember=TRUE, ...
# )
# {
#   
#   save.file <- paste(save.dir, "//WFO_Backbone.zip", sep="")
#   utils::download.file(WFO.url, destfile=save.file, ...)
#   utils::unzip(save.file)
#   
#   if (WFO.remember == TRUE) {
#     WFO.file1 <- paste(save.dir, "//classification.txt", sep="")
#     WFO.remember(WFO.file=WFO.file1)    
#   }    
#   
# }


## Get TROPICOS service ID
src_tropicos <- taxize::gnr_datasources() %>% 
  filter(title == "Tropicos - Missouri Botanical Garden") %>%
  pull(id)



## Load data ################################################################

## Global Tree Search
global_tree_search <- read_csv(paste0(path_data, "/", gts_file), show_col_types = F) %>% select(TaxonName, Author)


## World Flora Online 
## Load WFO classification with readr::read_tsv directly from zip file and much faster than fread
wfo_data <- read_tsv(
  file = unz(description = paste0(wfo_path, "/", wfo_file), filename = wfo_classification), 
  col_types = cols(.default = col_character()), 
  )


## Source functions #########################################################
source("R/species_clean.R", local = T)


