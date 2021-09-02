
## Download datasets if not in path desired

## --- 1. Global Tree Seach ------------------------------------------------- 
## Get Global Tree Search File from internet if not already downloaded

if (!(gts_file %in% list.files(recursive = T))) {
  
  time1 <- Sys.time()
  
  message("Downloading Global Tree Search dataset...")
  
  download.file(
    destfile = paste0(path_data, "/", gts_file), 
    url      = paste0("https://tools.bgci.org/", gts_file)
  )
  
  time2 <- Sys.time()
  dt    <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...Done", " - ", dt, " sec."))
  
} ## End if gts_file



## --- 2. World Flora Online ------------------------------------------------
## Get World Flora Online backbone dataset

if (!(wfo_file %in% list.files(recursive = T))) {
  
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
  dt    <- round(as.numeric(time2-time1, units = "secs"))
  message(paste0("...Done", " - ", dt, " sec."))
  
} ## End if wfo_class


