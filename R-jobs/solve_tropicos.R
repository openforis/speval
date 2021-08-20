
## Get input from specified environment and returns solved_tropicos
## set in rstudioapi::jobRunScript(), for.GlobalEnv: importEnv = T, exportEnv = "R_GlobalEnv"

library(tidyverse)
library(furrr)

## Test pipes working
test <- tibble(input = input) %>% pull(input)

## Not needed, taken from .GlobalEnv
# input <- species_clean("data/NFMA_species_mess.csv") %>%
#   filter(!is.na(input_ready)) %>%
#   pull(input_ready) %>%
#   unique()
# slices          <- c(0:trunc(length(input) / 100) * 100, length(input))
# slices



## --- RUN TROPICOS ---
message("...Running Tropicos.")
time1 <- Sys.time()

## Getting tropicos id for taxize::gnr_resolve() is not supplied
if(is.null(.src_tropicos)) {
  .src_tropicos <- taxize::gnr_datasources() %>% 
    filter(title == "Tropicos - Missouri Botanical Garden") %>% 
    pull(id)
}

## !!! SLICING THE TABLE IS SLOWER BUT RESULTS IN MORE MATCHES
# genus_tropicos <- taxize::gnr_resolve(sci = input_genus, data_source_ids = .src_tropicos, with_canonical_ranks = T)

## Run Tropicos
# ## map_dfr() should have increased performance over for loops and output directly a data frame
# #input           <- species_notsolved
# input           <- species_init
# slices          <- c(0:trunc(length(input) / 100) * 100, length(input))
# solved_tropicos <- purrr::map_dfr(.x = seq_along(slices[-length(slices)]), .f = function(x){
#   
#   message(paste0("Sequence: ", slices[x]+1, " to ", slices[x+1], "\n"))
#   tmp_list <- input[slices[x]+1:slices[x+1]]
#   taxize::gnr_resolve(sci = tmp_list, data_source_ids = src_tropicos, with_canonical_ranks = T)
#   
# }) ## End map_dfr()

## Run with furrr - multicore version of purrr::map(), themselves tidyverse equivalent of apply()  
plan(multisession)

solved_tropicos <- furrr::future_map_dfr(.x = seq_along(slices[-length(slices)]), .f = function(x){
  
  message(paste0("Sequence: ", slices[x]+1, " to ", slices[x+1], "\n"))
  tmp_list <- input[slices[x]+1:slices[x+1]]
  taxize::gnr_resolve(sci = tmp_list, data_source_ids = .src_tropicos, with_canonical_ranks = T)
  Sys.sleep(0.5)
  
}) ## End map_dfr()

plan(sequential)

time2 <- Sys.time()
dt    <- round(as.numeric(time2-time1, units = "secs"))
message(paste0("...Genus solved with Tropicos", " - ", dt, " sec."))
## --- END RUN TROPICOS ---



## output object to .GlobalEnv but just to be safe, also write csv back to demo file
write_csv(solved_tropicos, "demo/NFMA_job_tropicos.csv")


