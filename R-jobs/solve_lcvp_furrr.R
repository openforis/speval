
## Get input from specified environment and returns solved_lcvp
## set in rstudioapi::jobRunScript(), for.GlobalEnv: importEnv = T, exportEnv = "R_GlobalEnv"

library(tidyverse)
library(furrr)

## Test pipes working
test <- tibble(input = input) %>% pull(input)

# input <- species_clean("data/NFMA_species_mess.csv") %>%
#   filter(!is.na(input_ready)) %>%
#   pull(input_ready) %>%
#   unique()



## --- RUN LCVP with furrr::future_map_dfr() ---
message("...Running Leipzig Catalogue of Vascular Plants.")
time1 <- Sys.time()

## Run with furrr::future_map* family
plan(multisession)

solved_lcvp <- furrr::future_map_dfr(.x = seq_along(slices[-length(slices)]), .f = function(x){
  
  message(paste0("Sequence: ", slices[x]+1, " to ", slices[x+1], "\n"))
  tmp_list <- input[slices[x]+1:slices[x+1]]
  lcvplants::LCVP(tmp_list, max.distance = 2, synonyms = F, max.cores = 1)
  #Sys.sleep(0.5)
  
}) ## End map_dfr()

plan(sequential)

time2 <- Sys.time()
dt    <- round(as.numeric(time2-time1, units = "secs"))
message(paste0("...Taxons solved with LCVP", " - ", dt, " sec."))
## --- END RUN LCVP ---



## output object to .GlobalEnv but just to be safe, also write csv back to demo file
write_csv(solved_lcvp, "demo/NFMA_job_lcvp_dist2_furrr.csv")
write_tsv(tibble(NULL), paste0("demo/NFMA_job_lcvp_dist2_furrr-", dt,"-secs.txt"))
