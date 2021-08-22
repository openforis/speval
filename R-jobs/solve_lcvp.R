
## Get input from specified environment and returns solved_lcvp
## set in rstudioapi::jobRunScript(), for.GlobalEnv: importEnv = T, exportEnv = "R_GlobalEnv"

library(tidyverse)

## Test pipes working
test <- tibble(input = input) %>% pull(input)

# input <- species_clean("data/NFMA_species_mess.csv") %>%
#   filter(!is.na(input_ready)) %>%
#   pull(input_ready) %>%
#   unique()



## --- RUN LCVP ---
message("...Running Leipzig Catalogue of Vascular Plants.")

time1 <- Sys.time()
solved_lcvp <- lcvplants::LCVP(input, max.distance = 2, synonyms = F)
time2 <- Sys.time()
dt    <- round(as.numeric(time2-time1, units = "secs"))
message(paste0("...Taxons solved with LCVP", " - ", dt, " sec."))
## --- END RUN LCVP ---

## output object to .GlobalEnv but just to be safe, also write csv back to demo file
write_csv(solved_lcvp, "demo/NFMA_job_lcvp_dist2.csv")
write_tsv(tibble(NULL), paste0("demo/NFMA_job_lcvp_dist2-", dt,"-secs.txt"))
