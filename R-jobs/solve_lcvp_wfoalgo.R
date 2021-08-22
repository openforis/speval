## Get input from specified environment and returns solved_lcvp
## set in rstudioapi::jobRunScript(), for.GlobalEnv: importEnv = T, exportEnv = "R_GlobalEnv"

library(tidyverse)
library(furrr)
library(carrier)

## Test pipes working
test <- tibble(input = input) %>% pull(input)

## --- RUN WFO ---
message("...Running WFO.match() on LCVP data.")

time1 <- Sys.time()
future::plan(multisession)
solved_lcvp_wfoalgo <- furrr::future_map_dfr(.x = input_chunks, .f = crt_lcvp, .options = furrr::furrr_options(globals = FALSE))
future::plan(sequential, .cleanup = T)
time2 <- Sys.time()
dt    <- round(as.numeric(time2-time1, units = "secs"))
message(paste0("...Taxons solved with WFO", " - ", dt, " sec."))
## ---

## output object to .GlobalEnv but just to be safe, also write csv back to demo file
write_csv(solved_lcvp_wfoalgo, "demo/NFMA_job_lcvp_wfoalgo.csv")
write_tsv(tibble(NULL), paste0("demo/NFMA_job_lcvp_wfoalgo-", dt,"-secs.txt"))

