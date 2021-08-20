
## Get input from specified environment and returns solved_lcvp
## set in rstudioapi::jobRunScript(), for.GlobalEnv: importEnv = T, exportEnv = "R_GlobalEnv"

library(tidyverse)

# species_init <- species_clean("data/NFMA_species_mess.csv") %>%
#   filter(!is.na(input_ready)) %>%
#   pull(input_ready) %>%
#   unique()

## Test pipes working
test <- tibble(input = input) %>% pull(input)


time1 <- Sys.time()
solve_lcvp <- lcvplants::LCVP(input, max.distance = 2, synonyms = F)
time2 <- Sys.time()

dt <- round(as.numeric(time2-time1, units = "secs"))
message(paste0("...Taxons solved with LCVP", " - ", dt, " sec."))

write_csv(solve_lcvp, "results/job_lcvp_dist2.csv")

