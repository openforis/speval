


## TEST
path_bb <- "inst/data/backbones"

# Read lines from NCBI data

ncbi_url <- "https://ftp.ncbi.nih.gov/pub/taxonomy/taxdump_archive/"

ncbi_version <-  readLines(ncbi_url) |> stringr::str_extract('new\\_taxdump\\_2024\\-.[0-9]\\-.[0-9]') |> max(na.rm = T)

dl_ncbi <- download.file(
  url = paste0(ncbi_url, ncbi_version, ".zip"),
  destfile = file.path(path_bb, paste0("NCBI-", ncbi_version, ".zip"))
  )

unzip(zipfile = file.path(path_bb, paste0("NCBI-", ncbi_version, ".zip")), exdir = path_bb)
