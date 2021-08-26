
## Small functions necessary to run the taxonomic name resolution services

# ## SUPERSEEDED: multicore setup replaced with future::plan(multiprocess), compatible with all OS 
# # get operating system info
# # https://www.r-bloggers.com/identifying-the-os-from-r/
# get_os <- function(){
#   sysinf <- Sys.info()
#   if (!is.null(sysinf)){
#     os <- sysinf['sysname']
#     if (os == 'Darwin')
#       os <- "osx"
#   } else { ## mystery machine
#     os <- .Platform$OS.type
#     if (grepl("^darwin", R.version$os))
#       os <- "osx"
#     if (grepl("linux-gnu", R.version$os))
#       os <- "linux"
#   }
#   tolower(os)
# }

# Create MODE function (https://www.tutorialspoint.com/r/r_mean_median_mode.htm)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# get filename from path
get_filename <- function(.path){
  filename <- .path %>% str_remove("\\..*") %>% str_remove(".*/")
}
