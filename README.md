# speval: Plant Species Name Validation Tool

This app aims to help field inventory teams and other plants observers and 
enthusiasts to validate and keep your species list updated. 

The app takes a CSV file as input, checks for common species lists harmonization issues then compare plant species names against renowned taxonomic backbones. 

The outputs are: (1) a comparison of the different backbones for each name submitted and (2) a series of statistics to help users correct outdated names and typos but also decide what to do in case of conflict between different backbones.


## How to install

This apps is intended to run locally as the validation process can take from a few seconds to a few hours for long lists (national level surveys in tropical countries for example).

It requires R and Rstudio installation (see: https://posit.co/download/rstudio-desktop/)

Windows users may need to also install Rtools after R and Rstudio: https://cran.r-project.org/bin/windows/Rtools/rtools44/rtools.html 

Once R and Rstudio are install, from the R console run:

## Installation

    remotes::install_github("openforis/speval")

## Launch the shiny app

    speval::shiny_run_speval()


