#************************************************************************************************************
# SPECIES NAMES VALIDATION TOOL
# (c) Lauri Vesa, Javier Garcia-Perez, Elisee Tchana, Gael Sola. FAO
#  Last edited: 17 August 2021, L.Vesa  * See editing history below
#
# The script validates a list of species names given in a CSV file (code, scientific_name).
# The repositories are as follows:
# 1) The Leipzig Catalogue of Vascular Plant [offline]
# 2) Tropicos - Missouri Botanical Garden [online]
# 3) Kew  - Plants of the World Online [online]
# 4) NCBI - National Center for Biotechnology Information, db="taxonomy" [online]
# 5) WFO  - WorldFlora Online [offline]
# 6) GBIF - Global Biodiversity Information Facility [online]
# additional information:
# 7) GBIF - IUCN Red List search [online], (no results for all species!)
# 8) GlobalTreeSearch [offline]. (Used to check names occur in this DB, not  actually to validate names)
#

# you can activate these lines and add keys here!
#Sys.setenv(TROPICOS_KEY = "add your key here")
#Sys.setenv(ENTREZ_KEY= "add your key here")


# get operating system
#os = "linux"
os = get_os()
print(  paste0("Operating system: ", os))
message(paste0("Operating system: ", os))
message("-----------------------")

repo_name             <- c("LCVP","Tropicos","Kew","NCBI","WFO","GBIF","no hits")
repo_df               <- as.data.frame(repo_name) 
repo_df$species_count <- 0  # column for reporting search statistics  

#************************************************************************************************************
# folder for input data and GTS database
# db_folder <- getwd()
# 
# setwd(db_folder)

# Reads input data from CSV file (iFile) that contains 2 columns: code, scientific_name; or just 1 column: scientific_name
# iFile <- "user_specie.csv"
# filePath <- input$file1$datapath
# fileText <- paste(readLines(filePath), collapse = "\n")
# View(fileText)
# 


## !!! For testing only
iFile    <- "data/NFMA_genus_clean100.csv"
fileName <- "NFMA_genus_clean100"
## !!!

# use Shiny version
iFile    <- input$file1$datapath
fileName <- input$file1$name

message("Start Species Validation")
message("The search takes some time, have some coffee ")
message(" - - - - - - - - %")
Sys.sleep(0.5)

# Result files in offline version:
# outFile1 <- paste0("Result_", basename(iFile))   # the main search result table
# outFile2 <- paste0("Stat_",   basename(iFile))   # the count of names send to each repository search

outFile1 <- paste0("Result_", fileName)   # the main search result table
outFile2 <- paste0("Stat1_",  fileName)   # the count of names send to each repository search
outFile3 <- paste0("Stat2_",  fileName)   # the result statistics

# View(outFile1)

# The databases needed:
# 1) The Leipzig Catalogue of Vascular Plants (LCVP)  https://idiv-biodiversity.github.io/lcvplants/ 
#
# 2) World Flora database.        http://www.worldfloraonline.org/downloadData
WFO_file <- "classification.txt"

# 3) Global Tree Search database. https://tools.bgci.org/global_tree_search.php
#! global_tree_search_file <- paste0( db_folder, "/global_tree_search_trees_1_5.csv")
#global_tree_search_file <- "/global_tree_search_trees_1_5.csv"


#db_folder <- "D:/FAO/0 OPEN FORIS Project 2018/2. Software development and licences/Species search and validation/New version 2020"
#setwd(db_folder)

# Offline version: Reads input data from CSV file (iFile) that contains 2 columns: code, scientific_name; or just 1 column: scientific_name
# filePath <- input$file1$datapath
# fileText <- paste(readLines(filePath), collapse = "\n")
# View(fileText)
# 
# iFile <- input$file1$datapath
#
#******************************************************
# As the introduction to the topic, please to read the presentation of Javier Garcia-Perez:
# https://af30a669-7da6-4a89-abe5-5d88115c600c.filesusr.com/ugd/07a4b9_0bc71281e26e44d59b762a1acf7f1110.pdf
#
# Note:
# sp.accepted - collects all results after each module (repository search). Structure becomes from WFO run.
# sp.no_hit   - carries 'non-accepted' names to next module. Contains 2 fields: code, scientific_name
#               ('code' has no role here yet, but kept because of possible future purposes)
#
# Changes:
#   6.4.2021, LV: fixed a bug in Tropicos search 
#  31.3.2021, LV: improve handling of variants, subspecies, forms etc.
#  18.3.2021, LV: GBIF -> IUCN Red List search
#  15.3.2021, LV: reactivated WFO; updated global_tree_search_trees_1_5.csv
#  18.2.2021, LV: added LCVP as the first search repository, and added 'family' search into the end of the code
#                 WFO is deactivated
#   8.9.2020, LV: change in tnrs parameters: 'query' parameter removed in taximize 0.9.97
#                 tnrs: issues with Taxosaurus database ( www.taxosaurus.org ). Is this active?
#                 Tropicos API: https://services.tropicos.org/help
#   23.9.2020, LV: requires TROPICOS API key, preferably as an environment variable
#   17.8.2021, GS, LV: added species list pre-cleaning script  
#
# The list of required keys as environment variables:
# - TROPICOS_KEY
# - ENTREZ_KEY
#
#******************************************************

n_cores <- parallel::detectCores() - 1
if (n_cores==0) n_cores=1

## !!! Not optimal, run everytime a new species list is processed. MOVED TO: global.R
# # read Global Tree Search database into 2 lists: 1) global_tree_list, 2) global_tree_genus_list
# global_tree_search       <- read.csv( global_tree_search_file, header = T, stringsAsFactors = F )
# global_tree_list         <- as.array( global_tree_search$TaxonName )
# global_tree_genus_list   <- unique( stringr::word(global_tree_list , 1))
# rm(global_tree_search)

## !!! Not optimal, run everytime a new species list is processed. Moved to: global.R
# # Create MODE function (https://www.tutorialspoint.com/r/r_mean_median_mode.htm)
# getmode <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }

preprocess_species <- function(iFile) { 
  #  Preprocess input data
  DF_sp_list_original <- read.csv( iFile, header = T, stringsAsFactors = F )
  DF_sp_list_original <- subset( DF_sp_list_original, !is.na(scientific_name) & scientific_name != "" )
  DF_sp_list          <- DF_sp_list_original
  
  # trim leading and trailing whitespaces, and multiple spaces
  # https://stackoverflow.com/questions/25707647/merge-multiple-spaces-to-single-space-re$move-trailing-leading-spaces
  DF_sp_list$scientific_name <- trimws(gsub("\\s+", " ",    DF_sp_list$scientific_name))
  # remove all numbers in species names, https://stackoverflow.com/questions/13590139/remove-numbers-from-alphanumeric-characters
  DF_sp_list$scientific_name <- gsub('[0-9]+', '', DF_sp_list$scientific_name)
  
  # The recommended abbreviations for ranks below species are (https://www.iapt-taxon.org/nomen/main.php ; https://en.wikipedia.org/wiki/Infraspecific_name )
  # - subspecies - recommended abbreviation: subsp. (but "ssp." is also in use although not recognized by Art 26)
  # - varietas (variety) - recommended abbreviation: var.
  # - subvarietas (subvariety) - recommended abbreviation: subvar.
  # - forma (form) - recommended abbreviation: f.
  # - subforma (subform) - recommended abbreviation: subf.
  
  DF_sp_list$scientific_name <- textclean::mgsub(DF_sp_list$scientific_name, c(" SUBSP "," ssp. ", " ssp "," Ssp. ", " Ssp "," SSP. ", " SSP "," spp. ", " spp "," SPP. ", " SPP "), " subsp. ") 
  DF_sp_list$scientific_name <- textclean::mgsub(DF_sp_list$scientific_name, c(" varietas "," variety "," var "," Var "," Var. "," VAR "), " var. ")
  DF_sp_list$scientific_name <- textclean::mgsub(DF_sp_list$scientific_name, c(" subvarietas "," subvariety "," subvar "," Subvar "," Subvar. "," SUBVAR "), " subvar. ")
  DF_sp_list$scientific_name <- textclean::mgsub(DF_sp_list$scientific_name, c(" fo. "," fo "," form. "," form "," f "," FORM "," Form "), " f. ")
  DF_sp_list$scientific_name <- textclean::mgsub(DF_sp_list$scientific_name, c(" subforma "," subform "," Subform "," Subf. "), " subf. ")
  
  # check cases where column 'code' not given in input data 
  if("code" %in% colnames(DF_sp_list)) {
    DF_sp_list$code <-  DF_sp_list$code 
  } else {
    DF_sp_list$code <- "no data"
  }
  DF_sp_list$code[is.na(DF_sp_list$code)] <- "no data"
  
  # set order of columns 
  DF_sp_list <- DF_sp_list[, c("code", "scientific_name")]
  
  # Gael's preprocessing script
  sp_df <- DF_sp_list %>%
    dplyr::mutate(
      input_cor = scientific_name %>% 
        
        ## Remove doubles spaces and comma
        str_replace_all("  ", " ") %>%
        str_replace_all(",", "") %>%
        
        ## Remove dual names after / or \n separators
        str_replace("\\\n.*", "") %>%
        str_replace(" /.*", "") %>%
        str_replace("/.*", "") %>%
        
        ## Replace unknown species names coded using special characters with NA
        ## Special characters are considered < "a" in R
        if_else(. < "a", NA_character_, .),
      
      ## split in 5 to have genus / species / potential subsp. var. / potential subsp. name / left overs
      split_input    = str_split(input_cor, " ", n = 5),
      input_genus    = map_chr(split_input, 1, .default = NA_character_),
      input_epithet  = map_chr(split_input, 2, .default = NA_character_),
      input_var      = map_chr(split_input, 3, .default = NA_character_),
      input_var_name = map_chr(split_input, 4, .default = NA_character_),
      input_leftover = map_chr(split_input, 5, .default = NA_character_),
      
      ## Remove family names from genus
      input_family = if_else(str_detect(input_genus, "aceae"), input_genus, NA_character_) %>% str_to_title(), 
      input_genus  = if_else(str_detect(input_genus, "aceae"), NA_character_, input_genus) %>% str_to_title(),
      
      ## Clean species epithet to remove unknown species coded with sp or derivatives
      input_epithet = input_epithet %>% 
        str_remove("[:punct:]") %>%
#        if_else(. %in% check_sp, NA_character_, .) %>%
        str_to_lower(),
      
      ## Clear non subsp. / var. / f. from input_var and harmonize.
       input_var = input_var %>%
         str_to_lower() %>%
         str_remove("[:punct:]") %>%
      #   if_else(. %in% check_var, ., NA_character_) %>% 
         str_replace("v.*", "var."  ) %>% 
         str_replace("f.*", "f."    ) %>%
         str_replace("s.*", "subsp."),
       
      input_var_name = if_else(input_var %in% c("var.", "f.", "subsp."), input_var_name, NA_character_) %>% str_to_lower(),
      
      ## Make species names ready for correction
      input_ready = case_when(
        is.na(input_genus)     ~ NA_character_,
        is.na(input_epithet)   ~ input_genus,
        is.na(input_var)       ~ paste0(input_genus, " ", input_epithet),
        !is.na(input_var_name) ~ paste0(input_genus, " ", input_epithet, " ", input_var, " ", input_var_name),
        TRUE ~ NA_character_
      )
    )
  
  DF_sp_list <- sp_df %>%
    dplyr::select(code, scientific_name = input_ready) %>%
    dplyr::filter(!is.na(scientific_name))
  
  # remove " sp" and " sp." from scientific_name. WPO search works better 
  # DF_sp_list$scientific_name <- with(DF_sp_list,
  #   ifelse( str_sub(scientific_name,-3,-1) == " sp", str_sub(scientific_name, 1, nchar(scientific_name)-3), 
  #     ifelse( str_sub(scientific_name,-4,-1) == " sp." | str_sub(scientific_name,-4,-1) == " spp", str_sub(scientific_name, 1, nchar(scientific_name)-4),
  #       ifelse( str_sub(scientific_name,-5,-1) == " spp.", str_sub(scientific_name, 1, nchar(scientific_name)-5),
  #         scientific_name))))  
  
  return(DF_sp_list)
}



DF_sp_list <- preprocess_species( iFile )

# split list into two: 1) genus only, 2) pure species names 
## DF_2_genus <- DF_sp_list %>%
##  dplyr::filter(str_count(scientific_name, " ") == 0)

## DF_2_sp <- DF_sp_list %>%
#  dplyr::filter(str_count(scientific_name, " ") > 0)



#**********************************************
# check API keys
tropicos_key = getkey(service="tropicos")
tropicos_key
#**********************************************


#****************************************************************
# 1. LCVP (lcvplants)                                        ####
# The Leipzig Catalogue of Vascular Plants
# https://idiv-biodiversity.github.io/lcvplants/articles/taxonomic_resolution_using_lcplants.html#running-lcvplants
# Note: Database downloaded first. Not running as online search. 
#****************************************************************

print(  paste0("I. LCVP: Number of species to check: ", nrow(DF_sp_list)))
message(paste0("I. LCVP: Number of species to check: ", nrow(DF_sp_list)))

repo_df$species_count[1] <- nrow(DF_sp_list)

sp.accepted <- LCVP(DF_sp_list$scientific_name, max.distance = 2, max.cores = n_cores)
# sp.accepted$fuzzysum <- sp.accepted$Insertion +  sp.accepted$Deletion + sp.accepted$Substitution

sp.accepted <- sp.accepted %>%
  select(scientific_name=Submitted_Name, Authors, Status, Accepted_Taxon=LCVP_Accepted_Taxon, 
         PL_Comparison, Alternative=PL_Alternative, Score)

# remove duplicates (check this later!)
sp.accepted <- sp.accepted[!duplicated(sp.accepted$scientific_name),] 

sp.hit <- sp.accepted %>%
  dplyr::filter(Status == "accepted" | Status == "synonym")

sp.no_hit <- sp.accepted %>%
  dplyr::filter(Status != "accepted" & Status != "synonym")

sp.accepted <- sp.hit
rm(sp.hit)

sp.accepted$Veri_source <- "LCVP"
sp.no_hit$""            <- NULL

n_left = nrow(sp.no_hit)

#******************************************************
# 2. Tropicos (taxize -- gnr_resolve)              ####
# Tropicos - Missouri Botanical Garden
# https://www.tropicos.org/home
# http://services.tropicos.org/help?method=SearchNameXml
# see taxize manual at https://cran.r-project.org/web/packages/taxize/taxize.pdf
# and also this guidance: http://viktoriawagner.weebly.com/blog/cleaning-species-names-with-r-ii-taxize
#******************************************************

# src <- c("The International Plant Names Index", "Catalogue of Life", "Tropicos - Missouri Botanical Garden")

src_tr   <- "Tropicos - Missouri Botanical Garden"
y1       <- subset( gnr_datasources(), title == src_tr )
src_tropicos = y1$id  
rm(y1)

repo_df$species_count[2] <- n_left

if ( n_left > 0) {
  
  # species list to check
  sp.list <- unique(sp.no_hit$scientific_name)
  print(  paste0("II. taxize-gnr_resolve (tropicos): Number of species to check: ", n_left ))
  message(paste0("II. taxize-gnr_resolve (tropicos): Number of species to check: ", n_left ))  
  if (n_left>100) {
    # https://stackoverflow.com/questions/7060272/split-up-a-dataframe-by-number-of-rows
    chunk = 100
    species_split <- split(sp.no_hit, rep(1:ceiling(nrow(sp.no_hit)/chunk), each=chunk, length.out=nrow(sp.no_hit))) 
  } else {
    species_split  = NA
  }
  
  # see parameters at https://cran.r-project.org/web/packages/taxize/taxize.pdf
  
  if (is.na(species_split)) {
    temp2 <- gnr_resolve(sci = sp.list, data_source_ids = src_tropicos, with_canonical_ranks = T )
    
  } else {
    for (i in 1:length(species_split)) {
      out <- gnr_resolve(sci = as.vector(species_split[[i]]$scientific_name), data_source_ids = src_tropicos, with_canonical_ranks = T )
      Sys.sleep(0.5)
      # Now put them back together into one data.frame
      if (i==1) temp2 <- out
      if (i>1 & nrow(out>0))  temp2 <- rbind(temp2, out)
      print(  paste("Tropicos big data run: ", i*chunk))
      message(paste("Tropicos big data run: ", i*chunk))
      rm(out)
    }
  } 
  
  if (!is.null(temp2)) {
    if (nrow(temp2)>0) {
      
      #check if species names dropped out (e.g. Diosporus coki is not returned!) in resulting dataframe
      sp.list_dropped <-  setdiff(sp.list, temp2$user_supplied_name) 
      
      if (length(sp.list_dropped)>0) {
        nj= nrow(temp2) + 1
        for (j in 1:length(sp.list_dropped)) {
          temp2[nj,] <- NA
          temp2$user_supplied_name[nj] <- sp.list_dropped[j]
          temp2$score[nj] <- 0
          nj = nj+1
        } 
      }
      temp2$matched_name2[is.na(temp2$matched_name2)]           <- ""
      temp2$user_supplied_name[is.na(temp2$user_supplied_name)] <- ""
      
      temp2$scientific_name    <- temp2$user_supplied_name
      temp2$user_supplied_name <- NULL
      
      sp.hit <- temp2 %>%
        dplyr::left_join(sp.no_hit, by="scientific_name") %>%
        dplyr::mutate(Accepted_Taxon = ifelse( score>0.9 & matched_name2 != "", matched_name2, scientific_name),
          Status         = ifelse( score>0.9 & matched_name2 != "", "accepted", "" ),
          Status         = ifelse(str_count(scientific_name, " ") == 0 & score==0.750 & scientific_name == matched_name2, "accepted", Status ),
          Status         = ifelse(score==0.750 & length(matched_name2) > 0, "accepted", Status ),
          Accepted_Taxon = ifelse( Status=="accepted" & length(matched_name2) > 0, matched_name2, Accepted_Taxon),
          Veri_source    = "Tropicos",
          PL_Comparison  = "",
          Score          = score,
          Score          = ifelse(str_count(scientific_name, " ") > 0 & score==0.750 & scientific_name != matched_name2 & length(matched_name2) > 0, "0.75 (fuzzy)", Score ),
          Status         = ifelse(str_count(scientific_name, " ") > 0 & str_count(matched_name2, " ") == 0, "", Status),
          Accepted_Taxon = ifelse(str_count(scientific_name, " ") > 0 & str_count(matched_name2, " ") == 0, "", Accepted_Taxon)) %>%
        dplyr::select( -submitted_name , -score, -matched_name2,-data_source_title) 
      
      
      sp.no_hit <- sp.hit %>%      
        dplyr::filter( Status != "accepted") %>%
        dplyr::mutate(Score="", Accepted_Taxon = "", Authors = "", Veri_source="")
      
      sp.hit <- sp.hit %>%      
        dplyr::filter( Status == "accepted")
      
      sp.accepted <- rbind(sp.accepted, sp.hit)
      sp.accepted[is.na(sp.accepted)] <- ""
      
      rm(sp.hit); rm(temp2)
      
      n_left  <- nrow(sp.no_hit)
    } #  if (!is.na(temp2)) 
  } #  if (!is.null(temp2))
}


#******************************************************
# 3. Kew (taxize -- pow_search)                    ####
# Plants of the World Online
# http://www.plantsoftheworldonline.org/
# http://powo.science.kew.org/terms-and-conditions
# see parameters at https://rdrr.io/cran/taxize/man/pow_search.html
#******************************************************



repo_df$species_count[3] <- n_left

if (n_left > 0) {   
  
  print(  paste0("III. taxize-pow_search: Number of species to check: ", n_left))
  message(paste0("III. taxize-pow_search: Number of species to check: ", n_left))
  
  for (i in 1: nrow(sp.no_hit)) {
    Sys.sleep(2.0)
    temp2 <- pow_search( sp.no_hit$scientific_name[i], limit=1 )
    temp2 <- temp2$data
    if (is.null(temp2)) {
      sp.no_hit$Score[i] = "0"
    } else if (temp2$accepted==TRUE) {
      sp.no_hit$Score[i] = "1"
      if (sp.no_hit$scientific_name[i] == temp2$name) {
        sp.no_hit$Accepted_Taxon[i] = sp.no_hit$scientific_name[i]
        sp.no_hit$Status[i]   = "accepted"
      }
      if (sp.no_hit$scientific_name[i] != temp2$name) {
        sp.no_hit$Accepted_Taxon[i] = temp2$name
        sp.no_hit$Status[i]   = "synonym"
      }
    } else if (any(names(temp2) == 'synonymOf')) {
      if (temp2$synonymOf$accepted == TRUE)  {
        sp.no_hit$Score[i]    = "1"
        sp.no_hit$Accepted_Taxon[i] = temp2$synonymOf$name
        sp.no_hit$Status[i]   = "synonym"
      }
    } 
  }
  
  sp_hit <- sp.no_hit %>%
    dplyr::filter(Score == "1") %>%
    dplyr::mutate(Veri_source = "Kew")
  
  if (nrow(sp_hit) > 0) {
    sp.accepted <- rbind(sp.accepted, sp_hit)
  }
  
  rm(sp_hit)
  
  sp.no_hit <- sp.no_hit %>% 
    dplyr::filter(Score=="0") %>%
    dplyr::mutate(Score="")
  
  n_left  <- nrow(sp.no_hit)
}

#******************************************************
# 4. NCBI (rentrez -- entrez_search)              ####
# National Center for Biotechnology Information
# https://www.ncbi.nlm.nih.gov/
# https://github.com/ropensci/taxize/
# https://www.ncbi.nlm.nih.gov/home/develop/https-guidance/
# https://cran.r-project.org/web/packages/rentrez/vignettes/rentrez_tutorial.html
#******************************************************

repo_df$species_count[4] <- n_left

if (n_left > 0) {   
  
  print(  paste0("IV. taxize-tnrs (NCBI): Number of species to check: ", n_left))
  message(paste0("IV. taxize-tnrs (NCBI): Number of species to check: ", n_left))
  ping_test <- ncbi_ping()
  
  if (ping_test==FALSE) {
    print(  "NCBI ping request is FALSE! NCBI is not accessible")
    message("NCBI ping request is FALSE! NCBI is not accessible")
  } else {
    # species list to check
    sp.check2 <- sp.no_hit$scientific_name
    jono      <- c()
    
    for (i in 1:n_left) {
      if (i %% 10 == 0 ) print(i)
      s_hit   <- entrez_search(db="taxonomy", term=sp.check2[i] )
      jono[i] = s_hit$count
      Sys.sleep(3)
      rm(s_hit)
    }
    
    sp.no_hit$ncbi <- jono
    rm(jono); rm(sp.check2)
    
    if (sum(sp.no_hit$ncbi) > 0) {
      sp.hit <- sp.no_hit  %>%
        dplyr::filter(ncbi == 1)  %>%
        dplyr::mutate(Score = ncbi, Status = "accepted", Accepted_Taxon = scientific_name, Veri_source = "NCBI") %>%
        dplyr::select(-ncbi)
      
      sp.no_hit  <- sp.no_hit %>%
        dplyr::filter(ncbi==0)     
      
      sp.accepted <- rbind(sp.accepted, sp.hit)
    }
    
    sp.no_hit$ncbi <- NULL
    n_left         <- nrow(sp.no_hit)
  }   #  ncbi_ping() == TRUE
}


#****************************************************************
# 5. WorldFlora Online (WFO)                                 ####  
# http://www.worldfloraonline.org/
# Note: Database downloaded first. Not running as online search. 
#****************************************************************

repo_df$species_count[5] <- n_left

if (n_left > 0) {
  
  bParallel= TRUE
  # not parallel for less than n_cores
  bParallel <- ifelse(nrow(sp.no_hit) < n_cores, FALSE, bParallel)
  
  if (bParallel==TRUE) {
    if (os == "linux") {
 #    required package: doMC
      registerDoMC(n_cores)  #change the 2 to your number of CPU cores 
      
      cl <- parallel::makeCluster(n_cores, outfile= "")
      ##END LINUX WAY
    }
    
    if (os == "windows") {
      #      memory.limit(size=56000)
      cl <- parallel::makeCluster(n_cores, outfile= "")
    }
    registerDoParallel(cl)
  }
  
  print(  paste0("V. WFO: Number of species to check: ", nrow(sp.no_hit)))
  message(paste0("V. WFO: Number of species to check: ", nrow(sp.no_hit)))
  
  # https://stackoverflow.com/questions/17350867/split-data-set-and-pass-the-subsets-in-parallel-to-function-then-recombine-the-r
  cuts <- cut(1:nrow(sp.no_hit), n_cores)
  
  if (bParallel == TRUE) {
    sys.time<-system.time({ 
      
      sp.hit <- foreach(x=levels(cuts), .packages = "WorldFlora", .combine=rbind, .multicombine=TRUE) %dopar% { 
        WFO.match(sp.no_hit[cuts==x ,]$scientific_name, WFO.file=WFO_file)
      }
    })  
    stopCluster(cl); print("Cluster stopped.")
    # insert serial backend, otherwise error in repetitive tasks, https://github.com/tobigithub/R-parallel/wiki/R-parallel-Errors
    registerDoSEQ() 
    
    print(sys.time)
    
  } else {
    sp.hit <- try( WFO.match(spec.data=sp.no_hit,spec.name="scientific_name", WFO.file=WFO_file, counter=10), silent = TRUE) 
    # Note:  Fuzzy.min = TRUE, is the default setting, see e.g. https://www.biorxiv.org/content/10.1101/2020.02.02.930719v1.full.pdf
  }
  
  if (exists("sp.hit")) {
    if (!is.null(nrow(sp.hit))) {
      sp.hit$row_name <- as.numeric(row.names(sp.hit))
      # remove new rows in returned dataframe
      sp.hit          <- subset(sp.hit,(row_name - trunc(row_name) == 0 ))
      
      sp.hit <- sp.hit %>%
        dplyr::select(scientific_name=spec.name, Authors=scientificNameAuthorship, Status=taxonomicStatus,
            Accepted_Taxon=scientificName, PL_Comparison=Old.name, Alternative=Old.name, Matched, Fuzzy) %>% 
        dplyr::mutate(Authors = ifelse( str_count(scientific_name, " ") > 0 & str_count(Accepted_Taxon, " ") == 0, "", Authors  ),
          Status         = ifelse( str_count(scientific_name, " ") > 0 & str_count(Accepted_Taxon, " ") == 0, "", Status        ),
          Matched        = ifelse( str_count(scientific_name, " ") > 0 & str_count(Accepted_Taxon, " ") == 0, "", Matched       ),
          Accepted_Taxon = ifelse( str_count(scientific_name, " ") > 0 & str_count(Accepted_Taxon, " ") == 0, "", Accepted_Taxon),
          Score          = ifelse(Matched=="TRUE" & Fuzzy=="TRUE", "matched (fuzzy)", ifelse(Matched=="TRUE", "matched", ""    )),
          Veri_source    = ifelse(Matched=="TRUE", "WFO", ""),
          Status         = tolower(Status),
          Alternative    = ifelse(Alternative==scientific_name, "", Alternative),
          PL_Comparison  = "") %>%
        dplyr::select(-Matched, -Fuzzy)
      
      sp.accepted <- rbind(sp.accepted, sp.hit)
      
      sp.no_hit <- sp.hit %>%
        dplyr::filter(Veri_source=="")
      
      n_left    <- nrow(sp.no_hit)
      rm(sp.hit)
    } # exists("sp.hit")
  }
}

#******************************************************
# 6. GBIF (rgbif) ####
# Global Biodiversity Information Facility
# https://www.gbif.org/
# https://docs.ropensci.org/rgbif/
# Note: in case of GBIF we do not really "validate" the name
# but rather we check that species has been recorded by
# that given name into the GBIF database
#******************************************************

repo_df$species_count[6] <- n_left

if (n_left > 0) { # rgbif
  
  print(  paste0("VI. GBIF: Number of species to check: ", n_left))
  message(paste0("VI. GBIF: Number of species to check: ", n_left))
  
  # https://cran.r-project.org/web/packages/rgbif/rgbif.pdf
  keys2 <- sapply(sp.no_hit$scientific_name, function(x) name_suggest(x)$data$key[1], USE.NAMES=FALSE)
  
  if (unique(sapply(keys2, is.null)) != TRUE ) {
    
    sp.result3 <- rgbif::occ_search(taxonKey= keys2, limit=20)
    sp.no_hit$gbifName <- "na"
    
    for (i in 1 : length( keys2 )) {
      # print(i)
      if (!is.null( keys2[i][[1]]) ) {
        a <- getmode( sp.result3[[i]]$data$scientificName )
        
        if (!is.null(a)) {
          a <- strsplit(a, " ")
          if (length(a[[1]])>=2)
            sp.no_hit[i,]$gbifName <- paste(a[[1]][1], a[[1]][2], sep=" ")
        }
      }
    }
    
    rm(sp.result3); rm(a)
    
    sp.hit <- sp.no_hit %>%
      dplyr::filter(gbifName != "" & gbifName != "na") %>%
      dplyr::mutate(Veri_source = "GBIF",
        Status = ifelse( scientific_name==gbifName, "accepted", Status),
        Status = ifelse( scientific_name!=gbifName, "synonym (Check this!)", Status),
        Accepted_Taxon = ifelse( scientific_name==gbifName, scientific_name, Accepted_Taxon),
        Alternative = ifelse( scientific_name!=gbifName, gbifName, Alternative)) %>%
      dplyr::select(-gbifName)
    
    sp.no_hit <- sp.no_hit %>%
      dplyr::filter(gbifName == "" | gbifName == "na") %>%
      dplyr::select(-gbifName)
    
    sp.accepted <- rbind(sp.accepted, sp.hit, sp.no_hit)
    
    sp.accepted[is.na(sp.accepted)] <- ""
    rm(sp.hit)
    
  }
  rm(keys2)
}
repo_df$species_count[7] <- nrow(sp.no_hit)
rm(sp.no_hit)


#******************************************************
# clean LCVP Accepted_Taxon names, add genus 
sp.accepted <- sp.accepted %>%
  dplyr::mutate(Number_spaces  = str_count(scientific_name, " "),
    Number_spaces       = ifelse(Number_spaces>1, 1, Number_spaces),
    Accepted_Taxon      = ifelse( Veri_source=="LCVP", stringr::word(Accepted_Taxon,start=1,end=Number_spaces+1), Accepted_Taxon),
    Alternative         = ifelse( Veri_source=="LCVP" & Alternative != "", stringr::word(Alternative,start=1,end=Number_spaces+1), Alternative),
    Alternative         = ifelse( Alternative == "unresolved" | Alternative == scientific_name, "", Alternative),
    genus               = stringr::word(scientific_name,1),
    genusAccepted_Taxon = stringr::word(Accepted_Taxon,1),
    genusAccepted_Taxon = ifelse(genusAccepted_Taxon ==genus, "", genusAccepted_Taxon), 
    Accepted_Taxon      = ifelse( Accepted_Taxon==scientific_name, "", Accepted_Taxon)) %>%
  dplyr::select(-Number_spaces, -PL_Comparison) %>%
  distinct()

sp.accepted[is.na(sp.accepted)] <- ""



#******************************************************
# 7. IUCN code in GBIF (rgbif)                     ####
# Global Biodiversity Information Facility 
# https://en.wikipedia.org/wiki/Wikipedia:Conservation_status
#
# str(sp.result3$data$iucnRedListCategory)
#******************************************************
# 'ND' added by Lauri. It indicates that species exists in GBIF, but field $iucnRedListCategory did not exists
iucn_code  <- c('ND','NE','DD','LC','NT','VU','EN','CR','EW','EX')
iucn_label <- c('No Red List Data','Not evaluated','Data deficient','Least Concern','Near Threatened','Vulnerable','Endangered','Critically Endangered','Extinct in the wild','Extinct')


sp.df1 <- sp.accepted %>%
  dplyr::filter(Status != "") %>%
  dplyr::select(species=scientific_name) %>%
  distinct()
sp.df2 <- sp.accepted %>%
  dplyr::filter(Status != "" & Accepted_Taxon !="") %>%
  dplyr::select(species=Accepted_Taxon) %>%
  distinct()
sp.df3 <- sp.accepted %>%
  dplyr::filter(Status != "" & Alternative !="") %>%
  dplyr::select(species=Alternative) %>%
  distinct()

sp.df   <- rbind(sp.df1, sp.df2, sp.df3)
sp.df   <- sp.df %>%
  dplyr::filter( str_count(species, " ") > 0) %>%
  distinct()

sp.list <- unique(sp.df$species)
rm(sp.df1); rm(sp.df2); rm(sp.df3)

print(  paste0("VII. IUCN code in GBIF: Number of species to check: ", length(sp.list)))
message(paste0("VII. IUCN code in GBIF: Number of species to check: ", length(sp.list)))


# as above
keys2                     <- sapply(sp.list, function(x) name_suggest(x)$data$key[1], USE.NAMES=FALSE)
sp.result3                <- rgbif::occ_search(taxonKey= keys2, limit=20) 
sp.df$iucnRedListCategory <- "" 

for (i in 1 : length( keys2 )) {
  if (!is.null( keys2[i][[1]]) & ("iucnRedListCategory" %in% colnames(sp.result3[[i]]$data))) {
    a <- getmode( sp.result3[[i]]$data$iucnRedListCategory )
    if (!is.null(a)) sp.df[i,]$iucnRedListCategory <- a
  }
  if (!is.null( keys2[i][[1]]) & !"iucnRedListCategory" %in% colnames(sp.result3[[i]]$data)) sp.df[i,]$iucnRedListCategory <- 'ND'
}

sp.df$iucnRedList_index   <- match(sp.df$iucnRedListCategory, iucn_code)
sp.df$iucnRedListCategory <- ifelse(is.na(sp.df$iucnRedList_index), "", iucn_label[sp.df$iucnRedList_index])
sp.df$iucnRedList_index   <- NULL

rm(sp.result3); rm(keys2) 
rm(a)

sp.accepted <- sp.accepted %>%
  dplyr::left_join(sp.df, by = c("scientific_name" = "species")) %>%
  dplyr::mutate(iucnRL_scientific_name = iucnRedListCategory) %>%
  dplyr::select(-iucnRedListCategory)
# rename(iucnRL_scientific_name = iucnRedListCategory)
sp.accepted <- sp.accepted %>%
  dplyr::left_join(sp.df, by = c("Accepted_Taxon" = "species")) %>%
  dplyr::mutate(iucnRL_Accepted_Taxon = iucnRedListCategory) %>%
  dplyr::select(-iucnRedListCategory)
# rename(iucnRL_Accepted_Taxon = iucnRedListCategory)
sp.accepted <- sp.accepted %>%
  dplyr::left_join(sp.df, by = c("Alternative" = "species")) %>%
  dplyr::mutate(iucnRL_Alternative = iucnRedListCategory) %>%
  dplyr::select(-iucnRedListCategory)
# rename(iucnRL_Alternative = iucnRedListCategory)

sp.accepted[is.na(sp.accepted)] <- ""
rm(sp.df)



#**********************************************************
# 8. get family names (NCBI, taxize -- tax_name)       ####
# 
#**********************************************************
sp.family <- sp.accepted %>%
  dplyr::select(genus) %>%
  dplyr::arrange(genus) %>%
  distinct()

sp.family2 <- sp.accepted %>%
  dplyr::select(genusAccepted_Taxon) %>%
  dplyr::mutate(genus = genusAccepted_Taxon) %>%
  dplyr::select(-genusAccepted_Taxon) %>%
  distinct()

sp.family <- rbind(sp.family, sp.family2)
rm(sp.family2)
sp.family <- sp.family %>%
  dplyr::filter(genus != "") %>%
  distinct() %>%
  dplyr::arrange(genus)

print(  paste0("VIII. NCBI search for family. Number to check: ", nrow(sp.family)))
message(paste0("VIII. NCBI search for family. Number to check: ", nrow(sp.family)))


fm <- sapply(sp.family$genus, function(x) tax_name(sci = x, get = "family", db = "ncbi", messages=FALSE, ask=FALSE)$family, USE.NAMES=FALSE)
sp.family$family <- fm
sp.family[is.na(sp.family)] <- ""
rm(fm)

sp.family$family2 <- sp.family$family

sp.accepted <- sp.accepted %>%
  dplyr::left_join(sp.family, by="genus") %>%
  dplyr::select(-genus, -family2) %>%
  dplyr::mutate(genus= ifelse(family=="", genusAccepted_Taxon, "")) %>%
  dplyr::select(-genusAccepted_Taxon)

sp.family$family <- NULL

sp.accepted <- sp.accepted %>%
  dplyr::left_join(sp.family, by="genus") %>%
  dplyr::select(-genus) %>%
  dplyr::mutate(family= ifelse(family=="" & family2 != "", family2, family)) %>%
  dplyr::select(-family2) 

rm(sp.family)



#**********************************************************
# 9. GlobalTreeSearch                                  ####
# 
# https://tools.bgci.org/global_tree_search.php
# Note: Database downloaded first. Running as local search.
#**********************************************************

print(  "IX. GlobalTreeSearch: checking scientific_name, Accepted_Taxon, Alternative")
message("IX. GlobalTreeSearch: checking scientific_name, Accepted_Taxon, Alternative")

sp.accepted$GTS_scientific_name <- with(sp.accepted,
                                        ifelse( scientific_name %in% global_tree_list | scientific_name %in% global_tree_genus_list , "Match", ""))

sp.accepted$GTS_Accepted_Taxon  <- with(sp.accepted,
                                        ifelse( Accepted_Taxon!=scientific_name & ( Accepted_Taxon %in% global_tree_list | Accepted_Taxon %in% global_tree_genus_list ), "Match", ""))

sp.accepted$GTS_Alternative   <- with(sp.accepted,
                                      ifelse( Alternative %in% global_tree_list | Alternative  %in% global_tree_genus_list, "Match", ""))


# get code back into dataframe
sp.accepted <- DF_sp_list %>%
  dplyr::left_join(sp.accepted, by="scientific_name") 

sp.accepted[is.na(sp.accepted)] <- "" 

write.csv(sp.accepted, outFile1, row.names = FALSE)
write.csv(repo_df,     outFile2, row.names = FALSE)


#******************************************************
# Compute result statistics
#

stats_table <- sp.accepted %>%
  dplyr::mutate(Status_Score = paste(Status, Score, sep=" - "), Veri_source= ifelse(Veri_source=="", " - ", Veri_source)) %>%
  group_by(Status_Score, Veri_source) %>%
  dplyr::summarise(n=n(), .groups = "drop_last") %>%
  spread(Veri_source, n) %>%
  as.data.frame()

stats_table[is.na(stats_table)] <- 0

col_order <- c("Status_Score", repo_name, " - " )
col_order <- subset(col_order, 
                    !is.na(
                      match(col_order, names(stats_table))
                    ))

stats_table        <- stats_table[, col_order] # reorder the columns
stats_table$nodata <- stats_table$' - '
stats_table$' - '  <- NULL

write.csv(stats_table, outFile3, row.names = FALSE)



#View(sp.accepted)
#View(repo_df)

Sys.sleep(0.5)
message("End - Species Validation successfully executed")
Sys.sleep(0.5)
message("You can see the results of the analysis by clicking on 'Result table'.")
message(" Processing Successful 100%")
message("-----------------------")

# END #################################################