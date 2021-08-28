#******************************************************
# SPECIES SYNONYM SEARCH TOOL
# (c) Lauri Vesa, FAO
# version 8th April 2021
#
# Reads input data from CSV file as 'sp.accepted'
# 
#******************************************************
# set input folder and file names
#******************************************************

setwd("C:/Temp/")

iFile <- "D:/FAO/0 OPEN FORIS Project 2018/2. Software development and licences/Species search and validation/New version 2020/Result_test_subset_alder1.csv"
sp.accepted <- read.csv(iFile,header = T, stringsAsFactors = F)
outFile     <- "Result_synonyms.csv"

# WFO database:
# http://www.worldfloraonline.org/downloadData
WFO_file    <- "D:/WorldFlora/classification.txt"

#******************************************************

library("utils")
library("stringr")
library("data.table") 
library("WorldFlora")
library("taxize")
library("plyr")
library("dplyr")

if (!exists("WFO.data1")) WFO.data1 <- data.table::fread(WFO_file, encoding="UTF-8")

#create temporary index
sp.accepted <- sp.accepted %>% mutate(codeRow = paste0("R_",row_number()))
# word count
sp.accepted$words <- str_count(sp.accepted$scientific_name, '\\w+')

sp.accepted$synonymList <- ""
sp.accepted$synonymList_SourceField <- ""

for (i in (1:nrow(sp.accepted))) {
  nname = ifelse(sp.accepted$words[i]>1 & sp.accepted$Status[i]=="accepted" & sp.accepted$Accepted_Taxon[i]=="", sp.accepted$scientific_name[i],
          ifelse(sp.accepted$words[i]>1 & sp.accepted$Status[i]=="accepted" & sp.accepted$Accepted_Taxon[i]!="", sp.accepted$Accepted_Taxon[i],
          ifelse(sp.accepted$words[i]>1 & sp.accepted$Status[i]=="synonym"  & sp.accepted$Accepted_Taxon[i]!="", sp.accepted$Accepted_Taxon[i], "Name skipped.."
        )))
  namesource = ifelse(sp.accepted$words[i]>1 & sp.accepted$Status[i]=="accepted" & sp.accepted$Accepted_Taxon[i]=="", "scientific_name",
               ifelse(sp.accepted$words[i]>1 & sp.accepted$Status[i]=="accepted" & sp.accepted$Accepted_Taxon[i]!="", "scientific_name",
               ifelse(sp.accepted$words[i]>1 & sp.accepted$Status[i]=="synonym"  & sp.accepted$Accepted_Taxon[i]!="", "Accepted_Taxon", ""
        )))
  

  print( paste(i, nname, sep=": ") )
  if (nname != "Name skipped..") {
    search1 <- WFO.synonyms(nname, WFO.data=WFO.data1, Fuzzy=0 )
    search1 <- subset(search1, taxonomicStatus == "Synonym")
    search1 <- subset(search1, scientificName != nname)
  
    if (nrow(search1)>0 & grepl(" ", nname)) {   # no synonyms for genus-only names
      synonym_names <- paste( unique(search1$scientificName), collapse='; ') 
      sp.accepted$synonymList[i] <- synonym_names 
      sp.accepted$synonymList_SourceField[i] <- paste0(namesource, " * WFO")
    }
  }
}

# word count for synonyms, drop out single genus names
sp.accepted$words2                  <- str_count(sp.accepted$synonymList, '\\w+')
sp.accepted$synonymList             <- ifelse(sp.accepted$words2 > 1, sp.accepted$synonymList, "" ) 
sp.accepted$synonymList_SourceField <- ifelse(sp.accepted$words2 > 1, sp.accepted$synonymList_SourceField, "" ) 
sp.accepted$words2                  <- NULL

# ________________________________________

#******************************************************
# 3. taxize - pow_search  (Kew)                    ####
# Plants of the World Online
# http://www.plantsoftheworldonline.org/
# http://powo.science.kew.org/terms-and-conditions
# see parameters at https://rdrr.io/cran/taxize/man/pow_search.html
#******************************************************

kew_search <- function(sp.accepted, sp.no_hit) {
    
  print(paste0("taxize-pow_search: Number of species to check: ", nrow(sp.no_hit)))
  
  sp.no_hit$synonymList2 = ""
  syn_list= c()

  for (i in 1: nrow(sp.no_hit)) {
    Sys.sleep(0.4)
    temp2 <- pow_search( sp.no_hit$NNAME[i], limit=10 )
    temp2 <- temp2$data
    if (!is.null(temp2)) {
     laskuri=0
     for (j in 1:nrow(temp2)) {    
      if (temp2$accepted==TRUE) {
        if (sp.no_hit$NNAME[i] != temp2$name[j] ) {
          laskuri = laskuri+1
          syn_list[laskuri]  = temp2$name[j]
        }
      } else if (any(names(temp2) == 'synonymOf')) {
        if (temp2$synonymOf$accepted == TRUE )  {
          laskuri = laskuri+1
          syn_list[laskuri]  = temp2$synonymOf$name
        }
      }
     }}
    if (length(syn_list>0)) sp.no_hit$synonymList2[i] <- paste( unique(syn_list), collapse='; ')
    syn_list= c()
  }
  
  
  sp.no_hit <- sp.no_hit %>%
    dplyr::filter( synonymList2 != "") %>%
    dplyr::select(-NNAME)

  if (nrow(sp.no_hit) > 0) {
    
    sp.accepted <- sp.accepted %>%
      left_join( sp.no_hit, by = "codeRow") %>%
      mutate( synonymList2           = ifelse( is.na(synonymList2), "", synonymList2)) %>%                      
      mutate( synonymList            = ifelse( synonymList2 != "", synonymList2, synonymList )) %>%
      mutate(synonymList_SourceField = ifelse( synonymList2 != "", paste0(NSOURCE, " * Kew"), synonymList_SourceField )) %>%
      dplyr::select(-synonymList2)
  }
  
  return(sp.accepted)
}


# get name to search *********************
sp.no_hit <- sp.accepted %>%
  dplyr::filter(words>1 & synonymList == "" & Status=="accepted") %>%
  mutate(NNAME   = ifelse(Accepted_Taxon=="", scientific_name, Accepted_Taxon))     %>%
  mutate(NSOURCE = ifelse(Accepted_Taxon=="", "scientific_name", "Accepted_Taxon")) %>%
  dplyr::select( codeRow, NNAME, NSOURCE )

sp.accepted$words   <- NULL
if (nrow(sp.no_hit) > 0) sp.accepted <- kew_search(sp.accepted, sp.no_hit)

sp.accepted$NSOURCE <- NULL
sp.accepted$NNAME   <- NULL
sp.accepted$codeRow <- NULL

write.csv(sp.accepted, outFile,row.names = F)
