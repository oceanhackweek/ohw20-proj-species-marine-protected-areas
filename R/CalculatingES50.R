#loading robis
installed <- rownames(installed.packages())
if ( !("robis" %in% installed) ){
  if ( !("remotes" %in% installed) )install.packages("remotes")
  remotes::install_github("iobis/robis")
}

#load libraries
library(robis)
library('ggplot2')
library("rnaturalearth")
library("rnaturalearthdata")
library(roperators)
library(dplyr)
library(magrittr) 
library(vegan)
library(wdpar)
library(terra)
library(raster)
library(ncdf4)
library(tibble)
library(tidyverse)
library(leaflet)
library(occ)
library(spocc)
source("wdpar-package.R")

# Run wdpa_read_country function from wdpar-package.R 
WDPA <- wdpa_read_country("Iceland") %>%
  dplyr::arrange(desc(REP_AREA)) %>% #organize by largest to smallest
  dplyr::filter(MARINE >= 1) 

#create dataframe to mutate
ES50_df <- WDPA[,c("WDPAID", "STATUS_YR", "REP_AREA", "IUCN_CAT", "geom")]

#calculate time points for each MPA
ES50_df <- ES50_df %>% 
  dplyr::filter(STATUS_YR > 0) %>% 
  dplyr::mutate(age = 2021 - STATUS_YR,
                prior20 = STATUS_YR - 20,
                after10 = STATUS_YR + 10,
                after20 = STATUS_YR + 20,
                after40 = STATUS_YR + 40,
                after60 = STATUS_YR + 60,
                after80 = STATUS_YR + 80,
                after100 = STATUS_YR + 100,
                after120 = STATUS_YR + 120) 

#convert sf to WKT string
ES50_df$WKT = st_as_text(ES50_df$geom)

#function for extracting obis dates
extract_obis_date <- function(x = c("2016-01-02", "2016-01-03T05:06:07", "June 29, 1999")){
  as.Date(substring(x, 1, nchar("1234-56-78")), format = "%Y-%m-%d")
}

#preallocate a column for species count and ES50
ES50_df$SpeciesCountALL = NA
ES50_df$ES50_ALL = NA

ES50_df$SpeciesCount_Prior20 = NA
ES50_df$ES50__Prior20 = NA

ES50_df$SpeciesCount_After10 = NA
ES50_df$ES50_After10 = NA

ES50_df$SpeciesCount_After20 = NA
ES50_df$ES50_After20 = NA

ES50_df$SpeciesCount_After40 = NA
ES50_df$ES50_After40 = NA

ES50_df$SpeciesCount_After60 = NA
ES50_df$ES50_After60 = NA

ES50_df$SpeciesCount_After80 = NA
ES50_df$ES50_After80 = NA

ES50_df$SpeciesCount_After100 = NA
ES50_df$ES50_After100 = NA

ES50_df$SpeciesCount_After120 = NA
ES50_df$ES50_After120 = NA

#loop through all polygons and extract OBIS data

for (i in 2:nrow(ES50_df)){ #I have to figure out why the first MPA is giving an error
  SpeciesOccurence = occurrence(geometry = ES50_df$WKT[[i]])
  if ("individualCount" %in% colnames(SpeciesOccurence)){
  #convert individual counts from character to numeric
  SpeciesOccurence$individualCount <- as.numeric(SpeciesOccurence$individualCount) 
  SpeciesOccurence$individualCount[is.na(SpeciesOccurence$individualCount)] <- 1 #convert NANs to 1; I'm assuming that if it's listed, there was at least one count, even if it wasn't listed
  SpeciesOccurence$Count <- 1 * SpeciesOccurence$individualCount
  SpeciesOccurence$eventDate = extract_obis_date(SpeciesOccurence$eventDate) #change eventDate from character to date
  #calculate the number of unique species
  SpeciesCount <- aggregate(SpeciesOccurence$Count, by=list(Category=SpeciesOccurence$scientificName),FUN=sum)
  ES50_df$SpeciesCount[i] = nrow(SpeciesCount)
  if (nrow(SpeciesCount) >= 50){
    ES50_df$ES50_ALL = rarefy(SpeciesCount$x,50) #calculate ES50 for all records
  }
    #Calculate ES50 before and after specified dates
    SpeciesOccurence_prior20 =subset(SpeciesOccurence, SpeciesOccurence$year < ES50_df$STATUS_YR[i])
    SpeciesCount_prior20 <- aggregate(SpeciesOccurence_prior20$Count, by=list(Category=SpeciesOccurence_prior20$scientificName),FUN=sum)
    if (nrow(SpeciesOccurence_prior20) >= 50){
      ES50_df$ES50_prior20 = rarefy(SpeciesCount_prior20 $x,50) #calculate ES50 for all records
    }
    SpeciesOccurence_after10
    SpeciesOccurence_after20
    SpeciesOccurence_after40
    SpeciesOccurence_after60
    SpeciesOccurence_after80
    SpeciesOccurence_after100
    SpeciesOccurence_after120
  }
  }



#Calculate ES50 before and after a specified date
MPA_established = as.Date(c("2007-06-22")) #what date was the MPA established

SpeciesOccurence_preMPA <- subset(SpeciesOccurence, SpeciesOccurence$eventDate < MPA_established) #Species Occurences before the MPA was established
SpeciesOccurence_postMPA <- subset(SpeciesOccurence, SpeciesOccurence$eventDate > MPA_established) #Species Occurences after the MPA was established

#Calculate unique species pre-MPA
SpeciesCount_preMPA <- aggregate(SpeciesOccurence_preMPA$Count, by=list(Category=SpeciesOccurence_preMPA$scientificName),FUN=sum)

#Calculate ES50 pre-MPA
ES50_preMPA = rarefy(SpeciesCount_preMPA$x,50) #calculate ES50
print(ES50_preMPA)

#Calculate unique species post-MPA
SpeciesCount_postMPA <- aggregate(SpeciesOccurence_postMPA$Count, by=list(Category=SpeciesOccurence_postMPA$scientificName),FUN=sum)

#Calculate ES50 post-MPA
ES50_postMPA = rarefy(SpeciesCount_postMPA$x,50) #calculate ES50
print(ES50_postMPA)