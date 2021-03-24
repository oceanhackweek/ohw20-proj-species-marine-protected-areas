#loading robis
installed <- rownames(installed.packages())
if ( !("robis" %in% installed) ){
  if ( !("remotes" %in% installed) )install.packages("remotes")
  remotes::install_github("iobis/robis")
}

#setwd
setwd('C:/Users/nposd/Documents/GitHub/ohw20-proj-species-marine-protected-areas/R')

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
library(rapport)
source("wdpar-package.R")

# Run wdpa_read_country function from wdpar-package.R 
WDPA <- wdpa_read_country(country = 'Cuba', team = "ohw-obis-mpa",
                          ext = ".gpkg") %>%
  dplyr::filter(MARINE >= 1) 
  #dplyr::arrange(describe(REP_AREA)) %>% #organize by largest to smallest


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

ES50_df$SpeciesCount_prior20 = NA
ES50_df$ES50__prior20 = NA

ES50_df$SpeciesCount_after10 = NA
ES50_df$ES50_after10 = NA

ES50_df$SpeciesCount_after20 = NA
ES50_df$ES50_after20 = NA

ES50_df$SpeciesCount_after40 = NA
ES50_df$ES50_after40 = NA

ES50_df$SpeciesCount_after60 = NA
ES50_df$ES50_after60 = NA

ES50_df$SpeciesCount_after80 = NA
ES50_df$ES50_after80 = NA

ES50_df$SpeciesCount_after100 = NA
ES50_df$ES50_after100 = NA

ES50_df$SpeciesCount_after120 = NA
ES50_df$ES50_after120 = NA

#loop through all polygons and extract OBIS data
#blah = st_simplify(ES50_df$geom[[1]])
#blah2 = st_as_text(ES50_df$geom[[1]])
#blah3 = st_convex_hull(ES50_df$geom[[1]])
#blah3 = st_as_text(blah3)


for (i in 1:nrow(ES50_df)){ #I have to figure out why the first MPA is giving an error
  hull = st_convex_hull(ES50_df$geom[[i]])
  text = st_as_text(hull)
  SpeciesOccurence = occurrence(geometry = text)
  if (is.integer(SpeciesOccurence)){
  if ("individualCount" %in% colnames(SpeciesOccurence)){
  #convert individual counts from character to numeric
  SpeciesOccurence$individualCount <- as.numeric(SpeciesOccurence$individualCount) 
  SpeciesOccurence$individualCount[is.na(SpeciesOccurence$individualCount)] <- 1 #convert NANs to 1; I'm assuming that if it's listed, there was at least one count, even if it wasn't listed
  SpeciesOccurence$Count <- 1 * SpeciesOccurence$individualCount
  SpeciesOccurence$eventDate = extract_obis_date(SpeciesOccurence$eventDate) #change eventDate from character to date
  }
  #calculate the number of unique species
  SpeciesCount <- aggregate(SpeciesOccurence$Count, by=list(Category=SpeciesOccurence$scientificName),FUN=sum)
  ES50_df$SpeciesCountALL[i] = nrow(SpeciesCount)
  if (nrow(SpeciesCount) >= 50){
    ES50_df$ES50_ALL[i] = rarefy(SpeciesCount$x,50) #calculate ES50 for all records
  }
    #Calculate ES50 before and after specified dates
    SpeciesOccurence_prior20 = subset(SpeciesOccurence, SpeciesOccurence$year < ES50_df$STATUS_YR[i])
    SpeciesCount_prior20 <- aggregate(SpeciesOccurence_prior20$Count, by=list(Category=SpeciesOccurence_prior20$scientificName),FUN=sum)
    ES50_df$SpeciesCount_prior20[i] = nrow(SpeciesOccurence_prior20) #save the number of species for that time frame
    if (nrow(SpeciesOccurence_prior20) >= 50){
      ES50_df$ES50_prior20[i] = rarefy(SpeciesCount_prior20 $x,50) #calculate ES50 prior 20 years
    }
    SpeciesOccurence_after10 = subset(SpeciesOccurence, SpeciesOccurence$year > ES50_df$STATUS_YR[i] & SpeciesOccurence$year < ES50_df$after10[i])
    SpeciesCount_after10 <- aggregate(SpeciesOccurence_after10$Count, by=list(Category=SpeciesOccurence_after10$scientificName),FUN=sum)
    ES50_df$SpeciesCount_after10[i] = nrow(SpeciesOccurence_after10) #save the number of species for that time frame
    if (nrow(SpeciesOccurence_after10) >= 50){
      ES50_df$ES50_after10[i] = rarefy(SpeciesCount_after10 $x,50) #calculate ES50 after 10 years
    }
    SpeciesOccurence_after20 = subset(SpeciesOccurence, SpeciesOccurence$year > ES50_df$STATUS_YR[i] & SpeciesOccurence$year < ES50_df$after20[i])
    SpeciesCount_after20 <- aggregate(SpeciesOccurence_after20$Count, by=list(Category=SpeciesOccurence_after20$scientificName),FUN=sum)
    ES50_df$SpeciesCount_after20[i] = nrow(SpeciesOccurence_after20) #save the number of species for that time frame
    if (nrow(SpeciesOccurence_after20) >= 50){
      ES50_df$ES50_after20[i] = rarefy(SpeciesCount_after20 $x,50) #calculate ES50 after 20 years
    }
    SpeciesOccurence_after40 = subset(SpeciesOccurence, SpeciesOccurence$year > ES50_df$STATUS_YR[i] & SpeciesOccurence$year < ES50_df$after40[i])
    SpeciesCount_after40 <- aggregate(SpeciesOccurence_after40$Count, by=list(Category=SpeciesOccurence_after40$scientificName),FUN=sum)
    ES50_df$SpeciesCount_after40[i] = nrow(SpeciesOccurence_after40) #save the number of species for that time frame
    if (nrow(SpeciesOccurence_after40) >= 50){
      ES50_df$ES50_after40[i] = rarefy(SpeciesCount_after40 $x,50) #calculate ES50 after 40 years
    }
    SpeciesOccurence_after60 = subset(SpeciesOccurence, SpeciesOccurence$year > ES50_df$STATUS_YR[i] & SpeciesOccurence$year < ES50_df$after60[i])
    SpeciesCount_after60 <- aggregate(SpeciesOccurence_after60$Count, by=list(Category=SpeciesOccurence_after60$scientificName),FUN=sum)
    ES50_df$SpeciesCount_after60[i] = nrow(SpeciesOccurence_after60) #save the number of species for that time frame
    if (nrow(SpeciesOccurence_after60) >= 50){
      ES50_df$ES50_after60[i] = rarefy(SpeciesCount_after60 $x,50) #calculate ES50 after 60 years
    }
    SpeciesOccurence_after80 = subset(SpeciesOccurence, SpeciesOccurence$year > ES50_df$STATUS_YR[i] & SpeciesOccurence$year < ES50_df$after80[i])
    SpeciesCount_after80 <- aggregate(SpeciesOccurence_after80$Count, by=list(Category=SpeciesOccurence_after80$scientificName),FUN=sum)
    ES50_df$SpeciesCount_after80[i] = nrow(SpeciesOccurence_after80) #save the number of species for that time frame
    if (nrow(SpeciesOccurence_after80) >= 50){
      ES50_df$ES50_after80[i] = rarefy(SpeciesCount_after80 $x,50) #calculate ES50 after 80 years
    }
    SpeciesOccurence_after100 = subset(SpeciesOccurence, SpeciesOccurence$year > ES50_df$STATUS_YR[i] & SpeciesOccurence$year < ES50_df$after100[i])
    SpeciesCount_after100 <- aggregate(SpeciesOccurence_after100$Count, by=list(Category=SpeciesOccurence_after100$scientificName),FUN=sum)
    ES50_df$SpeciesCount_after100[i] = nrow(SpeciesOccurence_after100) #save the number of species for that time frame
    if (nrow(SpeciesOccurence_after100) >= 50){
      ES50_df$ES50_after100[i] = rarefy(SpeciesCount_after100 $x,50) #calculate ES50 after 100 years
    }
    SpeciesOccurence_after120 = subset(SpeciesOccurence, SpeciesOccurence$year > ES50_df$STATUS_YR[i] & SpeciesOccurence$year < ES50_df$after120[i])
    SpeciesCount_after120 <- aggregate(SpeciesOccurence_after120$Count, by=list(Category=SpeciesOccurence_after120$scientificName),FUN=sum)
    ES50_df$SpeciesCount_after120[i] = nrow(SpeciesOccurence_after120) #save the number of species for that time frame
    if (nrow(SpeciesOccurence_after120) >= 50){
      ES50_df$ES50_after120[i] = rarefy(SpeciesCount_after80 $x,50) #calculate ES50 after 120 years
    }
  }
}
