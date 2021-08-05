#loading robis
installed <- rownames(installed.packages())
if ( !("robis" %in% installed) ){
  if ( !("remotes" %in% installed) )install.packages("remotes")
  remotes::install_github("iobis/robis")
}

#set working directory
setwd('C:/Users/nposd/Documents/GitHub/ohw20-proj-species-marine-protected-areas/R')

#Set save directory
saveDir = paste('I:/Shared drives/ohw-obis-mpa/Updates/')

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
library(ggplot2)
library(scales)
source("wdpar-package.R")

# Run wdpa_read_country function from wdpar-package.R 
Country = "Cuba" #country of interest
WDPA <- wdpa_read_country(country = Country, team = "ohw-obis-mpa",
                          ext = ".gpkg") %>%
  dplyr::filter(MARINE >= 1) #only keep marine MPAs


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


#preallocate a column for species count, ES50, and phylum ratio at each timestep
ES50_df$SpeciesCountALL = NA
ES50_df$ES50_ALL = NA
ES50_df$Phylum_ALL = NA

ES50_df$SpeciesCount_prior20 = NA
ES50_df$ES50_prior20 = NA
ES50_df$Phylum_prior20 = NA

ES50_df$SpeciesCount_after10 = NA
ES50_df$ES50_after10 = NA
ES50_df$Phylum_after10 = NA

ES50_df$SpeciesCount_after20 = NA
ES50_df$ES50_after20 = NA
ES50_df$Phylum_after20 = NA

ES50_df$SpeciesCount_after40 = NA
ES50_df$ES50_after40 = NA
ES50_df$Phylum_after40 = NA

ES50_df$SpeciesCount_after60 = NA
ES50_df$ES50_after60 = NA
ES50_df$Phylum_after60 = NA

ES50_df$SpeciesCount_after80 = NA
ES50_df$ES50_after80 = NA
ES50_df$Phylum_after80 = NA

ES50_df$SpeciesCount_after100 = NA
ES50_df$ES50_after100 = NA
ES50_df$Phylum_after100 = NA

ES50_df$SpeciesCount_after120 = NA
ES50_df$ES50_after120 = NA
ES50_df$Phylum_after120 = NA

#loop through all of the MPAs for the country of interest
for (i in 1:nrow(ES50_df)){ 
  hull = st_convex_hull(ES50_df$geom[[i]]) #find the convex hull for the polygon
  text = st_as_text(hull) #convert it to text
  SpeciesOccurence = try(occurrence(geometry = text)) #query OBIS for species within that polygon
  if (inherits(SpeciesOccurence, "try-error")) SpeciesOccurence <- NULL
  if (!is_empty(SpeciesOccurence)){ #if there is OBIS data..
  if ("individualCount" %in% colnames(SpeciesOccurence)){ #if individualCount is an available column..
  SpeciesOccurence$individualCount <- suppressWarnings(as.numeric(SpeciesOccurence$individualCount)) #introduces NAs
  SpeciesOccurence$individualCount[is.na(SpeciesOccurence$individualCount)] <- 1 #convert NANs to 1; I'm assuming that if it's listed, there was at least one count
  SpeciesOccurence$Count <- 1 * SpeciesOccurence$individualCount #make a new column for Counts of each species
  } else {
  SpeciesOccurence$Count = 1 #if individualCount is not a column, I'm assuming that if it's listed there was at least 1 count
  }
  SpeciesCount <- aggregate(SpeciesOccurence$Count, by=list(Category=SpeciesOccurence$scientificName),FUN=sum)#aggregate based on species
  ES50_df$SpeciesCountALL[i] = nrow(SpeciesCount) #calculate the number of unique species
  if (nrow(SpeciesCount) >= 50){ #if there are more than 50 species...
    ES50_df$ES50_ALL[i] = rarefy(SpeciesCount$x,50) #calculate ES50 for all records
  }
  #Calculate ES50 before and after specified dates
  if ("date_year" %in% colnames(SpeciesOccurence)){ #the column date_year seemed the most consistenly filled out..
    SpeciesOccurence_prior20 = subset(SpeciesOccurence, SpeciesOccurence$date_year < ES50_df$STATUS_YR[i]) #subset the data for species present 20 years before MPA designation
    if (dim(SpeciesOccurence_prior20)[1] > 0){
    SpeciesCount_prior20 <- aggregate(SpeciesOccurence_prior20$Count, by=list(Category=SpeciesOccurence_prior20$scientificName),FUN=sum)
    ES50_df$SpeciesCount_prior20[i] = nrow(SpeciesOccurence_prior20) #save the number of species for that time frame
    if (nrow(SpeciesOccurence_prior20) >= 50){
      ES50_df$ES50_prior20[i] = rarefy(SpeciesCount_prior20 $x,50) #calculate ES50 prior 20 years
    }
    }
    
    if (ES50_df$after10[i] < 2022){
    SpeciesOccurence_after10 = subset(SpeciesOccurence, SpeciesOccurence$date_year > ES50_df$STATUS_YR[i] & SpeciesOccurence$date_year < ES50_df$after10[i]) #subset the data for species 10 years after before MPA designation
    if (dim(SpeciesOccurence_after10)[1] > 0){
      SpeciesCount_after10 <- aggregate(SpeciesOccurence_after10$Count, by=list(Category=SpeciesOccurence_after10$scientificName),FUN=sum)
    ES50_df$SpeciesCount_after10[i] = nrow(SpeciesOccurence_after10) #save the number of species for that time frame
    if (nrow(SpeciesOccurence_after10) >= 50){
      ES50_df$ES50_after10[i] = rarefy(SpeciesCount_after10 $x,50) #calculate ES50 after 10 years
    }
    }
    }
    
    if (ES50_df$after20[i] < 2022){
    SpeciesOccurence_after20 = subset(SpeciesOccurence, SpeciesOccurence$date_year > ES50_df$STATUS_YR[i] & SpeciesOccurence$date_year < ES50_df$after20[i]) #subset the data for species present 20 years after MPA designation
    if (dim(SpeciesOccurence_after20)[1] > 0){
    SpeciesCount_after20 <- aggregate(SpeciesOccurence_after20$Count, by=list(Category=SpeciesOccurence_after20$scientificName),FUN=sum)
    ES50_df$SpeciesCount_after20[i] = nrow(SpeciesOccurence_after20) #save the number of species for that time frame
    if (nrow(SpeciesOccurence_after20) >= 50){
      ES50_df$ES50_after20[i] = rarefy(SpeciesCount_after20 $x,50) #calculate ES50 after 20 years
    }
    }
    }
    
    if (ES50_df$after40[i] < 2022){
    SpeciesOccurence_after40 = subset(SpeciesOccurence, SpeciesOccurence$date_year > ES50_df$STATUS_YR[i] & SpeciesOccurence$date_year < ES50_df$after40[i]) #subset the data for species present 40 years after MPA designation
    if (dim(SpeciesOccurence_after40)[1] > 0){
      SpeciesCount_after40 <- aggregate(SpeciesOccurence_after40$Count, by=list(Category=SpeciesOccurence_after40$scientificName),FUN=sum)
    ES50_df$SpeciesCount_after40[i] = nrow(SpeciesOccurence_after40) #save the number of species for that time frame
    if (nrow(SpeciesOccurence_after40) >= 50){
      ES50_df$ES50_after40[i] = rarefy(SpeciesCount_after40 $x,50) #calculate ES50 after 40 years
    }
    }
    }
    
    if (ES50_df$after60[i] < 2022){
    SpeciesOccurence_after60 = subset(SpeciesOccurence, SpeciesOccurence$date_year > ES50_df$STATUS_YR[i] & SpeciesOccurence$date_year < ES50_df$after60[i]) #subset the data for species present 60 years after MPA designation
    if (dim(SpeciesOccurence_after60)[1] > 0){
    SpeciesCount_after60 <- aggregate(SpeciesOccurence_after60$Count, by=list(Category=SpeciesOccurence_after60$scientificName),FUN=sum)
    ES50_df$SpeciesCount_after60[i] = nrow(SpeciesOccurence_after60) #save the number of species for that time frame
    if (nrow(SpeciesOccurence_after60) >= 50){
      ES50_df$ES50_after60[i] = rarefy(SpeciesCount_after60 $x,50) #calculate ES50 after 60 years
    }
    }
    }
    
    if (ES50_df$after80[i] < 2022){
    SpeciesOccurence_after80 = subset(SpeciesOccurence, SpeciesOccurence$date_year > ES50_df$STATUS_YR[i] & SpeciesOccurence$date_year < ES50_df$after80[i]) #subset the data for species present 80 years after MPA designation
    if (dim(SpeciesOccurence_after80)[1] > 0){
      SpeciesCount_after80 <- aggregate(SpeciesOccurence_after80$Count, by=list(Category=SpeciesOccurence_after80$scientificName),FUN=sum)
    ES50_df$SpeciesCount_after80[i] = nrow(SpeciesOccurence_after80) #save the number of species for that time frame
    if (nrow(SpeciesOccurence_after80) >= 50){
      ES50_df$ES50_after80[i] = rarefy(SpeciesCount_after80 $x,50) #calculate ES50 after 80 years
    }
    }
    }
    
    if (ES50_df$after100[i] < 2022){
    SpeciesOccurence_after100 = subset(SpeciesOccurence, SpeciesOccurence$date_year > ES50_df$STATUS_YR[i] & SpeciesOccurence$date_year < ES50_df$after100[i]) #subset the data for species present 100 years after MPA designation
    if (dim(SpeciesOccurence_after100)[1] > 0){
      SpeciesCount_after100 <- aggregate(SpeciesOccurence_after100$Count, by=list(Category=SpeciesOccurence_after100$scientificName),FUN=sum)
    ES50_df$SpeciesCount_after100[i] = nrow(SpeciesOccurence_after100) #save the number of species for that time frame
    if (nrow(SpeciesOccurence_after100) >= 50){
      ES50_df$ES50_after100[i] = rarefy(SpeciesCount_after100 $x,50) #calculate ES50 after 100 years
    }
    }
    }
    
    if (ES50_df$after120[i] < 2022){
    SpeciesOccurence_after120 = subset(SpeciesOccurence, SpeciesOccurence$date_year > ES50_df$STATUS_YR[i] & SpeciesOccurence$date_year < ES50_df$after120[i]) #subset the data for species present 120 years after MPA designation
    if (dim(SpeciesOccurence_after120)[1] > 0){
      SpeciesCount_after120 <- aggregate(SpeciesOccurence_after120$Count, by=list(Category=SpeciesOccurence_after120$scientificName),FUN=sum)
    ES50_df$SpeciesCount_after120[i] = nrow(SpeciesOccurence_after120) #save the number of species for that time frame
    if (nrow(SpeciesOccurence_after120) >= 50){
      ES50_df$ES50_after120[i] = rarefy(SpeciesCount_after80 $x,50) #calculate ES50 after 120 years
    }
    }
    }
  }
  }
}

#save data frame
write.csv(ES50_df,paste(saveDir,Country,'_ES50_df.csv', sep=""))
