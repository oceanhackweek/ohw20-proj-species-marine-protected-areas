## Code to extract co-variant data for MPAS
## Jan 15, 2020
##Updated Feb 11, 2021 - JE and RF

library(sf)
library(dplyr)
library(wdpar)
library(terra)
library(raster)
library(ncdf4)
source("setup.R")
source("wdpar-package.R")
source("Rene-setup.R")

# Run wdpa_read_country function from wdpar-package.R 
WDPA <- wdpa_read_country("Iceland") %>%
  dplyr::arrange(desc(REP_AREA)) %>% #organize by largest to smallest
  dplyr::filter(MARINE == 2) #only accept those that are completely marine

#read in GEBCO data - 2014 dataset, 30 arc-sec
GEBCO <- raster("C:/Users/renef/Dropbox/UMaine/Code/GEBCO/GEBCO_2014_2d.nc")
#read in MaskedGEBCO data 
MaskedGEBCO <- raster("C:/Users/renef/Dropbox/UMaine/Code/GEBCO/MaskedGEBCO.nc")

#read in EarthByte Roughness database - rotates from original
EarthByteRotate <- raster("C:/Users/renef/Dropbox/UMaine/Code/EarthbyteData/roughness_100km.15.2_rotated.nc")


################################################
###### Calculate Covariants and Create DF ######
################################################

##### Create Dataframe to mutate
Covariant_df <- WDPA[,c("WDPAID", "STATUS_YR", "REP_AREA", "IUCN_CAT", "geom")]



#extract geometry from GEBCO database, calculate mean depth & mean roughness, add to covariant_df
#extract geometry from EarthByte Roughness database, calculate mean roughness <- NEED TO FIX
#correct for that the README says values in mGals, multipled by 100

Covariant_df <- Covariant_df %>%
  dplyr::mutate(Mean_depth = extract(GEBCO, WDPA, fun = mean),
                Mean_roughness = extract(EarthByteRotate, WDPA, fun = mean)/100)


#retrieve maxlat, minlat, maxlon, minlon, calculate mean lat and mean long, add to covariant_df

Covariant_df <- Covariant_df %>%
  dplyr::mutate(as.data.frame(t(sapply(1:nrow(WDPA), function(i) as.vector(extent(WDPA[i,]))))),
                mean_lon = (V1+V2)/2,
                mean_lat = (V3+V4)/2)


#### Calculate minimum distance from edge of MPA to land ####


#Calculate time points for each MPA, add to covariant_df

Covariant_df <- Covariant_df %>% 
  dplyr::filter(STATUS_YR > 0) %>% 
  dplyr::mutate(age = 2021 - STATUS_YR,
                prior20 = STATUS_YR - 20,
                after10 = STATUS_YR + 10,
                after20 = STATUS_YR + 20,
                after40 = STATUS_YR + 40,
                after60 = STATUS_YR + 60,
                after80 = STATUS_YR + 80,
                after100 = STATUS_YR + 100,
                after120 = STATUS_YR + 120) %>%
  dplyr::relocate(geom, .after = after120)



#Recode ICUN to be numberic instead of character?


