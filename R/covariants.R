## Code to extract co-variant data for MPAS
## Jan 15, 2020

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
#read in MaskedGEBCO data (see below for creation of dataset)
MaskedGEBCO <- raster("C:/Users/renef/Dropbox/UMaine/Code/GEBCO/MaskedGEBCO.nc")
#mask GEBCO - Ran on 1/18/2021
#saved as MaskedGEBCO for future use
#GEBCO_land_1 <- GEBCO
#GEBCO_land_1[GEBCO_land_1 > 0] <- 1
#maskedGEBCO <- mask(GEBCO_land_1, GEBCO_land_1, inverse = TRUE, maskvalue = 1, updatevalue = 0)

#to make it such that land = NA and sea = 0:
#MaskedGEBCO_NA <- mask(MaskedGEBCO, MaskedGEBCO, maskvalue = 1)
#to make it such that sea = NA and land = 1:
#MaskedGEBCO_NA_SEA <- mask(MaskedGEBCO, MaskedGEBCO, maskvalue = 0)


#read in EarthByte Roughness database - rotates from original
EarthByteRotate <- raster("C:/Users/renef/Dropbox/UMaine/Code/EarthbyteData/roughness_100km.15.2_rotated.nc")
#read in original EarthByte roughness data
#EarthByte <- raster("C:/Users/renef/Dropbox/UMaine/Code/EarthbyteData/roughness_100km.15.2.nc", varname = "z")
#EarthByteRotate <- rotate(EarthByte)

#######

#extract geometry from GEBCO database, calculate mean depth
Mean_depth <- extract(GEBCO, WDPA, fun = mean)

#extract geometry from EarthByte Roughness database, calculate mean roughness
#correct for that the README says values in mGals, multipled by 100
Mean_roughness <- (extract(EarthByteRotate, WDPA, fun = mean)/100)


#retrieve maxlat, minlat, maxlon, minlon
MPA_ext <- as.data.frame(t(sapply(1:nrow(WDPA), function(i) as.vector(extent(WDPA[i,])))))
colnames(MPA_ext) <- c('xmin', 'xmax', 'ymin', 'ymax')

#calculate mean lat and mean lon, add to MPA_ext dataframe
MPA_ext <- MPA_ext %>% 
            mutate(mean_lon = (xmin+xmax)/2) %>% 
            mutate(mean_lat = (ymin+ymax)/2)

#Calculate minimum distance from edge of MPA to land




#Create dataframe with Covariants
Covariant_df <- WDPA[,c("WDPAID", "STATUS_YR", "REP_AREA", "IUCN_CAT", "geom")]
Covariant_df <- cbind(Covariant_df, Mean_depth, MPA_ext, Mean_roughness)

