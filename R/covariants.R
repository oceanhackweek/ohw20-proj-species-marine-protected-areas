## Code to extract co-variant data for MPAS
## Jan 15, 2020
##Updated Feb 11, 2021 - JE and RF

library(sf)
library(dplyr)
library(wdpar)
library(terra)
library(raster)
library(ncdf4)
#source("setup.R") do not need with new googledrive setup
source("wdpar-package.R")
source("google-filestream.R")

# Run wdpa_read_country function from wdpar-package.R 
#only accept those that are completely marine

WDPA <- wdpa_read_country("Iceland") %>%
  dplyr::arrange(desc(REP_AREA)) %>% #organize by largest to smallest
  dplyr::filter(MARINE >= 1) 

#read in GEBCO data - 2014 dataset, 30 arc-sec
GEBCO <- raster("C:/Users/renef/Dropbox/UMaine/Code/GEBCO/GEBCO_2014_2d.nc")
#read in MaskedGEBCO data 
MaskedGEBCO <- raster("C:/Users/renef/Dropbox/UMaine/Code/GEBCO/MaskedGEBCO.nc")


################################################
###### Calculate Covariants and Create DF ######
################################################

#extract geometry from GEBCO database, calculate mean depth & mean slope, add to covariant_df
#calculate slope for entire GEBCO raster... if correct, this could be saved as a file so it doesnt need to be remade each time
Allterrain <- raster::terrain(GEBCO, opt="slope", unit = "degrees", neighbors=8)


#' Augments a sf table with bpunding box min, max and mean for x and y
#' @param x single row sf table of POLYGON or MULTIPOLYGON
#' @param key ignored
#' @return the input table but with added xmin, xmax, ymin, ymax, xmean, ymean variables added
compute_extent <- function(x, key){
  b <- sf::st_bbox(x) %>% as.numeric
  x %>%
    dplyr::mutate(xmin = b[1],
                  xmax = b[3],
                  ymin = b[2],
                  ymax = b[4],
                  xmean = mean(b[c(1,3)]),
                  ymean = mean(b[c(2,4)]))
}

##### Create Dataframe to mutate
#Covariant_df <- WDPA[,c("WDPAID", "STATUS_YR", "REP_AREA", "IUCN_CAT", "geom")]

this_year <- as.numeric(format(Sys.Date(), "%Y"))

# trim WDPA to just the variables we want
# compute mean depth and slope per mpa (multipolygon)
# iterate over each mpa (row) and compute the extent of the bbox
# bind the output back together
# filter just the ones with a known status year
# compute ages and benchmark dates
# move geom to the end (prettier)
# save to the google drive with a timestamped file

Covariant_df <- WDPA %>%
  dplyr::select(WDPAID, STATUS_YR, REP_AREA, IUCN_CAT) %>%
  dplyr::mutate(Mean_depth = extract(GEBCO, WDPA, fun = mean)[,1],
                Mean_slope = extract(Allterrain, WDPA, fun = mean)[,1]) %>%
  dplyr::rowwise() %>%
  compute_extent() %>%
  dplyr::bind_rows() %>%
  dplyr::filter(STATUS_YR > 0) %>% 
  dplyr::mutate(age = this_year - STATUS_YR,
                prior20 = STATUS_YR - 20,
                after10 = STATUS_YR + 10,
                after20 = STATUS_YR + 20,
                after40 = STATUS_YR + 40,
                after60 = STATUS_YR + 60,
                after80 = STATUS_YR + 80,
                after100 = STATUS_YR + 100,
                after120 = STATUS_YR + 120) %>%
  dplyr::relocate(geom, .after = after120) %>%
  gd_write_sf(sprintf("%s-covar-Iceland.gpkg", format(Sys.Date(), "%Y-%m-%d")))


#retrieve maxlat, minlat, maxlon, minlon, calculate mean lat and mean long, add to covariant_df

#Covariant_df <- Covariant_df %>%
#  dplyr::mutate(as.data.frame(t(sapply(1:nrow(WDPA), function(i) as.vector(extent(WDPA[i,]))))),
#                mean_lon = (V1+V2)/2,
#                mean_lat = (V3+V4)/2) %>%
#  dplyr::rename(xmin = V1, xmax = V2, ymin = V3, ymax = V4)


#### Calculate minimum distance from edge of MPA to land ####


#Calculate time points for each MPA, add to covariant_df

#Covariant_df <- Covariant_df %>% 
#  dplyr::filter(STATUS_YR > 0) %>% 
#  dplyr::mutate(age = 2021 - STATUS_YR,
#                prior20 = STATUS_YR - 20,
#                after10 = STATUS_YR + 10,
#                after20 = STATUS_YR + 20,
#                after40 = STATUS_YR + 40,
#                after60 = STATUS_YR + 60,
#                after80 = STATUS_YR + 80,
#                after100 = STATUS_YR + 100,
#                after120 = STATUS_YR + 120) %>%
#  dplyr::relocate(geom, .after = after120)



#Recode ICUN to be numberic instead of character?


