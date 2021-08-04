# This provides function to assemble features per MPA (global) for...
# bathymetry (mean/var depth, mea/var slope)
# obpg (climatological mean/var for sst, par, pic, poc, and chlor_a)
# geometry (centroid, distance form shore, distance to neighbors)

library(sf)
library(dplyr)
#library(stars)

source("wdpar-package.R")
source("gebco.R")
source("obpg.R")

#' Extract GEBCO depth and slope info per MPA
#' @param x sf table of global (or any other dataset) mpa
#' @param bathy terra::SpatRaster of bathymetry
#' @param slope terra::SpatRaster of bathymetry slope
#' @param ofile optional output filename
#' @return tibble of WDPA and Gebco data per MPA
extract_bathymetry <- function(
  x = wdpa_read_global(),
  bathy = read_gebco(),
  slope = read_gebco_slope(),
  ofile = NA){
  
  if (FALSE){ # for development - only runs if manually runs
    PATH <- "/Volumes/GoogleDrive/Shared\ drives/ohw-obis-mpa/Data"
    x = wdpa_read_global() %>%
      sf::st_transform("+proj=eqc")
    bathy = read_gebco(path = PATH,
                       form = 'SpatRaster')
    terra::crs(bathy) <- as.character(terra::crs(x))
    slope = read_gebco_slope(path = PATH,
                             form = "SpatRaster")
    terra::crs(slope) <- as.character(terra::crs(x))
    ofile <- file.path(PATH, "wdpa_gebco.csv.gz")
  }
  
    z <- x %>% 
      sf::st_drop_geometry()
    vx <- x %>%  as("Spatial") %>% vect() 
    y <- terra::extract(bathy, vx,
                 function(x){
                  c(mean(x, na.rm = T), var(x, na.rm = TRUE))
                 })
    z <- z %>%
      dplyr::mutate(gebco_depth_mean = y$bathy.1, 
                    gebco_depth_var = y$bathy.2)
    y <- terra::extract(slope, vx,
                 function(x){
                   c(mean(x, na.rm = T), var(x, na.rm = TRUE))
                 })
    z <- z %>% 
      dplyr::mutate(gebco_slope_mean = y$bathy.1, 
                    gebco_slope_var = y$bathy.2)
    
    if (!is.na(ofile)){
      readr::write_csv(z, ofile)
    }
    
    z
}

#' Extract OBPG features per MPA
#' @param x sf table of global (or any other dataset)
#' @param annual terra::SpatRaster of OBPG annual climatology
#' @param seasonal terra::SpatRaster of OBPG seasaonal climatology
#' @param ofile optional output filename
#' @return tibble of WDPA and Gebco data per MPA
extract_obpg_annual <- function(
  x = wdpa_read_global(),
  annual = read_obpg_annual_climatology(),
  seasonal = read_obpg_seasonal_climatology(),
  ofile = NA){
  
  if (FALSE){ # for development - only runs if manually runs
    PATH <- "/Volumes/GoogleDrive/Shared\ drives/ohw-obis-mpa/Data"
    x = wdpa_read_global() #%>%
      #sf::st_transform("+proj=eqc")
    annual = read_obpg_annual_climatology(path = PATH, form = "SpatRaster")
    #terra::crs(annual) <- as.character(terra::crs(x))
    seasonal = read_obpg_seasonal_climatology(path = PATH, form = "SpatRaster")
    ofile = file.path(PATH, "wdpa_obpg.csv.gz")
  }

  names(annual) <- paste0("AC_", names(annual))
  
  
  seasonal = sapply(names(seasonal),
                    function(nm) {
                      names(seasonal[[nm]]) <- paste0(names(seasonal[[nm]]), "_", nm)
                      seasonal[[nm]]
                    }, simplify = FALSE)
  
  z <- x %>% 
    sf::st_drop_geometry()
  vx <- x %>%  as("Spatial") %>% vect() 
  
  y <- terra::extract(annual, vx, 
    function(x){
      c(mean = mean(x, na.rm = T), var = var(x, na.rm = TRUE))
    }) %>%
    dplyr::select(-ID)
  
  z <- z %>%
    dplyr::bind_cols(y)
  
  y <- lapply(seasonal,
              function(season, vx = NULL){
                terra::extract(season, vx, 
                      function(x){
                        c(mean = mean(x, na.rm = T), var = var(x, na.rm = TRUE))
                      }) %>%
                dplyr::select(-ID)
              }, vx = vx) %>%
    dplyr::bind_cols() %>%
    dplyt::as_tibble()
  
  z <- z %>%
    dplyr::bind_cols(y)
  
  if (!is.na(ofile)){
    readr::write_csv(z, ofile)
  }
  
  z
  
}


#' Read the Natural Earth Coastline
#' 
#' @seealso https://www.naturalearthdata.com/
#' @param scale character, one of small, medium or large
#' @return sf tibble of coastline polygons
read_coastline <- function(scale = c('small', 'medium', 'large')[1]){
  rnaturalearth::ne_coastline(scale = scale, returnclass = 'sf')
}


#' Extract location information for MPAs: centroid, distance to shore
#' 
#' @param x sf table of global (or any other dataset) mpa
#' @return tibbe of WPDAIS info, centoid location and distance to shore
geolocation <- function(x = wdpa_read_global() %>%
                              sf::st_transform("+proj=eqc"),
                        coast = read_coastline(scale = 10) %>%
                             sf::st_transform("+proj=eqc")){
  if (FALSE){
    x = wdpa_read_global() %>%
      sf::st_transform("+proj=eqc")
    
    coast = read_coastline() %>%
      sf::st_transform("+proj=eqc")
    
    PATH <- "/Volumes/GoogleDrive/Shared\ drives/ohw-obis-mpa/Data"
    bathy = read_gebco(path = PATH,
                               form = 'SpatRaster') %>%
     terra::project(as.character(terra::crs(x)))
    mask <- bathy >= 0
    vbathy <- as.contour(mask)
    
  }
  centroid = sf::st_centroid(x)
  cxy = sf::st_coordinates(centroid) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(cX = "X", cY = "Y")
  
  vcentroid <- centroid %>% as("Spatial") %>% vect()
  dist <- terra::distance(vcentroid, vbathy)
  
  
  
  
  
  #foo
  
  
  
  
  
}