library(terra)
library(raster)
library(stars)
library(magrittr) # for pipe

#' Retrieve a SpatRaster object of GEBCO 2014 (bathymetry) data
#' 
#' @param filename character, the *local* filename - path different for everyone
#' @param path character, the filepath
#' @param form character, specifies the class to return - "SpatRaster" (terra package) or 
#'        "stars" (stars package)
#' @return \code{\link[terra]{SpatRaster}}
read_gebco <- function(
  filename = "GEBCO_2014_2D.nc",
  path = ".",
  form = c("SpatRaster", "stars")[2]){
  
  filename <- file.path(path[1], filename[1])
  reader_fun <- switch(tolower(form[1]),
                       "spatraster" = terra::rast,
                       "stars" = stars::read_stars,
                       stop("unknown form:", form[1]))
  
  x <- reader_fun(filename)
  if (tolower(form[1]) == "stars") sf::st_crs(x) <- "+proj=eqc"
  setNames(x, "bathy")
}


#' Retrieve a SpatRaster object of GEBCO 2014 (bathymetry) slope data
#' 
#' @param filename character, the *local* filename - path is different for everyone
#' @param path character, the filepath
#' @param form character, specifies the class to return - "SpatRaster" (terra package) or 
#'        "stars" (stars package)
#' @return \code{\link[terra]{SpatRaster}}
read_gebco_slope <- function(
  filename = "GEBCO_2014_2D_slope.tif",
  path = ".",
  form = c("SpatRaster", "stars")[2]){
  
  filename <- file.path(path[1], filename[1])
  reader_fun <- switch(tolower(form[1]),
                       "spatraster" = terra::rast,
                       "stars" = stars::read_stars,
                       stop("unknown form:", form[1]))
  
  x <- reader_fun(filename)
  if (tolower(form[1]) == "stars") sf::st_crs(x) <- "+proj=eqc"
  setNames(x, "slope")
}


#' Compute gebco (bathymetry) slope 
#' 
#' @param x bathymetry data as a \code{\link[terra]{SpatRaster}}
#' @param ... other arguments for \code{\link[terra]{terrain}}
#' @return \code{\link[terra]{SpatRaster}} of slope
gebco_slope <- function(x = read_gebco(), ...){
  raster::terrain(as(x, "RasterLayer"), ...)
}

