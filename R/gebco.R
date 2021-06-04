library(terra)
library(raster)

#' Retrieve a SpatRaster object of GEBCO 2014 (bathymetry) data
#' 
#' @param filename character, the *local* filename - path different for everyone
#' @return \code{\link[terra]{SpatRaster}}
read_gebco <- function(
  filename = "GEBCO_2014_2D.nc"){
    
  terra::rast(filename)
}


#' Retrieve a SpatRaster object of GEBCO 2014 (bathymetry) slope data
#' 
#' @param filename character, the *local* filename - path is different for everyone
#' @return \code{\link[terra]{SpatRaster}}
read_gebco_slope <- function(
  filename = "GEBCO_2014_2D_slope.tif"){
    
  terra::rast(filename)
}


#' Compute gebco (bathymetry) slope 
#' 
#' @param x bathymetry data as a \code{\link[terra]{SpatRaster}}
#' @param ... other arguments for \code{\link[terra]{terrain}}
#' @return \code{\link[terra]{SpatRaster}} of slope
gebco_slope <- function(x = read_gebco(), ...){
  raster::terrain(as(x, "RasterLayer"), ...)
}

