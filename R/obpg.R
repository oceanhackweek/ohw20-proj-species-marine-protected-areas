library(terra)

#' Read in the the stack of OBPG mission-length climatologies (9km)
#'
#' @param filename the name of the file
#' @return \code{\link[terra]{SpatRaster}} with layers for chlor_a, par, pic, poc and sst
read_obpg_annual_climatology <- function(
  filename = "obpg_clim_world_9km_2002-2020.tif"){
  
  terra::rast(filename)
}

#' Read in the the stack of OBPG mission-length climatologies (9km)
#'
#' @param filename the name of the file
#' @return \code{\link[terra]{SpatRaster}} with layers for chlor_a, par, pic, poc and sst
read_obpg_seasonal_climatology <- function(
  filename = c(
    "chlor_a" = "obpg_seasonclim_world_chlor_a_9km_2002-2020.tif",
    "par" = "obpg_seasonclim_world_par_9km_2002-2020.tif",
    "pic" = "obpg_seasonclim_world_pic_9km_2002-2020.tif",
    "poc" = "obpg_seasonclim_world_poc_9km_2002-2020.tif",
    "sst" = "obpg_seasonclim_world_sst_9km_2002-2020.tif"){
  
  if (length(filename) > 1){
    x <- sapply(filename, terra::rast, simplify = FALSE)
  } else {
    x <- terra::rast(filename)
  }
  x
}

