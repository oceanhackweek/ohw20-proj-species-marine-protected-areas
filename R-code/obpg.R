library(terra)
library(stars)
library(magrittr) # for pipe

#' Read in the the stack of OBPG mission-length climatologies (9km)
#'
#' @param filename the name of the file
#' @param path character, the filepathstr(dist)
#' @param form character, specifies the class to return - "SpatRaster" (terra package) or 
#'        "stars" (stars package)
#' @return \code{\link[terra]{SpatRaster}} with layers for chlor_a, par, pic, poc and sst
read_obpg_annual_climatology <- function(
  filename = "obpg_clim_world_9km_2002-2020.tif",
  path = ".",
  form = c("SpatRaster", "stars")[2]){
  
  filename <- file.path(path[1], filename[1])
  
  reader_fun <- switch(tolower(form[1]),
                       "spatraster" = terra::rast,
                       "stars" = stars::read_stars,
                       stop("unknown form:", form[1]))
  
  reader_fun(filename)
}

#' Read in the the stack of OBPG mission-length climatologies (9km)
#'
#' @param filename the name of the file
#' @param path character, the filepath
#' @param form character, specifies the class to return - "SpatRaster" (terra package) or 
#'        "stars" (stars package)
#' @return \code{\link[terra]{SpatRaster}} with layers for chlor_a, par, pic, poc and sst
read_obpg_seasonal_climatology <- function(
  filename = c(
    "chlor_a" = "obpg_seasonclim_world_chlor_a_9km_2002-2020.tif",
    "par" = "obpg_seasonclim_world_par_9km_2002-2020.tif",
    "pic" = "obpg_seasonclim_world_pic_9km_2002-2020.tif",
    "poc" = "obpg_seasonclim_world_poc_9km_2002-2020.tif",
    "sst" = "obpg_seasonclim_world_sst_9km_2002-2020.tif"),
  path = ".",
  form = c("SpatRaster", "stars")[2]){
  files <- file.path(path[1], filename)
  
  reader_fun <- switch(tolower(form[1]),
                       "spatraster" = terra::rast,
                       "stars" = stars::read_stars,
                       stop("unknown form:", form[1]))
  
  if (length(files) > 1){
    x <- sapply(files, 
                 function(f) {
                   reader_fun(f)
                 },
               simplify = FALSE) %>%
      setNames(names(filename))
  } else {
    x <- reader_fun(files)
  }
  x
}


