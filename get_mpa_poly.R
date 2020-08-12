library(dplyr)
library(sf)

# the server doesn't like this request
# library(wdpar)
#x <- wdpa_fetch("USA")



#' Download a copy of the mpa database from NOAA
#' 
#' @seealso https://marineprotectedareas.noaa.gov/dataanalysis/mpainventory/
#' @param url character, the URL of the MPA dataset
#' @param dest_dir the destination directory where we save the data
#' @return integer, 0 if successful
download_noaa_mpa <- function(url = "https://marineprotectedareas.noaa.gov/media/data/NOAA_MPAI_2020_IUCN_gdb.zip",
                              dest_dir = "."){
  
  # not so big, but slooow server
  ok <- download.file(url, 
                      file.path(dst_dir, basename(url)))
  if (ok == 0){
    file_list <- unzip(basename(url))
  } else {
    stop("download of file failed")
  }
  ok
}

#' Read the NOAA mpa dataset
#' 
#' @param path the path to the data directory
#' @param site_id character, one or more site identifiers, aet to "all" to get all
#' @return sf class object
read_noaa_mpa <- function(path = "NOAA_MPAI_v2020.gdb",
                          site_id = c("MA15", "MA16", "MA17")){
  x <- sf::read_sf(path)
  if (!("all" %in% site_id)) x <- x %>% dplyr::filter(Site_ID %in% site_id)
  x
}


#' Download a copy of the mpa database from NOAA - but it is just points not polygons
#'
#' @seealso  https://catalog.data.gov/dataset/u-s-marine-protected-areas-boundaries-mpa-inventory
#' @param url character, the URL of the MPA dataset
#' @param dest_dir the destination directory where we save the data
#' @return integer, 0 if successful
download_noaa_inv <- function(url = "https://nmsmarineprotectedareas.blob.core.windows.net/marineprotectedareas-prod/media/archive/pdf/helpful-resources/inventory/mpa_inventory_2014_public_shp.zip",
                              dest_dir = "."){
  
  # not so big, but slooow server
  ok <- download.file(url, 
                      file.path(dst_dir, basename(url)))
  if (ok == 0){
    file_list <- unzip(basename(url), 
                       exdir = sub(".zip", "", basename(url)))
  } else {
    stop("download of file failed")
  }
  ok
}

#' Read the NOAA mpa inventory dataset - points not polygons?
#' 
#' @param path the path to the data directory
#' @param site_id character, one or more site identifiers, aet to "all" to get all
#' @return sf class object
read_noaa_inv <- function(path = "mpa_inventory_2014_public_shp",
                          site_id = c("MA15", "MA16", "MA17")){
  x <- sf::read_sf(path)
  if (!("all" %in% site_id)) x <- x %>% dplyr::filter(Site_ID %in% site_id)
  x
}

