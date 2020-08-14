library(dplyr)
library(sf)

# the server doesn't like this request
# library(wdpar)
#x <- wdpa_fetch("USA")



#' Download a copy of the 2020 mpa database from NOAA
#' 
#' @seealso https://marineprotectedareas.noaa.gov/dataanalysis/mpainventory/
#' @param url character, the URL of the MPA dataset
#' @param dest_dir the destination directory where we save the data
#' @return integer, 0 if successful
download_noaa_mpa_2020 <- function(url = "https://marineprotectedareas.noaa.gov/media/data/NOAA_MPAI_2020_IUCN_gdb.zip",
                              dest_dir = "."){
  
  ofile <- file.path(dest_dir,basename(url))
  # not so big, but slooow server
  ok <- download.file(url, ofile, mode = 'wb')
  if (ok == 0){
    file_list <- unzip(ofile,
                       exdir = sub(".zip", "", ofile))
  } else {
    stop("download of file failed")
  }
  ok
}

#' Read the NOAA 2020 mpa dataset
#' 
#' @param path the path to the data directory
#' @param site_id character, one or more site identifiers, aet to "all" to get all
#' @return sf class object
read_noaa_mpa_2020 <- function(path = normalizePath("~/NOAA_MPAI_2020_IUCN_gdb/NOAA_MPAI_v2020.gdb"),
                          site_id = c("MA15", "MA16", "MA17")){
  x <- sf::read_sf(path) %>%
    sf::st_transform(crs = st_crs(4326))
  xy_limits <- st_bbox_by_feature(x)
  x <- dplyr::bind_cols(x, xy_limits)
  if (!("all" %in% site_id)) x <- x %>% dplyr::filter(Site_ID %in% site_id)
  x
}


#' Download a copy of the 2014 mpa database from NOAA - but it is just points not polygons
#'
#' @seealso  https://catalog.data.gov/dataset/u-s-marine-protected-areas-boundaries-mpa-inventory
#' @param url character, the URL of the MPA dataset
#' @param dest_dir the destination directory where we save the data
#' @return integer, 0 if successful
download_noaa_mpa_2014 <- function(url = "https://nmsmarineprotectedareas.blob.core.windows.net/marineprotectedareas-prod/media/archive/pdf/helpful-resources/inventory/mpa_inventory_2014_public_shp.zip",
                              dest_dir = "."){
  
  ofile <- file.path(dest_dir,basename(url))
    
  # not so big, but slooow server
  ok <- download.file(url, ofile)
  if (ok == 0){
    file_list <- unzip(ofile, 
                       exdir = sub(".zip", "", ofile))
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
read_noaa_mpa_2014 <- function(path = normalizePath("~/mpa_inventory_2014_public_shp"),
                          site_id = c("MA15", "MA16", "MA17")){
  x <- sf::read_sf(path)  
  xy_limits <- st_bbox_by_feature(x)
  x <- dplyr::bind_cols(x, xy_limits)
  if (!("all" %in% site_id)) x <- x %>% dplyr::filter(Site_ID %in% site_id)
  x
}


#' Read the NOAA mpa dataset for a specified year - this wraps around read_noaa_mpa_2020() and read_noaa_mpa_2014
#' 
#' @param year numeric or 4-digit string of the year to read
#' @param ... further arguments passed to read_noaa_mpa_2020() or read_noaa_mpa_2014
#'        In particular look for path and site_id
#' @return sf class object
read_noaa_mpa <- function(year = c("2014", "2020")[2], ...){
  switch(as.character(year[1]),
         "2014" = read_noaa_mpa_2014(...),
         "2020" = read_noaa_mpa_2020(...),
         stop("year not known:", year[1]))
}


#' Given a simple feature data frame, compute the bounding box coordinates
#' for each feature.
#'
#' @seealso https://github.com/r-spatial/sf/issues/1179
#'
#' @param x data frame of simple features
#' @return data frame (tibble) of xmin, ymin, xmax, and ymax 
st_bbox_by_feature = function(x) {
  x <- sf::st_geometry(x)
  get_bb_as_vector <- function(y) {
    as.vector(sf::st_bbox(y))
  }
  z <- do.call(rbind, lapply(x, get_bb_as_vector))
  colnames(z) <- c("xmin", "ymin", "xmax", "ymax")
  dplyr::as_tibble(z)
}


#' Clip a set of features (polygons in this case) to those fully enclosed by the prescribe
#' bounding box.
#' 
#' @param x a data frame of simple features
#' @param bb numeric, a named vector with bounding box coordinates (xmin, ymin, xmax, ymax) in any order
#' @return a data frame of the filtered feature set - possible with no features
clip_noaa_mpa <- function(x = read_noaa_mpa(), bb = c(xmin = -72, xmax = -63, ymin = 39, ymax = 46)){
  x %>%
    dplyr::filter(xmin >= bb[1] & xmax <= bb[2] & ymin >= bb[3] & ymax <= bb[4])
}