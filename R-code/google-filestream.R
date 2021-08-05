library(dplyr)
library(sf)
library(googledrive)
library(readr)

#' Read a file on Google Drive
#' 
#' Inspired by and adpated from \href{https://github.com/tiernanmartin/miscgis/blob/master/R/drive_read.R#L15}{miscgis}
#' 
#' @param x dribble - see \code{\link[googledrive]{drive_get}}
#' @param path character or NULL, if character provide the full path specification
#'   for saving the file.  If NULL the file is not saved to disk.
#' @param read_fun the unquoted function name to read the file with
#' @param ... other arguments for \code{read_fun}
#' @return varies with \code{read_fun}
drive_read <- function (x, path = NULL, read_fun, ...){
  
  d <- googledrive::drive_get(id = googledrive::as_id(x %>% dplyr::slice(1)))
  
  do_temp <- is.null(path[1])
  if (do_temp){
    filename <- tempfile()
  } else {
    filename <- path
  }
  
  d %>% googledrive::drive_download(path = filename, overwrite = TRUE)
  
  result <- suppressMessages(suppressWarnings(read_fun(filename[1], ...)))
  
  if(do_temp){
    unlink(filename, recursive = TRUE)
  }
  
  invisible(result)
}

#' Read a geospatial file on Google Drive using \code{\link[sf]{read_sf}}
#' 
#' 
#' @param filename character, the name of the file
#' @param team character, the name of the team drive
#' @param ... other arguments for \code{\link[sf]{read_sf}}
#' @return simple features table (tibble)
gd_read_sf <- function(filename = "CovariantExample.gpkg", team = "ohw-obis-mpa", ...){
  x <- googledrive::drive_find(filename, shared_drive = team) %>%
    drive_read(read_fun = sf::read_sf, ...)
  return(x)
}

#' Write a spatial file to Google Drive
#' 
#' @param x sf spatial object
#' @param filename the name to attach to the file
#' @param path dribble destination online directory description as per \code{\link[googledrive](drive_get)}
#' @param overwrite logical, by default it is an error to overwrite
#' @param ... other other arguments for \code{\link[sf]{write_sf}}
#' @return the input \code{x}
gd_write_sf <- function(x, filename = "test.gpkg",  
                        path = googledrive::drive_get("Data/wdpa/", team = "ohw-obis-mpa"),
                        overwrite = FALSE,
                        ...){
  
  temp_file <- file.path(tempdir(check = TRUE), filename[1])
  ok <- sf::write_sf(x, temp_file, ...)
  
  r <- googledrive::drive_upload(media = temp_file,
        path = path,
        name = filename,
        overwrite = overwrite)
  unlink(temp_file)
  return(x)
}

#' Read a CSV file on Google Drive using \code{\link[readr]{read_csv}}
#' 
#' 
#' @param filename character, the name of the file
#' @param team character, the name of the team drive
#' @param ... other arguments for \code{\link[readr]{read_csv}}
#' @return table (tibble)
gd_read_csv <- function(filename = "WDPA_WDOECM_marine_csv.csv", team = "ohw-obis-mpa", ...){
  x <- googledrive::drive_find(filename, shared_drive = team) %>%
    drive_read(read_fun = readr::read_csv, ...)
  return(x)
}

#' Write a CSV file to Google Drive
#' 
#' @param x data frame of tibble
#' @param filename the name to attach to the file
#' @param path dribble destination online directory description as per \code{\link[googledrive](drive_get)}
#' @param overwrite logical, by default it is an error to overwrite
#' @param ... other other arguments for \code{\link[readr]{write_csv}}
#' @return the inut data frame
gd_write_csv <- function(x, filename = "test.csv",  
                        path = googledrive::drive_get("Data", team = "ohw-obis-mpa"),
                        overwrite = FALSE,
                        ...){
  
  temp_file <- file.path(tempdir(check = TRUE), filename[1])
  ok <- readr::write_csv(x, temp_file, ...)
  
  r <- googledrive::drive_upload(media = temp_file,
                                 path = path,
                                 name = filename,
                                 overwrite = overwrite)
  unlink(temp_file)
  return(x)
}


gpkg_example <- function(){
  require(leaflet)
  x <- gd_read_sf("Iceland.gpkg") %>%
    dplyr::arrange(desc(REP_AREA))
  leaflet::leaflet(data = x %>%
                     dplyr::slice(3)) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons()
}

csv_example <- function(){
  # read a CSV file
  x <- gd_read_csv("WDPA_WDOECM_marine_csv.csv")
  
  # tabulate IUCN_CAT by MARINE designation
  # and write as CSV - note we specify the path separate from the filename
  # and path is relative to the team drive
  y <- x %>%
    dplyr::count(MARINE, IUCN_CAT) %>%
    gd_write_csv(filename = "WDPA_WDOECM_marine_tally.csv",
                 path = googledrive::drive_get("Data/wdpa/", team = "ohw-obis-mpa"))
  
}