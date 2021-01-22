# README
#
# Adaptation of https://cran.r-project.org/web/packages/wdpar/vignettes/wdpar.html

# be sure you have moved to the Code/R directory first
source("setup.R")

library(sf)
library(wdpar)
library(dplyr)

# note table of accepted countries
# https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3#Officially_assigned_code_elements


#' Fetch WDPA data by country.  Be patient with this - it can take a while.
#' 
#' You may get a warning about the "GDAL Error 1: ..." but it doens't seem to be fatal and 
#' the download is unaffected.
#' 
#' @param country character, either accepted country name or ISO-3 code
#' @param output_dir character, the output directory path (created if it doesn't exist)
#'   Within this directory two items are written - the shapefile downloaded
#'   and a FlatGeoBuf file (simple features table)
#' @param ... other arguments for wdpar::wdpa_fetch()
#' @return sf table of protected areas
#' @examples
#' \dontrun{
#' x <- wdpa_fetch_country(country = 'Cuba')
#' plot((x %>% dplyr::filter(MARINE == "1"))['REP_AREA'])
#' }
wdpa_fetch_country <- function(country = 'Chile',
                                output_dir = get_path("Data", "wdpa", country[1]),
                                ...){
  if (!dir.exists(output_dir)){
    stopifnot(dir.create(output_dir, recursive = TRUE))
  }
  x <- wdpar::wdpa_fetch(country[1], wait = TRUE, download_dir = output_dir, force_download = TRUE, ...) %>%
    wdpa_write_country(country = country[1], overwrite = TRUE)
  return(x)
}


#' Write a WDPA country level spatial dataset to geopackage format. 
#' 
#' @param x a simple features table
#' @param country character, name of the country to write
#' @param path character, the path to save to dataset
#' @param ext, character, the file extension, by default we write Geopackage, "*.gpkg"
#' @param overwrite logical, if TRUE allow existing fiels to overwrite
#' @return the input table
wdpa_write_country <- function(x, country = "Genovia",
                               path = get_path("Data", "wdpa", country[1]),
                               ext = ".gpkg",
                               overwrite = FALSE){
  filename <- file.path(path, paste0(country[1], ext[1]))
  # for reasons I don't understand, write_sf has been returning a data.frame
  # not a sf object
  y <- sf::write_sf(x, dsn = filename, delete_dsn = overwrite)
  x
}


#' Read a previously downloaded WDPA dataset for a country
#' 
#' @param country character, the name of the country
#' @param path character, the path to the country
#' @param ext character, the file extension, by default we look for GeoPackage or ".gpkg"
#' @return a simple features table
#' @examples
#' \dontrun{
#' x <- wdpa_read_country("Cuba")
#' plot(x['REP_AREA'])
#' }
wdpa_read_country <- function(country = 'Cuba',
                              path = get_path("Data", "wdpa", country[1]),
                              ext = ".gpkg"){
  
    filename <- file.path(path, paste0(country[1], ext[1]))
    stopifnot(file.exists(filename))
    sf::read_sf(filename)
}


iceland_example <- function(){
  require(leaflet)
  x <- wdpa_read_country("Iceland") %>%
    dplyr::arrange(desc(REP_AREA))
  leaflet::leaflet(data = x %>%
            dplyr::slice(3)) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons()
}
