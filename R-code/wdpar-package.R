# README
#
# Adaptation of https://cran.r-project.org/web/packages/wdpar/vignettes/wdpar.html

# be sure you have moved to the Code/R directory first

# I don't think we need this anymore
# source("setup.R")

# but we do need these
source("R-code/google-filestream.R")
require(wdpar)

# note table of accepted countries
# https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3#Officially_assigned_code_elements


#' Fetch WDPA data by country.  Be patient with this - it can take a while.
#' 
#' You may get a warning about the "GDAL Error 1: ..." but it doens't seem to be fatal and 
#' the download is unaffected.
#' 
#' @param country character, either accepted country name or ISO-3 code
#' @param ... other arguments for wdpar::wdpa_fetch()
#' @return sf table of protected areas
#' @examples
#' \dontrun{
#' x <- wdpa_fetch_country(country = 'Cuba')
#' plot((x %>% dplyr::filter(MARINE == "1"))['REP_AREA'])
#' }
wdpa_fetch_country <- function(country = 'Chile',
                                ...){
  x <- wdpar::wdpa_fetch(country[1], wait = TRUE, download_dir = tempdir(), force_download = TRUE, ...) %>%
    wdpa_write_country(country = country[1], overwrite = TRUE)
  return(x)
}


#' Write a WDPA country level spatial dataset to geopackage format. 
#' 
#' @param x a simple features table
#' @param country character, name of the country to write
#' @param path character, the path to save to dataset
#' @param team character, the name of the Google team
#' @param ext, character, the file extension, by default we write Geopackage, "*.gpkg"
#' @param overwrite logical, if TRUE allow existing fiels to overwrite
#' @return the input table
wdpa_write_country <- function(x, country = "Genovia",
                               path = "Data/wdpa",
                               team = "ohw-obis-mpa",
                               ext = ".gpkg",
                               overwrite = FALSE){
  filename <- paste0(country[1], ext[1])
  gpath <- googledrive::drive_get(file.path(path[1], country[1]), team = team[1])
  if (nrow(gpath) == 0){
    gpath <- googledrive::drive_mkdir(country,
                                      path = googledrive::drive_get(path[1],team = team[1]))
  }
  y <- gd_write_sf(x, filename, path = gpath, overwrite = overwrite)
  x
}

#' Read a previously downloaded WDPA dataset for a country
#' 
#' @param country character, the name of the country
#' @param team character, the name of the team drive
#' @param ext character, the file extension, by default we look for GeoPackage or ".gpkg"
#' @return a simple features table
#' @examples
#' \dontrun{
#' x <- wdpa_read_country("Cuba")
#' plot(x['REP_AREA'])
#' }
wdpa_read_country <- function(country = 'Cuba',
                              team = "ohw-obis-mpa",
                              ext = ".gpkg"){
  filename <- paste0(country, ext)
  x <- gd_read_sf(filename, team = team)
  return(x)
}

#' Read a previously downloaded WDPA global dataset from a geodatabase. 
#' iThe database has 16991 unique WPDA_ID 
#' 
#' NOTE: this is a direct read from a mounted Google Drive to the ohw-obis-mpa 
#' project directory. IT's a very large database (not huge, but big) and reading 
#' indirectly via http to Google Drive is waaay too slow.  If the data has not been 
#' synced to your local drive the first read maybe slowish.
#' 
#' @param path character, path to the Google Drive ohw-obis-mpa directory
#' @param name character, the name of the database
#' @param ext character, the file extension, by default we look for ".gdb"
#' @param filestream logical if TRUE then use googlefilestream
#' @param team character, the name of the Google team
#' @return a simple features table
#' @examples
#' \dontrun{
#' x <- wdpa_read_global(path = "~/Google Drive/Shared drives/ohw-obis-mpa")
#' y <- x %>% dplyr::filter(ISO3 == "MEX") %>% dplyr::slice_max(REP_M_AREA, n = 10)
#' plot(y['STATUS_YR'])
#' }
wdpa_read_global <- function(path = "~/Google Drive/Shared drives/ohw-obis-mpa",
                             name = "WDPA_WDOECM_Apr2021_Public_marine",
                             filestream = FALSE,
                             ext = ifelse(filestream, ".gpkg", ".gdb"),
                             team = "ohw-obis-mpa"){
  
  
  if (filestream){
    filename <- sprintf("%s.%s", name[1], ext[1])
    x <- gd_read_sf(filename, team = team)
  } else {
    
    if (!dir.exists(path[1])) stop("path not found:", path[1])
    dbpath <- file.path(path, "Data", "wdpa", "global", 
                          name[1],
                          sprintf("%s%s", name[1], ext[1]))
    if (!dir.exists(dbpath)) stop("database path not found:", dbpath)
    x <- suppressWarnings(sf::read_sf(dbpath))
    return(x)
  }
}



#' Generate a sf object of random points within and around a set of polygons.
#' 
#' Polygons are expanded by \code{buffer}, and then samples are drawn from
#' those.  See \code{\link[sf]{st_buffer}}
#' 
#' @param x sf object of polygons
#' @param buffer
#' @param n integer, the number of points
#' @return a sf object of n points
random_points <- function(x = wdpa_read_country(),
                          buffer = 0.1,
                          n = 1000){
  
  pts <- suppressMessages(suppressWarnings(sf::st_buffer(x, dist = buffer) %>%
                            sf::st_sample(size = n)))
  return(pts)
}

#' Given a set of MPAs and observations, 
#' determine which observations belong to which MPA
#'
#' @param mpa sf object of MPA polygons
#' @param obs sf object of observation points
#' @param ... other arguments for \code{\link[sf{st_intersects}}
#' @return a list of match vectors by index. If obs has 1000 points
#'   then a 1000 length list of integer vectors is returned where each
#'   vector has zero or more integers indicating which mpa polygon each 
#'   belongs to. For example, the first 3 points below do not intersect
#'   any polygons, while the 4th intersects with 3 polygons and the 
#'   the 5th intersects with just one.
#'    $ : int(0) 
#'    $ : int(0) 
#'    $ : int(0) 
#'    $ : int [1:3] 42 71 177
#'    $ : int 40
mpa_match <- function(mpa = wdpa_read_country(country = "Iceland"),
                      obs = random_points(mpa),
                      ...){
  
  z <- sf::st_intersects(obs, mpa, ...)
  return(z)
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

random_points_example <- function(){
  
  mpa <- wdpa_read_country("Cuba")
  pts <- random_points(mpa, n = 500, buffer = 0.1)
  
  inout <- lengths(mpa_match(mpa, pts)) > 0
  
    cols <- c("orange", "green")[as.numeric(inout)+1]
  pchs <- c("x", "+")[as.numeric(inout)+1]
  plot(sf::st_geometry(mpa), col = "transparent")
  plot(sf::st_geometry(pts), col = cols, pch = pchs, add = TRUE)
  
  # throws error on pts
  # Error in UseMethod("metaData") : 
  #  no applicable method for 'metaData' applied to an object of class "c('sfc_POINT', 'sfc')"
  # require(leaflet)
  # pal <- colorFactor(c("orange", "green"), domain = c("FALSE", "TRUE"))
  # leaflet::leaflet(data = mpa) %>%
  #   leaflet::addTiles() %>%
  #   leaflet::addPolygons() %>%
  #   leaflet::addCircleMarkers(data = pts,
  #                             radius = 6,
  #                             stroke = FALSE,
  #                             opacity = 0.6,
  #                             color = ~pal(as.character(inout)))
  
}



