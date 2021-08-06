source("R-code/google-filestream.R")
source("R-code/wdpar-package.R")

#' Ensure that a set of OBIS observations actually fall within a set of MPAs
#' 
#' @param obs tibble of OBIS observations
#' @param mpa sf MULTIPOLYGON object of MPAs form WDPA
#' @param form character one of 
#' \itemize{
#' \item{tibble return a tibble}
#' \item{sf return an sf object (spatially referenced tibble)}
#' \item{index a list of indices that provide the match between obs and mpas}
#' }
#' @return tibble of obs with possibly fewer rows
#' @examples 
#' \dontrun{
#'   mpa = wdpa_read_country("Belgium")
#'   obs = read_obis_country("Belgium")
#'   obs_strict <- obis_strict_match(obs, mpa)
#'   dim(obs)
#'   # [1] 511490    161
#'   dim(obs_strict)
#'   # [1] 480302    161
#' }
obis_strict_match <- function(obs = read_obis_country("Belgium"),
                              mpa = wdpa_read_country("Belgium"),
                              form = c("tibble", "sf", "index")[1]){
  
  obsf <- obis_as_sf(obs, crs = sf::st_crs(mpa) )
  ix <- mpa_match(mpa, obsf)
  
  switch(tolower(form[1]),
         "tibble" = obs %>% dplyr::filter(lengths(ix) > 0),
         "sf"     =  obsf %>% dplyr::filter(lengths(ix) > 0),
         ix) # anything else
}



#' Cast an OBIS data frame as a simple feature (POINT)
#' 
#' @param x tibble of OBIS data
#' @param ... other arguments for \code{\link[sf]{st_as_sf}} - note \code{crs} argument
#' @return sf object (POINT)
obis_as_sf <- function(x, ...){
  sf::st_as_sf(x, coords = c("decimalLongitude", "decimalLatitude"), ...)
}


#' Given a simple features tibble (POLYGON or MUTLIPOLYGON), retrieve OBIS observation records.
#'
#' @param x POLYGON or MULTIPOLYGON sf object such as for a country
#' @param policy character either "convex hull" or "strict"
#'    We fetch the convex hull to simplify the fetch, but that may allow some observations that are outside MPAs.
#'    If "convex hull" then we allow these to be returned.  If "strict" then we filter for those observations that are in
#'    the MPAs (in mean within or on the boundary).
#' @return tibble of OBIS observations, possibly empty
obis_fetch_mpa <- function(x = read_wdpa_country("Belgium"),
                           policy = c("convex hull", "strict")[1]) {
  
  
  # Fetch one row (one MPA) 
  # @param x a single-row sf POLYGON or MULTIPOLYGON object
  # @param key, used by dplyr to store the value of the gouping variable for this iteration but ignored here
  # @return NULL (upon error) or tibble of OBIS results
  get_one_mpa <- function(x, key){
    r <- try(robis::occurrence(geometry = sf::st_as_text(sf::st_convex_hull(sf::st_geometry(x)))))
    if (inherits(r, "try-error")){
      warning("error fetching obis datafir WDPAID:", x$WDPAID[1])
      r <- NULL
    }
    r
  }
  
  r <- x %>%
    dplyr::rowwise() %>%
    dplyr::group_map(get_one_mpa,
                     .keep = TRUE) %>%
    dplyr::bind_rows()

  if (tolower(policy[1]) == "strict"){
    r <- obis_strict_match(x, r)
  }
  
  r
}

#' Fetch OBIS data by country
#' 
#' No error checking is done to ensure the country folderand WDPA file
#' 
#' @param country character
#' @param save_file logical, if TRUE save into the country's data folder
#' @param ... other arguments for obis_fetch_mpa
#' @return tibble of OBIS observations, possibly empty
obis_fetch_country <- function(country = "Genovia",
                               save_file = TRUE,
                               ...){
  
  x <- wdpa_read_country(country = country[1]) %>%
    obis_fetch_mpa(...) 
  
  if (save_file)  x <- write_obis_country(x, country = country[1])
  
  x
}

#' Read an OBIS country file
#' 
#' @param country character
#' @param team charcater, the name of the team drive
#' @return data table
read_obis_country <- function(country = "Genovia",
                              team = "ohw-obis-mpa"){
  gd_read_csv(filename = sprintf("%s_obis.csv.gz", country, team = team))
}

#' Write an OBIS CSV file to the googe drive
#' 
#' No error checking is done to ensure the country folder exists
#' 
#' @param x OBIS table of data
#' @param country character 
#' @param team charcater, the name of the team drive
#' @return the input data table
write_obis_country <- function(x,
                           country = "Genovia",
                           team = "ohw-obis-mpa"){
  
  gd_write_csv(x, 
               filename = sprintf("%s_obis.csv.gz", country[1]), 
               path = googledrive::drive_get(file.path("Data","wdpa",country[1]) , team = "ohw-obis-mpa"))
}

