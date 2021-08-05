source("R-code/google-filestream.R")
source("R-code/wdpar-package.R")

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
      warning("error fetching obis data")
      print(r)
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
    # refilter to just those captured by the polygons - no outliers
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
