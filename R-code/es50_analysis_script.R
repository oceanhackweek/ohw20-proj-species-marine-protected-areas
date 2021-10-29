

library(dplyr)
library(robis)
library(vegan)
library(wdpar)


source("es50_function.R")
source("wdpar-package.R")



#Example script to use calculate_es50() for all mpa records from a country
country = "Belgium"

#we could dynamically find a list of ages by looking at a summary of the year column after reading the WDPA or in the occurrence records...
ages <- c(-20, 0, 20)

wdpa <- wdpa_read_country(country, team = "ohw-obis-mpa", ext = ".gpkg") 

wdpa <- wdpa %>%
  dplyr::filter(MARINE > 0)

x <- wdpa %>% 
  dplyr::select(WDPAID, 
                NAME, 
                IUCN_CAT,
                REP_AREA,
                REP_M_AREA, 
                STATUS, 
                STATUS_YR, 
                geom) %>% 
  dplyr::filter(sapply(.data$geom, function(x) inherits(x, "MULTIPOLYGON"))) %>% 
  dplyr::group_by(WDPAID) %>% 
  dplyr::group_map(calculate_es50, ages, .keep=TRUE) %>% 
  dplyr::bind_rows()




### make an example single mpa to refine es50 function
mpa <- wdpa %>% 
  dplyr::filter(WDPAID == 67809) %>% 
  dplyr::select(WDPAID, 
                NAME, 
                IUCN_CAT,
                REP_AREA,
                REP_M_AREA, 
                STATUS, 
                STATUS_YR, 
                geom)
