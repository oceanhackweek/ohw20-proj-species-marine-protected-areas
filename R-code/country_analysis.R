

#' Retrieve records of all marine protected areas in a country, then calculate es50 in each mpa at various timepoints
#' 
#' @param country character string name of country
#' @param ages vector of ages (numeric)
#' @return table with all mpas for country and calculated es50 for all possible ages
#' 
#' 
country_analysis <- function(country, ages) {
  
  calculate_es50 <- function(mpa, key, ages) {
    
    es50 <- function(age, tbl) {
      
      if (tbl$STATUS_YR + age > 2021) {
        return("doesn't work")
      } else {
        return("works")
      }
    }
    
    hull = st_convex_hull(mpa$geom)
    text = st_as_text(hull)
    
    species_occurence = try(occurrence(geometry = text))
    
    if (inherits(species_occurence, "try-error")) {
      species_occurence <- NULL
      cat("occurence fetching try-error")
    }
    
    if (!is_empty(species_occurence)) {
      if ("individualCount" %in% colnames(species_occurence)) {
        
        species_occurence$individualCount <- as.numeric(species_occurence$individualCount)
        species_occurence$individualCount[is.na(species_occurence$individualCount)] <- 1
        species_occurence$Count <- 1 * species_occurence$individualCount
        
      } else {
        species_occurence$Count <- 1
      }
      
      species_count <- aggregate(species_occurence$Count, by=list(Category=species_occurence$scientificName),FUN=sum)
      
      mpa <- mpa %>% 
        dplyr::mutate(species_count_all = nrow(species_count), .before=geom)
      
      if (nrow(species_count) >= 50){
        total_es50 = vegan::rarefy(species_count$x,50)
        
        mpa <- mpa %>% 
          dplyr::mutate(total_es50 = total_es50, .before=geom)
      }
      
      es50_by_age <- sapply(ages, es50, mpa)
      
      mpa <- mpa %>% 
        dplyr::mutate(before_10 = es50_by_age[1],
                      age_0     = es50_by_age[2],
                      age_10    = es50_by_age[3],
                      age_20    = es50_by_age[4],
                      age_40    = es50_by_age[5],
                      .before=geom)
    }
    
    return(mpa)
  }
  
  WDPA <- wdpa_read_country(country, team = "ohw-obis-mpa", ext = ".gpkg") %>%
    dplyr::filter(MARINE > 0)
  
  x <- WDPA %>% 
    dplyr::select(WDPAID, 
                  NAME, 
                  IUCN_CAT, 
                  REP_M_AREA, 
                  STATUS, 
                  STATUS_YR, 
                  geom) %>% 
    dplyr::mutate(WKT = st_as_text(geom)) %>% 
    dplyr::group_by(WDPAID) %>% 
    dplyr::group_map(calculate_es50, ages, .keep=TRUE) %>% 
    dplyr::bind_rows()
  
  return(x)
}