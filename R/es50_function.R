

#' Function to calculate ES50 for one MPA over all available data; then if possible, across a vector of timepoints (ages)
#' function and variable names need to be more clear
#' small script below calculate_es50() demonstrates usage
#' es50 calculation at each timepoint does not function yet; only returns a test of whether or not the calculation can be done
#' mutating es50 calculation for each age must be made dynamic; currently hard wired
#' 
#' @param mpa a subset row from a WDPA query - represents one mpa
#' @param key grouping variable - WDPAID
#' @param ages vector of ages
#' @return a one row tibble (representing one mpa) with mutated columns for each es50 calculation
#' 
#' 
calculate_es50 <- function(mpa, key, ages) {
  
  # need to add argument to either calculate cumulatively or binned for each age
  # using ages 20 and 40 for example, calculate es50 from 0-20, 20-40; or 0-20, 0-40
  es50 <- function(age, mpa) {
    
    if (mpa$STATUS_YR + age > 2021) {
      return(NA)
    } 
    
    #filter occurence records for bin calculation
    if (age == 0) {
      age_year <- mpa$STATUS_YR + 9
      
      age_records <- species_occurence %>% 
        dplyr::filter(dplyr::between(.data$date_year, mpa$STATUS_YR, age_year))
    } else {
      bin_start <- mpa$STATUS_YR + age
      
      bin_end <- bin_start + 9
      
      age_records <- species_occurence %>% 
        dplyr::filter(dplyr::between(.data$date_year, bin_start, bin_end))
    }
    
    #compute es50 for filtered occurrence records by bin
    if (nrow(age_records) > 0) {
      species_counts <- aggregate(age_records$Count, by=list(Category=age_records$scientificName), FUN=sum)
      
      species_count_20 <- nrow(species_counts)
      
      if (nrow(age_records) >= 50) {
        es_50 <- vegan::rarefy(species_counts$x, 50)
        
        return(es_50)
      }
      else {
        return(NA)
      }
    }
    
  }
  
  hull = st_convex_hull(mpa$geom)
  text = st_as_text(hull)
  
  #added distinct() to check for duplicates
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
      dplyr::mutate(species_count_all = nrow(species_count), .before=.data$geom)
    
    if (nrow(species_count) >= 50){
      total_es50 = vegan::rarefy(species_count$x,50)
      
      mpa <- mpa %>% 
        dplyr::mutate(total_es50 = total_es50, .before=geom)
    }
    
    if ("date_year" %in% colnames(species_occurence)) {
      
      es50_by_age <- sapply(ages, es50, mpa)
      
      mpa <- mpa %>% 
        dplyr::mutate(age_0     = es50_by_age[1],
                      age_10    = es50_by_age[2],
                      age_20    = es50_by_age[3],
                      age_30    = es50_by_age[4],
                      age_40    = es50_by_age[5],
                      age_50    = es50_by_age[6],
                      age_60    = es50_by_age[7],
                      .before=geom)
    }
  }
  return(mpa)
}



#Example script to use calculate_es50() for all mpa records from a country
country = "USA"

#we could dynamically find a list of ages by looking at a summary of the year column after reading the WDPA or in the occurrence records...
ages <- c(0, 10, 20, 30, 40, 50, 60)

WDPA <- wdpa_read_country(country, team = "ohw-obis-mpa", ext = ".gpkg") %>%
  dplyr::filter(MARINE > 0)

x <- WDPA %>% 
  dplyr::select(WDPAID, 
                NAME, 
                IUCN_CAT,
                REP_AREA,
                REP_M_AREA, 
                STATUS, 
                STATUS_YR, 
                geom) %>% 
  dplyr::mutate(WKT = st_as_text(geom)) %>% 
  dplyr::group_by(WDPAID) %>% 
  dplyr::group_map(calculate_es50, ages, .keep=TRUE) #%>% 
  dplyr::bind_rows()





