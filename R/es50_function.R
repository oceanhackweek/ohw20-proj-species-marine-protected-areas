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
calculate_es50 <- function(mpa, 
                           key, 
                           ages) {
  
  # need to add argument to either calculate cumulatively or binned for each age
  # using ages 20 and 40 for example, calculate es50 from 0-20, 20-40; or 0-20, 0-40
  es50 <- function(mpa, age) {
    
    if (mpa$STATUS_YR + age > 2021) {
      cat(paste(mpa$WDPAID, "too young, age:", age, "\n", sep=" "))

      mpa <- mpa %>% 
        dplyr::mutate("age_{age}" := NA, .before=.data$geom)
      
      return(mpa)
    } 
    
    #filter occurrence records for bin calculation
    if (age == 0) {
      
      age_year <- mpa$STATUS_YR + 19
      
      age_records <- species_occurence %>% 
        dplyr::filter(dplyr::between(.data$date_year, mpa$STATUS_YR, age_year))
      
    } else {
      bin_start <- mpa$STATUS_YR + age
      
      bin_end <- bin_start + 19
      
      age_records <- species_occurence %>% 
        dplyr::filter(dplyr::between(.data$date_year, bin_start, bin_end))
    }
    
    #compute es50 for filtered occurrence records by bin
    if (nrow(age_records) > 0) {
      species_counts <- aggregate(age_records$Count, 
                                  by=list(Category=age_records$scientificName), 
                                  FUN=sum)
      
      phylum_counts <- aggregate(age_records$Count, 
                                by=list(Category=age_records$phylum), 
                                FUN=sum)
      
      #can be used to store the number of unique species
      species_count <- nrow(species_counts)
      
      if (nrow(species_counts) >= 50) {
        cat(paste(mpa$WDPAID, "calculating es50, age:", age, "\n", sep=" "))
        
        es_50 <- vegan::rarefy(species_counts$x, 50)
        
        mpa <- mpa %>% 
          dplyr::mutate("age_{age}"         := es_50,
                        "species_age_{age}" := nrow(species_counts),
                        "phylum_age_{age}"   := nrow(phylum_counts),
                        "records_age_{age}" := sum(species_counts$x),
                        .before=.data$geom)
        
        return(mpa)
      }
      else {
        cat(paste(mpa$WDPAID, "Less than 50 species records, age:", age, "\n", sep=" "))
        
        mpa <- mpa %>% 
          dplyr::mutate("age_{age}" := NA, 
                        .before=.data$geom)
      }
    } else {
      mpa <- mpa %>% 
        dplyr::mutate("age_{age}" := NA, 
                      .before=.data$geom)
    }
    return(mpa)
  }
  
  #hull = st_convex_hull(mpa$geom)
  #text = st_as_text(hull)
  
  #added distinct() to check for duplicates
  species_occurence = try(occurrence(geometry = st_as_text(st_convex_hull(mpa$geom))))
  
  cat("\n")
  
  if (inherits(species_occurence, "try-error")) {
    species_occurence <- NULL
    cat("occurence fetching try-error", "\n")
  }
  
  if (!is_empty(species_occurence)) {
    if ("individualCount" %in% colnames(species_occurence)) {
  
      species_occurence$individualCount <- as.numeric(species_occurence$individualCount)
      species_occurence$individualCount[is.na(species_occurence$individualCount)] <- 1
      species_occurence$Count <- 1 * species_occurence$individualCount
      
    } else {
      species_occurence$Count <- 1
    }
    
    species_count <- aggregate(species_occurence$Count, 
                               by=list(Category=species_occurence$scientificName),
                               FUN=sum)
    
    phylum_count <- aggregate(species_occurence$Count,
                              by=list(Category=species_occurence$phylum),
                              FUN=sum)
    
    mpa <- mpa %>% 
      dplyr::mutate(species_count_all = nrow(species_count),
                    phylum_count_all = nrow(phylum_count),
                    records_all = sum(species_count$x),
                    .before=.data$geom)
    
    if (nrow(species_count) >= 50){
      total_es50 = vegan::rarefy(species_count$x,50)
      
      mpa <- mpa %>% 
        dplyr::mutate(total_es50 = total_es50, 
                      .before=.data$geom)
    }
    
    records_before <- species_occurence %>% 
      dplyr::filter(.data$date_year < mpa$STATUS_YR)
    
    species_count_before <- aggregate(records_before$Count, 
                                      by=list(Category=records_before$scientificName),
                                      FUN=sum)
    
    phylum_count_before <- aggregate(records_before$Count, 
                                     by=list(Category=records_before$phylum),
                                     FUN=sum)
    
    n_records_before <- sum(species_count_before$x)
    
    mpa <- mpa %>% 
      dplyr::mutate(species_count_before = nrow(species_count_before),
                    phylum_count_before = nrow(phylum_count_before),
                    records_before = n_records_before,
                    .before=.data$geom)
    
    if (nrow(species_count_before) >= 50) {
      es50_before <- vegan::rarefy(species_count_before, 50)
      
      mpa <- mpa %>% 
        dplyr::mutate(es50_before = es50_before,
                      .before=.data$geom)
    }
    
    records_after <- species_occurence %>% 
      dplyr::filter(.data$date_year >= mpa$STATUS_YR)
    
    species_count_after <- aggregate(records_after$Count, 
                                     by=list(Category=records_after$scientificName),
                                     FUN=sum)
    
    phylum_count_after <- aggregate(records_after$Count, 
                                    by=list(Category=records_after$phylum),
                                    FUN=sum)
    
    n_records_after <- sum(species_count_after$x)
    
    mpa <- mpa %>% 
      dplyr::mutate(species_count_after = nrow(species_count_after),
                    phylum_count_after = nrow(phylum_count_after),
                    records_after = n_records_after,
                    .before=.data$geom)
    
    if (nrow(species_count_after) >= 50) {
      es50_after <- vegan::rarefy(species_count_after$x, 50)
      
      mpa <- mpa %>% 
        dplyr::mutate(es50_after = es50_after,
                      .before=.data$geom)
    }
    
    if ("date_year" %in% colnames(species_occurence)) {

      for (age in ages) {
        mpa <- mpa %>% 
          es50(age)
      }
    }
  } else {
    cat(paste(mpa$WDPAID, "No species records", "\n", sep=" "))
  }
  
  return(mpa)
}



#Example script to use calculate_es50() for all mpa records from a country
country = "USA"

#we could dynamically find a list of ages by looking at a summary of the year column after reading the WDPA or in the occurrence records...
ages <- c(-40, -20, 0, 20, 40, 60, 80)

WDPA <- wdpa_read_country(country, team = "ohw-obis-mpa", ext = ".gpkg") %>%
  dplyr::filter(MARINE > 0)

wdpa <- head(WDPA, n=4)

x <- wdpa %>% 
  dplyr::select(WDPAID, 
                NAME, 
                IUCN_CAT,
                REP_AREA,
                REP_M_AREA, 
                STATUS, 
                STATUS_YR, 
                geom) %>% 
  dplyr::group_by(WDPAID) %>% 
  dplyr::group_map(calculate_es50, ages, .keep=TRUE) %>% 
  dplyr::bind_rows()


mpa <- WDPA %>% 
  dplyr::filter(WDPAID == 342) %>% 
  dplyr::select(WDPAID, 
                NAME, 
                IUCN_CAT,
                REP_AREA,
                REP_M_AREA, 
                STATUS, 
                STATUS_YR, 
                geom)
