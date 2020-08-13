# This script shows how we created the small database of OBPG by fetching one year
# of monthly low resolution data for sst, chlor_a and par.  We also include one
# month of daily sst for the same region and resolution.

library(ohwobpg)
library(dplyr)

#' Fetch one or more dates of a particular OBPG product specification
#'
#' @param dates Date-class, vector of one or more dates
#' @param bb numeric, a 4-element bounding box in [west, east, south, north] order
#' @param param character, the name of the parameter
#' @param period character, the name of the period
#' @param res character, the name of the resolution
#' @param path character, the output storage path
#' @return a data frame database
do_fetch <- function(dates,
                     bb = c(-180, 180, -90, 90),
                     param = c("sst", "chlor_a")[1],
                     period = c("MO", "8D", "DAY")[1],
                     res = c("9km", "4km")[1],
                     path = "."){

  # determine which suite the data belongs to
  suite <- switch(param[1],
    "sst" = "SST",
    "chlor_a" = "CHL",
    "par" = "PAR",
    "pic" = "PIC",
    "poc" = "POC",
    toupper(param[1])) # this last one is a guess and might not work

  # build the URLs
  urls <- ohwobpg::obpg_build_url(
    dates = dates,
    param = param[1],
    suite = suite,
    period = period[1],
    res = res[1])

  # create a navigation list
  nc1 <- ohwobpg::obpg_open(urls[1])
  nav <- ohwobpg::obpg_nc_nav(nc1,
                              bb = bb,
                              res = ohwobpg::obpg_res(what = res[1]),
                              varname = param[1])
  ohwobpg::obpg_close(nc1)

  # grab the files
  for (this_url in urls){
    new_data <- ohwobpg::obpg_fetch(this_url, nav, outpath = path)
  }

  return(ohwobpg::as_database(urls))
}
