# Load libraries
library(robis, quietly = TRUE)
library(ohwobpg, quietly = TRUE) # Ocean color procesing group
library(dplyr, quietly = TRUE)
library(biomod2, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(raster, quietly = TRUE)
library(viridis, quietly = TRUE)
source("Scripts/do_fetch.R")

## Create a directory to download satelite data to plug into marine protected areas (mpa)

path <- "/home/user/Escritorio/TIOJORGE/CONGRESOS_PRESENTACIONES/2020/OceanHawk/Scripts/mpa" # It has to be automatized to function or directory

## If first time create a specific folder. If not, go directly to read_database
dir.create(path, recursive = TRUE, showWarnings = TRUE)

## Identify current project
ID<- "Mustelus"

## Define spatiotemporal boundaries
minLat<- -40
maxLat<- -30
minLon<- -60
maxLon<- -50
LatLon<-c(minLat,maxLat, minLon, maxLon)

TimeStart<- as.Date("2016-01-01")
TimeEnd<- as.Date("2017-01-01")
By_time<- "month" # can be either day or other useful in seq

TimeFrame<- seq(TimeStart, TimeEnd, By_time)

## Set satellite grain
# low resolution for space savings
RES <- "9km" # 4km ## Spatial resolution
period = "MO" # "8D" "DAY" # Time period MO- Monthly, 8 days, 1 day
                     
# from https://github.com/BigelowLab/ohwobpg/blob/master/inst/scripts/fetch_ohw_obpg.R
# now we grab the files for each parameter.  The function returns a small database
# (as a data frame) that identifies the files downloaded.

m_sst <- do_fetch(TimeFrame,
                  bb = LatLon,
                  path = path,
                  param = "sst",
                  res = RES,
                  period = period)

m_chl <- do_fetch(TimeFrame,
                  bb = LatLon,
                  path = path,
                  param = "chlor_a",
                  res = RES,
                  period = period)

m_par <-  do_fetch(TimeFrame,
                   bb = LatLon,
                   path = path,
                   param = "par",
                   res = RES,
                   period = period)

## Save database

db <- list.files(path, pattern = glob2rx("*.tif"), full.names = TRUE, recursive = TRUE) %>%
  as_database() %>%
  write_database(path,filename=paste(ID,".csv.gz", sep=""))# salva la base de datos como csv
  
db$date<-as.Date(db$date)

## read_database After created
db <- read_database(path, filename=paste(ID,".csv.gz", sep="")) # Just to double check date here is OK, but in first option is chr!
colnames(db)

# SEPARO CADA VARIABLE
db_sst<- db %>% 
       dplyr::filter(
       param == "sst"  & 
       per   == period &
       res   == RES)

db_chloa<- db %>% 
       dplyr::filter(
       param == "chlor_a"  & 
       per   == period     &
       res   == RES)

db_par<- db %>% 
       dplyr::filter(
       param == "par"  & 
       per   == period     &
       res   == RES)

## GENERO STCKS CON CADA VARIABLE       
sst_stack<-raster::stack(as_filename(db_sst,path))
chloa_stack<-raster::stack(as_filename(db_chloa,path))
par_stack<-raster::stack(as_filename(db_par,path))
