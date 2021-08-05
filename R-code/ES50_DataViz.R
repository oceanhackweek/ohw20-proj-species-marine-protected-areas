
#load libraries
library(dplyr)
library(tibble)
library(tidyverse)
library(ggplot2)
library(scales)

#set working directory
setwd('I:/Shared drives/ohw-obis-mpa/Updates/')

#load data frame
Country = "Cuba"
ES50_df = read.csv(paste(Country,'_ES50_df.csv',sep=""))

#data visualization
Phylum <- aggregate(SpeciesOccurence$Count, by=list(Category=SpeciesOccurence$phylum),FUN=sum)
bp<- ggplot(Phylum, aes(x="", y=x, fill=Category))+
  geom_bar(width = 1, stat = "identity")
pie = bp + coord_polar("y", start=0)
pie + scale_fill_brewer("Blues") +
  theme(axis.text.x = element_blank())