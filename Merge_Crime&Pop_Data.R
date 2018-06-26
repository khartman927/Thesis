# Clear memory
rm(list=ls())

# Set directory
setwd("/Users/kerihartman/Dropbox/Hertie/Thesis/Data")
  
# Load libraries
library(tidyverse)
library(plyr)
library(stringr)

# Load pop and crime datasets
load("CrimeData.rda") #df
load("Popdata2.rda") #dat

# Merge pop and crime datasets
fulldf <- merge(dat, df, by = c("nat", "year"), all = TRUE)

save(fulldf, file = "CompleteData.Rda")