# Clear memory
rm(list=ls())

# Set directory
setwd("/Users/kerihartman/Dropbox/Hertie/Thesis/Data")

# Load libraries
library(readxl)
library(tidyverse)
library(plyr)
library(stringr)
library(countrycode)

# Load pop data
load("PopData.rda")

# Create age categories
dat[["minor"]] <- 0
for (i in 0:13) {
  dat[["minor"]] <- dat[["minor"]] + dat[[paste("age", i, sep ="")]]
}

dat[["juv"]] <- 0
for (i in 14:17) {
  dat[["juv"]] <- dat[["juv"]] + dat[[paste("age", i, sep ="")]]
}

dat[["yngadlt"]] <- 0
for (i in 18:24) {
  dat[["yngadlt"]] <- dat[["yngadlt"]] + dat[[paste("age", i, sep ="")]]
}

dat[["age25.34"]] <- 0
for (i in 25:34) {
  dat[["age25.34"]] <- dat[["age25.34"]] + dat[[paste("age", i, sep ="")]]
}

dat[["age35.44"]] <- 0
for (i in 35:44) {
  dat[["age35.44"]] <- dat[["age35.44"]] + dat[[paste("age", i, sep ="")]]
}

dat[["age45.54"]] <- 0
for (i in 45:54) {
  dat[["age45.54"]] <- dat[["age45.54"]] + dat[[paste("age", i, sep ="")]]
}

dat[["age55.64"]] <- 0
for (i in 55:64) {
  dat[["age55.64"]] <- dat[["age55.64"]] + dat[[paste("age", i, sep ="")]]
}

dat[["retired"]] <- 0
for (i in 65:95) {
  dat[["retired"]] <- dat[["retired"]] + dat[[paste("age", i, sep ="")]]
}

# Create labor market access vars
dat$full_lma <- dat$EU + dat$asylum + dat$family + dat$work + dat$befreit + dat$otherbefristet + dat$longterm
dat$part_lma <- dat$asylumseeker + dat$duldung + dat$educ
dat$no_lma <- dat$pending + dat$undoc

# Combine mar status vars
dat$married <- dat$verheiratet + dat$Lebenspartnerschaft
dat$widow <- dat$verwitwet + dat$Lebenspartner.verstorben
dat$divorced <- dat$geschieden + dat$Lebenspartnerschaft.aufgehoben
dat$single <- dat$ledig


save(dat, file = "PopData2.Rda")

