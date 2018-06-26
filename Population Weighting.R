# Clear memory
rm(list=ls())

# Set directory
setwd("/Users/kerihartman/Dropbox/Hertie/Thesis/Data")

# Load libraries
library(tidyverse)
library(plyr)
library(stringr)
library(plm)
library(stargazer)
library(naniar)

# Load data
load("CompleteData.rda")


# Drop cases for no total pop data
fulldf <- fulldf[!is.na(fulldf$totalpop),]
fulldf <- fulldf[fulldf$totalpop != 0,]

# Define vars
vars <- names(fulldf)
keep <- c("totalpop", "region", "nat", "year")
vars <- vars[!vars %in% keep]

for (i in 1:length(vars)) {
  fulldf[vars[i]] <- fulldf[vars[i]] / fulldf$totalpop
}



# Create logged DVs
fulldf$arsincrtot <- log(fulldf$crtotal + sqrt(fulldf$crtotal^2+1))
fulldf$arsinduld <- log(fulldf$duldung + sqrt(fulldf$duldung^2+1))
fulldf$arsinasyl <- log(fulldf$asylum + sqrt(fulldf$asylum^2+1))
fulldf$arsinskr <- log(fulldf$asylumseeker + sqrt(fulldf$asylumseeker^2+1))
fulldf$arsinvio <- log(fulldf$violent + sqrt(fulldf$violent^2+1))
fulldf$arsintheft <- log(fulldf$theft + sqrt(fulldf$theft^2+1))
fulldf$arsindrug <- log(fulldf$drug + sqrt(fulldf$drug^2+1))

save(fulldf, file = "WtdData.rda")


# Determine which category to use for BL

apply(X = cbind(fulldf$Baden.Württemberg, fulldf$Bayern, fulldf$Brandenburg, fulldf$Bremen, fulldf$Hamburg,
              fulldf$Hessen, fulldf$Mecklenburg.Vorpommern, fulldf$Niedersachsen, fulldf$Nordrhein.Westfalen, fulldf$Rheinland.Pfalz,
              fulldf$Saarland, fulldf$Sachsen, fulldf$Berlin, fulldf$Sachsen.Anhalt, fulldf$Schleswig.Holstein, fulldf$Thüringen, fulldf$stateNA), 2, FUN = median)

apply(X = cbind(fulldf$single, fulldf$married, fulldf$widow, fulldf$msunknown, fulldf$divorced, fulldf$marNA), 2, FUN = median)

apply(X = cbind(fulldf$res0, fulldf$res1.3, fulldf$res4.5, fulldf$res6.7, fulldf$res8.9, fulldf$res10.14,
                fulldf$res15.19, fulldf$res20.24, fulldf$res25.29, fulldf$res30.34, fulldf$res34.39, fulldf$res40plus, fulldf$resNA), 2, FUN = median)

apply(X = cbind(fulldf$männlich, fulldf$weiblich, fulldf$sexNA), 2, FUN = median)

apply(X = cbind(fulldf$minor, fulldf$juv, fulldf$yngadlt, fulldf$age25.34, fulldf$age35.44, fulldf$age45.54,
                fulldf$age55.64, fulldf$retired, fulldf$ageNA), 2, FUN = median)


apply(X = cbind(fulldf$bornDE, fulldf$firstgen, fulldf$genNA), 2, FUN = median)
