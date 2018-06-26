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
library(car)
library(lmtest)
library(estimatr)

# Load data
load("WtdData.rda")

# Add total pop

tot_skr <- plm(arsincrtot ~ arsinskr + factor(year) + minor + totalpop +
                 Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                 Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                 Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                 weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                 age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                 res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
               data = fulldf[fulldf$year != "2012",], model = 'within', index =c('nat', 'year'))

tot_asyl <- plm(arsincrtot ~ arsinasyl + factor(year) + minor + totalpop +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012",], model = 'within', index =c('nat', 'year'))

tot_duld <- plm(arsincrtot ~ arsinduld + factor(year) + minor + totalpop +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012",], model = 'within', index =c('nat', 'year'))

# Reg wts

# Create regression weights - separate for each year
years <- c("2012", "2013", "2014", "2015", "2016")
popyears <- NULL

for (i in 1:length(years)) {
  popyears[i] <- sum(fulldf$totalpop[fulldf$year == years[i]], na.rm = TRUE)
  fulldf$weight[fulldf$year == years[i]] <-fulldf$totalpop[fulldf$year == years[i]] /  popyears[i]
}

wt_skr <- plm(arsincrtot ~arsinskr + factor(year) + minor +
                 Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                 Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                 Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                 weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                 age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                 res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
               data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', weights = weight, index =c('nat', 'year'))

wt_asyl <- plm(arsincrtot ~ arsinasyl + factor(year) + minor +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', weights = weight, index =c('nat', 'year'))

wt_duld <- plm(arsincrtot ~ arsinduld + factor(year) + minor +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', weights = weight, index =c('nat', 'year'))

# Levels

# Load data
load("CompleteData.rda")

## Fixed effects

fulldf <- fulldf[!is.na(fulldf$totalpop),]
fulldf <- fulldf[fulldf$totalpop != 0,]

# Delete errors in employment data
fulldf$miniemp[fulldf$ftptemp + fulldf$miniemp > fulldf$totalpop] <- NA
fulldf$ftptemp[fulldf$ftptemp + fulldf$miniemp > fulldf$totalpop] <- NA


fulldf$arsincrtot <- log(fulldf$crtotal + sqrt(fulldf$crtotal^2+1))
fulldf$arsinduld <- log(fulldf$duldung + sqrt(fulldf$duldung^2+1))
fulldf$arsinasyl <- log(fulldf$asylum + sqrt(fulldf$asylum^2+1))
fulldf$arsinskr <- log(fulldf$asylumseeker + sqrt(fulldf$asylumseeker^2+1))

lv_skr <- plm(arsincrtot ~arsinskr + factor(year) + totalpop + minor +
                 Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                 Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                 Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                 weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                 age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                 res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
               data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

lv_asyl <- plm(arsincrtot ~ arsinasyl + factor(year) + totalpop + minor +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

lv_duld <- plm(arsincrtot ~ arsinduld + factor(year) + totalpop + minor +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

regslist <- list(tot_skr, tot_asyl, tot_duld,
                 wt_skr, wt_asyl, wt_duld,
                 lv_skr, lv_asyl, lv_duld)


# Robust se's
robust.se <- NULL
robust.se <- list()

for (i in 1:length(regslist)) {
  cov <- plm::vcovHC(regslist[[i]], method = "arellano", cluster = "group")
  robust.se[[i]] <- sqrt(diag(cov))
}

stargazer(regslist,# include the model objects we want to output
          type = "html",  # set type to html
          out = "levels.html",  # name the output file
          dep.var.labels = "Total Crime",
          se = robust.se)


