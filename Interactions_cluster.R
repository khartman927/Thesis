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

# Load data
load("WtdData.rda")

# Inverse hyperbolic transformation
fulldf$arsinftpt <- log(fulldf$ftptemp + sqrt(fulldf$ftptemp^2+1))
fulldf$arsinmini <- log(fulldf$miniemp + sqrt(fulldf$miniemp^2+1))

#Models - Total Crime
tot_duld_ftpt <- plm(arsincrtot ~ arsinduld*arsinftpt + year + minor +
               Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
               Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
               Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + bornDE + genNA +
               weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
               age45.54 + age55.64 + retired + res0 + res4.5 + res6.7 + res8.9 + res10.14 +
               res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus + resNA + ageNA, 
                data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

tot_duld_mini <- plm(arsincrtot ~ arsinduld*arsinmini  + year + minor +
                       Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                       Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                       Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + bornDE + genNA +
                       weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                       age45.54 + age55.64 + retired + res0 + res4.5 + res6.7 + res8.9 + res10.14 +
                       res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus + resNA + ageNA, 
                     data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

# Violent Crime

vio_duld_ftpt <- plm(arsinvio ~ arsinduld*arsinftpt + year + minor +
               Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
               Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
               Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + bornDE + genNA +
               weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
               age45.54 + age55.64 + retired + res0 + res4.5 + res6.7 + res8.9 + res10.14 +
               res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus + resNA + ageNA, 
                data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

vio_duld_mini <- plm(arsinvio ~ arsinduld*arsinmini  + year + minor +
                       Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                       Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                       Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + bornDE + genNA +
                       weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                       age45.54 + age55.64 + retired + res0 + res4.5 + res6.7 + res8.9 + res10.14 +
                       res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus + resNA + ageNA, 
                     data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

# Drug Crime

drug_duld_ftpt <- plm(arsindrug ~ arsinduld*arsinftpt + year + minor +
               Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
               Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
               Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + bornDE + genNA +
               weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
               age45.54 + age55.64 + retired + res0 + res4.5 + res6.7 + res8.9 + res10.14 +
               res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus + resNA + ageNA, 
                 data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

drug_duld_mini <- plm(arsindrug ~ arsinduld*arsinmini  + year + minor +
                        Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                        Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                        Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + bornDE + genNA +
                        weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                        age45.54 + age55.64 + retired + res0 + res4.5 + res6.7 + res8.9 + res10.14 +
                        res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus + resNA + ageNA, 
                      data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

# Theft


rob_duld_ftpt <- plm(arsintheft ~ arsinduld*arsinftpt + year + minor +
               Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
               Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
               Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + bornDE + genNA +
               weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
               age45.54 + age55.64 + retired + res0 + res4.5 + res6.7 + res8.9 + res10.14 +
               res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus + resNA + ageNA, 
                data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

rob_duld_mini <- plm(arsintheft ~ arsinduld*arsinmini  + year + minor +
                       Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                       Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                       Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + bornDE + genNA +
                       weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                       age45.54 + age55.64 + retired + res0 + res4.5 + res6.7 + res8.9 + res10.14 +
                       res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus + resNA + ageNA, 
                     data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))


# Output

regslist <- list(tot_duld_ftpt, tot_duld_mini,
                 vio_duld_ftpt, vio_duld_mini,
                drug_duld_ftpt, drug_duld_mini,
                rob_duld_ftpt, rob_duld_mini)

# Robust se's
robust.se <- NULL
robust.se <- list()

for (i in 1:length(regslist)) {
  cov <- plm::vcovHC(regslist[[i]], method = "arellano", cluster = "group")
  robust.se[[i]] <- sqrt(diag(cov))
}

stargazer(regslist,# include the model objects we want to output
          type = "html",  # set type to html
          out = "interaction_regs_sparse.html",  # name the output file
          se = robust.se)


