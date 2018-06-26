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

state_skr <- plm(arsincrtot ~ arsinskr + factor(year) + totalpop + 
                   Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                   Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                   Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA, 
                 data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless",], model = 'within', index =c('nat', 'year'))

state_asyl <- plm(arsincrtot ~ arsinasyl + factor(year) + totalpop + 
                    Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                    Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                    Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA, 
                  data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

state_duld <- plm(arsincrtot ~ arsinduld + factor(year) + totalpop + 
                    Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                    Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                    Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA, 
                  data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

emp_skr <- plm(arsincrtot ~ arsinskr + factor(year) + totalpop + factor(year) + totalpop + ftptemp + miniemp, 
               data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless",], model = 'within', index =c('nat', 'year'))

emp_asyl <- plm(arsincrtot ~ arsinasyl + factor(year) + totalpop + ftptemp + miniemp, 
                data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

emp_duld <- plm(arsincrtot ~ arsinduld + factor(year) + totalpop + ftptemp + miniemp,
                data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

indiv_skr <- plm(arsincrtot ~arsinskr + factor(year) + totalpop + minor + bornDE + genNA +
                   weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                   age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                   res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                 data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

indiv_asyl <- plm(arsincrtot ~ arsinasyl + factor(year) + totalpop + minor + bornDE + genNA +
                    weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                    age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                    res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                  data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless" & fulldf$nat != "Germany",], model = 'within', index =c('nat', 'year'))

indiv_duld <- plm(arsincrtot ~ arsinduld + factor(year) + totalpop + minor + bornDE + genNA +
                    weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                    age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                    res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                  data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

tot_skr <- plm(arsincrtot ~arsinskr + factor(year) + totalpop + minor +
                 Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                 Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                 Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                 weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                 age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                 res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
               data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

tot_asyl <- plm(arsincrtot ~ arsinasyl + factor(year) + totalpop + minor +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

tot_duld <- plm(arsincrtot ~ arsinduld + factor(year) + totalpop + minor +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

mix_skr <- plm(arsincrtot ~arsinskr + factor(year) + totalpop + minor +
                 Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                 Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                 Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + bornDE + genNA +
                 weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                 age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                 res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
               data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

mix_asyl <- plm(arsincrtot ~ arsinasyl + factor(year) + totalpop + minor +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

mix_duld <- plm(arsincrtot ~ arsinduld + factor(year) + totalpop + minor +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

sparse_skr <- plm(arsincrtot ~arsinskr + factor(year) + totalpop, 
                  data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

sparse_asyl <- plm(arsincrtot ~ arsinasyl + factor(year) + totalpop, 
                   data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))

sparse_duld <- plm(arsincrtot ~ arsinduld + factor(year) + totalpop, 
                   data = fulldf[fulldf$year != "2012" & fulldf$nat != "Stateless" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', index =c('nat', 'year'))


regslist <- list(sparse_skr, sparse_asyl, sparse_duld,
                 state_skr, state_asyl, state_duld, 
                 mix_skr, mix_asyl, mix_duld, 
                 tot_skr, tot_asyl, tot_duld)

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

