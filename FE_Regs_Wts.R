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
library(clubSandwich)

# Load data
load("WtdData.rda")



# Create regression weights - separate for each year
years <- c("2012", "2013", "2014", "2015", "2016")
popyears <- NULL

for (i in 1:length(years)) {
  popyears[i] <- sum(fulldf$totalpop[fulldf$year == years[i]], na.rm = TRUE)
  fulldf$weight[fulldf$year == years[i]] <-fulldf$totalpop[fulldf$year == years[i]] /  popyears[i]
}


# Models

state_skr <- plm(arsincrtot ~ arsinskr + factor(year) + 
                   Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                   Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                   Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA, 
                 data = fulldf[fulldf$year != "2012",], model = 'within', weights = weight, index =c('nat', 'year'))

state_asyl <- plm(arsincrtot ~ arsinasyl + factor(year) + 
                    Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                    Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                    Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA, 
                  data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', weights = weight, index =c('nat', 'year'))

state_duld <- plm(arsincrtot ~ arsinduld + factor(year) + 
                    Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                    Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                    Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA, 
                  data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', weights = weight, index =c('nat', 'year'))

indiv_skr <- plm(arsincrtot ~arsinskr + factor(year) + minor + bornDE + genNA +
                   weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                   age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                   res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                 data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', weights = weight, index =c('nat', 'year'))

indiv_asyl <- plm(arsincrtot ~ arsinasyl + factor(year) + minor + bornDE + genNA +
                    weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                    age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                    res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                  data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany",], model = 'within', weights = weight, index =c('nat', 'year'))

indiv_duld <- plm(arsincrtot ~ arsinduld + factor(year) + minor + bornDE + genNA +
                    weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                    age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                    res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                  data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', weights = weight, index =c('nat', 'year'))

tot_skr <- plm(arsincrtot ~arsinskr + factor(year) + minor +
                 Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                 Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                 Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                 weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                 age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                 res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
               data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', weights = weight, index =c('nat', 'year'))

tot_asyl <- plm(arsincrtot ~ arsinasyl + factor(year) + minor +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', weights = weight, index =c('nat', 'year'))

tot_duld <- plm(arsincrtot ~ arsinduld + factor(year) + minor +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', weights = weight, index =c('nat', 'year'))

mix_skr <- plm(arsincrtot ~arsinskr + factor(year) + minor +
                 Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                 Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                 Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + bornDE + genNA +
                 weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                 age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                 res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
               data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', weights = weight, index =c('nat', 'year'))

mix_asyl <- plm(arsincrtot ~ arsinasyl + factor(year) + minor +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', weights = weight, index =c('nat', 'year'))

mix_duld <- plm(arsincrtot ~ arsinduld + factor(year) + minor +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', weights = weight, index =c('nat', 'year'))

sparse_skr <- plm(arsincrtot ~arsinskr + factor(year), 
                  data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', weights = weight, index =c('nat', 'year'))

sparse_asyl <- plm(arsincrtot ~ arsinasyl + factor(year), 
                   data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', weights = weight, index =c('nat', 'year'))

sparse_duld <- plm(arsincrtot ~ arsinduld + factor(year), 
                   data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",], model = 'within', weights = weight, index =c('nat', 'year'))

# Violent crime

vio_skr <- plm(arsinvio ~ arsinskr + factor(year) + minor +
                 Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                 Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                 Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                 weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                 age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                 res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
               data = fulldf[fulldf$year != "2012",],model = 'within', weights = weight, index =c('nat', 'year'))

vio_asyl <- plm(arsinvio ~ arsinasyl + factor(year) + minor +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012",],model = 'within', weights = weight, index =c('nat', 'year'))

vio_duld <- plm(arsinvio ~ arsinduld + factor(year) + minor +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012",],model = 'within', weights = weight, index =c('nat', 'year'))

# Drug crime

drug_skr <- plm(arsindrug ~ arsinskr + factor(year) + minor +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012",],model = 'within', weights = weight, index =c('nat', 'year'))

drug_asyl <- plm(arsindrug ~ arsinasyl + factor(year) + minor +
                   Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                   Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                   Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                   weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                   age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                   res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                 data = fulldf[fulldf$year != "2012",],model = 'within', weights = weight, index =c('nat', 'year'))

drug_duld <- plm(arsindrug ~ arsinduld + factor(year) + minor +
                   Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                   Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                   Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                   weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                   age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                   res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                 data = fulldf[fulldf$year != "2012",],model = 'within', weights = weight, index =c('nat', 'year'))

# Theft

rob_skr <- plm(arsintheft ~ arsinskr + factor(year) + minor +
                 Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                 Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                 Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                 weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                 age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                 res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
               data = fulldf[fulldf$year != "2012",],model = 'within', weights = weight, index =c('nat', 'year'))

rob_asyl <- plm(arsintheft ~ arsinasyl + factor(year) + minor +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012",],model = 'within', weights = weight, index =c('nat', 'year'))

rob_duld <- plm(arsintheft ~ arsinduld + factor(year) + minor +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012",],model = 'within', weights = weight, index =c('nat', 'year'))

# Basic regs list

regslist <- list(sparse_skr, sparse_asyl, sparse_duld,
                 state_skr, state_asyl, state_duld, 
                 mix_skr, mix_asyl, mix_duld, 
                 tot_skr, tot_asyl, tot_duld)

regslist <- list(tot_skr, tot_asyl, tot_duld, 
                 vio_skr, vio_asyl, vio_duld,
                 drug_skr, drug_asyl, drug_duld, 
                 rob_skr, rob_asyl, rob_duld)

# Robust se's
robust.se <- NULL
robust.se <- list()

for (i in 1:length(regslist)) {
  cov <- plm::vcovHC(regslist[[i]], method = "arellano", cluster = "group")
  robust.se[[i]] <- sqrt(diag(cov))
}

stargazer(regslist,# include the model objects we want to output
          type = "html",  # set type to html
          out = "wtd_regs.html")  # name the output fil





