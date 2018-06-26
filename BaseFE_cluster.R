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
library(sjPlot)

# Load data
load("WtdData.rda")


## Fixed effects

state_skr <- plm(arsincrtot ~ arsinskr + factor(year) + 
                      Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                      Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                      Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA, 
                   data = fulldf[fulldf$year != "2012",], model = 'within', index =c('nat', 'year'))

state_asyl <- plm(arsincrtot ~ arsinasyl + factor(year) + 
                   Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                   Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                   Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA, 
                 data = fulldf[fulldf$year != "2012",], model = 'within', index =c('nat', 'year'))

state_duld <- plm(arsincrtot ~ arsinduld + factor(year) + 
                    Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                    Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                    Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA, 
                  data = fulldf[fulldf$year != "2012",], model = 'within', index =c('nat', 'year'))

emp_skr <- plm(arsincrtot ~ arsinskr + factor(year) + ftptemp + miniemp, 
             data = fulldf[fulldf$year != "2012",], model = 'within', index =c('nat', 'year'))

emp_asyl <- plm(arsincrtot ~ arsinasyl + factor(year) + ftptemp + miniemp, 
             data = fulldf[fulldf$year != "2012",], model = 'within', index =c('nat', 'year'))

emp_duld <- plm(arsincrtot ~ arsinduld + factor(year) + ftptemp + miniemp,
             data = fulldf[fulldf$year != "2012",], model = 'within', index =c('nat', 'year'))

indiv_skr <- plm(arsincrtot ~arsinskr + factor(year) + minor + bornDE + genNA +
                      weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                      age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                      res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                   data = fulldf[fulldf$year != "2012",], model = 'within', index =c('nat', 'year'))

indiv_asyl <- plm(arsincrtot ~ arsinasyl + factor(year)+ minor + bornDE + genNA +
                   weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                   age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                   res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                 data = fulldf[fulldf$year != "2012" & fulldf$nat != "Germany",], model = 'within', index =c('nat', 'year'))

indiv_duld <- plm(arsincrtot ~ arsinduld + factor(year)+ minor + bornDE + genNA +
                   weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                   age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                   res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                 data = fulldf[fulldf$year != "2012",], model = 'within', index =c('nat', 'year'))

tot_skr <- plm(arsincrtot ~arsinskr + factor(year)+ minor +
                     Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                     Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                     Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                     weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                     age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                     res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                   data = fulldf[fulldf$year != "2012",], model = 'within', index =c('nat', 'year'))

tot_asyl <- plm(arsincrtot ~ arsinasyl + factor(year)+ minor +
                 Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                 Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                 Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                 weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                 age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                 res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
               data = fulldf[fulldf$year != "2012",], model = 'within', index =c('nat', 'year'))

tot_duld <- plm(arsincrtot ~ arsinduld + factor(year)+ minor +
                 Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                 Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                 Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + ftptemp + miniemp + bornDE + genNA +
                 weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                 age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                 res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
               data = fulldf[fulldf$year != "2012",], model = 'within', index =c('nat', 'year'))

mix_skr <- plm(arsincrtot ~arsinskr + factor(year)+ minor +
                 Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                 Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                 Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + bornDE + genNA +
                 weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                 age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                 res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
               data = fulldf[fulldf$year != "2012",], model = 'within', index =c('nat', 'year'))

mix_asyl <- plm(arsincrtot ~ arsinasyl + factor(year)+ minor +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012",], model = 'within', index =c('nat', 'year'))

mix_duld <- plm(arsincrtot ~ arsinduld + factor(year)+ minor +
                  Baden.Württemberg + Bayern + Brandenburg + Bremen + Hamburg + Hessen + Mecklenburg.Vorpommern + 
                  Niedersachsen + Rheinland.Pfalz + Saarland + Sachsen + Berlin +
                  Sachsen.Anhalt + Schleswig.Holstein + Thüringen + stateNA + bornDE + genNA +
                  weiblich + sexNA + widow + marNA + married + divorced + juv + yngadlt + age35.44 +
                  age45.54 + age55.64 + retired + res0 + ageNA + resNA + res4.5 + res6.7 + res8.9 + res10.14 +
                  res15.19 + res20.24 + res25.29 + res30.34 + res34.39 + res40plus, 
                data = fulldf[fulldf$year != "2012",], model = 'within', index =c('nat', 'year'))

sparse_skr <- plm(arsincrtot ~arsinskr + factor(year), 
               data = fulldf[fulldf$year != "2012",], model = 'within', index =c('nat', 'year'))

sparse_asyl <- plm(arsincrtot ~ arsinasyl + factor(year), 
                data = fulldf[fulldf$year != "2012",], model = 'within', index =c('nat', 'year'))

sparse_duld <- plm(arsincrtot ~ arsinduld + factor(year), 
                data = fulldf[fulldf$year != "2012",], model = 'within', index =c('nat', 'year'))


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
          out = "fe_regs.html",  # name the output file
          dep.var.labels = "Total Crime",
          se = robust.se)



# Test for differences between models
a <- as.matrix(coeftest(regslist[[9]], plm::vcovHC(regslist[[9]], method = "arellano", cluster = "group")))
b <- as.matrix(coeftest(regslist[[6]], plm::vcovHC(regslist[[6]], method = "arellano", cluster = "group")))

se <- sqrt((a[1,2])^2 + (b[1,2])^2)
diff <- a[1,1] - b[1,1]
z <- diff/se
2*pnorm(-abs(1.85))



# Coefficient plots

library(dotwhisker)
plotdf <- data.frame()

for (i in 1:length(regslist)) {
  tidydf <- broom::tidy(regslist[[i]])
  plotdf <- rbind(plotdf, tidydf[1,])
}

plotdf$model <- 1:12

plotdf$term[plotdf$term == "arsinskr"] <- "Asylum seekers"
plotdf$term[plotdf$term == "arsinasyl"] <- "Protected status"
plotdf$term[plotdf$term == "arsinduld"] <- "Temporary stay \n of deportation"
       
dwplot(plotdf) +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "#1e1e1e"), 
        plot.background = element_rect(fill = "#1e1e1e"),
        panel.grid.major = element_line(color = "#515151"),
        panel.grid.minor = element_line(color = "#515151"),
        axis.title = element_text(color = "white", size = 12),
        axis.text=element_text(colour="white", size = 12)) +
  geom_vline(xintercept = 0, color = "white") + 
  scale_color_manual(values = c("#58b098", "#58b098", "#58b098",
                                                             "#58b098", "#58b098", "#58b098", 
                                                             "#58b098", "#58b098", "#58b098", 
                                                             "#58b098", "#58b098", "#58b098")) +
  scale_size_manual(values = c(6, 6, 6,6, 6, 6,6, 6, 6,6, 6, 6)) +
  xlab("Elasticity with Total Crimes per Capita")
       
       





