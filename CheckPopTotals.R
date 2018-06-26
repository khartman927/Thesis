# Clear memory
rm(list=ls())

# Set directory
setwd("/Users/kerihartman/Dropbox/Hertie/Thesis/Data")

# Load libraries
library(tidyverse)
library(ggplot2)
library(stringr)

# Load data
load("CompleteData.rda")

# Create vars for correct totals
fulldf$statetot <- fulldf$Baden.Württemberg + fulldf$Bayern + fulldf$Brandenburg + fulldf$Bremen + fulldf$Hamburg +
  fulldf$Hessen + fulldf$Mecklenburg.Vorpommern + fulldf$Niedersachsen + fulldf$Nordrhein.Westfalen + fulldf$Rheinland.Pfalz +
  fulldf$Saarland + fulldf$Sachsen + fulldf$Berlin + fulldf$Sachsen.Anhalt + fulldf$Schleswig.Holstein + fulldf$Thüringen

fulldf$martot <- fulldf$single + fulldf$married + fulldf$widow + fulldf$msunknown + fulldf$divorced

fulldf$sextot <- fulldf$männlich + fulldf$weiblich

fulldf$gentot <- fulldf$bornDE + fulldf$firstgen

fulldf$restot <- fulldf$res0 + fulldf$res1.3 + fulldf$res4.5 + fulldf$res6.7 + fulldf$res8.9 + fulldf$res10.14 +
  fulldf$res15.19 + fulldf$res20.24 + fulldf$res25.29 + fulldf$res30.34 + fulldf$res34.39 + fulldf$res40plus

fulldf$agetot <- fulldf$minor + fulldf$juv + fulldf$yngadlt + fulldf$age25.34 + fulldf$age35.44 + fulldf$age45.54 +
  fulldf$age55.64 + fulldf$retired

# Define total pop as largest number
fulldf$totalpop <- apply(cbind(fulldf$statetot, fulldf$martot, fulldf$sextot, fulldf$gentot, fulldf$restot, fulldf$agetot), 1, max)


# Define unknown categories for vars
fulldf$stateNA <- fulldf$totalpop - fulldf$statetot
fulldf$marNA <- fulldf$totalpop - fulldf$single - fulldf$married - fulldf$widow - fulldf$divorced
fulldf$sexNA <- fulldf$totalpop - fulldf$sextot
fulldf$genNA <- fulldf$totalpop - fulldf$gentot
fulldf$resNA <- fulldf$totalpop - fulldf$restot
fulldf$ageNA <- fulldf$totalpop - fulldf$agetot

# Delete irrelevant cases
fulldf <- fulldf[fulldf$nat != "Stateless" & fulldf$nat != "Unlisted/Unknown" & fulldf$nat != "Germany" & fulldf$nat != "Foreigners (all)",]
fulldf <- fulldf[!is.na(fulldf$crtotal),]

# Create total emp
fulldf$emp <- mapply(sum, fulldf$ftptemp, fulldf$miniemp, na.rm = FALSE)


# Isolate problematic cases
problems <- fulldf %>% 
  filter(emp > totalpop & year != "2012") %>% 
  select(nat, year, emp, totalpop)

problems$diff <- problems$emp - problems$totalpop

summary(problems$diff)
sd(problems$diff)

# Delete problematic cases
fulldf$ftptemp[fulldf$emp>fulldf$totalpop]<-NA
fulldf$miniemp[fulldf$emp>fulldf$totalpop]<-NA
fulldf$emp[fulldf$emp>fulldf$totalpop]<-NA




save(fulldf, file = "CompleteData.rda")

# Boxplot to see extent of changes
# Looks to be the result of rounding error as all vars in multiples of five
p <- ggplot(data = fulldf, 
            mapping = aes(x = "", y = stateNA)) 
p + geom_boxplot()


totaloff <- c(fulldf$stateNA, fulldf$sexNA, fulldf$genNA, fulldf$resNA, fulldf$ageNA, fulldf$marNA - fulldf$msunknown)

sd(totaloff[totaloff != 0], na.rm = TRUE)

summary(totaloff[totaloff != 0])


