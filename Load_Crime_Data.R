# Clear memory
rm(list=ls())

# Set directory
setwd("/Users/kerihartman/Dropbox/Hertie/Thesis/Data")

# Load libraries
library(readxl)
library(tidyverse)
library(plyr)
library(stringr)

# Import data for each year
dat2016 <- read_excel("STD-TV-16-T62-TV-Staatsangehoerigkeiten_excel (4).xlsx", 
                      col_names = TRUE, range = "A5:GQ1067")
dat2015 <- read_excel("2015_tb62_TatverdaechtigeNichtdeutscheStraftatenStaatsangehoerigkeit_excel.xlsx", 
                      col_names = TRUE, range = "A5:GR1038")
dat2014 <- read_excel("2014_tb62_TatverdaechtigeNichtdeutscheStraftatenStaatsangehoerigkeit_excel.xlsx", 
                      col_names = TRUE, range = "A5:GP929")
dat2013 <- read_excel("2013_tb62_TatverdaechtigeNichtdeutscheStraftatenStaatsangehoerigkeit_excel.xls", 
                      col_names = TRUE, range = "A5:GW1011")
dat2012 <- read_excel("2012_tb62_TatverdaechtigeNichtdeutscheStraftatenStaatsangehoerigkeit_excel.xls", 
                      col_names = TRUE, range = "A5:GS1012")

# Create a data list
dfList <- list(dat2012=dat2012, dat2013=dat2013, dat2014=dat2014, dat2015=dat2015, dat2016=dat2016)

# Use crime codes as key, drop description strings, and recode total crimes to 999999
for (i in 1:length(dfList)) {
  names(dfList[[i]])[1] <- "kennzahl"
  dfList[[i]][,2] <- NULL
  dfList[[i]][1,1] <- "999999"
  dfList[[i]][['kennzahl']] <- paste("a", dfList[[i]][['kennzahl']], sep = "")
}

# Make data tidy
for (i in 1:length(dfList)) {
  dfList[[i]] <- dfList[[i]] %>%
    tidyr::gather(key = country, value = value,
                  -kennzahl, na.rm = TRUE) %>% 
    tidyr::spread(key = kennzahl, value = value)
}  

# Merge datasets
df <- dplyr::bind_rows(dfList, .id = "year")
df$year <- sub('^...', '', df$year)

# Clean country names

# Drop countries that no longer exist
df <- df[!grepl("-Altfälle-", df$country),]

# Combine data for unknown/unlisted
crimevars <- na.omit(str_extract(names(df),"^a.*$"))
years <- unique(df$year)
for(i in years) {
  df <- rbind(df, c(i, "Unlisted/Unknown", colSums(
    rbind(as.numeric(df[df$country =="Ungeklärt" & df$year==i,
                        crimevars]), 
          as.numeric(df[df$country =="Ohne Angabe" & df$year == i, crimevars])), na.rm = TRUE)))
}

df <- df[!grepl("Ungeklärt|Ohne Angabe", df$country),]

# Convert names to English
df$nat <- countrycode(df$country, 'country.name.de', 'country.name.en')

# Hard code the cases that didnt work
df %>% 
  filter(is.na(nat), year == "2012") %>% 
  select(country)

df$nat[df$country == "Großbritannien/Nordirland"] <- "United Kingdom"
df$nat[df$country == "Kambotscha"] <- "Cambodia"
df$nat[df$country == "Nichtdeutsche insgesamt"] <- "Foreigners (all)"
df$nat[df$country == "Staatenlos"] <- "Stateless"
df$nat[df$country == "Tatverdächtige insgesamt"] <- "Total"
df$nat[df$country == "Unlisted/Unknown"] <- "Unlisted/Unknown"

df$country <- NULL

# Convert to numerics
cols <- names(df)
cols.char <- c("year","nat")
cols.num <- cols[!cols %in% cols.char]
DF.char <- df[cols.char]
DF.num <- as.data.frame(lapply(df[cols.num],as.numeric))
df <- cbind(DF.char, DF.num)

# Create key dependent variables for groups of crimes

crimevars <- na.omit(str_extract(names(df), "^a.*$"))

df$crtotal <- df$a890000 # Total crimes without migration
df$violent <- df$a892000 # Violent crimes total
df$theft <- df$a....00 # Diebstahl
df$drug <- df$a730000 # BtMG

save(df, file = "CrimeData.Rda")