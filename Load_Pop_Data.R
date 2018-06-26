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
library(rvest)

# Import population data
popsex <- read_excel("gender_nationality.xlsx", col_names = TRUE, range = "A7:D1057")
popms <- read_excel("familienstand-nationality.xlsx", col_names = TRUE, range = "A7:J1057")
popage <- read_excel("age-nationality.xlsx", col_names = TRUE, range = "A7:CU1057")
popstate <- read_excel("bundesland-nat.xlsx", col_names = TRUE, range = "A6:B16811")
popres <- read_excel("reslength-nat.xlsx", col_names = TRUE, range = "A8:N2098")
popgen <- read_excel("generation-nat.xlsx", col_names = TRUE, range = "A7:D4177")
popstatus <- read_excel("resstatus-nat.xlsx", col_names = TRUE, range = "A7:C10775")
popemp <- read_excel("employed-nat.xlsx", col_names = TRUE, range = "A3:D4308")

## Fill in values for gen, res, status tables
popres <- fill(popres, X__1)
popgen <- fill(popgen, c(X__1, X__2))
popstatus <- fill(popstatus, names(popstatus)[1])

## Create data list for DeStatis data
popList <- list(sex = popsex, ms=popms, age = popage, state = popstate, res = popres, gen = popgen, status = popstatus)

# Basic data cleaning for Destatis data - eliminate filler, add year column, convert to numerics
for (i in 1:length(popList)) {
  names(popList[[i]])[1] <- "country" # Rename country column
  popList[[i]]<-popList[[i]][!(popList[[i]][['country']]=="davon:"),] 
  popList[[i]]<-popList[[i]][!(popList[[i]][['country']]=="Insgesamt"),] 
  popList[[i]][["year"]] <- str_extract(popList[[i]][['country']], "(?<=31.12.)....$") 
  popList[[i]] <- fill(popList[[i]], year)
  popList[[i]] <- popList[[i]][!grepl("31.12", popList[[i]][['country']]),] 
  cols <- colnames(popList[[i]]) # Convert to numerics
  vars <- str_extract(cols, "X__\\d") 
  vars <- vars[!is.na(vars)]
  cols.char <- c("year","country", vars)
  cols.num <- cols[!cols %in% cols.char]
  DF.char <- popList[[i]][cols.char]
  DF.num <- as.data.frame(lapply(popList[[i]][cols.num],as.numeric))
  popList[[i]] <- cbind(DF.char, DF.num)
}

# Extra cleaning for age dataset 
colnames(popList[[3]])[3] <- "age0"
for (i in 4:98) {
  colnames(popList[[3]])[i] <- paste("age", str_extract(colnames(popList[[3]])[i], "\\d+"), sep = "")
}

# Extra cleaning for res status dataset
popList[[7]] <- popList[[7]] %>%
  tidyr::spread(key = X__1, value = Insgesamt) 

colnames(popList[[7]])[3] <- "pending"
colnames(popList[[7]])[4] <- "asylumseeker"
colnames(popList[[7]])[5] <- "otherbefristet"
colnames(popList[[7]])[6] <- "asylum"
colnames(popList[[7]])[7] <- "work"
colnames(popList[[7]])[8] <- "educ"
colnames(popList[[7]])[9] <- "family"
colnames(popList[[7]])[10] <- "shortterm"
colnames(popList[[7]])[11] <- "duldung"
colnames(popList[[7]])[12] <- "EU"
colnames(popList[[7]])[13] <- "undoc"
colnames(popList[[7]])[14] <- "longterm"
colnames(popList[[7]])[15] <- "befreit"

popList[[7]][["shortterm"]] <- NULL

# Extra cleaning for Bundesländer dataset
popList[[4]][["state"]] <- str_extract(popList[[4]][['country']], 
  "Baden-Württemberg|Bayern|Berlin|Brandenburg|Bremen|Hamburg|Hessen|Mecklenburg-Vorpommern|Niedersachsen|Nordrhein-Westfalen|Rheinland-Pfalz|Saarland|^Sachsen$|Sachsen-Anhalt|Schleswig-Holstein|Thüringen")
popList[[4]] <- fill(popList[[4]], state)
popList[[4]] <- popList[[4]][!grepl("Baden-Württemberg|Bayern|Berlin|Brandenburg|Bremen|Hamburg|Hessen|Mecklenburg-Vorpommern|Niedersachsen|Nordrhein-Westfalen|Rheinland-Pfalz|Saarland|^Sachsen$|Sachsen-Anhalt|Schleswig-Holstein|Thüringen", popList[[4]][['country']]),]
popList[[4]] <- popList[[4]] %>%
  tidyr::spread(key = state, value = Insgesamt) 

# Extra cleaning for generation dataset
popList[[6]] <- popList[[6]] %>%
  tidyr::spread(key = X__3, value = insgesamt) 
popList[[6]][["total"]] <- popList[[6]][["männlich"]] + popList[[6]][["weiblich"]]
popList[[6]][["männlich"]] <- NULL
popList[[6]][["weiblich"]] <- NULL
popList[[6]] <- popList[[6]] %>%
  tidyr::spread(key = X__2, value = total) 
colnames(popList[[6]])[3] <- "firstgen"
colnames(popList[[6]])[4] <- "bornDE"

# Extra cleaning for reslength dataset

vars <- str_extract(colnames(popList[[5]]), "Aufenthaltsdauer.*$")
vars <- vars[!is.na(vars)]
popList[[5]] <- popList[[5]] %>%
  tidyr::unite(values, vars) %>% 
  tidyr::spread(key = X__2, value = values) %>% 
  tidyr::separate(männlich, into = paste(vars, "m", sep="_")) %>% 
  tidyr::separate(weiblich, into = paste(vars, "w", sep="_"))

cols <- colnames(popList[[5]]) # Convert to numerics again
cols.char <- c("year","country")
cols.num <- cols[!cols %in% cols.char]
DF.char <- popList[[5]][cols.char]
DF.num <- as.data.frame(lapply(popList[[5]][cols.num],as.numeric))
popList[[5]] <- cbind(DF.char, DF.num)

# Combine male + female data
popList[[5]]["res0"] <- popList[[5]][[cols[3]]] + popList[[5]][[cols[15]]]
popList[[5]]["res1-3"] <- popList[[5]][[cols[4]]] + popList[[5]][[cols[16]]]
popList[[5]]["res4-5"] <- popList[[5]][[cols[5]]] + popList[[5]][[cols[17]]]
popList[[5]]["res6-7"] <- popList[[5]][[cols[6]]] + popList[[5]][[cols[18]]]
popList[[5]]["res8-9"] <- popList[[5]][[cols[7]]] + popList[[5]][[cols[19]]]
popList[[5]]["res10-14"] <- popList[[5]][[cols[8]]] + popList[[5]][[cols[20]]]
popList[[5]]["res15-19"] <- popList[[5]][[cols[9]]] + popList[[5]][[cols[21]]]
popList[[5]]["res20-24"] <- popList[[5]][[cols[10]]] + popList[[5]][[cols[22]]]
popList[[5]]["res25-29"] <- popList[[5]][[cols[11]]] + popList[[5]][[cols[23]]]
popList[[5]]["res30-34"] <- popList[[5]][[cols[12]]] + popList[[5]][[cols[24]]]
popList[[5]]["res34-39"] <- popList[[5]][[cols[13]]] + popList[[5]][[cols[25]]]
popList[[5]]["res40plus"] <- popList[[5]][[cols[14]]] + popList[[5]][[cols[26]]]

popList[[5]] <- popList[[5]][!colnames(popList[[5]]) %in% cols.num]

# Combine data for two Serbias 
years <- (c(2012, 2013, 2014, 2015, 2016))

for (i in 1:length(popList)) 
{
  cols <- names(popList[[i]])
  cols.char <- c("year","country")
  cols.num <- cols[!cols %in% cols.char]
  for(j in years) 
  {
    popList[[i]] <- rbind(popList[[i]], c(j, "Serbia", colSums(
      rbind(as.numeric(popList[[i]][(popList[[i]][['country']]=="Serbien (einschl. Kosovo) (03.06.2006-16.02.2008)" 
                                     & popList[[i]][['year']]==j), cols.num]), 
            as.numeric(popList[[i]][(popList[[i]][['country']]=="Serbien (ohne Kosovo) (ab 17.02.2008)" 
                                     & popList[[i]][['year']]==j), cols.num])), na.rm = TRUE))) 
  }
  popList[[i]] <- popList[[i]][!grepl("Serbien", popList[[i]][['country']]),]
}

# Combine data for two Sudans
for (i in 1:length(popList)) 
{
  cols <- names(popList[[i]])
  cols.char <- c("year","country")
  cols.num <- cols[!cols %in% cols.char]
  for(j in years) 
  {
    popList[[i]] <- rbind(popList[[i]], c(j, "Sudan", colSums(
      rbind(as.numeric(popList[[i]][(popList[[i]][['country']]=="Sudan (einschließlich Südsudan) (bis 08.07.2011)" 
                                     & popList[[i]][['year']]==j), cols.num]), 
            as.numeric(popList[[i]][(popList[[i]][['country']]=="Sudan (ohne Südsudan) (ab 09.07.2011)" 
                                     & popList[[i]][['year']]==j), cols.num])), na.rm = TRUE))) 
  }
  popList[[i]] <- popList[[i]][!grepl("Sudan \\(", popList[[i]][['country']]),]
}


# Combine data for UK and overseas
for (i in 1:length(popList)) 
{
  cols <- names(popList[[i]])
  cols.char <- c("year","country")
  cols.num <- cols[!cols %in% cols.char]
  for(j in years) 
  {
    popList[[i]] <- rbind(popList[[i]], c(j, "United Kingdom", colSums(
      rbind(as.numeric(popList[[i]][(popList[[i]][['country']]=="Britische Überseegebiete" 
                                     & popList[[i]][['year']]==j), cols.num]), 
            as.numeric(popList[[i]][(popList[[i]][['country']]=="Vereinigtes Königreich" 
                                     & popList[[i]][['year']]==j), cols.num])), na.rm = TRUE))) 
  }
  popList[[i]] <- popList[[i]][!grepl("Vereinigtes Königreich|Britische", popList[[i]][['country']]),]
}

# Cleaning employment data
popemp <- popemp[!str_detect(popemp$Staatsangehörigkeit, " insgesamt$"),]# Remove continent totals
popemp$country <- popemp$Staatsangehörigkeit
popemp$Staatsangehörigkeit <- NULL

popemp$ftptemp <- popemp$`Sozialversicherungspflichtig Beschäftigte (SVB)` # Rename vars
popemp$miniemp <- popemp$`Ausschließllich geringfügig Beschäftigte (agB)`
popemp$`Ausschließllich geringfügig Beschäftigte (agB)` <- NULL
popemp$`Sozialversicherungspflichtig Beschäftigte (SVB)` <- NULL

popemp <- popemp[str_detect(popemp$Auswertemonatswert, "^Dezember"),] # Remove months not December
popemp$year <- str_extract(popemp$Auswertemonatswert, "(?<=Dezember )....$")
popemp$Auswertemonatswert <- NULL

# Convert to numbers, * = NA
popemp$miniemp <- as.numeric(popemp$miniemp)

# Add to list
popList <- c(list(popemp), popList)


# Remove countries that no longer exist 
for (i in 1:length(popList)) {
  popList[[i]] <- popList[[i]][!grepl("Sowjetunion|Jugoslawien|Serbien und Montenegro", 
                                      popList[[i]][['country']]),]
}


# Convert country names to English, hardcoding cases that didnt work
for (i in 1:length(popList)) 
 {
  popList[[i]][['nat']] <- countrycode(popList[[i]][['country']], 'country.name.de', 'country.name.en')
  popList[[i]][['nat']][(popList[[i]][['country']] == "Staatenlos")] <- "Stateless"
  popList[[i]][['nat']][(popList[[i]][['country']] == "Ungeklärt / Ohne Angabe")] <- "Unlisted/Unknown"
  popList[[i]][['nat']][(popList[[i]][['country']] == "Serbia")] <- "Serbia"
  popList[[i]][['nat']][(popList[[i]][['country']] == "United Kingdom")] <- "United Kingdom"
  popList[[i]][['nat']][(popList[[i]][['country']] == "Großbritannien und Nordirland")] <- "United Kingdom"
  popList[[i]][['nat']][(popList[[i]][['country']] == "Weißrußland")] <- "Belarus"
  popList[[i]][['nat']][(popList[[i]][['country']] == "Staatenlos/keine Angabe")] <- "Stateless"
}

popList[[i]][['country']] <- NULL
popList[[i]][['nat']] <- !is.na(popList[[i]][["nat"]])

# Merge population dataset
popdat <- join_all(popList, by = c("nat", "year"), type = "full", match = "all")
popdat$totalpop <- popdat$Insgesamt
popdat$Insgesamt <- NULL
popdat$msunknown <- popdat$unbekannt
popdat$unbekannt <- NULL

# Get world regions via webscraping
url <- "http://www.iucnredlist.org/technical-documents/data-organization/countries-by-regions"
url_parsed <- read_html(url)
css <- "h2+ table td"
nodes <- html_nodes(url_parsed, css = css) 
regions <- html_text(nodes)

# Split into two variables
regionsdf <- data.frame(stringr::str_split(regions, " – to ", simplify = TRUE))
names(regionsdf)[1] <- "country"
names(regionsdf)[2] <- "region"

# Hard code cases that didnt work
regionsdf$region[187] <- "Sub-Saharan Africa" # St Helena
regionsdf$region[243] <- "South America" # Venezuela 
regionsdf$region[59] <- "Caribbean Islands" # Curacao
regionsdf$region[27] <- "South America" # Bolivia

# Convert country names to standard formatting
regionsdf$nat <- countrycode(regionsdf$country, 'country.name.en', 'country.name.en')

# France matched twice
regionsdf <- regionsdf[!grepl("Clipperton Island – see under France", regionsdf$country),]

# Hard code some failed cases and drop others
regionsdf$nat[regionsdf$country == "Finland [excludes the Åland Islands]"] <- "Finland"
regionsdf$nat[regionsdf$country == "Papua New Guinea [includes the Bismarck Archipelago and the North Solomons]"] <- "Papua New Guinea"
regionsdf$nat[regionsdf$country == "United Kingdom [excludes Guernsey, Jersey and Isle of Man]"] <- "United Kingdom"
regionsdf <- regionsdf[!is.na(regionsdf$nat),]
regionsdf$country <- NULL

# Change & to and for better feasibility with stargazer
regionsdf$region <- str_replace(regionsdf$region, "&", "and")

# Merge regional data
dat <- merge(popdat, regionsdf, by = "nat", all.x = TRUE)


# Kosovo was not in regional dataset
dat$region[dat$nat == "Kosovo"] <- "Europe"

# Recode stateless/unknown
dat$region <- as.character(dat$region)
dat$region[dat$nat == "Stateless"] <- "Stateless or unknown"
dat$region[dat$nat == "Unlisted/Unknown"] <- "Stateless or unknown"

# Convert to numerics again
cols <- colnames(dat) # Convert to numerics
cols.char <- c("year","nat", "region")
cols.num <- cols[!cols %in% cols.char]
DF.char <- dat[cols.char]
DF.num <- as.data.frame(lapply(dat[cols.num],as.numeric))
dat <- cbind(DF.char, DF.num)

dat$country <- NULL

save(dat, file = "PopData.Rda")


