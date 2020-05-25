library(gtalibrary)
library(readxl)
library(splitstackshape)
rm(list = ls())
gta_setwd()

project.path="0 projects/39 VDMA/"
source(paste0(project.path, "/help files/definitions.R"))


cutoff.date="2020-01-01"
vdma.sectors=c(0,unique(cpc.to.hs$cpc[cpc.to.hs$hs %in% vdma.hs]))


# Simon 2020-05-19:
# Map 4 (true for all sectoral maps 4 as well): 
# Download MFN tariffs for target countries, calculate simple average tariff

# Find out which product categories are needed
unique(substr(vdma.hs,0,2))

groups <- list()

for (gr in vdma.sectors[vdma.sectors != 0]) {
  eval(parse(text=paste0("groups <- c(groups,list(list(",gr,", c(unique(cpc.to.hs$hs[cpc.to.hs$cpc == gr & cpc.to.hs$hs %in% vdma.hs])))))")))
}

sets <- as.numeric(unique(unique(substr(vdma.hs,0,2))))
sets <- sets[sets!=84]
sets <- c(sets, 84.1, 84.2)

tariffs <- data.frame()

for (set in sets) {
  dataset <- read_excel(paste0(project.path,"data/all_tariffs_",set,".xls"))
  dataset <- dataset[5:nrow(dataset),c(1:5,9:11)]
  names(dataset) <- c("country","year","HS.level","HS.version","HS.code","av.tariff","tariff.min","tariff.max")
  tariffs <- rbind(tariffs, dataset)
  rm(dataset)
}

dataset <- tariffs

dataset = subset(dataset, HS.level == 6)
# dataset = subset(dataset, HS.level == 6 & !(HS.code %in% c("380840","380859")))
# dataset = subset(dataset, !(country %in% c("Poland", "Slovak Republic", "Czech Republic", "Malta", "Cyprus", "Hungary", "Latvia", "Lithuania",
                                           # "Estonia", "Slovenia", "Bulgaria", "Croatia")))

## Add country info
countries <- gtalibrary::country.names
dataset$country[dataset$country=="European Union"] <- paste(subset(countries, un_code %in% unique(subset(gtalibrary::country.correspondence,name=="EU-28")$un_code))$name, collapse = ";")
dataset <- cSplit(indt = dataset, splitCols = "country", sep = ";", direction = "long")
dataset$country <- as.character(dataset$country)
dataset$country[dataset$country=="Antigua and Barbuda"] <- "Antigua & Barbuda"
dataset$country[dataset$country=="Bahrain, Kingdom of"] <- "Bahrain"
dataset$country[dataset$country=="Bolivia, Plurinational State of"] <- "Bolivia"
dataset$country[dataset$country=="Cabo Verde"] <- "Cape Verde"
dataset$country[dataset$country=="C?te d'Ivoire"] <- "Ivory Coast"
dataset$country[dataset$country=="Côte d'Ivoire"] <- "Ivory Coast"
dataset$country[dataset$country=="Democratic Republic of the Congo"] <- "DR Congo"
dataset$country[dataset$country=="Eswatini"] <- "Swaziland"
dataset$country[dataset$country=="European Union"] <- "European Union"
dataset$country[dataset$country=="Hong Kong, China"] <- "Hong Kong"
dataset$country[dataset$country=="Korea, Republic of"] <- "Republic of Korea"
dataset$country[dataset$country=="Kuwait, the State of"] <- "Kuwait"
dataset$country[dataset$country=="Kyrgyz Republic"] <- "Kyrgyzstan"
dataset$country[dataset$country=="Lao People's Democratic Republic"] <- "Lao"
dataset$country[dataset$country=="Macao, China"] <- "Macao"
dataset$country[dataset$country=="Moldova, Republic of"] <- "Republic of Moldova"
dataset$country[dataset$country=="North Macedonia"] <- "Macedonia"
dataset$country[dataset$country=="Russian Federation"] <- "Russia"
dataset$country[dataset$country=="Saint Kitts and Nevis"] <- "Saint Kitts & Nevis"
dataset$country[dataset$country=="Saint Vincent and the Grenadines"] <- "Saint Vincent & the Grenadines"
dataset$country[dataset$country=="Saudi Arabia, Kingdom of"] <- "Saudi Arabia"
dataset$country[dataset$country=="The Gambia"] <- "Gambia"
dataset$country[dataset$country=="Trinidad and Tobago"] <- "Trinidad & Tobago"
dataset$country[dataset$country=="Venezuela, Bolivarian Republic of"] <- "Venezuela"
dataset$country[dataset$country=="Viet Nam"] <- "Vietnam"
dataset$country[dataset$country=="Czech Republic"] <- "Czechia"
dataset$country[dataset$country=="Slovak Republic"] <- "Slovakia"
dataset <- merge(dataset, unique(subset(countries, select = c("un_code", "name"))), by.x = "country", by.y = "name", all.x = T)
unique(subset(dataset, is.na(un_code))$country)



# Convert HS codes to HS2012
hs.codes = data.frame(hs.code = as.character(unique(dataset$HS.code)))
for(row in 1:nrow(hs.codes)){
  hs.codes$converted.codes[row] = paste(gta_hs_vintage_converter(hs.codes$hs.code[row]), collapse = ";")
};rm(row)
hs.codes = splitstackshape::cSplit(hs.codes, "converted.codes", sep = ";", direction = "long")
hs.codes$hs6 = as.numeric(hs.codes$converted.codes)
hs.codes$hs.code = as.character(hs.codes$hs.code)

#convert to numeric
dataset$av.tariff <- as.numeric(dataset$av.tariff)
dataset$tariff.min <- as.numeric(dataset$tariff.min)
dataset$tariff.max <- as.numeric(dataset$tariff.max)

dataset = merge(dataset, subset(hs.codes, select = c("hs.code", "hs6")), by.x = "HS.code", by.y = "hs.code", all.x = T)
tariff.dataset = aggregate(av.tariff ~ country + un_code + hs6, dataset, mean)
tariff.dataset = merge(tariff.dataset, aggregate(tariff.min ~ country + un_code + hs6, dataset, min), by = c("country", "un_code", "hs6"), all.x = T)
tariff.dataset = merge(tariff.dataset, aggregate(tariff.max ~ country + un_code + hs6, dataset, max), by = c("country", "un_code", "hs6"), all.x = T)
tariff.dataset$group = NA

# Filter to hs codes in vdma set
tariff.dataset <- subset(tariff.dataset, hs6 %in% vdma.hs)

i=1
for(i in 1:length(groups)){
  tariff.dataset$group[tariff.dataset$hs6 %in% unlist(groups[[i]][2])] <- unlist(groups[[i]][1])
}
tariff.dataset = subset(tariff.dataset, is.na(group)==F)

# Add world group
tariff.dataset.world <- tariff.dataset
tariff.dataset.world$group = 0
tariff.dataset <- rbind(tariff.dataset, tariff.dataset.world)
rm(tariff.dataset.world)
table(tariff.dataset$group)

statistics = expand.grid(un_code = unique(tariff.dataset$un_code), group = unique(tariff.dataset$group))

## Average non-weighted tariff height for each 6-digit HS code in a given group
statistics = merge(statistics,
                   aggregate(av.tariff ~ group + country + un_code, tariff.dataset, mean),
                   by=c("group", "un_code"), all.x = T)
names(statistics)[4] = "average.tariff"

openxlsx::write.xlsx(statistics, file = paste0(project.path,"data/avg.tariffs.xlsx"))
save(statistics, file = paste0(project.path,"data/avg.tariffs.Rdata"))

openxlsx::write.xlsx(tariff.dataset, file = paste0(project.path,"data/tariff.dataset.xlsx"))
save(tariff.dataset, file = paste0(project.path,"data/tariff.dataset.Rdata"))


# SIMON 2020-05-19: 3G (New Map): Level of simple average tariff faced by foreign firms,
# Countries with free trade agreement should be white 

gta_trade_value_bilateral(importing.country = 276,
                          keep.importer = T,
                          hs.codes= vdma.hs,
                          keep.hs=T,
                          trade.data=2016)
tbb=trade.base.bilateral

gta_trade_value_bilateral(importing.country = 276,
                          keep.importer = T,
                          hs.codes= vdma.hs,
                          keep.hs=T,
                          trade.data=2017)
tbb=rbind(trade.base.bilateral, tbb)

gta_trade_value_bilateral(importing.country = 276,
                          keep.importer = T,
                          hs.codes= vdma.hs,
                          keep.hs=T,
                          trade.data=2018)
trade.base.bilateral=rbind(trade.base.bilateral, tbb)
rm(tbb)

trade.base.bilateral=aggregate(trade.value ~ i.un + a.un + hs6, trade.base.bilateral, function(x) sum(x)/3) ##` accounts for zeros in some year(s!`


# check which countries germany does not import from
trade.partners=unique(trade.base.bilateral$a.un)
not.trade.partners <- gtalibrary::country.names$un_code[! gtalibrary::country.names$un_code %in% trade.partners]

# merge avg tariffs to trade data
trade.tariffs <- unique(merge(trade.base.bilateral[,c("a.un","hs6")], tariff.dataset[,c("country","un_code","hs6","av.tariff")], by.x = c("a.un","hs6"), by.y = c("un_code","hs6"), all.x=T))

## Average non-weighted tariff height for each 6-digit HS code in a given group
import.statistics = aggregate(av.tariff ~ country + a.un, trade.tariffs, mean)
names(import.statistics)[3] = "average.tariff"

openxlsx::write.xlsx(import.statistics, file = paste0(project.path,"data/avg.import.tariffs.xlsx"))
save(import.statistics, file = paste0(project.path,"data/avg.import.tariffs.Rdata"))


# CLEAN FTA country data:
fta <- read.csv(paste0(project.path, "/help files/EU Free Trade Agreements.csv"), sep=",",header = F)
names(fta) <- "name"
fta$name <- as.character(fta$name)
fta$name <- trimws(fta$name, which = "both")
fta <- merge(fta, gtalibrary::country.names[,c("un_code","name")], by="name", all.x=T)
# Somehow that doesn't work for all countries, adding moldova, palestine manually
# fta$un_code[57] <- 498
# fta$un_code[68] <- 275
save(fta, file = paste0(project.path,"data/eu.fta.Rdata"))
