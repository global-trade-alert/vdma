rm(list=ls())
library(gtalibrary)
library(splitstackshape)
library(dplyr)

gta_setwd()

path = "0 projects/39 VDMA/"



## all data
load(paste0(path,"data/VDMA data.Rdata"))
cpc.titles=read.csv(paste0(path,"help files/VDMA sector titles.csv"), stringsAsFactors = F, sep=";")
names(cpc.titles)=c("cpc","cpc.name")


vdma.master$instrument[vdma.master$instrument=="all GTA and WTO TBT interventions"]="any foreign policy intervention incl. export incentives plus technical barriers to trade"
vdma.master$instrument[vdma.master$instrument=="all GTA import-related and WTO TBT interventions"]="policies implemented by the importer incl. technical barriers to trade"
vdma.master$instrument[vdma.master$instrument=="WTO TBT interventions"]="technical barriers to trade"
vdma.master$instrument[vdma.master$instrument=="all GTA import-related interventions"]="policies implemented by the importer excl. technical barriers to trade"


vdma.master=merge(vdma.master, country.names[,c("un_code","name")], by.x="importer.un", by.y="un_code", all.x=T)
vdma.master=merge(vdma.master, cpc.titles, by="cpc", all.x=T)
vdma.master=vdma.master[,c("cpc","cpc.name", "name" ,"instrument", "trade.share" )]
names(vdma.master)=c("CPC sector code","CPC sector name", "Export destination" ,"Policy instrument group", "Share of German sectoral exports affected" )


vdma.master$`Share of German sectoral exports affected`[vdma.master$`Share of German sectoral exports affected`==-1]="no exports to destination"

xlsx::write.xlsx(vdma.master, file=paste0(path,"results/VDMA project data.xlsx"),row.names = F, sheetName = "Trade coverage data")

# 
# 
# #Load necessary files
# vdma <- openxlsx::read.xlsx(paste0(path,"help files/VDMA HS codes to classify.xlsx"),sheet=2)
# gta <- gtalibrary::hs.codes
# ics <- gtalibrary::ics.names
# 
# # Prettify each set
# vdma.cut <- vdma[,c("ICS.codes","English.description","German.description","HS.(6-digit)")]
# names(vdma.cut) <- c("ics.code","en.description","de.description","hs.code")
# vdma.cut$hs.code <- as.numeric(vdma.cut$hs.code)
# gta.cut <- gta[,c("hs.code","hs.description")]
# gta.cut$hs.description = iconv(gta.cut$hs.description, to="UTF-8") # Somehow had encoding issues in this column when saving as xlsx
# ics.cut <- ics[,c("ics.code","ics.description")]
# 
# # Add to xlsx export as sheet
# xlsx <- list("ICS" = ics[,c("ics.code","ics.description","ics.level")])
# xlsx <- c(xlsx, list("GTA HS" = gta.cut))
# xlsx <- c(xlsx, list("VDMA" = unique(vdma.cut[,c("hs.code","en.description","de.description")])))
# 
# # Create conversion file
# conversion <- cSplit(vdma.cut, which(colnames(vdma.cut)=="ics.code"), direction="long", sep=",")
# conversion <- merge(conversion, ics.names[,c("ics.code","ics.description","ics.level")], by="ics.code")
# conversion <- merge(conversion, gta.cut, by="hs.code",all.x=T)
# conversion <- conversion[with(conversion, order(ics.code)),]
# names(conversion) <-  c("hs.code", "ics.code", "vdma.en.description", "vdma.de.description", "ics.description", "ics.level", "gta.hs.description")
# conversion <- conversion[,c("ics.code", "ics.description", "hs.code", "vdma.en.description", "vdma.de.description", "ics.level", "gta.hs.description")]
# 
# # export xlsx
# xlsx <- c(list("Conversion"=conversion), xlsx)
# 
# # Saving problems because of non utf-encoding
# # openxlsx::write.xlsx(x = xlsx, file=paste0(path,"results/VDMA hs and ics codes.xlsx"),rowNames=F)
# 
# xlsx::write.xlsx(ics[,c("ics.code","ics.description")], file=paste0(path,"results/VDMA project data.xlsx"),row.names = F, sheetName = "ICS nomenclature", append = T)
 