rm(list=ls())
library(gtalibrary)
library(ggplot2)
library(tidyverse)

# setwd("GTA cloud"
gta_setwd()

project.path="0 projects/39 VDMA/"

load(paste0(project.path,"data/avg.tariffs.Rdata"))
load(paste0(project.path,"data/avg.import.tariffs.Rdata"))
load(paste0(project.path,"data/eu.fta.Rdata"))
load(paste0(project.path,"data/VDMA data.Rdata"))
load(paste0(project.path,"data/VDMA data - GER importer.Rdata"))
source(paste0(project.path, "help files/definitions.R"))

vdma.country.un=gtalibrary::country.names$un_code[gtalibrary::country.names$name %in% vdma.countries$gta.name]

gta_colour_palette()

# DEFINITIONS
marked.country = 276
# color.low = gta_colour$red[4]
color.low = "#f6f6f6"
color.high = gta_colour$red[1]
output.path = paste0(project.path,"results/maps/")

cpc.titles=read.csv(paste0(project.path,"help files/VDMA sector titles.csv"), stringsAsFactors = F, sep=";")
names(cpc.titles)=c("cpc","cpc.name")

# "Share of German exports affected in {CPC 3-digit} by {VDMA instrument}"

# check if any of the 40 trading partners has 0 trade
nrow(subset(vdma.master, importer.un %in% vdma.country.un & trade.share == -1))

# Mapping function
map <- function(data, legend.name, color.high, color.low, marked.country, title, caption, values = c(0,0.2,1), limits = c(0,1), breaks=seq(0,1,.2), white.countries = c()) { 
  plot = ggplot() +
  geom_polygon(data= subset(world, country != "Antarctica"), aes(x = long, y = lat, group = group, fill = value), size = 0.15, color = "white") +
  # geom_polygon(data=subset(world, UN %in% subset(world, value==0)$UN & UN %in% vdma.country.un), aes(x=long, y=lat, group = group), fill="#f6f6f6", size = 0.15, colour = "white") +
  geom_polygon(data=subset(world, ((UN %in% subset(world, value==-1)$UN) | (! UN %in% vdma.country.un)) &  country != "Antarctica"), aes(x=long, y=lat, group = group), fill="#c6c6c6", size = 0.15, colour = "white") +
  geom_polygon(data=subset(world, (UN %in% subset(world, value==-1)$UN) & (UN %in% vdma.country.un)), aes(x=long, y=lat, group = group), fill="#c6c6c6", size = 0.15, colour = "white") + ## <- This is a line to signal top 40 countries with no exports.
  geom_polygon(data=subset(world, country == "Greenland"), aes(x=long, y=lat, group = group), fill="#c6c6c6", size = 0.15, colour = "white") +
  geom_polygon(data=subset(world, UN %in% white.countries & country != "Antarctica"), aes(x=long, y=lat, group = group), fill="#f9f9f9", size = 0.15, colour = "white") +
  geom_polygon(data=subset(world, UN == marked.country), aes(x=long, y=lat, group = group), fill="#414141", size = 0.15, colour = "white") +
  coord_fixed() + # Important to fix world map proportions
  scale_x_continuous(limits=c(-13900000,17000000))+
  labs(x="", y="", caption = caption) +
  ggtitle(title) +
  scale_fill_gradientn(name=legend.name,
                      na.value="#c6c6c6",
                      limits=limits,
                      colors = c(gta_colour$green[1], "#ffdd00", gta_colour$red[1]),
                      labels=scales::percent,
                      values = values,
                      breaks=breaks,
                      # labels=c("0","250","500","750"),
                      guide=guide_colorbar(barwidth=15, label.hjust = 0.5, title.position = "top"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.53,-0.1),
        legend.justification = c(0.5,0),
        legend.direction = "horizontal",
        plot.title = element_text(family = "", face = "bold", colour = "#333333", size = 10, hjust = 0.5, margin = margin(b=10)),
        legend.title = element_text(vjust= 0.3, family="", colour = "#333333", size = 11*0.8, margin = margin(r=10,b=5)),
        legend.text = element_text(family="", colour = "#333333", size = 11*0.8, angle = 0, hjust=0, vjust=0, margin = margin(r=10)),
        legend.text.align = 0,
        legend.background = element_rect(fill="transparent"),
        plot.background = element_rect(fill="#e6e6e6"),
        plot.margin = unit(c(0.05,0.03,0.05,0.03), "npc"),
        plot.caption = element_text(hjust = 0.5, margin = margin(t=30),size=8, color="#777777"),
        legend.margin = margin(b=20,l=20)
  )


return (plot)
}




vdma.master$instrument[vdma.master$instrument=="Third party export incentives"]="third-party export incentives"
vdma.master$instrument[vdma.master$instrument=="Importer TBT"]="technical barriers to trade"
vdma.master$instrument[vdma.master$instrument=="Importer subsidies to local firms"]="subsidies granted by the importer to local firms"
vdma.master$instrument[vdma.master$instrument=="Importer tariff increases"]="simple average tariff raised by the importer"
vdma.master$instrument[vdma.master$instrument=="All other importer policies that limit imports"]="any other trade barrier raised by the importer"


instrument.order=c("third-party export incentives",
                   "technical barriers to trade",
                   "subsidies granted by the importer to local firms",
                   "simple average tariff raised by the importer",
                   "any other trade barrier raised by the importer")

i.nr=0
for (instr in instrument.order) {
  
  i.nr=i.nr+1
  
  print(instr)
  
  for (c in unique(vdma.master$cpc)) {

    # Simple average MFN tariffs instead of trade shares for map 4
    if (instr == "simple average tariff raised by the importer") {
    
    world <- gta_plot_map_df(data=subset(statistics, group == c),
                               countries="un_code",
                               values="average.tariff") 
    world$value <- world$value/100
    
    # Get max value of g20 countries
    maxG20 <- max(world$value[world$UN %in% country.names$un_code[country.names$is.g20]],na.rm = T)
    maxValue <- ifelse(maxG20 > 0.25, maxG20, 0.25)

    if(c==0){
      
      title = paste0("Exposure of German exports of ",tolower(cpc.titles$cpc.name[cpc.titles$cpc==c]),"\nto ",instr)
      plot.name=paste0("Sector ",c," (",cpc.titles$cpc.name[cpc.titles$cpc==c],") - Map ",i.nr," (",instr,")")
      
    } else {
      
      title = paste0("Exposure of German exports of '",cpc.titles$cpc.name[cpc.titles$cpc==c],"' (CPC code ",c,")\nto ",instr)
      plot.name=paste0("Sector ",c," (",cpc.titles$cpc.name[cpc.titles$cpc==c],") - Map ",i.nr," (",instr,")")
    }
    
    map1 <- map(title = title, 
                data=world,
                legend.name = paste0("Simple average tariff raised by importing country"),
                color.high = color.high,
                limits = c(0,maxValue),
                values = c(0,0.1,maxValue,1),
                breaks = c(seq(0,maxValue,0.1),maxValue),
                color.low = color.low,
                marked.country = marked.country,
                white.countries = unique(c(fta$un_code,country.names$un_code[country.names$is.eu==T])),
                caption = "Sources: Global Trade Alert and WTO TBT databases, February 2020.\nNote: Countries benefiting from a Free Trade Agreement are marked in white.")
    
    map1
    
    gta_plot_saver(plot=map1,
                   path=output.path,
                   name=plot.name,
                   pdf=T,
                   width = 21,
                   height = 14.2)
    
    openxlsx::write.xlsx(unique(data.frame(country = world$country,
                                           value = world$value)),
                         file = paste0(output.path,plot.name," - data.xlsx"))
    
    rm(map1)
    rm(world)
    print(c)
      
    } else {
    
    world <- gta_plot_map_df(data=subset(vdma.master, instrument == instr & cpc == c),
                             countries="importer.un",
                             values="trade.share")

    if(c==0){
      
      title = paste0("Exposure of German exports of ",tolower(cpc.titles$cpc.name[cpc.titles$cpc==c]),"\nto ",instr)
      plot.name=paste0("Sector ",c," (",cpc.titles$cpc.name[cpc.titles$cpc==c],") - Map ",i.nr," (",instr,")")
      
    } else {
      
      title = paste0("Exposure of German exports of '",cpc.titles$cpc.name[cpc.titles$cpc==c],"' (CPC code ",c,")\nto ",instr)
      plot.name=paste0("Sector ",c," (",cpc.titles$cpc.name[cpc.titles$cpc==c],") - Map ",i.nr," (",instr,")")
    }

    # Plot Map
    #linebreak(title, 9)
    map1 <- map(title = title, 
                data=world,
                legend.name = paste0("Share of German exports affected on 1 January 2020"),
                color.high = color.high,
                color.low = color.low,
                marked.country = marked.country,
                caption = "Sources: Global Trade Alert and WTO TBT databases, February 2020.")

    map1

    gta_plot_saver(plot=map1,
                   path=output.path,
                   name=plot.name,
                   pdf=T,
                   width = 21,
                   height = 14.2)
    
    openxlsx::write.xlsx(unique(data.frame(country = world$country,
                                           value = world$value)),
                         file = paste0(output.path,plot.name," - data.xlsx"))
     rm(map1)
     rm(world)
     print(c)
     
    }

  }
  
  
}

# SIMON 2020-05-19: Provide underlying data for all maps Sector 0, 1-5, in one excel
mapsdata <- vdma.master[vdma.master$cpc==0 & vdma.master$instrument != "simple average tariff raised by the importer",c("importer.un","trade.share","instrument")]
mapsdata <- rbind(mapsdata, data.frame(importer.un = statistics$un_code[statistics$group==0],
                                        trade.share = statistics$average.tariff[statistics$group==0],
                                       instrument = 4))

mapsdata$instrument[mapsdata$instrument=="third-party export incentives"] <- 1
mapsdata$instrument[mapsdata$instrument=="technical barriers to trade"] <- 2
mapsdata$instrument[mapsdata$instrument=="subsidies granted by the importer to local firms"] <- 3
mapsdata$instrument[mapsdata$instrument=="any other trade barrier raised by the importer"] <- 5
mapsdata$instrument <- as.numeric(mapsdata$instrument)
mapsdata <- mapsdata[with(mapsdata, order(instrument)),]
rownames(mapsdata) <- NULL
mapsdata$instrument <- paste0("Map ",mapsdata$instrument)

mapsdata.xlsx <- as.data.frame(pivot_wider(mapsdata, names_from = "instrument", values_from = "trade.share"))
mapsdata.xlsx <- merge(mapsdata.xlsx, gtalibrary::country.names[,c("name","un_code")], by.x = "importer.un", by.y="un_code")
mapsdata.xlsx <- mapsdata.xlsx[,c("name","Map 1","Map 2","Map 3","Map 4","Map 5")]
names(mapsdata.xlsx) <- c("Country","Map 1 (trade share)","Map 2 (trade share)","Map 3 (trade share)","Map 4 (simple average tariff)","Map 5 (trade share)")
openxlsx::write.xlsx(mapsdata.xlsx, file = paste0(project.path,"results/xlsx output/main map data.xlsx"))


# IMPORTER MAPS

vdma.master.imports$instrument[vdma.master.imports$instrument=="Importer TBT"]="technical barriers to trade"
vdma.master.imports$instrument[vdma.master.imports$instrument=="Importer subsidies to local firms"]="subsidies granted by Germany to local firms"
vdma.master.imports$instrument[vdma.master.imports$instrument=="Importer tariff increases"]="tariffs raised by the European Union"
vdma.master.imports$instrument[vdma.master.imports$instrument=="All other importer policies that limit imports"]="any other trade barrier raised by Germany"
vdma.master.imports$instrument[vdma.master.imports$instrument=="Foreign exports benefiting from export incentives"]="foreign export inventives"



instrument.importer.order=c("technical barriers to trade",
                            "subsidies granted by Germany to local firms",
                            "tariffs raised by the European Union",
                            # "any other trade barrier raised by Germany",
                            "foreign export inventives")
i.nr = 0
for (instr in instrument.importer.order) {
    
  i.nr = i.nr+1
  
  # SIMON 2020-05-19: 3G (New Map): Level of simple average tariff faced by foreign firms,
  # Countries with free trade agreement should be white 
  if (i.nr == 3) {
    title = paste0("Average tariff faced by foreign firms")
    plot.name=paste0("German imports (All mechanical and plant engineering products) - Map 3 (average tariffs faced by foreign firms)")
    
    world <- gta_plot_map_df(data=import.statistics,
                             countries="a.un",
                             values="average.tariff")
    world$value <- world$value/100
    
    # Get max value of g20 countries
    maxG20 <- max(world$value[world$UN %in% country.names$un_code[country.names$is.g20]],na.rm = T)
    maxValue <- ifelse(maxG20 > 0.25, maxG20, 0.25)
    
    map1 <- map(title = title, 
                data=world,
                legend.name = paste0("Simple average tariff affecting foreign firms"),
                color.high = color.high,
                limits = c(0,maxValue),
                values = c(0,0.1,maxValue,1),
                breaks = c(seq(0,maxValue,0.1),maxValue),
                color.low = color.low,
                marked.country = marked.country,
                white.countries = unique(c(fta$un_code,country.names$un_code[country.names$is.eu==T])),
                caption = "Sources: Global Trade Alert and WTO TBT databases, February 2020.\nNote: Countries benefiting from a Free Trade Agreement are marked in white.")
    
    map1
    
    gta_plot_saver(plot=map1,
                   path=output.path,
                   name=plot.name,
                   pdf=T,
                   width = 21,
                   height = 14.2)
    
    openxlsx::write.xlsx(unique(data.frame(country = world$country,
                                           value = world$value)),
                         file = paste0(output.path,plot.name," - data.xlsx"))
    
    rm(map1)
    rm(world)

  } else {
    
    # Create maps for germany as importer
    title = paste0("Exposure of German imports to\n",instr)
    plot.name=paste0("German imports (All mechanical and plant engineering products) - Map ",i.nr," (",instr,")")
    
    world <- gta_plot_map_df(data=subset(vdma.master.imports, instrument == instr),
                             countries="exporter.un",
                             values="trade.share")
    
    # Apparently map plotting function has problems with only 1 and NA values, adding a 0 where it doesn't matter
    if (instr == "tariffs raised by Germany") {
      world$value[world$country=="Haiti"] <- 0
    }
    
    map1 <- map(title = title, 
                data=world,
                legend.name = paste0("Share of German imports affected on 1 January 2020"),
                color.high = color.high,
                color.low = color.low,
                marked.country = marked.country,
                caption = "Sources: Global Trade Alert and WTO TBT databases, February 2020.\nNote: Priority destinations with zero German exports between 2016 and 2018 marked in blue.")
    
    map1
    
    gta_plot_saver(plot=map1,
                   path=output.path ,
                   name=plot.name,
                   pdf=T,
                   width = 21,
                   height = 14.2)
    
    openxlsx::write.xlsx(unique(data.frame(country = world$country,
                                           value = world$value)),
                         file = paste0(output.path,plot.name," - data.xlsx"))
    
    rm(map1)
    rm(world)
    
  }

}


# save maps in folder "results/maps"
