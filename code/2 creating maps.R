rm(list=ls())
library(gtalibrary)
library(ggplot2)

# setwd("GTA cloud"
gta_setwd()

project.path="0 projects/39 VDMA/"

load(paste0(project.path,"data/VDMA data.Rdata"))
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

# Mapping function
map <- function(data, legend.name, color.high, color.low, marked.country, title, caption) { plot = ggplot() +
  geom_polygon(data= subset(world, country != "Antarctica"), aes(x = long, y = lat, group = group, fill = value), size = 0.15, color = "white") +
  geom_polygon(data=subset(world, UN == marked.country), aes(x=long, y=lat, group = group), fill="#414141", size = 0.15, colour = "white") +
  geom_polygon(data=subset(world, UN %in% subset(world, value==0)$UN & UN %in% vdma.country.un), aes(x=long, y=lat, group = group), fill="#f6f6f6", size = 0.15, colour = "white") +
  geom_polygon(data=subset(world, (UN %in% subset(world, value==-1)$UN) | (! UN %in% vdma.country.un)), aes(x=long, y=lat, group = group), fill="#c6c6c6", size = 0.15, colour = "white") +
  geom_polygon(data=subset(world, (UN %in% subset(world, value==-1)$UN) & (UN %in% vdma.country.un)), aes(x=long, y=lat, group = group), fill=gta_colour$blue[2], size = 0.15, colour = "white") + ## <- This is a line to signal top 40 countries with no exports.
  geom_polygon(data=subset(world, country == "Greenland"), aes(x=long, y=lat, group = group), fill="#c6c6c6", size = 0.15, colour = "white") +
  coord_fixed() + # Important to fix world map proportions
  scale_x_continuous(limits=c(-13900000,17000000))+
  labs(x="", y="", caption = caption) +
  ggtitle(title) +
  scale_fill_gradient(name=legend.name,
                      na.value="#c6c6c6",
                      low = color.low,
                      high = color.high,
                      labels=scales::percent,
                      breaks=seq(0,1,.2),
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

# Function to auto linebreak text
linebreak <- function(str, n) {
  string <- strsplit(str, split = " ")[[1]]
  new.string <- c()
  counter = 1
  while (length(string)>0) {
    if (counter %% n == 0) {
      new.string <- c(new.string, "\n")
    } else {
      new.string <- c(new.string, paste0(string[1]," "))
      string <- string[-1]
    }
    counter=counter+1
  }
  return(paste0(new.string,collapse=""))
}




vdma.master$instrument[vdma.master$instrument=="Third party export incentives"]="third-party export incentives"
vdma.master$instrument[vdma.master$instrument=="Importer TBT"]="technical barriers to trade"
vdma.master$instrument[vdma.master$instrument=="Importer subsidies to local firms"]="subsidies granted by the importer to local firms"
vdma.master$instrument[vdma.master$instrument=="Importer tariff increases"]="tariffs raised by the importer"
vdma.master$instrument[vdma.master$instrument=="All other importer policies that limit imports"]="any other trade barrier raised by the importer"

  
instrument.order=c("third-party export incentives",
                   "technical barriers to trade",
                   "subsidies granted by the importer to local firms",
                   "tariffs raised by the importer",
                   "any other trade barrier raised by the importer")


i.nr=0
for (instr in instrument.order) {
  
  i.nr=i.nr+1
  
  print(instr)
  for (c in unique(vdma.master$cpc)) {

    world <- gta_plot_map_df(data=subset(vdma.master, instrument == instr & cpc == c & trade.share != -1),
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
                caption = "Sources: Global Trade Alert and WTO TBT databases, January 2020.\nNote: Destinations with zero German exports between 2016 and 2018 marked in grey.")

    map1

    gta_plot_saver(plot=map1,
                   path=output.path ,
                   name=plot.name,
                   pdf=T,
                   width = 21,
                   height = 14.2)
     rm(map1)
     print(c)

  }
}


# save maps in folder "results/maps"
