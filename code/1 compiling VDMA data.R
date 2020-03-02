library(gtalibrary)
rm(list = ls())
gta_setwd()

project.path="0 projects/39 VDMA"
source(paste0(project.path, "/help files/definitions.R"))


cutoff.date="2020-01-01"
vdma.sectors=c(0,unique(cpc.to.hs$cpc[cpc.to.hs$hs %in% vdma.hs]))

## Germany as the exporter

## trade data, taking the average observed trade between 2016 and 2018
gta_trade_value_bilateral(exporting.country = 276,
                          keep.exporter = T,
                          hs.codes= vdma.hs,
                          keep.hs=T,
                          trade.data=2016)
tbb=trade.base.bilateral

gta_trade_value_bilateral(exporting.country = 276,
                          keep.exporter = T,
                          hs.codes= vdma.hs,
                          keep.hs=T,
                          trade.data=2017)
tbb=rbind(trade.base.bilateral, tbb)

gta_trade_value_bilateral(exporting.country = 276,
                          keep.exporter = T,
                          hs.codes= vdma.hs,
                          keep.hs=T,
                          trade.data=2018)
trade.base.bilateral=rbind(trade.base.bilateral, tbb)
rm(tbb)

trade.base.bilateral=aggregate(trade.value ~ i.un + a.un + hs6, trade.base.bilateral, function(x) sum(x)/3) ##` accounts for zeros in some year(s!`



# check which countries germany does not export to
trade.partners=unique(trade.base.bilateral$i.un)
not.trade.partners <- gtalibrary::country.names$un_code[! gtalibrary::country.names$un_code %in% trade.partners]

#### loading TBT data
load("data/WTO SPS & TBT/WTO SPS & TBT database.Rdata")

## filtering notifications for TBT only, non-removed and if targeted, than at Germany plus announced in GTA coverage period
notifications=subset(notifications, i.un %in% trade.partners )
notifications=subset(notifications, intervention.type=="Technical Barrier to Trade")
notifications=subset(notifications, (is.na(date.removed)|date.removed>=cutoff.date))
nongerman.targets=targeted.jurisdictions$wto.id[! targeted.jurisdictions$wto.id %in% subset(targeted.jurisdictions, a.un==276)$wto.id]
notifications=subset(notifications, ! wto.id %in% nongerman.targets)
notifications=subset(notifications, date.announced>="2008-11-01")

## could add a filter for the presence of an implementation date ... 



## converting ics
products.ics=subset(products.ics, wto.id %in% notifications$wto.id)
products.ics=merge(products.ics, gtalibrary::ics.to.hs, by.x="ics.code",by.y="ics")
products.ics$ics.code=NULL
products.ics=unique(products.ics)

names(products.ics)=c("wto.id",  "hs.code")

products.hs=unique(rbind(products.hs, products.ics))

tbt.affected=merge(notifications[,c("wto.id","i.un")], products.hs, by="wto.id", all.x=T)
tbt.affected=subset(tbt.affected, ! i.un %in% country.names$un_code[country.names$is.eu]) ## avoid EU cases

tbt.affected=unique(tbt.affected[,c("i.un","hs.code")])
names(tbt.affected)=c("i.un","affected.product")
tbt.affected$affected=1

# UPDATE 24.2.2020
# Replace Maps 1-4 with five maps, each showing export exposure to a 
# different (mutually exclusive) class of trade distortions. The five classes are 

affected <- list()

###### (a) third party export incentives, 

#### GTA os
gta_data_slicer(in.force.on.date = cutoff.date,
                keep.in.force.on.date = "Yes",
                affected.flows = "outward subsidy",
                gta.evaluation = c("red","amber"),
                affected.country = "Germany",
                keep.affected = T,
                hs.codes = vdma.hs,
                keep.hs=T,
                intervention.types = c("Competitive devaluation","Consumption subsidy"),
                keep.type = F)
master.sliced=subset(master.sliced, a.un==276)
gta.os.interventions=unique(master.sliced$intervention.id)
gta.tl=read.csv("data/database replica/gta_tariff_line.csv")
gta.jur=read.csv("data/database replica/gta_jurisdiction.csv")
gta.it=read.csv("data/database replica/gta_it_revised.csv")
gta.it=subset(gta.it, intervention_id %in% gta.os.interventions & affected_jurisdiction_id == gta.jur$id[gta.jur$name=="Germany"])[,c("distorted_market_id","tariff_line_id")]
gta.it=merge(gta.it, gta.tl[,c("id","code")], by.x="tariff_line_id", by.y="id", all.x=T)
gta.it=merge(gta.it, gta.jur[,c("id","un_code")], by.x="distorted_market_id", by.y="id", all.x=T)

gta.it=unique(gta.it[,c("un_code","code")])
names(gta.it)=c("i.un","affected.product")
gta.it$affected=1

gta.outward.subsidy=subset(gta.it, affected.product %in% vdma.hs)
rm(gta.it, gta.jur, gta.tl)

affected <- c(affected, list("a"=list("Third party export incentives",gta.outward.subsidy)))

# (b) importer TBT,

affected <- c(affected, list("b"=list("Importer TBT",tbt.affected)))

# (c) importer subsidies to local firms, 
# (d) importer tariff increases, and
# (e) all other importer policies that limit imports.

types <- gtalibrary::int.mast.types

int.types <- list(c("c","Importer subsidies to local firms",list(types$intervention.type[types$mast.chapter.id %in% c("L")])),
                  c("d","Importer tariff increases",list(types$intervention.type[types$mast.chapter.id %in% c("TARIFF")])),
                  c("e","All other importer policies that limit imports",list(types$intervention.type[! types$mast.chapter.id %in% c("A","B","CAP","FDI","MIG","N","P","L","TARIFF")])))

for (i in int.types) {
  gta_data_slicer(in.force.on.date = cutoff.date,
                  keep.in.force.on.date = "Yes",
                  affected.flows = "inward",
                  gta.evaluation = c("red","amber"),
                  affected.country = "Germany",
                  keep.affected = T,
                  hs.codes = vdma.hs,
                  keep.hs=T,
                  intervention.types = i[[3]],
                  keep.type = T)
  
  master.sliced=subset(master.sliced, a.un==276)[,c("i.un","a.un","affected.product")]
  master.sliced=cSplit(master.sliced, which(names(master.sliced)=="affected.product"), sep=",", direction = "long")
  temp=subset(master.sliced, affected.product %in% vdma.hs)
  temp$a.un=NULL
  temp$affected=1
  
  eval(parse(text=paste0("affected <- c(affected, list(",i[[1]]," = list('",i[[2]],"',temp)))")))
  rm(temp)
  
}

## The loop below needs to be changed to loop over vdma.instruments. That implies change the code inside the loop too.
##  when you do that, add an if clause for instrument=="TBT measure". in it we will have to compute the trade share without our function.
## I sketch out that TBT calculation at the bottom.

vdma.master=data.frame()

#3 removing sector 483, 448, 452, 469, 491, and 495
vdma.sectors=vdma.sectors[! vdma.sectors %in% c(483, 448, 452, 469, 491, 495)]


for(sector in vdma.sectors){
  
  
  if(sector==0){
    
    sec.codes=vdma.hs
    
  }else{
    
    sec.codes=vdma.hs[vdma.hs %in% cpc.to.hs$hs[cpc.to.hs$cpc==sector]]
  }
  
  estimate.base=subset(trade.base.bilateral, hs6 %in% sec.codes)
  
  
  if(nrow(estimate.base)>0){
    
    for (aff in affected) {
      estimate=merge(estimate.base, unique(rbind(aff[[2]])), by.x=c("i.un","hs6"), by.y=c("i.un","affected.product"), all.x=T)
      
      
      estimate[is.na(estimate)]=0
      
      if(any(estimate$affected==1)){
        
        estimate=merge(aggregate(trade.value ~ i.un, estimate, sum),
                       aggregate(trade.value ~ i.un, subset(estimate, affected==1), sum),
                       by="i.un", all=T)
        
      } else{
        
        estimate=aggregate(trade.value ~ i.un, estimate, sum)
        estimate$trade.value.x=estimate$trade.value
        estimate$trade.value=NULL
        estimate$trade.value.y=0
        
      }
      
      estimate[is.na(estimate)]=0
      
      estimate$trade.share=round(estimate$trade.value.y/estimate$trade.value.x,4)
      
      vdma.master=rbind(vdma.master,
                        data.frame(instrument=aff[[1]],
                                   cpc=sector,
                                   importer.un=estimate$i.un,
                                   trade.share=estimate$trade.share,
                                   stringsAsFactors = F))
      rm(estimate)
      
    }
    
    
    
    ## please add a correction that countries to which Germany does not export get marked with trade.share -1. [use trade_value_bilateral outside the loop for the vdma hs codes]
    non.destination=country.names$un_code[! country.names$un_code %in% subset(trade.base.bilateral, hs6 %in% sec.codes)$i.un]
    
    vdma.master=rbind(vdma.master,
                      data.frame(instrument=affected$a[[1]],
                                 cpc=sector,
                                 importer.un=non.destination,
                                 trade.share=-1,
                                 stringsAsFactors = F))
    
    vdma.master=rbind(vdma.master,
                      data.frame(instrument=affected$b[[1]],
                                 cpc=sector,
                                 importer.un=non.destination,
                                 trade.share=-1,
                                 stringsAsFactors = F))
    
    vdma.master=rbind(vdma.master,
                      data.frame(instrument=affected$c[[1]],
                                 cpc=sector,
                                 importer.un=non.destination,
                                 trade.share=-1,
                                 stringsAsFactors = F))
    
    vdma.master=rbind(vdma.master,
                      data.frame(instrument=affected$d[[1]],
                                 cpc=sector,
                                 importer.un=non.destination,
                                 trade.share=-1,
                                 stringsAsFactors = F))
    
    vdma.master=rbind(vdma.master,
                      data.frame(instrument=affected$e[[1]],
                                 cpc=sector,
                                 importer.un=non.destination,
                                 trade.share=-1,
                                 stringsAsFactors = F))
    rm(estimate)
    
    
  }
  print(sector)
}


save(vdma.master, file=paste0(project.path,"/data/VDMA data.Rdata"))















## Germany as the importer

## trade data, taking the average observed trade between 2016 and 2018
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

#### loading TBT data
load("data/WTO SPS & TBT/WTO SPS & TBT database.Rdata")

## filtering notifications for TBT only, non-removed and if targeted, than at Germany plus announced in GTA coverage period
notifications=subset(notifications, i.un == 276 )
notifications=subset(notifications, intervention.type=="Technical Barrier to Trade")
notifications=subset(notifications, (is.na(date.removed)|date.removed>=cutoff.date))
nonpartner.targets=targeted.jurisdictions$wto.id[! targeted.jurisdictions$wto.id %in% subset(targeted.jurisdictions, a.un %in% trade.partners)$wto.id]
notifications=subset(notifications, ! wto.id %in% nonpartner.targets)
notifications=subset(notifications, date.announced>="2008-11-01")

## could add a filter for the presence of an implementation date ... 



## converting ics
products.ics=subset(products.ics, wto.id %in% notifications$wto.id)
products.ics=merge(products.ics, gtalibrary::ics.to.hs, by.x="ics.code",by.y="ics")
products.ics$ics.code=NULL
products.ics=unique(products.ics)

names(products.ics)=c("wto.id",  "hs.code")

products.hs=unique(rbind(products.hs, products.ics))

tbt.affected=merge(notifications[,c("wto.id","i.un")], products.hs, by="wto.id", all.x=T)

tbt.affected=subset(tbt.affected, is.na(hs.code)==F)
tbt.affected=unique(tbt.affected[,c("i.un","hs.code")])
names(tbt.affected)=c("i.un","affected.product")
tbt.affected$affected=1



## GERMAN interventions.
affected <- list()


# (b) importer TBT,

affected <- c(affected, list("b"=list("Importer TBT",tbt.affected)))

# (c) importer subsidies to local firms, 
# (d) importer tariff increases, and
# (e) all other importer policies that limit imports.

types <- gtalibrary::int.mast.types

int.types <- list(c("c","Importer subsidies to local firms",list(types$intervention.type[types$mast.chapter.id %in% c("L")])),
                  c("d","Importer tariff increases",list(types$intervention.type[types$mast.chapter.id %in% c("TARIFF")])),
                  c("e","All other importer policies that limit imports",list(types$intervention.type[! types$mast.chapter.id %in% c("A","B","CAP","FDI","MIG","N","P","L","TARIFF")])))

for (i in int.types) {
  gta_data_slicer(in.force.on.date = cutoff.date,
                  keep.in.force.on.date = "Yes",
                  affected.flows = "inward",
                  gta.evaluation = c("red","amber"),
                  implementing.country = "Germany",
                  keep.implementer = T,
                  hs.codes = vdma.hs,
                  keep.hs=T,
                  intervention.types = i[[3]],
                  keep.type = T)
  
  master.sliced=subset(master.sliced, i.un==276)[,c("i.un","a.un","affected.product")]
  master.sliced=cSplit(master.sliced, which(names(master.sliced)=="affected.product"), sep=",", direction = "long")
  temp=subset(master.sliced, affected.product %in% vdma.hs)
  temp$i.un=NULL
  temp$affected=1
  
  eval(parse(text=paste0("affected <- c(affected, list(",i[[1]]," = list('",i[[2]],"',temp)))")))
  rm(temp)
}

## The loop below needs to be changed to loop over vdma.instruments. That implies change the code inside the loop too.
##  when you do that, add an if clause for instrument=="TBT measure". in it we will have to compute the trade share without our function.
## I sketch out that TBT calculation at the bottom.

vdma.master=data.frame()
  
  
sec.codes=vdma.hs
estimate.base=subset(trade.base.bilateral, hs6 %in% sec.codes)


if(nrow(estimate.base)>0){
  
  for (aff in affected) {

    if (aff[[1]]=="Importer TBT") {
      estimate=merge(estimate.base, unique(rbind(aff[[2]])), by.x=c("i.un","hs6"), by.y=c("i.un","affected.product"), all.x=T)
    } else {
      estimate=merge(estimate.base, unique(rbind(aff[[2]])), by.x=c("a.un","hs6"), by.y=c("a.un","affected.product"), all.x=T)
    }
    estimate[is.na(estimate)]=0
    
    if(any(estimate$affected==1)){
      
      estimate=merge(aggregate(trade.value ~ a.un, estimate, sum),
                     aggregate(trade.value ~ a.un, subset(estimate, affected==1), sum),
                     by="a.un", all=T)
      
    } else{
      
      estimate=aggregate(trade.value ~ a.un, estimate, sum)
      estimate$trade.value.x=estimate$trade.value
      estimate$trade.value=NULL
      estimate$trade.value.y=0
      
    }
    
    estimate[is.na(estimate)]=0
    
    estimate$trade.share=round(estimate$trade.value.y/estimate$trade.value.x,4)
    
    vdma.master=rbind(vdma.master,
                      data.frame(instrument=aff[[1]],
                                 cpc=0,
                                 exporter.un=estimate$a.un,
                                 trade.share=estimate$trade.share,
                                 stringsAsFactors = F))
    rm(estimate)
    
  }
  
  
  
  ## please add a correction that countries to which Germany does not export get marked with trade.share -1. [use trade_value_bilateral outside the loop for the vdma hs codes]
  non.origin=country.names$un_code[! country.names$un_code %in% subset(trade.base.bilateral, hs6 %in% sec.codes)$a.un]
  
  vdma.master=rbind(vdma.master,
                    data.frame(instrument=affected$b[[1]],
                               cpc=0,
                               exporter.un=non.origin,
                               trade.share=-1,
                               stringsAsFactors = F))
  
  vdma.master=rbind(vdma.master,
                    data.frame(instrument=affected$c[[1]],
                               cpc=0,
                               exporter.un=non.origin,
                               trade.share=-1,
                               stringsAsFactors = F))
  
  vdma.master=rbind(vdma.master,
                    data.frame(instrument=affected$d[[1]],
                               cpc=0,
                               exporter.un=non.origin,
                               trade.share=-1,
                               stringsAsFactors = F))
  
  vdma.master=rbind(vdma.master,
                    data.frame(instrument=affected$e[[1]],
                               cpc=0,
                               exporter.un=non.origin,
                               trade.share=-1,
                               stringsAsFactors = F))
  
}

vdma.master.imports <- vdma.master
save(vdma.master.imports, file=paste0(project.path,"/data/VDMA data - GER importer.Rdata"))
