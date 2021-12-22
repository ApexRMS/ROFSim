#install.packages("C:/Users/HughesJo/Documents/rsyncrosim_1.3.1.tar.gz",repos=NULL,type="source")

library(rsyncrosim)

cDir = "C:/Users/endicotts/Documents/gitprojects/ROFSyncSim/"

sourceData = "C:/Users/endicotts/Documents/gitprojects/ROFSyncSim/ROFDemo_data"

#inPath = "C:/Users/HughesJo/Documents/InitialWork/OntarioFarNorth/RoFModel/SpaDESOutputs/v1/ROF_CCSM4_RCP45_res125_rep01/outputs/ROF_CCSM4_RCP45_res125_rep01/ROF_CCSM4_RCP45_res125_rep01.qs"
iters = c("ROF_CNRM-ESM2-1_SSP585_res125_rep02","ROF_CNRM-ESM2-1_SSP370_res125_rep04")

inPath = c("C:/Users/HughesJo/Documents/InitialWork/OntarioFarNorth/RoFModel/SpaDESOutputs/v2/iter/iter.qs")

libName = "ROFDemo5"

#delete(paste0(cDir,"/",libName,".ssim"),force=T)

cLib = ssimLibrary(paste0(cDir,"/",libName),package="ROFSim")

cProj= project(cLib,"Demo")

datasheet(cProj)

#TO DO: extract this info from input range map
cSheet="ROFSim_CaribouRange"
cc=data.frame(Name=c("James Bay","Missisa","Ozhiski","Nipigon","Pagwachuan"))
saveDatasheet(cProj,cc,name=cSheet)
datasheet(cProj,cSheet)

cSheet="ROFSim_SpaDESSimObject"
cc=data.frame(Name=c("burnMap","biomassMap","rstLCC","standAgeMap"),Description=c("cumulative burn map","total biomass (g/m^2) filtered by cohortData","Map of land cover classes","Map of time since transition"))
saveDatasheet(cProj,cc,name=cSheet)
datasheet(cProj,cSheet)

cSheet="ROFSim_Rasters"
cc=data.frame(Name=c("Caribou Ranges","Harvest","Anthropogenic Disturbance","Natural Disturbances","Provincial Land Cover","Stand Age","Leading Type", "Linear Features", "Eskers"))
cc$SpaDESSimObject[cc$Name=="Stand Age"]="standAgeMap"
saveDatasheet(cProj,cc,name=cSheet)
datasheet(cProj,cSheet,optional=T)

############
#scenarios - run control
rcScnS = scenario(cProj,"Run Control 2020, 2060")
#TO DO: Ask Val how to get "Total Iterations" option
cSheet="ROFSim_RunControl"
cc=data.frame(MinimumIteration=1,MaximumIteration=2,MinimumTimestep=2020,MaximumTimestep=2060,OutputFrequency=10)
saveDatasheet(rcScnS,cc,name=cSheet)
datasheet(rcScnS,cSheet,optional=T)

rcScn = scenario(cProj,"Run Control 2020")
cSheet="ROFSim_RunControl"
cc=data.frame(MinimumIteration=1,MaximumIteration=1,MinimumTimestep=2020,MaximumTimestep=2020)
saveDatasheet(rcScn,cc,name=cSheet)
datasheet(rcScn,cSheet,optional=T)

# scenario - data - anthro #===============================
datScn <- scenario(cProj, "data - anthro")

cSheet <- "core_Pipeline"
cc <- data.frame(StageNameID = "Prepare Spatial Data", RunOrder = 1)
saveDatasheet(datScn, cc, name = cSheet)

cSheet="ROFSim_RasterFile"
cc=data.frame(RastersID="Natural Disturbances",Filename=file.path(sourceData,"fireAFFES2020_250.tif"))
cc=rbind(cc,data.frame(RastersID="Harvest",Filename=file.path(sourceData,"harvMNRF2018_250.tif")))
cc=rbind(cc,data.frame(RastersID="Provincial Land Cover",Filename=file.path(sourceData,"plc250.tif")))
cc$Timestep=NA
cc=rbind(cc,data.frame(RastersID="Anthropogenic Disturbance",Timestep=2040,
                       Filename=file.path(sourceData,"mines_ras250.tif")))
saveDatasheet(datScn,cc,name=cSheet,append=F)
datasheet(datScn,cSheet)

cSheet="ROFSim_ExternalFile"
cc=data.frame(PolygonsID="Eskers",File=file.path(sourceData,"/esker.shp"))
cc=rbind(cc,data.frame(PolygonsID="Linear Features",File=file.path(sourceData,"/rail.shp")))
cc=rbind(cc,data.frame(PolygonsID="Linear Features",File=file.path(sourceData,"/util2020.shp")))
cc=rbind(cc,data.frame(PolygonsID="Ranges",File=file.path(sourceData,"/project_ranges.shp")))
cc$Timestep=NA
cc=rbind(cc,data.frame(PolygonsID="Linear Features",Timestep=2020,File=file.path(sourceData,"/road_ORNMNRFROF2020.shp")))
cc=rbind(cc,data.frame(PolygonsID="Linear Features",Timestep=2030,File=file.path(sourceData,"/RoF_MNRF_2020.shp")))
saveDatasheet(datScn,cc,name=cSheet,append=F)
datasheet(datScn,cSheet)

# scenario - data - current #===============================
datScnCur <- scenario(cProj, "data - current")

cSheet <- "core_Pipeline"
cc <- data.frame(StageNameID = "Prepare Spatial Data", RunOrder = 1)
saveDatasheet(datScnCur, cc, name = cSheet)

cSheet="ROFSim_RasterFile"
cc=data.frame(RastersID="Natural Disturbances",Filename=paste0(sourceData,"/fireAFFES2020_250.tif"))
cc=rbind(cc,data.frame(RastersID="Harvest",Filename=paste0(sourceData,"/harvMNRF2018_250.tif")))
cc=rbind(cc,data.frame(RastersID="Provincial Land Cover",Filename=paste0(sourceData,"/plc250.tif")))
saveDatasheet(datScnCur,cc,name=cSheet,append=F)
datasheet(datScnCur,cSheet)

cSheet="ROFSim_ExternalFile"
cc=data.frame(PolygonsID="Eskers",File=paste0(sourceData,"/esker.shp"))
cc=rbind(cc,data.frame(PolygonsID="Linear Features",File=paste0(sourceData,"/rail.shp")))
cc=rbind(cc,data.frame(PolygonsID="Linear Features",File=paste0(sourceData,"/util2020.shp")))
cc=rbind(cc,data.frame(PolygonsID="Ranges",File=paste0(sourceData,"/project_ranges.shp")))
cc=rbind(cc,data.frame(PolygonsID="Linear Features",File=paste0(sourceData,"/road_ORNMNRFROF2020.shp")))
saveDatasheet(datScnCur,cc,name=cSheet,append=F)
datasheet(datScnCur,cSheet)

############
#scenarios - caribou - current

cbScn = scenario(cProj,"Caribou - current")

datasheet(cbScn)
cSheet="core_Pipeline"
cc=data.frame(StageNameID="Caribou Habitat",RunOrder=1)
saveDatasheet(cbScn,cc,name=cSheet)
datasheet(cbScn,cSheet)

cSheet="ROFSim_RunCaribouRange"
cc=data.frame(Range="Missisa",CoeffRange="Missisa")
saveDatasheet(cbScn,cc,name=cSheet)
datasheet(cbScn,cSheet)

cSheet="ROFSim_CaribouDataSource"
cc <- data.frame(LandCoverRasterID = "Provincial Land Cover", 
                 ProjectShapeFileID = "Ranges",
                 EskerShapeFileID = "Eskers", 
                 LinearFeatureShapeFileID = "Linear Features", 
                 NaturalDisturbanceRasterID = "Natural Disturbances",
                 HarvestRasterID = "Harvest", 
                 AnthropogenicRasterID = "Anthropogenic Disturbance",
                 EskerRasterID = "Eskers",
                 LinearFeatureRasterID = "Linear Features")
saveDatasheet(cbScn,cc,name=cSheet)
datasheet(cbScn,cSheet)

cSheet="ROFSim_CaribouModelOptions"
cc=data.frame(RunDistMetrics=T,RunCaribouHabitat=T,RunDemographicModel=T,padProjPoly=T)
saveDatasheet(cbScn,cc,name=cSheet)
datasheet(cbScn,cSheet)

# Use prepared data now
# cSheet="ROFSim_RasterFile"
# cc=data.frame(RastersID="Natural Disturbances",Filename=paste0(sourceData,"/fireAFFES2020_250.tif"))
# cc=rbind(cc,data.frame(RastersID="Harvest",Filename=paste0(sourceData,"/harvMNRF2018_250.tif")))
# cc=rbind(cc,data.frame(RastersID="Provincial Land Cover",Filename=paste0(sourceData,"/plc250.tif")))
# saveDatasheet(cbScn,cc,name=cSheet,append=F)
# datasheet(cbScn,cSheet)
# 
# cSheet="ROFSim_ExternalFile"
# cc=data.frame(PolygonsID="Eskers",File=paste0(sourceData,"/esker.shp"))
# cc=rbind(cc,data.frame(PolygonsID="Linear Features",File=paste0(sourceData,"/rail.shp")))
# cc=rbind(cc,data.frame(PolygonsID="Linear Features",File=paste0(sourceData,"/util2020.shp")))
# cc=rbind(cc,data.frame(PolygonsID="Ranges",File=paste0(sourceData,"/project_ranges.shp")))
# cc=rbind(cc,data.frame(PolygonsID="Linear Features",File=paste0(sourceData,"/road_ORNMNRFROF2020.shp")))
# saveDatasheet(cbScn,cc,name=cSheet,append=F)
# datasheet(cbScn,cSheet)

dependency(cbScn,rcScn)
dependency(cbScn, datScnCur)
datasheet(cbScn)

#cbRes = run(cbScn)


#############
#Caribou with change in anthropogenic disturbance
cbcScn = scenario(cProj,"Caribou - anthro",sourceScenario=cbScn)
dependency(cbcScn,rcScn,remove=T,force=T)
dependency(cbcScn,datScnCur,remove=T,force=T)
dependency(cbcScn,rcScnS)
dependency(cbcScn,datScn)

# Moved to prep data
# cSheet="ROFSim_RasterFile"
# cc=data.frame(RastersID="Natural Disturbances",Filename=paste0(sourceData,"/fireAFFES2020_250.tif"))
# cc=rbind(cc,data.frame(RastersID="Harvest",Filename=paste0(sourceData,"/harvMNRF2018_250.tif")))
# cc=rbind(cc,data.frame(RastersID="Provincial Land Cover",Filename=paste0(sourceData,"/plc250.tif")))
# cc$Timestep=NA
# cc=rbind(cc,data.frame(RastersID="Anthropogenic Disturbance",Timestep=2040,Filename=paste0(sourceData,"/mines_ras250.tif")))
# saveDatasheet(cbcScn,cc,name=cSheet,append=F)
# datasheet(cbcScn,cSheet)
# 
# cSheet="ROFSim_ExternalFile"
# cc=data.frame(PolygonsID="Eskers",File=paste0(sourceData,"/esker.shp"))
# cc=rbind(cc,data.frame(PolygonsID="Linear Features",File=paste0(sourceData,"/rail.shp")))
# cc=rbind(cc,data.frame(PolygonsID="Linear Features",File=paste0(sourceData,"/util2020.shp")))
# cc=rbind(cc,data.frame(PolygonsID="Ranges",File=paste0(sourceData,"/project_ranges.shp")))
# cc$Timestep=NA
# cc=rbind(cc,data.frame(PolygonsID="Linear Features",Timestep=2020,File=paste0(sourceData,"/road_ORNMNRFROF2020.shp")))
# cc=rbind(cc,data.frame(PolygonsID="Linear Features",Timestep=2030,File=paste0(sourceData,"/RoF_MNRF_2020.shp")))
# saveDatasheet(cbcScn,cc,name=cSheet,append=F)
# datasheet(cbcScn,cSheet)

#cbcRes = run(cbcScn)

############
#scenarios - import SpaDES
spScn = scenario(cProj,"Import SpaDES")

datasheet(spScn)
cSheet="core_Pipeline"
cc=data.frame(StageNameID="Spades Import",RunOrder=1)
saveDatasheet(spScn,cc,name=cSheet)
datasheet(spScn,cSheet)

dependency(spScn,rcScnS)

cSheet="ROFSim_SpaDESGeneral"
cc=data.frame(Iteration=c(1,2),Filename=c(gsub("iter",iters[1],inPath,fixed=T),gsub("iter",iters[2],inPath,fixed=T)))
saveDatasheet(spScn,cc,name=cSheet)
datasheet(spScn,cSheet)

cSheet="ROFSim_SpaDESRuntimeRasters"
cc=data.frame(RastersID=c("Stand Age"))
saveDatasheet(spScn,cc,name=cSheet)
datasheet(spScn,cSheet)

datasheet(spScn,cSheet,optional=T)

spRes = run(spScn)
#TO DO: figure out how to landcover legend table, stand age colours, etc.

############
#scenarios - make LCC from SpaDES
siScn = scenario(cProj,"Make LCC from SpaDES")
dependency(siScn,spScn)
mergeDependencies(siScn)=T

cSheet="core_Pipeline"
cc=data.frame(StageNameID="Generate LCC from Cohort Data",RunOrder=1)
saveDatasheet(siScn,cc,name=cSheet)
datasheet(siScn,cSheet)

lccRes = run(siScn)

###############
#Spades caribou scenario
cbsScn = scenario(cProj,"Caribou - spades - anthro",sourceScenario=cbcScn)
#cbsScn = scenario(cProj,"Caribou - spades - anthro")
# cSheet="ROFSim_RasterFile"
# cc=data.frame(RastersID="Harvest",Filename=paste0(sourceData,"/harvMNRF2018_250.tif"))
# cc$Timestep=NA
# cc=rbind(cc,data.frame(RastersID="Anthropogenic Disturbance",Timestep=2040,Filename=paste0(sourceData,"/mines_ras250.tif")))
# saveDatasheet(cbsScn,cc,name=cSheet,append=F)
# datasheet(cbsScn,cSheet)

cSheet="ROFSim_CaribouDataSource"
cc=data.frame(LandCoverRasterID="Provincial Land Cover",
              ProjectShapeFileID="Ranges",
              EskerShapeFileID="Eskers",
              LinearFeatureShapeFileID="Linear Features",
              NaturalDisturbanceRasterID="Stand Age",
              HarvestRasterID="Harvest",
              AnthropogenicRasterID="Anthropogenic Disturbance",
              EskerRasterID = "Eskers",
              LinearFeatureRasterID = "Linear Features")
saveDatasheet(cbsScn,cc,name=cSheet,append=F)
datasheet(cbsScn,cSheet)

dependency(cbsScn,spRes)
dependency(cbsScn,lccRes)
mergeDependencies(cbsScn)=T

#cbsRes = run(cbsScn)

##########
#add legend to landcover - after map is created in UI

#lccRes = scenario(cProj,10)
#install.packages("RColorBrewer")
library(RColorBrewer)
library(dplyr)
library(readr)
library(raster)

#example map
filepath(cLib)
#lccRes = scenario(cProj,27)
scenarioId(lccRes)
#mapPath = paste0(filepath(cLib),".input/Scenario-",scenarioId(lccRes),"/ROFSim_RasterFile/PLC_it_1_ts_2020.tif")
iMap = datasheetRaster(lccRes,"ROFSim_RasterFile",timestep=2020,iteration=1,subset=expression(RastersID=="Provincial Land Cover"))
fTab = freq(iMap)
fTab

# name of the map that needs a legend
mapName <- "ChangeView"

# Setting path to custom legend
legendPath <- "."
fileName <- "colormap_mapID_ROFSim_InputRastersMap-IDtemplateC.txt"
fileNameMod <- "colormap_mapID_ROFSim_InputRastersMap-ID.txt"


# get the list of charts to identify which one needs a legend
myCharts <- datasheet(cProj, 
                      name = "corestime_Maps", 
                      includeKey = T)
myCharts

myCharts$Criteria

# ID value for map that needs legend is
myChart <- filter(myCharts, Name == mapName)
mapId <- myChart$MapID[1]
mapId <- paste0("map", as.character(mapId))
rasterId <- myChart$Criteria[1]
rasterId=gsub("Map2","Map",rasterId,fixed=T)
rasterId <- c(44,18)#abs(parse_number(strsplit(rasterId,"|",fixed=T)[[1]]))
#empirically, 18 and 45 sort of work.

newFileName <- gsub("mapID", mapId, fileNameMod)
newFileNames=list()
if(length(rasterId)==1){
  newFileNames[["a"]] <- gsub("ID", as.character(rasterId), newFileName)
}else{
  for(rr in rasterId){
    newFileNames[[as.character(rr)]] <- gsub("ID", as.character(rr), newFileName)
  }
}

# get the legend directory for the library
libProperties <- ssimLibrary(cLib, summary = T)
legendDir <- filter(libProperties, property == "External input files:")
legendDir <- as.character(legendDir$value)
legendDir <- paste0(legendDir,"\\Project-",as.character(projectId(cProj)))

#edit legend file
lTab = read.csv(paste(legendPath, fileName, sep="/"))
names(lTab)=c("ID","RGB1","RGB2","RGB3","C","Label")
lTab$ID = as.numeric(lTab$ID)
lTab=subset(lTab,!is.na(RGB1))

omitRare = subset(data.frame(fTab),count<1)
names(omitRare)=c("ID","frequency")
merge(lTab,omitRare)

lTab
combo = farNorthLandcover(lTab,omitRare)

lines = c("# Syncrosim Generated Provincial Land Cover Color Map (QGIS-compatible),,,,,",
  "INTERPOLATION:DISCRETE")

lines = c(lines,paste(combo$ID,combo$RGB1,combo$RGB2,combo$RGB3,combo$C,combo$Label,sep=","))

fileConn<-file(paste(legendPath, fileNameMod, sep="/"))
writeLines(lines, fileConn)
close(fileConn)


# copy the custom legend to the legend directory
sourceFile <- paste(legendPath, fileNameMod, sep="/")
for(nn in newFileNames){
  destFile <- paste(legendDir, nn, sep = "/")
  file.copy(sourceFile, destFile, overwrite = T)
}

# delete the temp folder to get rid of any cached bitmaps
tempDir <- filter(libProperties, property == "Temporary files:")
tempDir <- as.character(tempDir$value)
unlink(tempDir, recursive = T, force = T)

