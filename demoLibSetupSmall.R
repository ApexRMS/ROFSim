#install.packages("C:/Users/HughesJo/Documents/rsyncrosim_1.3.1.tar.gz",repos=NULL,type="source")

library(rsyncrosim)

# should scenarios be run or should existing results be used? 
doRun <- T

#cDir = "C:/Users/endicotts/Documents/gitprojects/ROFSyncSim/"
#sourceData = "C:/Users/endicotts/Documents/gitprojects/ROFSyncSim/ROFDemo_data"
#iters = c("ROF_CNRM-ESM2-1_SSP370_res125_rep03", "ROF_CNRM-ESM2-1_SSP370_res125_rep04")
#inPath = file.path(sourceData, "SpaDESOutputs/iter/iter.qs")
#sourceData2 = sourceData

sourceData = "C:/Users/HughesJo/Documents/InitialWork/OntarioFarNorth/RoFModel/"
cDir = paste0(sourceData,"/UI")
iters = c("ROF_CNRM-ESM2-1_SSP585_res125_rep02", "ROF_CNRM-ESM2-1_SSP370_res125_rep04")
inPath = file.path(sourceData, "SpaDESOutputs/v2/iter/iter.qs")
sourceData2 = "C:/Users/HughesJo/Documents/InitialWork/OntarioFarNorth/ROFData"

libName = "ROFDemoS7"

#delete(paste0(cDir,"/",libName,".ssim"),force=T)

cLib = ssimLibrary(paste0(cDir,"/",libName),package="ROFSim")

cProj= project(cLib,"Demo")

datasheet(cProj)

allScn <- scenario(cProj)

allRes <- subset(allScn, allScn$isResult == "Yes")

if(doRun){
  # Make sure the library uses the correct R installation
  rConfig <- datasheet(cLib, name = "core_RConfig")
  rConfig <- addRow(rConfig, c(ExePath = list.files(R.home("bin"), "Rscript", full.names = TRUE)))
  saveDatasheet(cLib, rConfig, name = "core_RConfig")
  
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
  cc=data.frame(Name=c("Caribou Ranges","Harvest","Anthropogenic Disturbance","Natural Disturbances","Provincial Land Cover","SpaDES Land Cover","SpaDES Stand Age","SpaDES Leading Type", "Linear Features", "Eskers"))
  cc$SpaDESSimObject[cc$Name=="SpaDES Stand Age"]="standAgeMap"
  saveDatasheet(cProj,cc,name=cSheet)
  datasheet(cProj,cSheet,optional=T)
}
#scenarios - run control ############
rcScnS = scenario(cProj,"Run Control 2020, 2060")
rcScn = scenario(cProj,"Run Control 2020")
#TO DO: Ask Val how to get "Total Iterations" option
if(doRun){
  cSheet="ROFSim_RunControl"
  cc=data.frame(MinimumIteration=1,MaximumIteration=2,MinimumTimestep=2020,MaximumTimestep=2060,OutputFrequency=10)
  saveDatasheet(rcScnS,cc,name=cSheet)
  datasheet(rcScnS,cSheet,optional=T)
  
  cSheet="ROFSim_RunControl"
  cc=data.frame(MinimumIteration=1,MaximumIteration=1,MinimumTimestep=2020,MaximumTimestep=2020)
  saveDatasheet(rcScn,cc,name=cSheet)
  datasheet(rcScn,cSheet,optional=T)
}

# scenario - data - context #===============================
#Used in multiple scenarios - so specify here once, and pass info as dependency.
#Note I am not running this scenario - passing on the inputs, rather than the outputs.
#Following the principle of never specifying the same piece of input info in more than one place.
#One could change the input landcover or polygon layer here, and it would update everywhere throughout the library.
datContextScn <- scenario(cProj, "data - context")
if(doRun){
  cSheet <- "core_Pipeline"
  cc <- data.frame(StageNameID = "Prepare Spatial Data", RunOrder = 1)
  saveDatasheet(datContextScn, cc, name = cSheet)
  
  cSheet="ROFSim_RasterFile"
  cc=data.frame(RastersID="Provincial Land Cover",Filename=file.path(sourceData2,"plc250.tif"))
  saveDatasheet(datContextScn,cc,name=cSheet,append=F)
  datasheet(datContextScn,cSheet)
  
  cSheet="ROFSim_ExternalFile"
  cc=data.frame(PolygonsID="Ranges",File=file.path(sourceData2,"/project_ranges.shp"))
  saveDatasheet(datContextScn,cc,name=cSheet,append=F)
  datasheet(datContextScn,cSheet)
} 

# scenario - data - base linear #===============================
#Again, used in multiple data processing scenarios. Not specifying the same input info more than once.
datLinearScn <- scenario(cProj, "data - base linear")
if(doRun){
  cSheet <- "core_Pipeline"
  cc <- data.frame(StageNameID = "Prepare Spatial Data", RunOrder = 1)
  saveDatasheet(datLinearScn, cc, name = cSheet)
  
  cSheet="ROFSim_ExternalFile"
  cc=data.frame(PolygonsID="Linear Features",File=file.path(sourceData2,"/rail.shp"))
  cc=rbind(cc,data.frame(PolygonsID="Linear Features",File=file.path(sourceData2,"/util2020.shp")))
  saveDatasheet(datLinearScn,cc,name=cSheet,append=F)
  datasheet(datLinearScn,cSheet)
} 

# scenario - data - baseline #===============================
datShareScn <- scenario(cProj, "data - baseline")
if(doRun){
  cSheet <- "core_Pipeline"
  cc <- data.frame(StageNameID = "Prepare Spatial Data", RunOrder = 1)
  saveDatasheet(datShareScn, cc, name = cSheet)
  
  cSheet="ROFSim_RasterFile"
  cc=data.frame(RastersID="Natural Disturbances",Filename=file.path(sourceData2,"fireAFFES2020_250.tif"))
  cc=rbind(cc,data.frame(RastersID="Harvest",Filename=file.path(sourceData2,"harvMNRF2018_250.tif")))
  cc$Timestep=NA
  saveDatasheet(datShareScn,cc,name=cSheet,append=F)
  datasheet(datShareScn,cSheet)
  
  cSheet="ROFSim_ExternalFile"
  cc=data.frame(PolygonsID="Eskers",File=file.path(sourceData2,"/esker.shp"))
  saveDatasheet(datShareScn,cc,name=cSheet,append=F)
  datasheet(datShareScn,cSheet)
  
  dependency(datShareScn,datContextScn)
  dependency(datShareScn,datLinearScn)
  mergeDependencies(datShareScn)=T
  
  datShareRes <- run(datShareScn)
} else {
  # get results scnID if it exists
  scnID <- subset(allRes, grepl("data - baseline \\(", allRes$name))$scenarioId
  
  if(length(scnID) > 0){
    datShareRes <- scenario(cProj, max(scnID))
  }
}

# scenario - data - anthro #===============================
#Note I am only adding the 2040 and 2030 data here. No need to repeat the 2020 baseline calculations. 

datScn <- scenario(cProj, "data - anthro")

if(doRun){
  cSheet <- "core_Pipeline"
  cc <- data.frame(StageNameID = "Prepare Spatial Data", RunOrder = 1)
  saveDatasheet(datScn, cc, name = cSheet)
  
  cSheet="ROFSim_RasterFile"
  cc=data.frame(RastersID="Anthropogenic Disturbance",Timestep=2040,
                         Filename=file.path(sourceData2,"mines_ras250.tif"))
  saveDatasheet(datScn,cc,name=cSheet,append=F)
  datasheet(datScn,cSheet)
  
  cSheet="ROFSim_ExternalFile"
  cc=data.frame(PolygonsID="Linear Features",Timestep=2030,File=file.path(sourceData2,"/RoF_MNRF_2020.shp"))
  saveDatasheet(datScn,cc,name=cSheet,append=F)
  datasheet(datScn,cSheet)
  
  dependency(datScn,datContextScn)
  dependency(datScn,datLinearScn)
  mergeDependencies(datScn)=T
  
  datRes <- run(datScn)
} else {
  # get results scnID if it exists
  scnID <- subset(allRes, grepl("data - anthro \\(", allRes$name))$scenarioId
  
  if(length(scnID) > 0){
    datRes <- scenario(cProj, max(scnID))
  }
}

# scenarios - caribou - current ############

cbScn = scenario(cProj,"Caribou - current")

if(doRun){
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
  
  dependency(cbScn,rcScn)
  dependency(cbScn, datShareRes)
  datasheet(cbScn)
  
  
  cbRes = run(cbScn)
}

# scenarios - caribou - anthropogenic disturbance #############
cbcScn = scenario(cProj,"Caribou - anthro",sourceScenario=cbScn)
# seems like results dependencies don't stay if just do the above
dependency(cbcScn,rcScn,remove=T,force=T)
dependency(cbcScn, rcScnS)
dependency(cbScn, datShareRes)
dependency(cbcScn, datRes)
mergeDependencies(cbcScn)=T

if(doRun){
  cbcRes = run(cbcScn)
}