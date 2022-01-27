# install.packages("C:/Users/HughesJo/Documents/rsyncrosim_1.3.1.tar.gz",repos=NULL,type="source")

library(rsyncrosim)

# should scenarios be run or should existing results be used?
doRun <- TRUE

cDir <- "C:/Users/endicotts/Documents/gitprojects/ROFSyncSim/"
sourceData <- "C:/Users/endicotts/Documents/gitprojects/ROFSyncSim/ROFDemo_data"
iters <- c("ROF_CNRM-ESM2-1_SSP370_res125_rep03", "ROF_CNRM-ESM2-1_SSP370_res125_rep04")
inPath <- file.path(sourceData, "SpaDESOutputs/iter/iter.qs")
sourceData2 <- sourceData

# sourceData = "C:/Users/HughesJo/Documents/InitialWork/OntarioFarNorth/RoFModel/"
# cDir = paste0(sourceData,"/UI")
# iters = c("ROF_CNRM-ESM2-1_SSP585_res125_rep02", "ROF_CNRM-ESM2-1_SSP370_res125_rep04")
# inPath = file.path(sourceData, "SpaDESOutputs/v2/iter/iter.qs")
# sourceData2 = "C:/Users/HughesJo/Documents/InitialWork/OntarioFarNorth/ROFData"

libName <- "ROFDemoS7"

# delete(paste0(cDir,"/",libName,".ssim"),force=T)

cLib <- ssimLibrary(paste0(cDir, "/", libName), package = "ROFSim")

cProj <- project(cLib, "Demo")

# datasheet(cProj)

allScn <- scenario(cProj)

allRes <- subset(allScn, allScn$isResult == "Yes")

if (doRun) {
  # Make sure the library uses the correct R installation
  rConfig <- datasheet(cLib, name = "core_RConfig")
  rConfig <- addRow(rConfig, c(ExePath = list.files(R.home("bin"), "Rscript",
                                                    full.names = TRUE)))
  saveDatasheet(cLib, rConfig, name = "core_RConfig")

  # TO DO: extract this info from input range map
  cSheet <- "ROFSim_CaribouRange"
  cc <- data.frame(Name = c("James Bay", "Missisa", "Ozhiski", "Nipigon", "Pagwachuan"))
  saveDatasheet(cProj, cc, name = cSheet)
  # datasheet(cProj,cSheet)

  cSheet <- "ROFSim_SpaDESSimObject"
  cc <- data.frame(Name = c("burnMap", "biomassMap", "rstLCC", "standAgeMap"), 
                   Description = c("cumulative burn map", 
                                   "total biomass (g/m^2) filtered by cohortData", 
                                   "Map of land cover classes", 
                                   "Map of time since transition"))
  saveDatasheet(cProj, cc, name = cSheet)
  # datasheet(cProj,cSheet)

  cSheet <- "ROFSim_Rasters"
  cc <- data.frame(
    Name = c("Caribou Ranges", "Harvest", "Anthropogenic Disturbance",
             "Natural Disturbances", "Provincial Land Cover", "SpaDES Land Cover", 
             "SpaDES Stand Age", "SpaDES Leading Type", "Linear Features", "Eskers")
  )
  cc$SpaDESSimObject[cc$Name == "SpaDES Stand Age"] <- "standAgeMap"
  saveDatasheet(cProj, cc, name = cSheet)
  # datasheet(cProj,cSheet,optional=T)
}
# scenarios - run control ############
rcFutScn <- scenario(cProj, "Run Control 2020, 2060")
rcCurScn <- scenario(cProj, "Run Control 2020")
# TO DO: Ask Val how to get "Total Iterations" option
if (doRun) {
  cSheet <- "ROFSim_RunControl"
  cc <- data.frame(MinimumIteration = 1, MaximumIteration = 2,
                   MinimumTimestep = 2020, MaximumTimestep = 2060, 
                   OutputFrequency = 10)
  saveDatasheet(rcFutScn, cc, name = cSheet)
  # datasheet(rcFutScn,cSheet,optional=T)

  cSheet <- "ROFSim_RunControl"
  cc <- data.frame(MinimumIteration = 1, MaximumIteration = 1, 
                   MinimumTimestep = 2020, MaximumTimestep = 2020)
  saveDatasheet(rcCurScn, cc, name = cSheet)
  # datasheet(rcCurScn,cSheet,optional=T)
}

# scenario - data - context #===============================
# Used in multiple scenarios - so specify here once, and pass info as dependency.
# Note I am not running this scenario - passing on the inputs, rather than the outputs.
# Following the principle of never specifying the same piece of input info in more than one place.
# One could change the input landcover or polygon layer here, and it would update everywhere throughout the library.
datContextScn <- scenario(cProj, "data - context")
if (doRun) {
  cSheet <- "core_Pipeline"
  cc <- data.frame(StageNameID = "Prepare Spatial Data", RunOrder = 1)
  saveDatasheet(datContextScn, cc, name = cSheet)

  cSheet <- "ROFSim_RasterFile"
  cc <- data.frame(RastersID = "Provincial Land Cover", 
                   Filename = file.path(sourceData2, "plc250.tif"))
  saveDatasheet(datContextScn, cc, name = cSheet, append = FALSE)
  # datasheet(datContextScn,cSheet)

  cSheet <- "ROFSim_ExternalFile"
  cc <- data.frame(PolygonsID = "Ranges", 
                   File = file.path(sourceData2, "/project_ranges.shp"))
  saveDatasheet(datContextScn, cc, name = cSheet, append = FALSE)
  # datasheet(datContextScn,cSheet)
}

# scenario - data - base linear #===============================
# Again, used in multiple data processing scenarios. Not specifying the same input info more than once.
datLinearScn <- scenario(cProj, "data - base linear")
if (doRun) {
  cSheet <- "core_Pipeline"
  cc <- data.frame(StageNameID = "Prepare Spatial Data", RunOrder = 1)
  saveDatasheet(datLinearScn, cc, name = cSheet)

  cSheet <- "ROFSim_ExternalFile"
  cc <- data.frame(PolygonsID = "Linear Features", 
                   File = file.path(sourceData2, "/rail.shp"))
  cc <- rbind(cc, data.frame(PolygonsID = "Linear Features", 
                             File = file.path(sourceData2, "/util2020.shp")))
  saveDatasheet(datLinearScn, cc, name = cSheet, append = FALSE)
  # datasheet(datLinearScn,cSheet)
}

# scenario - data - baseline #===============================
datBaselineScn <- scenario(cProj, "data - baseline")
if (doRun) {
  cSheet <- "core_Pipeline"
  cc <- data.frame(StageNameID = "Prepare Spatial Data", RunOrder = 1)
  saveDatasheet(datBaselineScn, cc, name = cSheet)

  cSheet <- "ROFSim_RasterFile"
  cc <- data.frame(RastersID = "Natural Disturbances",
                   Filename = file.path(sourceData2, "fireAFFES2020_250.tif"))
  cc <- rbind(cc, data.frame(RastersID = "Harvest", 
                             Filename = file.path(sourceData2, "harvMNRF2018_250.tif")))
  cc$Timestep <- NA
  saveDatasheet(datBaselineScn, cc, name = cSheet, append = FALSE)
  # datasheet(datBaselineScn,cSheet)

  cSheet <- "ROFSim_ExternalFile"
  cc <- data.frame(PolygonsID = "Eskers", File = file.path(sourceData2, "/esker.shp"))
  cc$Timestep <- NA
  cc <- rbind(cc, data.frame(PolygonsID = "Linear Features", 
                             Timestep = 2020,
                             File = file.path(sourceData2, "road_ORNMNRFROF2020.shp")))
  saveDatasheet(datBaselineScn, cc, name = cSheet, append = FALSE)
  # datasheet(datBaselineScn,cSheet)

  dependency(datBaselineScn, datContextScn)
  dependency(datBaselineScn, datLinearScn)
  mergeDependencies(datBaselineScn) <- TRUE

  datBaselineRes <- run(datBaselineScn)
} else {
  # get results scnID if it exists
  scnID <- subset(allRes, grepl("data - baseline \\(", allRes$name))$scenarioId

  if (length(scnID) > 0) {
    datBaselineRes <- scenario(cProj, max(scnID))
  }
}

# scenario - data - anthro #===============================
# Note I am only adding the 2040 and 2030 data here. No need to repeat the 2020 baseline calculations.

datAnthroScn <- scenario(cProj, "data - anthro")

if (doRun) {
  cSheet <- "core_Pipeline"
  cc <- data.frame(StageNameID = "Prepare Spatial Data", RunOrder = 1)
  saveDatasheet(datAnthroScn, cc, name = cSheet)

  cSheet <- "ROFSim_RasterFile"
  cc <- data.frame(
    RastersID = "Anthropogenic Disturbance", Timestep = 2040,
    Filename = file.path(sourceData2, "mines_ras250.tif")
  )
  saveDatasheet(datAnthroScn, cc, name = cSheet, append = FALSE)
  # datasheet(datAnthroScn,cSheet)

  cSheet <- "ROFSim_ExternalFile"
  cc <- data.frame(PolygonsID = "Linear Features", Timestep = 2030,
                   File = file.path(sourceData2, "/RoF_MNRF_2020.shp"))
  saveDatasheet(datAnthroScn, cc, name = cSheet, append = FALSE)
  # datasheet(datAnthroScn,cSheet)

  dependency(datAnthroScn, datContextScn)
  dependency(datAnthroScn, datLinearScn)
  mergeDependencies(datAnthroScn) <- TRUE

  datAnthroRes <- run(datAnthroScn)
} else {
  # get results scnID if it exists
  scnID <- subset(allRes, grepl("data - anthro \\(", allRes$name))$scenarioId

  if (length(scnID) > 0) {
    datAnthroRes <- scenario(cProj, max(scnID))
  }
}

# scenarios - caribou - current ############

cbCurScn <- scenario(cProj, "Caribou - current")

if (doRun) {
  # datasheet(cbCurScn)
  cSheet <- "core_Pipeline"
  cc <- data.frame(StageNameID = "Caribou Habitat", RunOrder = 1)
  saveDatasheet(cbCurScn, cc, name = cSheet)
  # datasheet(cbCurScn,cSheet)

  cSheet <- "ROFSim_RunCaribouRange"
  cc <- data.frame(Range = "Missisa", CoeffRange = "Missisa")
  saveDatasheet(cbCurScn, cc, name = cSheet)
  # datasheet(cbCurScn,cSheet)

  cSheet <- "ROFSim_CaribouDataSource"
  cc <- data.frame(
    LandCoverRasterID = "Provincial Land Cover",
    ProjectShapeFileID = "Ranges",
    EskerShapeFileID = "Eskers",
    LinearFeatureShapeFileID = "Linear Features",
    NaturalDisturbanceRasterID = "Natural Disturbances",
    HarvestRasterID = "Harvest",
    AnthropogenicRasterID = "Anthropogenic Disturbance",
    EskerRasterID = "Eskers",
    LinearFeatureRasterID = "Linear Features"
  )
  saveDatasheet(cbCurScn, cc, name = cSheet)
  # datasheet(cbCurScn,cSheet)

  cSheet <- "ROFSim_CaribouModelOptions"
  cc <- data.frame(RunDistMetrics = TRUE, RunCaribouHabitat = TRUE,
                   RunDemographicModel = TRUE, padProjPoly = TRUE)
  saveDatasheet(cbCurScn, cc, name = cSheet)
  # datasheet(cbCurScn,cSheet)

  dependency(cbCurScn, rcCurScn)
  dependency(cbCurScn, datBaselineRes)
  # datasheet(cbCurScn)


  cbCurRes <- run(cbCurScn)
}

# scenarios - caribou - anthropogenic disturbance #############
cbAnthroScn <- scenario(cProj, "Caribou - anthro", sourceScenario = cbCurScn)

if (doRun) {
  dependency(cbAnthroScn, rcCurScn, remove = TRUE, force = TRUE)
  dependency(cbAnthroScn, rcFutScn)
  dependency(cbAnthroScn, datBaselineRes)
  dependency(cbAnthroScn, datAnthroRes)
  mergeDependencies(cbAnthroScn) <- TRUE

  cbAnthroRes <- run(cbAnthroScn)
}

# scenarios - import SpaDES ############
impSpdsScn <- scenario(cProj, "Import SpaDES")

if (doRun) {
  # datasheet(impSpdsScn)
  cSheet <- "core_Pipeline"
  cc <- data.frame(StageNameID = "Spades Import", RunOrder = 1)
  saveDatasheet(impSpdsScn, cc, name = cSheet)
  # datasheet(impSpdsScn,cSheet)

  dependency(impSpdsScn, rcFutScn)

  cSheet <- "ROFSim_SpaDESGeneral"
  cc <- data.frame(Iteration = c(1, 2), 
                   Filename = c(gsub("iter", iters[1], inPath, fixed = TRUE),
                                gsub("iter", iters[2], inPath, fixed = TRUE)))
  saveDatasheet(impSpdsScn, cc, name = cSheet)
  # datasheet(impSpdsScn,cSheet)

  cSheet <- "ROFSim_SpaDESRuntimeRasters"
  cc <- data.frame(RastersID = c("SpaDES Stand Age"))
  saveDatasheet(impSpdsScn, cc, name = cSheet)
  # datasheet(impSpdsScn,cSheet)

  # datasheet(impSpdsScn,cSheet,optional=T)

  impSpdsRes <- run(impSpdsScn)
} else {
  # get results scnID if it exists
  scnID <- subset(allRes, grepl("Import SpaDES \\(", allRes$name))$scenarioId

  if (length(scnID) > 0) {
    impSpdsRes <- scenario(cProj, max(scnID))
  }
}

# TO DO: figure out how to landcover legend table, stand age colours, etc.

# scenarios - make LCC from SpaDES ############
lccSpdsScn <- scenario(cProj, "Make LCC from SpaDES")

if (doRun) {
  dependency(lccSpdsScn, impSpdsScn)
  mergeDependencies(lccSpdsScn) <- TRUE

  cSheet <- "core_Pipeline"
  cc <- data.frame(StageNameID = "Generate LCC from Cohort Data", RunOrder = 1)
  saveDatasheet(lccSpdsScn, cc, name = cSheet)
  # datasheet(lccSpdsScn,cSheet)

  lccSpdsRes <- run(lccSpdsScn)
} else {
  # get results scnID if it exists
  scnID <- subset(allRes, grepl("Make LCC from SpaDES \\(", allRes$name))$scenarioId

  if (length(scnID) > 0) {
    lccSpdsRes <- scenario(cProj, max(scnID))
  }
}

# data preparation for SpaDES #=================================
datSpdsScn <- scenario(cProj, "data - anthro - SpaDES")

if (doRun) {
  cSheet <- "core_Pipeline"
  cc <- data.frame(StageNameID = "Prepare Spatial Data", RunOrder = 1)
  saveDatasheet(datSpdsScn, cc, name = cSheet)

  dependency(datSpdsScn, datContextScn)
  dependency(datSpdsScn, lccSpdsRes)
  dependency(datSpdsScn, impSpdsRes)
  mergeDependencies(datSpdsScn) <- TRUE

  datSpdsRes <- run(datSpdsScn)
} else {
  # get results scnID if it exists
  scnID <- subset(allRes, grepl("data - anthro - SpaDES \\(", allRes$name))$scenarioId

  if (length(scnID) > 0) {
    datSpdsRes <- scenario(cProj, max(scnID))
  }
}

# scenarios - caribou - spades - anthro ###############
cbSpdsScn <- scenario(cProj, "Caribou - spades - anthro", sourceScenario = cbAnthroScn)

if (doRun) {
  # already depends on datAnthroRes and datBaselineRes from cbAnthroScn
  dependency(cbSpdsScn, datSpdsRes)
  mergeDependencies(cbSpdsScn) <- TRUE

  cSheet <- "ROFSim_CaribouDataSource"
  cc <- data.frame(
    LandCoverRasterID = "SpaDES Land Cover",
    ProjectShapeFileID = "Ranges",
    EskerShapeFileID = "Eskers",
    LinearFeatureShapeFileID = "Linear Features",
    NaturalDisturbanceRasterID = "SpaDES Stand Age",
    HarvestRasterID = "Harvest",
    AnthropogenicRasterID = "Anthropogenic Disturbance",
    EskerRasterID = "Eskers",
    LinearFeatureRasterID = "Linear Features"
  )
  saveDatasheet(cbSpdsScn, cc, name = cSheet, append = FALSE)
  # datasheet(cbSpdsScn,cSheet)

  cbSpdsRes <- run(cbSpdsScn)
}

# Get summary of simulation times
# purrr::map_dfr(lst(cbCurRes, cbAnthroRes, cbSpdsRes,
#                    datBaselineRes, datAnthroRes, datSpdsRes),
#                ~rsyncrosim::runLog(.x) %>%
#                  stringr::str_extract("Total simulation .*")) %>%
#   t()

# add legend to landcover - after map is created in UI ##########

# lccSpdsRes = scenario(cProj,10)
# install.packages("RColorBrewer")
library(RColorBrewer)
library(dplyr)
library(readr)
library(raster)

# example map
filepath(cLib)
# lccSpdsRes = scenario(cProj,27)
scenarioId(lccSpdsRes)
# mapPath = paste0(filepath(cLib),".input/Scenario-",scenarioId(lccSpdsRes),"/ROFSim_RasterFile/PLC_it_1_ts_2020.tif")
iMap <- datasheetRaster(lccSpdsRes, "ROFSim_RasterFile", timestep = 2020, 
                        iteration = 1, 
                        subset = expression(RastersID == "SpaDES Land Cover"))
fTab <- freq(iMap)
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
  includeKey = TRUE
)
myCharts

myCharts$Criteria

# ID value for map that needs legend is
myChart <- filter(myCharts, Name == mapName)
mapId <- myChart$MapID[1]
mapId <- paste0("map", as.character(mapId))
rasterId <- myChart$Criteria[1]
rasterId <- gsub("Map2", "Map", rasterId, fixed = TRUE)
rasterId <- c(44, 18) # abs(parse_number(strsplit(rasterId,"|",fixed=T)[[1]]))
# empirically, 18 and 45 sort of work.

newFileName <- gsub("mapID", mapId, fileNameMod)
newFileNames <- list()
if (length(rasterId) == 1) {
  newFileNames[["a"]] <- gsub("ID", as.character(rasterId), newFileName)
} else {
  for (rr in rasterId) {
    newFileNames[[as.character(rr)]] <- gsub("ID", as.character(rr), newFileName)
  }
}

# get the legend directory for the library
libProperties <- ssimLibrary(cLib, summary = TRUE)
legendDir <- filter(libProperties, property == "External input files:")
legendDir <- as.character(legendDir$value)
legendDir <- paste0(legendDir, "\\Project-", as.character(projectId(cProj)))

# edit legend file
lTab <- read.csv(paste(legendPath, fileName, sep = "/"))
names(lTab) <- c("ID", "RGB1", "RGB2", "RGB3", "C", "Label")
lTab$ID <- as.numeric(lTab$ID)
lTab <- subset(lTab, !is.na(RGB1))

omitRare <- subset(data.frame(fTab), count < 1)
names(omitRare) <- c("ID", "frequency")
merge(lTab, omitRare)

lTab
combo <- farNorthLandcover(lTab, omitRare)

lines <- c(
  "# Syncrosim Generated Provincial Land Cover Color Map (QGIS-compatible),,,,,",
  "INTERPOLATION:DISCRETE"
)

lines <- c(lines, paste(combo$ID, combo$RGB1, combo$RGB2, combo$RGB3, combo$C,
                        combo$Label, sep = ","))

fileConn <- file(paste(legendPath, fileNameMod, sep = "/"))
writeLines(lines, fileConn)
close(fileConn)


# copy the custom legend to the legend directory
sourceFile <- paste(legendPath, fileNameMod, sep = "/")
for (nn in newFileNames) {
  destFile <- paste(legendDir, nn, sep = "/")
  file.copy(sourceFile, destFile, overwrite = TRUE)
}

# delete the temp folder to get rid of any cached bitmaps
tempDir <- filter(libProperties, property == "Temporary files:")
tempDir <- as.character(tempDir$value)
unlink(tempDir, recursive = TRUE, force = TRUE)
