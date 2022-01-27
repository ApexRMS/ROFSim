# data prep only scenario set up
library(rsyncrosim)

# Install ROFSim package using file path to ssimpkg file
# addPackage("path/to/ROFSim.ssimpkg")

rootPth <- system.file("extdata", package = "caribouMetrics")

libName <-  "demoSSimLib2"

#cDir <- "C:/Users/endicotts/Documents/gitprojects/ROFSyncSim"
cDir = "C:/Users/HughesJo/Documents/InitialWork/OntarioFarNorth/RoFModel/UI"

delete(paste0(cDir,"/",libName,".ssim"),force=T)

dir.create(cDir)

cLib <- ssimLibrary(paste0(cDir, "/", libName), package = "ROFSim", overwrite = TRUE)

cProj <- project(cLib, "Demo")

# Make sure the library uses the correct R installation
rConfig <- datasheet(cLib, name = "core_RConfig")
rConfig <- addRow(rConfig, c(ExePath = list.files(R.home("bin"), "Rscript", full.names = TRUE)))
saveDatasheet(cLib, rConfig, name = "core_RConfig")

# inspect datasheets used at the project level
cProjDS <- datasheet(cProj)

# set available options for input data
cSheet <- "ROFSim_CaribouRange"
cc <- data.frame(Name = c("James Bay", "Missisa", "Ozhiski", "Nipigon",
                          "Pagwachuan"))
saveDatasheet(cProj, cc, name = cSheet)

cSheet <- "ROFSim_Rasters"
cc <- data.frame(Name = c("Anthropogenic Disturbance",
                          "Natural Disturbance", "Provincial Land Cover", 
                          "Linear Features", "Eskers"))
saveDatasheet(cProj, cc, name = cSheet)

# scenarios - run control
rcScn <- scenario(cProj, "Run Control 2020 - 2100")
cSheet <- "ROFSim_RunControl"
cc <- data.frame(MinimumIteration = 1, MaximumIteration = 1,
                 MinimumTimestep = 2020, MaximumTimestep = 2100, OutputFrequency = 10)
saveDatasheet(rcScn, cc, name = cSheet)

# scenario - data - only #===============================
datScn <- scenario(cProj, "data - only")

cSheet <- "core_Pipeline"
cc <- data.frame(StageNameID = "Prepare Spatial Data", RunOrder = 1)
saveDatasheet(datScn, cc, name = cSheet)

cSheet <- "ROFSim_RasterFile"
cc <- data.frame(RastersID = "Natural Disturbance", 
                 Filename = file.path(rootPth, "natDist.tif"))
cc <- rbind(cc, data.frame(RastersID = "Anthropogenic Disturbance", 
                           Filename = file.path(rootPth, "anthroDist.tif")))
cc <- rbind(cc, data.frame(RastersID = "Provincial Land Cover", 
                           Filename = file.path(rootPth, "landCover.tif")))
cc$Timestep <- NA
cc <- rbind(cc, data.frame(RastersID = "Anthropogenic Disturbance", 
                           Timestep = 2040,
                           Filename = file.path(rootPth, "linFeatTif.tif")))
saveDatasheet(datScn, cc, name = cSheet, append = FALSE)

cSheet <- "ROFSim_ExternalFile"
cc <- data.frame(PolygonsID = "Eskers", File = file.path(rootPth, "esker.shp"))
cc <- rbind(cc, data.frame(PolygonsID = "Linear Features", 
                           File = file.path(rootPth, "rail.shp")))
cc <- rbind(cc, data.frame(PolygonsID = "Linear Features",
                           File = file.path(rootPth, "utilities.shp")))
cc <- rbind(cc, data.frame(PolygonsID = "Ranges", 
                           File = file.path(rootPth, "projectPoly.shp")))
cc$Timestep <- NA
cc <- rbind(cc, data.frame(PolygonsID = "Linear Features", 
                           Timestep = 2020,
                           File = file.path(rootPth, "roads.shp")))
cc <- rbind(cc, data.frame(PolygonsID = "Linear Features", 
                           Timestep = 2040,
                           File = file.path(rootPth, "esker.shp")))
saveDatasheet(datScn, cc, name = cSheet, append = FALSE)

dependency(datScn, rcScn)

# scenario - caribou - current #===============================
cbScn <- scenario(cProj, "Caribou - 2020-2100")

cSheet <- "core_Pipeline"
cc <- data.frame(StageNameID = c("Prepare Spatial Data", "Caribou Habitat"),
                 RunOrder = 1:2)
saveDatasheet(cbScn, cc, name = cSheet)

cSheet <- "ROFSim_RunCaribouRange"
cc <- data.frame(Range = "Nipigon", CoeffRange = "Nipigon")
saveDatasheet(cbScn, cc, name = cSheet)

cSheet <- "ROFSim_CaribouDataSource"
cc <- data.frame(LandCoverRasterID = "Provincial Land Cover", 
                 ProjectShapeFileID = "Ranges",
                 EskerShapeFileID = "Eskers",
                 LinearFeatureShapeFileID = "Linear Features",
                 EskerRasterID = "Eskers",
                 LinearFeatureRasterID = "Linear Features",
                 NaturalDisturbanceRasterID = "Natural Disturbance", 
                 AnthropogenicRasterID = "Anthropogenic Disturbance")
saveDatasheet(cbScn, cc, name = cSheet)

cSheet <- "ROFSim_CaribouModelOptions"
cc <- data.frame(RunDistMetrics = TRUE, RunCaribouHabitat = TRUE, 
                 RunDemographicModel = TRUE, padProjPoly = TRUE)
saveDatasheet(cbScn, cc, name = cSheet)

# cSheet <- "ROFSim_RasterFile"
# cc <- data.frame(RastersID = "Natural Disturbance", 
#                  Filename = file.path(rootPth, "natDist.tif"))
# cc <- rbind(cc, data.frame(RastersID = "Harvest", 
#                            Filename = file.path(rootPth, "anthroDist.tif")))
# cc <- rbind(cc, data.frame(RastersID = "Provincial Land Cover", 
#                            Filename = file.path(rootPth, "landCover.tif")))
# cc$Timestep <- NA
# cc <- rbind(cc, data.frame(RastersID = "Harvest", 
#                            Timestep = 2040,
#                            Filename = file.path(rootPth, "linFeatTif.tif")))
# saveDatasheet(cbScn, cc, name = cSheet, append = FALSE)
# 
# cSheet <- "ROFSim_ExternalFile"
# cc <- data.frame(PolygonsID = "Eskers", File = file.path(rootPth, "esker.shp"))
# cc <- rbind(cc, data.frame(PolygonsID = "Linear Features", 
#                            File = file.path(rootPth, "rail.shp")))
# cc <- rbind(cc, data.frame(PolygonsID = "Linear Features",
#                            File = file.path(rootPth, "utilities.shp")))
# cc <- rbind(cc, data.frame(PolygonsID = "Ranges", 
#                            File = file.path(rootPth, "projectPoly.shp")))
# cc$Timestep <- NA
# cc <- rbind(cc, data.frame(PolygonsID = "Linear Features", 
#                            Timestep = 2020,
#                            File = file.path(rootPth, "roads.shp")))
# cc <- rbind(cc, data.frame(PolygonsID = "Linear Features", 
#                            Timestep = 2040,
#                            File = file.path(rootPth, "esker.shp")))
dependency(cbScn, datScn)

# run the scenario
# cbRes <- run(cbScn)



# Open the library in the SyncroSim UI
shell.exec(filepath(cLib))

