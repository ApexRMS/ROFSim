# ROFSim - Transformer 4 - Run Bird Models
library(rsyncrosim)
library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(gbm)

localDebug = F
if(!localDebug){
  # Load environment
  e <- ssimEnvironment()
  myLib <- ssimLibrary()
  mySce <- scenario()
  # Source the helpers
  source(file.path(e$PackageDirectory, "helpers.R"))
  # moved from helpers
  GLOBAL_Session = session()
  GLOBAL_Library = ssimLibrary(session = GLOBAL_Session)
  GLOBAL_Project = project(GLOBAL_Library, project = as.integer(e$ProjectId))
  GLOBAL_Scenario = scenario(GLOBAL_Library, scenario = as.integer(e$ScenarioId))
  GLOBAL_RunControl = GetDataSheetExpectData("ROFSim_RunControl", GLOBAL_Scenario)
  GLOBAL_MaxIteration = GetSingleValueExpectData(GLOBAL_RunControl, "MaximumIteration")
  GLOBAL_MinIteration = GetSingleValueExpectData(GLOBAL_RunControl, "MinimumIteration")
  GLOBAL_MinTimestep = GetSingleValueExpectData(GLOBAL_RunControl, "MinimumTimestep")
  GLOBAL_MaxTimestep = GetSingleValueExpectData(GLOBAL_RunControl, "MaximumTimestep")
  GLOBAL_TotalIterations = (GLOBAL_MaxIteration - GLOBAL_MinIteration + 1)
  GLOBAL_TotalTimesteps = (GLOBAL_MaxTimestep - GLOBAL_MinTimestep + 1)
}else{
  e=list()
  e$PackageDirectory = "C:/Users/HughesJo/Documents/SyncroSim/Packages/ROFSim"
  t = try(source(file.path(e$PackageDirectory, "helpers.R")),silent=T) #this will throw Error in .local(.Object, ...) : A library name is required. Don't worry about it.
  source("./scripts/loadSSimLocalForDebug.R") #run outside of SSim for debugging caribouMetrics package
}
# Get all datasheets ------------------------------------------------------
myDatasheetsNames <- c("RasterFile",
                       "ExternalFile",
                       "RunBirdSpecies",
                       "BirdModelDir")
loadDatasheet <- function(name){
  sheet <- tryCatch(
    {
      datasheet(mySce, name = name, lookupsAsFactors = FALSE,
                optional = TRUE)
    },
    error = function(cond){
      return(NULL)
    },
    warning = function(cond){
      return(NULL)
    }
  )
}
allParams <- lapply(myDatasheetsNames, loadDatasheet)
names(allParams) <- myDatasheetsNames

# Filter Timesteps --------------------------------------------------------
uniqueIterFromData <-
  unique(c(allParams$ExternalFile$Iteration,
           allParams$RasterFile$Iteration))
uniqueIterFromData <- uniqueIterFromData[!is.na(uniqueIterFromData)]
if(length(uniqueIterFromData)==0){uniqueIterFromData<-GLOBAL_MinIteration}
uniqueTsFromData <-
  unique(c(allParams$ExternalFile$Timestep,
           allParams$RasterFile$Timestep))
uniqueTsFromData <- uniqueTsFromData[!is.na(uniqueTsFromData)]
if(length(uniqueTsFromData)==0){uniqueTsFromData<-GLOBAL_MinTimestep}

iterationSet <- GLOBAL_MinIteration:GLOBAL_MaxIteration
iterationSet <- iterationSet[iterationSet %in% uniqueIterFromData]
timestepSet <- seq(GLOBAL_MinTimestep,GLOBAL_MaxTimestep,by=GLOBAL_RunControl$OutputFrequency)

# Run model ---------------------------------------------------------------
progressBar(type = "begin", totalSteps = length(iterationSet) * length(timestepSet))

# Avoid growing list to help memory allocation time
birdDensAll <- vector("list", length = length(iterationSet))
birdDensAll <- lapply(birdDensAll,
                      function(x){vector("list", length = length(timestepSet))})
birdDensAll <- setNames(birdDensAll, paste0("it_", iterationSet)) %>%
  lapply(function(x) {setNames(x, paste0("ts_", timestepSet))})


allParams$RasterFile=unique(allParams$RasterFile)
allParams$ExternalFile=unique(allParams$ExternalFile)

for (iteration in iterationSet) {
  for (tt in seq_along(timestepSet)) {
    #iteration=1;tt=1
    timestep=timestepSet[tt]
    iteration=1;tt=1
    #iteration=1;tt=1
    timestep=timestepSet[tt]
    if(tt==length(timestepSet)){
      numSteps=1
    }else{
      numSteps=timestepSet[tt+1]-timestep
      if(numSteps<=0){
        stop("Bug: timestepSet should be sorted low to high.")
      }
    }
    
    progressBar(type = "report", iteration, timestep)
    
    # Filter inputs based on iteration and timestep
    InputRastersNA <- filterInputs(subset(allParams$RasterFile,is.na(Timestep)),
                                   iteration, timestep, min(timestepSet),useMostRecent="RastersID")
    InputRastersT <- filterInputs(subset(allParams$RasterFile,!is.na(Timestep)),
                                  iteration, timestep, min(timestepSet),useMostRecent="RastersID")
    InputVectorsNA <- filterInputs(subset(allParams$ExternalFile,is.na(Timestep)),
                                   iteration, timestep, min(timestepSet))
    InputVectorsT <- filterInputs(subset(allParams$ExternalFile,!is.na(Timestep)),
                                  iteration, timestep, min(timestepSet),useMostRecent="PolygonsID")
    # skip landscape calcs if no change since previous timestep
    if((all(nrow(InputRastersT) == 0, nrow(InputVectorsT) == 0) ||
        all(c(InputRastersT$noChng, InputVectorsT$noChng))) &&
       timestep != min(timestepSet)){
      doLandscape <- FALSE
    } else {
      doLandscape <- TRUE
    }
    InputRasters=rbind(InputRastersNA,InputRastersT)
    InputRasters=subset(InputRasters,!is.na(Filename))
    InputVectors=rbind(InputVectorsNA,InputVectorsT)
    InputVectors=subset(InputVectors,!is.na(File))
    InputRasters
    
    if(doLandscape){
      plcRas <-  tryCatch({
        raster(filter(InputRasters, RastersID == "Provincial Land Cover")$File)
      }, error = function(cond) { stop("land cover can't be null") })
      
      # layerize plc
      plc_layers <- raster::layerize(plcRas)
      
      # translate plc values to land cover class names
      plc_classes <- read.csv(file.path(allParams$BirdModelDir$BirdModelDir, 
                                        "plcClasses.csv"))
      plc_classes <- plc_classes %>%
        mutate(Class = Class %>% str_replace_all("[^[:alpha:]]", "_") %>%
                 str_replace_all("\\_+", "_"))
      
      names(plc_layers) <- plc_classes %>%
        filter(Code %in% raster::unique(plcRas)) %>%
        pull(Class)
      
      eskerRas <- tryCatch({
        raster(filter(InputRasters, RastersID == "Eskers")$File) >0
      }, error = function(cond) { stop("Eskers are required")})
      
      # use linear feature raster in caribouMetrics and lines in disturbance
      linFeatRas <- tryCatch({
        filtered <- filter(InputRasters, RastersID == "Linear Features")$File
        raster(filtered) > 0
      }, error = function(cond) { NULL })
      
      projectPol <- st_read(filter(InputVectors, RastersID == "Study Area")$File)
      
      # do moving window
      rastfw750 <- raster::focalWeight(plc_layers, 750, type = "Gauss")
      plc_layers750 <- purrr::map(1:raster::nlayers(plc_layers),
                                  ~pfocal::pfocal(plc_layers[[.x]], rastfw750,
                                                  transform_function = "MULTIPLY",
                                                  reduce_function = "SUM",
                                                  mean_divider = "KERNEL_COUNT"))
      plc_layers750 <- map(plc_layers750, ~`names<-`(.x, paste0(names(.x), "_750")))
      
      # make a raster stack of predictors
      pred_stk <- raster::stack(plc_layers750, linFeatRas, eskerRas)
      
      SPP <- allParams$RunBirdSpecies$BirdSpecies
      
      # load models
      bird_mods <- purrr::map(SPP,
                              ~list.files(file.path(sourceData, "ROFBirdModels"),
                                          pattern = paste0(.x, ".*rds"),
                                          full.names = TRUE))
      
      # make predictions
      pred_out <- purrr::map2(
        bird_mods, SPP,
        ~raster::predict(pred_stk, readRDS(.x),
                         filename = file.path(e$TransferDirectory,
                                              paste0(paste("OutputBirdDensity",
                                                           SPP,
                                                           "it", iteration,
                                                           "ts", timestep,
                                                           sep= "_"), ".tif")),
                         type = "response", overwrite = TRUE)
      )
      
      # Build df and save the datasheet
      birdDensDf <- data.frame(BirdSpeciesID = SPP, 
                               Iteration = iteration,
                               Timestep = timestep,
                               FileName =  file.path(e$TransferDirectory, 
                                                     paste0(paste("OutputBirdDensity",
                                                                  SPP,
                                                                  "it", iteration, 
                                                                  "ts", timestep,
                                                                  sep= "_"), ".tif")))
      
    } else {
      birdDensDf <- birdDensAll[[paste0("it_",iteration)]][[paste0("ts_",timestepSet[tt-1])]]
      birdDensDf$Timestep <- timestep 
    }
    
    birdDensAll[[paste0("it_",iteration)]][[paste0("ts_",timestep)]] <- 
      birdDensDf
  }
}

birdDensMerged <- data.frame(bind_rows(unlist(birdDensAll, recursive = F)))
saveDatasheet(ssimObject = mySce, name = "OutputBirdDensity", data = birdDensMerged)



