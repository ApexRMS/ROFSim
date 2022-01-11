# ROFSim - Transformer 3 - Run Caribou Model

# Packages ----------------------------------------------------------------

library(rsyncrosim)
library(caribouMetrics)
library(raster)
library(sf)
library(dplyr)
library(tidyr)

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
                       "RunCaribouRange", 
                       "CaribouModelOptions",
                       "CaribouDataSource")

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

# Modify the source data table
allParams$CaribouDataSourceWide <- allParams$CaribouDataSource %>%
  pivot_longer(values_to = "VarID", names_to = "CaribouVarID",
               cols=tidyselect::all_of(names(allParams$CaribouDataSource))) %>%
  rowwise() %>%
  mutate(type=ifelse(grepl("Raster", CaribouVarID, fixed=TRUE), "raster", "shapefile")) %>%
  ungroup() %>% drop_na() %>% as.data.frame()

# Get variables -----------------------------------------------------------

if (nrow(allParams$RasterFile > 0)){
  allParams$RasterFile <- allParams$RasterFile %>%
    left_join(filter(allParams$CaribouDataSourceWide, type=="raster"),
              by = c("RastersID" = "VarID")) %>%
    as.data.frame()
}

if (nrow(allParams$ExternalFile > 0)){
  allParams$ExternalFile <- allParams$ExternalFile %>%
    left_join(filter(allParams$CaribouDataSourceWide, type == "shapefile"),
              by = c("PolygonsID" = "VarID")) %>%
    as.data.frame()
}

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
#timestepSet <- timestepSet[timestepSet %in% uniqueTsFromData]

# Default parameter values from R package
argList = list(bufferWidth=NA,padProjPoly=NA,padFocal=NA,
               NumDemographicTrajectories=35,modelVersion=NA,survivalModelNumber=NA,recruitmentModelNumber=NA,
               InitialPopulation=1000,P_0=NA,P_K=NA,a=NA,b=NA,K=NA,r_max=NA,s=NA,
               l_R=NA,h_R=NA,l_S=NA,h_S=NA,interannualVar="list(R_CV = 0.46, S_CV = 0.08696)",probOption=NA)
defaults= c(formals(demographicCoefficients),formals(disturbanceMetrics),formals(demographicRates),formals(popGrowthJohnson))

for(i in 1:length(argList)){
  #i=20
  argName = names(argList)[i]
  argVal = argList[[argName]]
  
  if(is.null(optArg(allParams$CaribouModelOptions[[argName]]))){
    if(is.na(argVal)){
      allParams$CaribouModelOptions[[argName]]=defaults[[argName]]
    }else{
      allParams$CaribouModelOptions[[argName]] = argVal
    }
  }
}

doDistMetrics=optArg(allParams$CaribouModelOptions$RunDistMetrics)
doDemography=optArg(allParams$CaribouModelOptions$RunDemographicModel)
if(grepl("list",allParams$CaribouModelOptions$interannualVar)){
  allParams$CaribouModelOptions$interannualVar=list(eval(parse(text=allParams$CaribouModelOptions$interannualVar))) 
}
if(!grepl("M",allParams$CaribouModelOptions$recruitmentModelNumber)){
  allParams$CaribouModelOptions$recruitmentModelNumber=paste0("M",allParams$CaribouModelOptions$recruitmentModelNumber)
}
if(!grepl("M",allParams$CaribouModelOptions$survivalModelNumber)){
  allParams$CaribouModelOptions$survivalModelNumber=paste0("M",allParams$CaribouModelOptions$survivalModelNumber)
}

# Run model ---------------------------------------------------------------
progressBar(type = "begin", totalSteps = length(iterationSet) * length(timestepSet))

# Avoid growing list to help memory allocation time
habitatUseAll <- vector("list", length = length(iterationSet))
habitatUseAll <- lapply(habitatUseAll, 
                        function(x){vector("list", length = length(timestepSet))})
habitatUseAll <- setNames(habitatUseAll, paste0("it_", iterationSet)) %>% 
  lapply(function(x) {setNames(x, paste0("ts_", timestepSet))})
distMetricsAll <- habitatUseAll
distMetricsTabAll <- habitatUseAll
popMetricsTabAll <- habitatUseAll

for (iteration in iterationSet) {
  if(is.null(doDemography)||doDemography){
    #demographic rates from disturbance metrics.
    #regression model parameter sampling is done once for each population at the beginning of the simulation 
    popGrowthPars <- demographicCoefficients(allParams$CaribouModelOptions$NumDemographicTrajectories,
                                             modelVersion = allParams$CaribouModelOptions$modelVersion,
                                             survivalModelNumber = allParams$CaribouModelOptions$survivalModelNumber,
                                             recruitmentModelNumber = allParams$CaribouModelOptions$recruitmentModelNumber)
    N=allParams$CaribouModelOptions$InitialPopulation
    pars = data.frame(N0=N)
  }  

  #Note: assuming timestepSet is ordered low to high
  for (tt in seq_along(timestepSet)) {
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
    print(iteration)
    print(timestep)
    envReportProgress(iteration, timestep)
    
    # Filter inputs based on iteration and timestep
    InputRastersNA <- filterInputs(subset(allParams$RasterFile,is.na(Timestep)), 
                                 iteration, timestep, min(timestepSet),useMostRecent="RastersID")
    InputRastersT <- filterInputs(subset(allParams$RasterFile,!is.na(Timestep)), 
                                   iteration, timestep, min(timestepSet),useMostRecent="RastersID")
    InputRasters=rbind(InputRastersNA,InputRastersT)
    InputRasters=subset(InputRasters,!is.na(Filename))
    InputVectorsNA <- filterInputs(subset(allParams$ExternalFile,is.na(Timestep)),
                                 iteration, timestep, min(timestepSet))
    InputVectorsT <- filterInputs(subset(allParams$ExternalFile,!is.na(Timestep)),
                                   iteration, timestep, min(timestepSet),useMostRecent="PolygonsID")
    InputVectors=rbind(InputVectorsNA,InputVectorsT)
    InputVectors=subset(InputVectors,!is.na(File))
    
    # Call the main function with all arguments extracted from datasheets
    plcRas <-  tryCatch({
      raster(filter(InputRasters, CaribouVarID == "LandCoverRasterID")$File)
    }, error = function(cond) { stop("land cover can't be null") })
    
    # Reclass landcover if needed
    # UI TO DO: allow user to input plcLU table (same format as plcToResType in caribouMetrics package). If table is specified, reclass regardless of whether the number of classes is <9.
    # TO DO: need better way of recognizing landcover class types - maybe just require user to specify? # of classes assumptions will potentially cause trouble on reduced landscapes where not all classes are represented.
    if ((max(values(plcRas), na.rm = TRUE) <= 9)){
      warning(paste0("Assuming landcover classes are: ",paste(paste(resTypeCode$ResourceType,resTypeCode$code),collapse=",")))
    }else if(is.element((max(values(plcRas), na.rm = TRUE)), c(28:30))){
      #TO DO: add PLC legend file to caribouMetrics package, and report here.
      warning(paste0("Assuming Ontario provincial landcover classes: ",paste(paste(plcToResType$ResourceType,plcToResType$PLCCode),collapse=",")))
      plcRas[plcRas==30]=29
      plcRas <- reclassPLC(plcRas,plcToResType)
    }else if((max(values(plcRas), na.rm = TRUE) == 39)){
      warning(paste0("Assuming national landcover classes: ",paste(paste(lccToResType$ResourceType,lccToResType$PLCCode),collapse=",")))
      plcRas <- reclassPLC(plcRas,lccToResType)
    }else{
      stop("Landcover classification not recognized. Please specify...")
    }
    
    eskerRas <- tryCatch({
      raster(filter(InputRasters, CaribouVarID == "EskerRasterID")$File)
    }, error = function(cond) { stop("Eskers are required")})
    
    # always use raster esker since it has been converted to density
      eskerFinal <- eskerRas
    
    natDistRas <- tryCatch({
      raster(filter(InputRasters, CaribouVarID == "NaturalDisturbanceRasterID")$File)
    }, error = function(cond) { NULL })

    #If this raster contains something that looks like ages, interpret as time since natural disturbance.
    if(!is.null(natDistRas)&&(cellStats(natDistRas,"max")>10)){
      distPersistence=optArg(allParams$CaribouModelOptions$DisturbancePersistence)
      if(is.null(distPersistence)){distPersistence=40}
      natDistRas=natDistRas<=distPersistence
    }
    
    anthroDistRas <- tryCatch({
      raster(filter(InputRasters, CaribouVarID == "AnthropogenicRasterID")$File)
    }, error = function(cond) { NULL })
    
    harvRas <- tryCatch({
      raster(filter(InputRasters, CaribouVarID == "HarvestRasterID")$File)
    }, error = function(cond) { NULL })
    
    # use linear feature raster in caribouMetrics and lines in disturbance
    linFeatRas <- tryCatch({
      filtered <- filter(InputRasters, CaribouVarID == "LinearFeatureRasterID")$File
      raster(filtered)
    }, error = function(cond) { NULL })

    linFeatShp <- tryCatch({
      filtered <- filter(InputVectors, CaribouVarID == "LinearFeatureShapeFileID")$File
      read_sf(filtered)
    }, error = function(cond) { NULL })
    
    projectPol1 <- st_read(filter(InputVectors, CaribouVarID == "ProjectShapeFileID")$File) 
    if("RANGE_NAME" %in% colnames(projectPol1)){
      projectPol = rename(projectPol1, Range = RANGE_NAME)
    }else{
      if("Range" %in% colnames(projectPol1)){
        projectPol = projectPol1
      } else {
        if(nrow(projectPol1) == 1){
          projectPol = mutate(projectPol1, Range = allParams$RunCaribouRange$Range)
        } else{
          stop("Caribou range polygons must have a Range column")
        }
      }
    } 
    
    # Rename range in expected format
    renamedRange <- rename(allParams$RunCaribouRange, coefRange = CoeffRange)
    
    projectPoltmp <- projectPol %>% 
      filter(Range %in% renamedRange$Range) 

    if(is.null(doDistMetrics)||doDistMetrics){
      if(is.null(harvRas)){
        combineAnthro=anthroDistRas
      }else{
        if(is.null(anthroDistRas)){
          combineAnthro=harvRas
        }else{
          combineAnthro=harvRas+anthroDistRas
        }
      }
      
      # use preppedData list
      fullDist <- disturbanceMetrics(
        preppedData = list(refRast=!is.na(plcRas),
                           natDist = natDistRas,
                           anthroDist = combineAnthro,
                           linFeat = linFeatShp,
                           projectPolyOrig = projectPoltmp),
        padFocal = optArg(allParams$CaribouModelOptions$PadFocal),
        bufferWidth =  optArg(allParams$CaribouModelOptions$ECCCBufferWidth) 
      )
      
      # Build df and save the datasheet
      fds <- subset(fullDist@disturbanceMetrics,select=c(Range,Anthro,Fire,Total_dist,fire_excl_anthro))
      names(fds)[1]="RangeID"
      fds <- gather(fds, MetricTypeDistID, Amount, Anthro:fire_excl_anthro, factor_key=FALSE)
      distMetricsTabDf <- fds
      distMetricsTabDf$Iteration <- iteration
      distMetricsTabDf$Timestep <- timestep
      
      distMetricsTabAll[[paste0("it_",iteration)]][[paste0("ts_",timestep)]] <- 
        distMetricsTabDf
      
      ## Save to DATA folder
      writeRaster(fullDist@processedData, bylayer = TRUE, format = "GTiff",
                  suffix = paste(names(fullDist@processedData), 
                                 paste(renamedRange$Range, collapse = "_"),
                                 paste0("it_",iteration), 
                                 paste0("ts_",timestep), sep = "_"),
                  filename = file.path(e$TransferDirectory, "OutputDistMetrics"), 
                  overwrite = TRUE)
      
      # Build df and save the datasheet
      distMetricsDf <- data.frame(MetricTypeDistID = names(fullDist@processedData), 
                                  Iteration = iteration,
                                  Timestep = timestep)
      distMetricsDf$FileName <- file.path(e$TransferDirectory, 
                                          paste0(paste("OutputDistMetrics",
                                                       distMetricsDf$MetricTypeDistID,
                                                       paste(renamedRange$Range, collapse = "_"),
                                                       "it", distMetricsDf$Iteration, 
                                                       "ts", distMetricsDf$Timestep,
                                                       sep= "_"), ".tif"))
      distMetricsDf <- distMetricsDf %>% 
        expand_grid(RangeID = renamedRange$Range)
      
      distMetricsAll[[paste0("it_",iteration)]][[paste0("ts_",timestep)]] <- 
        distMetricsDf
      
      if(is.null(doDemography)||doDemography){
        covTableSim <- subset(fullDist@disturbanceMetrics,select=c(Anthro,Fire,Total_dist,fire_excl_anthro,Range)) 
        names(covTableSim)[names(covTableSim)=="Range"]=c("polygon")
        covTableSim$area="FarNorth"
        
        rateSamples <- demographicRates(
          covTable = covTableSim,
          popGrowthPars = popGrowthPars,
          ignorePrecision = FALSE,
          returnSample = TRUE,
          useQuantiles = TRUE        )     
        
        if(is.element("N",names(pars))){
          pars=subset(pars,select=c(scnID,polygon,area,replicate,N))
          names(pars)[names(pars)=="N"]="N0"
        }
        
        pars=merge(pars,rateSamples)
        
        pars = cbind(pars,popGrowthJohnson(pars$N0,numSteps=numSteps,R_bar=pars$R_bar,
                                            S_bar=pars$S_bar,
                                            P_0=allParams$CaribouModelOptions$P_0,
                                            P_K=allParams$CaribouModelOptions$P_K,
                                            a=allParams$CaribouModelOptions$a,
                                            b=allParams$CaribouModelOptions$b,
                                            K=allParams$CaribouModelOptions$K,
                                            r_max=allParams$CaribouModelOptions$r_max,
                                            s=allParams$CaribouModelOptions$s,
                                            l_R=allParams$CaribouModelOptions$l_R,
                                            h_R=allParams$CaribouModelOptions$h_R,
                                            l_S=allParams$CaribouModelOptions$l_S,
                                            h_S = allParams$CaribouModelOptions$h_S,
                                            interannualVar=allParams$CaribouModelOptions$interannualVar[[1]],
                                            probOption=allParams$CaribouModelOptions$probOption))
     
        # Build df and save the datasheet
        fds <- subset(pars,select=c(polygon,replicate,S_bar,R_bar,N,lambda))
        fds$replicate=as.numeric(gsub("V","",fds$replicate))
        names(fds)=c("RangeID","Replicate","survival","recruitment","N","lambda")
        fds <- pivot_longer(fds, !(RangeID|Replicate),names_to="MetricTypeDemogID",values_to="Amount")
        popMetricsTabDf <- fds
        popMetricsTabDf$Iteration <- iteration
        popMetricsTabDf$Timestep <- timestep
        
        popMetricsTabAll[[paste0("it_",iteration)]][[paste0("ts_",timestep)]] <- 
          popMetricsTabDf
        
      }
    }
    #TO DO: handle polygon inputs for natural disturbance, anthro disturbance, and harvest
    #TO DO: check that disturbanceMetrics calculations handle multiple ranges properly
    #TO DO: accept anthropogenic disturbance polygons or rasters, and behave properly when they are missing.
    #UI TO DO: add option to save elements of res@processedData
    #TO DO: speed up by using updateCaribou and implementing/using updateDisturbance to avoid repeat geospatial processing. 
    #       Would need to id which layers change over time.
    
    #Note: This code is helpful for building and sharing reproducible examples for debugging. Leave in for now.
    #d=list(landCover=readAll(plcRas),esker=eskerFinal,natDist=readAll(natDistRas),anthroDist=NULL,
    #       harv=readAll(harvRas),linFeat=linFeatFinal,projectPoly=projectPoltmp,caribouRange=renamedRange,
    #       padProjPoly=optArg(allParams$CaribouModelOptions$PadProjPoly),
    #       padFocal = optArg(allParams$CaribouModelOptions$PadFocal),
    #       doScale = optArg(allParams$CaribouModelOptions$doScale))
    #saveRDS(d,paste0("C:/Users/HughesJo/Documents/InitialWork/OntarioFarNorth/RoFModel/UI/debugData.RDS"))
    doCarHab <- optArg(allParams$CaribouModelOptions$RunCaribouHabitat)
    if(is.null(doCarHab)||doCarHab){
      res <- caribouHabitat(
        preppedData = list(refRast = plcRas, 
                           esker = eskerRas, 
                           natDist = natDistRas,
                           anthroDist = combineAnthro,
                           linFeat = linFeatRas, 
                           projectPolyOrig = projectPoltmp),
        caribouRange = renamedRange,       # Caribou Range
        # Options
        padProjPoly = optArg(allParams$CaribouModelOptions$PadProjPoly),
        padFocal = optArg(allParams$CaribouModelOptions$PadFocal),
        doScale = optArg(allParams$CaribouModelOptions$doScale)
      )
     
      ## Save to DATA folder
      writeRaster(res@habitatUse, bylayer = TRUE, format = "GTiff",
                  suffix = paste(names(res@habitatUse), 
                                 paste(renamedRange$Range, collapse = "_"),
                                 paste0("it_",iteration), 
                                 paste0("ts_",timestep), sep = "_"),
                  filename = file.path(e$TransferDirectory, "OutputHabitatUse"), 
                  overwrite = TRUE)
      
      # Build df and save the datasheet
      habitatUseDf <- data.frame(SeasonID = names(res@habitatUse), 
                                 Iteration = iteration,
                                 Timestep = timestep)
      habitatUseDf$FileName <- file.path(e$TransferDirectory, 
                                         paste0(paste("OutputHabitatUse",
                                                      habitatUseDf$Season,
                                                      paste(renamedRange$Range, collapse = "_"),
                                                      "it", habitatUseDf$Iteration, 
                                                      "ts", habitatUseDf$Timestep,
                                                      sep= "_"), ".tif"))
      habitatUseDf <- habitatUseDf %>% 
        expand_grid(RangeID = renamedRange$Range)
      
      habitatUseAll[[paste0("it_",iteration)]][[paste0("ts_",timestep)]] <- 
        habitatUseDf
      
    }
    
    #QUESTION: faster to crop to projectPoly or landCover?
  }
}

if(is.null(doCarHab)||doCarHab){
  habitatUseMerged <- bind_rows(unlist(habitatUseAll, recursive = F))
  saveDatasheet(ssimObject = mySce, name = "OutputSpatialHabitat", data = habitatUseMerged)
}

if(is.null(doDistMetrics)||doDistMetrics){
  distMetricsTabMerged <- bind_rows(unlist(distMetricsTabAll, recursive = F))
  
  distMetricsMerged <- data.frame(bind_rows(unlist(distMetricsAll, recursive = F)))
  saveDatasheet(ssimObject = mySce, name = "OutputSpatialDisturbance", data = distMetricsMerged)
  
  saveDatasheet(ssimObject = mySce, name = "OutputDisturbanceMetrics", data = distMetricsTabMerged)
  if(is.null(doDemography)||doDemography){
    popMetricsTabMerged <- data.frame(bind_rows(unlist(popMetricsTabAll, recursive = F)))
   
    #Force iteration to enable demographic plotting in case where we only have one landscape iteration.
    #Need better solution for scenario where there are multiple demographic replicates for more than one landscape iteration.
    if(length(unique(popMetricsTabMerged$Iteration))==1){
      popMetricsTabMerged$Iteration=popMetricsTabMerged$Replicate
    } 
    saveDatasheet(ssimObject = mySce, name = "OutputPopulationMetrics", data = popMetricsTabMerged)
  }
}

envEndSimulation()
