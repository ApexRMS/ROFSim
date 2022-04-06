# ROFSim - Transformer 2 - Generate LCC from cohort

# Set transformer name
transformerName <-"Generate LCC from Cohort Data"

# Packages ----------------------------------------------------------------

library(rsyncrosim)
library(raster)
library(qs)
library(data.table)
library(magrittr)
library(dplyr)
library(SpaDES.core)
library(SpaDES.tools)
library(caribouMetrics)

# Load Environment --------------------------------------------------------

e <- ssimEnvironment()
myLib <- ssimLibrary()
mySce <- scenario()

# Get all datasheets ------------------------------------------------------

# Access all datasheets of importance
myDatasheets <- datasheet(mySce)

# Only select datasheets from the ROF package
subFilter <- sapply(X = myDatasheets$name, FUN = grepl, pattern="^(ROF)\\w+")
myDatasheetsFiltered <- myDatasheets[subFilter,]
myDatasheetsNames <- myDatasheetsFiltered$name

# Source Functions --------------------------------------------------------

source(file.path(e$PackageDirectory, "makeLCCFromCohortData_helper.R"))
source(file.path(e$PackageDirectory, "helpers.R"))
legendPath <- e$PackageDirectory
source(file.path(legendPath,"legendHelpers.R"))

#inLCC<-raster("C:/Users/HughesJo/Documents/gitprojects/ChurchillAnalysis/inputNV/Provincial-Landcover-2000/FarNorthLandCover/Version 1.4/TIF Format/New folder/Class/FarNorth_LandCover_Class_RoF.tif")

lccClassTable = data.table(
  standLeading = c("Conifer Treed", "Mixed Treed", "Deciduous Treed"), 
  LCCclass = c(18,17,16)) # HARDCODED TO MATCH RESOURCE TYPES

#sparseness classes - in terms of SpaDES far north landcover classes
fileName <- "colormap_mapID_ROFSim_InputRastersMap-IDtemplateC.txt"
lTab = read.csv(paste(legendPath, fileName, sep="/"))
names(lTab)=c("ID","RGB1","RGB2","RGB3","C","Label")
lTab$ID = as.numeric(lTab$ID)
lTab=subset(lTab,!is.na(RGB1))
cTab = farNorthLandcover(lTab)

lccSparsenessTable = subset(cTab,select=c("ID","type"))
names(lccSparsenessTable)=c("LCCclass","sparseness")
# Omit everything except dense classes because not clear how transitions work for sparse classes
# Note this is currently only used to id dense classes that are eligible for transition.
# Assuming all other landcover classes remain static.
lccSparsenessTable<-subset(lccSparsenessTable,grepl("Treed",sparseness))

# Specify class and age threshold for 
youngTab <- subset(cTab, type=="young",select=c(ID, Label))
youngTab$AgeThreshold <- 20 # From Far North Landcover Classification descriptions
youngTab$isTreed = F; youngTab$isTreed[grepl("Treed",youngTab$Label)]=T

# SpaDES Info -------------------------------------------------------------

# Get the spades datasheet 
spadesDatasheet <- datasheet(ssimObject = mySce, name = "ROFSim_SpaDESGeneral")

# If datasheet is not empty, get the path
if(nrow(spadesDatasheet) == 0){
  stop("No SpaDES files specified.")
} else {
  if (sum(is.na(spadesDatasheet$Filename))>0){
    stop("No SpaDES files specified.")
  }
}

# Run control -------------------------------------------------------------
runControlSheet <- datasheet(mySce, "RunControl")
if (nrow(runControlSheet) == 0){
  stop("Run Control datasheet is empty.")
}

# Iterations
iterationSet <- runControlSheet$MinimumIteration:runControlSheet$MaximumIteration

if(sum(!(iterationSet %in% unique(spadesDatasheet$Iteration)))>0){
  stop("Iterations required by run control are not encoded in SpaDES datasheet")
} else{
  spadesDatasheet <- spadesDatasheet %>% 
    filter(Iteration %in% iterationSet)
}

# Timesteps
timestepSet <- seq(from = runControlSheet$MinimumTimestep,
                   to = runControlSheet$MaximumTimestep, 
                   by = runControlSheet$OutputFrequency)

# Extract variables -------------------------------------------------------

# Transfer dir
tmp <- e$TransferDirectory

# Get datasheet
outputSheet <- datasheet(mySce, "RasterFile", lookupsAsFactors = FALSE, 
                         empty = TRUE, optional = TRUE)

for (theIter in iterationSet){
  #theIter=1
  # Load the spades object 
  spadesObjectPath <- spadesDatasheet %>% 
    filter(Iteration == theIter) %>% 
    pull(Filename)
  
  outputs <- getSpadesOutputs(spadesObjectPath,timestepSet)%>% 
    filter(objectName %in% c("cohortData", "pixelGroupMap","standAgeMap"))

  preamblePath = strsplit(spadesObjectPath,"/",fixed=T)[[1]]
  typeBit = preamblePath[length(preamblePath)]
  preamblePath = preamblePath[1:(length(preamblePath)-2)]
  lccPath = paste0(c(preamblePath,"LCC.tif"),collapse="/")
  
  if(!file.exists(lccPath)){
    typeBit = paste0("simOutPreamble_",typeBit)
    typeBit = gsub("_SSP","_",typeBit,fixed=T)
    typeBit = strsplit(typeBit,"_",fixed=T)[[1]]
    typeBit = typeBit[1:(length(typeBit)-1)]
    typeBit=paste0(paste(typeBit,collapse="_"),".qs")
    spadesPreamble<-qs::qread(paste0(c(preamblePath,typeBit),collapse="/"))
    rstLCC <- spadesPreamble$LCC
    writeRaster(rstLCC,lccPath)
    rm(spadesPreamble)
  }else{  
    rstLCC<-raster(lccPath)
  }

  for (ts in sort(unique(outputs$Timestep))){
    #ts=2020
    print(paste(theIter,ts))
    
    outputsFiltered <- outputs %>% 
      filter(Timestep == ts)
    
    if(nrow(outputsFiltered) >3){ stop("invalid number of outputs") }
    
    cohort_data <- as.data.table(qs::qread(outputsFiltered %>% 
                                             filter(objectName == "cohortData") %>% 
                                             pull(file)))
    pixelGroupMap <- raster(outputsFiltered %>% 
                              filter(objectName == "pixelGroupMap") %>% 
                              pull(file))
    names(pixelGroupMap) <- "pixelGroup"
    
    # Make file name
    filePathLeading <- file.path(tmp, paste0("Leading", "_", paste(paste0("it_",theIter), 
                                                        paste0("ts_",ts), sep = "_"), 
                                      ".tif"))
    filePathLCC <- file.path(tmp, paste0("LCC", "_", paste(paste0("it_",theIter), 
                                                                   paste0("ts_",ts), sep = "_"), 
                                             ".tif"))
    
    # Populate sheet
    rStack <- getLCCFromCohortData(cohortData = cohort_data,
                                   pixelGroupMap = pixelGroupMap,
                                   rstLCC = rstLCC,
                                   lccClassTable = lccClassTable,
                                   lccSparsenessTable=lccSparsenessTable,
                                   e = e)

    
    #identify recently disturbed areas
    ageMap <- raster(outputsFiltered %>% 
                              filter(objectName == "standAgeMap") %>% 
                              pull(file))
    names(ageMap) <- "age"
    isDisturbed <- ageMap <= unique(youngTab$AgeThreshold)
    isDisturbed <- reclassify(isDisturbed, rcl = matrix(c(0, NA), ncol = 2), 
                              right = NA)
    
    # If isDisturbed and isTreed change LCC to 20 if isDisturbed and !isTreed 19
    # otherwise LCC
    isTreed <- !is.na(rStack[[2]])
    
    # if 1 disturbed and not treed if 2 disturbed and treed
    toUpdate <- isDisturbed + isTreed
    
    updatedLCCras <- mask(rStack[[1]], toUpdate, maskvalue = 2, 
                           updatevalue = filter(youngTab, isTreed) %>% pull(ID))
    
    updatedLCCras <- mask(updatedLCCras, toUpdate, maskvalue = 1, 
                           updatevalue = filter(youngTab, !isTreed) %>% pull(ID))

    writeRaster(rStack[[2]], overwrite = TRUE,
                filename = filePathLeading)

    writeRaster(updatedLCCras, overwrite = TRUE,
                filename = filePathLCC)
    
    rm(cohort_data);rm(pixelGroupMap);rm(rStack);rm(updatedLCCras)
    #sort(sapply(ls(), function(x) {object.size(get(x)) }))
    
    tmpsheet <- data.frame(RastersID = c("SpaDES Land Cover","SpaDES Leading Type"), 
                           Filename = c(filePathLCC,filePathLeading) 
                           )
    tmpsheet$Iteration = theIter 
    tmpsheet$Timestep = ts
    tmpsheet$TransformerID = transformerName
    outputSheet <- bind_rows(outputSheet, tmpsheet)
    
  }
  #rstLCC=NULL
  #sort( sapply(ls(),function(x){object.size(get(x))})) 
}

saveDatasheet(mySce, outputSheet, "RasterFile", append = TRUE)
