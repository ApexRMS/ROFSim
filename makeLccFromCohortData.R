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

#names(spadesObject)
#outLCC<-raster("C:/Users/HughesJo/Documents/gitprojects/ChurchillAnalysis/inputNV/Provincial-Landcover-2000/FarNorthLandCover/Version 1.4/TIF Format/New folder/Class/FarNorth_LandCover_Group_RoF.tif")
outLCC <- raster("C:/Users/endicotts/Documents/gitprojects/ROFSyncSim/ROFDemo_data/plc250.tif") 
# inRat = levels(outLCC)[[1]]
# 
# inRat[1:12,]
# 
# # Output classes - in terms of outLCC
# inRat

lccClassTable = data.table(
  standLeading = c("pureCon_dense", "pureCon_open", "pureCon_sparse",
                   "pureDec_dense","pureDec_open", "pureDec_sparse",
                   "mixed_dense","mixed_open", "mixed_sparse"), 
  LCCclass = c(1,2,3,
               4,5,6, 
               7,8,9)) # HARDCODED TO MATCH RESOURCE TYPES
# WHICH RESOURCE TYPES!!? the resource types in caribouMetrics eg 6 = water

#sparseness classes - in terms of SpaDES far north landcover classes
legendPath <- "."
source(paste0(legendPath,"/legendHelpers.R"))
fileName <- "colormap_mapID_ROFSim_InputRastersMap-IDtemplateC.txt"
lTab = read.csv(paste(legendPath, fileName, sep="/"))
names(lTab)=c("ID","RGB1","RGB2","RGB3","C","Label")
lTab$ID = as.numeric(lTab$ID)
lTab=subset(lTab,!is.na(RGB1))
cTab = farNorthLandcover(lTab)

lccSparsenessTable = subset(cTab,select=c("ID","type"))
lccSparsenessTable$type[lccSparsenessTable$type=="young"]="young dense"

names(lccSparsenessTable)=c("LCCclass","sparseness")
lccSparsenessTable=subset(lccSparsenessTable,(grepl("sparse",sparseness)|grepl("dense",sparseness)|grepl("open",sparseness)))


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

# Function for reclassifying LCC05 values to resource types
LCC05ToResType <- function(FromList, ToVal, landcover){
  reclassDT <- data.table("Is" = FromList,
                          "becomes" = rep(ToVal, length(FromList)))
  
  reclassedLCC <- raster::reclassify(landcover, reclassDT)
  
  return(reclassedLCC)
}

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
  #sort( sapply(ls(),function(x){object.size(get(x))})) 
  spadesObject=NULL
  spadesObject <- qs::qread(spadesObjectPath)
  
  # Filter them
  outputs <- outputs(spadesObject) %>% 
    make_paths_relative("outputs") %>% 
    filter(objectName %in% c("cohortData", "pixelGroupMap")) %>% 
    filter(saveTime %in% timestepSet) %>% 
    rename(Timestep = saveTime)
  
  # For now, reconstruct the relative paths based on basenames
  outputs$file <- file.path(dirname(spadesObjectPath), basename(outputs$file))
  
  rm(spadesObject)
  
  preamblePath = strsplit(spadesObjectPath,"/",fixed=T)[[1]]
  typeBit = preamblePath[length(preamblePath)]
  preamblePath = preamblePath[1:(length(preamblePath)-2)]
  
  typeBit = paste0("simOutPreamble_",typeBit)
  typeBit = gsub("_SSP","_",typeBit,fixed=T)
  typeBit = strsplit(typeBit,"_",fixed=T)[[1]]
  typeBit = typeBit[1:(length(typeBit)-2)]
  typeBit=paste0(paste(typeBit,collapse="_"),".qs")
  spadesPreamble<-qs::qread(paste0(c(preamblePath,typeBit),collapse="/"))
  rstLCC <- spadesPreamble$LCC
  rm(spadesPreamble)
  #freq(rstLCC)
  #freq(outLCC)

  for (ts in sort(unique(outputs$Timestep))){
    #ts=2020
    print(paste(theIter,ts))
    
    outputsFiltered <- outputs %>% 
      filter(Timestep == ts)
    
    if(nrow(outputsFiltered) != 2){ stop("invalid number of outputs") }
    
    cohort_data <- as.data.table(qs::qread(outputsFiltered %>% 
                                             filter(objectName == "cohortData") %>% 
                                             pull(file)))
    pixelGroupMap <- raster(outputsFiltered %>% 
                              filter(objectName == "pixelGroupMap") %>% 
                              pull(file))
    names(pixelGroupMap) <- "pixelGroup"
    rstLCC <- raster::resample(rstLCC,
                               pixelGroupMap,method="ngb")

    outLCC <- raster::resample(outLCC,pixelGroupMap,method="ngb")
    # Make file name
    filePathLeading <- file.path(tmp, paste0("Leading", "_", paste(paste0("it_",theIter), 
                                                        paste0("ts_",ts), sep = "_"), 
                                      ".tif"))
    filePathLCC <- file.path(tmp, paste0("LCC", "_", paste(paste0("it_",theIter), 
                                                                   paste0("ts_",ts), sep = "_"), 
                                             ".tif"))
    
    # Populate sheet
    updated_Leading_tmp <- makeLCCfromCohortData(cohortData = cohort_data,
                                             pixelGroupMap = pixelGroupMap,
                                             rstLCC = rstLCC,
                                             lccClassTable = lccClassTable,
                                             lccSparsenessTable=lccSparsenessTable)
    
    #freq(updated_Leading_tmp)
    
    #staticMask = !((rstLCC==11)|(rstLCC==12)|(rstLCC==13))
    #updated_LCC_tmp[staticMask]=rstLCC[staticMask]
    writeRaster(updated_Leading_tmp, overwrite = TRUE,
                filename = filePathLeading)

    writeRaster(outLCC, overwrite = TRUE,
                filename = filePathLCC)
    
    rm(cohort_data);rm(pixelGroupMap);rm(updated_LCC_tmp);rm(updated_Leading_tmp)
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
