# ROFSim - Transformer 1 - Import SpaDES Outputs

# Set transformer name
transformerName <- "Spades Import"

# Packages ----------------------------------------------------------------

library(rsyncrosim)
library(raster)
library(qs)
library(data.table)
library(magrittr)
library(dplyr)
library(SpaDES.core)
library(SpaDES.tools)

# Load Environment --------------------------------------------------------

e <- ssimEnvironment()
myLib <- ssimLibrary()
mySce <- scenario()

# Source helper functions -------------------------------------------------
source(file.path(e$PackageDirectory, "makeLCCFromCohortData_helper.R"))
source(file.path(e$PackageDirectory, "helpers.R"))

# Get all datasheets ------------------------------------------------------

myDatasheets <- datasheet(mySce)

# Only select datasheets from the ROF package 
subFilter <- sapply(X = myDatasheets$name, FUN = grepl, pattern="^(ROF)\\w+")
myDatasheetsFiltered <- myDatasheets[subFilter,]
myDatasheetsNames <- myDatasheetsFiltered$name

# Figure out variables to extract from spades -----------------------------

# Get all vars data

allRasters <- datasheet(mySce, "ROFSim_Rasters", lookupsAsFactors = FALSE)

allPolygons <- datasheet(mySce, "ROFSim_Polygons", lookupsAsFactors = FALSE)

# Find out which variables to extract

spadesRasterVars <- datasheet(mySce, "ROFSim_SpaDESRuntimeRasters", 
                              lookupsAsFactors = FALSE)
if(nrow(spadesRasterVars)>0){
  spadesRasterVars <- spadesRasterVars %>% 
    left_join(allRasters, by = c("RastersID" = "Name")) %>% 
    rename(var = RastersID) %>% 
    mutate(type = "raster")
}

spadesFileVars <- datasheet(mySce, "ROFSim_SpaDESRuntimeFiles", 
                            lookupsAsFactors = FALSE)
if(nrow(spadesFileVars)>0){
  spadesFileVars <- spadesFileVars %>% 
    left_join(allPolygons, by = c("PolygonsID" = "Name")) %>% 
    rename(var = FileVariableID) %>% 
    mutate(type = "file")
}

allVars <- rbind(spadesRasterVars, spadesFileVars)

# Verify all required vars have a matching sim object
if(sum(is.na(allVars$SpaDESSimObject))>0){
  stop("Crosswalk between variable and spades object not provided.")
}

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

# Get empty datasheets
rasterFiles <- datasheet(mySce, "RasterFile", lookupsAsFactors = FALSE, 
                         empty = TRUE, optional = TRUE)
extFiles <- datasheet(mySce, "ExternalFile", lookupsAsFactors = FALSE,
                      empty = TRUE, optional = TRUE)

for (theIter in iterationSet){
  print(theIter)
  #theIter =1
  # Load the spades object 
  spadesObjectPath <- spadesDatasheet %>% 
    filter(Iteration == theIter) %>% 
    pull(Filename)
  
  #sort(sapply(ls(), function(x) {object.size(get(x)) }))
  outputs <- getSpadesOutputs(spadesObjectPath,timestepSet) %>% 
    filter(objectName %in% allVars$SpaDESSimObject)  
  
  for (rowVar in seq_len(length.out = nrow(allVars))){
    print(rowVar)
    #rowVar=1
    theVar <- as.character(allVars$SpaDESSimObject[rowVar])
    theVarName <- as.character(allVars$var[rowVar])
    theVarType <- allVars$type[rowVar]
    
    # Re-filter based on specific var this time
    outputsFiltered <- outputs %>% 
      filter(objectName == theVar)
    
    for (rowOut in seq_len(length.out = nrow(outputsFiltered))){
      #rowOut=1
      theTs <- outputsFiltered$Timestep[rowOut]
      
      if (theVarType == "raster") {
        
        tmpObj <- raster(outputsFiltered$file[rowOut])
        
        # Make file name
        filePath <- file.path(tmp, paste0(theVar, "_", paste(paste0("it_",theIter), 
                                                             paste0("ts_",theTs), sep = "_"), 
                                          ".tif"))
        
        # Write tmp file
        writeRaster(tmpObj, filePath, overwrite = TRUE)
        
        # Populate sheet
        rasterFiles <- addRow(rasterFiles, list(Iteration = theIter,
                                                Timestep = theTs,
                                                RastersID = theVarName, 
                                                Filename = filePath,
                                                TransformerID = transformerName))
        
      } else if (theVarType == "file") {
        
        tmpObj <- st_read(outputsFiltered[rowOut])
        
        # TODO fix this case, how to get the file here?
        filePath <- file.path(tmp, paste0(theVar, "_", paste(paste0("it_",theIter), 
                                                             paste0("ts_",theTs), sep = "_"), 
                                          ".ext"))
        
        # Write tmp file
        writeRaster(tmpObj, filePath)
        
        # Populate sheet
        extFiles <- addRow(extFiles, list(Iteration = theIter,
                                          Timestep = theTs,
                                          PolygonsID = theVarName, 
                                          Filename = filePath,
                                          TransformerID = transformerName))
        
      }
    }
  }
  
}

saveIfNotEmpty <- function(sheet, name){
  if(nrow(sheet) != 0){
    saveDatasheet(mySce, sheet, name, append = TRUE)
  }
}

mapply(list(rasterFiles, extFiles), 
       FUN = saveIfNotEmpty, 
       name = list("RasterFile", "ExternalFile"))

# -------------------------------------------------------------------------
