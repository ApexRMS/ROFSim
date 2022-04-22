getSpadesOutputs<-function(spadesObjectPath,timestepSet){
  dn <- dirname(spadesObjectPath)
  #if outputs have already been extracted, use that and don't bother trying to find and open large .qs object
  outputsPath = paste0(dn,"/outputs.csv")
  if(file.exists(outputsPath)){
    outputsRaw <- read.csv(outputsPath)
  }else{
    spadesObject <- qs::qread(spadesObjectPath)
    outputsRaw <- outputs(spadesObject)
    write.csv(outputsRaw,outputsPath)
    rm(spadesObject) 
  }
  #Filter outputs
  outputs<- outputsRaw %>% 
    make_paths_relative("outputs") %>% 
    filter(saveTime %in% timestepSet) %>% 
    rename(Timestep = saveTime)
  
    outputs$file <-paste0(dirname(spadesObjectPath),"/",basename(outputs$file))
    outputs <- outputs %>% 
      bind_rows(data.frame(objectName = "standAgeMap", 
                           Timestep = timestepSet, 
                           file = file.path(dirname(spadesObjectPath), 
                                            paste0("standAgeMap_", timestepSet, ".tif"))))
    return(outputs)
}



###############################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Description:Function combines information on leading species from landR output
#             with stand density information from landcover to get leading species and density
#             The function has two main steps
#             1) Defining vegetation type; 2) defining the level of openness;
#             and 3) combining the information into cover classes for forested cells.
#
# Required inputs: 
#                 - The cohort data accessed from a sim object 
#                     (`sim$cohortData`)
#                 - The pixel group map linked to that cohort data again
#                     accessed through the sim object (`sim$pixelGroupMap`)
#                 - The initial land cover map (`sim$rstLCC`)
#                 - A lookup table defining the land cover type as conifer,
#                     deciduous, or mixed and as dense, sparse, or open and the
#                     associated land cover ID value
# Outputs: 
#         - An updated raster of forested areas.
#
# Authors: Josie Hughes & C.E. Simpkins (based on code by Tati Micheletti)
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###############################################################################
getLCCFromCohortData <- function(cohortData,
                                  pixelGroupMap,
                                  rstLCC,
                                  lccClassTable,lccSparsenessTable, e){
  #cohortData=cohort_data
  library(data.table)
  library(raster)
  
  ### Step 1: Define vegetation type ###
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # A) Assign type for each species based on equivalence table (using LandR)
  if(requireNamespace("LandR", quietly = TRUE)){
    sppEquivalencies <- LandR::sppEquivalencies_CA
  } else {
    # NOTE: this will not get updates added to LandR package but avoids dep on
    # github only package
    sppEquivalencies <- read.csv(file.path(e$PackageDirectory, "LandR_sppEquivalencies_CA.csv"))
    sppEquivalencies <- as.data.table(sppEquivalencies)
  }
  
  for (x in 1:length(unique(cohortData$speciesCode))) {
    cohortData[speciesCode == unique(cohortData$speciesCode)[x],
               Type := sppEquivalencies[LandR == unique(cohortData$speciesCode)[x]]$Type]
  }
  
  cohortData[Type == "Deciduous"]$Type <- "deciduous"
  cohortData[Type == "Conifer"]$Type <- "conifer"
  
  # B) Calculate species cover based on percent biomass
  cohortData[, coverIndex := B]
  cohortData[, totalCoverIndex := sum(coverIndex), by = c("pixelGroup")]
  cohortData[, treeTypeCoverIndex := sum(coverIndex), by = c("pixelGroup", "Type")]
  cohortData[, percTree := treeTypeCoverIndex/totalCoverIndex, 
             by = c("pixelGroup", "Type") ]
  
  # B.1) Simplify and dcast cohortData to be able to compare the percentages
  cohortDataSim <- unique(cohortData[, c("pixelGroup", "Type", "percTree")])
  cohortDataD <- dcast(data = cohortDataSim, formula = pixelGroup ~ Type, 
                       fill = 0)
  
  # C) Mark pure and mixed stands based on a 75% threshold
  cohortDataD[, Deciduous := fifelse(deciduous >= 0.75, 1, 0)]
  cohortDataD[, Conifer := fifelse(conifer >= 0.75, 1, 0)]
  cohortDataD[, standLeading := colnames(.SD)[max.col(.SD, ties.method="first")], 
              .SDcols = c("Deciduous", "Conifer")]
  cohortDataD[, standLeading := fifelse(Deciduous+Conifer == 0, "Mixed", standLeading)]
  
  # D) Simplifying
  cohortDataSim <- unique(cohortDataD[, c("pixelGroup", "standLeading")])

  #get  leading species map 
  finalDT  <- cohortDataSim
  finalDT <- merge(finalDT, data.table(leadingClass = c(1,2,3), 
                                       standLeading = c("Conifer", 
                                                      "Deciduous", 
                                                      "Mixed")),
                   by = "standLeading", all.x = TRUE)
  
  leadingClass <- SpaDES.tools::rasterizeReduced(reduced = finalDT, 
                                                    fullRaster = pixelGroupMap, 
                                                    newRasterCols = "leadingClass", 
                                                    mapcode = "pixelGroup")

  ### Step 2: Define level of openness ###
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # A) Take original LCC classes and divide them into "dense" or "sparse"
  # Note we this is currently only used to id dense classes that are eligible for transition.
  # Assuming all other landcover classes remain static.
  sparsenessMap <- rstLCC
  
  sparsenessMap[!sparsenessMap[] %in% lccSparsenessTable[["LCCclass"]]] <- NA
  
  sparse <- lccSparsenessTable[["LCCclass"]][grep(pattern = "sparse", x = lccSparsenessTable[["sparseness"]])]
  open <- lccSparsenessTable[["LCCclass"]][grep(pattern = "open", x = lccSparsenessTable[["sparseness"]])]
  dense <- lccSparsenessTable[["LCCclass"]][grep(pattern = "Treed", x = lccSparsenessTable[["sparseness"]])]
  
  # dense = 1; open = 2; sparse =  3
  sparsenessMap[sparsenessMap[] %in% dense] <- -1
  sparsenessMap[sparsenessMap[] %in% open] <- -2
  sparsenessMap[sparsenessMap[] %in% sparse] <- -3
  
  sparsenessMap <- -sparsenessMap
  
  sparsenessMap <- ratify(sparsenessMap)
  rat <- raster::levels(sparsenessMap)[[1]]
  rat$sparseness[rat$ID==1]="Treed"
  rat$sparseness[rat$ID==2]="open"
  rat$sparseness[rat$ID==3]="sparse"
  levels(sparsenessMap) <- rat
  names(sparsenessMap) <- "sparsenessMap"
  sparsenessMapDT <- raster::unique(na.omit(data.table::data.table(
    getValues(stack(sparsenessMap, pixelGroupMap)))))

  finalDT <- merge(cohortDataSim, sparsenessMapDT, all.x = TRUE)
  
  finalDT <- merge(finalDT, data.table(sparsenessMap = c(1,2,3), 
                                       sparseness = c("Treed", 
                                                      "open", 
                                                      "sparse")),
                   by = "sparsenessMap", all.x = TRUE)
  finalDT=subset(finalDT,!is.na(sparseness))
  finalDT[, standLeading  := paste(standLeading, sparseness, sep = " ")]
  finalDT = merge(finalDT,lccClassTable,by="standLeading")  
  
  #Pixel Groups assigned to more than one sparseness class?
  #subset(as.data.frame(table(finalDT$pixelGroup)),Freq>1)

  # Get the new classes to the LCC where they are supposed to be
  newLCCClass <- SpaDES.tools::rasterizeReduced(reduced = finalDT, 
                                                fullRaster = pixelGroupMap, 
                                                newRasterCols = "LCCclass", 
                                                mapcode = "pixelGroup")

  DT <- data.table(pixelID = 1:ncell(newLCCClass),
                   getValues(stack(rstLCC, newLCCClass)))
  names(DT) <- c("pixelID", "LCC", "newLCC")
  DT[, updatedLCC := fifelse(!is.na(newLCC), newLCC, LCC)]
  updatedLCCras <- raster::setValues(x = raster(rstLCC), 
                                     values = DT[["updatedLCC"]])
  updatedLCCras <- floor(updatedLCCras)

  return(stack(updatedLCCras,leadingClass))
}
