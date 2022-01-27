###############################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Description:Function takes the output from landR, primarily the pixel group
#             map and the cohort biomass, and converts them into the 5 dynamic
#             land cover types used as covariates in the Ring of Fire RSPF,
#             note that there are 11 covariates, but the majority are not 
#             simulated by LandR. The function is made up of three broad steps
#             1) Defining vegetation type; 2) defining the level of openness;
#             and 3) updating natural disturbance cells based of burns created
#             by firesense.
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
#         - An updated raster of the study area with all of the covariate 
#           land cover types represented (both dynamic and static), saved out
#           as a .tif (geotif) file.
#
# Author: C.E. Simpkins (based on code by Tati Micheletti)
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###############################################################################

makeLCCfromCohortData <- function(cohortData,
                                  pixelGroupMap,
                                  rstLCC,
                                  lccClassTable,lccSparsenessTable){
  #cohortData=cohort_data
  library(LandR)
  library(data.table)
  library(raster)
  
  ### Step 1: Define vegetation type ###
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # A) Assign type for each species based on equivalence table (using LandR)
  sppEquivalencies <- LandR::sppEquivalencies_CA
  
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
  cohortDataD[, pureDec := fifelse(deciduous >= 0.75, 1, 0)]
  cohortDataD[, pureCon := fifelse(conifer >= 0.75, 1, 0)]
  cohortDataD[, standLeading := colnames(.SD)[max.col(.SD, ties.method="first")], 
              .SDcols = c("pureDec", "pureCon")]
  cohortDataD[, standLeading := fifelse(pureDec+pureCon == 0, "mixed", standLeading)]
  
  # D) Simplifying
  cohortDataSim <- unique(cohortDataD[, c("pixelGroup", "standLeading")])
  

  ### Step 2: Define level of openness ###
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # A) Take original LCC classes and divide them into "dense" or "sparse"
  sparsenessMap <- rstLCC
  
  sparsenessMap[!sparsenessMap[] %in% lccSparsenessTable[["LCCclass"]]] <- NA
  
  sparse <- lccSparsenessTable[["LCCclass"]][grep(pattern = "sparse", x = lccSparsenessTable[["sparseness"]])]
  open <- lccSparsenessTable[["LCCclass"]][grep(pattern = "open", x = lccSparsenessTable[["sparseness"]])]
  dense <- lccSparsenessTable[["LCCclass"]][grep(pattern = "dense", x = lccSparsenessTable[["sparseness"]])]
  
  # dense = 1; open = 2; sparse =  3
  sparsenessMap[sparsenessMap[] %in% dense] <- -1
  sparsenessMap[sparsenessMap[] %in% open] <- -2
  sparsenessMap[sparsenessMap[] %in% sparse] <- -3
  
  sparsenessMap <- -sparsenessMap
  
  sparsenessMap <- ratify(sparsenessMap)
  rat <- raster::levels(sparsenessMap)[[1]]
  rat$sparseness[rat$ID==1]="dense"
  rat$sparseness[rat$ID==2]="open"
  rat$sparseness[rat$ID==3]="sparse"
  levels(sparsenessMap) <- rat
  names(sparsenessMap) <- "sparsenessMap"
  
  sparsenessMapDT <- raster::unique(na.omit(data.table::data.table(
    getValues(stack(sparsenessMap, pixelGroupMap)))))

  finalDT  <- merge(cohortDataSim, sparsenessMapDT, all.x = TRUE)
  
  finalDT <- merge(finalDT, data.table(sparsenessMap = c(1,2,3), 
                                       sparseness = c("dense", 
                                                      "open", 
                                                      "sparse")),
                   by = "sparsenessMap", all.x = TRUE)
  finalDT=subset(finalDT,!is.na(sparseness))
  finalDT[, standLeading  := paste(standLeading, sparseness, sep = "_")]
  finalDT = merge(finalDT,lccClassTable,by="standLeading")  
  
  #Pixel Groups assigned to more than one sparseness class. why?
  #subset(as.data.frame(table(finalDT$pixelGroup)),Freq>1)
  #subset(finalDT,pixelGroup==2110)

  # Get the new classes to the LCC where they are supposed to be
  newLCCClass <- SpaDES.tools::rasterizeReduced(reduced = finalDT, 
                                                fullRaster = pixelGroupMap, 
                                                newRasterCols = "LCCclass", 
                                                mapcode = "pixelGroup")

  DT <- data.table(pixelID = 1:ncell(newLCCClass),
                   getValues(stack(rstLCC, newLCCClass)))
  names(DT) <- c("pixelID", "LCC", "newLCC")
  DT[, updatedLCC := fifelse(!is.na(newLCC), newLCC, newLCC)]
  updatedLCCras <- raster::setValues(x = raster(rstLCC), 
                                     values = DT[["updatedLCC"]])
  updatedLCCras <- floor(updatedLCCras)

  return(updatedLCCras)
}
