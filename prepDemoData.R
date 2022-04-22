# prepare data from raw files. This script is copied from the MissisaBooPaper
# repo and object names have not been changed but in and out paths have. The
# Missisa data covers a smaller area and has no harvest so different data paths are
# used for that

library(raster)
library(sf)
library(dplyr)
library(fasterize)
library(osfr)

#location for source data for UI
sourceData <- "C:/Users/endicotts/Documents/gitprojects/ROFSyncSim/ROFDemo_data"

# location for raw input data
inData <- "C:/Users/endicotts/Documents/gitprojects/MissisaBooPaper/data/inputNV/"

# location of merged plc with tiles 15-17
plc17Pth <- "C:/Users/endicotts/Documents/gitprojects/ChurchillAnalysis/inputNV/z17-27class.tif"

harvPth <- "C:/Users/endicotts/Documents/gitprojects/ChurchillAnalysis/inputNV/AR_Master_2018.gdb"

# output resolution
outRes <- 125

# # Download the zip file of raw files from OSF for Missisa paper
# osf_proj <- osf_retrieve_node("https://osf.io/r9mkp/")
# 
# osf_ls_files(osf_proj) %>% filter(name == "inputNV.zip") %>% 
#   osf_download(path = inData)
# 
# # extract it to the data folder of the current project
# unzip("data/inputNV.zip", exdir = inData)

outMiss <- file.path(sourceData)

if(!dir.exists(outMiss)){
  dir.create(outMiss)
}

caribouRanges <- st_read(file.path(inData, "caribouRanges/Caribou_Range_Boundary.shp"), 
                         quiet = TRUE)

# Provincial landcover tile that covers Missisa range
plc16 <- terra::rast(file.path(inData, "Provincial-Landcover-2000/z16-27class.tif"))

plc17 <- terra::rast(plc17Pth)

plc17Pth2 <- paste0(outMiss, "plc17_temp.tif")
plc17 <- terra::project(plc17, terra::crs(plc16), method = "near", 
                        filename = plc17Pth2)
rm(plc16, plc17)

plc16vrt <- terra::vrt(c(plc17Pth2, 
                      file.path(inData, "Provincial-Landcover-2000/z16-27class.tif")),
                    paste0(tempfile(), ".vrt"))


# buffer project area to 6km because that is larger than the largest window
# radius times 3 to account for offsets

miss <- caribouRanges %>%
  filter(RANGE_NAME %in% c("Missisa", "James Bay", "Pagwachuan", "Nipigon")) %>%
  st_transform(st_crs(plc16vrt)) %>%
  summarise(OGF_ID = first(OGF_ID),
            geometry = st_union(geometry) %>% st_buffer(6000 *3))

# plc #=========================================================================
plcMiss <- terra::crop(plc16vrt, miss, snap = "out")

spds <- terra::rast(file.path(sourceData, "SpaDESOutputs/LCC.tif"))
# Use the crs from the SpaDES data
crsUse <- st_crs(spds)
crsUseR <- terra::crs(spds)

plcMiss <- terra::project(plcMiss, spds, method = "near", align = TRUE,
                          filename = file.path(outMiss, 
                                               paste0("plc", outRes, ".tif")),
                          overwrite = TRUE, datatype = "INT1U")

tmpltRastMiss <- plcMiss

terra::compareGeom(spds, plcMiss, ext= FALSE, rowcol = F, res = T)

miss <- st_transform(miss, crsUse)
# plcMiss250 <- raster::aggregate(plcMiss, fact = 10, fun = modal,
#                                 filename = paste0(outMiss, "plc250.tif"),
#                                 overwrite = TRUE, datatype = "INT1U")
# 
# tmpltRastMiss <- raster::raster(plcMiss250)

# eskers #======================================================================
# eskers from Quaternary Geology
qg_lines <- read_sf("data/inputNV/Eskers/GIS_DATA/Quaternary/lines_ll.shp")

eskersQG <- qg_lines %>% filter(stringr::str_detect(FEATURE, "esker")) %>% 
  st_transform(crsUse)

eskerMiss <- st_filter(eskersQG, miss)

st_write(eskerMiss, paste0(outMiss, "esker.shp"), append = FALSE)
# road #========================================================================

# Need 2010 and 2020 versions use year constructed in MNRF data
road_ORN <- st_read("data/inputNV/ORNSEGAD_20201028/Non_Sensitive.gdb")

road_MNRF <- st_read("data/inputNV/MNRRDSEG_20201028/Non_Sensitive.gdb",
                     layer = "MNRF_ROAD_SEGMENT")

# Missisa
road_ORNMiss <- st_filter(road_ORN, miss %>% st_transform(st_crs(road_ORN))) %>%
  st_transform(crsUse)

road_MNRFMiss <- st_filter(road_MNRF, miss %>% st_transform(st_crs(road_MNRF))) %>%
  st_transform(crsUse)


# MNRF is more detailed and appears more accurate when compared to satellite but
# is missing some roads in settlements. Need 100m buffer because sometimes they
# show the same road in slightly different locations
road_MNRFMissBuf <- st_buffer(road_MNRFMiss, 100) %>% st_union()

# remove this road from ORN b/c it is counted in MNRF but route is quite different
road_ORNMiss <- road_ORNMiss %>% 
  filter(ABBREVIATED_STREET_NAME != "MARTEN FALLS WINTER RD" | 
           is.na(ABBREVIATED_STREET_NAME))

road_ORNMissMinusMNRF <- st_difference(road_ORNMiss, road_MNRFMissBuf)

road_ORNMNRFMiss <- bind_rows(road_ORNMissMinusMNRF, road_MNRFMiss) %>%
  select(contains("YEAR_CONSTRUCTED"))

road_ORNMNRFMiss2010 <- road_ORNMNRFMiss %>%
  filter(YEAR_CONSTRUCTED <= 2010 | YEAR_CONSTRUCTED_MODIFIER == "Before" | is.na(YEAR_CONSTRUCTED)) %>% 
  select(YEAR_CONSTRUCTED)

st_write(road_ORNMNRFMiss %>% st_union(), paste0(outMiss, "road_ORNMNRFMiss2020.shp"),
         append = FALSE)
st_write(road_ORNMNRFMiss2010 %>% st_union(), paste0(outMiss, "road_ORNMNRFMiss2010.shp"),
         append = FALSE)

# rail #========================================================================
railON <- st_read("data/inputNV/Rail/ORWN_TRACK.shp") %>%
  st_transform(crsUse)

# rail does not have dates associated so assuming no new ones
railMiss <- st_filter(railON, miss)

st_write(railMiss, paste0(outMiss, "rail.shp"),
         append = FALSE)

# Utilities #===================================================================
utilON <- st_read("data/inputNV/Utilities/Utility_Line.shp") %>%
  st_transform(crsUse)

# Utilities use Business effective date for age "Date that the record becomes
# effective in relation to the business i.e. the date MNR became aware of its
# existence."

utilMiss2020 <- st_filter(utilON, miss)

utilMiss2010 <- utilMiss2020 %>% filter(BUSINESS_1 < as.Date("2010-01-01"))

st_write(utilMiss2020, paste0(outMiss, "util2020.shp"),
         append = FALSE)

st_write(utilMiss2010, paste0(outMiss, "util2010.shp"),
         append = FALSE)

# Fire AFFES #================================================================
# Use 1980 as oldest fires considered since the data Rempel used included
# "pre-1990" disturbances which meant still visible in 1990 and documentation
# says expect them to be visible ~ 10 years

fireAFFES <- st_read(file.path(inData, "FIREDSTB/LIO-2020-10-28/FIRE_DISTURBANCE_AREA.shp")) %>%
  st_transform(crsUse)

fireAFFESMiss2020 <- st_filter(fireAFFES, miss) %>%
  filter(between(FIRE_YEAR, 1990, 2020))

fireAFFESMiss2010 <- st_filter(fireAFFES, miss) %>%
  filter(between(FIRE_YEAR, 1980, 2010))

tmpltRastMiss <- as(tmpltRastMiss, "Raster")

fireAFFESMiss2020 <- fasterize::fasterize(fireAFFESMiss2020, tmpltRastMiss,
                                          background = 0)

raster::writeRaster(fireAFFESMiss2020, 
                    file.path(outMiss, paste0("fireAFFES2020_", outRes,".tif")),
                    datatype = "INT1U", overwrite = TRUE)

# ROF development #========================= 

# Operational Cell Claims data was processed in QGIS and the resulting shapefile
# is provided. Processing involved selecting mining claim areas in the ring of
# fire by hand. For simplicity, only mining claims associated within the ring of
# fire (crescent) and associated large area to the south within the database
# were used in modelling. Identifying the proportion of claims that are/will be
# mined and the extent of mining within a claim was outside the scope of this
# work.
mines_sf <- read_sf(file.path(inData, "ROFDevelopment/mine_area.shp"))

mines_sf <- mines_sf %>% st_transform(crsUse)

mines_ras <- fasterize::fasterize(mines_sf, tmpltRastMiss, background = 0)

raster::writeRaster(mines_ras, file.path(outMiss, paste0("mines_ras", outRes,".tif")), overwrite = TRUE,
                    datatype = "INT1U")

# proposed roads

# The proposed roads were also extracted by hand in QGIS from the Operational
# Alienation and Claim Cells data. Operational_Alienation polygons were filtered
# to include only rows with the description "Application for SRO, Public Lands
# Act, see Section 28(3) and 30(b) Mining Act." Duplicate roads were removed by
# hand, keeping the one with the more recent entry time. An additional section
# of road was added based on the Operational Cell claims where there is a linear
# section between the Webequie winter road and the ROF claims. This produced
# polygons of the road area but lines were needed to calculate density.

# Steps to convert polygons to lines
# 1. GDAL Calc: 1*(A>0), Output NoData=0; Raster Type=Byte **Important for GRASS
# 2. r.thin
# 3. r.to.vect
# 4. smooth (tolerance=0.5)

road_prop <- read_sf("data/inputNV/ROFDevelopment/proposed_road_line.shp")

road_propMiss <- road_prop %>% 
  st_transform(crsUse) %>% 
  st_filter(miss)

# combine proposed roads with existing. Same problem as above where some roads
# are in both so use 100m buffer to remove roads that are already present
road_ORNMNRFMissBuf <- st_buffer(road_ORNMNRFMiss, 100) %>% st_union()

road_propMissMinus2020 <- st_difference(road_propMiss, road_ORNMNRFMissBuf)

road_ORNMNRFPlusPropMiss <- rbind(road_propMissMinus2020 %>% select(YEAR_CO),
                                  road_ORNMNRFMiss) %>% 
  st_union() %>% st_sf()

st_write(road_ORNMNRFPlusPropMiss, paste0(outMiss, "road_ROFDevelopment.shp"))

road_dev <- read_sf(paste0(outMiss, "road_ROFDevelopment.shp"))


# MNRF Harvest data from annual report #========================================
anReport <- st_layers(harvPth)

# CC clearcut, SE selection, SH shelterwood for 2002 to present ie 2016
Harvest_CC02 <- st_read(harvPth, 
                        layer = "Harvest_CC02")
Harvest_SE02 <- st_read(harvPth, 
                        layer = "Harvest_SE02")
Harvest_SH02 <- st_read(harvPth, 
                        layer = "Harvest_SH02")

# CC clearcut, SE selection, SH shelterwood for 2017 and 2018
Harvest_CC17 <- st_read(harvPth, 
                        layer = "Harvest_CC17")
Harvest_SE17 <- st_read(harvPth, 
                        layer = "Harvest_SE17")
Harvest_SH17 <- st_read(harvPth, 
                        layer = "Harvest_SH17")

# Estimated harvest from 1990 to 2003
Harvest_Est_1990_03 <- st_read(harvPth, 
                               layer = "Harvest_Est_1990_03")

# contains some "MULTISURFACE" geometry types which cause problems
# Harvest_CC17 %>% filter(st_geometry_type(Shape) == "MULTISURFACE")
tmpfile <-  tempfile(fileext = ".shp")
st_write(Harvest_CC17, tmpfile)
Harvest_CC17 <- st_read(tmpfile) %>% 
  rename(Shape = geometry)

harvestAllMNRF <- rbind(Harvest_CC02 %>% select(AR_YEAR),
                        Harvest_SE02 %>% select(AR_YEAR),
                        Harvest_SH02 %>% select(AR_YEAR), 
                        Harvest_CC17 %>% select(AR_YEAR),
                        Harvest_SE17 %>% select(AR_YEAR),
                        Harvest_SH17 %>% select(AR_YEAR), 
                        Harvest_Est_1990_03 %>% transmute(AR_YEAR = YRDEP))

harvMNRFROF <- st_transform(harvestAllMNRF, crsUse) %>% st_filter(miss)

harvMNRFROF2018 <- fasterize::fasterize(harvMNRFROF %>% st_cast(),
                                        tmpltRastMiss, 
                                        background = 0)

raster::writeRaster(harvMNRFROF2018, 
                    file.path(outMiss, paste0("harvMNRF2018_", outRes,".tif")),
                    overwrite = TRUE, datatype = "INT1U")
