library(rsyncrosim)
library(ggplot2)
library(raster)
theme_set(theme_bw())

# Use the rsyncrosim package to access model results from the SyncroSim UI and
# do further analyses in R

# Set paths to library folder, library name, and scenario name
libDir <- "C:/Users/endicotts/Documents/gitprojects/ROFSyncSim/"
libName <- "ROFDemoS1"
scnName <- "Caribou - anthro"

# load library
cLib <- ssimLibrary(paste0(libDir, "/", libName))

# Get list of all results scenarios
allRes <- scenario(cLib, results = TRUE)

# get results scnID for the most recent result
exampleScn <- max(subset(allRes, grepl(paste0(scnName, " \\("), allRes$name))$scenarioId)

# Load result tables
rScn <- scenario(cLib, exampleScn)
distMetrics <- datasheet(rScn, "OutputDisturbanceMetrics")
popMetrics <- datasheet(rScn, "OutputPopulationMetrics")

# See changes in disturbance metrics over time
distMetricsPlot <- ggplot(data = distMetrics, aes(x = Timestep, y = Amount)) +
  geom_line(size = 0.5) +
  facet_wrap(~MetricTypeDistID, scales = "free", ncol = 1) +
  xlab("Time") +
  ylab("Response") +
  theme(legend.position = "none")
distMetricsPlot

# change the N metric to log10 N
popMetrics$MetricTypeDemogID <- as.character(popMetrics$MetricTypeDemogID)
popMetrics$Amount[popMetrics$MetricTypeDemogID == "N"] <- log10(popMetrics$Amount[popMetrics$MetricTypeDemogID == "N"] + 0.001)
popMetrics$MetricTypeDemogID[popMetrics$MetricTypeDemogID == "N"] <- "log10N"

# See changes in demographic metrics over time
popMetricsPlot <- ggplot(data = popMetrics,
                         aes(x = Timestep, y = Amount, group = as.factor(Replicate),
                             colour = as.factor(Replicate))) +
  geom_line(size = 0.5) +
  facet_wrap(~MetricTypeDemogID , scales = "free") +
  xlab("Time") +
  ylab("Response") +
  theme(legend.position = "none")
popMetricsPlot

# load raster results
myHabitatRasters <- datasheetRaster(rScn, "OutputSpatialHabitat")

# look at names to choose rasters to compare
names(myHabitatRasters)

fall2020 <- myHabitatRasters[[1]]
fall2060 <- myHabitatRasters[[5]]

# subtract to see the change in relative probability of selection
changeFall2060 <- fall2060 - fall2020

# beige areas show no change, yellow to red areas show lower probabilities of selection
plot(changeFall2060, col = hcl.colors(10, "Heat 2"))

