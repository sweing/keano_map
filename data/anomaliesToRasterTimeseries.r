# ----------------------------------------------
# BASE
# ----------------------------------------------
rm(list=ls())
source("./trunk/base/init.r", chdir=TRUE)
# ----------------------------------------------


# ----------------------------------------------
# CONFIG
# ----------------------------------------------
macroResolution = 2
rasterResolution = 0.2
operation = "median"
selectedIso3 = "AUT"
nObservations = 7
# ----------------------------------------------

# ----------------------------------------------
# PREPARE
# ----------------------------------------------

fileName = paste0("rasterAnomalies", "_m", macroResolution, "_r", rasterResolution, "_", operation, ".rData")
dataPath = file.path(folders$ocoData, "daily")
baseData = loadData(file.path(dataPath, fileName))

#baseData[, id := .GRP, by = .(rasterLats, rasterLons)]

#baseData$rasterLats = as.double(baseData$rasterLats)
#baseData$rasterLons = as.double(baseData$rasterLons)
baseData = merge(baseData, makeGrid(rasterResolution), all.x = TRUE, by = c("rasterLats", "rasterLons"))

baseData = baseData[iso3 %in% selectedIso3]
baseData[!is.na(anomaly), n := .N, by=id]
plotData = baseData[n >= nObservations & !is.na(anomaly)]
plotData$rasterLats = sprintf("%.1f", plotData$rasterLats)
plotData$rasterLons = sprintf("%.1f", plotData$rasterLons)
plotData$id <- paste0("ID: ", plotData$id, " (", plotData$rasterLats, ", ", plotData$rasterLons, ")")[match(plotData$id, plotData$id)]




# ----------------------------------------------

# ----------------------------------------------
# PLOT & SAVE
# ----------------------------------------------
ggplot(data = plotData, aes(x=date, y=anomaly, group=id))+
    geom_line(stat = "identity")+
    facet_wrap(~ id, ncol = 4) +
    geom_hline(yintercept=0, linetype="dashed", color = "red")+
    ylab(paste0("Mean CO2 concentration anomalies compared to surrounding area (ppm)")) +
    xlab("")+
    theme_bw()+
    theme(legend.title = element_blank())

file = file.path(folders$tmp, "plots", paste0("RasterAnomalyPlot_", operation, "_", selectedIso3, ".png"))
ggsave(file, width = 10, height = 8, units="in")
# ----------------------------------------------