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
#selectedIso3 = c("CHN", "USA", "IND", "RUS", "JPN", "DEU", "BRA", "KOR", "AUT", "FRA", "ESP", "DNK")
selectedIso3 = "DEU"
nObservations = 12
# ----------------------------------------------

# ----------------------------------------------
# LOAD, PREPARE DATA
# ----------------------------------------------
fileName = paste0("rasterAnomalies", "_m", macroResolution, "_r", rasterResolution, "_", operation, ".rData")
dataPath = file.path(folders$ocoData, "daily")
baseData = loadData(file.path(dataPath, fileName))

baseData = baseData[!is.na(anomaly)]

baseData = merge(baseData, makeGrid(rasterResolution), all.x = TRUE, by = c("rasterLats", "rasterLons"))
baseData = baseData[order(date)]
# ----------------------------------------------

# ----------------------------------------------
# CREATE CUMULATIVE MEAN OF CELLS
# ----------------------------------------------
baseData[, tmp := anomaly-shift(anomaly, n = 1L, type="lag", fill = 0), by=c("id", "rasterLats", "rasterLons")]

baseData[anomaly != tmp, skip := 1:.N, by=c("id")]
baseData[, skip := na.locf(skip, na.rm=FALSE), by=c("id")]
baseData[is.na(skip), skip := 0]

baseData[, cummean := cumsum(tmp)/(seq_along(tmp)-skip), by=c("id")][, `:=` (skip = NULL)]
baseData[, nDaily := 1:.N, by=c("id", "date")]
baseData[, NDaily := .N, by=c("id", "date")]

#SELECT LAST CUMULATIVE MEAN EVERY DAY AS DAY VALUE
baseData = baseData[nDaily == NDaily]
# ----------------------------------------------

# ----------------------------------------------
# FINALIZE DATA
# ----------------------------------------------
baseData = baseData[iso3 %in% selectedIso3]
baseData[!is.na(anomaly), n := .N, by=id]

#SELECT REGIONS WITH MORE THAN nObservations
plotData = baseData[n >= nObservations & !is.na(anomaly)]
plotData$rasterLats = sprintf("%.1f", plotData$rasterLats)
plotData$rasterLons = sprintf("%.1f", plotData$rasterLons)
plotData$id <- paste0("ID: ", plotData$id, " (", plotData$idLats, ", ", plotData$idLons, ")")[match(plotData$id, plotData$id)]

#MAKE CUMULATIVE MINIMA, QUARTERLY
plotData$quarter = quarter(plotData$date)
plotData$year = year(plotData$date)
plotData[as.Date(date, format= "%y-%m-%d") == max(as.Date(date, format= "%y-%m-%d")), lastDay := 1, by=c("quarter", "id", "year")]
plotData = setDT(plotData)[ ,.SD[which.max(as.Date(date, format= "%y-%m-%d"))],  by = .(quarter, id, year)]
plotData[!is.na(cummean), cummin := cummin(cummean), by = "id"]
plotData[cummean != cummin, cummin := NA]
# ----------------------------------------------

# ----------------------------------------------
# PLOT AND SAVE
# ----------------------------------------------
ggplot(data = plotData, aes(x=date, y=cummean, group=id))+
    geom_line(stat = "identity")+
    geom_point(data = plotData, aes(x=date, y=cummin, group=id))+
    facet_wrap(~ id, ncol = 4) +
    geom_hline(yintercept=0, linetype="dashed", color = "red")+
    ylab(paste0("Mean CO2 concentration anomalies compared to surrounding area (ppm), center in parenthesis")) +
    xlab("")+
    theme_bw()+
    theme(legend.title = element_blank())

file = file.path(folders$tmp, "plots", paste0("anomalyPlot_", selectedIso3, "_", operation, ".png"))
ggsave(file, width = 10, height = 8, units="in")
# ----------------------------------------------
print("Jobs done.")

