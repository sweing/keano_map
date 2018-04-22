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
#countries = countryGroups$EU28
#countries = c("CHN", "USA", "IND", "RUS", "JPN", "DEU", "BRA", "KOR", "AUT", "FRA", "ESP", "DNK")
selectedIso3 = "DEU"
nObservations = 12
# ----------------------------------------------


fileName = paste0("rasterAnomalies", "_m", macroResolution, "_r", rasterResolution, "_", operation, ".rData")
dataPath = file.path(folders$ocoData, "daily")
baseData = loadData(file.path(dataPath, fileName))

baseData = baseData[!is.na(anomaly)]

baseData = merge(baseData, makeGrid(rasterResolution), all.x = TRUE, by = c("rasterLats", "rasterLons"))
baseData = baseData[order(date)]
baseData[, tmp := anomaly-shift(anomaly, n = 1L, type="lag", fill = 0), by=c("id", "rasterLats", "rasterLons")]

baseData[anomaly != tmp, skip := 1:.N, by=c("id")]
baseData[, skip := na.locf(skip, na.rm=FALSE), by=c("id")]
baseData[is.na(skip), skip := 0]

#baseData[tmp == 0, count := 1:.N, by=c("id")]
#baseData[, count := na.locf(count, na.rm=FALSE), by=c("id")]

baseData[, cummean := cumsum(tmp)/(seq_along(tmp)-skip), by=c("id")][, `:=` (skip = NULL)]
#baseData[, cummean := cumsum(tmp)/count, by=c("id")]
#baseData[, cummean := cumsum(tmp), by=c("id")]

baseData[, nDaily := 1:.N, by=c("id", "date")]
baseData[, NDaily := .N, by=c("id", "date")]

#Select last cumulative mean every day as the day value
baseData = baseData[nDaily == NDaily]

baseData = baseData[iso3 %in% selectedIso3]
baseData[!is.na(anomaly), n := .N, by=id]
plotData = baseData[n >= nObservations & !is.na(anomaly)]
plotData$rasterLats = sprintf("%.1f", plotData$rasterLats)
plotData$rasterLons = sprintf("%.1f", plotData$rasterLons)
plotData$id <- paste0("ID: ", plotData$id, " (", plotData$idLats, ", ", plotData$idLons, ")")[match(plotData$id, plotData$id)]

plotData[, cummin := cummin(cummean), by = "id"]

plotData[cummean != cummin, cummin := NA]
         
ggplot(data = plotData, aes(x=date, y=cummean, group=id))+
    geom_line(stat = "identity")+
    geom_point(data = plotData, aes(x=date, y=cummin, group=id))+
    facet_wrap(~ id, ncol = 4) +
    geom_hline(yintercept=0, linetype="dashed", color = "red")+
    ylab(paste0("Mean CO2 concentration anomalies compared to surrounding area (ppm)")) +
    xlab("")+
    theme_bw()+
    theme(legend.title = element_blank())

file = file.path(folders$tmp, "plots", paste0("anomalyPlot_", selectedIso3, "_", operation, ".png"))
ggsave(file, width = 10, height = 8, units="in")
