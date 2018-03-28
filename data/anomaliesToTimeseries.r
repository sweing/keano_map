# ----------------------------------------------
# BASE
# ----------------------------------------------
rm(list=ls())
source("./sandbox/daytona/trunk/base/init.r", chdir=TRUE)
# ----------------------------------------------


# ----------------------------------------------
# CONFIG
# ----------------------------------------------
macroResolution = 2
rasterResolution = 0.2
operation = "median"
#countries = countryGroups$EU28
countries = c("CHN", "USA", "IND", "RUS", "JPN", "DEU", "BRA", "KOR", "AUT", "FRA", "ESP", "DNK")
# ----------------------------------------------


fileName = paste0("rasterAnomalies", "_m", macroResolution, "_r", rasterResolution, "_", operation, ".rData")
dataPath = "../daytona/data/oco2/v8rData/daily"
baseData = loadData(file.path(dataPath, fileName))


#baseData[, tmp := anomaly-anomaly[1], by=c("iso3", "rasterLats", "rasterLons")]
baseData[, tmp := anomaly-shift(anomaly, n = 1L, type="lag", fill = 0), by=c("iso3", "rasterLats", "rasterLons")]

baseData[anomaly != tmp, skip := 1:.N, by=c("iso3")]
baseData[, skip := na.locf(skip, na.rm=FALSE), by=c("iso3")]
baseData[is.na(skip), skip := 0]

#baseData[tmp == 0, count := 1:.N, by=c("iso3")]
#baseData[, count := na.locf(count, na.rm=FALSE), by=c("iso3")]

baseData[, cummean := cumsum(tmp)/(seq_along(tmp)-skip), by=c("iso3")][, `:=` (skip = NULL)]
#baseData[, cummean := cumsum(tmp)/count, by=c("iso3")]
#baseData[, cummean := cumsum(tmp), by=c("iso3")]

baseData[, nDaily := 1:.N, by=c("iso3", "date")]
baseData[, NDaily := .N, by=c("iso3", "date")]
baseData = baseData[nDaily == NDaily]

plotData = baseData[iso3 %in% countries & date >= "2014-12-31"]
#plotData[, cummin := cummin(cummean), by=c("iso3")][cummean == cummin, min := 1][, cummin := NULL]
#plotData = plotData[iso3 %in% selected & date > "2014-12-31"]

ggplot(data = plotData, aes(x=date, y=cummean, group=iso3))+
    #geom_smooth(method = "lm")+
    geom_line(stat = "identity")+
    #geom_point(data = plotData[min == 1], aes(date, cummean), color = "#888888")+
    facet_wrap(~ iso3, ncol = 4) +
    geom_hline(yintercept=0, linetype="dashed", color = "red")+
    ylab("CO2 in atmosphere, distance to macro region median") +
    xlab("")+
    theme_bw()+
    theme(legend.title = element_blank())

file = file.path(folders$tmp, "plots", paste0("anomalyPlot_", operation, ".png"))
ggsave(file, width = 10, height = 8, units="in")
