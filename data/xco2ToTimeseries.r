# ----------------------------------------------
# BASE
# ----------------------------------------------
rm(list=ls())
source("./sandbox/daytona/trunk/base/init.r", chdir=TRUE)
# ----------------------------------------------

dataPath = "../daytona/data/oco2/v8rData/daily"
baseData = loadData(file.path(dataPath, "rasterXco2.rData"))

baseData[, tmp := xco2-shift(xco2, n = 1L, type="lag", fill = 0), by=c("iso3", "rasterLats", "rasterLons")]
baseData[xco2 != tmp, skip := 1:.N, by=c("iso3")]
baseData[, skip := na.locf(skip, na.rm=FALSE), by=c("iso3")]

baseData[is.na(skip), skip := 0]
baseData[, cummean := cumsum(tmp)/(seq_along(tmp)-skip), by=c("iso3")][, `:=` (skip = NULL, tmp = NULL)]

# baseData[, nDaily := 1:.N, by=c("iso3", "date")]
# baseData[, NDaily := .N, by=c("iso3", "date")]
# 
# baseData = baseData[nDaily == NDaily]

#selected = c("ISL")
selected = c("CHN", "USA", "IND", "RUS", "JPN", "DEU", "IRN", "AUT", "FRA", "AUS", "BRA", "PRK")
plotData = baseData[iso3 %in% selected & date > "2014-12-31"]

ggplot(data = plotData, aes(x=date, y=cummean, group=iso3))+
    #geom_smooth(method = "lm")+
    geom_line(stat = "identity")+
    facet_wrap(~ iso3, ncol = 6) +
    ylab("Mean CO2 concentration in atmosphere (ppm)") +
    xlab("")+
    theme_bw()+
    theme(legend.title = element_blank())

file = file.path(folders$tmp, "plots", "xco2.png")

ggsave(file, width = 10, height = 5, units="in")

