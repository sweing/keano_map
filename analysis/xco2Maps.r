# ----------------------------------------------
# BASE
# ----------------------------------------------
rm(list=ls())
source("./sandbox/daytona/trunk/base/init.r", chdir=TRUE)
# ----------------------------------------------

operation = "mean"


dataPath = "../daytona/data/oco2/v8rData/daily"
baseData = loadData(file.path(dataPath, paste0("rasterXco2.rData")))


#baseData[, tmp := anomaly-anomaly[1], by=c("iso3", "rasterLats", "rasterLons")]
baseData[, tmp := xco2-shift(xco2, n = 1L, type="lag", fill = 0), by=c("iso3", "rasterLats", "rasterLons")]

baseData[xco2 != tmp, skip := 1:.N, by=c("iso3")]
baseData[, skip := na.locf(skip, na.rm=FALSE), by=c("iso3")]
baseData[is.na(skip), skip := 0]

#baseData[tmp == 0, count := 1:.N, by=c("iso3")]
#baseData[, count := na.locf(count, na.rm=FALSE), by=c("iso3")]

baseData[, cummean := cumsum(tmp)/(seq_along(tmp)-skip), by=c("iso3")][, `:=` (skip = NULL, tmp = NULL)]
#baseData[, cummean := cumsum(tmp)/count, by=c("iso3")]


baseData[, nDaily := 1:.N, by=c("iso3", "date")]
baseData[, NDaily := .N, by=c("iso3", "date")]
baseData = baseData[nDaily == NDaily]


plotData = baseData[, .(cummean = mean(cummean, na.rm=TRUE)), by=c("rasterLats", "rasterLons", "date")]

wr <- map_data("world", col = 1:10, wrap=c(-180,180) )
for(day in as.character(unique(baseData$date))){
    print(day)
    ggplot(wr, aes(x = long, y = lat)) +
        #mapWorld +
        #geom_polygon(fill = "white", colour = "black") +
        #geom_point(data=performanceData, inherit.aes=FALSE,aes(x=lons, y=lats, color=changesum),size=0.5) +
        geom_tile(data=plotData[date == day], aes(x=rasterLons, y=rasterLats, fill=cummean)) + 
        scale_color_gradient2(limits = c(385, 410), low = "blue", high = "red") +
        #scale_fill_gradient2(na.value = '#FFFFFF', low = "#3794bf", mid = "#FFFFFF", high = "#df8640", breaks=c(min(plotData$cummean, na.rm=TRUE), min(min(plotData$cummean, na.rm=TRUE)/2, 0.1), labels=c("Maximum CO2 ppm decrease", "" ,"Minimum CO2 ppm decrease"))+
        #scale_fill_gradient2(na.value = '#FFFFFF', low = "#FFFFFF", mid = "#eaeaea", high = "#000000")+
        theme_bw() + 
        #sc +
        coord_equal()+
        theme(legend.title = element_blank(),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12))+
        labs(x="Longitude",y="Latitude")+
        coord_cartesian(xlim = c(min(plotData$rasterLons), max(plotData$rasterLons)))
    
    file = file.path(folders$tmp, "plots", "dailyMaps", paste0("anomalyMap_", day, ".png"))
    ggsave(file, width = 10, height = 5, units="in")
}
