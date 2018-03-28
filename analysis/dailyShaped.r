# ----------------------------------------------
# BASE
# ----------------------------------------------
rm(list=ls())
source("./sandbox/daytona/trunk/base/init.r", chdir=TRUE)
# ----------------------------------------------

dataPath = "../daytona/data/oco2/v8rData/daily"

part1 = loadData(file.path(dataPath, "daily_part1.rData"))
part2 = loadData(file.path(dataPath, "daily_part2.rData"))

baseData = rbind(part1, part2)

rm(part1, part2)


library(raster)
dfr <- rasterFromXYZ(baseData)  #Convert first two columns as lon-lat and third as value                
plot(dfr$xco2)

baseData[, id := .GRP, by = .(lats, lons)] 
baseData[, xco2 := removeOutliers(xco2)]
baseData = baseData[!is.na(xco2)]

baseData[, daily_median := median(xco2), by=c("time")][, xco2_anomaly := xco2 - daily_median][, daily_median := NULL]
baseData$time = as.Date(baseData$time,format="%Y-%m-%d")
baseData[, yearQuarter := as.yearqtr(time)]
#baseValues = baseData[, .SD[which.min(time), ], by=.(lons, lats)] 
#baseData = merge(baseData, baseValues[, .(xco2_anomaly_base = xco2_anomaly, lons, lats)], by=c("lons", "lats"), all.x=TRUE)
#baseData[, xco2_anomaly_dist := xco2_anomaly - xco2_anomaly_base][, xco2_anomaly_base := NULL]

meanData = baseData[, .(xco2_anomaly_mean = mean(xco2_anomaly)), by=.(id)]
baseData = merge(baseData, meanData[, .(xco2_anomaly_mean = xco2_anomaly_mean, id)], by=c("id"), all.x=TRUE)


baseData = baseData[, .(xco2 = mean(xco2), xco2_anomaly_mean = mean(xco2_anomaly_mean)), by=.(iso3, time, yearQuarter)]

#minData = merge(baseData[, list(time = min(time)), by=.(iso3, yearQuarter)], baseData[, c("iso3", "yearQuarter", "time", "xco2_anomaly_mean"), with=FALSE], by = c("iso3", "yearQuarter", "time"), all.x=TRUE)


#baseData = merge(baseData, minData[, list(baseValue = xco2_anomaly_mean), by=.(iso3, yearQuarter)], by=c("iso3", "yearQuarter"))


baseData[, dist := xco2_anomaly_mean]
#baseData = baseData[, list(iso3, date, value=index, variable)]


#baseData[, dist := xco2_anomaly]
#baseData[, month := month(date)]


baseData[!is.na(dist), distc1y := lapply(.SD, absolutChange), by = .(iso3, yearQuarter), .SDcols = c("dist")]
baseData[!is.na(dist), cummin := lapply(.SD, cummin), by = .(iso3, yearQuarter), .SDcols = c("dist")]
baseData$mindistance = 0
baseData[cummin == dist, mindistance :=1]

baseData[mindistance==1, mincount := distc1y]
baseData[, changesum := sum(mincount, na.rm = TRUE), by=.(iso3, yearQuarter)]
baseData[is.na(mincount), mindistance := NA]

rankData = baseData[, .(changesum = mean(changesum)), by=.(iso3, yearQuarter)]
rankData[, rank := frank(changesum), by="yearQuarter"]

baseData = merge(baseData, rankData[,.(rank = rank, iso3 = iso3, yearQuarter = yearQuarter)], by=c("iso3", "yearQuarter"))


                                                       
dataPath = "./sandbox/daytona/tmp/data/oco2/"
saveData(baseData, file.path(dataPath, "shapedWorldData_daily.rData"))                                                     

performanceData = baseData[, .(changesum=mean(changesum, na.rm=TRUE)), by=iso3]
performanceData[changesum == 0, changesum := NA]


library(rworldmap)

malMap <- joinCountryData2Map(performanceData, joinCode = "ISO3",
                              nameJoinColumn = "iso3")


# malDF is a data.frame with the ISO3 country names plus a variable to
# merge to the map data

plotFolder = file.path("./sandbox/daytona/tmp/plots")
png(file.path(plotFolder, "performance_map_iso3_daily.png"), width = 2000, height = 1000)

mapCountryData(malMap, nameColumnToPlot="changesum", catMethod = "categorical",
               missingCountryCol = gray(.8), addLegend=FALSE, colourPalette = "heat", mapTitle = "", oceanCol = "black")
dev.off()

#dataPath = "./sandbox/daytona/tmp/data/oco2/"
#saveData(finalData, file.path(dataPath, "shapedWorldData.rData"))
#saveData(finalData, file.path(dataPath, "shapedWorldDataPerformance.rData"))

# This will join your malDF data.frame to the country map data
#plotFolder = file.path("./sandbox/daytona/tmp/plots")
#png(file.path(plotFolder, "performance_map_iso3.png"), width = 2000, height = 1000)

mapCountryData(malMap, nameColumnToPlot="changesum_constant", catMethod = "categorical",
               missingCountryCol = gray(.8), addLegend=FALSE, colourPalette = "heat", mapTitle = "", oceanCol = "black")
#dev.off()





anomalyData = baseData[, .(xco2_anomaly = mean(xco2_anomaly)), by=.(lats, lons, id)]

anomalyData$id = 1:nrow(anomalyData)

ggplot(wr, aes(x = long, y = lat, group = group)) +
    geom_polygon(fill = "white", colour = "black") +
    #geom_point(data=performanceData, inherit.aes=FALSE,aes(x=lons, y=lats, color=changesum),size=0.5) +
    geom_tile(data=anomalyData, aes(x=lons, y=lats, fill=xco2_anomaly, group=id), alpha=0.8) + 
    #scale_fill_continuous(na.value = 'white')+
    #scale_color_gradient(limits = c(-30, +30), low = "blue", high = "red") +
    scale_fill_gradient2(limits = c(-10, +10), na.value = 'white', low = "#3794bf", mid = "#FFFFFF", high = "#df8640")+
    theme_bw() + 
    #sc +
    coord_equal()+
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12))+
    labs(x="Longitude",y="Latitude")+
    coord_cartesian(xlim = c(-180, 180))

plotFolder = file.path("./sandbox/daytona/plots")
ggsave(file.path(plotFolder, "anomaly_map2.png"), width = 15, height = 8)
