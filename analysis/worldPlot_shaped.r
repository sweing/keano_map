# ----------------------------------------------
# BASE
# ----------------------------------------------
rm(list=ls())
source("./sandbox/daytona/trunk/base/init.r", chdir=TRUE)
# ----------------------------------------------

shapedPath = "../daytona/data/oco2/v8rData"


shapedData = loadData(file.path(shapedPath, "shaped.rData"))
# ----------------------------------------------
# DERASTERIZE RASTERED DATA TO 2° WIDTH & MERGE WITH ANOMALIES
# ----------------------------------------------
#rasteredData[, lats := roundUp(from=lats,to=2)-1]
#rasteredData[, lons := roundUp(from=lons,to=2)-1]
#rasteredData = merge(rasteredData, globalCo2Data, by=c("date"))
#rasteredData[, id := .GRP, by = .(lats, lons)]  ## MAKE ID RANDOMIZED STANDARD 1 DEGREE, 2 DEGREE, UNIQUE IDENTIFICATION

shapedData = cbind(shapedData, date = as.Date(shapedData$time,format="%Y-%m-%d"))
shapedData$date = as.yearmon(shapedData$date, "%b %Y")

shapedData = shapedData[, .(xco2 = mean(xco2, na.rm = TRUE)), by=.(iso3, date)]

shapedData[, average := mean(xco2, na.rm=TRUE), by="date"]
shapedData[, average_constant := mean(average, na.rm=TRUE)]
# ----------------------------------------------

# ----------------------------------------------
# BUILD DATASET
# ----------------------------------------------

shapedData[, xco2 := removeOutliers(xco2)]
shapedData[, month := month(date)]



#approxData = rasteredData[, .(id, date, xco2, month)]
approxData = shapedData[, .(xco2 = sum(xco2, na.rm = TRUE)), by=.(iso3, month)]
approxData[xco2 > 0, len := 1][, xco2 := NULL]


shapedData = merge(shapedData, approxData, by=c("iso3", "month"), all.x = TRUE)
shapedData[len == 1, xco2a := na.spline(xco2), by=c("iso3", "month")]

shapedData[is.na(xco2), xco2 := xco2a][, len := NULL]

shapedData = shapedData[order(iso3, date)]
shapedData = shapedData[!is.na(iso3)]



shapedData[, len := .N, by="iso3"]

shapedData = shapedData[len >= 12]

shapedData = movingAverage(data=shapedData, by="iso3", variable = "xco2")
shapedData = movingAverage(data=shapedData, by="iso3", variable = "average")

shapedData[, dist := xco2_ma12 - average_ma12]

shapedData[, dist_constant := xco2_ma12 - average_constant]

distanceData = shapedData[order(iso3, date), c("iso3", "date", "dist", "dist_constant")]


distanceData[!is.na(dist) & month(date) %in% c(1, 4, 7, 10), distc1y := lapply(.SD, absolutChange), by = iso3, .SDcols = c("dist")]
distanceData[!is.na(dist_constant) & month(date) %in% c(1, 4, 7, 10), distc1y_constant := lapply(.SD, absolutChange), by = iso3, .SDcols = c("dist_constant")]

#rasteredData[!is.na(dist), dist.lag :=c(NA, dist[-.N]), by=id]
#rasteredData[!is.na(dist), distc1y := dist-dist.lag, by = id]


# ----------------------------------------------
# SET DUMMY = 1 IF POINT IS MINIMUM DISTANCE TO MEAN TO PREVEOUS PERIODS
# ----------------------------------------------

distanceData[!is.na(dist) & month(date) %in% c(1, 4, 7, 10), cummin := lapply(.SD, cummin), by = .(iso3), .SDcols = c("dist")]
distanceData[!is.na(dist_constant) & month(date) %in% c(1, 4, 7, 10), cummin_constant := lapply(.SD, cummin), by = .(iso3), .SDcols = c("dist_constant")]
distanceData$mindistance = 0
distanceData[cummin == dist, mindistance :=1]
distanceData[cummin_constant == dist_constant, mindistance_constant :=1]

#distanceData = distanceData[, -c("changesum"), with=F]
distanceData[mindistance==1, mincount := distc1y]
distanceData[mindistance_constant==1, mincount_constant := distc1y_constant]
distanceData[, changesum := sum(mincount, na.rm = TRUE), by=iso3]
distanceData[, changesum_constant := sum(mincount_constant, na.rm = TRUE), by=iso3]
#distanceData[, changesum_constant := sum(distc1y, na.rm = TRUE), by=id]

finalData = merge(distanceData[, -c("dist", "dist_constant"), with=FALSE], shapedData, by=c("iso3", "date"))

index = finalData[date >= "Jän 2016", .(iso3, date, xco2_ma12)]
index = melt(index, id.vars=c("iso3", "date"))

index = toIndex(index, "Jän 2016")
index = dcast(index, iso3 + date ~ variable, value.var="value")
setnames(index, "xco2_ma12", "xco2_ma12_index")

finalData = merge(finalData, index, by=c("iso3", "date"))

#performanceData = merge(raster, unique(distanceData[, .(lons, lats, changesum_constant), by=id]), by=c("lats", "lons"), all=T)
performanceData = unique(distanceData[, .(changesum), by=iso3])
performanceData[changesum_constant == 0, changesum_constant := NA]
performanceData = performanceData[!is.na(changesum)]



library(rworldmap)

# malDF is a data.frame with the ISO3 country names plus a variable to
# merge to the map data

malMap <- joinCountryData2Map(performanceData, joinCode = "ISO3",
                              nameJoinColumn = "iso3")

dataPath = "./sandbox/daytona/tmp/data/oco2/"
saveData(finalData, file.path(dataPath, "shapedWorldData.rData"))
#saveData(finalData, file.path(dataPath, "shapedWorldDataPerformance.rData"))

# This will join your malDF data.frame to the country map data
plotFolder = file.path("./sandbox/daytona/tmp/plots")
png(file.path(plotFolder, "performance_map_iso3.png"), width = 2000, height = 1000)

mapCountryData(malMap, nameColumnToPlot="changesum", catMethod = "categorical",
               missingCountryCol = gray(.8), addLegend=FALSE, colourPalette = "heat", mapTitle = "", oceanCol = "black")
dev.off()






anomalieData = shapedData[, .(xco2 = mean(xco2, na.rm = TRUE)), by=c("iso3")]
anomalieData[, mean := mean(xco2, na.rm = TRUE)]
anomalieData[, anomaly := xco2 - mean]


# malDF is a data.frame with the ISO3 country names plus a variable to
# merge to the map data

malMap <- joinCountryData2Map(distanceData, joinCode = "ISO3",
                              nameJoinColumn = "iso3")
# This will join your malDF data.frame to the country map data

png(file.path(plotFolder, "anomaly_map_iso3.png"), width = 2000, height = 1000)

mapCountryData(malMap, nameColumnToPlot="xco2", catMethod = "categorical",
               missingCountryCol = gray(.8), addLegend=FALSE,  colourPalette = "heat", mapTitle = "", oceanCol = "black")

dev.off()

