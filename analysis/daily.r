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


baseData[, xco2 := removeOutliers(xco2)]
baseData = baseData[!is.na(xco2)]
#baseData = baseData[, .(xco2 = mean(xco2)), by=.(iso3, date)]
baseData[, daily_median := median(xco2), by=c("time")][, xco2_anomaly := xco2 - daily_median][, daily_median := NULL]

#baseData[, id := .GRP, by = .(lats, lons)]

baseData = cbind(baseData, date = as.Date(baseData$time,format="%Y-%m-%d"))
#meanData$date = as.Date(meanData$date,format="%m/%d/%y %H:%M")
baseData$date = as.yearmon(baseData$date, "%b %Y")


#baseData[, meanAnomaly := mean(xco2_anomaly, na.rm=TRUE)]

baseData[, dist_constant := xco2_anomaly]
baseData[, month := month(date)]


baseData[!is.na(dist_constant) & month(date) %in% c(1, 4, 7, 10), distc1y_constant := lapply(.SD, absolutChange), by = id, .SDcols = c("dist_constant")]
baseData[!is.na(dist_constant) & month(date) %in% c(1, 4, 7, 10), cummin_constant := lapply(.SD, cummin), by = .(id), .SDcols = c("dist_constant")]
baseData$mindistance = 0
baseData[cummin_constant == dist_constant, mindistance_constant :=1]

baseData[mindistance_constant==1, mincount_constant := distc1y_constant]
baseData[, changesum_constant := sum(mincount_constant, na.rm = TRUE), by=id]
baseData[is.na(mincount_constant), mindistance_constant := NA]


performanceData = baseData[, .(lons=mean(lons), lats=mean(lats), changesum_constant=mean(changesum_constant, na.rm=TRUE)), by=id]
performanceData[changesum_constant == 0, changesum_constant := NA]


wr <- map_data("world", col = 1:10, wrap=c(-180,180) )

plotData = performanceData[!is.na(changesum_constant)]

ggplot(wr, aes(x = long, y = lat, group = group)) +
    #mapWorld +
    geom_polygon(fill = "white", colour = "black") +
    #geom_point(data=performanceData, inherit.aes=FALSE,aes(x=lons, y=lats, color=changesum),size=0.5) +
    geom_tile(data=plotData, aes(x=lons, y=lats, fill=changesum_constant, group=id), alpha=0.8) + 
    #scale_color_gradient(limits = c(-30, +30), low = "blue", high = "red") +
    #scale_fill_gradient2(na.value = '#FFFFFF', low = "#3794bf", mid = "#FFFFFF", high = "#df8640", breaks=c(min(performanceData$changesum_constant, na.rm=TRUE), min(performanceData$changesum_constant, na.rm=TRUE)/2, 0.1), labels=c("Maximum CO2 ppm decrease", "" ,"Minimum CO2 ppm decrease"))+
    scale_fill_gradient2(na.value = '#FFFFFF', low = "black", mid = "yellow", high = "#df8640")+
    theme_bw() + 
    #sc +
    coord_equal()+
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12))+
    labs(x="Longitude",y="Latitude")+
    coord_cartesian(xlim = c(min(plotData$lons), max(plotData$lons)))




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
