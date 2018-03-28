# ----------------------------------------------
# BASE
# ----------------------------------------------
rm(list=ls())
source("./sandbox/daytona/trunk/base/init.r", chdir=TRUE)
# ----------------------------------------------

rasterPath = "../daytona/data/oco2/v8rData"
tempPath =  "../daytona/data/temperature/rData"

globalCo2Data = as.data.table(read.csv(file.path("../daytona/data/NOAA", "co2_mm_gl.txt"), skip=57, header=TRUE, sep=""))
setnames(globalCo2Data, c("year", "month", "decimal", "average", "trend", "X"))
globalCo2Data[, X:=NULL]
globalCo2Data = cbind(globalCo2Data, date = strptime(paste(globalCo2Data$year, "1", globalCo2Data$month), format="%Y %d %m"))
globalCo2Data$date = as.yearmon(globalCo2Data$date, "%b %Y")

globalCo2Data[, `:=`(year = NULL,
                     month = NULL,
                     decimal = NULL,
                     trend = NULL)]


rasteredData = loadData(file.path(rasterPath, "rastered.rData"))
# ----------------------------------------------
# DERASTERIZE RASTERED DATA TO 2° WIDTH & MERGE WITH ANOMALIES
# ----------------------------------------------
#rasteredData[, lats := roundUp(from=lats,to=2)-1]
#rasteredData[, lons := roundUp(from=lons,to=2)-1]
#rasteredData = merge(rasteredData, globalCo2Data, by=c("date"))
rasteredData[, id := .GRP, by = .(lats, lons)]  ## MAKE ID RANDOMIZED STANDARD 1 DEGREE, 2 DEGREE, UNIQUE IDENTIFICATION
rasteredData[, average := mean(xco2, na.rm=TRUE), by="date"]
rasteredData[, average_constant := mean(average, na.rm=TRUE)]

df.id = unique(rasteredData[, .(lats, lons, id)])

rasteredData = rasteredData[, .(xco2 = mean(xco2, na.rm=TRUE),
                                average = mean(average, na.rm=TRUE),
                                average_constant = mean(average_constant)), by=c("id", "date")]

rasteredData = merge(rasteredData, df.id, by=c("id"))
# ----------------------------------------------

# ----------------------------------------------
# BUILD DATASET
# ----------------------------------------------

rasteredData[, xco2 := removeOutliers(xco2)]
rasteredData[, month := month(date)]



#approxData = rasteredData[, .(id, date, xco2, month)]
approxData = rasteredData[, .(xco2 = sum(xco2, na.rm = TRUE)), by=.(id, month)]
approxData[xco2 > 0, len := 1][, xco2 := NULL]

rasteredData = merge(rasteredData, approxData, by=c("id", "month"), all.x = TRUE)
rasteredData[len == 1, xco2a := na.spline(xco2), by=c("id", "month")]

rasteredData[is.na(xco2), xco2 := xco2a]
rasteredData = rasteredData[order(id, date)]

rasteredData = movingAverage(data=rasteredData, by="id", variable = "xco2")
rasteredData = movingAverage(data=rasteredData, by="id", variable = "average")

distanceData = copy(rasteredData)

distanceData[, dist := xco2_ma12 - average_ma12]

distanceData[, dist_constant := xco2_ma12 - average_constant]

distanceData = distanceData[order(lats, lons, date), c("id", "lats", "lons", "date", "dist", "dist_constant")]


distanceData[!is.na(dist) & month(date) %in% c(1, 4, 7, 10), distc1y := lapply(.SD, absolutChange), by = id, .SDcols = c("dist")]
distanceData[!is.na(dist_constant) & month(date) %in% c(1, 4, 7, 10), distc1y_constant := lapply(.SD, absolutChange), by = id, .SDcols = c("dist_constant")]

#rasteredData[!is.na(dist), dist.lag :=c(NA, dist[-.N]), by=id]
#rasteredData[!is.na(dist), distc1y := dist-dist.lag, by = id]


# ----------------------------------------------
# SET DUMMY = 1 IF POINT IS MINIMUM DISTANCE TO MEAN TO PREVEOUS PERIODS
# ----------------------------------------------

distanceData[!is.na(dist) & month(date) %in% c(1, 4, 7, 10), cummin := lapply(.SD, cummin), by = .(id), .SDcols = c("dist")]
distanceData[!is.na(dist_constant) & month(date) %in% c(1, 4, 7, 10), cummin_constant := lapply(.SD, cummin), by = .(id), .SDcols = c("dist_constant")]
distanceData$mindistance = 0
distanceData[cummin == dist, mindistance :=1]
distanceData[cummin_constant == dist_constant, mindistance_constant :=1]

#distanceData = distanceData[, -c("changesum"), with=F]
distanceData[mindistance==1, mincount := distc1y]
distanceData[mindistance_constant==1, mincount_constant := distc1y_constant]
distanceData[, changesum := sum(mincount, na.rm = TRUE), by=id]
distanceData[, changesum_constant := sum(mincount_constant, na.rm = TRUE), by=id]
distanceData[is.na(mincount_constant), mindistance_constant := NA]
distanceData[is.na(mincount), mindistance := NA]
#distanceData[, changesum_constant := sum(distc1y, na.rm = TRUE), by=id]

finalData = merge(distanceData, rasteredData, by=c("id", "lats", "lons", "date"))
#finalData$date = as.Date(finalData$date)


dataPath = "./sandbox/daytona/tmp/data/oco2/"
saveData(finalData, file.path(dataPath, "rasterWorldData.rData"))


#performanceData = merge(raster, unique(distanceData[, .(lons, lats, changesum_constant), by=id]), by=c("lats", "lons"), all=T)
performanceData = distanceData[, .(lons=mean(lons), lats=mean(lats), changesum_constant=mean(changesum_constant, na.rm=TRUE)), by=id]
performanceData[changesum_constant == 0, changesum_constant := NA]



#performanceData = performanceData[!is.na(changesum_constant)]
# performanceData[, `:=`(lats = lats+0.5,
#                        lons = lons+0.5)]
library(sp)
library(rgdal)
library(raster)
WGScoor<-  performanceData[,2:4]
coordinates(WGScoor)=~lons+lats

gridded(WGScoor) <- TRUE
WGScoor= raster(WGScoor)


B <- SpatialPoints(WGScoor)

library(rgeos)      ## for gBuffer()
library(raster)     ## for bind()

pp <- list()
for(i in seq_along(B)) {
    pp[i] <- gBuffer(B[i], width=1, quadsegs=1, capStyle="SQUARE")
}
PP <- do.call(bind, pp)


#p <- SpatialPolygonsDataFrame(PP, performanceData)

( pid <- sapply(slot(p, "polygons"), function(x) slot(x, "ID")) )
#( performanceData <- data.frame( ID=1:length(p), row.names = pid) )    
p <- SpatialPolygonsDataFrame(p, performanceData)
class(p) 

# leaflet(p) %>%
#     addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
#                 color = ~pal(changesum_constant))
#library("RColorBrewer")
#myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
#sc <- scale_colour_gradientn(colours = colorRampPalette(c("#3E3CC3", "#F3F9FB", "#3cc13e"))(100), limits=c(-20, 20), na.value = "white")
#color_palette <- colorRampPalette(c("#3794bf", "#FFFFFF", "#df8640"))(100)
#sc = scale_fill_manual(values = color_palette)


#mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders

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

plotFolder = file.path("./sandbox/daytona/plots")
ggsave(file.path(plotFolder, "performance_map.png"), width = 10, height = 5)



us <- map("us")
plotData = performanceData[lons %in% unique(round(us$long)+0.5) & lats %in% unique(round(us$lat)+0.5)]

library("ggmap")
# Eurasia: lon 32
# Eastasia: lon 120
mapgilbert <- get_map(location = c(lon = 32, 
                                   lat = 40), 
                      zoom = 3,
                      maptype = "satellite")


ggmap(mapgilbert) +
    #geom_point(data = performanceData, aes(x = lons, y = lats, fill = "red", alpha = 0.8), size = 1, shape = 21) + 
    #guides(fill=FALSE, alpha=FALSE, size=FALSE)+
    geom_tile(data=performanceData, aes(x=lons, y=lats, fill=changesum_constant, group=id))+
    scale_fill_gradient2(na.value = 'transparent', low = "#3794bf", mid = "#FFFFFF", high = "#df8640")+
    theme_bw() + 
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12))+
    labs(x="Longitude",y="Latitude")

ggsave(file.path(plotFolder, "performance_map_eurasia.png"), width = 6, height = 5)

ggplot(us, aes(x = long, y = lat, group = group)) +
    geom_polygon(fill = "white", colour = "black") +
    geom_tile(data=plotData, aes(x=lons, y=lats, fill=changesum_constant, group=id), alpha=0.8) + 
    scale_fill_gradient2(na.value = '#FFFFFF', low = "#3794bf", mid = "#FFFFFF", high = "#df8640")+
    theme_bw() + 
    coord_equal()+
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12))+
    labs(x="Longitude",y="Latitude")+
    coord_cartesian(xlim = c(min(plotData$lons), max(plotData$lons)))

plotFolder = file.path("./sandbox/daytona/plots")
ggsave(file.path(plotFolder, "performance_map_us.png"), width = 10, height = 5)

#predictQuarterly(data=rasteredData[id == 5844], groupVar="id", variable = "xco2")


# library("RColorBrewer")
# myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
# sc <- scale_colour_gradientn(colours = colorRampPalette(c("#3E3CC3", "#F3F9FB", "#3cc13e"))(100), limits=c(-3, 3), na.value = "white")
# color_palette <- colorRampPalette(c("#3794bf", "#FFFFFF", "#df8640"))(100)
# sc = scale_fill_manual(values = color_palette)

anomalieData = rasteredData[, .(xco2 = mean(xco2, na.rm = TRUE)), by=c("id", "lats", "lons")]
anomalieData[, mean := mean(xco2, na.rm = TRUE)]
anomalieData[, anomaly := xco2 - mean]
wr <- map_data("world", col = 1:10, wrap=c(-180,180) )
ggplot(wr, aes(x = long, y = lat, group = group)) +
    geom_polygon(fill = "white", colour = "black") +
    #geom_point(data=performanceData, inherit.aes=FALSE,aes(x=lons, y=lats, color=changesum),size=0.5) +
    geom_tile(data=anomalieData, aes(x=lons, y=lats, fill=anomaly, group=id), alpha=0.8) + 
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

plotFolder = file.path("./sandbox/daytona/tmp/plots")
ggsave(file.path(plotFolder, "anomaly_map.png"), width = 15, height = 8)

## SHUFFE

anomalieData[sample(nrow(anomalieData), nrow(anomalieData)), ]
