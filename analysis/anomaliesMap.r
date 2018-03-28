# ----------------------------------------------
# BASE
# ----------------------------------------------
rm(list=ls())
source("./sandbox/daytona/trunk/base/init.r", chdir=TRUE)
# ----------------------------------------------

# ----------------------------------------------
# CONFIG
# ----------------------------------------------
macroResolution = 0.1
rasterResolution = 0.4
operation = "mean"
countries = c("AUT", "DEU")
# ----------------------------------------------


fileName = paste0("rasterAnomalies", "_m", macroResolution, "_r", rasterResolution, "_", operation, ".rData")
dataPath = "../daytona/data/oco2/v8rData/daily"
baseData = loadData(file.path(dataPath, fileName))

baseData = baseData[iso3 %in% countries]

#baseData = baseData[, .(anomaly = max(anomaly)-min(anomaly)), by=c("rasterLats", "rasterLons")]
baseData = baseData[, .(anomaly = mean(anomaly)), by=c("rasterLats", "rasterLons")]

wr <- map_data("world", col = 1:10, wrap=c(-180,180) )

plotData = baseData[!is.na(anomaly)]

ggplot(wr, aes(x = long, y = lat)) +
    #mapWorld +
    #geom_polygon(fill = "white", colour = "black") +
    #geom_point(data=performanceData, inherit.aes=FALSE,aes(x=lons, y=lats, color=changesum),size=0.5) +
    geom_tile(data=plotData, aes(x=rasterLons, y=rasterLats, fill=anomaly)) + 
    #scale_color_gradient(limits = c(-30, +30), low = "blue", high = "red") +
    #scale_fill_gradient2(na.value = '#FFFFFF', low = "#3794bf", mid = "#FFFFFF", high = "#df8640", breaks=c(min(performanceData$changesum_constant, na.rm=TRUE), min(performanceData$changesum_constant, na.rm=TRUE)/2, 0.1), labels=c("Maximum CO2 ppm decrease", "" ,"Minimum CO2 ppm decrease"))+
    scale_fill_gradient2(na.value = '#FFFFFF', low = "#FFFFFF", mid = "#eaeaea", high = "#000000")+
    theme_bw() + 
    #sc +
    coord_equal()+
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12))+
    labs(x="Longitude",y="Latitude")+
    coord_cartesian(xlim = c(min(plotData$rasterLons), max(plotData$rasterLons)))

file = file.path(folders$tmp, "plots", paste0("anomalyMap_", operation, ".png"))
ggsave(file, width = 10, height = 5, units="in")
