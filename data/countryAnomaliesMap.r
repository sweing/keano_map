# ----------------------------------------------
# BASE
# ----------------------------------------------
rm(list=ls())
source("./sandbox/daytona/trunk/base/init.r", chdir=TRUE)
# ----------------------------------------------

library(ggmap)


dataPath = "../daytona/data/oco2/v8rData/daily"
country = "AUT"
resolution = 0.8
operation = "median"

fileName = paste0("Anomalies_", country, "_m", resolution, "_", operation)
#fileName = "Anomalies_AUT_m1_median.rData"
baseData = loadData(file.path(dataPath, paste0(fileName, ".rData")))


#baseData = baseData[lons > 116 & lats > 29 & lons < 122 & lats < 33]

### Set a range
lat = c(round(min(baseData$lats))-2, round(max(baseData$lats))+2)
lon = c(round(min(baseData$lons))-2, round(max(baseData$lons))+2)
### Get a map



get_countryMap <- get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 9, source = "google")

# Draw the map
countryMap = ggmap(get_countryMap)

# Add the points layer
countryMap = countryMap + 
    geom_point(data = baseData[anomaly > -4 & anomaly < 4], aes(x = lons, y = lats, colour = anomaly), size = 0.6)+
    scale_colour_gradientn(colours = c('red', 'white', 'darkblue'),
                           values   = c(1, 0.5, 0))+
    theme(axis.ticks.x = element_blank(), 
          axis.ticks.y = element_blank(),
          legend.position = "none",
          axis.text.x= element_blank(),
          axis.text.y= element_blank())+
    ylab(NULL) +
    xlab(NULL) +
    #scale_x_continuous(limits = c(min(baseData$lons), max(baseData$lons)), expand = c(0, 0)) +
    scale_x_continuous(limits = c(12.9, 14), expand = c(0, 0)) +
    #scale_y_continuous(limits = c(min(baseData$lats), max(baseData$lats)), expand = c(0, 0))
    scale_y_continuous(limits = c(47.3, 48), expand = c(0, 0))

countryMap



file = file.path(folders$tmp, "plots", paste0(fileName, ".jpg"))
ggsave(file, width = 5, height = 5)

