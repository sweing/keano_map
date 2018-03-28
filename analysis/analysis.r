# ----------------------------------------------
# BASE
# ----------------------------------------------
rm(list=ls())
source("./common/base/init.r", chdir=TRUE)
# ----------------------------------------------

finalData = loadData("oco2.rData")


finalData = baseData[, .(xco2 = mean(xco2), quadrant = 1), by=time]

finalData[!is.na(xco2), xco2_r12 := lapply(.SD, rollapplyr, FUN=mean, list(-(11:0)), fill = NA, partial = FALSE), 
          by=c("quadrant"), .SDcols = c("xco2")]


finalData[, time :=NULL]

finalData = ts(data = finalData, frequency = 12, start = c(2014, 9))
finalData
## Not run:
m <- seas(finalData)
udg(x, "x13mdl")
## End(Not run)
## Not run:
m <- seas(AirPassengers)
view(m)
## End(Not run)

ggplot(data=finalData, aes(x=time, y=xco2))+
    geom_point()+
    geom_line(data=finalData, aes(x=time, y=xco2_r12, group=FALSE))



# loading the required packages
library(ggmap)

# getting the map
mapgilbert <- get_map(location = c(lon = mean(baseData$lons), lat = mean(baseData$lats)), zoom = 8,
                      maptype = "satellite", scale = 2)


# plotting the map with some points on it
ggmap(mapgilbert) +
    geom_point(data = baseData, aes(x = lons, y = lats, fill = xco2, alpha = 0.8), size = 4, shape = 0) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE)