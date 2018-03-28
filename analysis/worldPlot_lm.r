# ----------------------------------------------
# BASE
# ----------------------------------------------
rm(list=ls())
source("./common/base/init.r", chdir=TRUE)
# ----------------------------------------------

# ----------------------------------------------
# LOAD DATA
# ----------------------------------------------
rasterPath = "../daytona/data/oco2/rData"
tempPath =  "../daytona/data/temperature/rData"

rasteredData = loadData(file.path(rasterPath, "rastered.rData"))
tempData = loadData(file.path(tempPath, "tempanomaly.rData"))
# ----------------------------------------------

# ----------------------------------------------
# DERASTERIZE RASTERED DATA TO 2° WIDTH & MERGE WITH ANOMALIES
# ----------------------------------------------
RoundUp <- function(from,to) ceiling(from/to)*to
rasteredData[, lats := RoundUp(from=lats,to=2)+1]
rasteredData[, lons := RoundUp(from=lons,to=2)+1]
rasteredData[, ID := .GRP, by = .(lats, lons)]

df.id = unique(rasteredData[, .(lats, lons, ID)])

rasteredData = rasteredData[, .(xco2 = mean(xco2, na.rm=TRUE),
                                average = mean(average, na.rm=TRUE)), by=c("ID", "date")]

rasteredData = merge(rasteredData, df.id, by=c("ID"))
rasteredData = merge(rasteredData, tempData, by=c("lons", "lats", "date"), all.x = TRUE)
# ----------------------------------------------

# ----------------------------------------------
# BUILD DATASET
# ----------------------------------------------
rasteredData = rasteredData[date >= 2015]
rasteredData[, "ma_12_avg" := lapply(.SD, rollapplyr, FUN=mean, list(-(11:0)), fill = NA, partial = T),
             by = ID, .SDcols = c("average")]


remove_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
}

rasteredData[, xco2 := remove_outliers(xco2)]


rasteredData$month = month(rasteredData$date)
#rasteredData[, abslats := abs(lats)]
rasteredData[, factorlats := round(lats/30)*month]
rasteredData[, winter := 0]
rasteredData[lats > 30 & month %in% c(10, 11, 12, 1, 2, 3), winter := 1]
rasteredData[lats < -30 & month %in% c(4, 5, 6, 7, 8, 9), winter := 1]
rasteredData[,winteranomaly := 0]
rasteredData[month %in% c(9, 10, 3, 4), winteranomaly := tempanomaly]

#tsData = ts(rasteredData, start=c(min(rasteredData$date)), end=c(max(rasteredData$date)), frequency=12)
library("plm")
fixed = plm(xco2 ~ ma_12_avg + winteranomaly + factor(winter), data=rasteredData, index=c("ID", "date"))


merger = rasteredData[!is.na(ma_12_avg) & !is.na(xco2) & !is.na(winteranomaly), c("ID", "date"), with=FALSE]
merger = cbind(merger, fixed_res = fixed$residuals)
fittedData = merge(rasteredData, merger, by=c("ID", "date"), all.x=T)
fittedData[,xco2_fitted := xco2 - fixed_res]

# ----------------------------------------------
# STL + APPROX
# ----------------------------------------------
# library("plm")
# fittedvalues = NULL
# lengthID = length(unique(rasteredData$ID))
# for(i in unique(rasteredData$ID)){
#     #print(i)
#     #i = 1300 ##NORMAL CASE
#     #i = 1200
#     #i = 43573 ##NA CASE
#     #i = 20248 ##EXTREME CASE
#     #i = 46000 ##WELL PERFORMER
#     lmData = rasteredData[ID == i, c("ID", "date", "xco2"), with=FALSE]
#     #tsData = ts(lmData, start=c(min(lmData$date)), end=c(max(lmData$date)), frequency=12)
#     # arima = tryCatch({arima(lmData[,5], order = c(0, 0, 1), seasonal=c(0,1,2), xreg=lmData[,7:8])},
#     #                  error=function(cond) {
#     #                      return(NULL)
#     #               })
#     tsData = ts(lmData$xco2, start=c(min(lmData$date)), end=c(max(lmData$date)), frequency=12)
#     fit = tryCatch({stl(tsData, s.window="period", na.action = na.approx)},
#                    error=function(cond) {
#                        return(NULL)
#                    })
#     if(!is.null(fit)){
#         tsTmp = cbind(xco2 = tsData, xco2_fitted = fit$time.series[,2])
#         lmData = cbind(lmData, xco2_fitted = c(tsTmp[,2]))
#         fittedvalues = rbind(fittedvalues, lmData)
#         rm(tsTmp)
#         rm(lmData)
#     }
#     print(paste(lengthID, "ID's left."))
#     lengthID = lengthID - 1
# }
# fittedData = merge(rasteredData, fittedvalues[, c("ID", "date", "xco2_fitted"), with=FALSE], by=c("ID", "date"), all.x = TRUE)
# ----------------------------------------------

# library("forecast")
# fittedvalues = NULL
# lengthID = length(unique(rasteredData$ID))
# for(i in unique(rasteredData$ID)){
#     #print(i)
#     #i = 26642 ##NORMAL CASE
#     #i = 43573 ##NA CASE
#     #i = 20248 ##EXTREME CASE
#     #i = 46000 ##WELL PERFORMER
#     lmData = rasteredData[ID == i]
#     #tsData = ts(lmData, start=c(min(lmData$date)), end=c(max(lmData$date)), frequency=12)
#     # arima = tryCatch({arima(lmData[,5], order = c(0, 0, 1), seasonal=c(0,1,2), xreg=lmData[,7:8])},
#     #                  error=function(cond) {
#     #                      return(NULL)
#     #               })
#     arima = tryCatch({auto.arima(lmData[,5], xreg=lmData[,c(7:8, 10)], D=1, d=0)},
#                      error=function(cond) {
#                          return(NULL)
#                      })
#     
#     
#     if(!is.null(arima)){
#         lmData = cbind(lmData, res = arima$residuals[1:nrow(lmData)])
#     }
#     lmData[, fitted := lmData$xco2 - lmData$res]
#     
#     #tsData = ts(lmData$xco2, start=c(min(lmData$date)), end=c(max(lmData$date)), frequency=12)
#     #fit <- stl(tsData, s.window = "period", na.action=na.approx)
#     
#     # lmModel = tryCatch({lm(xco2 ~ tempanomaly + ma_12_avg + factor(month), data=lmData)},
#     #          error=function(cond) {
#     #              return(NULL)
#     #          })
#     # if(!is.null(lmModel)){
#     #     fittedmodel = c(fittedmodel, lmModel$fitted.values)
#     # }
#     fittedvalues = c(fittedvalues, lmData$fitted)
#     print(paste(lengthID, "ID's left."))
#     lengthID = lengthID - 1
# }

#lmmodel  = lm(xco2 ~ tempanomaly + ma_12_avg +  month + ID, data=rasteredData)
#merger = rasteredData[!is.na(tempanomaly) & !is.na(xco2) & !is.na(ma_12_avg) & !is.na(month), c("ID", "date"), with=FALSE]
#merger = cbind(merger, xco2_fitted = fittedvalues)
#fittedData = merge(rasteredData, merger, by=c("ID", "date"), all.x=T)

#fittedData = cbind(rasteredData, xco2_fitted = fittedvalues)

    
fittedData = fittedData[order(lats, lons, date)]

saveData(fittedData, file.path(rasterPath, "fitted_2deg.rData"))

fittedData[, avg_constant :=mean(average), by=ID]

fittedData[, dist := xco2_fitted - avg_constant]
# ----------------------------------------------


# ----------------------------------------------
# SET DUMMY = 1 IF POINT IS MINIMUM DISTANCE TO MEAN TO PREVEOUS PERIODS
# ----------------------------------------------
fittedData[!is.na(dist), cummin := lapply(.SD, cummin), by = .(ID), .SDcols = c("dist")]
fittedData$mindistance = 0
fittedData[cummin == dist, mindistance :=1]


#rasteredData = rasteredData[, -c("changesum"), with=F]

changeFun = function(x) (x - shift(x))
fittedData[!is.na(cummin) & mindistance==1, distc1y := lapply(.SD, changeFun), by = ID, .SDcols = c("cummin")]

fittedData[mindistance==1, mincount := distc1y]
fittedData[, changesum := sum(mincount, na.rm = TRUE), by=ID]
# ----------------------------------------------


# ----------------------------------------------
# CALCULATE PERFORMANCE
# ----------------------------------------------
performanceData = unique(fittedData[, .(lons, lats, changesum), by=ID])
#performanceData = fittedData[date == "Jän 2015", .(lons, lats, mincount), by=ID]
performanceData[changesum == 0, changesum := NA]

# performanceData[, `:=`(lats = lats+0.5,
#                        lons = lons+0.5)]

# ----------------------------------------------

# ----------------------------------------------
# PLOT PERFORMANCE ON WORLD MAP
# ----------------------------------------------
wr <- map_data("world", col = 1:10, wrap=c(-180,180) )
# Prepare a map of World
wrmap <- ggplot(wr, aes(x = long, y = lat, group = group)) +
    geom_polygon(fill = "white", colour = "black") +
    #geom_point(data=performanceData, inherit.aes=FALSE,aes(x=lons, y=lats, color=changesum),size=0.5) +
    geom_tile(data=performanceData, aes(x=lons, y=lats, fill=changesum, group=ID), alpha=0.8) + 
    #scale_fill_continuous(na.value = 'white')+
    #scale_color_gradient(limits = c(-30, +30), low = "blue", high = "red") +
    scale_fill_gradient2(na.value = 'white', low = "#3794bf", mid = "#FFFFFF", high = "#df8640")+
    theme_bw() + 
    #sc +
    coord_equal()+
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12))+
    labs(x="Longitude",y="Latitude")+
    coord_cartesian(xlim = c(-180, 180))


wrmap

plotFolder = file.path("./sandbox/daytona/plots")
ggsave(file.path(plotFolder, "world_change_plot_lm.png"), width = 15, height = 8)
# ----------------------------------------------

