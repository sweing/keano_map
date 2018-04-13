# ----------------------------------------------
# BASE
# ----------------------------------------------
rm(list=ls())
source("./trunk/base/init.r", chdir=TRUE)
# ----------------------------------------------

dataPath = file.path(folders$ocoData, "daily")
dataFiles = list.files(dataPath, pattern = "\\.rData$")
dataFiles = dataFiles[startsWith(dataFiles, "oco2_LtCO2_")]

# ----------------------------------------------
# CONFIG
# ----------------------------------------------
macroResolution = 2
rasterResolution = 0.2
operations = c("median", "mean")
# ----------------------------------------------

# ----------------------------------------------
# CALCULATE ANOMALIES
# ----------------------------------------------
finalData = NULL
for(operation in operations){
    if (operation == "mean")
        operationFunction = mean
    if (operation == "median")
        operationFunction = median
    
    for(dataFile in dataFiles){
        #dataFile = "oco2_LtCO2_161230_B8100r_171007005500s.rData"
        print(dataFile)
        tmp = loadData(file.path(dataPath, dataFile))
        #DROP ANTARCTICA
        #tmp = tmp[!is.na(iso3) & iso3 != "ATA"]
        tmp$rasterLats = rasterResolution*round(tmp$lats/rasterResolution)
        tmp$rasterLons = rasterResolution*round(tmp$lons/rasterResolution)
        tmp$date = as.Date(tmp$time,format="%Y-%m-%d")
        tmp[, nRaster := .N, by=c("rasterLats", "rasterLons", "iso3", "date")]
        #tmp[, nIso := .N, by=c("iso3", "date")]
        #tmp = tmp[nRaster >= 10 & nIso >= 30]
        tmp = tmp[nRaster >= 10]
        #tmp[rasterLats > 0 & rasterLons < -40, hemi := "NAM"]
        #tmp[rasterLats < 0 & rasterLons < -40, hemi := "SAM"]
        
        #tmp[rasterLats > 20 & rasterLons > -40 & rasterLons < 90, hemi := "EUR"]
        #tmp[rasterLats < 20 & rasterLons > -40 & rasterLons < 90, hemi := "AFR"]
        
        #tmp[rasterLats > 0 & rasterLons > 90, hemi := "NAS"]
        #tmp[rasterLats < 0 & rasterLons > 90, hemi := "SAS"]
        
        #tmp[iso3 %in% c("CAN", "RUS", "ISL", "NOR", "SWE", "FIN", "GRL", "FRO"), hemi := "NOR"]
        #tmp[rasterLats > 60, hemi := "NOR"]
        
        tmp[, macroRasterLats := macroResolution*round(tmp$lats/macroResolution)]
        tmp[, macroRasterLons := macroResolution*round(tmp$lons/macroResolution)]
        
        #tmp = tmp[, anomaly := xco2 - operationFunction(xco2), by = "iso3"]
        tmp = tmp[, anomaly := xco2 - operationFunction(xco2), by = c("macroRasterLats", "macroRasterLons")]
        #tmp = tmp[, anomaly := xco2 - operationFunction(xco2)]
        tmp = tmp[!is.na(iso3)]
        tmp = tmp[, .(anomaly = mean(anomaly)), by=.(rasterLats, rasterLons, date, iso3)]
        tmp[, anomaly := removeOutliers(anomaly)]
        finalData = rbind(finalData, tmp)
        rm(tmp)
    }
    print(paste0("SAVING AS '",  paste0("rasterAnomalies", "_m", macroResolution, "_r", rasterResolution, "_", operation, ".rData"), "'"))
    saveData(finalData, file.path(dataPath, paste0("rasterAnomalies", "_m", macroResolution, "_r", rasterResolution, "_", operation, ".rData")))
}
# ----------------------------------------------
