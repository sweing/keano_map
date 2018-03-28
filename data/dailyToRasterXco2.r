# ----------------------------------------------
# BASE
# ----------------------------------------------
rm(list=ls())
source("./sandbox/daytona/trunk/base/init.r", chdir=TRUE)
# ----------------------------------------------

dataPath = "../daytona/data/oco2/v8rData/daily"
dataFiles = list.files(dataPath, pattern = "\\.rData$")
dataFiles = dataFiles[startsWith(dataFiles, "oco2_LtCO2_")]
finalData = NULL

    
for(dataFile in dataFiles){
    #dataFile = "oco2_LtCO2_161230_B8100r_171007005500s.rData"
    print(dataFile)
    tmp = loadData(file.path(dataPath, dataFile))
    tmp = tmp[!is.na(iso3)]
    tmp$rasterLats = floor(tmp$lats) + 0.5
    tmp$rasterLons = floor(tmp$lons) + 0.5
    tmp$date = as.Date(tmp$time,format="%Y-%m-%d")
    tmp[, nRaster := .N, by=c("rasterLats", "rasterLons", "iso3", "date")]
    tmp[, nIso := .N, by=c("iso3", "date")]
    tmp = tmp[nRaster >= 10 & nIso >= 50]
    tmp = tmp[, .(xco2 = mean(xco2)), by=.(rasterLats, rasterLons, date, iso3)]
    finalData = rbind(finalData, tmp)
    rm(tmp)
}
print("SAVING")
saveData(finalData, file.path(dataPath, paste0("rasterXco2.rData")))



