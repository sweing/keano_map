# ----------------------------------------------
# BASE
# ----------------------------------------------
rm(list=ls())
source("./trunk/base/init.r", chdir=TRUE)
# ----------------------------------------------

library("httr")
library("ncdf4")  
library("sp")
library("rworldmap")

rawPath = folders$rawData
savePath = file.path(folders$ocoData, "daily")

# ----------------------------------------------
# DOWNLOAD MOST RECENT DATA
# ----------------------------------------------
txtFiles = list.files(rawPath, pattern = "\\.txt$")

downloadList = NULL
for(txtFile in txtFiles){
    #txtFile = "OCO2_L2_Lite_FP_V8r_links_20171028_145113.txt"
    tmp = as.data.table(read.csv(file.path(rawPath, txtFile), header=FALSE, stringsAsFactors = FALSE))
    tmp[endsWith(tmp$V1, ".nc4"), nc4 := V1][, V1 := NULL]
    tmp = tmp[!is.na(nc4)]
    downloadList = rbind(downloadList, tmp)
    rm(tmp)
}

downloadList = downloadList[!(duplicated(downloadList)|duplicated(downloadList, fromLast=TRUE))]

countriesSP = getMap(resolution='high')

for(url in downloadList$nc4){
    nc4Name = sub('.*\\/', '', url)
    if(file.exists(file.path(savePath, gsub(".nc4", ".rData", nc4Name))))
        next
    
    print(paste(gsub(".nc4", ".rData", nc4Name), "does not exist. Downloading."))
    
    GET(url,
        write_disk(file.path(folders$tmp, nc4Name), overwrite=TRUE),
        authenticate(authUser, authPassword))
    #METHOD 2:
    #download.file(url, destfile = file.path(folders$tmp, nc4Name), method="wget", extra=paste0("--user=", authUser, " --password=", authPassword))
    
    print("Downloading completed. Processing.")
    
    
    prec = nc_open(file.path(folders$tmp, nc4Name))
    file.remove(file.path(folders$tmp, nc4Name))
    
    lons = ncvar_get(prec,"longitude") 
    lats = ncvar_get(prec,"latitude")  
    time = ncvar_get(prec,"time") 
    xco2 = ncvar_get(prec,"xco2")
    
    tmp = as.data.table(cbind(lats, lons, time , xco2))
    tmp$time = as.POSIXct(tmp$time, origin = "1970-01-01 00:00:00")
    
    dat <- data.frame(long = tmp$lons,
                      lat = tmp$lats)
    coordinates(dat) <- ~ long + lat
    proj4string(dat) <- proj4string(countriesSP)
    geoTmp = sp::over(dat, countriesSP)
    iso3 = as.character(geoTmp$ISO3)
    
    tmp = cbind(tmp, iso3=iso3)
    
    print("Processing completed. Saving.")
    saveData(tmp, file=file.path(savePath, gsub(".nc4", ".rData", nc4Name)))
    
    rm(list = c("tmp", "prec", "lons", "lats", "time", "xco2", "dat"))
    gc()

}
# ----------------------------------------------
