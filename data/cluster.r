# ----------------------------------------------
# BASE
# ----------------------------------------------
rm(list=ls())
source("./trunk/base/init.r", chdir=TRUE)
# ----------------------------------------------
#library(Hmisc)
dataPath = file.path(folders$ocoData, "daily")
baseData = loadData(file.path(dataPath, "median_anomalies.rData"))

baseData[, tmp := anomaly-shift(anomaly, n = 1L, type="lag", fill = 0), by=c("iso3", "rasterLats", "rasterLons")]
baseData[anomaly != tmp, skip := 1:.N, by=c("iso3")]
baseData[, skip := na.locf(skip, na.rm=FALSE), by=c("iso3")]

baseData[is.na(skip), skip := 0]
baseData[, cummean := cumsum(tmp)/(seq_along(tmp)-skip), by=c("iso3")][, `:=` (skip = NULL, tmp = NULL)]

#baseData[, nDaily := 1:.N, by=c("iso3", "date")]
#baseData[, NDaily := .N, by=c("iso3", "date")]
#baseData = baseData[nDaily == NDaily]


library(TSclust)
### The most common use case begins with a set of time series we want to cluster.
### This package includes several example datasets.
### 

dcast(baseData

data(interest.rates)
###transformation of the interest rates
trans.inter.rates <- log(interest.rates[2:215,]) - log(interest.rates[1:214,])

##use the dist function of the proxy package to easily create the dist object
#applying ACF with geometric decaying to each pair of time series
tsdist <- diss( t(trans.inter.rates) , "ACF", p=0.05)

names(tsdist) <- colnames(interest.rates)

#perform hierachical clustering to the dist object
hc <- hclust(tsdist)

#show the results
plot(hc)

mahdist <- diss( t(trans.inter.rates) , "AR.MAH", p=0.05)$p_value

pvalues.clust(mahdist, 0.05)
