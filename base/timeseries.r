predictQuarterly <- function(data=data, nowIs = NULL, groupVar=groupVar, variable = variable){
    
    #data = copy(distanceData[id == 5844])
    #nowIs = "Jän 2017"
    #groupVar = "id"
    #variable = "dist"
    if(is.null(nowIs))
        nowIs = max(data$date)
    
    df.nowis = data[, c("date", groupVar, variable), with=FALSE]
    df.nowis = df.nowis[date <= nowIs]
    #quarter(as.Date("Jän 2008"))
 
    #get last month of quarter
    #as.yearmon(as.Date(as.yearqtr("2017Q4", format="%YQ%q"), frac=1))
    dateToPredict = as.yearmon(as.Date(as.yearqtr(nowIs, format="%b %Y"), frac=1))
    nowIs = as.yearmon(nowIs)
    diffBetween = as.integer((dateToPredict-nowIs)*12)
    finalData = NULL
    for(g in unique(data[[groupVar]])){
        #g = "wg"
        tmp = df.nowis[get(groupVar) == g]
        
        fit = tryCatch({auto.arima(tmp[[variable]])},
                       error=function(cond) {
                           return(NULL)}
                       
        )
        
        if(!is.null(fit)){
            fcast <- forecast(fit, h=diffBetween)
            fcast.date = as.yearmon(seq(as.Date(nowIs),by="month",length.out=diffBetween+1))[2:(diffBetween+1)]
            tmp.fcast = data.table(date = fcast.date, forecast = fcast$mean[1:diffBetween], id = g, predicted=1)
            setnames(tmp.fcast, c("date", variable, groupVar, "predicted"))
            tmp = rbind(tmp, tmp.fcast, fill=TRUE)
            tmp[is.na(predicted), predicted := 0]
            finalData = rbind(finalData, tmp, fill=TRUE)
        }
        rm(tmp)
    }
    
    
    return(finalData)
}


movingAverage <- function(data=data, by=by, variable = variable, maFactor = NULL){
    #data = rasteredData
    #groupVar = "id"
    #variable = "xco2a"
    
    if(is.null(maFactor))
        maFactor = 12
    
    data[, paste0(variable, "_ma", maFactor) := lapply(.SD, rollapplyr, FUN=mean, list(-((maFactor-1):0)), fill = NA, partial = FALSE),
                 by = by, .SDcols = variable]
    return(data)
}

toIndex = function(data, baseYear) {
    data = merge(data, data[date == baseYear, list(baseValue = value), by=.(iso3, variable)], by=c("iso3", "variable"))
    data[, index := value/baseValue]
    data = data[, list(iso3, date, value=index, variable)]
    #tr[[length(tr) + 1]] = list(name = "index", baseYear = baseYear)
    return (data)
} 
