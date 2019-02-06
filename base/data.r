removeOutliers <-  function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(.1, .9), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
}

roundUp <- function(from,to) {
    ceiling(from/to)*to
}

absolutChange <- function(x) {
    x - shift(x)
}

makeGrid <- function(mergeResolution, gridResolution = 1) {
    grd = as.data.table(expand.grid(rasterLats =  seq(from = -90, to = 90, by = mergeResolution), rasterLons = seq(from = -180, to = 180, by = mergeResolution)))
    
    grd$rasterLats = mergeResolution*round(grd$rasterLats/mergeResolution)
    grd$rasterLons = mergeResolution*round(grd$rasterLons/mergeResolution)
    
    if(!is.null(gridResolution)){
        grd[, `:=` (idLats = gridResolution*round(rasterLats/gridResolution),
                    idLons = gridResolution*round(rasterLons/gridResolution))]
        grd = grd[order(idLats, idLons)]
        grd[, id := .GRP, by = .(idLats, idLons)]
        grd = grd[order(id)]
    }
    return(grd)
}
    
