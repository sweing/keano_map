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