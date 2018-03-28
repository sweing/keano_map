source("./timeseries.r")
source("./data.r")
source("./env.r")
source("./helpers.r")
source("./globals.r")
source("../../auth.txt")

requiredPackages = c("data.table", "ggplot2", "zoo", "forecast")

if (javaInstalled)
    requiredPackages = c(requiredPackages, "xlsx")

loadPackages(requiredPackages)

