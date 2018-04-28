# ----------------------------------------------
# LOAD AND PREPARE DATA
# ----------------------------------------------
#rm(list=ls())
# dataPath = "./sandbox/daytona/tmp/data/oco2"
# load(file.path(dataPath, "rasterWorldData.rData"))
# finalData = data[date > 2015.5]
# 
# performanceData = finalData[, .(lons=mean(lons), lats=mean(lats), changesum_constant=mean(changesum_constant, na.rm=TRUE)), by=id]
# performanceData[changesum_constant == 0, changesum_constant := NA]

# ----------------------------------------------

# ----------------------------------------------
# BASE
# ----------------------------------------------
#source("./sandbox/daytona/trunk/shiny/init.r", chdir=TRUE)
#source("./init.r")
# ----------------------------------------------



# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
    
    # Application title
    headerPanel("CO2 performance"),
    
    #sidebarPanel(verbatimTextOutput("Click_text")),
    #sidebarPanel(dataTableOutput('table')),
    sidebarPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Plot", plotOutput("plot2")),
                    tabPanel("Forecast", plotOutput("forecastplot")),
                    tabPanel("Data", dataTableOutput('table')))
        
        ),
    #sidebarPanel(plotlyOutput("plotly2")),
    
    mainPanel(
        tags$style(type = "text/css", "#mymap {height: calc(80vh - 80px) !important;}"),
        leafletOutput("mymap"))
))
