# ----------------------------------------------
# BASE
# ----------------------------------------------
rm(list=ls())
setwd("/mnt/data/stefan/Projects/GitHub/oco_daytona")
source("./trunk/base/init.r", chdir=TRUE)
loadPackages(c("shiny", "leaflet", "plotly"))
# ----------------------------------------------

# ----------------------------------------------
# BASE
# ----------------------------------------------
#
#source("./init.r")
#rm(list=ls())
#source("./sandbox/daytona/trunk/shiny/init.r", chdir=TRUE)
# ----------------------------------------------

# ----------------------------------------------
# CONFIG
# ----------------------------------------------
macroResolution = 2
rasterResolution = 0.2
operation = "median"
selectedIso3 = "AUT"
nObservations = 12
# ----------------------------------------------

# ----------------------------------------------
# LOAD, PREPARE DATA
# ----------------------------------------------
fileName = paste0("rasterAnomalies", "_m", macroResolution, "_r", rasterResolution, "_", operation, ".rData")
dataPath = file.path(folders$ocoData, "daily")
baseData = loadData(file.path(dataPath, fileName))

baseData = baseData[!is.na(anomaly)]

baseData = merge(baseData, makeGrid(rasterResolution), all.x = TRUE, by = c("rasterLats", "rasterLons"))
baseData = baseData[order(date)]
# ----------------------------------------------

# ----------------------------------------------
# CREATE CUMULATIVE MEAN OF CELLS
# ----------------------------------------------
baseData[, tmp := anomaly-shift(anomaly, n = 1L, type="lag", fill = 0), by=c("id", "rasterLats", "rasterLons")]

baseData[anomaly != tmp, skip := 1:.N, by=c("id")]
baseData[, skip := na.locf(skip, na.rm=FALSE), by=c("id")]
baseData[is.na(skip), skip := 0]

baseData[, cummean := cumsum(tmp)/(seq_along(tmp)-skip), by=c("id")][, `:=` (skip = NULL)]
baseData[, nDaily := 1:.N, by=c("id", "date")]
baseData[, NDaily := .N, by=c("id", "date")]

#SELECT LAST CUMULATIVE MEAN EVERY DAY AS DAY VALUE
baseData = baseData[nDaily == NDaily]
# ----------------------------------------------

# ----------------------------------------------
# FINALIZE DATA
# ----------------------------------------------
baseData = baseData[iso3 %in% selectedIso3]
baseData[!is.na(anomaly), n := .N, by=id]

#SELECT REGIONS WITH MORE THAN nObservations
plotData = baseData[n >= nObservations & !is.na(anomaly)]
plotData$rasterLats = sprintf("%.1f", plotData$rasterLats)
plotData$rasterLons = sprintf("%.1f", plotData$rasterLons)
plotData$id <- paste0("ID: ", plotData$id, " (", plotData$idLats, ", ", plotData$idLons, ")")[match(plotData$id, plotData$id)]

#MAKE CUMULATIVE MINIMA, QUARTERLY
plotData$quarter = quarter(plotData$date)
plotData$year = year(plotData$date)
plotData[as.Date(date, format= "%y-%m-%d") == max(as.Date(date, format= "%y-%m-%d")), lastDay := 1, by=c("quarter", "id", "year")]
plotData = setDT(plotData)[ ,.SD[which.max(as.Date(date, format= "%y-%m-%d"))],  by = .(quarter, id, year)]
plotData[!is.na(cummean), cummin := cummin(cummean), by = "id"]
plotData[cummean != cummin, cummin := NA]
# ----------------------------------------------

# if(!exists("finalData")){
#     dataPath = "./sandbox/daytona/tmp/data/oco2"
#     finalData = loadData(file.path(dataPath, "rasterWorldData.rData"))
#     #finalData = data
#     finalData$date = as.yearmon(finalData$date)
#     finalData = finalData[date > 2015.5]
#     performanceData = finalData[, .(lons=mean(lons), lats=mean(lats), changesum_constant=mean(changesum_constant, na.rm=TRUE)), by=id]
#     performanceData[changesum_constant == 0, changesum_constant := NA]
# }
#performanceData[order(changesum_constant), rank := 1:.N]


pal <- colorNumeric(
    palette = colorRampPalette(c("purple4", "plum"))(10) ,
    domain = plotData$cummean,
    n = 3
)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
    output$mymap = renderLeaflet({leaflet(plotData) %>% addTiles() %>%
        # addRectangles(
        #     lng1= ~lons -0.5, lat1= ~lats -0.5,
        #     lng2= ~lons +0.5, lat2= ~lats +0.5,
        #     #fillColor = "red",
        #     #fill = TRUE,
        #     smoothFactor = 0.2,
        #     layerId=~id,
        #     stroke = FALSE,
        #     #weight = 0.5,
        #     #fillOpacity = ~-changesum_constant,
        #     color = ~pal(changesum_constant),
        #     fillOpacity = 0.5,
        #     popupOptions = popupOptions(closeButton = FALSE),
        #     popup = ~paste("Longitude:", lons, "± 0.5°",
        #                    "<br/>Latitude:", lats, "± 0.5°",
        #                    "<br/><br/>XCO2:", as.character(round(changesum_constant, digits = 2)), "ppm",
        #                    "<br/>World rank:", as.character(rank)))
        # addLegend("bottomright", pal = pal, values = ~changesum_constant,
        #           title = "",
        #           bins = 6,
        #           labFormat = labelFormat(transform = function(x) (-1) * x),
        #           opacity = 1
        # ) %>% 
        setView(lng = 15, lat = 37.45, zoom = 3) %>%
       
        addMiniMap("bottomleft") %>%
            addGraticule(group = "Graticule",
                         interval = 1,
                         style = list(color = "#000000", weight = 0.2, opacity = 0.5)) %>%
            addLayersControl(overlayGroups = c("Graticule"),
                             options = layersControlOptions(collapsed = FALSE))%>%
            addMarkers(lat = ~idLats, 
                       lng = ~idLons,
                       layerId = ~id,
                       clusterOptions = markerClusterOptions(),
                       popupOptions = popupOptions(closeButton = FALSE),
                       popup = ~paste("Longitude:", idLons, "± 0.5°",
                                      "<br/>Latitude:", idLats, "± 0.5°",
                                      "<br/><br/>XCO2:", as.character(round(cummean, digits = 2)), "ppm"))%>%
            addRectangles(
                lng1= ~idLons -0.5, lat1= ~idLats -0.5,
                lng2= ~idLons +0.5, lat2= ~idLats +0.5,
                fillColor = ~pal(cummean),
                #fill = TRUE,
                smoothFactor = 0.2,
                #layerId = ~id,
                stroke = FALSE,
                #opacity = 0.5,
                fillOpacity = 0.08
                #weight = 0.5,
                #color = ~pal(changesum_constant)
                #fillOpacity = ~-changesum_constant/5,
                )
        })

    output$mymap2 = renderLeaflet({leaflet() %>% addTiles() %>%
            addRasterImage(WGScoor, colors=pal, opacity=0.6) %>%
            addLegend(pal=pal, values=values(WGScoor), title="Pb concentration")
    
        
        })
    
    observeEvent(input$mymap_marker_click, { # update the location selectInput on map clicks
        click <- input$mymap_marker_click
        #print(click)
        if(is.null(click))
            return()
        text<-paste("Latitude ", click$lat, "Longtitude ", click$lng)
        text2<-paste("You've selected point ", click$id)
        #mymap$clearPopups()
        #mymap$showPopup( click$lat, click$lng, text)
        output$Click_text<-renderText({
            click$id
        })
        
    })
    
    output$table <- renderDataTable({plotData[id == input$mymap_marker_click$id, c("date", "idLons", "idLats", "cummean"), with=FALSE]})
    
    output$forecastplot <-renderPlot({
        autoplot(forcastfc(input$mymap_marker_click$id))
        #49296
    })
    
    output$plot2<-renderPlot({
        ggplot(plotData[id == input$mymap_marker_click$id],aes(x=date,y=cummean,group=id))+
            geom_line()+
            geom_smooth(method = "loess")+
            geom_point(data = plotData[id == input$mymap_marker_click$id], aes(date, cummean), color = "#888888", size=4)+
            theme(axis.text=element_text(size=16),
                        axis.title=element_text(size=16))+
            #theme_bw()+
            geom_hline(yintercept=0, linetype="dashed", color = "red")+
            scale_y_continuous("CO2 in atmosphere, distance to global mean")+
            #geom_smooth(method = "smooth.spline2", se= F)+
            scale_x_yearmon("Time", format="%b %y", breaks=seq(2015.5, 2019, 0.5))+
            geom_label(data = plotData[id == input$mymap_marker_click$id], aes(date, cummean-0.2, label=paste(round(cummean, digits = 3))), position="identity")
        })
    # 
    # output$plotly2 <-renderPlotly({
    #     plot_ly(finalData[id == input$mymap_marker_click$id], x = ~date, y = ~dist_constant, name = 'CO2 in atmosphere, distance to global mean', text= 'CO2 in atmosphere, distance to global mean',  type = 'scatter', mode = 'lines')
    # })
    
    output$plotly2 <-renderPlotly({
        plot_ly(plotData[id == input$mymap_marker_click$id], x = ~as.Date(date)) %>%
        # add_lines(y = ~fitted(loess(dist_constant ~ as.factor(date))),
        #           line = list(color = 'rgba(7, 164, 181, 1)'),
        #           name = "Loess Smoother") %>%
        add_lines(y = ~cummean) %>%
        #add_markers(y = ~dist_constant, text = rownames(mtcars), showlegend = FALSE) %>%
        layout(xaxis = list(title = 'Date'),
               yaxis = list(title = 'CO2 in atmosphere, distance to global mean'),
               legend = list(x = 0.80, y = 0.90))
    })
    
    
    
       
})