

library(shiny)
library(caret)

#options(shiny.maxRequestSize=60*1024^2) 
source("load_train_bloodDonation.R")
source("predict_bloodDonation.R")

shinyServer(function(input, output) {
        
        output$volDonated <- renderText({paste0("Volume Donated: ", input$volumeDonated, " c.c.")})
        output$start <- renderText({paste0("First Donation: ", floor((as.Date('2007-02-28')-input$dateRange[1])/30)[[1]]," months ago. \n")})
        output$recent <- renderText({paste0("Most Recent Donation: ",  floor((as.Date('2007-02-28')-input$dateRange[2])/30)[[1]], " months ago. \n")})

        
        Prediction <- eventReactive(input$submitButton, {
                predictDonation(floor((as.Date('2007-02-28')-input$dateRange[1])/30)[[1]],
                                floor((as.Date('2007-02-28')-input$dateRange[2])/30)[[1]],
                                input$volumeDonated, 
                                fit.glm.final, fit.rpart.final, fit.svm.final, fit.rf.final)
                                     })
                
                
        output$prediction <- renderTable({
                Prediction()
        })
})

        
        
        
        
        
        
        
        
        
        
        
        
                # channel <- switch(input$Channel, "1" = "MCOM", "2" = "BCOM", "3" = "FEDFIL", "4" = "ESEND")
        # plotData <- subset(tranTyp.state, APPL_ID==channel)
        
#         output$SelectedString <- renderText({print(
#                 paste0("Selected: ", 
#                        switch(input$Channel, "1" = "Macys.com ", "2" = "Bloomingdales.com ", "3" = "FEDFIL ", "4" = "ESEND "),
#                        switch(input$TranType, "1" = "Booked", "2" = "Shipped", "3" = "Exchanged", "4" = "Canceled", "5"="Returned"),
#                        " Registered Customer Transactions."
#                 ))})
#         
#         output$TranTypeMap <- renderGvis({
#                 
#                 selChannel <- switch(input$Channel, "1" = "MCOM", "2" = "BLCOM", "3" = "FIL", "4" = "ESEND")
#                 selTranType <- switch(input$TranType, "1" = "Booked", "2" = "Shipped", "3" = "Exchanged", "4" = "Canceled", "5"="Returned")
#                 plotData <- subset(tranTyp.state, APPL_ID==selChannel & TRAN_TYP_ID==selTranType , select=c("STATE", "TRAN_TYP_ID","Transaction.Volume"))
#                 
#                 gvisGeoChart(plotData, locationvar="STATE", colorvar="Transaction.Volume", 
#                              options=list(region="US", displayMode="regions", resolution="provinces", height=400, width=1000), 
#                              chartid = "BF2014TransactionTypesByState")
#                 
#         })
#         
#         output$BarplotKnownUnknown <- renderGvis({
#                 
#                 barplotInput <- input$BarplotChannel #it's a vector
#                 
#                 for(i in 1:length(barplotInput)){barplotInput[i] <- switch(barplotInput[i],
#                                                                            "1"="MCOM", "2"="BLCOM", "3"="FIL", "4"="ESEND")}
#                 
#                 plotData1 <- resultDF[resultDF$APPL_ID %in% barplotInput, ]
#                 
#                 gvisColumnChart(plotData1, 
#                                 options=list(height=500, 
#                                              vAxis.viewWindowMode="maximized"),
#                                 chartid = "BARPLOT_BF2014KnownVsUnknownCustomerRevenue")
#         })  
#         
#         output$PDPHeatMap <- renderChart2({
#                 
#                 heat.metrics <- input$PDPHeatmapMetrics #it's a vector
#                 
#                 for(i in 1:length(heat.metrics)){
#                         heat.metrics[i] <- switch(heat.metrics[i],
#                                                   "1"="Clicks", "2"="Add.to.Bag.Sessions", "3"="Orders")
#                 }
#                 
#                 heat.metrics <- c("PDP.Component", heat.metrics)
#                 PDPData <- subset(PDPData, select=heat.metrics)
#                 
#                 # Using Polycharts!
#                 # Bugs: Tooltip line breaks are screwing up
#                 # Rotating axes labels??
#                 PDPmelt = ddply(melt(PDPData), .(variable), transform, heatkey = value/sum(value))
#                 names(PDPmelt) = c("Component", "Metric", "Value", "HeatKey")
#                 PDPmelt$prettyVal <- format(PDPmelt$Value, big.mark=',', scientific=F)
#                 PDPmelt$HeatKey <- round(PDPmelt$HeatKey, digits=3)
#                 
#                 hmap <- rPlot(Component ~ Metric, color = "HeatKey", data = PDPmelt, type = "tile",
#                               tooltip = "#!function(X) {
#                               return X.Component + ' generates ' + X.HeatKey*100 + '% of Total PDP ' + X.Metric + ' ('+ X.Value + ')'
#         }!#"
#                 )  #polycharts
#                 hmap$addParams(height = 480, width = 580)
#                 hmap$guides(reduceXTicks = FALSE)
#                 hmap$guides("{color: {scale: {type: gradient, lower: yellow, upper: red}}}")
#                 hmap$guides(y = list(numticks = length(unique(PDPmelt$Component))))
#                 
#                 return(hmap)
#                 
#                 #  return(hmap)
#         })
