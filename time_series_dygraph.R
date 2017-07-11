library(dygraphs)
     #env <<- environment()  # can use globalenv(), parent.frame(), etc
     sgPalette1 <- c("#cae4db", "#dcae1d", "#00303f", "#7a9d96")

     df <- data_frame(year = 1821:1934, count = as.numeric(datasets::lynx), group = "estimate")
     df <- df %>% filter(year >= 1900)
     df.f <- data_frame(year = 1935:1944, count = summary(forecast(ts(df$count)))$`Point Forecast`, group = "prediction")
     df2 <- df %>% bind_rows(df.f) %>% spread(group, count)
     
     f.cast <- forecast(datasets::lynx, h = 5)
     
     df2 <- cbind(datasets::lynx,  f.cast$mean)
     df2 <- window(df2,1900)
     
     
    p <- dygraph(df2) %>%
       dySeries("datasets::lynx", 
           label = "Actual",
           strokeWidth = 2) %>%
  
        dySeries(c("f.cast$lower.80%", "f.cast$mean", "f.cast$upper.80%"), 
           label = "Predicted", 
           strokePattern = "dashed") %>%
  
        dyAxis("x", axisLineColor = "white", 
         axisLabelColor = "gray", 
         gridLineColor = "lightgray") %>%
  
        dyAxis("y", axisLineColor = "white", 
         axisLabelColor = "gray", 
         gridLineColor = "lightgray") %>%
  
        dyLegend(show = "follow", hideOnMouseOut = FALSE, labelsSeparateLines = TRUE, width = 100) %>%
  
        dyCrosshair(direction = "vertical") %>%
  
        dyEvent("1935-01-01", "1935", labelLoc = "bottom") %>%
  
        dyOptions(colors = sgPalette1,stepPlot = TRUE)
    p
