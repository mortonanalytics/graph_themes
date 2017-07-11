library(dygraphs)
     #env <<- environment()  # can use globalenv(), parent.frame(), etc
     
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
       dySeries( "f.cast$mean", 
                label = "Predicted", 
                strokePattern = "dashed") %>%
       dyAxis("x", axisLineColor = "white", 
              axisLabelColor = "gray", 
              gridLineColor = "lightgray") %>%
       dyAxis("y", axisLineColor = "white", 
              axisLabelColor = "gray", 
              gridLineColor = "lightgray") %>%
       dyLegend(show = "follow") %>%
       dyCrosshair(direction = "vertical") %>%
       dyEvent("1934-01-01", "1934", labelLoc = "bottom") %>%
       dyOptions(colors = sgPalette1)
    p
