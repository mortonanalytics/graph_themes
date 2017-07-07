#plot uses time series data and a forecast to simple, clean plot

library(ggplot2)
library(tidyr)
library(dplyr)
library(forecast)

df <- data_frame(year = 1821:1934, count = as.numeric(datasets::lynx), group = "estimate")
df <- df %>% filter(year >= 1900)
df.f <- data_frame(year = 1935:1944, count = summary(forecast(ts(df$count)))$`Point Forecast`, group = "prediction")
df2 <- df %>% bind_rows(df.f)

max.est.year <- df2 %>% filter(group == "estimate") %>% filter(year == max(year)) %>% select(year)
max.est.count <- df2 %>% filter(group == "estimate") %>% filter(year == max(year)) %>% select(count)

###ggplot

ggplot(df2, aes( x = year, y = count)) +
  geom_ribbon(data = df2 %>% filter(group == "estimate"),
              aes( ymin = count - 1000, ymax = count + 1000), 
              fill = "lightblue", 
              alpha = .5)+
  geom_path(aes(linetype = group),color = "#82ac82", lwd = 1.2) +
  geom_vline(xintercept = max.est.year$year, linetype = 2) +
  geom_text(aes(max.est.year$year,max.est.year$year, label = max.est.year$year, vjust = 6))+
  annotate( geom = "text", 
            x = max.est.year$year + 2, 
            y = max.est.count$count + (.2 * max.est.count$count), 
            label = format(max.est.count$count, big.mark = ","),
            colour = "#82ac82", 
            fontface = 2,
            size = 6) +
  theme(
    aspect.ratio = .25,
    axis.title = element_blank(),
    axis.text = element_text(color = "darkgrey", size = 10),
    axis.ticks = element_line(color = "darkgrey"),
    plot.background = element_blank(),
    panel.background = element_blank(), 
    ##plot.background = element_rect(fill = "#f2f2f2"), ##optional backgroup
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.position = "top"
  )
  
