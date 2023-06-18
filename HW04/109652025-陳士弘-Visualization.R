library(rio)
getwd()
setwd("C:/R/Homework/1017")
xlsx <- import("weatherdata.xlsx")

library(lubridate)
date <- xlsx$Date
dt <- mdy_hms(date)

library(ggplot2)
Sys.setlocale("LC_ALL", "English")

data1 <- data.frame(time = dt
                   , RH = xlsx$RH)

p1 <- ggplot(data = data1, aes(time, RH))+
  geom_point(size = 1, color = "black", shape = 15, alpha = 1)+
  geom_smooth()+
  labs(title = "Relative Humidity June-Oct 2020"
       , x = "Date-Time"
       , y = "RH [%]")

data2 <- data.frame(time = dt
                    , RH = xlsx$Rain)

p2 <- ggplot(data = data2, aes(time, RH))+
  geom_bar(stat = "identity", color = "blue")+
  labs(title = "Precipitation June-Oct 2020"
       , x = "Date-Time"
       , y = "Precipitation [mm]")

library(gridExtra)
grid.arrange(p1,p2,nrow = 2)

