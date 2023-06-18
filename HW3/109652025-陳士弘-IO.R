install.packages("rio")
library(rio)
getwd()
setwd("C:/R/data")
rio_xlsx <- import("weatherdata.xlsx")
str(rio_xlsx)
head(rio_xlsx)

rain <- rio_xlsx$Rain
maxRain <- max(rain) # 9.8
cat("最大單筆雨量紀錄值是 ", maxRain, "mm")
index <- which(rain == maxRain) # 9736
cat("發生的日期時間為", rio_xlsx$Date[index]) # 07/04/20 19:15:00
total <- sum(rain == maxRain)
cat("共計", total, "筆")

cat("The data number:   ", total, file = "Output.txt", 
    sep = " ", "\n")
cat("The maximum rainfall per 5 min.:   ", maxRain, "mm", 
    file = "Output.txt", sep = " ", "\n", append = TRUE)
cat("The date and time: ", rio_xlsx$Date[index],
    file = "Output.txt", sep = " ", "\n", append = TRUE)
