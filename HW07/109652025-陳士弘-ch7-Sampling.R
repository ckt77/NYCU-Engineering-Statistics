library(rio)
setwd("C:/R/Engineering_Statistics/Ch7.R.Sampling/codes")
weatherdata <- import("weatherdata.xlsx")

library(MASS)
fit.lnorm <- fitdistr(weatherdata$Temperature, densfun = "lognormal")
mean <- fit.lnorm$estimate[1]
sd <- fit.lnorm$estimate[2]
x <- seq(10, 40, 0.1)
y <- dlnorm(x, meanlog = mean, sdlog = sd)
weatherdata.lnorm <- data.frame(x = x, y = y, label = "lnorm")

density <- density(weatherdata$Temperature)
weatherdata.density <- data.frame(x = density$x
                           , y = density$y
                           , label = "density")

data.plot <- rbind.data.frame(weatherdata.lnorm, weatherdata.density)

library(ggplot2)
p1 <- ggplot(data = weatherdata, aes(Temperature, after_stat(density))) +
  labs(title = "June 01-Sept. 30 2020 Temperature",
       x = "Temperature",
       y = "Density")+
  scale_y_continuous(breaks = c(0, 0.03, 0.06, 0.09))+
  geom_histogram(binwidth = 0.2,
                 fill = "black",
                 color = "white",
                 linewidth = 0.1,
                 alpha = 0.8)+
  geom_line(data = data.plot
            , aes(x, y, color = label) # define color by label 
            , linewidth = 0.6)+
  theme_bw()

i <- 0
n1 <- 10
nt <- 1000
np <- length(weatherdata$Temperature)
temperature.mean.10 <- 0

for (i in 1:nt) {
  index <- sample(1:np, n1)
  result <- weatherdata$Temperature[index]
  temperature.mean.10[i] <- mean(result)
}
temperature.mean.10 <- as.data.frame(temperature.mean.10)

i <- 0
n2 <- 50
nt <- 1000
np <- length(weatherdata$Temperature)
temperature.mean.50 <- 0

for (i in 1:nt) {
  index <- sample(1:np, n2)
  result <- weatherdata$Temperature[index]
  temperature.mean.50[i] <- mean(result)
}
temperature.mean.50 <- as.data.frame(temperature.mean.50)

i <- 0
n3 <- 100
nt <- 1000
np <- length(weatherdata$Temperature)
temperature.mean.100 <- 0

for (i in 1:nt) {
  index <- sample(1:np, n3)
  result <- weatherdata$Temperature[index]
  temperature.mean.100[i] <- mean(result)
}

temperature.mean.100 <- as.data.frame(temperature.mean.100)

p2 <- ggplot(data = temperature.mean.10, aes(temperature.mean.10))+
  geom_histogram(bins = 30)+
  coord_cartesian(xlim = c(15,35))+
  labs(x = "sample mean, random sampling n = 10, 1000 times")

p3 <- ggplot(data = temperature.mean.50, aes(temperature.mean.50))+
  geom_histogram(bins = 30)+
  coord_cartesian(xlim = c(15,35))+
  labs(x = "sample mean, random sampling n = 50, 1000 times")

p4 <- ggplot(data = temperature.mean.100, aes(temperature.mean.100))+
  geom_histogram(bins = 30)+
  coord_cartesian(xlim = c(15,35))+
  labs(x = "sample mean, random sampling n = 100, 1000 times")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 4)
