d <- c(37.0,37.5,38.1,40.0,40.2,40.8,41.0,
       42.0,43.1,43.9,44.1,44.6,45.0,46.1,
       47.0,50.2,55.0,56.0,57.0,58.0,62.0,
       64.3,68.8,70.1,74.5)
d.sort <- sort(d)

n <- length(d)
i <- 1:n
xi <- (i-0.5)/n

x.norm.quantile <- qnorm(xi) # mean = 0, sd = 1
x.norm.quantile
data <- data.frame(x = x.norm.quantile, y = d.sort)
y <- quantile(d.sort, c(0.25, 0.75), type = 5)
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
ypred <- slope * x[1]
int <- y[1] - slope * x[1]

library(ggplot2)
p2 <- ggplot()+
  aes(sample = d.sort)+
  stat_qq(distribution = qnorm)+
  stat_qq_line(line.p = c(0.25, 0.75), col = "blue")+
  labs(title = "QQplot",
       x = "Theoretical Normal Distribution Quantile",
       y = "Observed Data")

data <- data.frame(x =d)
density <- density(d) 
data.density <- data.frame(x = density$x, y = density$y)

library(MASS)
fit.weibull <- fitdistr(d,"weibull")
shape <- fit.weibull$estimate[1] 
scale <- fit.weibull$estimate[2] 
x <- seq(25,100,0.1)
d.weibull <- dweibull(x, shape = shape, scale = scale)
fit.normal <- fitdistr(d,"normal")
mean <- fit.normal$estimate[1]
sd <- fit.normal$estimate[2]
d.normal <- dnorm(x, mean = mean, sd = sd)
fit <- data.frame(x = x, wei = d.weibull, nor = d.normal)

p1 <- ggplot(data = data ,aes(x,..density..))+
  labs(title = "Fitting distribution",
       x = "Material strength",
       y = "Density")+
  scale_x_continuous(breaks = seq(25,100,25))+
  geom_line(data = data.density, aes(x,y),
            color = "black",
            size =1.0)+
  geom_line(data = fit, aes(x,wei),
            color = "red",
            size =1.0)+
  geom_line(data = fit, aes(x,nor),
            color = "black",
            size =1.0,
            linetype = 2)+
  theme_bw()

library(gridExtra)
grid.arrange(p1,p2,nrow = 2)