library(ggplot2)

n <- length(cars$dist)
r <- cor(cars$speed,cars$dist,method = "pearson")
print("r ~= 0.8, 速度與煞車距離呈高度正相關。")

model.line <- lm(dist ~ speed, data = cars)
a.line <- as.numeric(model.line$coefficients[1])
b.line <- as.numeric(model.line$coefficients[2])
anova <- anova(model.line)
SSE.line <- anova$`Sum Sq`[2]
SSR.line <- anova$`Sum Sq`[1]
SSTo <- SSE.line + SSR.line
r.square.line <- round(1 - SSE.line / SSTo, 2)
se.line <- round(sqrt(SSE.line / (n - 2)), 2)
pre.x <- seq(0, 30, 0.01)
pre.y <- a.line + b.line * pre.x
pre.line <- data.frame(x = pre.x, y = pre.y)

data <- cars
data$speed <- log(data$speed)
data$dist <- log(data$dist)

model.curve <- lm(dist ~ speed, data = data)
e <- exp(1)
a.curve <- as.numeric(e ^ model.curve$coefficients[1])
b.curve <- as.numeric(model.curve$coefficients[2])
SSE.curve <- 0.0
y.mean <- mean(cars$dist)
for (i in 1:n) {
  y.pre <- a.curve*cars$speed[i] ^ b.curve
  SSE.curve = SSE.curve + (cars$dist[i] - y.pre) ^ 2
}
se.curve <- round(sqrt(SSE.curve / (n - 2)), 2)
r.square.curve <- round(1 - SSE.curve / SSTo, 2)

pre.x <- seq(0, 30, 0.01)
pre.y <- a.curve * (pre.x ^ b.curve)
pre.curve <- data.frame(x = pre.x, y = pre.y)

sub.text <- expression(paste("r"^"2", ":0.65 se:15.38(line) ", "r"^"2",":0.66 se:15.22(Curve)"))

ggplot(data = cars, aes(speed, dist))+
  geom_point()+
  geom_line(data = pre.line, aes(x, y), linetype = 2,
            linewidth = 1)+
  geom_line(data = pre.curve, aes(x, y), linewidth = 1)+
  labs(title = "Scatter Plot: Speed v.s. Distance",
       subtitle = sub.text,
       caption = expression(paste("dist=-17.58+3.93xspeed (dashed line), dist=0.48x", "speed" ^ "1.6")),
       x = "Speed, mph",
       y = "Distance, ft")+
  xlim(0, 30)+
  scale_y_continuous(breaks = c(0, 40, 80, 120))
print("非線性模型更適合描述速度與距離的關係，因其決定係數較線性模型更高、se較線性模型更小。")
