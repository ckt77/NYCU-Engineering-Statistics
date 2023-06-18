population.mean <- 50
population.sd <- 10
num.sample <- 20
t.star <- qt(0.95, df = num.sample - 1)

sample.mean <- c()
sample.sd <- c()
lower.bound <- c()
upper.bound <- c()
number <- c(1:100)
color <- c()
B <- c()

for (i in 1:100){
  sample <- rnorm(num.sample, population.mean, population.sd)
  sample.sd[i] <- sd(sample)
  sample.mean[i] <- mean(sample)
  B[i] <- t.star * sample.sd[i] / sqrt(num.sample)
  lower.bound[i] <- sample.mean[i] - B[i]
  upper.bound[i] <- sample.mean[i] + B[i]
  if (lower.bound[i] <= population.mean && population.mean <= upper.bound[i])
    color[i] = "black"
  else
    color[i] = "red"
}

data <- data.frame(number, sample.sd, sample.mean, lower.bound, upper.bound, B, color)

library(ggplot2)  
ggplot(data = data)+
  geom_errorbar(aes(x = number, ymin = lower.bound, 
                    ymax = upper.bound), color = color)+
  geom_point(aes(number, sample.mean), shape = 21, size = 2.5, 
             color = color)+
  geom_hline(yintercept = 50, linetype = 2, color = "red")+
  labs(subtitle = "Understanding CL",
       x = "Number of sample",
       y = "Confidence Intervals")
