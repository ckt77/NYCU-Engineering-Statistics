# (1)
p <- 0.1
n.typhoon <- 20
prob <- (1 - p) ^ n.typhoon
cat("P(n.strike == 0) = ", prob, " is a theoretical probability.\n")

# (2)
set.seed(4)
sample.frequency <- vector()
prob.list <- vector()
prob.not.strike <- function(n){
  n.typhoon <- 20
  m <- 0
  for (i in 1:n){
    not.strike = TRUE
    for(j in 1:n.typhoon){
      num <- sample(0:99, 1)
      if (num < 10){
        not.strike = FALSE
        break
      }
    }
    
    if (not.strike)
      m <- m + 1
  }
  
  ans <- m / n
  cat(m, "of", n, "are results of number of non-strike typhoon.\n")
  cat(ans, " is computed probability.\n")
  cat("Sample number is ", n, "\n")
  return(ans)
}

sample.frequency <- c(sample.frequency, 8)
prob.list <- c(prob.list, prob.not.strike(8))

# (3)
set.seed(NULL)
for (i in 4:15){
  sample.frequency <- c(sample.frequency, 2 ^ i)
  prob.list <- c(prob.list, prob.not.strike(2 ^ i))
}

title <- "Probability of Typhoon Strike Taiwan"
data <- data.frame(x = sample.frequency, y = prob.list)
data.line <- data.frame(x = c(0, 2 ^ 15), y = c(prob, prob))

library(ggplot2)
ggplot(data = data, aes(x,y))+
  geom_point()+
  labs(title = title,
       x = "Number of Random Sampling",
       y = "Probability of n.strike = 0")+
  geom_line(data = data.line, aes(x,y),
            linetype = 2,
            color = "black",
            linewidth = 1.)+
  scale_x_continuous(breaks = seq(0, 30000, by = 10000))+
  scale_y_continuous(breaks = seq(0.05, 0.125, by = 0.025))
  theme_bw()
