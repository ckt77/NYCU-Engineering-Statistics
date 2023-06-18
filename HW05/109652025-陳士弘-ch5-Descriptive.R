data <- data.frame(elasticModulus = c(37.0,37.5,38.1,40.0,40.2,40.8,41.0,
                       42.0,43.1,43.9,44.1,44.6,45.0,46.1,
                       47.0,50.2,55.0,56.0,57.0,58.0,62.0,
                       64.3,68.8,70.1,74.5))

library(ggplot2)
hisp <- hist(data$elasticModulus, breaks = 5, plot = FALSE)
density <- density(data$elasticModulus)
data.density <- data.frame(x = density$x, y = density$y)

p1 = ggplot(data = data, aes(elasticModulus,..density..))+
  geom_histogram(fill = "blue"
                 , alpha = 0.5
                 , breaks = hisp$breaks)+
  geom_line(data = data.density
            , aes(x,y)
            , color = "red"
            , size = 1)+
  labs(title = "Histogram and Density curve of data"
       , x = "Measurement data [Mpa]", y = "Density")+
  theme_bw()

library(moments)
skewness(data$elasticModulus)
# 0.7942066, 正值, 右偏分配, 非嚴重偏斜分配
kurtosis(data$elasticModulus)
# 2.411438, <3, 扁平分布

p2 <- ggplot(data = data, aes(elasticModulus))+
  geom_boxplot(outlier.color = "red",
               outlier.shape = 10,
               outlier.size = 3,
               coef = 1.0)+
  labs(title = "Boxplot of data",
       x = "Measurement data [Mpa]")+
  theme_bw()+
  scale_y_discrete()
#有偏離值(如圖右下角)

library(gridExtra)
grid.arrange(p1,p2, nrow = 2)
