data(iris)
setosa.index <- which(iris$Species == "setosa")
versicolor.index <- which(iris$Species == "versicolor")
d.setosa <- iris$Sepal.Length[setosa.index]
d.versicolor <- iris$Sepal.Length[versicolor.index]
sepal.length <- matrix(c(d.setosa, d.versicolor))
species <- matrix(c(rep("setosa", length(setosa.index)), rep("versicolor", length(versicolor.index))))
data <- data.frame(sepal.length, species)

shapiro.test(d.setosa)
shapiro.test(d.versicolor)
# 兩筆資料的p值皆 > 0.05，無法拒絕虛無假設

library(ggplot2)
ggplot(data = data, aes(species, sepal.length))+
  geom_boxplot()+
  coord_flip()+
  geom_point()+
  labs(title = "Boxplot Sepal data",
       y = "Sepal Length (mm)",
       x = "Species")

library(BSDA)
z.test(x = d.versicolor, 
       y = d.setosa,
       alternative = "two.sided",
       mu = 0,
       sigma.x = sd(d.versicolor),
       sigma.y = sd(d.setosa),
       conf.level = 0.95)
# p值 < 0.05, 拒絕虛無假設(即結論為兩筆數據的平均數不相等)
