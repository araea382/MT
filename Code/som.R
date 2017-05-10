library(kohonen)
library(som)

train <- train_g2_L16B[,c(14,54,69,89)]
train <- as.matrix(scale(train))

set.seed(1)
train.som <- som(as.matrix(scale(train)), grid = somgrid(5, 5, "hexagonal"))
plot(train.som)


data("wines")
som.wines <- som(scale(wines), grid = somgrid(5, 5, "hexagonal"))
summary(som.wines)


wines.sc <- scale(wines)
set.seed(7)
wine.som <- som(wines.sc, grid = somgrid(5, 4, "hexagonal"))
plot(wine.som, main = "Wine data")



data("iris")
som.iris <- som(scale(iris[,-5]), grid = somgrid(4, 4, "hexagonal"))
plot(som.iris, type="count")
plot(som.iris, type="dist.neighbours")
plot(som.iris, type="codes")
plot(som.iris, type = "property", property=som.iris$codes[[1]])

