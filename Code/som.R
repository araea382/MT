library(kohonen)
library(som)

train <- train_g2_L16B[,c(14,54,69,89)]
train <- as.matrix(scale(train))

set.seed(1)
train.som <- som(as.matrix(scale(train)), grid = somgrid(2, 2, "hexagonal"))
plot(train.som)


data("wines")
som.wines <- som(scale(wines), grid = somgrid(5, 5, "hexagonal"))
summary(som.wines)


wines.sc <- scale(wines)
set.seed(7)
wine.som <- som(wines.sc, grid = somgrid(5, 4, "hexagonal"))
plot(wine.som, main = "Wine data")



