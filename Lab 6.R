#Lab 6


#Trees for the Titanic

data(Titanic)
require(randomForest)
fit <- randomForest(Survived~., data = Titanic)
fit
importance(fit)
varImpPlot(fit)
plot(fit)
getTree(fit,1, labelVar = TRUE)
