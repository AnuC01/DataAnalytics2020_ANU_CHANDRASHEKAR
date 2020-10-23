#LAB 5


#Trees for the Titanic

#rpart
data(Titanic)
library(rpart)
library(rpart.plot)
require(rpart)
survivedR <- rpart(Survived~.,data = Titanic)
survivedR
plot(survivedR)
text(survivedR)

#ctree 
data(Titanic)
require(party)
survivedC <- ctree(Survived~., data = Titanic)
plot(survivedC)

#hclust 
data(Titanic)
titanicH <- hclust(dist(Titanic))
survivedH <- Titanic[titanicH$Survived]
plot(titanicH)
plot(survivedH)
