#Anusahana Chandrashekar
#Assignment 6 - Term Project

#Dataset file from data.world and by James Gaskin (https://data.world/jamesgaskin/movies)

#Read Dataset

library("readxl")
actors <- read_excel(file.choose())
attach(actors)
actors

#Exploratory Data Analysis

library(ggplot2)
ggplot(data = actors) + geom_bar(mapping = aes(x = gender))
ggplot(data = actors) + geom_bar(mapping = aes(x = ethnicity))

#Analysis (w/o NA values)

new_actors <- na.omit(actors)

ggplot(data = new_actors) + geom_bar(mapping = aes(x = gender))
ggplot(data = new_actors) + geom_bar(mapping = aes(x = ethnicity))

ggplot(new_actors, aes(gender, fill = ethnicity)) + geom_bar()
ggplot(new_actors, aes(gender)) + geom_bar() + facet_wrap(~ethnicity)

#K-means Clustering Model
library(ggplot2)
head(new_actors)
str(new_actors)
summary(new_actors)

ggplot(new_actors, aes(x= gender, y = ethnicity))+geom_point()

set.seed(300)
k.max <- 12
wss <- sapply(1:k.max, function(k){kmeans(iris[,3:4], k, nstart = 20, iter.max = 20)$tot.withinss})
wss
plot(1:k.max, wss, type = "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")

#Regression Model Attempt 
boxplot(gender)
boxplot(ethnicity)

mm <- lm(gender ~ ethnicity)
mm
summary(mm)$coef

#Prediction (Predicting ethnicity from gender)
model <- lm(gender ~ ethnicity, data = new_actors)
model
new <- data.frame (
  gender = c(Female)
)
predict(model, newdata = new)
predict(model, newdata = new, interval = "confidence")
predict(model, newdata = new, interval = "prediction")
