#09/22/20

#LAB EXERCISES: beginning to work with data: distributions, correlations, Linear Regression

#EXERCISE 1: fitting a distribution beyond histograms
#For EPI
plot(ecdf(EPI_data$EPI), do.points = FALSE, verticals = TRUE)
plot(ecdf(EPI_data$EPI), do.points = TRUE, verticals = TRUE)
par(pty="s")
help("qqnorm")
help("qqplot")
qqnorm(EPI_data$EPI); 
qqline(EPI_data$EPI)
x<-seq(30,95,1)
x
x2<-seq(30,95,1)
x2
x2<-seq(30,96,1)
x2
qqplot(qt(ppoints(250), df = 5) , x, xlab = "Q-Q plot")
qqline(x)
#For Daly
plot(ecdf(EPI_data$DALY), do.points = FALSE, verticals = TRUE)
plot(ecdf(EPI_data$DALY), do.points = TRUE, verticals = TRUE)
par(pty="s")
help("qqnorm")
help("qqplot")
qqnorm(EPI_data$DALY); 
qqline(EPI_data$DALY)
x<-seq(30,95,1)
x
x2<-seq(30,95,1)
x2
x2<-seq(30,96,1)
x2
qqplot(qt(ppoints(250), df = 5) , x, xlab = "Q-Q plot")
qqline(x)
#For Water_H
plot(ecdf(EPI_data$WATER_H), do.points = FALSE, verticals = TRUE)
plot(ecdf(EPI_data$WATER_H), do.points = TRUE, verticals = TRUE)
par(pty="s")
help("qqnorm")
help("qqplot")
qqnorm(EPI_data$WATER_H); 
qqline(EPI_data$WATER_H)
x<-seq(30,95,1)
x
x2<-seq(30,95,1)
x2
x2<-seq(30,96,1)
x2
qqplot(qt(ppoints(250), df = 5) , x, xlab = "Q-Q plot")
qqline(x)
qqplot(EPI_data$EPI,EPI_data$DALY)
boxplot(EPI_data$EPI,EPI_data$DALY)#Comparing Distributions

#Linear basis and least-squares constraints
multivariate <- read.csv("~/Documents/DataAnalytics/multivariate.csv")
attach(multivariate)
mm<-lm(Homeowners~Immigrant)
mm
lm(formula = Homeowners~Immigrant)

#Multivariate.csv dataset
multivariate <- read.csv("~/Documents/DataAnalytics/multivariate.csv")
head(multivariate)
attach(multivariate)
help(lm)
mm <-lm(Homeowners~Immigrant)
mm 
summary(mm)$coef
plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm, col = 2 ,lwd = 3)
newImmigrantdata <- data.frame(Immigrant = c(0,20))
abline(mm)
abline(mm, col = 3, lwd = 3)
attributes(mm)
mm$coefficients

#GGPLOT EXAMPLES
#Creating Plots
#Chapter 2 -- R Graphics Cookbook. 
plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data = mtcars)
ggplot(mtcars, aes(x=wt,y=mpg))+geom_point()
plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)
lines(pressure$temperature, pressure$pressure/2, col = "red")
points(pressure$temperature, pressure$pressure/2, col = "blue")
library(ggplot2)
qplot(pressure$temperature, pressure$pressure, geom = "line")
qplot(temperature, pressure, data = pressure, geom = "line")
ggplot(pressure, aes(x=temperature, y=pressure))+ geom_line()+geom_point()
ggplot(pressure, aes(x=temperature, y=pressure))+ geom_line()+geom_point()

#CREATING BAR GRAPHS
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
qplot(factor(cyl), data = mtcars)
ggplot(mtcars,aes(x=factor(cyl)))+geom_bar()

#CREATING HISTOGRAMS USING GGPLOT
#Creating Histogram
#View the distribution of 1-D data with a histogram
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10)
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
qplot(mpg, data = mtcars, binwidth = 4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth = 4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth = 5)

#CREATING BOX-PLOTS USING GGPLOT
plot(ToothGrowth$supp, ToothGrowth$len)
boxplot(len~supp, data = ToothGrowth)
boxplot(len~supp+dose, data = ToothGrowth)
library(ggplot2)
qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot")
qplot(supp, len, data = ToothGrowth, geom = "boxplot")
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom = "boxplot")
qplot(interaction(supp, dose), len, data = ToothGrowth, geom = "boxplot")
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y = len)) + geom_boxplot()
