#DATA ANALYTICS LAB 1


#CREATING A DATA FRAME (RPI WEATHER DATA FRAME EXAMPLE)

days <- c('Mon', 'Tues', 'Wed', 'Thur', 'Fri', 'Sat', 'Sun')
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4)
snowed <- c('T', 'T', 'F', 'F', 'T', 'T', 'F')
help("data.frame")
RPI_Weather_Week <- data.frame(days, temp, snowed) # creating the data frame
RPI_Weather_Week 
head(RPI_Weather_Week) # first 6 rows
str(RPI_Weather_Week) # structure of data frame
summary(RPI_Weather_Week) 
RPI_Weather_Week[1,] #shows 1st row and all columns
RPI_Weather_Week[,1] #shows 1st column and all rows
RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5,c("days", "temp")]
RPI_Weather_Week$temp
subset(RPI_Weather_Week,subset=snowed==TRUE)
sorted.snowed <- order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]
dec.snow <- order(-RPI_Weather_Week$temp)
dec.snow
empty.DataFrame <- data.frame() 
v1 <- 1:10
v1
letters
v2 <- letters[1:10]
df <- data.frame(col.name.1 = v1, col.name.2 = v2)
df
#importing data & exporting data
#writing to CSV file
write.csv(df,file = 'saved_df1.csv')
df2 <- read.csv('saved_df1.csv')
df2


#EPI DATA EXAMPLE

EPI_data <- read.csv("Documents/DataAnalytics/Labs/Lab1/EPI_data.csv")
View(EPI_data)

#EXERCISE 1: exploring the distribution 

summary(EPI)
fivenum(EPI,na.rm=TRUE)
stem(EPI)
hist(EPI)
hist(EPI, seq(30.,95.,1.0),prob = TRUE)
lines(density(EPI,na.rm=TRUE, bw=1.))
rug(EPI)
#fitting a distribution beyond histograms
plot(ecdf(EPI),do.points=FALSE,verticals=TRUE) 
par(pty="s")
qqnorm(EPI); qqline(EPI)
x <- seq(30, 95, 1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

#for DALY variable
summary(EPI_data$DALY)
fivenum(EPI_data$DALY,na.rm=TRUE)
stem(EPI_data$DALY)
hist(EPI_data$DALY)
lines(density(EPI_data$DALY,na.rm=TRUE, bw=1.))
rug(EPI_data$DALY)
#fitting a distribution beyond histograms
plot(ecdf(EPI_data$DALY),do.points=FALSE,verticals=TRUE) 
par(pty="s")
qqnorm(EPI_data$DALY); qqline(EPI_data$DALY)
x <- seq(30, 95, 1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

#for WATER_H variable
summary(EPI_data$WATER_H)
fivenum(EPI_data$WATER_H,na.rm=TRUE)
stem(EPI_data$WATER_H)
hist(EPI_data$WATER_H)
lines(density(EPI_data$WATER_H,na.rm=TRUE, bw=1.))
rug(EPI_data$WATER_H)
#fitting a distribution beyond histograms
plot(ecdf(EPI_data$WATER_H),do.points=FALSE,verticals=TRUE) 
par(pty="s")
qqnorm(EPI_data$WATER_H); qqline(EPI_data$WATER_H)
x <- seq(30, 95, 1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for tdsn")
qqline(x)
#comparing distributions
boxplot(EPI, DALY)
qqplot(EPI, DALY)
boxplot(EPI, ENVHEALTH, ECOSYSTEM, DALY, AIR_H, WATER_H, AIR_E, WATER_E, BIODIVERSITY)

#EXERCISE 2: filtering (populations)

EPILand <- EPI[!Landlock]
ELand <- EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)
lines(density(ELand,na.rm=TRUE, bw=1.))
rug(ELand)
plot(ecdf(ELand),do.points=FALSE,verticals=TRUE) 
par(pty="s")
qqnorm(ELand); qqline(ELand)
x <- seq(30, 95, 1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for tdsn")
qqline(x)
boxplot(ELand, DALY)
qqplot(ELand, DALY)
boxplot(ELand, No_surface_water, Desert, High_Population_Density)
EPI_South_Asia <- EPI[ELand]
