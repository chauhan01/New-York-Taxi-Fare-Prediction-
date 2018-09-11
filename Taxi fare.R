
#-----------------Preparing enviroment-------------------------------

library(ggplot2)
library(sqldf)
library(dplyr)
library(lubridate)
library(geosphere)

#---------------------setting working directory---------------------------
path = "C:/Users/Ravi/Desktop/shubham data/R/Taxi fare"

setwd(path)

#-----------------------reading csv file--------------------------------
set.seed(100)

n_rows_count= 1048576

traindata<- read.csv("train.csv", header = TRUE, na.strings = "NA", nrows = n_rows_count)
testdata<- read.csv("test.csv", header = TRUE, na.strings = "NA")


#-----------------------Basic explortion of data------------------
head(traindata)
str(traindata)
summary(traindata)

#--------------------Data cleaning-----------------------------

colSums(is.na(traindata)) # checking for missing values

traindata <- na.omit(traindata) # droping all the recordes with missing values

testdata <- na.omit(testdata)

#there are some -ve values in fare amount which must be system error
# converting those -ve values to +ve

traindata$fare_amount <- abs(traindata$fare_amount)


# removing the records where latitude is greater than 90 and less than -90 and longitude greater than 180 and less than -180

traindata <- traindata[-which(traindata$pickup_latitude > 90 | traindata$pickup_latitude < -90 | traindata$dropoff_latitude > 90 | traindata$dropoff_latitude < -90),]
traindata <- traindata[-which(traindata$pickup_longitude > 180 | traindata$pickup_longitude < -180 | traindata$dropoff_longitude > 180 | traindata$dropoff_longitude < - 180),]

summary(traindata)


#Calculating distance from pickup and drop coordinates

traindata<- traindata %>% mutate(distance.in.km = by(traindata, 1:nrow(traindata),
                                                     function(row){
                                                       distHaversine(c(row$pickup_longitude, row$pickup_latitude), c(row$dropoff_longitude, row$dropoff_latitude))/1000
                                                     }))

#for test dataset
testdata<- testdata %>% mutate(distance.in.km = by(testdata, 1:nrow(testdata),
                                                     function(row){
                                                       distHaversine(c(row$pickup_longitude, row$pickup_latitude), c(row$dropoff_longitude, row$dropoff_latitude))/1000
                                                     }))


summary(traindata$distance.in.km)

#removing zero distance values

zerodist<- which(traindata$distance.in.km == 0)
traindata<- traindata[-zerodist,]



# removing the distance > 150 
traindata <- traindata[-(which(traindata$distance.in.km > 150)),]
max(traindata$distance.in.km)

#ploting the graph for distance and fare

gplot <- ggplot(data = traindata, aes(x = traindata$distance.in.km, y = traindata$fare_amount))+
  geom_point() + geom_line()+
  ggtitle("distance and fare plot") + xlab("distance in km") +ylab("fare")

gplot

#working on date and time variables
traindata1 <- traindata #creating backup

traindata$pickup_datetime <- as.Date(traindata$pickup_datetime)
traindata$year <- year(traindata$pickup_datetime) 
traindata$month <- month(traindata$pickup_datetime)
traindata$day <- day(traindata$pickup_datetime)
traindata$hour <- hour(traindata$pickup_datetime)
traindata$wday <- wday(traindata$pickup_datetime)

#for test dataset

testdata$pickup_datetime <- as.Date(testdata$pickup_datetime)
testdata$year <- year(testdata$pickup_datetime) 
testdata$month <- month(testdata$pickup_datetime)
testdata$day <- day(testdata$pickup_datetime)
testdata$hour <- hour(testdata$pickup_datetime)
testdata$wday <- wday(testdata$pickup_datetime)

str(testdata)


traindata$wday <- as.numeric(traindata$wday)

#Buldinf linear model

model <- lm(fare_amount~pickup_longitude + pickup_latitude + distance.in.km + year+ month + wday + dropoff_longitude+ dropoff_latitude +passenger_count, testdata)
summary(model)

testdata$fare_amount <- predict(model, newdata = testdata)
submission <- subset(testdata, select = c(1,14))
write.csv(submission, file="Submission.csv", row.names = FALSE)
