library(caTools)
library(ggplot2)
library(ggthemes)
library(Amelia)
library(corrplot)
library(dplyr)
library(tidyr)
library(lubridate)

taxi.train <- read.csv('train.csv')
##taxi.test <- read.csv('test.csv')

# -- clean/prep data -- # 

#missmap(taxi.train) -- no missing values

#create new datatime columns
taxi.train$pickup_hour <- hour(taxi.train$pickup_datetime)
taxi.train$pickup_weekday <- wday(taxi.train$pickup_datetime)
taxi.train$pickup_week <- week(taxi.train$pickup_datetime)
taxi.train$pickup_month <- month(taxi.train$pickup_datetime)
taxi.train$pickup_weekend <- ifelse(taxi.train$pickup_weekday==1 | taxi.train$pickup_weekday==7,"Weekend","not-Weekend")


### Data Explore/Vis ###
taxi.train$trip_duration <- as.numeric(taxi.train$trip_duration)
taxi.train$passenger_count <- as.numeric(taxi.train$passenger_count)
taxi.num <- sapply(taxi.train,is.numeric) %>% as.matrix
taxi.num <- taxi.train[,taxi.num]
#Correlation
corr.taxi <- cor(taxi.num)
print(corrplot(corr.taxi, method='circle'))

pl1 <- ggplot(taxi.train,aes(x=pickup_weekend,y=mean(trip_duration))) + geom_bar(stat='identity') + theme_classic() + labs(x="non-Weekend vs. Weekend",y="Avg. Trip Duration")
print(pl1)

pl2<- ggplot(taxi.train,aes(x=pickup_hour,y=trip_duration)) + geom_point(aes(color=passenger_count),alpha=0.3) 
pl2<- pl2 + scale_color_gradient(low='blue', high='red') + theme_classic() + ylim(0, 100000)
print(pl2)

pl3 <- ggplot(taxi.train,aes(x=pickup_month,y=trip_duration)) + geom_bar(stat='identity') + theme_classic()
print(pl3)

 
