
# Remove all the objects stored in R :
rm(list=ls())

# Setting the working directory :
setwd("D:/PROJECT DATA SCIENCE/4.BIKE RENTING EDWISOR")

# Load Libraries
x = c('dplyr',"ggplot2", "corrgram","outliers", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','tidyverse')

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

# Read the data
data = read.csv("day.csv", header = T)

# Viewing the dataset
head(data)

# Getting the dimensions of the data
dim(data)

# Getting the column names
colnames(data)

# Checking the unique values in each variable
unique(data$season)
unique(data$yr)
unique(data$mnth)
unique(data$holiday)
unique(data$weekday)
unique(data$workingday)
unique(data$weathersit)
### All the values are correct.

# Checking for missing values
as.data.frame(colSums(is.na(data)))
### No-Missing values.

# Getting the structure of the dataset
str(data)

# Changing the datatypes of several variables

data$season = as.factor(data$season)
data$yr = as.factor(data$yr)
data$mnth = as.factor(data$mnth)
data$holiday = as.factor(data$holiday)
data$weekday = as.factor(data$weekday)
data$workingday = as.factor(data$workingday)
data$weathersit = as.factor(data$weathersit)

# Summary of the data
summary(data)
# Comments : 1.'Date' variable is not needed according to the objective.
#            2. There is a skewed distribution of 'yr' variable.
#            3. All other data are well distributed.

# Dropping the variable
d1 = data[-c(1,2,4,6,11,14,15)]

#===================: UNI-VARIATE ANALYSIS :===========================
#======================================================================


#===================: Season :================================

# Getting the count of each values
table(d1$season)  

# Plotting the bargraph
plot(d1$season)

## Equally distributed data.

#====================: Month :================================

# Getting the count of each values
table(d1$mnth)  

# Plotting the bargraph
plot(d1$mnth)

## Well distributed.

#===================: Weekday :===============================

# Getting the count of each values
table(d1$weekday)  

# Plotting the bargraph
plot(d1$weekday)

## Well distributed.

#===================: Workingday :============================

# Getting the count of each values
table(d1$workingday)  

# Plotting the bargraph
plot(d1$workingday)

## Well distributed.

#==================: Weather Situation :======================

# Getting the count of each values
table(d1$weathersit)  

# Plotting the bargraph
plot(d1$weathersit)

## Less data for 3rd situation. Count of 'Clean' weather is high.


#==================: Temperature :============================

# Plotting the Histogram and Boxplot
boxplot(d1$temp,main="Boxplot for 'temp'",ylab="norm.temp",col='blue')
hist(d1$temp,main="Histogram for 'temp'",xlab = "Temp",col = "blue")

## Almost bell shaped distribution.No Outliers.


#==================: Humidity :============================

# Plotting the Histogram and Boxplot
boxplot(d1$hum,main="Boxplot for 'Humidity'",ylab="norm.humidity",col='blue')
hist(d1$hum,main="Histogram for 'Humidity'",xlab = "HUmidity",col = "blue")

## Bell shaped distribution.There are outliers in the data.



#==================: Windspeed :============================

# Plotting the Histogram and Boxplot
boxplot(d1$windspeed,main="Boxplot for 'Windspeed'",ylab="norm.Windspeed",col='blue')
hist(d1$windspeed,main="Histogram for 'Windspeed'",xlab = "Windspeed",col = "blue")

## Slightly skewed distribution and have some outliers.


#==================: Count :============================

# Plotting the Histogram and Boxplot
boxplot(d1$cnt,main="Boxplot for 'Count'",ylab="Count of bike rent",col='blue')
hist(d1$cnt,main="Histogram for 'Count'",xlab = "Count",col = "blue")

## Almost bell shaped distribution.No outliers.


#===================: BI-VARIATE ANALYSIS :===========================
#=====================================================================



#=====================: Season Vs Count :=============================

# Aggregating the count According to season
aggregate(d1$cnt,by = list(d1$season),FUN = median)

# Visualization
ggplot(d1, aes(x = cnt)) +
  geom_histogram(binwidth = 100,bins = 50,col='blue') +
  facet_wrap(~season)+
  ggtitle("Seasons")+
  xlab("Bike rental Count") +
  ylab("Total Count") 

plot(d1$season,d1$cnt,col='blue',
     main = "Season Vs Count",
     xlab="Seasons",ylab="Count of bike rental",pch=18,)

# CONCLUSIONS : 1. Season 2,3,4 (summer,fall,winter) has higher no of bookings.
#               2. Fall season has on average highest no of bookings.
#               3. Affecting the count variable so can be significant for modelling.
#               4. There are some outliers in the data.


#=====================: Month Vs Count :=============================

# Aggregating the count According to Month
aggregate(d1$cnt,by = list(d1$mnth),FUN = median)

# Visualization
ggplot(d1, aes(x = cnt)) +
  geom_histogram(binwidth = 100,bins = 50,col='blue') +
  facet_wrap(~mnth)+
  ggtitle("MONTHS")+
  xlab("Bike rental Count") +
  ylab("Total Count") 

plot(d1$mnth,d1$cnt,col='blue',
     main = "Month Vs Count",
     xlab="Month",ylab="Count of bike rental",pch=18,)

# CONCLUSIONS : 1.In month of Jun and Jul,highest bikes are rented.
#               2.Booking are increasing in starting month then getting higher in middle months and then decreasing in last months.
#               3.No-outliers.



#=====================: Weekday Vs Count :=============================

# Aggregating the count According to Weekday
aggregate(d1$cnt,by = list(d1$weekday),FUN = median)

# Visualization
ggplot(d1, aes(x = cnt)) +
  geom_histogram(binwidth = 100,bins = 50,col='blue') +
  facet_wrap(~weekday,)+
  ggtitle("WEEKDAY")+
  xlab("Bike rental Count") +
  ylab("Frequency") 

plot(d1$weekday,d1$cnt,col='blue',
     main = "Weekday Vs Count",
     xlab="Weekday",ylab="Count of bike rental",pch=18,)

# CONCLUSIONS : 1. The mean value is almost same for all the days.
#               2. Thursday has highest bookings.
#               3. NO-Outliers in the data.




#=====================: Workingday Vs Count :=============================

# Aggregating the count According to Workingday
aggregate(d1$cnt,by = list(d1$workingday),FUN = median)

# Visualization
ggplot(d1, aes(x = cnt)) +
  geom_histogram(binwidth = 100,bins = 50,col='blue') +
  facet_wrap(~workingday)+
  ggtitle("Workingday")+
  xlab("Bike rental Count") +
  ylab("Frequency") 

plot(d1$workingday,d1$cnt,col='blue',
     main = "Workingday Vs Count",
     xlab="workingday",ylab="Count of bike rental",pch=18,)

# CONCLUSIONS : 1. Working day has more bookings on an average.
#               2. No-Outliers.



#=====================: Weather Situation Vs Count :=============================

# Aggregating the count According to Weather
aggregate(d1$cnt,by = list(d1$weathersit),FUN = median)

# Visualization
ggplot(d1, aes(x = cnt)) +
  geom_histogram(binwidth = 100,bins = 50,col='blue') +
  facet_wrap(~weathersit)+
  ggtitle("WEATHER_SITUATION")+
  xlab("Bike rental Count") +
  ylab("Frequency") 

plot(d1$weathersit,d1$cnt,col='blue',
     main = "Weathersit. Vs Count",
     xlab="Weather",ylab="Count of bike rental",pch=18,)

# CONCLUSIONS : 1. Rainy(3) weather situation has lowest no of bookings.
#               2. Clear(1) weather has higer no of bookings.
#               3. No-Outliers.



#=====================: Temperature Vs Count :=============================

# Visualization

plot(d1$temp,d1$cnt,col='blue',
     main = "Temp Vs Count",
     xlab="Temp",ylab="Count of bike rental",pch=18,)


# CONCLUSIONS : 1.Linear relatioship between temperature and count.
#               2.There are some outliers also.
#               3.Significant variable for modelling.



#=====================: Humidity Vs Count :=============================

# Visualization

plot(d1$hum,d1$cnt,col='blue',
     main = "Humidity Vs Count",
     xlab="Humidity",ylab="Count of bike rental",pch=18,)


# CONCLUSIONS : 1.Complex relatioship between humidity and count.
#               2.There are some outliers also.


#=====================: Windspeed Vs Count :=============================

# Visualization

plot(d1$windspeed,d1$cnt,col='blue',
     main = "Windspeed Vs Count",
     xlab="Windspeed",ylab="Count of bike rental",pch=18,)


# CONCLUSIONS : 1.Higher bookings on low windspeed.
#               2.There are some outliers also.




#===================: MULTI-VARIATE ANALYSIS :===========================
#========================================================================

#===================: Season + Temp. ~ Count :===========================

p <- ggplot(d1, aes(x=cnt, y=temp, color=season, shape=season)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(title="Temperature vs Count",
       x="Count of bike rental", y = "Temperature")
p + theme_classic()  

# CONCLUSIONS: 1.In Season 3 (Fall), temperature has almost no effect on bookings.
#              2.In Season 1,2,4 (Spring,Summer,Winter), Temperature and booking counts have linear relationship.



#===================: Season + Workingday ~ Count :===========================

ggplot(d1, aes(fill=workingday, y=cnt, x=season)) + 
  geom_bar(position="dodge", stat="identity")

# CONCLUSIONS: 1. Working day is affecting the bike bookings only in Spring season.


#===================: Weather Situation + windspeed ~ Count :===========================

p <- ggplot(d1, aes(x=cnt, y=windspeed, color=weathersit, shape=weathersit)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(title="Windspeed vs Count",
       x="Count of bike rental", y = "Windspeed")
p + theme_classic() 

# CONCLUSIONS: 1. Decreasing windspeed increases no of bookings in all weather conditions.


# Pair-wise visualization of numerical variable 
pairs(d1[c("temp", "hum", "windspeed", "cnt")],main = "Pair plot with outliers",col='blue')

#==============================: OUTLIER ANALYSIS :=====================================
#=======================================================================================

# Variables that has outliers : Humidity, Windspeed, Count

rm.outlier(d1$hum, fill = FALSE, median = FALSE, opposite = FALSE)
rm.outlier(d1$windspeed, fill = FALSE, median = FALSE, opposite = FALSE)
rm.outlier(d1$windspeed, fill = FALSE, median = FALSE, opposite = FALSE)


pairs(d1[c("temp", "hum", "windspeed", "cnt")],main = "Pair plot without outliers",col='blue')

#=============================: Co-Relation Analysis :===================================

corr_mat = cor(d1[c('temp','hum','windspeed','cnt')],method = "kendall")
corr_mat

# CONCLUSIONS : 1.Temp is highly correlated to Count.
#               2.Windspeed and Humidity are less correlated to Count
#               3.NO independent variable is correlated. 


#=============================: MODELLING :============================================
#======================================================================================

# Separating the categorical variables

#factor_index = sapply(d1,is.factor)
#data_fact = d1[,factor_index]

#Get the names of factor variables
#factor_columns = names(as.data.frame(data_fact))

# Dummifying the variables
#library(dummies)
#data_dummy = dummy.data.frame(d1,factor_columns,omit.constants=FALSE)

# Divide the data into train and test set with stratified sampling

set.seed(3333)
train.index = createDataPartition(d1$cnt, p = .80, list = FALSE)
train = d1[ train.index,]
test  = d1[-train.index,]

#===============================: Linear regression :===============================

# 1. Count ~ Temp
mod_lin1 = lm(train$cnt~train$temp)
print(summary(mod_lin1))

# Conclusions : Very high F-statistic value.


# 2. Count ~ Temp+Windspeed
mod_lin2 = lm(data = d1,cnt~temp+windspeed)
summary(mod_lin2)

# 3. Count ~ all variables
mod_lin3 = lm(data=d1,cnt~.)
summary(mod_lin3)


#===============================: Random Forest :====================================

#1. Count ~ Temp
mod_rf1 = randomForest(cnt~temp,train,ntree=100)

RF_Predict = predict(mod_rf1, test[,-9])

ConfMatrix_RF = table(test$cnt, RF_Predict)


# End.