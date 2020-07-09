---
title: "Flight_Capstone"
author: "Nitin Yadav"
date: "29/02/2020"
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


## Introduction

# Problem Statement 

Airline industries are in continuous tussle to get more and more customers and in turn are working on very thin margins. The price of flight tickets are very unpredictable considering the dynamic nature of business and governing the law of demand and supply. At times we have noted that for a particular city or destination when we search for flight price , the price keeps getting dynamically updated depending on the search criteria, seat availability, date and time of travel etc. Hence it becomes very important for the Airline industry to have a right price prediction mechanism which is backed up by data and helps the industry to take a data driven decision.


# Need of Study Project

This is a problem of machine learning where we have been given 2 data sets i.e Train and Test set.
Train data consist of 10683 records 
Test data consist of 2671 records 


# Understanding business/social opportunity

This is a MAchine learning problem based on supervised learning. Here we train the algorithm using the Train dataset. In supervised machine learnng we know the Target variable and we try to identify the kep predictors on which the response variable (Y) is dependent. Based on the trained machine model, we then try to predict the target using a Test set. This is very crucial in Airline industry as price of a flight is very important parameter for a customer to take the travel decision and a right price point will be beneficial for both customer and the Airline company. Hence the better the MAchine learning model, better would be the accuracy and hence minimum error.



```{r}
# Loading  required libraries 

library(readxl)
library(readr)
library(dplyr)
library(corrplot)
library(mice)
library(DMwR)
library(car)
library(caret)
library(lattice)
library(tidyr)
library(data.table)
library(chron)
library(scales)
library(yarrr)
library(lubridate)
library(caTools)
library(gsubfn)
library(miscset)
library(e1071)
library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest)

```

```{r}
# Loading the Dataset 

# Setting the current working directory 
setwd("C:/Users/nitin/OneDrive/Desktop/R and python Programming/Datasets")
FTrain=read_excel("FlightPrice_train.xlsx")
FTest=read_excel("FlightPrice_test.xlsx")



```

## Data Report 

# Data collection in terms of time, frequency and methdology

If we see the data collected , we notice that data provided comprises of 4 months data starting from MArch till June 2019 and the data is provided for Weekday, weekends and for 24 hour time period across all days. There can be various methodologies to collect data i.e through APIs as the direct hstorical data for airline flights is not available, howwver different travel websites provided data in various fields which has to be cleaned first to get data in desired format

# visual inspection of Data (Rows, Columns, Descriptive Stats)

The Train dataset comprises of 10683 rows and 10 columns. Test set consist of 2671 row items and 10 variables. We observe the following in dataset 
1. Price is dependent variable, all other variable are independent or predictors
2. Except Price which is numeric, all other variables are in "Char" format which needs to be converted to categorical or right class
3. Date of Journey column needs to be separated into "Date", "Month" and "Year" columns and convert to Date format
4. Route Info has starting city as "Source" and end city as "Destination". We need to do feature engineering to create 2 columns i.e for Source and Destination using the separate function and see if this matches with the existing source and destination information provided.
5. Departure time and arrival time have to be converted to time format and the duration has to be put in either "Total hours" or "Total mins". We have taken "Total Mins"
6. Total stops have to be converted to factor category
7. Jet Airways and Indigo have the maximum number of flights followed by Air India
8. Delhi, Kolkata and Bangalore have the maximum flights starting from them as Source City
9. Cochin, Bangalore and Delhi have the maximum flights reaching there as Destination City
10. There are 3491 non-stop flights and 5625 flights with 1 stop.
11. There is a huge variation in the price , minimum is 1750 and maximum goes upto 79500. There are possibility of outliers in the PRice column.
12. Dates within 1st to 10th of month have highest number of flights and maximum flight are in month of May-June (Possibility of Summer Holidays)

# Understanding of Attributes (variable info)

1. Convert Date of Journey in Date, month and Year columns 
2. Convert the required variables to Factor or Date formats
3. Separate the Duration column in Hour and minutes to calculate Total Minutes 
4. Convert the Departure time in two brackets i.e day time (9am-9pm) and night time (9pm-9am)
5. Get the weekday information and create a separate column for the day of week from date of journey field

```{r}
# Visual inspection of data 


# Converting the data into required formats 


FTrain$Airline=as.factor(FTrain$Airline)
FTrain$Date_of_Journey=as.Date(FTrain$Date_of_Journey, format = "%d/%m/%Y")
FTrain$Date=format(FTrain$Date_of_Journey, "%d")
FTrain$Month=format(FTrain$Date_of_Journey, "%m")
FTrain$Year=format(FTrain$Date_of_Journey, "%Y")

FTrain$Date=as.factor(FTrain$Date)
FTrain$Month=as.factor(FTrain$Month)
FTrain$Year=as.factor(FTrain$Year)

FTrain$Source=as.factor(FTrain$Source)
FTrain$Destination=as.factor(FTrain$Destination)

summary(FTrain)
str(FTrain)
colnames(FTrain)

attach(FTrain)


# Separate source and destination 

separate(FTrain, Route, into=c("Source","Destination"), sep = "[^[:alnum:]]+", remove = TRUE,
  convert = FALSE, extra = "warn", fill = "warn")

# Converting Arrival and Departure in Time format

FTrain$Dep_Time=as.POSIXct(Dep_Time, format="%H:%M")

# Date transformation for Arrival time 

new_df <- separate(data = FTrain, col = Arrival_Time, into  = c('Time', 'Date'), sep = ' ')

FTrain=cbind(FTrain,new_df[,7])

# Renaming the arrival time column name 

names(FTrain)[names(FTrain)=="Time"]="Arrival-Time"

FTrain$`Arrival-Time`=as.POSIXct(FTrain$`Arrival-Time`, format="%H:%M")

#FTrain$Dep_Time=times(format(FTrain$Dep_Time, "%H:%M:%S"))
#FTrain$`Arrival-Time`=times(format(FTrain$`Arrival-Time`, "%H:%M:%S"))

names(FTrain)[names(FTrain)=="Date"]="Date of Travel"
names(FTrain)[names(FTrain)=="Month"]="Month of Travel"
names(FTrain)[names(FTrain)=="Year"]="Year of Travel"


FTrain$Total_Stops=as.factor(FTrain$Total_Stops)



```

```{r}

# Similar Data manipulation for Test set so that we can predict the flight prices using the test set.

FTest$Airline=as.factor(FTest$Airline)
FTest$Date_of_Journey=as.Date(FTest$Date_of_Journey, format = "%d/%m/%Y")
FTest$Date=format(FTest$Date_of_Journey, "%d")
FTest$Month=format(FTest$Date_of_Journey, "%m")
FTest$Year=format(FTest$Date_of_Journey, "%Y")

FTest$Date=as.factor(FTest$Date)
FTest$Month=as.factor(FTest$Month)
FTest$Year=as.factor(FTest$Year)

FTest$Source=as.factor(FTest$Source)
FTest$Destination=as.factor(FTest$Destination)

attach(FTest)

# Separate source and destination 

separate(FTest, Route, into=c("Source","Destination"), sep = "[^[:alnum:]]+", remove = TRUE,
  convert = FALSE, extra = "warn", fill = "warn")

# Converting Arrival and Departure in Time format

FTest$Dep_Time=as.POSIXct(Dep_Time, format="%H:%M")

# Date transformation for Arrival time 

new_df1 <- separate(data = FTest, col = Arrival_Time, into  = c('Time', 'Date'), sep = ' ')

FTest=cbind(FTest,new_df1[,7])

# Renaming the arrival time column name 

names(FTest)[names(FTest)=="Time"]="Arrival-Time"

FTest$Arrival_Time=as.POSIXct(FTest$Arrival_Time, format="%H:%M")

#FTrain$Dep_Time=times(format(FTrain$Dep_Time, "%H:%M:%S"))
#FTrain$`Arrival-Time`=times(format(FTrain$`Arrival-Time`, "%H:%M:%S"))

names(FTest)[names(FTest)=="Date"]="Date of Travel"
names(FTest)[names(FTest)=="Month"]="Month of Travel"
names(FTest)[names(FTest)=="Year"]="Year of Travel"


FTest$Total_Stops=as.factor(FTest$Total_Stops)



```





```{r}
# Keeping the variables of interest 


FTrain=FTrain[,c(1,2,12:14,3,4,6,15,8,9,11)]


temp <- separate(data = FTrain, col = Duration, into  = c('Hour', 'Mins'), sep = ' ')

temp$h1=parse_number(temp$Hour)
temp$h2=parse_number(temp$Mins)

temp$h2[is.na(temp$h2)] <- 0
temp$Total_Duration=60*temp$h1+temp$h2

FTrain=cbind(FTrain,temp$Total_Duration)

names(FTrain)[names(FTrain)=="temp$Total_Duration"]="Duration in Mins"

FTrain=FTrain[,-10]

str(FTrain)

```


```{r}
# Similar data manipulation for Test set 


FTest=FTest[,c(1,2,11:13,3,4,6,7:9)]

temp1 <- separate(data = FTest, col = Duration, into  = c('Hour', 'Mins'), sep = ' ')

temp1$h1=parse_number(temp1$Hour)
temp1$h2=parse_number(temp1$Mins)

temp1$h2[is.na(temp1$h2)] <- 0
temp1$Total_Duration=60*temp1$h1+temp1$h2

FTest=cbind(FTest,temp1$Total_Duration)

names(FTest)[names(FTest)=="temp1$Total_Duration"]="Duration in Mins"




FTest=FTest[,-10]


```


## Exploratory Data Analysis

# Univariate Analysis

1. Price and Total Duration are numeric categories and all other columns are either categorical or date class
2. Boxplot and histogram of Price shows the presence of outliers
3. Skewness is a mesure of symmetry , positive skewness for price (1.85) means the mean is more than median of the entries and hence it is right skewed
4. Kurtosis define the tail shape of data distribution , in this we have excess kurtosis (13.5) which is towards positive hence it indicates Fat tailed distribution or leptokurtic
5. Day of Travel shows that maximum number of flights are on Monday , Wednesday and Thursday
6  Departure Time and arrival time shows that maximum number of flights arrive and depart around 7 pm in evening 
7. Minimum of duration (in mins) is 75 mins and maximum is 2860.
8. Total count of flights is highest during Daytime, on wednesday as day of week and flight with 1 stop


# Bivariate Analysis

1. Average flight price on Sunday and Friday are highest and on Monday are lowest
2. Price of Daytime flight is more than night time
3. Jet Airways, Air India and Indigo have highest number of flights in May june month which is maximum or peak season from flights perspective due to summer season
4. Delhi, Kolkata and Bangalore are the popular choice as Source for boarding the flights
5. Cochin, Bangalore are the popular choice as Destination 
6. Average Flight price per week is high in the months of May and June compared to MArch April
7. Jet Airways command the highest price among the Airline categories as evident from box plot
8. Average flight price is high during the first 15 days of month compared to the month end days unless there is some specific festive occasion
9. Delhi and Kolkata commands the highest median price among the other source cities 
10. Delhi and Kolkata has highest number of flights as source city and also the count of 1 stops is high for these cities
11. Flight price and Flight duration in mins have a positive correlation of 0.56, means as duration increases flight price increases.


# Unwanted variable Removal, outlier treatment and Missing value Treatment

1. We remove unwanted variable like Route information and Additional info from our dataset as they are not contributing to the model and we have already extracted source and destination information from Route information.

2. For outlier treatment, we notice that outlier present in price , we take maximum value of price as 22500, and drop the data points above that point. By doing this we have eliminated around 322 entires of flight price having value higher than 22500

3. For NA values , we notice that there is 1 NA present in Total Stops column, hence we take complete cases and drop the single entry. After doing this transformation the final row count is 10361 and 16 rows


# Addition of New Variables

This step we have already covered as part of earlier description provided.


# Insights from EDA

We have already covered insights from EDA. For data imbalance, it make more sense when the classification is binary (0 or 1) but in our case the response variable (Price) is numeric so data imbalance would not play much role here. also the imbalance due to oultier entries is around 3% and very minimal.

Also techniques like clustering and PCA would have played role where we didnt have target column and we are trying to predict the target, but in our case we have been given the price information and we need to use the same to predict the test data once the model gets sufficient learning and tuning from train data.





```{r}

# Checking the NA values in the complete data set 



sum(is.na(FTrain))
colSums(is.na(FTrain))

# Only 1 NA value in the model , in the Total Stops variable, which is manageable.



```

```{r}
# Outlier assesment in the dataset 

hist(FTrain$Price)
boxplot(FTrain$Price, horizontal = TRUE, col="Red")


hist(FTrain$`Duration in Mins`)
boxplot(FTrain$`Duration in Mins`, horizontal = TRUE, Col="Green")

summary(FTrain)

```

```{r}
colnames(FTrain)

str(FTrain)
```

```{r}

FTrain$CombineDate_Time =paste(FTrain$Date_of_Journey, FTrain$Dep_Time)

FTrain$CombineDate_Time=as.Date(FTrain$CombineDate_Time)
FTrain %>% ggplot( aes(FTrain$CombineDate_Time)) + geom_freqpoly(binwidth=86400)

ggplot(FTrain, aes(x = FTrain$Date_of_Journey, fill = factor(FTrain$Airline))) +
    geom_bar(stat = "count")

ggplot(FTrain, aes(x = FTrain$`Date of Travel`, fill = factor(Source))) +
    geom_bar(stat = "count")

ggplot(FTrain, aes(x = FTrain$`Month of Travel`, fill = factor(Source))) +
    geom_bar(stat = "count")


ggplot(FTrain, aes(x = FTrain$Dep_Time, fill = factor(Source))) +
    geom_bar(stat = "count")


ggplot(FTrain, aes(x = FTrain$Dep_Time, fill = factor(Source))) +
    geom_bar(stat = "count") +
      scale_x_datetime(date_breaks = "4 hour",
                       date_labels = "%I:%M %p")


ggplot(FTrain, aes(x = FTrain$`Arrival-Time`, fill = factor(Destination))) +
    geom_bar(stat = "count") +
      scale_x_datetime(date_breaks = "4 hour",
                       date_labels = "%I:%M %p")


#ggplot(FTrain, aes(x = FTrain$`Arrival-Time`, fill = factor(Destination))) +
#geom_bar(stat = "count") +
#scale_x_datetime(date_breaks = "2 hour",labels = date_format("%H:%M:%S"))


FTrain %>% 
  mutate(wday = wday(FTrain$Date_of_Journey, label = TRUE)) %>% 
  ggplot(aes(x = wday)) +
    geom_bar()


FTrain %>% 
  count(week = floor_date(FTrain$Date_of_Journey, "week")) %>% 
  ggplot(aes(week, n)) +
    geom_line()


FTrain %>% 
  mutate(dep_hour = update(FTrain$Dep_Time, yday = 1)) %>% 
  ggplot(aes(dep_hour)) +
    geom_freqpoly(binwidth = 300)



ggplot(data = FTrain, aes(x = FTrain$Dep_Time)) +
  geom_histogram(binwidth = 50) + scale_x_datetime(date_breaks = "3 hour",
                       date_labels = "%I:%M %p")


ggplot(data = FTrain, aes(x = FTrain$`Arrival-Time`)) +
  geom_histogram(binwidth = 50) + scale_x_datetime(date_breaks = "3 hour",
                       date_labels = "%I:%M %p")


FTrain %>%
  group_by(FTrain$`Date of Travel`) %>%
  summarise(count = n())

```
```{r}

# Measurement of skewness and kurtosis 

# Skewness is a mesure of symmetry , positive skewness in this case means the mean is more than median of the entries and hence it is right skewed

skewness(FTrain$Price)

kurtosis(FTrain$Price)


# Kurtosis define the tail shape of data distribution , in this we have excess kurtosis which is towards positive hence it indicates Fat tailed distribution or leptokurtic


```


```{r}
FTrain$DayofWeek=weekdays(FTrain$Date_of_Journey)
FTrain$HourofDay= cut.POSIXt(FTrain$Dep_Time, breaks = "hour")
FTrain$HourofDay=as.ITime(FTrain$HourofDay)
FTrain$HourofDay <- hour(FTrain$HourofDay)
FTrain$Hourbracket <- ifelse( FTrain$HourofDay<9 | 21<FTrain$HourofDay, "Night Time","Day Time" )

FTrain$DayofWeek=as.factor(FTrain$DayofWeek)
FTrain$Hourbracket=as.factor(FTrain$Hourbracket)


```

```{r}
## Similar data manipulation for Test Data set

FTest1=FTest

FTest1$DayofWeek=weekdays(FTest1$Date_of_Journey)
FTest1$HourofDay= cut.POSIXt(FTest1$Dep_Time, breaks = "hour")
FTest1$HourofDay=as.ITime(FTest1$HourofDay)
FTest1$HourofDay <- hour(FTest1$HourofDay)
FTest1$Hourbracket <- ifelse( FTest1$HourofDay<9 | 21<FTest1$HourofDay, "Night Time","Day Time" )

FTest1$DayofWeek=as.factor(FTest1$DayofWeek)
FTest1$Hourbracket=as.factor(FTest1$Hourbracket)

FTest1=unique(FTest1)
FTest1=FTest1[complete.cases(FTest1), ]


```




```{r}
# Some Exploratory data analysis

# Removing duplicates and keeping only unique values 

FTrain=unique(FTrain)
dim(FTrain)

attach(FTrain)

aggregate(formula = Price ~ DayofWeek,
          data = FTrain,
          FUN = mean)

aggregate(formula = Price ~ Hourbracket,
          data = FTrain,
          FUN = mean)
```

```{r}
# Outlier and missing values treatment 

# Dropping the NA value, we found that only 1 NA value in Total stops, hence dropping it

FTrain=FTrain[complete.cases(FTrain), ]

boxplot(FTrain[,11:12], horizontal = TRUE, col="Red","Blue")

# Through boxplot we find that approx 3% of entries in the total dataset are outliers, so dropping those 

FTrain1=FTrain[which(FTrain$Price<22500),]

boxplot(FTrain1[,11:12], horizontal = TRUE, col="Red","Blue")
dim(FTrain1)

head(FTrain1[order(FTrain1$Price,decreasing=TRUE),])

tail(FTrain1[order(FTrain1$`Duration in Mins`),])


```

```{r}
# EDA on the train data set 

# Boxplot of numerical vs categorical 

boxplot(FTrain1$Price~FTrain1$DayofWeek, xlab="Day of Week",
        ylab="Flight price",main="Price by Day of week",col=c("Red","Green"))

boxplot(FTrain1$Price~FTrain1$Hourbracket, xlab="Hour of Day",
        ylab="Flight price",main="Price by Day vs Night",col=c("Blue","Cyan"))


boxplot(FTrain1$`Duration in Mins`~FTrain1$DayofWeek, xlab="Day of Week",
        ylab="Flight duration",main="Duration by Day of week",col=c("Red","Green"))

boxplot(FTrain1$`Duration in Mins`~FTrain1$Hourbracket, xlab="Hour of Day",
        ylab="Flight duration",main="Duration by Day vs Night",col=c("Blue","Cyan"))


boxplot(FTrain1$Price~FTrain1$Total_Stops, xlab="Hour of Day",
        ylab="Flight duration",main="Duration by Day vs Night",col=c("Blue","Cyan"))

```

## Independent variables that are singificant

Based on the data transformation and feature engineering we have done above , we can say that except the columns "Route" and "Additional Info", all other columns are significant in the model building. The same will get validated once we start building the model using Multiple linear regression, Decision Tree, Random Forest, Gradient Boost etc.


## Relationship between time of journey and Flight prices

Response to this section we have covered earlier. Flight prices are costlier during the day time and specific during the evening time. Also flight price on weekend are costlier compared to weekdays. Flight price in the morning hours 8-9 am and in evening 4-6 pm are higher compared to other time.

## Hypothesis Testing

# Flight Prices on Weekdays are cheaper than flight prices on weekends.

We did anova testing on the linear model built using Price and "DayofWeek" and found the P value very small and hence null hypothesis is rejected and we can say that flight price on weekends are costlier compared to weekdays


# Flight Prices during peak hours (9 AM till 9 PM ) are costlier than flights at other times.

We did a 2-tail t test for the same and found that P value is very small and less than 0.05 , hence null hypothesis is rejected and hence Flight price during peak hours 9am-9pm are higher than non peak hours ie 9pm-9am.



```{r}



pirateplot(formula = FTrain1$Price ~ Total_Stops, data=FTrain1,
           main="pirate plot of airline")

pirateplot(formula = FTrain1$Price ~ DayofWeek, data=FTrain1,
           main="pirate plot by day of week")

# Hypothesis testing 

# when we do hypothesis testing for the flight price to see if there is a significant different of flight price in day time versus night time, we found
# P value of less than 0.05 which means we reject the null hypothesis and accept alternative that there is significant price difference in flight during day time versus night time


t.test(formula = FTrain1$Price ~ Hourbracket,
       data = FTrain1,
       alternative = 'two.sided')

# Correlation between price and duration in mins shows that price of flight significantly depends on the total duration of flight

cor.test(formula = ~ Price + `Duration in Mins`,
         data = FTrain1)


# to check whether there is signficant difference in price over weekends compared to weekdays

Test= lm(formula = FTrain1$Price ~ DayofWeek,
                   data = FTrain1)

anova(Test)

# when we do Anova testing we found that P value is very small which indicates that flight price over weekend is significantly different compared to weekdays 

```

```{r}


ggplotGrid(ncol = 2,
  lapply(c("Airline","Source","Destination","Total_Stops","DayofWeek","Hourbracket"),
    function(col) {
        ggplot(FTrain1, aes_string(col)) + geom_bar() + coord_flip()
      
    }))


```

```{r}

# Numerical

qplot(FTrain1$Price, data = FTrain1, main=" Price Histogram Plot", xlab = "Price", ylab = "frequency")
plot(FTrain1$Price,FTrain1$`Duration in Mins`)


# Categorical

qplot(FTrain1$Airline, data = FTrain1, main=" Airline Bar plot", xlab = "Airline", ylab = "frequency")

Hday=as.factor(FTrain1$HourofDay)


# Numerical and Categorical together

# Plotting the price wrt categorical variable like Airline, Total stops, Source, Destination, Day of Week and Hour bracket 

qplot(Hday,FTrain1$Price, data = FTrain1, , geom="boxplot",main=" Price by Hour of day", xlab = "Hour of Day", ylab ="Price")



qplot(FTrain1$Airline,FTrain1$Price, data = FTrain1, , geom="boxplot",main=" Price by Airline", xlab = "Airline", ylab ="Price")
qplot(FTrain1$`Date of Travel`,FTrain1$Price, data = FTrain1, , geom="boxplot",main=" Price by Date of Travel", xlab = "Date of Travel", ylab ="Price")
qplot(FTrain1$Source,FTrain1$Price, data = FTrain1, , geom="boxplot",main=" Price by source", xlab = "Source", ylab ="Price")

ggplot(FTrain1, aes(x=Source, y=Price)) + geom_boxplot() + facet_grid(~Hourbracket) 
ggplot(FTrain1, aes(x=Destination, y=Price)) + geom_boxplot() + facet_grid(~Hourbracket) 


ggplot(FTrain1, aes(x=Hourbracket, y=Price)) + geom_boxplot() + facet_grid(~Airline)
ggplot(FTrain1, aes(x=Hourbracket, y=Price)) + geom_boxplot() + facet_grid(~DayofWeek)

qplot(FTrain1$Source,FTrain1$Price, data = FTrain1, , geom="boxplot",main=" Price by Source City", xlab = "Source City", ylab ="Price")

histogram(~FTrain1$Price|FTrain1$DayofWeek, data = FTrain1)


# Categorical against categorical 

qplot(FTrain1$Airline,fill=FTrain1$Source, data = FTrain1, geom="bar",
      main="Count of Airline by Source",
      xlab="Airline",
      ylab="Frequency")

qplot(FTrain1$Airline,fill=FTrain1$Destination, data = FTrain1, geom="bar",
      main="Count of Airline by Destination",
      xlab="Airline",
      ylab="Frequency")

qplot(FTrain1$Source,fill=FTrain1$Total_Stops, data = FTrain1, geom="bar",
      main="Count of Total Stops from Source",
      xlab="Source Cities",
      ylab="Frequency")

qplot(FTrain1$DayofWeek,fill=FTrain1$Hourbracket, data = FTrain1, geom="bar",
      main="Count of Day/Night by weekday",
      xlab="Day of Week",
      ylab="Frequency")


# Numerical against numerical

qplot(FTrain1$HourofDay, FTrain1$Price, data = FTrain1, color=FTrain1$Airline , main = "Price vs hour of day", xlab = "hour of day", ylab = "price")

qplot(FTrain1$Price, FTrain1$`Duration in Mins`, data = FTrain1, color=FTrain1$Airline , main = "Price vs duration", xlab = "Price", ylab = "Duration in mins")

qplot(FTrain1$Price, FTrain1$`Duration in Mins`, data = FTrain1, color=FTrain1$Total_Stops , main = "Price vs duration", xlab = "Price", ylab = "Duration in mins")

qplot(FTrain1$Price, FTrain1$`Duration in Mins`, data = FTrain1, color=FTrain1$Hourbracket , main = "Price vs duration", xlab = "Price", ylab = "Duration in mins")


ggplot(FTrain1, aes(x=Airline , y= Price, fill=DayofWeek))+geom_boxplot()

ggplot(FTrain1, aes(x=DayofWeek , y= Price, fill=Total_Stops))+geom_boxplot()


corrplot(cor(FTrain1[,c(11,12)]))
cor(FTrain1[,c(11,12)])     



```


```{r}


FTrain2=FTrain1

levels(FTrain2$Total_Stops)[levels(FTrain2$Total_Stops) == "non-stop"] <- "0"

FTrain2$TotStops=ifelse(FTrain2$Total_Stops==0,0 ,strapply(FTrain2$Total_Stops, "\\d+", as.numeric, simplify = TRUE))

```

```{r}

## Similar data manipulation for Test data set 

# Converting the non-stop to 0 

levels(FTest1$Total_Stops)[levels(FTest1$Total_Stops) == "non-stop"] <- "0"

FTest1$TotStops=ifelse(FTest1$Total_Stops==0,0 ,strapply(FTest1$Total_Stops, "\\d+", as.numeric, simplify = TRUE))

```



```{r}
# Getting the variables of interest in the data frame

FTrain3=FTrain2[,c(1,3,4,5,6,7,8,9,12,14,15,16,17,11)]

# Similar for Test Data set 

FTest2=FTest1[,c(1,3,4,5,6,7,8,9,11,12,13,14,15)]


```

```{r}

# Train and Test set split and doing the feature Engineering including Hot encoding and scaling 

set.seed(123)

sampleSplit=sample.split(FTrain3, SplitRatio = 0.7)

Flight.Train=subset(FTrain3,sampleSplit == TRUE)
Flight.Test=subset(FTrain3,sampleSplit == FALSE)

Flight.Train1=Flight.Train


Flight.Train1$`Date of Travel`=as.numeric(Flight.Train1$`Date of Travel`)
Flight.Train1$`Month of Travel`=as.numeric(Flight.Train1$`Month of Travel`)
Flight.Train1$`Year of Travel`=as.numeric(Flight.Train1$`Year of Travel`)


Flight.Train1$Dephr = as.numeric(format(Flight.Train1$Dep_Time, format='%H'))
Flight.Train1$Depmin = as.numeric(format(Flight.Train1$Dep_Time, format='%M'))
Flight.Train1$Arrhr = as.numeric(format(Flight.Train1$`Arrival-Time`, format='%H'))
Flight.Train1$Arrmin = as.numeric(format(Flight.Train1$`Arrival-Time`, format='%M'))


## Both Delhi and New Delhi are same , so replacing New Delhi with Delhi 

levels(Flight.Train1$Destination)[levels(Flight.Train1$Destination) == "New Delhi"] <- "Delhi"

Flight.Train1=Flight.Train1[,c(1:3,5:6,9,10,12:13,15:18,14)]


## Similar feature engineering for Test Set 

Flight.Test1=Flight.Test

Flight.Test1$`Date of Travel`=as.numeric(Flight.Test1$`Date of Travel`)
Flight.Test1$`Month of Travel`=as.numeric(Flight.Test1$`Month of Travel`)
Flight.Test1$`Year of Travel`=as.numeric(Flight.Test1$`Year of Travel`)


Flight.Test1$Dephr = as.numeric(format(Flight.Test1$Dep_Time, format='%H'))
Flight.Test1$Depmin = as.numeric(format(Flight.Test1$Dep_Time, format='%M'))
Flight.Test1$Arrhr = as.numeric(format(Flight.Test1$`Arrival-Time`, format='%H'))
Flight.Test1$Arrmin = as.numeric(format(Flight.Test1$`Arrival-Time`, format='%M'))


## Both Delhi and New Delhi are same , so replacing New Delhi with Delhi 

levels(Flight.Test1$Destination)[levels(Flight.Test1$Destination) == "New Delhi"] <- "Delhi"

Flight.Test1=Flight.Test1[,c(1:3,5:6,9,10,12:13,15:18,14)]
## Scaling and  Hot encoding for Train data

Dummyvar = dummyVars("~.", data = Flight.Train1)

Flight.Train1 = data.frame(predict(Dummyvar, newdata = Flight.Train1))


Flight.Train1[,-40]=lapply(Flight.Train1[,-40],function(x) if (is.numeric(x)){(x - min(x))/(max(x)-min(x))} else x)

Flight.Train1[is.na(Flight.Train1)]=0

## Scaling and hot encoding for Test data

Dummyvar1 = dummyVars("~.", data = Flight.Test1)

Flight.Test1 = data.frame(predict(Dummyvar1, newdata = Flight.Test1))

Flight.Test1[,-40]=lapply(Flight.Test1[,-40],function(x) if (is.numeric(x)){(x - min(x))/(max(x)-min(x))} else x)

Flight.Test1[is.na(Flight.Test1)]=0



```

```{r}

## Applying the same hot encoding on Test Data set which will be used for prediction

FTest3=FTest2

FTest3$`Date of Travel`=as.numeric(FTest3$`Date of Travel`)
FTest3$`Month of Travel`=as.numeric(FTest3$`Month of Travel`)
FTest3$`Year of Travel`=as.numeric(FTest3$`Year of Travel`)


FTest3$Dephr = as.numeric(format(FTest3$Dep_Time, format='%H'))
FTest3$Depmin = as.numeric(format(FTest3$Dep_Time, format='%M'))
FTest3$Arrhr = as.numeric(format(FTest3$Arrival_Time, format='%H'))
FTest3$Arrmin = as.numeric(format(FTest3$Arrival_Time, format='%M'))



## Both Delhi and New Delhi are same , so replacing New Delhi with Delhi 

levels(FTest3$Destination)[levels(FTest3$Destination) == "New Delhi"] <- "Delhi"

FTest3=FTest3[,c(1:3,5:6,9,10,12:13,14:17)]

Dummytest = dummyVars("~.", data = FTest3)

FTest3 = data.frame(predict(Dummytest, newdata = FTest3))


FTest3=data.frame(lapply(FTest3,function(x) if (is.numeric(x)){(x - min(x))/(max(x)-min(x))} else x))

FTest3[is.na(FTest3)]=0

```


## Multiple Linear Regression 

# Model Intrepretation 

For the model building , we take encoded data set set where all the categorical variables have been encoded with dummy variables as 0 or 1 . 
The intercept consider the effect of variables that we have not considered. We can intrepret the model such as For each one unit change of independent variable the price increase or decrease by the amount represented as slope keeping other variable constant

R square interpretation - 63% of the variation in the flight price is explained by the independent variables used in the model

We reject null hypothesis as t-stat has extremely low p-values which is p<0.05 and there is significant evidence that regression model exist

Adjusted R square shows the effect of adding more variables in the model. For eg current model is explained 63% by the independent variables which means remaining 40% of model is unexplained or by residuals.so we inflate the error component by multiplier which is division of Total degree of freedom by residual degree of freedom

Based on the model and P-stat we keep only the significant variables in the final model building and again compute the R2 and root mean square error 

We also check for correlation between the models using VIF 

RMSE - 2629.421

R2 - .633



```{r}

## Applying the multiple linear regression model on Train set and predicting using Test Set 

set.seed(123)

attach(Flight.Train1)

modelLR=lm(Flight.Train1$Price ~ . , data = Flight.Train1)
summary(modelLR)

modelLR1=lm(Price ~ Airline.Air.Asia+Airline.Air.India+Airline.GoAir+Airline.IndiGo+Airline.Jet.Airways+Airline.Multiple.carriers +Airline.SpiceJet+X.Date.of.Travel.+X.Month.of.Travel.+Source.Delhi+Source.Chennai+Source.Banglore+Source.Kolkata+DayofWeek.Friday+DayofWeek.Monday+DayofWeek.Sunday+DayofWeek.Tuesday+DayofWeek.Saturday+DayofWeek.Thursday+TotStops+Hourbracket.Day.Time+Depmin , data = Flight.Train1)
summary(modelLR1)

corrplot(cor(Flight.Train1[,-40]))
vif(modelLR1)

plot(modelLR1)

Predict.LR = predict(modelLR1,newdata = Flight.Test1[,-40])


## Calculating accuracy 

Mae.LR= sum(abs(Predict.LR - Flight.Test1[,40]))/length(Flight.Test1[,40])
Mae.LR

data.frame ( RMSE = RMSE(Predict.LR , Flight.Test1$Price),
             
             Rsquare = R2(Predict.LR , Flight.Test1$Price)
             
             )


```

## Ridge Regression 

# Regularization - 

Normal regression works by selecting the coefficients that minimize the loss function, however if the coefficient are large there may be chance of overfitting the training data and will not generalize well on unseen test data .To overcome this we will do regularization that will impact the large coefficients 

Ridge Regression - Loss function is minimized by adding a penalty parameter equivalent to square of magnitude of coefficients. The model will be tuned thru the hyperparameter lambda. This model will  generalize well on test data as it will be less sensitive to extreme variance.


RMSE - 2579
R2 - .634


```{r}

## Ridge Regression 


attach(Flight.Train1)

x = as.matrix(Flight.Train1[,-40])
y = Flight.Train1$Price


set.seed(123)

# Best lambda using cross validation

cv = cv.glmnet(x , y , alpha = 0)

# Display the best lamda value 

cv$lambda.min

# Fit the final model on training set 
 model.Ridge = glmnet(x , y, alpha = 0 , lambda = cv$lambda.min)
 
 
# Display model coefficients
 
coef(model.Ridge)

lambdas = 10^seq(2,-3, by= -.1)


# PRedicting on the test data set and calculate RMSE and R2 

Pred.Ridge = predict(model.Ridge, s = cv$lambda.min, newx = as.matrix(Flight.Test1[,-40]))

# Model performance metrics 


data.frame ( RMSE = RMSE(Pred.Ridge , Flight.Test1$Price),
             
             Rsquare = R2(Pred.Ridge , Flight.Test1$Price)
             
             )
```

## Lasso Regression

Lasso Regression - in this loss function is modified to minimize the complexity of model by limiting sum of absolute values of model coefficients

RMSE - 2626
R2 - .633

```{r}

## LAsso Regression 

# First step is to find the optimal lambda 

set.seed(123)

lambdas = 10^seq(2,-3, by= -.1)

# Setting alpha =1 in implementing the lasso regression

cv.Lasso = cv.glmnet( x , y , alpha = 1 , lambda = lambdas, standardize = TRUE, nfolds = 5)

# Best lambda using the cross validated output 

lambda_best = cv.Lasso$lambda.min
lambda_best

# Train the lasso model using the best lambda 

model.Lasso = glmnet(x, y , alpha = 1, lambda = lambda_best, standardize = TRUE)
coef(model.Lasso)


# PRedicting on the test data set and calculate RMSE and R2 

Pred.Lasso = predict(model.Lasso, s = lambda_best, newx = as.matrix(Flight.Test1[,-40]))


# Model performance metrics 


data.frame ( RMSE = RMSE(Pred.Lasso , Flight.Test1$Price),
             
             Rsquare = R2(Pred.Lasso , Flight.Test1$Price)
             
             )


```


## Elastic Net Regression

Elstic Net - It uses the properties of both ridge and lasso . It works by penalizing the model using both optimum alpha and lambda values.
We use caret package to find optimal  values and using the hyper tuning parametrs

RMSE - 2627
R2 - .633

```{r}

## USing Elastic Net Regresor 

set.seed(123)

# create training control object to specify how cross validation takes place

train_cont = trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 5,
                          search = "random",
                          verboseIter = TRUE)

# Build the elastic regression model using optimum alpha and lambda values . Argument tune length specifices 10 different combinations of value of alpha and lambda to be tested 

model.Elastic = train(Price ~ . , data = Flight.Train1,
                      method = "glmnet",
                      preProcess = c("center", "scale"),
                      tuneLength = 10,
                      trControl = train_cont)

# Best tuning parameter

model.Elastic$bestTune

plot(varImp(model.Elastic), top = 8, auto.key=TRUE, )

# PRedict using the test set and comparing the RMSE and R square 

Predict.Elastic = predict(model.Elastic,newdata = Flight.Test1[,-40])

data.frame ( RMSE = RMSE(Predict.Elastic , Flight.Test1$Price),
             
             Rsquare = R2(Predict.Elastic , Flight.Test1$Price)
             
             )



```

## KNN Regressor

KNN - used caret package , to find the optimal number of nearest neighbors to predict the value.
We used cross validation method  where 9 samples were used for train and one is used for test 

Minimum RMSE is achieved when K is 5 , we use both default method of RMSE and Rsquared method, and find out that Rsquare gives a slight better RMSE reduction

RMSE - 2116
R2 - .73


```{r}

# Using the KNN regressor model 

str(Flight.Train1)
attach(Flight.Train1)

trControl=trainControl(method = 'repeatedcv',
                       number = 10,
                       repeats = 3)

set.seed(100)

# Using the default method of RMSE 

fit.knn=train(Price~.,data=Flight.Train1,tuneGrid = expand.grid(k = c(3,5,7,11,13,15)),
              method = 'knn',
              trControl = trControl,
              preProc=c('center','scale'))

# Using the Rsquared method 

fit.knn1=train(Price~.,data=Flight.Train1,tuneGrid = expand.grid(k = c(3,5,7,11,13,15)),
              method = 'knn',
              metric = 'Rsquared',
              trControl = trControl,
              preProc=c('center','scale'))



fit.knn
fit.knn1
plot(fit.knn)
plot(fit.knn1)
varImp(fit.knn1)

# predicting the values using the Test set

Predict.knn = predict(fit.knn1,newdata = Flight.Test1[,-40])


## Calculating accuracy 

Mae.knn= sum(abs(Predict.knn - Flight.Test1[,40]))/length(Flight.Test1[,40])
Mae.knn

RMSE(Flight.Test1[,40],Predict.knn)
plot(Predict.knn ~ Flight.Test1$Price)



data.frame ( RMSE = RMSE(Predict.knn , Flight.Test1$Price),
             
             Rsquare = R2(Predict.knn , Flight.Test1$Price)
             
             )



```


```{r}

# creating another train and test data set which is original and in unscaled for Tree based algorithms 

Flight.Train2=Flight.Train
Flight.Test2=Flight.Test
levels(Flight.Train2$Destination)[levels(Flight.Train2$Destination) == "New Delhi"] <- "Delhi"
Flight.Train2$Destination=as.factor(Flight.Train2$Destination)

Flight.Train2$Dephr = as.numeric(format(Flight.Train2$Dep_Time, format='%H'))
Flight.Train2$Depmin = as.numeric(format(Flight.Train2$Dep_Time, format='%M'))
Flight.Train2$Arrhr = as.numeric(format(Flight.Train2$`Arrival-Time`, format='%H'))
Flight.Train2$Arrmin = as.numeric(format(Flight.Train2$`Arrival-Time`, format='%M'))
Flight.Train2$`Date of Travel`=as.numeric(Flight.Train2$`Date of Travel`)
Flight.Train2$`Month of Travel`=as.numeric(Flight.Train2$`Month of Travel`)

Flight.Test2$Dephr = as.numeric(format(Flight.Test2$Dep_Time, format='%H'))
Flight.Test2$Depmin = as.numeric(format(Flight.Test2$Dep_Time, format='%M'))
Flight.Test2$Arrhr = as.numeric(format(Flight.Test2$`Arrival-Time`, format='%H'))
Flight.Test2$Arrmin = as.numeric(format(Flight.Test2$`Arrival-Time`, format='%M'))
Flight.Test2$`Date of Travel`=as.numeric(Flight.Test2$`Date of Travel`)
Flight.Test2$`Month of Travel`=as.numeric(Flight.Test2$`Month of Travel`)


Flight.Train2=Flight.Train2[,c(1:3,5,6,9:10,12,13,15:18,14)]  

levels(Flight.Test2$Destination)[levels(Flight.Test2$Destination) == "New Delhi"] <- "Delhi"

Flight.Test2$Destination=as.factor(Flight.Test2$Destination)

Flight.Test2=Flight.Test2[,c(1:3,5,6,9:10,12,13,15:18,14)]


```


## Decision Trees

Decision Tree  - In this method the we split the tree starting from root node based on the condition that each split will have minimum impurity or less Gini index . Impure nodes will have more Gini index and have maximum variance .Pure node will have zero Gini index and impure as maximum Gini of 0.5
The split should be such that when we come from root node to child node the Gini gain is maximum for that particular split compared to other split for different predictors and Gini impurity reduces for that node compared to root node.

Decision Trees are powerful but have tendency to over fitting the data.We prune the tree to avoid overfitting based on complexity Parameter which says that error decrease should be more than Alpha ,if it is less than we stop adding the branches . Alpha is threshold and we generally take around 0.015

Common problem with Decision Trees are -

*Too many branches - chances of overfitting (capture of noise )
*Too less branch - under fitting ( Misclassification error )

Optimum number of branches are arrived based on pruning.CP is error reduction per node . Choose the node  where cross validation error is minimum  i.e xerror.

RMSE - 1885.7 
R2 -.787



```{r}
# Using Decision Tree algorithms wit rpart method

set.seed(123)

attach(Flight.Train2)

Tree.Flight = rpart(Price ~ . , data = Flight.Train2, method = "anova")
rpart.plot(Tree.Flight)

printcp(Tree.Flight)
plotcp(Tree.Flight)

Predict.Tree = predict(Tree.Flight,newdata = Flight.Test2[,-15])

data.frame ( RMSE = RMSE(Predict.Tree , Flight.Test2$Price),
             
             Rsquare = R2(Predict.Tree , Flight.Test2$Price)
             
             )


# Using the Caret package to predict the model and using the hyper parameter tuning 

set.seed(123)

trGrid = expand.grid(cp = seq(0,0.04,0.01))
tc = trainControl("cv",10)

Train.rpart = train( x = Flight.Train2[,-14],
                     y=Flight.Train2$Price,
                     method = "rpart",
                     tuneLength = 50,
                     trControl = tc,
                     tuneGrid = trGrid)

plot(Train.rpart)

varImp(Train.rpart)

plot(varImp(model.Elastic), top = 8, auto.key=TRUE, )

Predict.Tree1 = predict(Train.rpart,newdata = Flight.Test2[,-14])

data.frame ( RMSE = RMSE(Predict.Tree1 , Flight.Test2$Price),
             
             Rsquare = R2(Predict.Tree1 , Flight.Test2$Price)
             
             )


```

## Random Forest 

Random forest -this is an ensemble model technique in which we combine multiple models (Decision Tree in this case) to improve the predictive power of the model.

We use bootstrap aggregating (which is generating new training subset by sampling the data subset over and over again with replacement)
Through this we ensure that the each tree built within the forest is diverse and at the same time share communality that they have been built from the same subset of data.

We have to be careful with value of m "mtry in mode", if m is large variables become too correlated , if m is small, then predictive power of model decreases.
Optimal choice of m plays vital role in random forest model.

RMSE - 1654.5
R2 - .836





```{r}

## Building a random forest regressor 

set.seed(123)
# Building the RF with initial tree size of 501, mtry as 3 and terminal node size as 10 which will get refined
attach(Flight.Train2)


Model.rf=randomForest(Price ~ ., data = Flight.Train2 , ntree =501 , mtry =3, nodesize =10 , importance = TRUE)
print(Model.rf)

plot(Model.rf)

importance(Model.rf)
varImpPlot(Model.rf)

## Tune the Random forest model using the graph for optimum number of trees 

set.seed(123)
Model.trf=tuneRF( x = Flight.Train2[,-14], y = Flight.Train2$Price , mtryStart = 3 , stepFactor = 1.5 , ntreeTry = 271 ,
                  improve = 0.0001 , nodesize =10 ,trace = TRUE , plot = TRUE,
                  doBest = TRUE , importance = TRUE)

print(Model.trf)
importance(Model.trf)
varImpPlot(Model.trf)


## predicting on Test set using the tuned Random Forest

Predict.RF = predict(Model.trf,newdata = Flight.Test2[,-14])

data.frame ( RMSE = RMSE(Predict.RF , Flight.Test2$Price),
             
             Rsquare = R2(Predict.RF , Flight.Test2$Price)
             
             )

```

## Boosting method 

# Gradient Boosting 

Boosting - we will do boosting to sequentially train the weak learners.Difference in bagging and boosting is that bagging is parallel and boosting is sequential

Gradient boosting method - It builds on each model,  trying to fit the next model based on the residuals of previous model. We will use several tuning parameters to arrive at optimal model performance

RMSE - 2388.8
R2 - .66



```{r}

## Runiing the GBM model for model performance and prediction

library(gbm)

set.seed(123)

Model.gbm = gbm( formula = Flight.Train2$Price ~ . , 
                 distribution = "gaussian",
                 data = Flight.Train2,
                 n.trees = 10000,
                 interaction.depth = 1,
                 shrinkage = 0.001,
                 cv.folds = 5,
                 n.cores = NULL,  # Null uses all cores by default
                 verbose = FALSE)



Predict.gbm = predict(Model.gbm,newdata = Flight.Test2[,-14])

data.frame ( RMSE = RMSE(Predict.gbm , Flight.Test2$Price),
             
             Rsquare = R2(Predict.gbm , Flight.Test2$Price)
             
             )

```

## Extreme gradient Boosting 

Extreme gradient boosting - specialized implementation of gradient boosting decision trees designed for performance . Types are gradient boosting ,stochastic and regularized boosting. Some of advantages of using XGboost are

*Parallel computing - it is enabled with parallel processing
*Regularization - it is used to avoid overfitting in linear and tree based models
*Enabled cross validation - it is enabled with internal CV and not needed any additional package
*Missing values - model can handle missing values
*Tree pruning - it grows the tree upto a max depth and then prune backward until improvement in loss function.
*Bias and Variance - Unlike bagging model like Random forest which takes care of overfitting (high variance) but still retains some bias, Boosting method can handle both bias and variance

We will use several hyper parameters to tune the model . These values can be further ioptimized to get a most optimum XGBoost model. In my current case i have take learning rate as 0.045, depth as 6 and number of rounds as 650 

RMSE - 1677
R2 - .832




```{r}
# Running the XG boost model 

library(xgboost)
library(Matrix)

## XGboost works on the numeric variables so we have already done hot encoding and also we will convert the data frame to matrix type.

set.seed(123)

matrix.train =as.matrix(Flight.Train1[,-c(10,40)])
matrix.label=as.matrix(Flight.Train1[,40])

matrix.test = as.matrix(Flight.Test1[,-c(10,40)])

Model.xgb = xgboost( data = matrix.train,
                     label = matrix.label,
                     eta=0.001,
                     max_depth=3,
                     min_child_weight=3,
                     nrounds = 5000,
                     nfold=5,
                     objective ="reg:linear",
                     verbose = 0, # silent
                     early_stopping_rounds=10, # stop if no improvement for 10 consecutive trees
                     )



## Tuning the XG Boost model for nrounds, learning rate and maximum depth

lr =c(0.001,.01,.1,.3,.5,.7,1)
md=c(1,3,5,7,9)
nr=c(2,50,100,1000,4000,8000,14000)


for (i in md) {

Model.xgb = xgboost( data = matrix.train,
                     label = matrix.label,
                     eta=.7,
                     max_depth=5,
                     min_child_weight=3,
                     nrounds = 1000,
                     nfold=5,
                     objective ="reg:linear",
                     verbose = 0, # silent
                     early_stopping_rounds=10, # stop if no improvement for 10 consecutive trees
                     )

}

## Best fit xgboost model

BModel.xgb =  xgboost( data = matrix.train,
                     label = matrix.label,
                     eta=.045,
                     max_depth=6,
                     min_child_weight=3,
                     nrounds = 650,
                     nfold=5,
                     objective ="reg:linear",
                     verbose = 0, # silent
                     early_stopping_rounds=10, # stop if no improvement for 10 consecutive trees
                     )


Predict.xgb = predict(BModel.xgb,newdata = matrix.test)

importance = xgb.importance(feature_names = colnames(matrix.train),model = BModel.xgb)
xgb.plot.importance(importance_matrix = importance, top_n = 10)

data.frame ( RMSE = RMSE(Predict.xgb , Flight.Test1$Price),
             
             Rsquare = R2(Predict.xgb , Flight.Test1$Price)
             
             )



```

## Final model building for predicting the Airfare prices for Test dataset.




```{r}
# Final model intrepretation 

Model = c("Linear","Ridge", "Lasso","Elastic-Net","KNN","Decision Tree","Random Forest","GBM","XGBoost")
RMSE = c(2629.42,2579.11,2626,2627,2116,1885,1654.5,2388,1677)
Rsquare=c(0.633,0.634,.633,.633,.73,.787,.836,.66,.832)

FModel = data.frame(Model,RMSE,Rsquare)

barplot.default(FModel$RMSE , names.arg = Model)
barplot.default(FModel$Rsquare , names.arg = Model)



Test.matrix = as.matrix(FTest3)
Predict.final = predict(BModel.xgb,newdata = Test.matrix)

FTest2$Predicted.price =Predict.final

write.csv(FTest2, "C:/Users/nitin/OneDrive/Desktop/R and python Programming/Mentor Session, Books and PDFs/capstone Project/PredictedPrice.csv", row.names = TRUE)


```




