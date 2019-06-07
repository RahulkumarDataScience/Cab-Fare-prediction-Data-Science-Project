#remove all the objects stored
rm(list=ls())

#set current working directory
setwd("C:/Users/Rahhy/Downloads")

#Current working directory
getwd()

#Install multiple packages at a time
#install.packages(c("dplyr","plyr","reshape","ggplot2","data.table"))

#import libraries
library(tidyverse)
library(readr) #This will help import csv data
library(gridExtra)
library(corrgram)
library(caret)
library(mlbench)
library(e1071)
library(rpart)
library(randomForest)
library(ggfortify)
library(dplyr)
library(ggplot2)
library(data.table)
library(purrr)
library(gbm)

#Lets name our training dataset as train_cab
train_cab <- read_csv("C:/Users/Rahhy/Downloads/train_cab.csv")

#Check the first 10 observations
#View(train_cab)

#Also  name our testing dataset as test
test <-read_csv("C:/Users/Rahhy/Downloads/test.csv")

#Check the first 10 observations
#View(test)

#Getting the column names of the dataset
colnames(train_cab)
colnames(test)

#Getting the structure of the dataset
str(train_cab)
str(test)

#Getting the number of variables and obervation in the datasets
dim(train_cab)
dim(test)

#datatypes of train and test dataset
map(train_cab, class)
map(test, class)

#summaries of train and test data's

summary(train_cab)
summary(test)

#Clearly in the summary it can be seen there are so many anamolies:
# 1. fare amount is negative at few places and at a certain place it is 54343 which is not possible.
# 2. There are missing values in fare_amount and passenger_count
# 3. Number of passengers in few rows is 500 which is not possible for a cab to carry.

##################################Missing Values Analysis###############################################

#checking presence of Missing values in training set

missing_val = data.frame(apply(train_cab,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(train_cab)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]

## < - passenger_count, fare_amount is having missing values

#Plot these Missing values
ggplot(data = missing_val[1:3,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
ggtitle("Missing data percentage (Train)") + theme_bw()

#checking presence of Missing values in test set

missing_val1 = data.frame(apply(test,2,function(x){sum(is.na(x))}))
missing_val1$Columns = row.names(missing_val1)
names(missing_val1)[1] =  "Missing_percentage"
missing_val1$Missing_percentage = (missing_val1$Missing_percentage/nrow(test)) * 100
missing_val1 = missing_val1[order(-missing_val1$Missing_percentage),]
row.names(missing_val1) = NULL
missing_val1 = missing_val1[,c(2,1)]

## < - No missing values in test data 

#Mean Method Imputation
train_cab$passenger_count[is.na(train_cab$passenger_count)] = mean(train_cab$passenger_count, na.rm = T)
train_cab$fare_amount[is.na(train_cab$fare_amount)] = mean(train_cab$fare_amount, na.rm = T)

#check the number of Missing values after Imputation
sum(is.na(train_cab))

## < - now no missing values are present

#Now lets convert our pickup_datetime to numeric
train_cab$pickup_datetime = gsub( " UTC", "", as.character(train_cab$pickup_datetime))
test$pickup_datetime = gsub( " UTC", "", as.character(test$pickup_datetime))

train_cab$pickup_datetime <- as.numeric(as.POSIXct(train_cab$pickup_datetime,format="%Y-%m-%d %H:%M:%S"))
test$pickup_datetime <- as.numeric(as.POSIXct(test$pickup_datetime,format="%Y-%m-%d %H:%M:%S"))

train_cab$pickup_datetime[is.na(train_cab$pickup_datetime)] = mean(train_cab$pickup_datetime, na.rm = T)


################################## Removing Anamolies ###############################################

#Remove unwanted values and bring our dataset in proper shape
train_cab = train_cab[((train_cab['fare_amount'] >= 2.50) & (train_cab['fare_amount'] <=600)) & ((train_cab['pickup_longitude']>= -74.930)  &  (train_cab['pickup_longitude'] <= 74.025)) & ((train_cab['pickup_latitude']) >= -74.83 & (train_cab['pickup_latitude'] <= 74.00)) & ((train_cab['dropoff_longitude']) >= -74.83 & (train_cab['dropoff_longitude'] <= 74.00)) & ((train_cab['dropoff_latitude'] >= -74.00)  & (train_cab['dropoff_latitude'] <= 74.00)) & ((train_cab['passenger_count']  >= 1) & (train_cab['passenger_count'] <= 7)),]

train_cab$pickup_latitude = gsub( "0.00000", "0", as.numeric(train_cab$pickup_latitude))
train_cab$dropoff_latitude = gsub( "0.00000", "0", as.numeric(train_cab$dropoff_latitude))
train_cab$pickup_longitude= gsub( "0.00000", "0", as.numeric(train_cab$pickup_longitude))
train_cab$dropoff_longitude = gsub( "0.00000", "0", as.numeric(train_cab$dropoff_longitude))

#Remove rows containing 0 as value
train_cab = train_cab[apply(train_cab, 1, function(row) all(row !=0)),]
train_cab <- data.frame(sapply(train_cab, function(x) as.numeric(as.character(x))))
sapply(train_cab, class)

test = test[apply(test, 1, function(row) all(row !=0)),]
test <- data.frame(sapply(test, function(x) as.numeric(as.character(x))))
sapply(test, class)

############################################ Outlier Analysis #############################################

## BoxPlots - Distribution and Outlier Check

numeric_index = sapply(train_cab,is.numeric) 

numeric_data = train_cab[,numeric_index]

cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "fare_amount" , group = 1), data = subset(train_cab))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="fare_amount")+
           ggtitle(paste("Box plot of fare_amount for",cnames[i])))
}

gridExtra::grid.arrange(gn2,gn3,ncol=2)
gridExtra::grid.arrange(gn6,gn7,ncol=2)
gridExtra::grid.arrange(gn4,gn5,ncol=2)

for(i in cnames){
     print(i)
     val = train_cab[,i][train_cab[,i] %in% boxplot.stats(train_cab[,i])$out]
     print(length(val))
     train_cab[,i][train_cab[,i] %in% val] = NA
                }
train_cab <- data.frame(sapply(train_cab, function(x) ifelse(is.na(x), mean( x, na.rm = TRUE),x)))

num = sapply(test,is.numeric) 

num_data = test[,num]

cnamestest = colnames(num_data)

for(c in cnamestest){
  print(c)
  val = test[,c][test[,c] %in% boxplot.stats(test[,c])$out]
  print(length(val))
  test[,c][test[,c] %in% val] = NA
                    }
test <- data.frame(sapply(test, function(y) ifelse(is.na(y), mean(y, na.rm = TRUE),y)))

train_cab$passenger_count =  round(train_cab$passenger_count)
test$passenger_count =  round(test$passenger_count)

ggplot(train_cab, aes(x = fare_amount, y = pickup_latitude, group = 1)) +  geom_boxplot()
ggplot(train_cab, aes(x = fare_amount, y = pickup_datetime, group = 1)) +  geom_boxplot()
ggplot(train_cab, aes(x = fare_amount, y = pickup_longitude  , group = 1)) +  geom_boxplot()
ggplot(train_cab, aes(x = fare_amount, y = dropoff_longitude  , group = 1)) +  geom_boxplot()
ggplot(train_cab, aes(x = fare_amount, y = dropoff_latitude  , group = 1)) +  geom_boxplot()
ggplot(train_cab, aes(x = fare_amount, y = passenger_count  , group = 1)) +  geom_boxplot()

############################################ Histogram plots #############################################

hist(train_cab$fare_amount)
hist(train_cab$pickup_latitude)
hist(train_cab$pickup_longitude)
hist(train_cab$dropoff_latitude)
hist(train_cab$dropoff_longitude)
hist(train_cab$passenger_count)

################################## Feature Selection ################################################
## Correlation Plot 
corrgram(train_cab[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

train_cab = subset(train_cab, select = -c(pickup_longitude))

############################################ Scaling the data #######################################
#Standardisation
for(i in colnames(train_cab))
{
  print(i)
  train_cab[,i] = (train_cab[,i] - min(train_cab[,i]))/(max(train_cab[,i])-min(train_cab[,i]))
}

##### Splitting the data into train and test

n = nrow(train_cab)
trainIndex = sample(1:n, size = round(0.8*n), replace=FALSE)
train = train_cab[trainIndex ,]
test1 = train_cab[-trainIndex ,]

X_train = subset(train,select = -c(fare_amount))
y_train = subset(train,select = c(fare_amount))

X_test = subset(test1,select = -c(fare_amount))
y_test = subset(test1,select = c(fare_amount))

##### Using PCA

#principal component analysis
prin_comp = prcomp(X_train)

#compute standard deviation of each principal component
std_dev = prin_comp$sdev

#compute variance
pr_var = std_dev^2

#proportion of variance explained
prop_varex = pr_var/sum(pr_var)

#cdf plot for principle components
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#add a training set with principal components
X_train.data = data.frame( prin_comp$x)

# From the above plot selecting 45 components since it explains almost 95+ % data variance
X_train.data =X_train.data[,1:5]

#transform test into PCA
X_test.data = predict(prin_comp, newdata = X_test)
X_test.data = as.data.frame(X_test.data)

#select the first 5 components
X_test.data=X_test.data[,1:5]

######################################## Machine learning model##########################################

X_train.data$fare_amount = paste(y_train$fare_amount)
X_test.data$fare_amount = paste(y_test$fare_amount)

#### KNN
#Develop Model on training data
fit_LR = knnreg(fare_amount ~ ., data = X_train.data)
#Lets predict for testing data
pred_LR_test = predict(fit_LR,X_test.data)
# Results 
print(postResample(pred = pred_LR_test, obs =y_test$fare_amount))

###### Multiple Linear Regression
#Develop Model on training data
set.seed(100)
#Develop Model on training data
fit_LR = lm(fare_amount ~ ., data = X_train.data)
#Lets predict for testing data
pred_LR_test = predict(fit_LR,X_test.data)
# Results 
print(postResample(pred = pred_LR_test, obs =y_test$fare_amount))

###### SVM
#Develop Model on training data
fit_LR = svm(fare_amount~ ., data = train, scale = FALSE,type = "eps")
#Lets predict for testing data
pred_LR_test = predict(fit_LR,X_test)
# Results X_test
print(postResample(pred = pred_LR_test, obs =y_test$fare_amount))


###### Decision Tree

#Develop Model on training data
fit_DT = rpart(fare_amount ~., data = X_train.data, method = 'anova')
pred_DT_test = predict(fit_DT,X_test.data)
# Results
print(postResample(pred = pred_DT_test, obs = y_test$fare_amount))

###### Random Forest
#Develop Model on training data
fit_DT = randomForest(fare_amount ~., data = train)
pred_DT_test = predict(fit_DT,X_test)
# Results
print(postResample(pred = pred_DT_test, obs = y_test$fare_amount))

###### GBDT

#Develop Model on training data
fit_GBDT = gbm(fare_amount ~., data = X_train.data, n.trees = 500, interaction.depth = 2)
#Lets predict for testing data
pred_GBDT_test = predict(fit_GBDT,X_test.data, n.trees = 500)
# For testing data 
print(postResample(pred = pred_GBDT_test, obs = y_test$fare_amount))




