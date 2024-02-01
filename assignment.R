#Task 1. Exploratory data analysis
#Install and Load libraries 
library(tidyverse) 
install.packages("GGally") 
library("GGally")
install.packages("caret")
library(caret)
install.packages("purrr")
library(purrr)
install.packages("randomForest")
library(randomForest)
install.packages("missForest")
library(missForest)
install.packages("ranger")
library(ranger)

#supress warnings
options(warn=-1)

# Load data
bikedata <- read_csv("bike.csv")

#display first 5 rows of dataset
head(bikedata,5)
#display names of columns in the dataset
names(bikedata)
#display dimension of dataset
dim(bikedata)
#summary of dataset (mean, median, std, quartile)
summary(bikedata)
#display structure of dataset
str(bikedata)

#Rename the columns for easy understanding
names(bikedata)<-c('instant','dateday','season','year','month','holiday','weekday','workingday','weather','temperature','atemperature','humidity','windspeed','casual','registered','count')

#display first 5 rows to see newly named columns
head(bikedata)

# Data type conversion of attributes
bikedata <- bikedata %>% 
  mutate(
    instant = as.integer(instant),
    dateday = as.Date(dateday),
    season = as.factor(season),
    year = as.factor(year),
    month = as.factor(month),
    holiday = as.factor(holiday),
    weekday = as.factor(weekday),
    workingday = as.factor(workingday),
    weather = as.factor(weather),
    temperature = as.numeric(temperature),
    atemperature = as.numeric(atemperature),
    humidity = as.numeric(humidity),
    windspeed = as.numeric(windspeed),
    casual = as.integer(casual),
    registered = as.integer(registered),
    count = as.integer(count)
  )

#read modified dataset
head(bikedata)
str(bikedata)
dim(bikedata)

# Missing value analysis
summary(bikedata)

# Count missing values per column
missingpercol <- colSums(is.na(bikedata))
print(missingpercol)

# Count missing values in dataset
missing<-data.frame(apply(bikedata,2,function(x){sum(is.na(x))}))
names(missing)[1]='missingval'
missing


# Task 2. Attributes distribution and trend
# Monthly distribution of total bikes rented

#Month wise weekday distribution
ggplot(bikedata,aes(x=month,y=count, fill=weekday))+theme_bw( )+geom_col()+
  labs(x='Month',y='TotalCount',title='Month wiseweekday rental distribution') 
#Month wise seasonal distribution
ggplot(bikedata,aes(x=month,y=count, fill=season))+theme_bw( )+geom_col()+
  labs(x='Month',y='TotalCount',title='Month wise Seasonal rental distribution') 

#Yearly distribution of total bikes rented

#Year wise seasonal distribution 
ggplot(bikedata,aes(x=year,y=count,fill=season))+theme_bw( )+geom_col()+
  labs(x='Year',y='TotalCount',title='Year wise seasonal rental distribution') 

#Seasonal distribution of total bikes rented

# Season wise casual distribution
ggplot(bikedata,aes(x=season,y=count,fill=casual))+theme_bw( )+geom_col()+
  labs(x='Season',y='TotalCount',title='Season wise casual rental distribution') 
# Season wise registered distribution 
ggplot(bikedata,aes(x=season,y=count,fill=registered))+theme_bw( )+geom_col()+
  labs(x='Season',y='TotalCount',title='Season wise registered rental distribution') 
# season wise weather distribution
ggplot(bikedata,aes(x=season,y=count,fill=weather))+theme_bw( )+geom_col()+
  labs(x='Season',y='TotalCount',title='Season wise weather rental distribution') 
# Season wise weekday distribution
ggplot(bikedata,aes(x=season,y=count, fill=weekday))+theme_bw( )+geom_col()+
  labs(x='Season',y='TotalCount',title='Season wise weekday rental distribution') 

#Holiday distribution 

# Holiday wise seasonal distribution
ggplot(bikedata,aes(x=holiday,y=count,fill=season))+theme_bw( )+geom_col()+
  labs(x='Holiday',y='TotalCount',title='Holiday wise seasonal rental distribution') 
# Holiday wise Weather distribution
ggplot(bikedata,aes(x=holiday,y=count,fill=weather))+theme_bw( )+geom_col()+
  labs(x='Holiday',y='TotalCount',title='Holiday-weather wise rental distribution') 

#Weather distribution

#Weather wise seasonal distribution
ggplot(bikedata,aes(x=weather,y=count,fill=season))+theme_bw( )+geom_col()+ 
  labs(x='Weather',y='TotalCount',title='Weather wise seasonal rental distribution') 


#Boxplot for outlier analysis
#1 col 1 row
par(mfrow=c(1, 1))
#Total count
boxplot(bikedata$count,main='Totalcount',sub=paste(boxplot.stats(bikedata$count)$out))

#2 cols 2 rows
par(mfrow=c(2,2))

#Temperature
boxplot(bikedata$temperature, main="Temperature",sub=paste(boxplot.stats(bikedata$temperature)$out))

#Humidity
boxplot(bikedata$humidity,main="Humidity",sub=paste(boxplot.stats(bikedata$humidity)$out))

#Windspeed
boxplot(bikedata$windspeed,main="Windspeed",sub=paste(boxplot.stats(bikedata$windspeed)$out))

#Seasonal
boxplot(bikedata$season,main="Season",sub=paste(boxplot.stats(bikedata$season)$out))

#Yearly
boxplot(bikedata$year,main="Year",sub=paste(boxplot.stats(bikedata$year)$out))

#Monthly
boxplot(bikedata$month,main="Month",sub=paste(boxplot.stats(bikedata$month)$out))

#Weather situation
boxplot(bikedata$weather,main="Weather",sub=paste(boxplot.stats(bikedata$weather)$out))

#Weekday
boxplot(bikedata$weekday,main="Weekday",sub=paste(boxplot.stats(bikedata$weekday)$out))

#Pair plots
options(repr.plot.width=10, repr.plot.height=10) 

#pairs()
pairs.default(bikedata[,c(10:16)], col = c("orange", "blue"), main='Pairs Pairplot')

#ggpairs()
ggpairs(bikedata[,c(10:16)], mapping=ggplot2::aes(colour = c("orange")), title='GGpairs Pairplot')

# Correlation Analysis
cor <- cor(bikedata[,c(10:16)]) 
cor

#Preprocessing
dim(bikedata)
#windspeed and humidity have outliers
windhum <- subset(bikedata, select = c('windspeed', 'humidity'))
windhum[] <- lapply(windhum, function(x) replace(x, x %in% boxplot.stats(x)$out, NA)) #replace with NA
#read data
dim(bikedata)
head(bikedata)


#Remove the windspeed and humidity
n1 = subset(bikedata,select=-c(windspeed,humidity))

#Combine n1 and windhum 
bikedf = cbind(n1,windhum)
head(bikedf)

#display dimension before and after dropping ourlier
cat('Shape after dropping outlier:',dim(windhum))
cat('Shape before dropping outlier:',dim( drop_na(windhum)))

#Task 3. Split the dataset into train and test dataset

#index (70-30)
trainind<-sample(1:nrow(bikedf),0.7*nrow(bikedf))

#training data
traindata<-bikedf[trainind,]
#testing data
testdata<-bikedf[-trainind,]
#dimensions and read data
dim(traindata)
dim(testdata)
head(traindata,5)
head(testdata,5)

#Train attributes new 
train<-subset(traindata,select=c('season','year','month','holiday', 'weekday','workingday','weather','temperature','humidity','windspeed','count'))
#Test attributes new
test<-subset(testdata,select=c('season','year','month','holiday','weekday','workingday','weather','temperature','humidity','windspeed','count'))

#read new data
head(train,5)
head(test,5)

#Train categorical attributes new
traincat<-subset(train,select=c('season','holiday','workingday','weather','year'))
#Train numerical attributes new
trainnum<-subset(train,select=c('weekday','month','temperature','humidity','windspeed','count'))
#Test categorical attributes new
testcat<-subset(test,select=c('season','holiday','workingday','weather','year'))
#Test numerical attributes new
testnum<-subset(test,select=c('weekday','month','temperature', 'humidity','windspeed','count'))


#dummy variables 
other<-c('month','weekday','temperature','humidity','windspeed','count')

#Train encode
set.seed(2626)

#Categorical 
var<-setdiff(colnames(train),c(train$count,other))

#formula for dummy variables
f <- paste('~', paste(var, collapse = ' + '))

#create dummy variables and encoding
encoder<-dummyVars(as.formula(f), train)

#encode attributes predict
encodeatt<-predict(encoder,train)

#Bind
train_enc_att<-cbind(trainnum,encodeatt)
head(train_enc_att,5)

#Test encode
set.seed(5662)
#Categorical 
var<-setdiff(colnames(test),c(test$count,other))

#formula for dummy variables
f<- paste('~',paste(vars,collapse='+'))

#create dummy variable and encoding
encoder<-dummyVars(as.formula(f),test)

#encoder attributes predict
encodeatt<-predict(encoder,test)

#Bind
test_enc_att<-cbind(testnum,encodeatt)
head(test_enc_att,5)

#Task 4. Create a model using the random forest algorithm
set.seed(42)

#Impute missing values using miss forest
train_enc_att_imputed <- missForest(train_enc_att)$ximp

#Random Forest model
rfmod <- randomForest(count ~ ., train_enc_att_imputed, importance = TRUE, ntree = 200)
rfmod

#Cross validation for random forest
set.seed(6772)

#CV 
train.control<-trainControl(method='CV',number=3)

#CV prediction
rfCVpred<-train(count~.,train_enc_att_imputed,method='ranger',trControl=train.control)
rfCVpred

#CV prediction plot
res<-resid(rfCVpred)
ytrain<-train_enc_att_imputed$count
plot(ytrain,res,xlab='Observed',ylab='Residuals',main='Cross validation prediction plot')
abline(0,0)

#Task 5. Predict the performance of the model on the test dataset

# Impute missing values using missForest
test_enc_att_imputed <- missForest(test_enc_att)$ximp
ytest<-test_enc_att_imputed$count

set.seed(7889)
#Prediction
rfpred<-predict(rfmod,test_enc_att_imputed[,-c(6)])
head(rfpred,5)

set.seed(667)
#Root mean squared error
rmse<-RMSE(ytest,rfpred)
print(rmse)

#Mean squared error
mae<-MAE(ytest,rfpred)
print(mae)

#Residual plot
resi<-ytest-rfpred
plot(ytest,resi,xlab='Observed',ylab='Residuals',main='Residual plot')
abline(0,0)

#Final Random Forest model prediction
final=data.frame(ytest,rfpred)
write.csv(final,'Bike_Rental_Prediction_R.CSV',row.names=F)
final