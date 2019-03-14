rm(list=ls(all=TRUE))
setwd("E:\\CUTES\\cute2")#directory where you have kept your data
#READING DATA INTO CSV FILESdata<-read.csv("TS_Train-1540270465813.csv",header=T)
str(data)
View(data)
summary(data)

#data split to train and test 70:30
Train=data[c(1:63),]
Test=data[c(64:90),]
View(Train) 
View(Test) 


# Note that our target variable "value" is a numeric vector. 
#We are now using this column only to build the models.


traindatats <- ts(Train$value, frequency =7)#this is time series train data
#visualize the data
plot(traindatats)
plot(traindatats,type="l",lwd=3,col="blue",xlab="day",ylab="value", main="Time series plot") 

# Before we building the models, let us focus on visualization.
#We first decompose the data in to Trend and seasonality

Valuedecomp <- decompose(traindatats)
plot(Valuedecomp)


#                  MODEL BUILDING - HOLT WINTER'S METHOD


holtwin_value<-  HoltWinters(traindatats, beta=FALSE, gamma=TRUE,
                             seasonal="additive") #If data has sesoanlity component

## Analysis on train data
#holtwin_pred=holtwin_forecast_1$fitted
#head(holtwin_pred)
#View(holtwin_pred)

##Analysis on test data

library(forecast)
holtwin_forecasts = forecast(holtwin_value,h=7)
train_predict <- data.frame(holtwin_value$fitted)[1]
train_act <- traindatats[1:63] #doubt
plot(holtwin_forecasts)

##analyzing the model
library(DMwR)
regr.eval(train_act, train_predict)
#regr.eval(Train$value,holtwin_pred)


## AUTO ARIMA approach
par(mfrow=c(1,2))
autoarima_model <- auto.arima(ts_data, ic='aic')
acf(autoarima_model$residuals)
pacf(autoarima_model$residuals)
autoarima_model

##Use of ACF and PACF this will enable to see if any patterns
#acf(as.numeric(autoarima_model$residual) ,lag.max= 15, main="ACF Plot")

#pacf(as.numeric(autoarima_model$residual) ,lag.max= 15, main="PACF Plot")

#forecast_autoarima = forecast(autoarima_model, h=7) 
#regr.eval(Test$value,data.frame(forecast_autoarima)$point.Forecast)

#plot(forecast_autoarima)





# for building arima models first data should be stationary, if still trend  and seasonality still left build arima models

#residuals or errors should be random ,in acf pcaf we will check -residuals should show acf and pacf very low coreleation

#When there is trend there is auto regression AR
#If there is lot of randomness or fluctuations then moving averages are the best