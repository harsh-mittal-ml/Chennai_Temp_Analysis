
data <- read.csv("C:/Users/harsh.hm.mittal/OneDrive/Desktop/Predictive Analytics - Regression and Classification/Chennai_1990_2022_Madras.csv", stringsAsFactors=TRUE)
str(data)

data$time=as.Date(data$time,format = "%d-%m-%Y")
str(data)

n = nrow(data)
data$tms = 1:n
data$tms = data$tms - mean(data$tms)

## split the data into train and test

data_train = subset(data,time<=as.Date("2015-12-31"))
tail(data_train)

data_test = subset(data,time>as.Date("2015-12-31"))


## visualisation

plot(data_train$time[1:(3*365)],data_train$tavg[1:(3*365)],pch=20,
     xlab='',ylab='avg temp')


omega = 2*pi/365


## modl 1

mod1 = lm(tavg ~ tms + sin(omega*tms) + cos(omega*tms), data = data_train)
sum = summary(mod1)
sum
sigma=sum$sigma

data_train$fitted.values=NA #creaing a new col with all NAs
data_train[rownames(mod1$model),'fitted.values']=mod1$fitted.values #filling that col with fitted values

plot(data_train$time[1:(3*365)], data_train$tavg[1:(3*365)]
     ,pch=20,col='grey'
       ,xlab='',ylab='avg temp')

lines(data_train$time[1:(3*365)], data_train$fitted.values[1:(3*365)]
      ,col='red',lwd=2)

lines(data_train$time[1:(3*365)] 
      ,data_train$fitted.values[1:(3*365)]-1.96*sigma
      ,col='brown',lwd=2,lty=2)

lines(data_train$time[1:(3*365)] 
      ,data_train$fitted.values[1:(3*365)]+1.96*sigma
      ,col='brown',lwd=2,lty=2)

#### do prediction in test data


data_test$pred = predict(mod1,newdata = data_test)


plot(data_test$time, data_test$tavg
     ,pch=20,col='grey'
       ,xlab='',ylab='avg temp')

lines(data_test$time
      ,data_test$pred
      ,col='red',lwd=2)

lines(data_test$time
      ,data_test$pred-1.96*sigma
      ,col='blue',lwd=2,lty=2)
lines(data_test$time
      ,data_test$pred+1.96*sigma
      ,col='blue',lwd=2,lty=2)
#sin and cos were our engineered features
