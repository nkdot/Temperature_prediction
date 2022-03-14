# references 
# https://www.statology.org/remove-rows-in-r/
# https://stackoverflow.com/questions/23279550/select-every-nth-row-from-dataframe
# https://r-charts.com/base-r/axes/
# https://www.datamentor.io/r-programming/matrix/

# import libraries 
library(forecast) 
library(xts) 
library(TTR)

# import data set 
#path_to_file = '/Users/matth/Documents/CSUEB/BAN 673 - Time Series/Project'
path_to_file='C:/Users/tempt/Desktop/MSBA/Spring2022/BAN673/Project/australia_weather_dataset'
setwd(path_to_file) 

                                                #preprocesssing
###missing records in daily data
temp.data1 = read.csv('daily-minimum-temperatures.csv')
temp.data1$Date<-as.Date(temp.data1$Date,tryFormats=c("%m/%d/%Y","%m/%d/%Y"))
date_range<-seq(min(temp.data1$Date),max(temp.data1$Date),by=1)
missing_dates<-date_range[!date_range %in% temp.data1$Date] #2 dates are missing
temp.data1<-merge(x=data.frame(Date=date_range),y=temp.data1,all.x=TRUE)
temp.data1[!complete.cases(temp.data1),]#there are 2 missing values

#impute missing records with average of previous day & next day record
library(zoo)
temp.data1 <- zoo(temp.data1$Temp,temp.data1$Date)
temp.data1 <- na.approx(temp.data1)
temp.data1<-fortify.zoo(temp.data1)
temp.data1[c(1461,2922),2]
colnames(temp.data1)<-c('Date','Temp')
temp.data1[c(1461,2922),]

###aggregate to monthly data
temp.data1$Month<-format(temp.data1$Date,'%m')
temp.data1$Year<-format(temp.data1$Date,'%Y')
temp.data<-aggregate(Temp~Month + Year,temp.data1,mean)
temp.data$Date<-paste(temp.data$Month,temp.data$Year,sep="-")
temp.data<-temp.data[,c(4,3)]
temp.data
write.csv(temp.data,file='monthly_minimum_temperature.csv')

###outliers/extreme values
par(mfcol=c(1,2))
boxplot(temp.data1$Temp,ylab='Temperature',xlab='Daily minimum temperature')
boxplot(temp.data$Temp,xlab='Average monthly minimum temperature')
par(mfcol=c(1,1))


#STEP 1: descriptive stats
summary(temp.data$Temp) #min=4.641935; max=17.7129
#install.packages("moments")
#library(moments)
#skewness(temp.data$Temp)

# create time series 
temp.ts = ts(temp.data$Temp, start = c(1981, 1), end = c(1990, 12), freq = 12) 
temp.ts
                ###### STEP 3: VISUALIZE

#time series components
temp.stl <- stl(temp.ts, s.window = "periodic")
autoplot(temp.stl, main = "Average minimum monthly temperature in Australia- Time Series Components")

autocor<-Acf(temp.ts, lag.max = 12, main = "Autocorrelation for average minimum monthly temperature")
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

                ##### STEP 4
# plot time series data 
plot(temp.ts, 
     xlab = "Year",
     ylab = "Temperature", 
     main = "Average Minimum Monthly Temperature",
     xaxt = "n", 
     lwd = 2, 
     col = "brown")
# plot x axis 
# axis(1, at = seq(1, 2150, by = 52), labels = 1979:2020) 
axis(1, at = seq(1981, 1991), labels = 1981:1991) 

                            

                                ###predictability
#method 1: Using Hypothesis testing
temp.ar1<- Arima(temp.ts, order = c(1,0,0))
summary(temp.ar1)
ar1 <- 0.8137
s.e. <- 0.0545
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
        "Reject null hypothesis"
} else {
        "Accept null hypothesis"
}

#method 2:Using differencing method

diff.temp<-diff(temp.ts, lag = 1)
Acf(diff.temp, lag.max = 12, 
    main = "Autocorrelation using differencing method")

                        #### STEP 5
# partition the data 
ts.len = length(temp.ts) 
ts.len # 120 
train = round(ts.len*0.8) 
train # 96 
valid = ts.len-train 
valid # 24 
train+valid # 120 

train.ts = window(temp.ts, start = c(1981, 1), end = c(1981, train)) 
length(train.ts) 
train.ts 
valid.ts = window(temp.ts, start = c(1981, train+1), end = c(1981, train+valid)) 
length(valid.ts) 
valid.ts 

# plot training and validation data  
plot(temp.ts,
     xlab = "Year",
     ylab = "Temperature", 
     main = "Average Minimum Monthly Temperature with Training and Validation Partitions", 
     xaxt = "n", 
     lwd = 2, 
     col = "gray", # use gray or white 
     ylim = c(4, 30)) # set max higher to make room for lines, arrows, legend 
# x axis 
axis(1, at = seq(1981, 1991), labels = 1981:1991) 
# lines
lines(c(1981+train/12, 1981+train/12), c(0, 20)) 
# arrows
arrows(1981, 20, 1981+train/12, 20, code = 3, length = 0.1, angle = 30)
arrows(1981+train/12, 20, 1981+(train+valid)/12, 20, code = 3, length = 0.1, angle = 30) 
# text 
text(1985, 22, "Training")
text(1990, 22, "Validation")
# plot training and validation data 
lines(train.ts, col = "brown", lwd = 2)
lines(valid.ts, col = "brown", lwd = 2, lty = 2)
legend(1981, 30, 
       legend = c("Training Data",
                  "Validation Data"), 
       col = c("brown"), 
       lty = c(1, 2), 
       lwd = c(2), 
       bty = "n")


################################################################################
# plot functions 

# function to plot time series data with training and validation partitions 
plot_temp = function(plot_title) {
        plot(temp.ts,
             xlab = "Year",
             ylab = "Temperature", 
             main = plot_title, 
             xaxt = "n", 
             lwd = 2, 
             col = "gray", # use gray or white 
             ylim = c(4, 30)) # set max higher to make room for lines, arrows, legend 
        # x axis 
        axis(1, at = seq(1981, 1991), labels = 1981:1991) 
        # lines
        lines(c(1981+train/12, 1981+train/12), c(0, 20)) 
        # arrows
        arrows(1981, 20, 1981+train/12, 20, code = 3, length = 0.1, angle = 30)
        arrows(1981+train/12, 20, 1981+(train+valid)/12, 20, code = 3, length = 0.1, angle = 30) 
        # text 
        text(1985, 22, "Training")
        text(1990, 22, "Validation")
}

# function to plot legend for training and validation 
legend_TV = function() {
        legend(1981, 30, 
               legend = c("Average Monthly Minimum Temperature",
                          "Model for Training Period",
                          "Forecast for Validation Period"), 
               col = c("gray", "red", "red"), 
               lty = c(1, 1, 2), 
               lwd = c(2), 
               bty = "n")
}

# function to plot time series data with future prediction  
plot_future = function(plot_title) {
        plot(temp.ts,
             xlab = "Year",
             ylab = "Temperature", 
             main = plot_title, 
             xaxt = "n", 
             lwd = 2, 
             col = "gray", # use gray or white 
             ylim = c(4, 30), # make room for lines, arrows, legend 
             xlim = c(1981, 1993)) # make room for future data  
        # x axis 
        axis(1, at = seq(1981, 1993), labels = 1981:1993) 
        # lines
        lines(c(1991, 1991), c(0, 20)) 
        # arrows
        arrows(1981, 20, 1991, 20, code = 3, length = 0.1, angle = 30)
        arrows(1991, 20, 1993, 20, code = 3, length = 0.1, angle = 30) 
        # text 
        text(1986, 22, "Data Set")
        text(1992, 22, "Future")
}

# function to plot legend for future prediction 
legend_future = function() {
        legend(1981, 30, 
               legend = c("Average Monthly Minimum Temperature",
                          "Model for Data Set",
                          "Forecast for Future Two Years"), 
               col = c("gray", "red", "red"), 
               lty = c(1, 1, 2), 
               lwd = c(2), 
               bty = "n")
}


                                ####STEP 6      
################################################################################

# model 1: regression model with linear trend
train.lin = tslm(train.ts ~ trend)
summary(train.lin) 

train.lin.pred = forecast(train.lin, h = valid, level =0)

train.lin.pred$mean 

# plot lines 
plot_temp("Linear Regression")
lines(train.lin.pred$fitted, col = "red", lwd = 2)
lines(train.lin.pred$mean, col = "red", lwd = 2, lty =2)
legend_TV() 

################################################################################
# model 2: regression with quadratic trend 
train.quad = tslm(train.ts ~ trend + I(trend^2))
summary(train.quad) 

train.quad.pred = forecast(train.quad, h = valid, level =0)
train.quad.pred$mean 

# plot lines 
plot_temp("Quadratic Regression")
lines(train.quad.pred$fitted, col = "red", lwd = 2)
lines(train.quad.pred$mean, col = "red", lwd = 2, lty =2)
legend_TV() 

round(accuracy(train.quad.pred$mean, valid.ts), 3)
################################################################################
# model 3: regression with seasonality 
train.season = tslm(train.ts ~ season) 
summary(train.season) 

train.season.pred = forecast(train.season, h = valid, level = 0)
train.season.pred$mean 

plot_temp("Seasonality")
lines(train.season.pred$fitted, col = "red", lwd = 2)
lines(train.season.pred$mean, col = "red", lwd = 2, lty = 2)
legend_TV() 

round(accuracy(train.season.pred$mean, valid.ts), 3)
################################################################################
# model 4: regression with linear trend and seasonality 
train.lin.season = tslm(train.ts ~ trend + season) 
summary(train.lin.season) 

train.lin.season.pred = forecast(train.lin.season, h = valid, level = 0) 
train.lin.season.pred$mean 

plot_temp("Linear Trend with Seasonality")
lines(train.lin.season.pred$fitted, col = "red", lwd = 2)
lines(train.lin.season.pred$mean, col = "red", lwd = 2, lty = 2)
legend_TV() 

round(accuracy(train.lin.season.pred$mean, valid.ts), 3)
################################################################################
# model 5: regression with quadratic trend and seasonality 
train.quad.season = tslm(train.ts ~ trend + I(trend^2) + season) 
summary(train.quad.season) 

train.quad.season.pred = forecast(train.quad.season, h = valid, level = 0) 
train.quad.season.pred$mean 

plot_temp("Quadratic Trend with Seasonality") 
lines(train.quad.season.pred$fitted, col = "red", lwd = 2)
lines(train.quad.season.pred$mean, col = "red", lwd = 2, lty = 2) 
legend_TV() 

round(accuracy(train.quad.season.pred$mean, valid.ts), 3)
################################################################################
# model 6: auto-arima model 
auto.arima = auto.arima(train.ts) 
summary(auto.arima) 

auto.arima.pred = forecast(auto.arima, h = valid, level = 0) 
auto.arima.pred$mean 

plot_temp("Auto-ARIMA Model") 
lines(auto.arima.pred$fitted, col = "red", lwd = 2) 
lines(auto.arima.pred$mean, col = "red", lwd = 2, lty = 2) 
legend_TV()

round(accuracy(auto.arima.pred$mean, valid.ts), 3)
################################################################################
# model 7: arima(1,1,1)(1,1,1) model 
arima.season = Arima(train.ts, order = c(1,1,1), seasonal = c(1,1,1))
summary(arima.season) 

arima.season.pred = forecast(arima.season, h = valid, level = 0) 
arima.season.pred$mean 

plot_temp("Seasonal ARIMA Model") 
lines(arima.season.pred$fitted, col = "red", lwd = 2) 
lines(arima.season.pred$mean, col = "red", lwd = 2, lty = 2) 
legend_TV()

round(accuracy(arima.season.pred$mean, valid.ts), 3)
################################################################################
# model 8: two-level quadratic trend with seasonality and AR(1) for residuals 

# review predictions from model 5  
train.quad.season.pred$fitted
train.quad.season.pred$mean 

Acf(train.quad.season$residuals)
# ^^^ the residuals don't have high correlation 

residuals.ar1 = Arima(train.quad.season$residuals, order = c(1,0,0))
summary(residuals.ar1)
residuals.ar1$residuals 

# Acf(residuals.ar1$residuals, max.lag = 12)
# ^^^ the residuals of residuals don't have high correlation either 

residuals.ar1.pred = forecast(residuals.ar1, h = valid, level = 0) 
residuals.ar1.pred$mean 

valid.two.level.quad.pred = train.quad.season.pred$mean + residuals.ar1.pred$mean 
valid.two.level.quad.pred 


plot_temp("Two-Level Model: Quadratic Trend with Seasonality and AR(1) for Residuals") 
lines(train.quad.season.pred$fitted, col = "red", lwd = 2) 
lines(valid.two.level.quad.pred, col = "red", lwd = 2, lty = 2) 
legend_TV() 

round(accuracy(valid.two.level.quad.pred, valid.ts), 3)
################################################################################
# model 9: two-level linear trend with seasonality and auto-arima for residuals 

# review predictions from previous model   
train.lin.season.pred$fitted
train.lin.season.pred$mean 

Acf(train.lin.season.pred$residuals)
# ^^^ medium correlation for lag 1 

residuals.aa = auto.arima(train.lin.season.pred$residuals) 
summary(residuals.aa)
residuals.aa$residuals 

# Acf(residuals.aa$residuals, max.lag = 12)
# ^^^ low correlation 

residuals.aa.pred = forecast(residuals.aa, h = valid, level = 0) 
residuals.aa.pred$mean 

valid.two.level.lin.pred = train.lin.season.pred$mean + residuals.aa.pred$mean 
valid.two.level.lin.pred 

plot_temp("Two-Level Model: Linear Trend with Seasonality and Auto-ARIMA for Residuals") 
lines(train.lin.season.pred$fitted, col = "red", lwd = 2) 
lines(valid.two.level.lin.pred, col = "red", lwd = 2, lty = 2) 
legend_TV() 

round(accuracy(valid.two.level.lin.pred, valid.ts), 3)
################################################################################
# model 10: two-level quadtratic trend with seasonality and auto-arima for residuals 
Acf(train.quad.season.pred$residuals)
residuals.aa.quad = auto.arima(train.quad.season.pred$residuals) 
summary(residuals.aa.quad)
residuals.aa.quad$residuals 

residuals.aa.quad.pred = forecast(residuals.aa.quad, h = valid, level = 0) 
residuals.aa.quad.pred$mean 

valid.two.level.quad.pred = train.lin.season.pred$mean + residuals.aa.quad.pred$mean 
valid.two.level.quad.pred 

plot_temp("Two-Level Model: Quadratic Trend with Seasonality and Auto-ARIMA for Residuals") 
lines(train.lin.season.pred$fitted, col = "red", lwd = 2) 
lines(valid.two.level.quad.pred, col = "red", lwd = 2, lty = 2) 
legend_TV() 

round(accuracy(valid.two.level.quad.pred, valid.ts), 3)
################################################################################
# model 11: two-level linear trend with seasonality and AR(1) for residuals 

residuals.lin.ar1 = Arima(train.lin.season$residuals, order = c(1,0,0))
summary(residuals.lin.ar1)
residuals.lin.ar1$residuals 

# Acf(residuals.lin.ar1$residuals, max.lag = 12)
# ^^^ the residuals of residuals don't have high correlation either 

residuals.lin.ar1.pred = forecast(residuals.lin.ar1, h = valid, level = 0) 
residuals.lin.ar1.pred$mean 

valid.two.level.lin.ar1.pred = train.lin.season.pred$mean + residuals.lin.ar1.pred$mean 
valid.two.level.lin.ar1.pred 

plot_temp("Two-Level Model: Linear Trend with Seasonality and AR(1) for Residuals") 
lines(train.lin.season.pred$fitted, col = "red", lwd = 2) 
lines(valid.two.level.lin.ar1.pred, col = "red", lwd = 2, lty = 2) 
legend_TV() 

round(accuracy(valid.two.level.lin.ar1.pred, valid.ts), 3)

################################################################################
#model 12: Holt's winter model with automatic selection of parameters (ETS(M,N,M) model)
hw.ZZZ <- ets(train.ts, model = "ZZZ")
summary(hw.ZZZ)
hw.ZZZ.pred <- forecast(hw.ZZZ, h = valid, level = 0)

plot_temp("Holt's Winter model with automatic selection of parameters") 
lines(hw.ZZZ.pred$fitted, col = "red", lwd = 2) 
lines(hw.ZZZ.pred$mean, col = "red", lwd = 2, lty = 2) 
legend_TV() 

round(accuracy(hw.ZZZ.pred$mean, valid.ts), 3)

###################################################################################
# model 13: Two level model: HW model with AR(1) for residuals
Acf(hw.ZZZ$residuals,lag.max = 12)#the residuals of HW model have high correlation in lag 1
ets.train.residuals.ar1 = Arima(hw.ZZZ$residuals, order = c(1,0,0))
summary(ets.train.residuals.ar1)
Acf(ets.train.residuals.ar1$residuals, lag.max = 12)# no significance in residuals of residual

ets.residuals.ar1.pred = forecast(ets.train.residuals.ar1, h = valid, level = 0) 
ets.residuals.ar1.pred$mean 

valid.two.level.ets.pred = hw.ZZZ.pred$mean + ets.residuals.ar1.pred$mean 

plot_temp("Holt's Winter model with AR(1) for residuals") 
lines(hw.ZZZ.pred$fitted, col = "red", lwd = 2) 
lines(valid.two.level.ets.pred, col = "red", lwd = 2, lty = 2) 
legend_TV() 

round(accuracy(valid.two.level.ets.pred, valid.ts), 3)

################################################################################
                                ### STEP 7      
# compare accuracy of the models for the entire data set 
# set future to 12 or 24 for the next year or two 
future = 24

# get predictions for entire data set using all the models 

# model 1: regression with linear trend
temp.lin = tslm(temp.ts ~ trend)
temp.lin.pred = forecast(temp.lin, h = future, level =0)
class(round(accuracy(temp.lin.pred$fitted, temp.ts), 3)) # <<< what class is this??? 

# model 2: regression with quadratic trend 
temp.quad = tslm(temp.ts ~ trend + I(trend^2))
temp.quad.pred = forecast(temp.quad, h = future, level =0)

# model 3: regression with seasonality 
temp.season = tslm(temp.ts ~ season) 
temp.season.pred = forecast(temp.season, h = future, level = 0)

# model 4: regression with linear trend and seasonality 
temp.lin.season = tslm(temp.ts ~ trend + season) 
temp.lin.season.pred = forecast(temp.lin.season, h = future, level = 0) 

# model 5: regression with quadratic trend and seasonality 
temp.quad.season = tslm(temp.ts ~ trend + I(trend^2) + season) 
temp.quad.season.pred = forecast(temp.quad.season, h = future, level = 0) 

# model 6: auto-arima model 
temp.auto.arima = auto.arima(temp.ts) 
temp.auto.arima.pred = forecast(temp.auto.arima, h = future, level = 0)

# model 7: arima(1,1,1)(1,1,1) model 
temp.arima.season = Arima(temp.ts, order = c(1,1,1), seasonal = c(1,1,1))
temp.arima.season.pred = forecast(temp.arima.season, h = future, level = 0) 

# model 8: two-level quadratic trend with seasonality and AR(1) for residuals 
temp.residuals.quad.ar1 = Arima(temp.quad.season$residuals, order = c(1,0,0)) 
temp.residuals.quad.ar1.pred = forecast(temp.residuals.quad.ar1, h = future, level = 0) 
future.two.level.quad.ar1 = temp.quad.season.pred$fitted + temp.residuals.quad.ar1.pred$fitted 

# model 9: two-level linear trend with seasonality and auto-arima for residuals 
temp.residuals.lin.aa = auto.arima(temp.lin.season$residuals) 
temp.residuals.lin.aa.pred = forecast(temp.residuals.lin.aa, h = future, level = 0) 
future.two.level.lin.aa = temp.lin.season.pred$fitted + temp.residuals.lin.aa.pred$fitted 

# model 10: two-level quadratic trend with seasonality and auto-arima for residuals 
temp.residuals.aa.quad = auto.arima(temp.quad.season$residuals) 
temp.residuals.aa.quad.pred = forecast(temp.residuals.aa.quad, h = future, level = 0) 
future.two.level.quad.aa = temp.quad.season.pred$fitted + temp.residuals.aa.quad.pred$fitted 

# model 11: two-level linear trend with seasonality and AR(1) for residuals 
temp.residuals.lin.ar1 = Arima(temp.lin.season$residuals, order = c(1,0,0)) 
temp.residuals.lin.ar1.pred = forecast(temp.residuals.lin.ar1, h = future, level = 0) 
future.two.level.lin.ar1 = temp.lin.season.pred$fitted + temp.residuals.lin.ar1.pred$fitted 

# model 12: Holt's winter model
temp.hw.ZZZ <- ets(temp.ts, model = "ZZZ")
future.hw.ZZZ.pred <- forecast(hw.ZZZ, h = future, level = 0)

#model 13: Two level model with Holts winter & AR1 for residual

temp.ets.residuals.ar1 = Arima(temp.hw.ZZZ$residuals, order = c(1,0,0))
future.ets.residuals.ar1.pred = forecast(temp.ets.residuals.ar1, h = future, level = 0) 
future.two.level.ets.pred = future.hw.ZZZ.pred$fitted + future.ets.residuals.ar1.pred$fitted


# show accuracy of each model together 

# model 1: linear
round(accuracy(temp.lin.pred$fitted, temp.ts), 3)['Test set', c('RMSE', 'MAPE')]

# model 2: quadratic 
round(accuracy(temp.quad.pred$fitted, temp.ts), 3)['Test set', c('RMSE', 'MAPE')] 

# model 3: seasonal 
round(accuracy(temp.season.pred$fitted, temp.ts), 3)['Test set', c('RMSE', 'MAPE')] 

# model 4: linear with seasonality 
round(accuracy(temp.lin.season.pred$fitted, temp.ts), 3)['Test set', c('RMSE', 'MAPE')]  

# model 5: quadratic with seasonality 
round(accuracy(temp.quad.season.pred$fitted, temp.ts), 3)['Test set', c('RMSE', 'MAPE')] 

# model 6: auto-arima 
round(accuracy(temp.auto.arima.pred$fitted, temp.ts), 3)['Test set', c('RMSE', 'MAPE')] 

# model 7: arima model with seasonality 
round(accuracy(temp.arima.season.pred$fitted, temp.ts), 3)['Test set', c('RMSE', 'MAPE')] 

# model 8: two-level quadratic trend with seasonality and AR(1) for residuals 
round(accuracy(future.two.level.quad.ar1, temp.ts), 3)['Test set', c('RMSE', 'MAPE')] 

# model 9: two-level linear trend with seasonality and auto-arima for residuals 
round(accuracy(future.two.level.lin.aa, temp.ts), 3)['Test set', c('RMSE', 'MAPE')] 

# model 10: two-level quadratic trend with seasonality and auto-arima for residuals 
round(accuracy(future.two.level.quad.aa, temp.ts), 3)['Test set', c('RMSE', 'MAPE')] 

# model 11: two-level linear trend with seasonality and AR(1) for residuals 
round(accuracy(future.two.level.lin.ar1, temp.ts), 3)['Test set', c('RMSE', 'MAPE')] 

# model 12:
round(accuracy(future.hw.ZZZ.pred$fitted, temp.ts), 3)['Test set', c('RMSE', 'MAPE')] 

# MODEL 13:
round(accuracy(future.two.level.ets.pred, temp.ts), 3)['Test set', c('RMSE', 'MAPE')] 

################################################################################
                                        ### STEP 8

# plot most accurate models for the entire data set 

# model 8: two-level quadratic trend with seasonality and AR(1) for residuals 
future.two.level.quad.ar1.pred = temp.quad.season.pred$mean + temp.residuals.quad.ar1.pred$mean 
plot_future("Two-Level Quadratic Trend with Seasonality and AR(1) for Residuals") 
lines(future.two.level.quad.ar1, col = "red", lwd = 2) 
lines(future.two.level.quad.ar1.pred, col = "red", lwd = 2, lty = 2) 
future.two.level.quad.ar1.pred
legend_future() 

# model 9: two-level linear trend with seasonality and auto-arima for residuals 
future.two.level.lin.aa.pred = temp.lin.season.pred$mean + temp.residuals.lin.aa.pred$mean 
plot_future("Two-Level Linear Trend with Seasonality and Auto-ARIMA for Residuals") 
lines(future.two.level.lin.aa, col = "red", lwd = 2) 
lines(future.two.level.lin.aa.pred, col = "red", lwd = 2, lty = 2) 
future.two.level.lin.aa.pred
legend_future() 

#model 11:two-level linear trend with seasonality and AR(1) for residuals 

future.two.level.lin.ar1.pred = temp.lin.season.pred$mean + temp.residuals.lin.ar1.pred$mean 
plot_future("Two-level linear trend with seasonality and AR(1) for residuals")
lines(future.two.level.lin.ar1, col = "red", lwd = 2) 
lines(future.two.level.lin.ar1.pred, col = "red", lwd = 2, lty = 2) 
future.two.level.lin.ar1.pred
legend_future() 





