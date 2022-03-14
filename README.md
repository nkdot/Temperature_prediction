# Weather prediction in Australia using Time series Analytics
###### Australia is very familiar with extremes in climate, from severe frosts to intense heat waves. Importantly these extremes can have substantial impacts on Australia's unique flora and fauna, agriculture, urban infrastructure, and tourism. Many studies have chosen to try and understand observed changes in climate extremes and their future projections through the use of climate indices or ‘extremes indices’ which are based on daily measures of temperature and precipitation. In this project, we are going to apply different time series forecasting models, and perform model evaluation on the Australian weather dataset, and come up with the model of choice for future forecasting and deployment. 

### Dataset link: 
###### https://raw.githubusercontent.com/jbrownlee/Datasets/master/daily-min-temperatures.csv
### Data description:

###### Dataset contains daily minimum temperature in Melbourne Australia from the year 1980 to 1990.
### Data Preprocessing:

#### Missing values:
###### Two missing records on “1984-12-31” and “1988-12-31” are identified. Those missing records are imputed using linear interpolation(An average of the previous and next day is considered).

#### Extreme values/Outliers:
![alt text](https://github.com/nkdot/Temperature_prediction/blob/main/images/box_plot.png "Box plot")
###### From the above box plot, few outliers can be observed in the daily data.  Aggregating daily data into monthly data eliminated outliers in this case. The average monthly minimum temperature is considered to avoid modeling the noise in the daily data. 

#### Summary statistics: 
| Min  | 1st Qtr       | Median  |Mean|3rd Qtr| Max|
| -----|:-------------:| -----:|-----:|-----:|-----:|
|4.642 | 8.462| 11.118 |11.201| 13.773|17.713|

###### The monthly average minimum temperature ranges from 4.6 to 17.7 ℃. The mean is higher than the median. Temperature data has positive skewness (0.0414). That tells us that the temperature in the lower bounds is extremely low compared to the rest of the data which confirms extremely low weather patterns in Australia.

#### Predictability:
##### Approach 1:
###### Before diving into forecasting models, let's check if the data point are random walk.
![alt text](https://github.com/nkdot/Temperature_prediction/blob/main/images/hyp_test.png "Hypothesis testing using AR(1)")
###### As  the p-value(0.000315) is less than alpha(0.05), we reject the null hypothesis and thus the data is not a random walk.In other words, ,temperature is predictable, and its historical data and patterns can be used for non-naïve forecasting methods to get a better prediction compared to naïve methods.

##### Approach 2:
###### Alternatively,  correlogram of the differencing method can also be used for predicting the random walk. As we can see in the below correlogram, most of the lags are statistically significant. Hence we concluded that our temperature data set is definitely not a random walk.
![alt text](https://github.com/nkdot/Temperature_prediction/blob/main/images/differencing_acf.png "Acf using differencing method")

### Data Exploration: 
##### Plot:
![alt text](https://github.com/nkdot/Temperature_prediction/blob/main/images/plot.png "Plot")
###### Priliminary plots shows us a constant trend with additive seasonality. The temperature is high in the beginning of the year and starts to drop after the first quarter, and there’s an increase after the second quarter.

##### Correlogram: 
![alt text](https://github.com/nkdot/Temperature_prediction/blob/main/images/acf.png "ACF")
###### From the chart above we see that the data is highly correlated, as the autocorrelation coefficients in all the lags except lags 3 and 9 are substantially higher than the horizontal threshold limit. Lags 1, 2, 10, 11 and 12 have a positive autocorrelation coefficient. A positive autocorrelation in lag 1 represents trend in the data and a positive autocorrelation in lag 12 indicates the presence of seasonality in the data.

##### Time series components:
![alt text](https://github.com/nkdot/Temperature_prediction/blob/main/images/stl.png "Time series components")
###### We can observe additive seasonality, non-linear trend and some remainder for the given period. 

### Data partitioning & applying different models in training data:
###### In the aggregated monthly data, there were 120 data points. The first 8 years (96 months) were used for training data. The last 2 years (24 months) were used for validation. 

### Model comparison in validation data:
![alt text](https://github.com/nkdot/Temperature_prediction/blob/main/images/model_comparison.png "Model comparison table")

### Apply the best model in entire dataset:
###### Based on the accuracy in validation data, the two most accurate were the following, ranked first and second. 
###### 1.Two-Level Linear with Seasonality and Auto-ARIMA for Residuals
![alt text](https://github.com/nkdot/Temperature_prediction/blob/main/images/best1_plot.png "Two level model with Auto arima for residuals")

###### 2.Two-Level Linear with Seasonality and AR(1) for Residuals

![alt text](https://github.com/nkdot/Temperature_prediction/blob/main/images/best2_plot.png "Two level model with AR(1) for residuals")




