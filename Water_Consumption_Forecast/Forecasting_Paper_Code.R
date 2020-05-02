#Set WD (DELETE FOR FINAL VERSION)
setwd(file.path("C:","Users","henni","Documents","App State","Classes","Spring 2020","Time_Series","Final_Paper"))

#Package and Data Setup
library(tidyverse)
library(tsdl)
library(lubridate)
library(fable)
library(fpp2)

#Extract time series from tsdl package####
#information about data
attributes(subset(tsdl, "Utilities", 12)[[7]])
#number of obs
length(subset(tsdl, "Utilities", 12)[[7]])
#Missing values
sum(is.na(subset(tsdl, "Utilities", 12)[[7]]))

#Extraction
water_data <- subset(tsdl, "Utilities", 12)[[7]]

#Create time-series data frame with tsibble 
water_data_tsdf <- as_tsibble(water_data)

#Visualizing series with line plot and ACF ####
p1 <- ggplot(data=water_data_tsdf, aes(x=index, y=value))+
  geom_line()+
  theme_bw()+
  labs(y="Water Consumption", x="Time", title = "Time Series Plot of Monthly Water Consumption",
       subtitle = "Monthly Water Consumption in London, Ontario, 1966-1988",
       caption = "Data Source: Hipel and McLeod (1994)")


p2 <- ggAcf(water_data)+
  theme_bw()+
  labs(x="Lag", y="Autocorrelation",
       title = "ACF-Plot of Monthly Water Consumption",
       subtitle = "Monthly Water Consumption in London, Ontario, 1966-1988",
       caption = "Data Source: Hipel and McLeod (1994)")

#Create Figure
png("Figures/Time_Series_line_and_ACF.png", width = 8.66, height=5.75, units = "in", res = 600)
gridExtra::grid.arrange(p1, p2, nrow=2)
dev.off()

#Visualizing series with season and subseasonplot ####
p1 <- ggseasonplot(water_data)+
  theme_bw()+
  labs(y="Water Consumption", x="Seasons", title = "Seasonal Plot of Monthly Water Consumption",
       subtitle = "Monthly Water Consumption in London, Ontario, 1966-1988")

p2 <- ggsubseriesplot(water_data)+
  theme_bw()+
  labs(y="Water Consumption", x="Seasons", title = "Subseason Plot of Monthly Water Consumption",
       subtitle = "Monthly Water Consumption in London, Ontario, 1966-1988")


#Create Figure
png("Figures/Time_Series_season_subseason.png", width = 8.66, height=5.75, units = "in", res = 600)
gridExtra::grid.arrange(p2, p1, nrow=2)
dev.off()


#Descriptive Analysis and transformation/data engineering ####
#BoxCox Transformation necessary?
BoxCox.lambda(water_data)
water_trsf <- BoxCox(water_data, lambda = 0)
#lambda of 0 --> logarithmic transformation

#Average difference between the minimum and maximum observation of a year
water_data_tsdf%>%
  mutate(year=year(index))%>%
  as_tibble()%>%
  select(-index)%>%
  group_by(year)%>%
  summarize(diff_consumption = max(value) -min(value))%>%
  summarize(typ_diff = mean(diff_consumption))
  

#summary statistics for before and after transformation
water_data_tsdf%>%
  as.data.frame()%>%
  summarize(min_water = min(value),
            max_water = max(value),
            median_water = median(value),
            mean_water = mean(value),
            sd_water = sd(value),
            min_water_trs = min(BoxCox(value, lambda = 0)),
            max_water_trs = max(BoxCox(value, lambda = 0)),
            median_water_trs = median(BoxCox(value, lambda = 0)),
            mean_water_trs = mean(BoxCox(value, lambda = 0)),
            sd_water_trs = sd(BoxCox(value, lambda = 0)))%>%
  write_csv("Tables/summary_table.csv")

#Autocorrelation
#according to Hyndman (p.60-61), the lag should be 2*m for seasonal data. thus lag =24 for this data.
Box.test(water_trsf, lag = 2*frequency(water_data))

#Selecting Benchmarking forecasting method ####
#computing RMSE based on one step ahead time series cross-validation
e_mean <- tsCV(water_trsf, forecastfunction = meanf, h=1)
e_naive <- tsCV(water_trsf, forecastfunction = rwf, h=1)
e_snaive <- tsCV(water_trsf, forecastfunction = snaive, h=1)
e_drift <- tsCV(water_trsf, forecastfunction = rwf, drift=T , h=2)

sqrt(mean(e_mean^2, na.rm = TRUE)) #0.215
sqrt(mean(e_naive^2, na.rm = TRUE)) #0.083
sqrt(mean(e_snaive^2, na.rm = TRUE)) #0.079 #SMALLEST RMSE
sqrt(mean(e_drift^2, na.rm = TRUE)) #0.084

#How does the performance change based on forecasting Horizon
#Define function for increasing forecasting horizon
tsCV_rolling_h <- function(data, h, funct, drift=F){
  ###THE FUNCTION COMPUTES TIME SERIES CROSS VALIDATION ON A GRADUALLY INCREASING H####
  
  #Initialization
  output = 0
  #For functions without drift
  if (drift==F){
    #rolling forward forecast hoirzon
    for (h in 1:h){
      #each element of the output vector is the RSME of the tsCV
      output[h] <- sqrt(mean(
        tsCV(data, forecastfunction = funct, h=h)^2, #Call to tsCV
        na.rm = TRUE))
    }
    return(output)
  }
  #For rwf with drift
  if (drift==T){
    for (h in 1:h){
      
      output[h] <- sqrt(mean(tsCV(data, forecastfunction = funct, h=h, drift=T)^2, na.rm = TRUE))
    }
    return(output)
  }
}

mean_e_h <- tsCV_rolling_h(data = water_trsf, h=12, funct = meanf)
naive_e_h <- tsCV_rolling_h(data = water_trsf, h=12, funct = rwf)
snaive_e_h <- tsCV_rolling_h(data = water_trsf, h=12, funct = snaive)
drift_e_h <- tsCV_rolling_h(data = water_trsf, h=12, funct = rwf, drift = T)
#create tibble for visualization
tscv_tib <- tibble(mean_method = mean_e_h,
                   naive_method = naive_e_h,
                   snaive_method = snaive_e_h,
                   drift_method = drift_e_h)
#Create Figure
png("Figures/TSCV-Benchmarking_Methods-Appendix.png", width = 8.66, height=5.75, units = "in", res = 600)
tscv_tib%>%
  gather(key="Method", value = "RMSE")%>%
  mutate(h = rep(1:12, times=4, each=1))%>%
  ggplot(aes(x=h, y=RMSE, color=Method))+
  geom_line()+
  scale_x_continuous(breaks = seq(1,12))+
  labs(title = "RMSE of Benchmarking Methods",
       subtitle = "RMSE of Time-Series Cross-Validation with Increasing Forecasting Horizon",
       x="Forecasting Horizon",
       y="RMSE",
       color = "Forecasting \nMethod")+
  scale_color_manual(labels = c("Naive", "Drift", "Mean","Seasonal \nNaive"),
                     values = c("blue", "red", "green", "purple")) +
  theme_bw()
dev.off()
#Seasonal Naive method performs clearly the best

#Model Selection ####
#is the series stationary?

#testing for stationarity
water_trsf%>%ur.kpss()%>%summary()
#non stationary

#First determine seasonal differencing
nsdiffs(log(water_trsf))
#one seasonal differencing suggested

#now determine first differencing of seasonally differenced data
water_trsf%>%diff(12)%>%ndiffs()
#no first differencing necessary

#kpss test
water_trsf%>%diff(12)%>%ur.kpss()%>%summary()
#stationary after seasonal differencing

#Creating functions of the forecasting methods for time series cross-validation
farima <- function(data,h){
  forecast(auto.arima(data, stepwise = F, approximation = F), h=h)
}

fets <- function(data, h){
  forecast(ets(data), h=h)
}

ftbats <- function(data,h){
  forecast(tbats(data), h=h)
}

fbag <-function(data,h){
  forecast(baggedETS(data), h=h)
}

fnet <-function(data,h){
  forecast(nnetar(data), h=h)
}
 
#create function for time series cross-validation with above methods and shifting time horizon
tscv_fore_test <- function(data, h){
  test_frame <- data.frame(iteration = 1:h)
  for(i in 1:5){
    if (i==1){
    output_arima = 0
  
      for (h in 1:h){
        output_arima[h] <- sqrt(mean(
        tsCV(data, forecastfunction = farima, h=h)^2, #Call to tsCV
        na.rm = TRUE))
                    }
    test_frame <- cbind(test_frame, output_arima)
    }
    if (i==2){
      output_ets = 0
      
      for (h in 1:h){
        output_ets[h] <- sqrt(mean(
          tsCV(data, forecastfunction = fets, h=h)^2, #Call to tsCV
          na.rm = TRUE))
      }
      test_frame <- cbind(test_frame, output_ets)
    }
    if (i==3){
      output_tbats = 0
      
      for (h in 1:h){
        output_tbats[h] <- sqrt(mean(
          tsCV(data, forecastfunction = ftbats, h=h)^2, #Call to tsCV
          na.rm = TRUE))
      }
      test_frame <- cbind(test_frame, output_tbats)
    }
    if (i==4){
      output_bag = 0
      
      for (h in 1:h){
        output_bag[h] <- sqrt(mean(
          tsCV(data, forecastfunction = fbag, h=h)^2, #Call to tsCV
          na.rm = TRUE))
      }
      test_frame <- cbind(test_frame, output_bag)
    }
    if (i==5){
      output_net = 0
      
      for (h in 1:h){
        output_net[h] <- sqrt(mean(
          tsCV(data, forecastfunction = fnet, h=h)^2, #Call to tsCV
          na.rm = TRUE))
      }
      test_frame <- cbind(test_frame, output_net)
    }
    }
  return(test_frame)
}

#This call takes a very long time as time series-cross validation tends to be computationally very expensive
#Since it is a "leave-one-out" variation of k-fold cross-validation.
#consider reading in the csv-file with ready results.

#test_out <- tscv_fore_test(data = water_trsf, h=12)
test_out <- read_csv("Tables/shifting_h_forecasts.csv")

#Visualization of model accuracy --> ARIMA model performs best
#comparison to seasonal naive benchmark
snaive_rmse <- tscv_tib%>%
  gather(key="Method", value = "RMSE")%>%
  mutate(h = rep(1:12, times=4, each=1))%>%
  filter(Method == "snaive_method")
  
#Line plot of all methods
png("Figures/TSCV-Forecasting_Methods.png", width = 8.66, height=5.75, units = "in", res = 600)
test_out%>%
  pivot_longer(cols = -iteration, names_to = "Method", values_to = "RMSE")%>%
  ggplot(aes(x=iteration, y=RMSE, color=Method))+
  geom_line()+
  geom_line(data=snaive_rmse, aes( y=RMSE, x=h), linetype="dashed")+
  scale_x_continuous(breaks = seq(1,12))+
  labs(title = "RMSE of Forecasting Methods",
       subtitle = "RMSE of Time-Series Cross-Validation with Increasing Forecasting Horizon",
       x="Forecasting Horizon",
       y="RMSE",
       color = "Forecasting \nMethod")+
  scale_color_manual(labels = c("Arima", "Bagged ETS", "ETS","Neural Net \nAuto-\nregression", "TBATS", 
                                "Benchmark \nSeasonal \nNaive"),
                     values = c("blue", "red", "green", "purple", "black", "orange")) +
  theme_bw()
dev.off()

#Arima forecast with back transformation #####
water_fc <- auto.arima(water_data, stepwise = F, approximation = F, lambda = 0)%>%forecast(h=24)

#Visualization of forecast
png("Figures/Forecast.png", width = 8.66, height=5.75, units = "in", res = 600)
water_fc%>%
  autoplot()+
  labs(title = "Two-year Forecast of Water Consumption London, Ontario (Canada)",
       subtitle = "Back-transformed ARIMA(1,0,0)(0,1,1)[12] with Drift",
       y="Water Consumption in Megaliter/Day")+
  theme_bw()
dev.off()

#forecasts and prediction intervals
tibble(point_fc = water_fc$mean,
       lower95 = water_fc$lower[,2],
       lower80 = water_fc$lower[,1],
       upper80 = water_fc$upper[,1],
       upper95 = water_fc$upper[,2])%>%
  write_csv("Tables/Predictions.csv")

#Residual Diagnostics #####
#Time Series Plot of Residuals
p1 <- autoplot(water_fc$residuals)+
  theme_bw()+
  labs(title = "Residual Diagnostics Forecast Model of Monthly Water Consumption ",
       subtitle = "ARIMA(1,0,0)(0,1,1)[12] with Drift", y="Residual Value")

#ACF plot of residuals
q1 <- ggAcf(water_fc$residuals, lag=36)+
  theme_bw()+
  labs(title = "Serial Autorcorrelation of Residuals", x="Lag", y="Autocorrelation Coefficient")

#Histogram plus normal curve of residuals
q2 <- ggplot(data=NULL, aes(x = as.numeric(water_fc$residuals))) + 
  geom_histogram(aes(y =..density..),bins = 50, 
                 colour = "black", 
                 fill = "darkgray") +
  stat_function(fun = dnorm, args = list(mean = mean(as.numeric(water_fc$residuals)),
                                         sd = sd(as.numeric(water_fc$residuals))), color="red")+
  geom_rug()+
  geom_vline(xintercept = mean(as.numeric(water_fc$residuals)), linetype = "dashed")+
  theme_bw()+
  labs(title = "Residual Distribution", 
       x="Residual Value",
       y="Frequency")
#Arranging acf and histogram
p2 <- gridExtra::grid.arrange(q1,q2, nrow=1)

png("Figures/Resid_Diagnostics.png", width = 8.66, height=5.75, units = "in", res = 600)
gridExtra::grid.arrange(p1,p2, nrow=2)
dev.off()

checkresiduals(water_fc)

