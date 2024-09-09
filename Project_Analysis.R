# Installing the packages to be used 

install.packages(c("forecast", "tidyverse","kableExtra", "TSA", "tseries", "TSstudio",
                   "prophet", "gtsummary", "tsibble", "fable", "readxl", "lattice",
                   "latticeExtra", "lubricate","Kendall", "MASS"))
install.packages("webshot2")
install.packages("ggThemeAssist")
install.packages("gptstudio")
install.packages("extraDistr")
install.packages("installr")
install.packages("ts")
install.packages("base")
install.packages("plotly")

# Loading the required packages for the analysis.

library(forecast)
library(tidyverse)
library(TSA)
library(tseries)
library(TSstudio)
library(gtsummary)
library(tsibble)
library(fable)
library(readxl)
library(lattice)
library(latticeExtra)
library(lubridate)
library(Kendall)
library(webshot)
library(webshot2)
library(MASS)

Original_data_project <- read_excel("D:/Project_MK_Ultra/Project_Trials/Original_data_project.xlsx")
View(Original_data_project)

# Cleaning the time series data

Scabies.ts <- Original_data_project[ , c(1,4)]
View(Scabies.ts)

View(Scabies.ts)
# Converting the datasets into a time series data

Scabies_ts <- ts(Scabies.ts$SCABIES, 
                 start = c(2016,01),
                 end = c(2023,05),
                 frequency = 12)
Scabies_ts
class(Scabies_ts)
str(Scabies_ts)


# Descriptive statistics of the time series data.

summary(Scabies_ts)
sd(Scabies_ts)

# Seasonal Differencing of the time series data

diff_data <- diff(Scabies_ts, lag = 12)
plot(diff_data)

# Decomposing the time series data

decomposition <- decompose(Scabies_ts)
decomposition

# Visualizing the decomposed time series data.

autoplot(decomposition, 
         main = "Decomposition of the Scabies Time Series Datasets.") + 
 theme(panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.minor = element_line(linetype = "dashed"),
    axis.title = element_text(family = "serif",
        size = 20, face = "bold.italic"),
    plot.title = element_text(family = "serif",
        size = 25, face = "bold.italic")) +labs(x = "Monthly", y = NULL) + 
 theme(plot.title = element_text(hjust = 0.5))


# Visualizing the time series data sets

autoplot(Scabies_ts,
         main = "A Time Series Plot of A Monthly Scabies Recordings (2016 - 2023).",
         col = "green",
         linewidth = 0.6) + theme(axis.line = element_line(linetype = "solid"),
    panel.grid.major = element_line(linetype = "blank"),
    panel.grid.minor = element_line(linetype = "blank"),
    axis.title = element_text(family = "serif",
        size = 20, face = "bold.italic"),
    axis.text.y = element_text(size = 1),
    plot.title = element_text(family = "serif",
        size = 25, face = "bold.italic",
        hjust = 0.5), panel.background = element_rect(fill = NA)) +labs(x = "Monthly", y = "Number of cases ") +
 theme(axis.line = element_line(linetype = "blank"),
    axis.text = element_text(family = "serif",
        face = "bold.italic"), axis.text.x = element_text(family = "serif",
        size = 12), axis.text.y = element_text(family = "serif",
        size = 12))

ts_plot(Scabies_ts,
        title = "A Time Series Plot of A Monthly Scabies Recordings (2016 - 2023).",
        color = "green",
        Xtitle = "Monthly",
        Ytitle = "Number of cases")


### Checking for stationarity of the time series data

# Using the ADF test
# Ho: The Scabies time series data is not stationary.
# Ha: The Scabies time series data is stationary.

adf.test(Scabies_ts)

# Conclusion: Since the p-value (0.06664) is greater than alpha value(0.05),
# we fail to reject the Ho and conclude that the Scabies time series data is 
# not stationary, hence we to difference it.

# Using the PP test.
# Ho: The Scabies time series data is not stationary.
# Ha: The Scabies time series data is stationary.

pp.test(Scabies_ts)

# Conclusion: With PP test, the p-value is 0.01 indicating that the Scabies 
# time series data is stationary. This is due to the fact that the p-value is 
# lesser than alpha value 0.05, leading to the rejection of the null hypothesis
# Ho.


# Using the KPSS test
# Ho: The Scabies time series data is stationary.
# Ha: The Scabies time series data is not stationary.

kpss.test(Scabies_ts)

# Conclusion: Since the p-value is smaller than the alpha value 0.05, we
# reject Ho, and conclude that the Scabies time series data is not stationary.


# Differencing the time series datasets

Scabies_ts_diff1 <- diff(Scabies_ts, 
                         differences = 1,)
Scabies_ts_diff1

# Visualizing the First differenced Scabies time series datasets.

ts_plot(Scabies_ts_diff1,
        title = "A First Differenced of the Scabies Time Series datasets.")


# Testing for stationarity of the first differenced time series data.

# Using the ADF test.
# Ho: The first differenced of Scabies time series data is not 
# stationary.
# Ha: The first differenced of the Scabies time series data is stationary.

adf.test(Scabies_ts_diff1)

# Conclusion: With a p-value of 0.01, we can now confirm the stationarity of 
# the Scabies time series data.


# Using the KPSS test.
# Ho: The first differenced of the Scabies time series data is stationary.
# Ha: The first differenced of the Scabies time series data is not 
# stationary.

kpss.test(Scabies_ts_diff1)

# Conclusion: With a p-value of 0.1, greater than 0.05, we fail to reject Ho
# and conclude that the Scabies time series data is now stationary.


# Plotting of the ACF(MA) and PACF(AR) for the model selection.

par(mfrow = c(1,2))
acf(Scabies_ts_diff1, lag.max = 20,
    main = " An ACF Plot of The First Differenced of the Scabies Time Series datasets. ")

pacf(Scabies_ts_diff1, lag.max = 20,
     main = "A PACF Plot of The First Differenced of the Scabies Time Series datasets.")

# From the ACF and PACF plots the model that can be derived from it is an
# ARIMA(1,1,1), ARIMA(2,1,2) and ARIMA(0,1,3).
# We then build competing models with the models obtain.

Model_1 <- Arima(Scabies_ts, order = c(1,1,1))
Model_1  # AIC=937.25   AICc=937.55   BIC=944.54

Model_2 <- Arima(Scabies_ts, order = c(0,1,3))
Model_2  # AIC=933.38   AICc=933.89   BIC=943.1

Model_3 <- Arima(Scabies_ts, order = c(2,1,2))
Model_3  # AIC=936   AICc=936.77   BIC=948.15

Model_4 <- Arima(Scabies_ts, order = c(1,1,3))
Model_4  # AIC=935.38   AICc=936.15   BIC=947.54

Model_5 <- Arima(Scabies_ts, order = c(1,1,2))
Model_5  # AIC=934.4   AICc=934.91   BIC=944.13

Model_6 <- Arima(Scabies_ts, order = c(0,1,3), seasonal = c(0,0,1))
Model_6  # AIC=935.03   AICc=935.8   BIC=947.19

Model_7 <- Arima(Scabies_ts, order = c(1,1,2), seasonal = c(1,0,0))
Model_7  # AIC=935.9   AICc=936.67   BIC=948.05

Model_8 <- Arima(Scabies_ts, order = c(1,1,1), seasonal = c(0,0,1))
Model_8  # AIC=938.77   AICc=939.28   BIC=948.49

Model_9 <- Arima(Scabies_ts, order = c(1,1,3), seasonal = c(0,0,1))
Model_9  # AIC=937.03   AICc=938.12   BIC=951.61

Model_10 <- Arima(Scabies_ts, order = c(2,1,2), seasonal = c(1,0,0))
Model_10  # AIC=937.53   AICc=938.63   BIC=952.12

Model_11 <- Arima(Scabies_ts, order = c(0,1,3), seasonal = c(0,1,1))
Model_11  # AIC=867.23   AICc=868.08   BIC=878.88

Model_12 <- Arima(Scabies_ts, order = c(0,1,3), seasonal = c(0,1,0))
Model_12  # AIC=884.39   AICc=884.95   BIC=893.71

Model_13 <- Arima(Scabies_ts, order = c(0,1,3), seasonal = c(1,1,0))
Model_13  # AIC=872.96   AICc=873.82   BIC=884.62

Model_14 <- Arima(Scabies_ts, order = c(0,1,3), seasonal = c(1,1,1))
Model_14  # AIC=869.2   AICc=870.42   BIC=883.19

Model_15 <- Arima(Scabies_ts, order = c(1,1,2), seasonal = c(0,1,1))
Model_15  # AIC=866.79   AICc=867.65   BIC=878.44



# From the competing models, the optimal model is Model_15 which is 
# ARIMA(1,1,2)(0,1,1)[12] with an AIC=866.79, AICc=867.65, and a  BIC=878.44 which 
# is the model with the least information criteria.

# We then check for the appropriateness of the model, normality and randomness 
# of the residuals.

# Checking the normality of the residuals

# Ho: The residuals of ARIMA(1,1,2)(0,1,1)[12] is normally distributed.
# Ha: The residuals of ARIMA(1,1,2)(0,1,1)[12] is not normally distributed.

shapiro.test(residuals(Model_15))

# Conclusion: Since the p-value, 0.05176 is greater than alpha value, 0.05
# we fail to reject Ho and conclude that the residuals of the model ARIMA(0,1,3)
# is normally distributed.

# Checking for the appropriateness of the model.

# Ho: The model ARIMA(1,1,2)(0,1,1)[12] is appropriate.
# Ha: The model ARIMA(1,1,2)(0,1,1)[12] is not appropriate.

checkresiduals(Model_15)
check_res(Model_15)

# Conclusion: Since the p-value(0.9583) is greater than the alpha value(0.05)
# we fail to reject Ho and conclude that the model ARIMA(0,1,3) is appropriate.


# Since all the assumption are met, we then go ahead and forecast

# Forecasted Values for the next 12 months of the Scabies datasets

Forecasted_values <- forecast(Model_2, h = 12)
Forecasted_values

# Visualizing the forecasted values for the next 12 months.

plot_forecast(Forecasted_values,
              title = "Forecast of the ARIMA(1,1,2)(0,1,1)[12] model of the Scabies TS data.",
              Xtitle = "Months",
              Ytitle = "Number of cases",
              color = "green")

accuracy(Forecasted_values)

# TESTING THE ACCURACY OF THE SCABIES TIME SERIES DATASETS

# Splitting the data into train and test datasets (75% and 25%)

train_scabies <- window(ts(Scabies.ts$SCABIES, start = c(2016, 01), end = c(2021, 08), frequency = 12))
test_scabies <- window(ts(Scabies.ts$SCABIES, start = c(2021, 09), end = c(2023, 05), frequency = 12))                        
                        
model_train_scabies <- Arima(train_scabies, order = c(1,1,2), seasonal = c(0,1,1))
model_train_scabies

# Forecasting with the train model with test datasets.

predictions <- forecast(model_train_scabies, h = length(test_scabies))
predictions

# Testing the accuracy of the model


accuracy <- accuracy(model_train_scabies, test_scabies)
accuracy

update.packages("forecast")

correct = sum(predictions == test_scabies)
total = length(predictions)
accuracy = correct / total
return (accuracy)


    
model_train_scabies1 <- Arima(train_scabies, order = c(0,1,3))
model_train_scabies1

predictions1 <- forecast(model_train_scabies1, h = length(test_scabies))

accuracy1 <- accuracy(model_train_scabies1)
accuracy








# Fitting a Poisson regression to the Scabies time series
# datasets.

months <- as.factor(Scabies.ts$MONTHS)
Model_pr <- glm(Scabies.ts$SCABIES ~ months, data = Scabies.ts,
                family = "poisson")
summary(Model_pr)

# Visualize the model results

ggplot(Scabies.ts, aes(x = months, y = SCABIES)) +
 geom_point() +
 geom_smooth(method = "glm", family = "poisson")


# Testing for Over dispersion of the fitted Poisson model 

mean <- mean(Scabies.ts$SCABIES)
variance <- var(Scabies.ts$SCABIES)

if (variance > mean){
 print(" The model is overdispersed.")
}else{
 print("The model is not overdispersed.")
}

# Since it is over dispersed, we then fit a negative binomial regression 

data_list <- list(SCABIES = as.numeric(Scabies.ts$SCABIES),
                  MONTHS = factor(Scabies.ts$MONTHS))
summary(data_list)
model_ng <- glm.nb(SCABIES ~ MONTHS, 
                   data = data_list, 
                   na.action = "na.omit",
                   control = glm.control(maxit = 50))
summary(model_ng)







library(MASS)

data(birthwt)

# Fit a Poisson model
model_p <- glm(bwt ~ lwt + factor(race), data = birthwt, family = "poisson")

# Calculate the deviance of the Poisson model
deviance_pr <- Model_pr$deviance


deviance_ng <- model_ng$deviance

# Calculate the dispersion ratio
dispersion_ratio <- deviance_ng - deviance_pr / model_ng$df.residual

# Print the dispersion ratio
print(dispersion_ratio)






















































