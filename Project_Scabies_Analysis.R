# Installing the packages to be used 

install.packages(c("forecast", "tidyverse","kableExtra", "TSA", "tseries", "TSstudio",
                   "prophet", "gtsummary", "tsibble", "fable", "readxl", "lattice",
                   "latticeExtra", "lubricate","Kendall"))

# Loading the required packages for the analysis.

library(forecast)
library(tidyverse)
library(kableExtra)
library(TSA)
library(tseries)
library(TSstudio)
library(prophet)
library(gtsummary)
library(tsibble)
library(fable)
library(readxl)
library(lattice)
library(latticeExtra)
library(lubridate)
library(Kendall)

# Importing the datasets

Original_data_project <- read_excel("F:/Project_MK_Ultra/Project_Trials/Original_data_project.xlsx")
View(Original_data_project)

# Cleaning the time series data

Scabies.ts <- Original_data_project %>% 
 select(MONTHS, SCABIES)
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
        color = "black",
        Xtitle = "Monthly",
        Ytitle = "Number of cases")


### Checking for stationarity of the time series data

# Using the ADF test
# Ho: The Scabies time series data is not stationary.
# Ha: The Scabies time series data is stationary.

adf.test(Scabies_ts)

# Conclusion: Since the p-value (0.06197) is greater than alpha value(0.05),
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
                         differences = 1)
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
     main = "An PACF Plot of The First Differenced of the Scabies Time Series datasets.")

# From the ACF and PACF plots the model that can be derived from it is an
# ARIMA(1,1,1), ARIMA(2,1,2) and ARIMA(0,1,3).
# We then build competing models with the models obtain.

Model_1 <- Arima(Scabies_ts, order = c(1,1,1))
Model_1  # AIC=979.68   AICc=979.97   BIC=987.12

Model_2 <- Arima(Scabies_ts, order = c(0,1,3))
Model_2  # AIC=975.89   AICc=976.37   BIC=985.8   

Model_3 <- Arima(Scabies_ts, order = c(2,1,2))
Model_3  # AIC=978.34   AICc=979.08   BIC=990.73

Model_4 <- Arima(Scabies_ts, order = c(1,1,3))
Model_4  # AIC=977.87   AICc=978.6   BIC=990.26

Model_5 <- Arima(Scabies_ts, order = c(1,1,2))
Model_5  # AIC=976.69   AICc=977.18   BIC=986.6

Model_6 <- Arima(Scabies_ts, order = c(0,1,3), seasonal = c(0,0,1))
Model_6  # AIC=977.06   AICc=977.79   BIC=989.44

Model_7 <- Arima(Scabies_ts, order = c(1,1,2), seasonal = c(1,0,0))
Model_7  # AIC=977.84   AICc=978.58   BIC=990.23

Model_8 <- Arima(Scabies_ts, order = c(1,1,1), seasonal = c(0,0,1))
Model_8  # AIC=981.02   AICc=981.5   BIC=990.92

Model_9 <- Arima(Scabies_ts, order = c(1,1,3), seasonal = c(0,0,1))
Model_9  # AIC=979   AICc=980.04   BIC=993.87

Model_10 <- Arima(Scabies_ts, order = c(2,1,2), seasonal = c(1,0,0))
Model_10  # AIC=979.48   AICc=980.52   BIC=994.35


# From the competing models, the optimal model is Model_2 which is 
# ARIMA(0,1,3) which an AIC=975.89, AICc=976.37, and a  BIC=985.8 which 
# is the least of all of the models.

# We then check for the appropriateness of the model, normality and randomness 
# of the residuals.

# Checking the normality of the residuals

# Ho: The residuals of ARIMA(0,1,3) is normally distributed.
# Ha: The residuals of ARIMA(0,1,3) is not normally distributed.

shapiro.test(residuals(Model_2))

# Conclusion: Since the p-value, 0.1285 is greater than alpha value, 0.05
# we fail to reject Ho and conclude that the residuals of the model ARIMA(0,1,3)
# is normally distributed.

# Checking for the appropriateness of the model.

# Ho: The model ARIMA(0,1,3) is appropriate.
# Ha: The model ARIMA(0,1,3) is not appropriate.

checkresiduals(Model_2)
check_res(Model_2)

# Conclusion: Since the p-value(0.9583) is greater than the alpha value(0.05)
# we fail to reject Ho and conclude that the model ARIMA(0,1,3) is appropriate.


# Since all the assumption are met, we then go ahead and forecast

# Forecasted Values for the next 12 months of the Scabies datasets

Forecasted_values <- forecast(Model_2, h = 12)
Forecasted_values

# Visualizing the forecasted values for the next 12 months.

plot_forecast(Forecasted_values,
              title = "Forecast of the ARIMA(0,1,3) model of the Scabies TS data.",
              Xtitle = "Months",
              Ytitle = "Number of cases",
              color = "green")
