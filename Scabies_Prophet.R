library(prophet)
library(tidyverse)
library(readxl)

# importing the datasets.

Original_data <-  read_excel("F:/Project_MK_Ultra/Project_Trials/Original_data_project.xlsx")

# Cleaning the time series data

Scab.ts <- Original_data_project %>% 
 select(MONTHS, SCABIES) %>% 
 rename("ds" = "MONTHS", "y" = "SCABIES")
View(Scab.ts)

# Prophet Model

model <- prophet(Scab.ts)
future_1 <- make_future_dataframe(model, p = 12,
                                  freq = "month")

# forecasting future values

forecast1 <- predict(model, future_1)
forecast1

# Visualizing the forecasted values

dyplot.prophet(model, forecast1)
prophet_plot_components(model, forecast1)


install.packages('remotes')
remotes::install_github('facebook/prophet@*release', subdir = 'R')