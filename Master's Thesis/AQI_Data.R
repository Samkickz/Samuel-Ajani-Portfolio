# Install and Load necessary libraries
install.packages("dplyr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("prophet")
install.packages("corrplot")
install.packages("GGally")
install.packages("forecast")
library(readxl) # for reading Excel files
library(tseries) #for stationarity check
library(forecast) #for ARIMA modelling
library(GGally) #for pair plots
library(corrplot) #for correlation plot
library(prophet) #for prophet algorithm
library(dplyr) #for data manipulation
library(ggplot2) #for data visualization
library(plotly) #for advanced visualization

# Set the working directory and Load the dataset
url <- "https://github.com/Samkickz/AQI-forecast/raw/main/AQI_Data.xlsx"
destfile <- "AQI_Data.xlsx"
curl::curl_download(url, destfile)
AQI_Data <- read_excel(destfile)
View(AQI_Data)


##EXPLORATORY DATA ANALYSIS (EDA)##
# View the first few rows of the dataset
head(AQI_Data)

# Summarize the dataset
summary(AQI_Data)

# Check for missing values
sum(is.na(AQI_Data))

# Histogram of DEFRA_AQI
ggplot(AQI_Data, aes(x = DEFRA_AQI)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") + 
  labs(title = "Histogram of DEFRA_AQI", x = "DEFRA_AQI", y = "Frequency")

#Bubble chart showing possible connections between PM2.5, PM10, & AQI
ggplot(AQI_Data, aes(x = PM2.5, y = PM10, size = DEFRA_AQI)) +
  geom_point(alpha = 0.6) +
  labs(title = "Bubble Chart of PM2.5 vs PM10", x = "PM2.5", y = "PM10")

# Calculating correlations
cor_data <- cor(AQI_Data[, sapply(AQI_Data, is.numeric)])  # Select only numeric columns

# Setting plot dimensions and centralizing the plot
# Adjust 'pin' for plot size and 'mai' for margins (bottom, left, top, right)
par(pin = c(5, 5), mai = c(2, 0, 2, 2))

# Plotting the heatmap
corrplot(cor_data, method = "color")

# Select a subset of variables for the pair plot
selected_data <- AQI_Data[, c("DEFRA_AQI", "Temp_Celsius", "Humidity", "PM2.5")]

# Create a pair plot
ggpairs(selected_data)

# Selecting a subset of numeric variables
selected_data2 <- AQI_Data[, c("DEFRA_AQI", "UV_index", "Humidity", "Cloud_%", "Precip_inches")]

# Creating a scatter plot matrix
ggpairs(selected_data2)



# INTERACTIVE PLOTS#
#Precipation vs AQI
p <- ggplot(AQI_Data, aes(x = Precip_inches, y = DEFRA_AQI)) + 
  geom_point()
ggplotly(p)

# Scatter plot of CO vs. NO2
p2 <- plot_ly(AQI_Data, x = ~CO, y = ~NO2, type = 'scatter', mode = 'markers')
p2

# 3D Scatter Plot Temp vs Humidity vs CO
p3 <- plot_ly(AQI_Data, x = ~Temp_Celsius, y = ~Humidity, z = ~CO, type = 'scatter3d', mode = 'markers')
p3

# 3D Scatter Plot CO vs PM2.5 vs AQI
p4 <- plot_ly(AQI_Data, x = ~CO, y = ~PM2.5, z = ~DEFRA_AQI, type = 'scatter3d', mode = 'markers')
p4

# Pie Chart of data points by AQI
p5 <- plot_ly(AQI_Data, labels = ~DEFRA_AQI, type = 'pie')
p5

# Aggregating data by country
agg_data <- aggregate(cbind(PM2.5, PM10, NO2, SO2, CO, O3, DEFRA_AQI, Temp_Celsius, Humidity) ~ country, data = AQI_Data, mean)
#view aggregate
View(agg_data)

# Sorting the aggregated data by average DEFRA_AQI in descending order
agg_data <- agg_data[order(-agg_data$DEFRA_AQI), ]

# Selecting the top five countries with the highest average DEFRA_AQI
top_five_countries <- head(agg_data, 5)

# Viewing the result
View(top_five_countries)


#NARROW DOWN THE EDA TO THE TOP FIVE COUNTIRES#
# Filtering data for each country
india_data <- subset(AQI_Data, country == "India")
china_data <- subset(AQI_Data, country == "China")
pakistan_data <- subset(AQI_Data, country == "Pakistan")
malaysia_data <- subset(AQI_Data, country == "Malaysia")
vietnam_data <- subset(AQI_Data, country == "Vietnam")

# Define a function to summarize the data
summarize_data <- function(data) {
  library(dplyr)
  data %>%
    group_by(Date) %>%
    summarize(PM25_mean = mean(PM2.5, na.rm = TRUE),
              PM10_mean = mean(PM10, na.rm = TRUE),
              NO2_mean = mean(NO2, na.rm = TRUE),
              SO2_mean = mean(SO2, na.rm = TRUE),
              CO_mean = mean(CO, na.rm = TRUE),
              O3_mean = mean(O3, na.rm = TRUE),
              Temp_Celsius_mean = mean(Temp_Celsius, na.rm = TRUE),
              Humidity_mean = mean(Humidity, na.rm = TRUE),
              DEFRA_AQI_mean = mean(DEFRA_AQI, na.rm = TRUE))
}

# Applying the function to each country's data
india_summary <- summarize_data(india_data)
china_summary <- summarize_data(china_data)
pakistan_summary <- summarize_data(pakistan_data)
malaysia_summary <- summarize_data(malaysia_data)
vietnam_summary <- summarize_data(vietnam_data)

# View the first few rows of the resulting dataframe for one of the countries, e.g., India
head(india_summary)

#EDA FOR INDIA#
#Trend Analysis
ggplot(india_summary, aes(x = Date, y = DEFRA_AQI_mean)) + 
  geom_line(group = 1, color = "blue") + 
  labs(title = "Trend of DEFRA_AQI over Time in India", x = "Date", y = "Average DEFRA_AQI")

#Density plot
ggplot(india_summary, aes(x = DEFRA_AQI_mean)) + 
  geom_density(fill = "red") + 
  labs(title = "Density Plot of DEFRA_AQI in India", x = "Average DEFRA_AQI", y = "Density")

#Correlation analysis
cor_matrix <- cor(india_summary[, sapply(india_summary, is.numeric)])
corrplot(cor_matrix, method = "circle")

#Pair plots
ggpairs(india_summary[, c("PM25_mean", "PM10_mean", "NO2_mean", "SO2_mean", "CO_mean", "O3_mean", "Temp_Celsius_mean", "Humidity_mean", "DEFRA_AQI_mean")])

#Multi-variable trend analysis
ggplot(india_summary) + 
  geom_line(aes(x = Date, y = PM25_mean, colour = "PM2.5")) + 
  geom_line(aes(x = Date, y = PM10_mean, colour = "PM10")) +
  labs(title = "Time Series of PM2.5 and PM10 in India", x = "Date", y = "Concentration")

#Multi-variable Scatter Plot 
ggplot(india_summary, aes(x = Temp_Celsius_mean, y = DEFRA_AQI_mean, color = Humidity_mean)) + 
  geom_point(size = 3, alpha = 0.8) + 
  scale_color_gradient(low = "blue", high = "red") +  
  labs(title = "AQI vs Temperature Colored by Humidity", x = "Temperature (Celsius)", y = "AQI Mean") +
  theme_minimal() 

#Interactive plot between the gases
p6 <- plot_ly(india_summary, x = ~NO2_mean, y = ~CO_mean, z = ~SO2_mean, type = 'scatter3d', mode = 'markers')
p6


#EDA FOR CHINA#
#Trend Analysis
ggplot(china_summary, aes(x = Date, y = DEFRA_AQI_mean)) + 
  geom_line(group = 1, color = "blue") + 
  labs(title = "Trend of DEFRA_AQI over Time in China", x = "Date", y = "Average DEFRA_AQI")

#Density plot
ggplot(china_summary, aes(x = DEFRA_AQI_mean)) + 
  geom_density(fill = "red") + 
  labs(title = "Density Plot of DEFRA_AQI in China", x = "Average DEFRA_AQI", y = "Density")

#Correlation analysis
cor_matrix <- cor(china_summary[, sapply(china_summary, is.numeric)])
corrplot(cor_matrix, method = "circle")

#Pair plots
ggpairs(china_summary[, c("PM25_mean", "PM10_mean", "NO2_mean", "SO2_mean", "CO_mean", "O3_mean", "Temp_Celsius_mean", "Humidity_mean", "DEFRA_AQI_mean")])

#Multi-variable trend analysis
ggplot(china_summary) + 
  geom_line(aes(x = Date, y = PM25_mean, colour = "PM2.5")) + 
  geom_line(aes(x = Date, y = PM10_mean, colour = "PM10")) +
  labs(title = "Time Series of PM2.5 and PM10 in China", x = "Date", y = "Concentration")

#Multi-variable Scatter Plot 
ggplot(china_summary, aes(x = Temp_Celsius_mean, y = DEFRA_AQI_mean, color = Humidity_mean)) + 
  geom_point(size = 3, alpha = 0.8) + 
  scale_color_gradient(low = "blue", high = "red") +  
  labs(title = "AQI vs Temperature Colored by Humidity", x = "Temperature (Celsius)", y = "AQI Mean") +
  theme_minimal() 

#Interactive plot between the gases
p7 <- plot_ly(china_summary, x = ~NO2_mean, y = ~CO_mean, z = ~SO2_mean, type = 'scatter3d', mode = 'markers')
p7


#EDA FOR PAKISTAN#
#Trend Analysis
ggplot(pakistan_summary, aes(x = Date, y = DEFRA_AQI_mean)) + 
  geom_line(group = 1, color = "blue") + 
  labs(title = "Trend of DEFRA_AQI over Time in Pakistan", x = "Date", y = "Average DEFRA_AQI")

#Density plot
ggplot(pakistan_summary, aes(x = DEFRA_AQI_mean)) + 
  geom_density(fill = "red") + 
  labs(title = "Density Plot of DEFRA_AQI in Pakistan", x = "Average DEFRA_AQI", y = "Density")

#Correlation analysis
cor_matrix <- cor(pakistan_summary[, sapply(pakistan_summary, is.numeric)])
corrplot(cor_matrix, method = "circle")

#Pair plots
ggpairs(pakistan_summary[, c("PM25_mean", "PM10_mean", "NO2_mean", "SO2_mean", "CO_mean", "O3_mean", "Temp_Celsius_mean", "Humidity_mean", "DEFRA_AQI_mean")])

#Multi-variable trend analysis
ggplot(pakistan_summary) + 
  geom_line(aes(x = Date, y = PM25_mean, colour = "PM2.5")) + 
  geom_line(aes(x = Date, y = PM10_mean, colour = "PM10")) +
  labs(title = "Time Series of PM2.5 and PM10 in Pakistan", x = "Date", y = "Concentration")

#Multi-variable Scatter Plot 
ggplot(pakistan_summary, aes(x = Temp_Celsius_mean, y = DEFRA_AQI_mean, color = Humidity_mean)) + 
  geom_point(size = 3, alpha = 0.8) + 
  scale_color_gradient(low = "blue", high = "red") +  
  labs(title = "AQI vs Temperature Colored by Humidity", x = "Temperature (Celsius)", y = "AQI Mean") +
  theme_minimal() 

#Interactive plot between the gases
p8 <- plot_ly(pakistan_summary, x = ~NO2_mean, y = ~CO_mean, z = ~SO2_mean, type = 'scatter3d', mode = 'markers')
p8


#EDA FOR MALAYSIA#
#Trend Analysis
ggplot(malaysia_summary, aes(x = Date, y = DEFRA_AQI_mean)) + 
  geom_line(group = 1, color = "blue") + 
  labs(title = "Trend of DEFRA_AQI over Time in Malaysia", x = "Date", y = "Average DEFRA_AQI")

#Density plot
ggplot(malaysia_summary, aes(x = DEFRA_AQI_mean)) + 
  geom_density(fill = "red") + 
  labs(title = "Density Plot of DEFRA_AQI in Malaysia", x = "Average DEFRA_AQI", y = "Density")

#Correlation analysis
cor_matrix <- cor(malaysia_summary[, sapply(malaysia_summary, is.numeric)])
corrplot(cor_matrix, method = "circle")

#Pair plots
ggpairs(malaysia_summary[, c("PM25_mean", "PM10_mean", "NO2_mean", "SO2_mean", "CO_mean", "O3_mean", "Temp_Celsius_mean", "Humidity_mean", "DEFRA_AQI_mean")])

#Multi-variable trend analysis
ggplot(malaysia_summary) + 
  geom_line(aes(x = Date, y = PM25_mean, colour = "PM2.5")) + 
  geom_line(aes(x = Date, y = PM10_mean, colour = "PM10")) +
  labs(title = "Time Series of PM2.5 and PM10 in Malaysia", x = "Date", y = "Concentration")

#Multi-variable Scatter Plot 
ggplot(malaysia_summary, aes(x = Temp_Celsius_mean, y = DEFRA_AQI_mean, color = Humidity_mean)) + 
  geom_point(size = 3, alpha = 0.8) + 
  scale_color_gradient(low = "blue", high = "red") +  
  labs(title = "AQI vs Temperature Colored by Humidity", x = "Temperature (Celsius)", y = "AQI Mean") +
  theme_minimal() 

#Interactive plot between the gases
p9 <- plot_ly(malaysia_summary, x = ~NO2_mean, y = ~CO_mean, z = ~SO2_mean, type = 'scatter3d', mode = 'markers')
p9


#EDA FOR VIETNAM#
#Trend Analysis
ggplot(vietnam_summary, aes(x = Date, y = DEFRA_AQI_mean)) + 
  geom_line(group = 1, color = "blue") + 
  labs(title = "Trend of DEFRA_AQI over Time in Vietnam", x = "Date", y = "Average DEFRA_AQI")

#Density plot
ggplot(vietnam_summary, aes(x = DEFRA_AQI_mean)) + 
  geom_density(fill = "red") + 
  labs(title = "Density Plot of DEFRA_AQI in Vietnam", x = "Average DEFRA_AQI", y = "Density")

#Correlation analysis
cor_matrix <- cor(vietnam_summary[, sapply(vietnam_summary, is.numeric)])
corrplot(cor_matrix, method = "circle")

#Pair plots
ggpairs(vietnam_summary[, c("PM25_mean", "PM10_mean", "NO2_mean", "SO2_mean", "CO_mean", "O3_mean", "Temp_Celsius_mean", "Humidity_mean", "DEFRA_AQI_mean")])

#Multi-variable trend analysis
ggplot(vietnam_summary) + 
  geom_line(aes(x = Date, y = PM25_mean, colour = "PM2.5")) + 
  geom_line(aes(x = Date, y = PM10_mean, colour = "PM10")) +
  labs(title = "Time Series of PM2.5 and PM10 in Vietnam", x = "Date", y = "Concentration")

#Multi-variable Scatter Plot 
ggplot(vietnam_summary, aes(x = Temp_Celsius_mean, y = DEFRA_AQI_mean, color = Humidity_mean)) + 
  geom_point(size = 3, alpha = 0.8) + 
  scale_color_gradient(low = "blue", high = "red") +  
  labs(title = "AQI vs Temperature Colored by Humidity", x = "Temperature (Celsius)", y = "AQI Mean") +
  theme_minimal() 

#Interactive plot between the gases
p10 <- plot_ly(vietnam_summary, x = ~NO2_mean, y = ~CO_mean, z = ~SO2_mean, type = 'scatter3d', mode = 'markers')
p10



##ARIMA ALGORITHM##

#ARIMA FOR INDIA
# Convert the DEFRA_AQI_mean to a time series object
India_AQI_ts <- ts(india_summary$DEFRA_AQI_mean, frequency = 365)  # because our data is collected daily

# Check for stationarity using the Augmented Dickey-Fuller Test
adf.test(India_AQI_ts)

# Auto identify the best ARIMA model
auto_model_India <- auto.arima(India_AQI_ts)
summary(auto_model_India)

# Fit ARIMA model
India_arima_model <- arima(India_AQI_ts, order = c(0, 1, 1))
summary(India_arima_model)

# Check model diagnostics
checkresiduals(India_arima_model)

# Forecast future values for next 30 days
forecast_values <- forecast(India_arima_model, h = 30)  # 'h' is the number of periods to forecast
plot(forecast_values)



#ARIMA FOR CHINA
# Convert the DEFRA_AQI_mean to a time series object
china_AQI_ts <- ts(china_summary$DEFRA_AQI_mean, frequency = 365)  # because our data is collected daily

# Check for stationarity using the Augmented Dickey-Fuller Test
adf.test(china_AQI_ts)

# Auto identify the best ARIMA model
auto_model_china <- auto.arima(china_AQI_ts)
summary(auto_model_china)

# Fit ARIMA model
China_arima_model <- arima(china_AQI_ts, order = c(0, 0, 1))
summary(China_arima_model)

# Check model diagnostics
checkresiduals(China_arima_model)

# Forecast future values for next 30 days
forecast_values <- forecast(China_arima_model, h = 30)  # 'h' is the number of periods to forecast
plot(forecast_values)



#ARIMA FOR PAKISTAN
# Convert the DEFRA_AQI_mean to a time series object
pakistan_AQI_ts <- ts(pakistan_summary$DEFRA_AQI_mean, frequency = 365)  # because our data is collected daily

# Check for stationarity using the Augmented Dickey-Fuller Test
adf.test(pakistan_AQI_ts)

# Auto identify the best ARIMA model
auto_model_pakistan <- auto.arima(pakistan_AQI_ts)
summary(auto_model_pakistan)

# Fit ARIMA model
pakistan_arima_model <- arima(pakistan_AQI_ts, order = c(0, 1, 2))
summary(pakistan_arima_model)

# Check model diagnostics
checkresiduals(pakistan_arima_model)

# Forecast future values for next 30 days
forecast_values <- forecast(pakistan_arima_model, h = 30)  # 'h' is the number of periods to forecast
plot(forecast_values)



#ARIMA FOR MALAYSIA
# Convert the DEFRA_AQI_mean to a time series object
malaysia_AQI_ts <- ts(malaysia_summary$DEFRA_AQI_mean, frequency = 365)  # because our data is collected daily

# Check for stationarity using the Augmented Dickey-Fuller Test
adf.test(malaysia_AQI_ts)

# Auto identify the best ARIMA model
auto_model_malaysia <- auto.arima(malaysia_AQI_ts)
summary(auto_model_malaysia)

# Fit ARIMA model
malaysia_arima_model <- arima(malaysia_AQI_ts, order = c(1, 0, 0))
summary(malaysia_arima_model)

# Check model diagnostics
checkresiduals(malaysia_arima_model)

# Forecast future values for next 30 days
forecast_values <- forecast(malaysia_arima_model, h = 30)  # 'h' is the number of periods to forecast
plot(forecast_values)



#ARIMA FOR VIETNAM
# Convert the DEFRA_AQI_mean to a time series object
vietnam_AQI_ts <- ts(vietnam_summary$DEFRA_AQI_mean, frequency = 365)  # because our data is collected daily

# Check for stationarity using the Augmented Dickey-Fuller Test
adf.test(vietnam_AQI_ts)

# Auto identify the best ARIMA model
auto_model_vietnam <- auto.arima(vietnam_AQI_ts)
summary(auto_model_vietnam)

# Fit ARIMA model
vietnam_arima_model <- arima(vietnam_AQI_ts, order = c(0, 0, 1))
summary(vietnam_arima_model)

# Check model diagnostics
checkresiduals(vietnam_arima_model)

# Forecast future values for next 30 days
forecast_values <- forecast(vietnam_arima_model, h = 30)  # 'h' is the number of periods to forecast
plot(forecast_values)



##LINEAR REGRESSION##
# Linear Regression for India
lm_india <- lm(DEFRA_AQI_mean ~ PM25_mean + PM10_mean + NO2_mean + SO2_mean + CO_mean + O3_mean + Temp_Celsius_mean + Humidity_mean, data = india_summary)
summary(lm_india)

# Plotting diagnostic plots for the linear model
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(lm_india)

# Predictions for India
predictions_india <- predict(lm_india, newdata = india_summary)

# Create a data frame with actual and predicted values
results_india <- data.frame(Actual = india_summary$DEFRA_AQI_mean, Predicted = predictions_india)

# View the first few rows of the results
head(results_india)

# Plotting actual vs predicted values
ggplot(results_india, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted DEFRA_AQI for India", x = "Actual", y = "Predicted")



# Linear Regression for china
lm_china <- lm(DEFRA_AQI_mean ~ PM25_mean + PM10_mean + NO2_mean + SO2_mean + CO_mean + O3_mean + Temp_Celsius_mean + Humidity_mean, data = china_summary)
summary(lm_china)

# Plotting diagnostic plots for the linear model
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(lm_china)

# Predictions for china
predictions_china <- predict(lm_china, newdata = china_summary)

# Create a data frame with actual and predicted values
results_china <- data.frame(Actual = china_summary$DEFRA_AQI_mean, Predicted = predictions_china)

# View the first few rows of the results
head(results_china)

# Plotting actual vs predicted values
ggplot(results_china, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted DEFRA_AQI for china", x = "Actual", y = "Predicted")



# Linear Regression for Pakistan
lm_pakistan <- lm(DEFRA_AQI_mean ~ PM25_mean + PM10_mean + NO2_mean + SO2_mean + CO_mean + O3_mean + Temp_Celsius_mean + Humidity_mean, data = pakistan_summary)
summary(lm_pakistan)

# Plotting diagnostic plots for the linear model
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(lm_pakistan)

# Predictions for pakistan
predictions_pakistan <- predict(lm_pakistan, newdata = pakistan_summary)

# Create a data frame with actual and predicted values
results_pakistan <- data.frame(Actual = pakistan_summary$DEFRA_AQI_mean, Predicted = predictions_pakistan)

# View the first few rows of the results
head(results_pakistan)

# Plotting actual vs predicted values
ggplot(results_pakistan, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted DEFRA_AQI for pakistan", x = "Actual", y = "Predicted")


# Linear Regression for Malaysia
lm_malaysia <- lm(DEFRA_AQI_mean ~ PM25_mean + PM10_mean + NO2_mean + SO2_mean + CO_mean + O3_mean + Temp_Celsius_mean + Humidity_mean, data = malaysia_summary)
summary(lm_malaysia)

# Plotting diagnostic plots for the linear model
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(lm_malaysia)

# Predictions for malaysia
predictions_malaysia <- predict(lm_malaysia, newdata = malaysia_summary)

# Create a data frame with actual and predicted values
results_malaysia <- data.frame(Actual = malaysia_summary$DEFRA_AQI_mean, Predicted = predictions_malaysia)

# View the first few rows of the results
head(results_malaysia)

# Plotting actual vs predicted values
ggplot(results_malaysia, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted DEFRA_AQI for malaysia", x = "Actual", y = "Predicted")


# Linear Regression for Vietnam
lm_vietnam <- lm(DEFRA_AQI_mean ~ PM25_mean + PM10_mean + NO2_mean + SO2_mean + CO_mean + O3_mean + Temp_Celsius_mean + Humidity_mean, data = vietnam_summary)
summary(lm_vietnam)

# Plotting diagnostic plots for the linear model
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(lm_vietnam)

# Predictions for vietnam
predictions_vietnam <- predict(lm_vietnam, newdata = vietnam_summary)

# Create a data frame with actual and predicted values
results_vietnam <- data.frame(Actual = vietnam_summary$DEFRA_AQI_mean, Predicted = predictions_vietnam)

# View the first few rows of the results
head(results_vietnam)

# Plotting actual vs predicted values
ggplot(results_vietnam, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted DEFRA_AQI for vietnam", x = "Actual", y = "Predicted")





##PROPHET FOR FORECASTING##

#Prophet for India
#Initialize a Prophet model
m <- prophet()

#'Date' is the date column and 'DEFRA_AQI_mean' is the variable to forecast
india_df <- data.frame(ds = india_summary$Date, y = india_summary$DEFRA_AQI_mean)

#Fit the model for India
m_india <- prophet(india_df)

#Here, 'periods' is the number of periods to forecast, and 'freq' is the data frequency
future_india <- make_future_dataframe(m_india, periods = 30, freq = "day")
forecast_india <- predict(m_india, future_india)

# View the forecast
tail(forecast_india[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

#Plot the forecast
plot(m_india, forecast_india)
prophet_plot_components(m_india, forecast_india)

# Using the entire dataset for training
train_india <- india_df

# Fit the Prophet model
m_india <- prophet(train_india)

# Using time series cross-validation for evaluation
df_cv <- cross_validation(m_india, initial = 30, period = 10, horizon = 20, units = 'days')
df_performance <- performance_metrics(df_cv)
print(df_performance)

#Prediction Error plot
ggplot(df_cv, aes(x = yhat, y = y)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Prediction Error Plot",
       x = "Predicted",
       y = "Actual")




#Prophet for China
#Initialize a Prophet model
m <- prophet()

#'Date' is the date column and 'DEFRA_AQI_mean' is the variable to forecast
china_df <- data.frame(ds = china_summary$Date, y = china_summary$DEFRA_AQI_mean)

#Fit the model for china
m_china <- prophet(china_df)

#Here, 'periods' is the number of periods to forecast, and 'freq' is the data frequency
future_china <- make_future_dataframe(m_china, periods = 30, freq = "day")
forecast_china <- predict(m_china, future_china)

# View the forecast
tail(forecast_china[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

#Plot the forecast
plot(m_china, forecast_china)
prophet_plot_components(m_china, forecast_china)

# Using the entire dataset for training
train_china <- china_df

# Fit the Prophet model
m_china <- prophet(train_china)

# Using time series cross-validation for evaluation
df_cv <- cross_validation(m_china, initial = 30, period = 10, horizon = 20, units = 'days')
df_performance <- performance_metrics(df_cv)
print(df_performance)

#Prediction Error plot
ggplot(df_cv, aes(x = yhat, y = y)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Prediction Error Plot",
       x = "Predicted",
       y = "Actual")




#Prophet for Pakistan
#Initialize a Prophet model
m <- prophet()

#'Date' is the date column and 'DEFRA_AQI_mean' is the variable to forecast
pakistan_df <- data.frame(ds = pakistan_summary$Date, y = pakistan_summary$DEFRA_AQI_mean)

#Fit the model for pakistan
m_pakistan <- prophet(pakistan_df)

#Here, 'periods' is the number of periods to forecast, and 'freq' is the data frequency
future_pakistan <- make_future_dataframe(m_pakistan, periods = 30, freq = "day")
forecast_pakistan <- predict(m_pakistan, future_pakistan)

# View the forecast
tail(forecast_pakistan[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

#Plot the forecast
plot(m_pakistan, forecast_pakistan)
prophet_plot_components(m_pakistan, forecast_pakistan)

# Using the entire dataset for training
train_pakistan <- pakistan_df

# Fit the Prophet model
m_pakistan <- prophet(train_pakistan)

# Using time series cross-validation for evaluation
df_cv <- cross_validation(m_pakistan, initial = 30, period = 10, horizon = 20, units = 'days')
df_performance <- performance_metrics(df_cv)
print(df_performance)

#Prediction Error plot
ggplot(df_cv, aes(x = yhat, y = y)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Prediction Error Plot",
       x = "Predicted",
       y = "Actual")



#Prophet for Malaysia
#Initialize a Prophet model
m <- prophet()

#'Date' is the date column and 'DEFRA_AQI_mean' is the variable to forecast
malaysia_df <- data.frame(ds = malaysia_summary$Date, y = malaysia_summary$DEFRA_AQI_mean)

#Fit the model for malaysia
m_malaysia <- prophet(malaysia_df)

#Here, 'periods' is the number of periods to forecast, and 'freq' is the data frequency
future_malaysia <- make_future_dataframe(m_malaysia, periods = 30, freq = "day")
forecast_malaysia <- predict(m_malaysia, future_malaysia)

# View the forecast
tail(forecast_malaysia[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

#Plot the forecast
plot(m_malaysia, forecast_malaysia)
prophet_plot_components(m_malaysia, forecast_malaysia)

# Using the entire dataset for training
train_malaysia <- malaysia_df

# Fit the Prophet model
m_malaysia <- prophet(train_malaysia)

# Using time series cross-validation for evaluation
df_cv <- cross_validation(m_malaysia, initial = 30, period = 10, horizon = 20, units = 'days')
df_performance <- performance_metrics(df_cv)
print(df_performance)

#Prediction Error plot
ggplot(df_cv, aes(x = yhat, y = y)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Prediction Error Plot",
       x = "Predicted",
       y = "Actual")


#Prophet for Vietnam
#Initialize a Prophet model
m <- prophet()

#'Date' is the date column and 'DEFRA_AQI_mean' is the variable to forecast
vietnam_df <- data.frame(ds = vietnam_summary$Date, y = vietnam_summary$DEFRA_AQI_mean)

#Fit the model for vietnam
m_vietnam <- prophet(vietnam_df)

#Here, 'periods' is the number of periods to forecast, and 'freq' is the data frequency
future_vietnam <- make_future_dataframe(m_vietnam, periods = 30, freq = "day")
forecast_vietnam <- predict(m_vietnam, future_vietnam)

# View the forecast
tail(forecast_vietnam[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

#Plot the forecast
plot(m_vietnam, forecast_vietnam)
prophet_plot_components(m_vietnam, forecast_vietnam)

# Using the entire dataset for training
train_vietnam <- vietnam_df

# Fit the Prophet model
m_vietnam <- prophet(train_vietnam)

# Using time series cross-validation for evaluation
df_cv <- cross_validation(m_vietnam, initial = 30, period = 10, horizon = 20, units = 'days')
df_performance <- performance_metrics(df_cv)
print(df_performance)

#Prediction Error plot
ggplot(df_cv, aes(x = yhat, y = y)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Prediction Error Plot",
       x = "Predicted",
       y = "Actual")
