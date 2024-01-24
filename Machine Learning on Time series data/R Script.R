##--LOADING REQUIRED LIBRARIES--##
library(tidyverse)
library(prophet)
library(e1071)
library(lubridate)
library(linelist)
library(caret)
library(fpp2)
library(tseries)
library(TTR)
library(forecast)
library(corrgram)
library(corrplot)
library(GGally)
library(dplyr)
library(ggplot2)

#LOAD THE DATASET -- I PARSED THE DATA FROM GITHUB ACCOUNT TO AVOID THE STRESS OF MOVING DATA
IOT <- read.csv("https://raw.githubusercontent.com/Samkickz/IOT-Sensor-Data-forecasting/main/IOT-temp.csv", stringsAsFactors = TRUE)

#DETAILS OF DATASET
summary(IOT)
colnames(IOT)

#REMOVING 'room_id.id' COLUMN BECAUSE IT IS A CONSTANT
IOT_new <- IOT %>% select(-room_id.id)

#RENAMING COLUMN FOR SIMPLER IDENTIFICATION
names(IOT_new) [names(IOT_new) == "noted_date"] <- "time"
names(IOT_new) [names(IOT_new) == "out.in"] <- "location"


#CHECK IF R RECOGNIZES THE DATE/TIME WITH CLASS FUNCTION 
class(IOT_new$time)

#CONVERT DATE FROM TIME COLUMN TO CLASS 'DATE' USING LUBRIDATE
IOT_new$time <- dmy_hm(IOT_new$time)

#CREATE A FUNCTION TO CATEGORIZE TIME INTO SEASONS
seasons = function(x){
  if(x %in% 7:9) return("rainy")
  if(x %in% 10:11) return("post-monsoon")
  if(x %in% 12) return("winter")
}

#CREATE THE COLUMN 'SEASONS'
IOT_new$season = sapply(month(IOT_new$time), seasons)

#CREATE COLUMNS SHOWING DATE AND TIME SEPARATELY
IOT_new$date <- as.Date(IOT_new$time)
IOT_new$hour <- format(as.POSIXct(IOT_new$time), format = "%H:%M:%S")

#CREATE A SEPARATE COLUMN HOLDING VALUES FOR MONTHS
IOT_new$month <- as.Date(as.character(IOT_new$date),  "%Y-%m-%d")

#CONFIRM SUMMARY AND STRUCTURE OF NEW DATAFRAME
summary(IOT_new)
str(IOT_new)

#PLOT SHOWING AVE. TEMP FOR EACH MONTH
res <- aggregate(IOT_new$temp, list(date(IOT_new$month)), mean)
res$month <- as.Date(paste0("2018", res$Group.1, "-08"), "%d-%B-%Y")
ggplot(res, aes(x=Group.1, y=x)) +
  geom_point() + geom_line() + 
  scale_x_date(date_minor_breaks = "1 month",date_labels = "%B")+
  ggtitle("Plot of Ave. Temp for each day between Aug-Dec") +
  xlab("Month") + ylab("Ave. Temp(degree celsius)")

#PLOTS SHOWING COUNT BASED ON SEASONS AND LOCATION OF THE IOT DEVICE
location_count <- IOT_new %>% count(location)
ggplot(location_count, aes(x=location, y=n)) + 
  geom_bar(stat = "identity")

ggplot(IOT_new, 
       aes(x = location, 
           y = temp)) +
  geom_boxplot() +
  labs(title = "Temperature distribution by location")

#CHECK THE STRUCTURE OF THE DATAFRAME
str(IOT_new)

#CHANGE THE 'ID' COLUMN TO CLASS CHARACTER
IOT_new$id<- as.character(IOT_new$id)

#CHANGE THE 'SEASON' COLUMN TO CLASS FACTOR
IOT_new$season<- as.factor(IOT_new$season)

#CREATE A NEW DATAFRAME TO SHOWING THE RELATIONSHIP BETWEEN TEMP, LOCATION AND SEASON
df1 <- data.frame(IOT_new$temp, IOT_new$location, IOT_new$season)
str(df1)

#PLOTS SHOWING THE RELATIONSHIP
plot(df1$IOT_new.temp, df1$IOT_new.location)
plot(df1$IOT_new.temp, df1$IOT_new.season)







##-- MACHINE LEARNING ALGORITHM APPLIED--##



##-PROPHET-##

#FOR TEMPERATURE READINS TAKEN INSIDE THE ROOM#

#FILTER OUT TEMPERATURE READINGS TAKEN OUTSIDE THE ROOM
IOT_newIn = filter(IOT_new, location!='Out')
view(IOT_newIn)
str(IOT_newIn)

#FOR THE SAKE OF THIS ML ALGORITHM, CHOOSE A SINGLE TEMPERATURE PER DAY
#CREATE A NEW DATAFRAME FOR THIS
df2 <- data.frame(IOT_newIn$date, IOT_newIn$temp)
df2 <- df2[!duplicated(df2$IOT_newIn.date), ]
view(df2)
View(summary(df2))

#CREATE A PLOT OF THE NEW DATAFRAME ABOVE
plot(IOT_newIn.temp ~ IOT_newIn.date, df2, type = "l")

#RENAME COLUMNS IN DF2 TO 'DS' AND 'Y' WHICH IS THE ONLY COL NAME PROPHET RECOGNIZES
names(df2) [names(df2) == "IOT_newIn.date"] <- "ds"
names(df2) [names(df2) == "IOT_newIn.temp"] <- "y"

#FINALLY PASS THE DATAFRAME THROUGH THE ALGO LABEL OUTCOME 'M'
m <- prophet(df2)

#MAKE FUTURE DATAFRAME AND LABEL FUTURE
future <- make_future_dataframe(m, periods = 30)#30 DAYS IN A MONTH

#MAKE PREDICTION AND LABEL 'FORECAST'
forecast <- predict(m, future)

#PLOT THE ML ALGO DATA, W.R.T PREDICTED DATA
plot(m, forecast)
tail(forecast)

#CHECK PLOT COMPONENTS AND INFO
prophet_plot_components(m, forecast)



#FOR TEMPERATURE READINS TAKEN OUTSIDE THE ROOM#

#FILTER OUT TEMPERATURE READINGS TAKEN INSIDE THE ROOM
IOT_newOut = filter(IOT_new, location!='In')
view(IOT_newOut)
str(IOT_newOut)

#FOR THE SAKE OF THIS ML ALGORITHM, CHOOSE A SINGLE TEMPERATURE PER DAY
#CREATE A NEW DATAFRAME FOR THIS
df3 <- data.frame(IOT_newOut$date, IOT_newOut$temp)
df3 <- df3[!duplicated(df3$IOT_newOut.date), ]
view(df3)
View(summary(df3))

#CREATE A PLOT OF THE NEW DATAFRAME ABOVE
plot(IOT_newOut.temp ~ IOT_newOut.date, df3, type = "l")

#RENAME COLUMNS IN DF2 TO 'DS' AND 'Y' WHICH IS THE ONLY COL NAME PROPHET RECOGNIZES
names(df3) [names(df3) == "IOT_newOut.date"] <- "ds"
names(df3) [names(df3) == "IOT_newOut.temp"] <- "y"

#FINALLY PASS THE DATAFRAME THROUGH THE ALGO LABEL OUTCOME 'M'
m <- prophet(df3)

#MAKE FUTURE DATAFRAME AND LABEL FUTURE
future <- make_future_dataframe(m, periods = 30)#30 DAYS IN A MONTH

#MAKE PREDICTION AND LABEL 'FORECAST'
forecast <- predict(m, future)

#PLOT THE ML ALGO DATA, W.R.T PREDICTED DATA
plot(m, forecast)
tail(forecast[c("ds", "yhat", "yhat_lower", "yhat_upper")])
tail(forecast)

#CHECK PLOT COMPONENTS AND INFO
prophet_plot_components(m, forecast)





##-NAIVE FORECASTING METHOD-##

#FOR TEMPERATURE READINS TAKEN INSIDE THE ROOM#
glimpse(df2)

#SPLICE/SPLIT DATA INTO TRAINING SET AND TESTING SET
intrain <- createDataPartition(y=df2$y, p= 0.8, list = FALSE)
dat_train <- df2[intrain,]
dat_test <- df2[-intrain,]

#CHECK NUMBER OF ROWS IN TRAINING AND TESTING SET
nrow(dat_train); nrow(dat_test)

#CHANGE DATA TO TS FORMAT, A FORMAT PACKAGE-TSERIES CAN WORK ON
dat_ts <- ts(dat_train[, 2], start = c(2018, 7), end = c(2018, 12), frequency = 12)#12 MONTHS IN A YEAR

#CREATE A FUNCTION MAPE WHICH WILL BE USED TO %ERROR IN PREDICTED DATA
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

#RUN ML ALGO ON THE DATA
naive_mod <- naive(dat_ts, h = 4)

#CHECK SUMMARY
summary(naive_mod)

#RUN TEST FOR %ERROR
dat_test$naive = 25  #FROM PREDICTED VALUE IN SUMMARY
  mape(dat_test$y, dat_test$naive)  



##-SIMPLE EXPONENTIAL SMOOTHING-##
se_model <- ses(dat_ts, h = 4)

#SUMMARY
summary(se_model)

#%ERROR
dat_test$simplexp = 29
  mape(dat_test$y, dat_test$simplexp) 




#FOR TEMPERATURE READINS TAKEN OUTSIDE THE ROOM#
glimpse(df3)

#SPLICE/SPLIT DATA INTO TRAINING SET AND TESTING SET
intrain <- createDataPartition(y=df3$y, p= 0.8, list = FALSE)
dat_train <- df3[intrain,]
dat_test <- df3[-intrain,]

#CHECK NUMBER OF ROWS IN TRAINING AND TESTING SET
nrow(dat_train); nrow(dat_test)

#CHANGE DATA TO TS FORMAT, A FORMAT PACKAGE-TSERIES CAN WORK ON
dat_ts <- ts(dat_train[, 2], start = c(2018, 7), end = c(2018, 12), frequency = 12)#12 MONTHS IN A YEAR

#CREATE A FUNCTION MAPE WHICH WILL BE USED TO %ERROR IN PREDICTED DATA
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

#RUN ML ALGO ON THE DATA
naive_mod <- naive(dat_ts, h = 4)#FOR THE NEXT FOLLOWING 4 MONTHS

#CHECK SUMMARY
summary(naive_mod)

#RUN TEST FOR %ERROR
dat_test$naive = 36 #FROM PREDICTED VALUE IN SUMMARY
  mape(dat_test$y, dat_test$naive)




##-SIMPLE EXPONENTIAL SMOOTHING-##
se_model <- ses(dat_ts, h = 4)

#CHECK FOR SUMMARY OF THE MODEL CREATED
summary(se_model)

#%ERROR 
dat_test$simplexp = 36
  mape(dat_test$y, dat_test$simplexp) 