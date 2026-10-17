
library(zoo)
path <- "C:/Users/Usuari/Downloads/Series_temporals/IPC_CSV.csv"

content <- read.csv2(path, header = TRUE)
content <- content[1:297, ]
values<- as.numeric(content[,4])
raw_dates <- content[,3]
clean_dates <- as.Date(as.yearmon(raw_dates , format ="%YM%m")) 
plot(clean_dates, values, type = "l", main = "IPC Time Series", xlab = "Date", ylab = "Value")
#Now we forecast next time-series points using R 
library(lubridate)
library(forecast)


ipc_ts <- ts(rev(values), start = c(2002, 1), frequency = 12) #Generate time series object

fit<- auto.arima(ipc_ts)
forecasted_values <- forecast(fit,1)  #Predict the next month IPC 

plot(forecasted_values , xlab= "Period" , ylab = "IPC value" , main = "IPC evolution within the last 20 years", col.main = "darkgreen")

last_val <- tail(forecasted_values$mean , 1)

sprintf("The predicted value of the IPC for 2026 October is : %.2f  ",last_val)

#--------------------------------NEURAL NETWORK-------------------------------
#The prediction will be done using a Neural Network algorithm , amore sofisticated machine algorithm, and compare the value with the former prediction
#Import libraries
library(data.table)
library(mlbench)
library(deepnet)
library(caret)
library(forecast)


set.seed(123)
data <- content[1:297, 3:4]  #Filter out the missing values


# Filter out rows with missing values 
data_clean <- content[!is.na(content$Total), 3:4] #Syntax taught from AI
names(data_clean) <- c("Periodo", "Total")   #Only take the Periodo  and TOtal values

# Convert Period to Date and sort in chronological order 
data_clean$Fecha <- as.Date(as.yearmon(data_clean$Periodo, format = "%YM%m"))
data_clean <- data_clean[order(data_clean$Fecha), ]


data[,1]<-  as.Date(as.yearmon(content[,3] , format ="%YM%m")) 
data[,2] <- rev(data[,2])  #Puts in correct chronogical order 


ipc_vector <-  ts(data_clean$Total, start = c(2002, 1), frequency = 12)
model<- nnetar(y = ipc_vector, size = 10, linout = TRUE, maxit = 500) 

fc<- forecast(model , h =1 ) 

next_month_prediction <- fc$mean[1]

cat(sprintf("Prediction of IPC for the next month in Spain: %.1f%%\n", next_month_prediction ))


#Size is the number of hidden layers, and linout = TRUE asserts the predictionis a regression
#First I've tried to build a personal pipeline ehwere I've normalized the data by mysef, I've fit into this model. Result, a mess.The prediciton has elead to a nosense 85. With this neural network that handles automatically the normalization values and all the pipeline process, the value meets the realistic predictions



