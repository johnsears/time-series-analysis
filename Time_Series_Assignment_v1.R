
#**********************************************************************************************************
#              TIME SERIES CASE STUDY                                       *                                                                             *
#**********************************************************************************************************                                                                                                      *

# Group Members:                                                                                         *
# Ria Nag,Priya Chopra,Piyush Gaur and  Sanajana Rao
###################################################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
#################################################################################################

# Business objective:                                                                                     *

#"Global Mart" is an online store super giant having worldwide operations.
#It takes orders and delivers across the globe and deals with all the major product categories - consumer, corporate & home office.
#A business plan is required to be finalized for next 6 months to manage revenue and demand.
#The store has 7 different market segments in 3 major product categories .Thus there are a total of 21 segments of market in different product categories. 
#The  company strategy is to find out 2 most consistently profitable segments and focus on them.

#Goal 
#Forecast the Sales and Demand for next 6 months for 2 most profitable segments to manage the revenue and inventory accordingly and also validate the model built to indicate the forecast is accurate.


#**********************************************************************************************************                                                                                                      *


#** Install all Packages if packages are not already installed **
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("graphics")
#install.packages("forecast")
#install.packages("tseries")
#install.packages("cowplot")
#Loading the required libraries

library(graphics)
library(forecast)
library(tidyr)
library(dplyr)
library(tseries)
library(ggplot2)
library(cowplot)
## define working directory using the command setwd to the directory where your input files are located

#** Clear environment variables **
rm(list=ls())
#loading the file into R
data<-read.csv("Global Superstore.csv",stringsAsFactors =F )
#check if any row is duplicated
nrow(data[duplicated(data),])#no rows duplicated
#check for NA
colSums(is.na(data))
#only postal code column has NA values and it is not required for analysis
#check for blanks
which(sapply(data, function(x) length(which(x == ""))>0)==T)
#no blanks
#######Preparing the order date column to extract the month of the order#############
data$Order.Date <- as.POSIXlt(data$Order.Date, format = "%d-%m-%Y")
data$month_year <- format(data$Order.Date, "%m-%Y")

############################################################################
#aggregating profits,sales and quantity based on every month in each market segment
data_t<-data.frame(cbind(data$Segment,data$Market,data$Profit,data$month_year,data$Sales,data$Quantity))
data_final<- within(data_t,  category_market <- paste(X1,X2, sep ="-"))
data_final<- within(data_final,  category_market_month <- paste(category_market,X4, sep ="_"))
colnames(data_final)[3]<-"profit"
colnames(data_final)[5]<-"sales"
colnames(data_final)[6]<-"quantity"
colnames(data_final)[4]<-"month"
data_final$profit<-as.character(data_final$profit)
data_final$profit<-as.numeric(data_final$profit)
##################################################################################
#finding total profit per month per segment
total_profit_per_month<-aggregate(profit~category_market_month,data_final,sum)
#finding COV of profits for each segment
###################################################################################
total_profit_per_month<-separate(total_profit_per_month,category_market_month,into=c("category_market","month"),sep="_")
#finding average profit for each segment
average_profit<-aggregate(profit~category_market,total_profit_per_month,mean)
#highest average profit is for  consumer in APAC and consumer IN EU
total_profit<-aggregate(profit~category_market,total_profit_per_month,sum)
#highest total profit is for  consumer in APAC and consumer IN EU
sd<-aggregate(profit~category_market,total_profit_per_month,sd)
cv<-merge(sd,average_profit,by="category_market")
colnames(cv)[2]<-"sd"
colnames(cv)[3]<-"mean"
cv$cov<-cv$sd*100/cv$mean
#LOWEST COV  for profit IS FOR consumer IN EU and consumer in APAC
#####################################################################################
#plots for average profit,total profit and coefficient of variance of total monthly profits
#for top 5 segments
top_total_profit<-total_profit[order(total_profit$profit,decreasing = T),]
top_total_profit<-top_total_profit[1:5,]
top_avg_profit<-average_profit[order(average_profit$profit,decreasing = T),]
top_avg_profit<-top_avg_profit[1:5,]
least_cov_profit<-cv[order(cv$cov),]
least_cov_profit<-least_cov_profit[1:5,]
#making the plots
top_total_profit<-ggplot(top_total_profit,aes(x=category_market,y=profit))+geom_col(fill="blue")+labs(y="total_profit")+ theme(axis.text.x=element_text(angle=90, hjust=1))
top_avg_profit<-ggplot(top_avg_profit,aes(x=category_market,y=profit))+geom_col(fill="green")+labs(y="avg_profit")+ theme(axis.text.x=element_text(angle=90, hjust=1))
least_cov_profit<-ggplot(least_cov_profit,aes(x=category_market,y=cov))+geom_col(fill="red")+labs(y="cov")+ theme(axis.text.x=element_text(angle=90, hjust=1))
plot_grid(least_cov_profit,top_avg_profit,top_total_profit)#plot1
#####################################################################################
#filtering the entire dataset for consumer IN EU
data_EU<-subset(data,Segment=="Consumer"& Market=="EU" )
#filtering the entire dataset for CONSUMER IN APAC
data_APAC<-subset(data,Segment=="Consumer" & Market=="APAC")
########################################################################################
#Cleaning the dataset to keep only the columns required for time series
data_EU<-data_EU[,c(19,20,25)]
data_APAC<-data_APAC[,c(19,20,25)]
####################################################################################
#finding total sales and quantity per month
data_EU<-data_EU%>%group_by(month_year)%>%summarise_all(sum)
data_EU$month_year<-paste0("01-",data_EU$month_year)
data_EU$month_year<- as.POSIXlt(data_EU$month_year, format = "%d-%m-%Y")

data_APAC<-data_APAC%>%group_by(month_year)%>%summarise_all(sum)
data_APAC$month_year<-paste0("01-",data_APAC$month_year)
data_APAC$month_year<- as.POSIXlt(data_APAC$month_year, format = "%d-%m-%Y")
#########################################################################################
#sorting the data as per chronological order
data_EU_sorted<-data_EU[order(data_EU$month_year,decreasing = FALSE),]
data_APAC_sorted<-data_APAC[order(data_EU$month_year,decreasing = FALSE),]
for (i in 1:48)
{
data_EU_sorted$timestamps[i]<-i
}
data_APAC_sorted$timestamps<-data_EU_sorted$timestamps

####################################################################################


#dividing data into test and train
train_APAC_sales<-data_APAC_sorted[1:42,]
test_APAC_sales<-data_APAC_sorted[43:48,]
train_EU_sales<-data_EU_sorted[1:42,]
test_EU_sales<-data_EU_sorted[43:48,]
###############################################################################################

#classical decomposition model  for APAC_sales
timeser <- ts(train_APAC_sales$Sales)
plot(timeser)

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(timeser, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- train_APAC_sales$timestamps
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(0.1*Month) * poly(Month,1) + cos(0.1*Month) * poly(Month,1)
            + Month, data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines( global_pred, col='green', lwd=2)
legend(x="bottomright",legend = c("predicted timeseries","actual timeseries","smoothed timeseries"),col=c("green","black","blue"),lwd=1,cex=0.6)
#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit
#since ARMA model for locally predictable series is ARIMA(0,0,0) so no locally predictable part is present
#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)
#both kpss and adf test indicate that the timeseries is stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
timevals_out <- test_APAC_sales$timestamps

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- as.numeric(global_pred_out)
test_APAC_sales$Sales<-as.numeric(test_APAC_sales$Sales)

#Now, let's compare our prediction with the actual values, using MAPE

MAPE <- forecast::accuracy(fcast,test_APAC_sales$Sales)[5]
MAPE
#21.92

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser <- ts(data_APAC_sorted$Sales)
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black",ylab="actual sales in units of currency ",xlab="timestamps for each month")
lines(class_dec_pred, col = "red")
legend(x="topleft",legend = c("predicted sales","actual sales"),col=c("red","black"),lwd=1)
#######################################################################################################################
# ARIMA fit on consumer PRODUCTS in  APAC Sales

# ARIMA - APAC Consumer Sales #
timeser <- ts(train_APAC_sales$Sales)
plot(timeser)
autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)
#kpss and adf tests show that the residual series is white noise.
#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

fcast <- as.numeric(fcast_auto_arima$pred)
test_APAC_sales$Sales<-as.numeric(test_APAC_sales$Sales)


#Now, let's compare our prediction with the actual values, using MAPE

MAPE <- forecast::accuracy(fcast,test_APAC_sales$Sales)[5]
MAPE
#27.69

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
total_timeser <- ts(data_APAC_sorted$Sales)
plot(total_timeser, col = "black",ylab="actual sales in units of currency ",xlab="timestamps for each month")
lines(auto_arima_pred, col = "red")
legend(x="topleft",legend = c("predicted sales","actual sales"),col=c("red","black"),lwd=1)
#FORECASTING SALES for future 6 months post 48 months with model obtained from classical decomposition for consumer segment in APAC
#as MAPE VALUES for classical decomposition method were better than MAPE values of  AUTO ARIMA MODEL
future_timestamps<-c(49,50,51,52,53,54)
APAC_sales_fcast <- predict(lmfit,data.frame(Month =future_timestamps))
APAC_sales_fcast
#future Months:  1         2         3         4         5         6 
#sales :     62768.48  69143.97  76736.95  85639.15  95933.62 107693.05

#####################################################################################
######################################################################################

#classical decomposition model  for APAC_quantity
timeser <- ts(train_APAC_sales$Quantity)
plot(timeser)

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(timeser, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- train_APAC_sales$timestamps
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Quantity ~ sin(0.1*Month) * poly(Month,1) + cos(0.1*Month) * poly(Month,1)
            + Month, data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines( global_pred, col='green', lwd=2)
legend(x="bottomright",legend = c("predicted timeseries","actual timeseries","smoothed timeseries"),col=c("green","black","blue"),lwd=1,cex=0.6)
#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit
#since ARMA model for locally predictable series is ARIMA(0,0,0) so no differencing was done.

resi <- local_pred

adf.test(resi,alternative = "stationary")
kpss.test(resi)
#both kpss and adf tests show the timeseries is stationary
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
timevals_out <- test_APAC_sales$timestamps

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- as.numeric(global_pred_out)
test_APAC_sales$Quantity<-as.numeric(test_APAC_sales$Quantity)

#Now, let's compare our prediction with the actual values, using MAPE

MAPE <- forecast::accuracy(fcast,test_APAC_sales$Quantity)[5]
MAPE
#19.67

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser <- ts(data_APAC_sorted$Quantity)
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black",ylab="quantity",xlab="timestamps for each month")
lines(class_dec_pred, col = "red")
legend(x="topleft",legend = c("predicted quantity","actual quantity"),col=c("red","black"),lwd=1)

#############################################################################################################
# ARIMA - APAC Consumer Quantity #
timeser <- ts(train_APAC_sales$Quantity)
plot(timeser)
autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)
#both adf and kpss tests show the residual series is stationary
#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

fcast <- as.numeric(fcast_auto_arima$pred)
test_APAC_sales$Quantity<-as.numeric(test_APAC_sales$Quantity)



#Now, let's compare our prediction with the actual values, using MAPE

MAPE <- forecast::accuracy(fcast,test_APAC_sales$Quantity)[5]
MAPE
#26.24


#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
total_timeser <- ts(data_APAC_sorted$Quantity)
plot(total_timeser, col = "black",ylab="quantity",xlab="timestamps for each month")
lines(auto_arima_pred, col = "red")
legend(x="topleft",legend = c("predicted quantity","actual quantity"),col=c("red","black"),lwd=1)
#FORECASTING QUANTITY requirement or demand for future 6 months post 48 months with model obtained from classical decomposition for consumer segment in APAC
#as MAPE VALUES for classical decomposition method were better than MAPE values of  AUTO ARIMA MODEL
future_timestamps<-c(49,50,51,52,53,54)
APAC_quantity_fcast <- predict(lmfit,data.frame(Month =future_timestamps))
APAC_quantity_fcast
#future Months:  1         2         3       4        5         6 
#quantity :       1066.104 1198.792 1350.744 1522.938 1716.187 1931.121 

#################################################################################################################3
#####################################################################################################
######################################################################################3

#classical decomposition model  for EU_quantity

timeser_EU_Quantity <- ts(train_EU_sales$Quantity)

plot(timeser_EU_Quantity)

#Smoothing the series - Moving Average Smoothing

w3 <-1
smoothedseries_EU_Quantity <- stats::filter(timeser_EU_Quantity, 
                                            filter=rep(1/(2*w3+1),(2*w3+1)), 
                                            method='convolution', sides=2)

#Smoothing left end of the time series

diff_EU_Quantity <- smoothedseries_EU_Quantity[w3+2] - smoothedseries_EU_Quantity[w3+1]
for (i in seq(w3,1,-1)) {
  smoothedseries_EU_Quantity[i] <- smoothedseries_EU_Quantity[i+1] - diff_EU_Quantity
}

#Smoothing right end of the time series

n_EU_Quantity <- length(timeser_EU_Quantity)
diff_EU_Quantity <- smoothedseries_EU_Quantity[n_EU_Quantity-w3] - smoothedseries_EU_Quantity[n_EU_Quantity-w3-1]
for (i in seq(n_EU_Quantity-w3+1, n_EU_Quantity)) {
  smoothedseries_EU_Quantity[i] <- smoothedseries_EU_Quantity[i-1] + diff_EU_Quantity
}

#Plot the smoothed time series

timevals_in_EU_Quantity <- train_EU_sales$timestamps
lines(smoothedseries_EU_Quantity, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_EU_Quantity <- as.data.frame(cbind(timevals_in_EU_Quantity, as.vector(smoothedseries_EU_Quantity)))
colnames(smootheddf_EU_Quantity) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_EU_Quantity <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                        + Month, data=smootheddf_EU_Quantity)

global_pred_EU_Quantity <- predict(lmfit_EU_Quantity, Month=timevals_in_EU_Quantity)
summary(global_pred_EU_Quantity)
lines( global_pred_EU_Quantity, col='green', lwd=2)
legend(x="bottomright",legend = c("predicted timeseries","actual timeseries","smoothed timeseries"),col=c("green","black","blue"),lwd=1,cex=0.6)
#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_EU_Quantity <- timeser_EU_Quantity-global_pred_EU_Quantity
plot(local_pred_EU_Quantity, col='red', type = "l")
acf(local_pred_EU_Quantity)
acf(local_pred_EU_Quantity, type="partial")
armafit_EU_Quantity <- auto.arima(local_pred_EU_Quantity)

tsdiag(armafit_EU_Quantity)
armafit_EU_Quantity
#since ARMA model for locally predictable series is ARIMA(2,0,0) so no differencing was done.
#We'll check if the residual series is white noise

resi_EQ_Quantity <- local_pred_EU_Quantity-fitted(armafit_EU_Quantity)

adf.test(resi_EQ_Quantity,alternative = "stationary")


kpss.test(resi_EQ_Quantity)

#both adf and kpss tests show that the residual time series is stationary


#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out_EU_Quantity <- test_EU_sales$timestamps

global_pred_out_EU_Quantity <- predict(lmfit_EU_Quantity,data.frame(Month =timevals_out_EU_Quantity))

fcast_EU_Quantity <- as.numeric(global_pred_out_EU_Quantity)

test_EU_sales$Quantity<-as.numeric(test_EU_sales$Quantity)

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_EU_Quantity <- forecast::accuracy(fcast_EU_Quantity,test_EU_sales$Quantity)[5]

MAPE_EU_Quantity # 30.39741

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser_EU_Quantity <- ts(data_EU_sorted$Quantity)
class_dec_pred_EU_Quantity <- c(ts(global_pred_EU_Quantity),ts(global_pred_out_EU_Quantity))
plot(total_timeser_EU_Quantity, col = "black",ylab="actual quantity ",xlab="timestamps for each month")
lines(class_dec_pred_EU_Quantity, col = "red")
legend(x="topleft",legend = c("predicted quantity","actual quantity"),col=c("red","black"),lwd=1)
########################################################################################
# ARIMA fit on consumer PRODUCTS in  EU Qantity
timeser <- ts(train_EU_sales$Quantity)
plot(timeser)
autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)
#kpss and adf for this timeseries says its stationary
#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

fcast <- as.numeric(fcast_auto_arima$pred)
test_EU_sales$Quantity<-as.numeric(test_EU_sales$Quantity)


#Now, let's compare our prediction with the actual values, using MAPE

MAPE <- forecast::accuracy(fcast,test_EU_sales$Quantity)[5]
MAPE
#30.133

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser <- ts(data_EU_sorted$Quantity)
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black",ylab="quantity ",xlab="timestamps for each month")
lines(auto_arima_pred, col = "red")
legend(x="topleft",legend = c("predicted quantity","actual quantity"),col=c("red","black"),lwd=1)
#FORECASTING quantity requirement for future 6 months post 48 months with model obtained from AUTo ARIMA for consumer segment in EU
#as MAPE VALUE for AUTO ARIMA MODEL method wAS slightly better than MAPE values of  classical decomposition model

EU_quantity_fcast <- predict(autoarima,n.ahead=12)
EU_quantity_fcast<-EU_quantity_fcast$pred[7:12]
EU_quantity_fcast
#future Months:      1      2         3         4         5         6 
#quantity :      466.2458 463.7401 472.9520 467.6464 466.1350 470.3663 
#####################################################################################################
#######################################################################################################

#classical decomposition model  for EU_sales in consumer segment

timeser_EU_Sales <- ts(train_EU_sales$Sales)

plot(timeser_EU_Sales)

#Smoothing the series - Moving Average Smoothing

w3 <-1
smoothedseries_EU_Sales <- stats::filter(timeser_EU_Sales, 
                                            filter=rep(1/(2*w3+1),(2*w3+1)), 
                                            method='convolution', sides=2)

#Smoothing left end of the time series

diff_EU_Sales<- smoothedseries_EU_Sales[w3+2] - smoothedseries_EU_Sales[w3+1]
for (i in seq(w3,1,-1)) {
  smoothedseries_EU_Sales[i] <- smoothedseries_EU_Sales[i+1] - diff_EU_Sales
}

#Smoothing right end of the time series

n_EU_Sales <- length(timeser_EU_Sales)
diff_EU_Sales <- smoothedseries_EU_Sales[n_EU_Sales-w3] - smoothedseries_EU_Sales[n_EU_Sales-w3-1]
for (i in seq(n_EU_Sales-w3+1, n_EU_Sales)) {
  smoothedseries_EU_Sales[i] <- smoothedseries_EU_Sales[i-1] + diff_EU_Sales
}

#Plot the smoothed time series

timevals_in_EU_Sales <- train_EU_sales$timestamps
lines(smoothedseries_EU_Sales, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_EU_Sales <- as.data.frame(cbind(timevals_in_EU_Sales, as.vector(smoothedseries_EU_Sales)))
colnames(smootheddf_EU_Sales) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(0.1*Month) * poly(Month,1) + cos(0.1*Month) * poly(Month,1)
                        + Month, data=smootheddf_EU_Sales)

global_pred_EU_Sales <- predict(lmfit, Month=(timevals_in_EU_Sales))
summary(global_pred_EU_Sales)
lines( global_pred_EU_Sales, col='green', lwd=2)
legend(x="bottomright",legend = c("predicted timeseries","actual timeseries","smoothed timeseries"),col=c("green","black","blue"),lwd=1,cex=0.6)
#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_EU_Sales <- timeser_EU_Sales-global_pred_EU_Sales
plot(local_pred_EU_Sales, col='red', type = "l")
acf(local_pred_EU_Sales)
acf(local_pred_EU_Sales, type="partial")
armafit_EU_Sales <- auto.arima(local_pred_EU_Sales)

tsdiag(armafit_EU_Sales)
armafit_EU_Sales
#since ARMA model for locally predictable series is ARIMA(0,0,0) so no differencing was done.

#We'll check if the residual series is white noise

resi_EU_Sales <- local_pred_EU_Sales-fitted(armafit_EU_Sales)

adf.test(resi_EU_Sales,alternative = "stationary")

kpss.test(resi_EU_Sales)
#both kpss and dicky fuller tests show that the residual time series is stationary
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out_EU_Sales <- test_EU_sales$timestamps

global_pred_out_EU_Sales <- predict(lmfit,data.frame(Month =timevals_out_EU_Sales))

fcast_EU_Sales <- as.numeric(global_pred_out_EU_Sales)

test_EU_sales$Sales<-as.numeric(test_EU_sales$Sales)

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_EU_Sales<- forecast::accuracy(fcast_EU_Sales,test_EU_sales$Sales)[5]

MAPE_EU_Sales #21.72

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser_EU_Sales <- ts(data_EU_sorted$Sales)
class_dec_pred_EU_Sales <- c(ts(global_pred_EU_Sales),ts(global_pred_out_EU_Sales))
plot(total_timeser_EU_Sales, col = "black",ylab="actual sales in units of currency ",xlab="timestamps for each month")
lines(class_dec_pred_EU_Sales, col = "red")
legend(x="topleft",legend = c("predicted sales","actual sales"),col=c("red","black"),lwd=1)

########################################################################################
# ARIMA fit on consumer PRODUCTS in  EU Sales
timeser <- ts(train_EU_sales$Sales)
plot(timeser)
autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)
#both adf and kpss test show the timeseries is stationary
#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

fcast <- as.numeric(fcast_auto_arima$pred)
test_EU_sales$Sales<-as.numeric(test_EU_sales$Sales)


#Now, let's compare our prediction with the actual values, using MAPE

MAPE <- forecast::accuracy(fcast,test_EU_sales$Sales)[5]
MAPE
#28.92

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser <- ts(data_EU_sorted$Sales)
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")
plot(total_timeser, col = "black",ylab="actual sales in units of currency ",xlab="timestamps for each month")
lines(auto_arima_pred, col = "red")
legend(x="topleft",legend = c("predicted sales","actual sales"),col=c("red","black"),lwd=1)
#FORECASTING SALES for future 6 months post 48 months with model obtained from classical decomposition for consumer segment in EU
#as MAPE VALUES for classical decomposition method were better than MAPE values of  AUTO ARIMA MODEL
future_timestamps<-c(49,50,51,52,53,54)
EU_sales_fcast <- predict(lmfit,data.frame(Month =future_timestamps))
EU_sales_fcast
#future Months:  1         2         3         4         5         6 
#sales :       72231.06  80138.12  89030.24  98947.03 109920.11 121972.14  
#########################################################################################
