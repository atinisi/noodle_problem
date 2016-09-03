require(data.table)
require(plotly)
require(lubridate)
require(forecast)
require(caret)
require(doSNOW)

source("feature_engineering.R")
train_data <- fread("train.csv")
train_data[,Date:=as.Date(Date,format="%d-%m-%y")]



##### DESCRIPTIVE STAT 
train_data[,year_month:=paste(year(Date),month,sep = "")]
train_data[,conversion:=ifelse(Sales==0,0,Sales/Customers)]
#train_data[,StateHoliday:=as.factor(StateHoliday)]

summary_data <- train_data[,.(count_days=nrow(.SD),
                              avg_sale=avg_wo_zero(Sales),
                              avg_customer=avg_wo_zero(Customers),
                              SD_sale=sd(Sales),sum_promo=sum(Promo)),by="Store"]
sample <- train_data[Store==1]
plot_ly(sample[Sales!=0],x=Date,y=log(Sales))

fit_arima <- auto.arima(log(sample$Sales+1), stepwise=FALSE, approximation=FALSE)
fit_arima1 <- arima(sample$Sales,order=c(1,1,2),seasonal = list(order=c(1,1,1),period=14))
##########################




test_data_raw <- fread("test.csv")
test_data_raw[,Date:=as.Date(Date,format="%d-%m-%y")]

test_data <- test_data_raw
test_data$Id <- NULL
test_data$Customers <- rep(0,nrow(test_data))
test_data$Sales <- rep(0,nrow(test_data)) 


#Append test data with train data 
train_data <- rbind(train_data,test_data)

#Day of the year
train_data[,day_of_year:=strftime(Date, format = "%j")]
#month of the year
train_data[,month:=month(Date)]
#week of year 
train_data[,week_of_year:=isoweek(Date)]
#Day of the month
train_data[,day_of_month:=day(Date)]


store_data <- lapply(unique(train_data$Store),function(x)train_data[Store==x])
dir.create("Store_prediction")
cl <- makeCluster(15,type = "SOCK")
registerDoSNOW(cl)

foreach(dt=store_data,.errorhandling = "stop",.verbose = T) %dopar% model_train(dt)
                                                                                               






sample <- train_data[Store==1]




oput <- model_train(train_data[Store==2])

