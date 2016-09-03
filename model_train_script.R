model_train <- function(sample)
{
  
  ####### Feature engineering ################
  require(ranger)
  require(data.table)
  require(caret)
  require(lubridate)
  source("feature_engineering.R")
  
  
  sample <- sample[order(Date)]
  sample[,day_of_year:=as.numeric(day_of_year)]
  sample[,log_sale:=ifelse(Sales==0,0,log(Sales))]
  day_counter_cal_promo <- day_counter(sample$Promo)
  sample[,day_before_promo:=day_counter_cal_promo[[1]]]
  sample[,day_during_promo:=day_counter_cal_promo[[2]]]
  sample[,day_after_promo:=day_counter_cal_promo[[3]]]
  
  
  
  sample[,lag_1_holiday:=shift(x = StateHoliday,shift_by = -1)]
  sample[,forward_1_holiday:=shift(x = StateHoliday,shift_by = 1)]
  sample$forward_1_holiday[which(is.na(sample$forward_1_holiday))] <- "0"
  
  
  #temporal features
  sample[,sale_1_day:=summary_n_days(x =log_sale,fill_ip_with = NA,no_of_days = 1,
                                     FUN = mean,na.rm=T)]
  sample[,seasonal_lag_7:=seasonal_lag(log_sale,period = 7,fill_up_with = NA)]
  sample[,seasonal_lag_14:=seasonal_lag(log_sale,period = 14,fill_up_with = NA)]
  sample[,open_3_days:=summary_n_days(x=Open,fill_ip_with = NA,no_of_days = 3,FUN = sum)]
  
  #day_number
  sample[,day_number:=1:nrow(sample)]
  
  #Creating factor variable
  sample[,StateHoliday:=as.factor(StateHoliday)]
  sample[,lag_1_holiday:=as.factor(lag_1_holiday)]
  sample[,forward_1_holiday:=as.factor(forward_1_holiday)]
  
  #current trends
  sample[,trend_pred:=ridge_trend(x = sample,column_names = c("day_number","DayOfWeek","Promo"),dependent = "log_sale")]
  
  ########## FEATURE engineering ends here #####################
  
  
  
  
  
  
  #train data test data division
  sample_modified <- sample
  sample_modified$Store <- NULL
  sample_modified$Customers <- NULL
  sample_modified$Sales <- NULL
  #sample_modified$conversion <- NULL
  #sample_modified$year_month <- NULL
  #sample_modified$day_number <- NULL
  #sample_modified$Date <- NULL
  
  
  final_train_data <- sample_modified[Date < as.Date("2016-07-01")]
  final_test_data <- sample_modified[Date >= as.Date("2016-07-01")]
  final_train_data$Date <- NULL
  final_train_data <- final_train_data[!is.na(seasonal_lag_14)]
  
  
  model_training_data <- final_train_data
  #nearzero variable
  list_of_zero_variance <- nearZeroVar(model_training_data,saveMetrics = T)
  zero_var_column <- names(model_training_data)[list_of_zero_variance$zeroVar]
  if(length(zero_var_column)!=0)
    model_training_data <- model_training_data[,!zero_var_column,with=F]
  
  dummies <- dummyVars(log_sale ~. , data = model_training_data)
  model_training_data <- predict(object=dummies,model_training_data)
  model_training_data <- as.data.frame(model_training_data)
  model_training_data[] <- lapply(model_training_data, as.numeric)
  
  
  
  
  
  #############This is for validating ###################
  final_train_data_val <- sample_modified[Date < as.Date("2016-06-01")]
  final_test_data_val <- sample_modified[Date >= as.Date("2016-06-01") & Date < as.Date("2016-07-01")]
  final_train_data_val$Date <- NULL
  final_train_data_val <- final_train_data_val[!is.na(seasonal_lag_14)]
  
  
  model_training_data_val <- final_train_data_val
  #nearzero variable
  list_of_zero_variance <- nearZeroVar(model_training_data_val,saveMetrics = T)
  zero_var_column <- names(model_training_data_val)[list_of_zero_variance$zeroVar]
  if(length(zero_var_column)!=0)
    model_training_data_val <- model_training_data_val[,!zero_var_column,with=F]
  
  dummies_val <- dummyVars(log_sale ~. , data = model_training_data_val)
  model_training_data_val <- predict(object=dummies_val,model_training_data_val)
  model_training_data_val <- as.data.frame(model_training_data_val)
  model_training_data_val[] <- lapply(model_training_data_val, as.numeric)
  
  
  
  
  
  ########### MODEL TRAINING BEGINS HERE ##############################
  
  # tuneGrid <- expand.grid(list(nrounds=c(nrow(model_training_data)/10,nrow(model_training_data)/5,nrow(model_training_data)/3),
  #                              eta=c(0.1,.01,.001,.0001,.00001),alpha=c(1,2,3,4),lambda=c(1,2,3,4)))
  # 
  # tuneGrid <- expand.grid(list(nrounds=c(nrow(model_training_data)/10,nrow(model_training_data)/5,nrow(model_training_data)/3),
  #                              alpha=c(1,2,3,4),lambda=c(1,2,3,4)))
  # 
  # t0 <- proc.time()[3]
  # # xgb_train <- train(x = model_training_data, y= original_train_data$log_sale,
  # #                    tuneGrid=tuneGrid,metric="RMSE",
  # #                    method="xgbLinear",trControl=trainControl(method="cv",allowParallel = T,verboseIter=T,number = 1))
  # xgb_train <- train(x=model_training_data,y=final_train_data$log_sale,method="xgbLinear",
  #                    tuneGrid=tuneGrid,preProc=c("center","scale"),
  #                    trControl=trainControl(method="cv",allowParallel = F,verboseIter=T,number = 2))
  # 
  # t0 <- proc.time()[3]
  # tuneGrid = expand.grid(.committees = c(1,5,10,50,70,100,150),.neighbors = c(0,1,3,5,7,9))
  # xgb_train <- train(x=model_training_data,y=final_train_data[Open==1]$log_sale,
  #                    method="cubist",tuneGrid=tuneGrid,preProc=c("center","scale"),
  #                    trControl=trainControl(method="cv",allowParallel = T,verboseIter=T,number = 2))
  # t1 <- proc.time()[3] - t0
  
  #Only random forest has been trained 
  
  
  t0 <- proc.time()[3]
  xgb_train <- train(x=model_training_data,y=final_train_data$log_sale,
                     method = "ranger",
                     tuneLength = 20,
                     num.trees = 2000,
                     #importance = TRUE,
                     trControl = trainControl(method="cv",verboseIter=T,number = 2,allowParallel = F))
  
  t1 <- proc.time()[3] - t0
  
  
  xgb_train_val <- train(x=model_training_data_val,y=final_train_data_val$log_sale,
                         method = "ranger",
                         tuneLength = 20,
                         num.trees = 2000,
                         #importance = TRUE,
                         trControl = trainControl(method="cv",verboseIter=T,number = 2,allowParallel = F))
  
  t1 <- proc.time()[3] - t0
  
  # t0 <- proc.time()[3]
  # nnetGrid <- expand.grid(.decay = c(0, 0.01,.001,.1,.00001),.size = c(2:10),.bag = FALSE)
  # xgb_train <- train(x=model_training_data,y=final_train_data$log_sale,method="avNNet",
  #                    preProc = c("center", "scale"),tuneGrid=nnetGrid,
  #                    linout = TRUE,trace = FALSE,MaxNWts = 10 * (ncol(model_training_data) + 1) + 10 + 1,maxit = 1000,
  #                    trControl = trainControl(method = "cv",allowParallel = T,verboseIter = T,number = 2))
  # t1 <- proc.time()[3] - t0
  # 
  # t0 <- proc.time()[3]
  # marsGrid <- expand.grid(.degree = 1:2, .nprune = 10:30)
  # xgb_train <- train(x=model_training_data,y=final_train_data[Open==1]$log_sale,
  #                    method="earth",tuneGrid=marsGrid,
  #                    trControl = trainControl(method = "cv",allowParallel = T,verboseIter = T,number = 2))
  # t1 <- proc.time()[3] - t0
  # 
  # 
  # 
  # #xgb_train <- train(x=model_training_data,y=original_train_data$log_sale,method="xgbLinear",tuneGrid=tuneGrid)
  # t1 <- proc.time()[3] - t0
  
  
  
  
  
  
  
  
  ###PREDICTION STARTS HERE 
  #test_data prediction
  trend_lm <- lm(formula = log_sale ~ day_number+DayOfWeek+Promo,data = final_train_data)
  final_test_data$trend_pred <- predict(trend_lm,newdata=final_test_data)
  
  
  last_14_days_sale <- tail(final_train_data$log_sale,n = 14)
  final_test_data$forecast <- rep(NA,nrow(final_test_data))
  #final_test_data$forward_1_holiday[which(is.na(final_test_data$forward_1_holiday))] <- 1
  model_test_data <- predict(dummies,final_test_data)
  model_test_data <- as.data.frame(model_test_data)
  for (i in 1:nrow(final_test_data))
  {
    one_row <- model_test_data[i,]
    if(one_row$Open==0)
      final_test_data$forecast[i]=0
    else
    {
      # one_row <- predict(dummies,one_row)
      # one_row <- as.data.frame(one_row)
      one_row$seasonal_lag_7 <- last_14_days_sale[8]
      one_row$seasonal_lag_14 <- last_14_days_sale[1]
      one_row$sale_1_day <- last_14_days_sale[14]
      predicted_log_sale <- predict(xgb_train,one_row)
      last_14_days_sale <- c(last_14_days_sale[-1],predicted_log_sale)
      final_test_data$forecast[i] <- exp(predicted_log_sale)
    }
    
    
  }
  
  final_test_data <- as.data.table(final_test_data)
  final_test_data <- final_test_data[,.(Date,forecast)]
  final_test_data[,Store:=sample$Store[1]]
  
  
  
  
  
  
  
  #validation data prediction 
  trend_lm <- lm(formula = log_sale ~ day_number+DayOfWeek+Promo,data = final_train_data_val)
  final_test_data_val$trend_pred <- predict(trend_lm,newdata=final_test_data_val)
  
  
  last_14_days_sale <- tail(final_train_data_val$log_sale,n = 14)
  final_test_data_val$forecast <- rep(NA,nrow(final_test_data_val))
  model_test_data_val <- predict(dummies_val,final_test_data_val)
  model_test_data_val <- as.data.frame(model_test_data_val)
  for (i in 1:nrow(final_test_data_val))
  {
    one_row <- model_test_data_val[i,]
    if(one_row$Open==0)
      final_test_data_val$forecast[i]=0
    else
    {
      # one_row <- predict(dummies,one_row)
      # one_row <- as.data.frame(one_row)
      one_row$seasonal_lag_7 <- last_14_days_sale[8]
      one_row$seasonal_lag_14 <- last_14_days_sale[1]
      one_row$sale_1_day <- last_14_days_sale[14]
      predicted_log_sale <- predict(xgb_train,one_row)
      last_14_days_sale <- c(last_14_days_sale[-1],predicted_log_sale)
      final_test_data_val$forecast[i] <- exp(predicted_log_sale)
    }
    
    
  }
  
  final_test_data_val$actual <- exp(final_test_data_val$log_sale)
  wmape <- sum(abs(final_test_data_val$actual-final_test_data_val$forecast))/sum(final_test_data_val$actual)
  #p <- plot_ly(final_test_data_val,x=Date,y=actual,name="actual")
  #p <- add_trace(p,y=final_test_data_val$forecast,name="forecast")
  
  final_train_data_val <- as.data.table(final_test_data_val)
  final_test_data_val[,Store:=sample$Store[1]]
  final_test_data_val <- final_test_data_val[,.(Store,Date,DayOfWeek,Promo,actual,forecast)]
  
  store_result_list <- list(final_test_data,final_test_data_val,wmape)
  
  names(store_result_list) <- c("submission","validation_result","wmape")
  save(store_result_list,file = paste("Store_prediction/","store_",sample$Store[1],sep = ""))

}