require(moments)
require(data.table)
require(MASS)
require(elasticnet)

#average wo zero values 
avg_wo_zero <- function(x) mean(x[which(x!=0)],na.rm = T)

#mode (wihout NA !)
Mode <- function(x)
{
  x <- x[!is.na(x)]
  if(length(x)==0)
    return(NA)
  else
    return(as.numeric(names(sort(-table(x))[1])))
}


#lag or forward
shift<-function(x,shift_by){
  #stopifnot(is.numeric(shift_by))
  #stopifnot(is.numeric(x))
  
  if (length(shift_by)>1)
    return(sapply(shift_by,shift, x=x))
  
  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
    out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out<-x
  out
}

#Gives a summary of last n days 
summary_n_days <- function(x,fill_ip_with,no_of_days,FUN,...)
{
  b <- rep(NA,length(x))
  x <- c(rep(fill_ip_with,no_of_days),x)
  for(i in (no_of_days+1):length(x))
  {
    b[i-no_of_days] <- FUN(x[(i-no_of_days):(i-1)],...)
  }
  return(b)
}


#ridge trend function to extrapolate trend 
ridge_trend <- function(x,column_names,dependent)
{
  dep <- paste(dependent," ~")
  forma <- as.formula(paste(dep, paste(column_names,collapse = "+")))
  trend <- rep(NA,nrow(x))
  for(i in 2:nrow(x))
  {
    SD <- x[1:(i-1)]
    possibleError <- 
      tryCatch(fit_ridge <- lm(formula=forma,data=SD),error=function(e) e )
    if(inherits(possibleError, "error")) next
    
    # trend[i] <- predict(object = fit_ridge,newx=x[,column_names,with=FALSE][1]) #predict is not working !!
    y <- c(1,x[,column_names,with=FALSE][i])
    y <- sapply(y, function(x)x[1])
    trend[i] <- sum(y*coef(fit_ridge))
  }
  return(trend)
}


neural_trend <- function(x)
{
  neural_trend <- rep(NA,length(x))
  for (i in 3:length(x))
  {
    fit <- nnetar(x[1:(i-1)])
    l <- forecast(fit,h=1,lambda = 0)
    neural_trend[i] <- as.numeric(l$mean)
  }
  return(neural_trend)
}

holt_linear_trend <- function(x)
{
  holt_trend <- rep(NA,length(x))
  if(length(x) >=8)
  {
    for (i in 8:length(x))
    {
      l <- holt(log(x[1:(i-1)]), alpha=0.5, beta=0.4, damped=TRUE, initial="simple", h=1)
      holt_trend[i] <- exp(as.numeric(l$mean))
    }
  }
  
  return(log(holt_trend))
}




#Function to calculate day before an event, days during an event and days after an event
day_counter <- function(x)
{
  day_tagging <- mapply(x,shift(x,-1),shift(x,1),FUN = function(x,y,z)
    ifelse(y==0 & x==1,"s",ifelse(z==0 & x==1,"e",ifelse(z==1 & x==1 & y==1,1,0))))
  day_tagging[which(is.na(day_tagging))] <- 1
  day_before_promo <- vector()
  day_during_promo <- vector()
  day_after_promo <- vector()
  for(i in 1:length(x))
  {
    x1 <- day_tagging[i:length(day_tagging)]
    if(day_tagging[i]=="0")
      day_before_promo[i] <- min(which(x1=="s"))-1
    else
      day_before_promo[i] <- 0
    x2 <- day_tagging[1:i]
    if(day_tagging[i]!="0")
      day_during_promo[i] <- i-max(which(x2=="s"))+1
    else
      day_during_promo[i] <- 0
    if(day_tagging[i]=="0")
    {
      if(length(which(x2=="e"))==0)
        day_after_promo[i] <- NA
      else
        day_after_promo[i] <- i-max(which(x2=="e"))+1
    }
    
    else
      day_after_promo[i] <- 0
    
  }
  
  l <- list(day_before_promo,day_during_promo,day_after_promo)
  return(l)
}

seasonal_lag <- function(x,period,fill_up_with)
{
  b <- rep(NA,length(x))
  for(i in (period+1):length(x))
  {
    b[i] <- mean(x[i-period])
  }
  return(b)
}
