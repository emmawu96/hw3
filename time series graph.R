library(tseries)
# time series plot monthly summary

# read data
ts1 <- read.csv("PB monthly summaries_no title.csv", header=FALSE)

# arrange the rows in ascending order with columns staring from Jan to Dec
ts2 <- ts1[ nrow(ts1):1, ]
ts2 <- cbind(subset.data.frame(ts2)[,4:12],subset.data.frame(ts2)[,1:3])
ts2
# change ts2 to class "ts" 
ts3 <- ts(as.vector(unlist(t(ts2))),frequency=12,start=c(2000,1))


#Plot the time Series Graph
ts4 <- ts.plot(ts3,main="Time Series from 2010 to 2017", gpars=list(xlab="year", ylab="Apprehensions", lty=c(1:3)))

#create data for calculating the annual means and draw the means line
ts3.1<-as.vector(ts3)
for (i in 1:18 ){
  if (i == 1){
    from <- i
    to <- 12
    annualMeans <- c(mean(ts3.1[from :to]))
  }  
  else{
    from <- 1 + to
    to <- i*12
  }  
  annualMean<- mean(ts3.1[from: to])
  print(annualMean)
  annualMeans <- append(annualMeans, annualMean)
  segments(1999+i,annualMean,2000+i,annualMean, col = rgb( 0, .7, .9, .5),lwd=2)
}
annualMeans <- annualMeans[-1]  
## add text labels (year)
text(0.5+seq(from = 2000, to = 2017, by = 1),annualMeans[1:18]+6000,labels=2000:2017, cex=0.7, col = rgb(.9,  0, .7, .5) , font=2)
## add legend
legend('topright', col=c(rgb(0,0,0),  rgb( 0, .7, .9, .5)), lty=1, lwd=2, 
       legend=c("Monthly apprehensions", "Annual averages"), bg='white')  


# monthly time series graph from 2000 to 2017 
# Box plot across months will give us a sense on seasonal effect 
boxplot(ts3~cycle(ts3))

# Month plot function creates a Jan time series, a Feb time series, etc. 
# These 12 time series (of the original data) are plotted in one graph in a coplot style. 
monthplot(ts3, ylab= "Apprehensions" )

# forecast ( not sure if is the right model,and is not required for this project)
fit <- arima(ts3, order=c(1,0,0), list(order=c(2,1,0), period=12))
fore <- predict(fit, n.ahead=24)

# forcast with error bounds at 95% confidence level

U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se
ts.plot(ts3, fore$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"), col=c(1,2,4), lty=c(1,1,2))


# Decompostion of multiplicative time series -using decompose function
ts_components_add<- decompose(ts3, type = "additive") 
ts_components_multi<-decompose(ts3, type = "multiplicative") 
plot(ts_components_add)
plot(ts_components_multi)

# Decompostion of multiplicative time series -using stl function
stl_ts3 = stl(ts3, "periodic")
seasonal_stl_ts3   <- stl_ts3$time.series[,1]
trend_stl_ts3     <- stl_ts3$time.series[,2]
random_stl_ts3  <- stl_ts3$time.series[,3]

plot(ts3)
plot(as.ts(seasonal_stl_ts3))
plot(trend_stl_ts3)
plot(random_stl_ts3)
plot(stl_ts3)

# addjust the data by removing seasonal component
apprehensions_seasonally_adjusted <- ts3 - ts_components_multi$seasonal
plot(apprehensions_seasonally_adjusted)

# trend graph
fit2 <- stl(ts3, s.window=5)
plot(ts3, col="gray", main="PB apprehension trend", 
     ylab="Apprehensions", xlab="")
lines(fit2$time.series[,2],col="red",ylab="Trend")





