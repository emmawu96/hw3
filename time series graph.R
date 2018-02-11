# time series plot monthly summary

# read data
ts1 <- read.csv("PB monthly summaries_no title.csv")

# arrange the rows in ascending order with columns staring from Jan to Dec
ts2 <- ts1[ nrow(ts1):1, ]
ts2 <- cbind(subset.data.frame(ts2)[,4:12],subset.data.frame(ts2)[,1:3])

# change ts2 to class "ts"
ts3 <- ts(as.vector(unlist(t(ts2))),frequency=12,start=c(2000,1))

# monthly time series graph from 2000 t0 2017 
ts4 <-ts.plot(ts3, gpars=list(xlab="year", ylab="Apprehensions", lty=c(1:3)))

# Box plot across months will give us a sense on seasonal effect 
boxplot(ts3~cycle(ts3))

#  forecast
fit <- arima(ts3, order=c(1,0,0), list(order=c(2,1,0), period=12))
fore <- predict(fit, n.ahead=24)

#  error bounds at 95% confidence level

U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se
ts.plot(ts3, fore$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"), col=c(1,2,4), lty=c(1,1,2))



