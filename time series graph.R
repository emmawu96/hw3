# time series plot monthly summary

# read data
ts1 <- read.table(pipe("pbpaste"), sep="\t", header=F)

# arrange the rows in ascending order with columns staring from Jan to Dec
ts2 <- ts1[ nrow(ts1):1, ]
ts2 <- cbind(subset.data.frame(ts2)[,4:12],subset.data.frame(ts2)[,1:3])

# change ts2 to class "ts"
ts3 <- ts(as.vector(unlist(t(ts2))),frequency=12,start=c(2000,1))

# draw the graph
ts4 <-ts.plot(ts3, gpars=list(xlab="year", ylab="Apprehensions", lty=c(1:3)))

