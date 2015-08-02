require(quantmod)
source("get.ticker.R")
source("k_nearest_neighbours.R")

NIFTY <- get.ticker("NIFTY-I")
dates<-as.Date(index(NIFTY))
dates <- unique(dates)
n.dates <- length(dates)
n.start <- 80
signals <- rep(0,n.dates-n.start+1)
returns <- rep(0,n.dates-n.start+1)
maxdd <- rep(0,n.dates-n.start+1)
maxup <- rep(0,n.dates-n.start+1)
run.dates <- rep(0,n.dates-n.start+1)

for (i in n.start:n.dates){
  sig <- k.nearest.neighbor(NIFTY,as.character(dates[i]),100,20)
  run.dates[i-n.start+1] <- dates[i]
  signals[i-n.start+1] <- sig$signal
  returns[i-n.start+1] <- sig$performance$return
  maxdd[i-n.start+1] <- sig$performance$drawdown
  maxup[i-n.start+1] <- sig$performance$maxup
}

results <- data.frame(signals,returns,maxdd,maxup)
colnames(results) <- c("signal","returns","maxdd","maxup")
results <- as.xts(results,order.by=as.Date(run.dates))

sig <- k.nearest.neighbor(NIFTY,"2015-07-21",100,20)
yy <- sig$data
new.series <- Typ(NIFTY['2015-07-21'])
new.series <- new.series - as.numeric(new.series[1,1])
new.series.1 <- new.series[1:100,]
new.series.2 <- new.series[100:375,]

chart_Series(yy[,1])
add_TA(yy[,2],on=1,col="orange")
add_TA(yy[,3],on=1,col="orange")
add_TA(yy[,4],on=1,col="orange")
add_TA(yy[,5],on=1,col="orange")
add_TA(yy[,6],on=1,col="orange")
add_TA(yy[,7],on=1,col="orange")
add_TA(yy[,8],on=1,col="orange")
add_TA(yy[,9],on=1,col="orange")
add_TA(yy[,10],on=1,col="orange")

add_TA(new.series.1,on=1)
add_TA(new.series.2,on=1,col="green")
