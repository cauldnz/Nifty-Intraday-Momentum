require("sde")    ## for MOdist function
require ("sm")		## for kernel regression
require("dtw")    ## for DTW distance

Typ <- function(x){
  return((Cl(x)+Lo(x)+Hi(x))/3)
}

predict.close <- function(oldDays, newDay, distMethod, k=10) {

  # different alignment case for MOdist
  bMOdist = FALSE
  if (distMethod == "MOdist"){bMOdist=TRUE}
  
  # aggregate the past and the present
  aggDays <- cbind(oldDays[1:nrow(newDay),], newDay)
  if(!bMOdist){aggDays <- t(aggDays)}

  # compute the distance between days
  distMethod <- get(distMethod)
  distDays <- as.matrix(distMethod(aggDays))

  # take the k nearest neigbours
  if(!bMOdist){temp <- distDays[nrow(aggDays),]}
  if(bMOdist){temp <- distDays[ncol(aggDays),]}
  
  temp <- temp[-length(temp)]
  nbor <- which(rank(temp)<=k)
  
  return(nbor)

}


get.daily.series <- function(x,pattern.date, n.period=1, truncate=T){
  dates<-as.Date(index(x))
  dates <- unique(dates)
  n.dates <- length(dates)
  pattern.date <- as.Date(pattern.date)
  
  n.pattern <- match(pattern.date,dates)
  if (n.pattern < 1) {stop("no match found for specified date in data")}

  n.rows <- 375/n.period
  x <- Typ(x)
  old.series <- data.frame()

  for (i in 1:(n.dates)){
    if (i==n.pattern) next

    if (nrow(old.series)==0){
      old.series <- coredata(x[paste("",dates[i],"",sep="")])
      colnames(old.series) <- paste(colnames(old.series),".",dates[i],sep="")
      if(truncate==T){old.series <- data.frame(old.series[1:n.rows,])}
    } else{
      temp <- x[paste("",dates[i],"",sep="")]
      colnames(temp) <- paste(colnames(temp),".",dates[i],sep="")
      if(truncate==T){temp <- data.frame(temp[1:n.rows,])}
      old.series <- cbind(old.series,coredata(temp))
    }
  }
  
  new.series <- x[paste("",dates[n.pattern],"",sep="")]
  colnames(new.series) <- paste(colnames(new.series),".",dates[n.pattern],sep="")
  if(truncate==T){new.series <- new.series[1:n.rows,]}

  old.series <- as.xts(old.series,order.by=index(new.series))

  ## now convert the series as changes from the start
  for(i in 1:(n.dates-1)){
    old.series[,i] <- old.series[,i] - as.numeric(old.series[1,i])
  }
  
  new.series <- new.series - as.numeric(new.series[1,1])

  out <- list()
  out$old.series <- old.series
  out$new.series <- new.series
  return(out)
}

get.kernel.fit <- function(x,factor=2){
  n.col = ncol(x)
  kernel = data.frame()

  for (i in 1:n.col){
    y <- as.vector(x[,i])
    n = length(y)
    t = 1:n
    h = h.select(t, y, method = 'cv')/factor
    temp = sm.regression(t, y, h=h, display = 'none')
    mhat = approx(temp$eval.points, temp$estimate, t, method='linear')$y
    if(ncol(kernel)==0){
      kernel = data.frame(mhat)
      colnames(kernel) <- colnames(x)[i]
    } else {
      temp = data.frame(mhat)
      kernel = cbind(kernel,temp)
      colnames(kernel)[i] <- colnames(x)[i]
    }
  }

  kernel = as.xts(kernel,order.by=index(x))
}


k.nearest.neighbor <- function(x,date,time=180, k=10,smoothed=T){
  
  sig <- list()
  signal <- 0
  performance <- list()
  
  data <- x[paste("::",date,sep="")]
  data <- get.daily.series(data,date)
  
  if(smoothed){
    old.series <- get.kernel.fit(data$old.series)
    new.series <- get.kernel.fit(data$new.series)
    new.series.1 <- new.series[1:time,]
  } else{
    old.series <- data$old.series
    new.series <- data$new.series
    new.series.1 <- new.series[1:time,]
  }
  
  xx <- predict.close(old.series,new.series.1,"dtwDist",k)
  
  old.series <- data$old.series
  new.series <- data$new.series
  yy <- old.series[,as.numeric(xx[1])]
  for (i in 2:k){
    zz <- old.series[,as.numeric(xx[i])]
    yy <- cbind(yy,zz)
  }
  
  zzz <- rep(0,k)
  for (i in 1:k){
    zzz[i] <- coredata(yy[nrow(yy),i]) - coredata(yy[time,i])
  }
  
  results <- list()
  results$mean <- mean(zzz)
  results$median <- median(zzz)
  results$sd <- sd(zzz)
  results$percentile.down <- quantile(zzz,0.1)
  results$percentile.up <- quantile(zzz,0.9)
  results$range <- quantile(zzz,0.75) - quantile(zzz,0.25)

  if(results$mean<0 && results$median<0 && abs(results$median)/results$sd>0.1 && abs(results$percentile.down)/abs(results$percentile.up)>1.25) {signal=-1}
  if(results$mean>0 && results$median>0 && abs(results$median)/results$sd>0.1 && abs(results$percentile.up)/abs(results$percentile.down)>1.25) {signal=1}

  if(nrow(new.series)>time){
    performance$return <- as.numeric(coredata(new.series[nrow(new.series),1]) - coredata(new.series[time,1]))*signal
    if (signal==1) {
      performance$drawdown <- -as.numeric((min(coredata(new.series[time:nrow(new.series),])) - coredata(new.series[time,1])))
      performance$maxup <- as.numeric(max(coredata(new.series[time:nrow(new.series),])) - coredata(new.series[time,1]))
    } else if(signal==-1){
      performance$drawdown <- as.numeric(max(coredata(new.series[time:nrow(new.series),])) - coredata(new.series[time,1]))
      performance$maxup <- -as.numeric((min(coredata(new.series[time:nrow(new.series),])) - coredata(new.series[time,1])))
    } else {
      performance$drawdown <- 0
      performance$maxup <- 0
    }
  }

  sig$signal = signal
  sig$data = yy
  sig$results = results
  sig$performance = performance

  return(sig)
}
