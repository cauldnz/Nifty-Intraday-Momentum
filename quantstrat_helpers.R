
## helper functions for quantstrat. Includes sigAND functions from IKTrading package 
## from https://github.com/IlyaKipnis/IKTrading
#######################################################################
################### technical indicators ##############################
#######################################################################

"PRICEMOVE" <- function (x, startBar=10, ...) 
{
  days<-as.Date(index(x))
  days <- unique(days)
  y <- x[paste(days[1])]
  close = Cl(y[nrow(y),])
  move1 <- rep(NA,length(days))
  move1 <- data.frame(c(move1))
  
  for (i in 2:length(days)){
    y <- x[paste(days[i])]
    open <- Cl(y[startBar,])
    move1[i,] <- coredata(open)/coredata(close) -1
    close = Cl(y[nrow(y),])
  }
  
  move <- rep(NA,nrow(x))
  move <- data.frame(c(move))
  for (i in 1:nrow(x)){
    datex <- as.Date(index(x[i,]))
    n <- match(datex,days)
    move[i,] <- move1[n,]
  }
  colnames(move)<-c("move")
  move <- as.xts(move,index(x))
  return(move)    
}

"EOD" <- function (x, endBar=10, ...) 
{
  days<-as.Date(index(x))
  days <- unique(days)
  squareoff <- rep(0,nrow(x))
  squareoff <- data.frame(c(squareoff))
  k=1
  for (i in 1:length(days)){
    y <- x[paste(days[i])]
    for (j in 1:nrow(y)){
      if(j > nrow(y) - endBar){
        squareoff[k,] <- 1
      }
      k<-k+1
    }  
  }
  colnames(squareoff)<-c("eod")
  squareoff <- as.xts(squareoff,index(x))
  return(squareoff)  	
}

"SOD" <- function (x, startBar=10, ...) 
{
  days<-as.Date(index(x))
  days <- unique(days)
  tradetime <- rep(0,nrow(x))
  tradetime <- data.frame(c(tradetime))
  k=1
  for (i in 1:length(days)){
    y <- x[paste(days[i])]
    for (j in 1:nrow(y)){
      if(j > startBar){
        tradetime[k,] <- 1
      }
      k<-k+1
    }  
  }
  colnames(tradetime)<-c("sod")
  tradetime <- as.xts(tradetime,index(x))
  return(tradetime)  	
}

######################################################################
####################### combining signals ############################
######################################################################
"sigAND" <- function(label, data=mktdata, columns,  cross = FALSE) {
  ret_sig = NULL
  colNums <- rep(0, length(columns))
  for(i in 1:length(columns)) {
    colNums[i] <- match.names(columns[i], colnames(data))
  }
  ret_sig <- data[, colNums[1]]
  for(i in 2:length(colNums)) {
    ret_sig <- ret_sig & data[, colNums[i]]
  }
  ret_sig <- ret_sig*1
  if (isTRUE(cross)) 
    ret_sig <- diff(ret_sig) == 1
  colnames(ret_sig) <- label
  return(ret_sig)
}
