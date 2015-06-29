require (quantmod)
require(TTR)
require(quantstrat)
require(PerformanceAnalytics)
source("E:/R/WD/GoogleQuote.R")
source("E:/R/Systematic Trading/quantstrat_helpers.R")

## get the data first, for last 20 days, 2min data
NIFTY<-getIntradayPrice("NIFTY",src="google",period=60, interval=2, auto.assign=FALSE)
symbols <- c("NIFTY")

#set parameters
initDate=as.Date(index(get(symbols))[1])
.from=initDate
.startBar = 15        ## wait 20mins to check the opening move
.endBar = 15          ## squareoff at this point if open position
.threshold = 0.002    ## 0.20% opening up and down move threshold
.txnfees = -20        ## transaction charge in INR
.orderqty = 100       ## quantity to buy or sell
.stoploss = 0.0025    ## 0.25% is the stop
.takeprofit = 0.005   ## 0.50% take profit target
leverage = 4          ## leverage on the margin
initEq <- 0           ## investible capital
tradeSize <- initEq*leverage

# initialize
rm(list=ls(.blotter), envir=.blotter)
currency('INR')
Sys.setenv(TZ="UTC")
strategy.st <- portfolio.st <- account.st <- "NIFTYLongShort"
rm.strat(strategy.st)
initPortf(portfolio.st, symbols=symbols, initDate=initDate, currency='INR')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='INR',initEq=initEq)
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)
addPosLimit(portfolio=portfolio.st,symbol='NIFTY',timestamp=initDate,maxpos=.orderqty)

## indicators
add.indicator(strategy.st, name="PRICEMOVE",
              arguments=list(x=quote(Cl(mktdata)[,1]), startBar=.startBar),
              label="move")
add.indicator(strategy.st, name="EOD",
              arguments=list(x=quote(Cl(mktdata)[,1]), endBar=.startBar),
              label="eod")
add.indicator(strategy.st, name="SOD",
              arguments=list(x=quote(Cl(mktdata)[,1]), startBar=.startBar),
              label="sod")

# signals
add.signal(strategy.st, name='sigThreshold',
           arguments = list(
             column="move", threshold=-.threshold, cross=FALSE,
             relationship="lt"
           ),
           label='sell'
)
# signals
add.signal(strategy.st, name='sigThreshold',
           arguments = list(
             column="move", threshold=.threshold, cross=FALSE,
             relationship="gt"
           ),
           label='buy'
)
# signals
add.signal(strategy.st, name='sigThreshold',
           arguments = list(
             column="sod", threshold=1, cross=TRUE,
             relationship="eq"
           ),
           label='filter'
)
# signals
add.signal(strategy.st, name="sigAND",
           arguments=list(columns=c("filter", "buy"), cross=TRUE),
           label="long")
# signals
add.signal(strategy.st, name="sigAND",
           arguments=list(columns=c("filter", "sell"), cross=TRUE),
           label="short")
# signals
add.signal(strategy.st, name='sigThreshold',
           arguments = list(
             column="eod", threshold=1, cross=TRUE,
             relationship="eq"
           ),
           label='squareoff'
)
## entry rule
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty= .orderqty,
                        #osFUN=osMaxPos,
                        orderset='ocolong',
                        replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short' , sigval=TRUE,
                        orderside='short' ,
                        ordertype='market',
                        orderqty= -.orderqty,
                        #osFUN=osMaxPos,
                        orderset='ocoshort',
                        replace=FALSE
         ),
         type='enter',
         label='EnterSHORT'
)
## exit rule
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='squareoff', sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        orderset='ocolong',
                        replace=FALSE
         ),
         type='exit',
         label='longExit'
)
## exit rule
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='squareoff', sigval=TRUE,
                        orderside='short' ,
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        orderset='ocoshort',
                        replace=FALSE
         ),
         type='exit',
         label='shortExit'
)
## stoploss rule
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='short',
                        ordertype='stoptrailing',
                        orderqty='all',
                        TxnFees=.txnfees,
                        tmult = TRUE,
                        threshold = quote(.stoploss),
                        orderset='ocoshort',
                        replace=FALSE
         ),
         type='chain',parent = 'EnterSHORT',
         label='stoplossShort',
         enabled=TRUE
)
## stoploss rule
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long', sigval=TRUE,
                        orderside='long',
                        ordertype='stoptrailing',
                        orderqty='all',
                        TxnFees=.txnfees,
                        tmult = TRUE,
                        threshold = quote(.stoploss),
                        orderset='ocolong',
                        replace=FALSE
         ),
         type='chain',parent = 'EnterLONG',
         label='stoplossLong',
         enabled=TRUE
)
# take profit
add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=FALSE,
                        orderside='long',
                        ordertype='limit', 
                        tmult=TRUE, 
                        threshold=quote(.takeprofit),
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocolong'
         ),
         type='chain', parent='EnterLONG',
         label='TakeProfitLONG',
         enabled=FALSE
)
# take profit
add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=FALSE,
                        orderside='long',
                        ordertype='limit', 
                        tmult=TRUE, 
                        threshold=quote(.takeprofit),
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocoshort'
         ),
         type='chain', parent='EnterSHORT',
         label='TakeProfitSHORT',
         enabled=FALSE
)

## run back test
applyStrategy(strategy.st, portfolio.st)
#View(getOrderBook(portfolio.st)[[portfolio.st]]$NIFTY)
#updatePortf(portfolio.st, Symbols='NIFTY', Dates=paste('::',as.Date(Sys.time()),sep=''))
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(strategy.st)
chart.Posn(portfolio.st, "NIFTY", TA='add_SMA(n=10);add_SMA(n=100)')
View(t(tradeStats(portfolio.st, 'NIFTY')))
View(perTradeStats(portfolio.st))
#chart.ME(Portfolio=portfolio.st, Symbol='NIFTY', type='MAE', scale='percent')
#chart.ME(Portfolio=portfolio.st, Symbol='NIFTY', type='MFE', scale='percent')
#View(getAccount(account.st)$summary)
#require(lattice)
#xyplot(getAccount(account.st)$summary,type="h",col=4)
chart_Series((getAccount(account.st)$summary)$End.Eq)
add_TA(Cl(NIFTY))
