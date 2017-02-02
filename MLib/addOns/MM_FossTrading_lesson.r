#http://blog.fosstrading.com/search/label/Excel

#How to backtest a strategy in R

#Get The Data
# run the command below if quantmod isn't already installed
# install.packages("quantmod")
# use the quantmod package (loads TTR, xts, and zoo)
require(quantmod)
# pull SPX data from Yahoo (getSymbols returns an xts object)
getSymbols("^GSPC")

getSymbols("^GDAXI")
GSPC=GDAXI
#Create You Indicator
# calculate DVI indicator
dvi <- DVI(Cl(GSPC))  # Cl() extracts the close price column
rsi <- RSI(Cl(GSPC),2)  # Cl() extracts the close price column
adx <- ADX(GSPC)$ADX

#Ein ADX unter 30 spricht für eine Seitwärts-Bewegung. Liegt der ADX unter 15 #spricht dies für einen sogenannten "Anlagedruck". In diesem Fall ist eine #Kursbewegung in eine der beiden möglichen Richtungen wahrscheinlich. Der ADX zeigt #also lediglich die Stärke eines Trends an, nicht aber die Richtung 
library(sos)
findFn("training")
RSiteSearch("sos")
#Construct Your Trading Rule
# create signal: (long (short) if DVI is below (above) 0.5)
# lag so yesterday's signal is applied to today's returns
sig <- Lag(ifelse(dvi$DVI < 0.5 , 1, -1))         

sig <- Lag(ifelse(adx > 30, ifelse(dvi$DVI < 0.5 , 1, -1),0))         
sigRsi <- Lag(ifelse(rsi < 30, 1, ifelse (rsi > 70,-1,0)))   
sigRsi <- Lag(ifelse(rsi < 30, 1, -1))   
SAR -> Trainling Stop
DonchianChannel
SMA vers schnellem SMA
http://www.cfd-portal.com/indikatoren/atr-average-true-range/


# calculate signal-based returns
ret <- ROC(Cl(GSPC))*sig
colnames(ret)="DV"

retRsi <- ROC(Cl(GSPC))*sigRsi
colnames(retRsi)="RSI"



#Step 5: Evaluate strategy performance
# use the PerformanceAnalytics package
# install.packages("PerformanceAnalytics")
require(PerformanceAnalytics)
# create table showing drawdown statistics
table.Drawdowns(ret, top=10)
# create table of downside risk estimates
table.DownsideRisk(ret)
# chart equity curve, daily performance, and drawdowns
charts.PerformanceSummary(retRsi)
charts.PerformanceSummary(ret)
chart.TimeSeries(ret)
chart.Drawdown(retRsi)
table.AnnualizedReturns(ret)
table.AnnualizedReturns(retRsi)
chart.SnailTrail(ret) #hot
table.CalendarReturns(ret)
KellyRatio(ret)
Return.annualized(ret)
Return.annualized(retRsi)

maxDrawdown(ret)
maxDrawdown(retRsi)

chart.RelativePerformance(ret,retRsi)
table.UpDownRatios(ret,retRsi)

chart.RiskReturnScatter(na.omit(merge(ret,retRsi,mROC(Cl(GSPC)))))
SharpeRatio(ret)
SharpeRatio(retRsi)

last(ret)
last(retRsi)
mPlot(cumsum(na.omit(ret)),cumsum(na.omit(retRsi)))
mPlot(mRendite(ret),chart.RiskReturnScatter)


#SharpeRatio(ret)
KellyRatio(ret)
table.AnnualizedReturns(ret)
maxDrawdown(ret)


#Schätze aus der PerformanceAnalytics
#chart.Histogram(ret)
#chart.RelativePerformance
#chart.RiskReturnScatter
#chart.RollingCorrelation
#apply.rolling
#charts.TimeSeries
#Drawdowns(ret)
#DrawdownPeak(ret)
#findDrawdowns(ret)
#maxDrawdown()
#KellyRatio(ret)
#Omega(ret)
#Return.annualized(ret)
#DownsidePotential(ret)
#Return.clean(ret)  outlier ...
#TimingRatio(ret)
#########################################################################
#MMA-Position-Sizing  mit RSI
# Attach packages. You can install packages via:
# install.packages(c("quantmod","TTR","PerformanceAnalytics"))
library(quantmod)
library(TTR)
library(PerformanceAnalytics)

# Pull S&P500 index data from Yahoo! Finance
getSymbols("^GSPC", from="2000-01-01")

# Calculate the RSI indicator
rsi <- RSI(Cl(GSPC), 2)

# Calculate Close-to-Close returns
ret <- ROC(Cl(GSPC))
ret[1] <- 0

# This function gives us some standard summary
# statistics for our trades.
tradeStats <- function(signals, returns) {
  # Inputs:
  # signals : trading signals
  # returns : returns corresponding to signals
  
  # Combine data and convert to data.frame
  sysRet <- signals * returns * 100
  posRet <- sysRet > 0 # Positive rule returns
  negRet <- sysRet < 0 # Negative rule returns
  dat <- cbind(signals,posRet*100,sysRet[posRet],sysRet[negRet],1)
  dat <- as.data.frame(dat)
  
  # Aggreate data for summary statistics
  means <- aggregate(dat[,2:4], by=list(dat[,1]), mean, na.rm=TRUE)
  medians <- aggregate(dat[,3:4], by=list(dat[,1]), median, na.rm=TRUE)
  sums <- aggregate(dat[,5], by=list(dat[,1]), sum)
  
  colnames(means) <- c("Signal","% Win","Mean Win","Mean Loss")
  colnames(medians) <- c("Signal","Median Win","Median Loss")
  colnames(sums) <- c("Signal","# Trades")
  
  all <- merge(sums,means)
  all <- merge(all,medians)
  
  wl <- cbind( abs(all[,"Mean Win"]/all[,"Mean Loss"]),
               abs(all[,"Median Win"]/all[,"Median Loss"]) )
  colnames(wl) <- c("Mean W/L","Median W/L")
  
  all <- cbind(all,wl)
  return(all)
}

# This function determines position size and
# enables us to test several ideas with much
# greater speed and flexibility.
rsi2pos <- function(ind, indIncr=5, posIncr=0.25) {
  # Inputs:
  # ind : indicator vector
  # indIncr : indicator value increments/breakpoints
  # posIncr : position value increments/breakpoints
  
  # Initialize result vector
  size <- rep(0,NROW(ind))
  
  # Long
  size <- ifelse(ind < 4*indIncr, (1-posIncr*3), size)
  size <- ifelse(ind < 3*indIncr, (1-posIncr*2), size)
  size <- ifelse(ind < 2*indIncr, (1-posIncr*1), size)
  size <- ifelse(ind < 1*indIncr, (1-posIncr*0), size)
  
  # Short
  size <- ifelse(ind > 100-4*indIncr, 3*posIncr-1, size)
  size <- ifelse(ind > 100-3*indIncr, 2*posIncr-1, size)
  size <- ifelse(ind > 100-2*indIncr, 1*posIncr-1, size)
  size <- ifelse(ind > 100-1*indIncr, 0*posIncr-1, size)
  
  # Today's position ('size') is based on today's
  # indicator, but we need to apply today's position
  # to the Close-to-Close return at tomorrow's close.
  size <- lag(size)
  
  # Replace missing signals with no position
  # (generally just at beginning of series)
  size[is.na(size)] <- 0
  
  # Return results
  return(size)
}

# Calculate signals with the 'rsi2pos()' function,
# using 5 as the RSI step: 5, 10, 15, 20, 80, 85, 90, 95
# and 0.25 as the size step: 0.25, 0.50, 0.75, 1.00
sig <- rsi2pos(rsi, 5, 0.25)

# Break out the long (up) and short (dn) signals
sigup <- ifelse(sig > 0, sig, 0)
sigdn <- ifelse(sig < 0, sig, 0)

# Calculate rule returns
ret_up <- ret * sigup
colnames(ret_up) <- 'Long System Return'
ret_dn <- ret * sigdn
colnames(ret_dn) <- 'Short System Return'
ret_all <- ret * sig
colnames(ret_all) <- 'Total System Return'

# Create performance graphs
png(filename="20090606_rsi2_performance.png", 720, 720)
charts.PerformanceSummary(cbind(ret_up,ret_dn),methods='none',
                          main='RSI(2) Performance - RSI steps = 5, Size steps = 0.25')
dev.off()

# Print trade statistics table
cat('\nRSI(2) Trade Statistics - RSI steps = 5, Size steps = 0.25\n')
print(tradeStats(sig,ret))

# Print drawdown table
cat('\nRSI(2) Drawdowns - RSI steps = 5, Size steps = 0.25\n')
print(table.Drawdowns(ret_all, top=10))

# Print downside risk table
cat('\nRSI(2) Downside Risk - RSI steps = 5, Size steps = 0.25\n')
print(table.DownsideRisk(ret_all))

# Calculate signals with the 'rsi2pos()' function
# using new RSI and size step values
sig <- rsi2pos(rsi, 10, 0.3)

# Break out the long (up) and short (dn) signals
sigup <- ifelse(sig > 0, sig, 0)
sigdn <- ifelse(sig < 0, sig, 0)

# Calculate rule returns
ret_up <- ret * sigup
colnames(ret_up) <- 'Long System Return'
ret_dn <- ret * sigdn
colnames(ret_dn) <- 'Short System Return'
ret_all <- ret * sig
colnames(ret_all) <- 'Total System Return'

# Calculate performance statistics
png(filename="20090606_rsi2_performance_updated.png", 720, 720)
charts.PerformanceSummary(cbind(ret_up,ret_dn),methods='none',
                          main='RSI(2) Performance - RSI steps = 10, Size steps = 0.30')
dev.off()

# Print trade statistics table
cat('\nRSI(2) Trade Statistics - RSI steps = 10, Size steps = 0.30\n')
print(tradeStats(sig,ret))

# Print drawdown table
cat('\nRSI(2) Drawdowns - RSI steps = 10, Size steps = 0.30\n')
print(table.Drawdowns(ret_all, top=10))

# Print downside risk table
cat('\nRSI(2) Downside Risk - RSI steps = 10, Size steps = 0.30\n')
print(table.DownsideRisk(ret_all))

#######################################################################
#http://finzi.psych.upenn.edu/R/library/robfilter/html/robreg.filter.html
#dw.filter {robfilter}  http://finzi.psych.upenn.edu/R/library/robfilter/html/dw.filter.html
library(robfilter)
#Examples

# Generate random time series:
y <- cumsum(runif(500)) - .5*(1:500)
# Add jumps:
y[200:500] <- y[200:500] + 5
y[400:500] <- y[400:500] - 7
# Add noise:
n <- sample(1:500, 30)
y[n] <- y[n] + rnorm(30)


prices=Cl(GSPC)
prices=prices["2007::2009",]
y=Cl(prices)
plot(y)
y=coredata(y)[,1]
# Filtering with all methods:
#y.dw <- dw.filter(y,online=T, outer.width=31, inner.width=11, method="all")
# Plot:

#plot(y.dw)

# Filtering with trimmed RM and double window TRM only:
y2.dw <- dw.filter(y, online=T,outer.width=31, inner.width=11, method=spl("MED,RM,MTM,TRM,DWMTM,DWTRM"))
#Auf dem DWMTM sollte man mom(3) ganz guten Trend-Detektor bauen (geringe Werte)
#stehe dann für horizontale Stücke
#der RM ist schnell und glatt genug - und schneidet den langsamen MTM
#ein signal ist aber nur dan, wenn der DWMTM nicht horizontal ist.

plot(y2.dw)
head(y2.dw$slope)

new_Win(1)
plot(y2.dw)
lines(y)
plot(y)
newy=y
newy[]=y2.dw$level$RM
slope=y
slope[]=y2.dw$slope$RM

mPlot(y,newy)
mPlots(y,newy,slope)
#steigung
sslope <- dw.filter(y2.dw$slope$RM, online=T,outer.width=31, inner.width=11, method=spl("DWTRM"))
#krümmung  -- wenn die ein max hat dreht sich die steigung...
sSlope=y
sSlope[]=sslope$slope$DWTRM  #schnell .. mit slope

#wenn die 

mPlots(y,newy,slope,sSlope) #close, closeglatt, steigung von glatt, krümmung von glatt  -- 0-durchgänge und peeks (weg von 0 ) sind interessant
##################################

plot(y)
lines(newy,col="orange",lwd=2)
lines(ZLEMA(y,20),col="magenta",lwd=2) #noch schneller
lines(DEMA(y,20),col="red",lwd=2) #noch schneller
lines(EMA(y,20),col="blue",lwd=2) #noch schneller
#die diff zwischen EMA und  ZLEMA kann interessant sein


#### flat gehen wenn die vola zu hoch wird !!!
lines(VMA(y,volatility(mROC(prices))*100),col="brown",lwd=2)
mPlots(prices,volatility(mROC(prices))*100,runSD(prices))
plot(runSD(prices))
mPlot(runSD(prices),volatility(mROC(prices),calc="yang.zhang")*10000,ylog_=F)

v=(volatility(mROC(prices),calc="yang.zhang"))
hist(v,200)
VaR(v)

h=v
h[]=abs(ES(na.omit(v)))/10
colnames(h)="volaThresh"
norm_Win(2)
plot(y)
mPlot(y)
mPlot(v,h,ylog_=F)

head(y2.dw$level$MTM)



isTrend <- Lag(ifelse(y2.dw$level$MTM < 0.5 , 1, -1))
sig <- Lag(ifelse(dvi$DVI < 0.5 , 1, -1))         



########################################################################

library(NMOF) 
lower <- 1; upper <- 3; npar <- 1
res <- gridSearch(sin, lower = lower, upper = upper, npar = npar) #MMA





----------------------------
  
ta5 <<- T2$new(PortfolioName = "RenditePlus", bench="Dax")
ta5$member[["Rex"]]$item$t1Par


ta5$update()    
