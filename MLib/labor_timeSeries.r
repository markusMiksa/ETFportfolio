

#caTools
# hat  basic interface  - schnelle alternative zu rpart
#model = LogitBoost(Data, Label, nIter=20)
#Lab   = predict(model, Data)



#runmean align right
#runmin
#runmax
#runmed   runmedian ..  und runmad
#runsd

#filter(x, rep(1, 3))
#kernapply(x, k, circular = FALSE, ...)
#trendpattern2 = filter (beerprod, filter = c(1/4, 1/4, 1/4, 1/4), sides=1)
#lowess (locally weighted regression) 
#rescaler
#robust: robust version of sd, substract median and divide by median absolute deviation
#sd: subtract mean and divide by standard deviation

if (F)  #wie normalisiere ich realistisch mehrere zeitreihen in vergleichbare intervalle
{
y = xts(10000+sin( (1:500)/30),Sys.Date() + 1:500)
plot(y,type="l")
y=data$prices[,1]
y=y+100000

plot(y)
plot(mNorm(y))
plot(mNorm2(y))
plot(Mnorm(y))

purePlot(data$prices)
p=bt.apply.matrix(data$prices, function(y) y=y-first(y))   #gem.s 0- punkt
purePlot(p) #original

p=bt.apply.matrix(data$prices, function(y) {y=y-first(y)+10000; })   #gem.s 0- punkt
purePlot(mNorm(p)) #original




purePlot(mNorm(data$prices)) #mist
purePlot(mNorm2(data$prices)) #super mist
purePlot(Mnorm(data$prices))  #good
purePlot((data$prices[,ncol(data$prices)])) #better



purePlot(data$prices)
purePlot(mNorm(data$prices))
purePlot(mNorm2(data$prices))
purePlot(Mnorm(data$prices))
purePlot(Mnorm2(data$prices))

}

plot(momentum(y)/lag(y))



plot(na.omit(mRendite(momentum(y)/(SMA(y)))))
#wie kann man stabil - richtig normieren
plot(y)
z=momentum(y)/SMA(y,3) 
z=momentum(y)/lag(y)
z=iif(!is.finite(z),0,z)
plot(1+cumsum(z))
plot(runsd(z,100,align="right"))
z=cumsum(z)
zz=z / runsd(z,100,align="right")

#der nenner schwankt !!
plot(runsd(z,150,align="right"),type="l")
plot(zz)
plot(y)

purePlot(mNorm(data$prices))
purePlot(Mnorm(data$prices))
chart.Series(data$prices)


toPeriods(data$prices)
chartCompare2(p[,1],p[,2])
mchart2()
compute.cor
mZigZag
expectTrendBreak<-function(prices=edhec,t, maxdd=10, weights=NULL,geometric=TRUE, nStd=FALSE,visual=T)
  plotOBOS("^GDAXI")  
data.coint<-function(data,frame="")

  fast.smoothing
x=any.smoothing(dax,glaettung = 200,dw=5,visual=T)
x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM ="DAX")
x=indi.Generic("signal.any.smoothing", global_arg, par=list(glaettung=300,glaettung2=0,dw=0),xarg=list(fn="SMA",fn2="",onlyLong=T,q.w=""),visual=T, TRAINSYM ="DAX")

m.to.monthly

###################################################################
##lineare regression - mit optionalem channel (wenn level ==0 ), und prognose (nplus Tage)
#optional auch gewichtet,    w = "lm.w"
# oder nach lowess  w="lowess"
#gib model, p.value,r.squared  zurück
#w=T heisst weighted .. jüngere Werte sind dann wichtiger
####################################################################
m.lm.fit<-function(p,visual=F,getIndex=T, w ="",level=0,nplus=0,glaettung=0,w.method="dist.by.date",w.range=3)
  m.predict  

library(locfit)
x <- seq(0, 1, length.out=2048)
y <- 20*sqrt(x*(1-x))*sin((2*pi*1.05)/ (x+0.05))+rnorm(2048)
 plot(y~x)
 fit.ad <- locfit(y~x, maxk=500, alpha=c(0,0,log(2048)))
 plot(fit.ad, mpv = 2048)
 plot(predict(fit.ad, what="band"), type="p")

# fit and plot a univariate local regression
data(ethanol, package="locfit")
fit <- locfit(NOx ~ E, data=ethanol)
plot(fit, get.data=TRUE)

# a bivariate local regression with smaller smoothing parameter
fit <- locfit(NOx~lp(E,C,nn=0.5,scale=0), data=ethanol)
plot(fit)

# density estimation
data(geyser, package="locfit")
fit <- locfit( ~ lp(geyser, nn=0.1, h=0.8))
plot(fit,get.data=TRUE)


if (F)
{
######################################>
#lead-lag finden mit ccf - voraussetzung:  whiten
#https://onlinecourses.science.psu.edu/stat510/?q=node/75
#whitening
x = arima.sim(list(order = c(1,1,0), ar = 0.7), n = 200) 
z = cbind(x, lag(x,-3), lag(x,-4))  #3 prognose-spalten
y = 15+0.8*z[,2]+1.5*z[,3]   #die konstruierte Target Zeitreihe hängt linear von z.1..3 ab,  wobei diese zum Teil gelaged sind
ccf(z[,1],y,na.action = na.omit) #man sieht nichts

a = cbind(y,lag3x = lag(x,-3), lag4x=lag(x, -4))  #das ist z  ..
lm(y ~lag3x+lag4x, data = a, na.action = na.omit)  #findet genau die formel von y  - aber nur wenn du die lags weisst

Y=xts(y,Sys.Date()+1:len(y))
X=xts(x,Sys.Date()+1:len(x))
plot(X)
plot(Y)

hl=HotLags2.cc(Y,X,visual=T)

hl$fit
M=na.omit(data.frame(y=Y,y=coredata(X)))
var_est <- VAR(ts(na.omit(merge(Y,X))), p=30, type="both",lag.max=5,ic="AIC")
mchart(m.xts(fitted.values(var_est)))
pred <- predict(var_est, n.ahead = 60, ci = 0.95)  #anderes Signifikanz-Intervall 
fcst=pred$fcst[1
               ]
purePlot(Y,m.xts(predict(var_est))[,1],main="s")

var_est
summary(var_est)

VARselect(ts(x), lag.max = 30, type = "none",season = NULL, exogen = NULL)

var_est <- VAR(EuStockMarkets, p=30, type="both",lag.max=3,ic="AIC")
summary(var_est)
predict(var_est,n.ahead=8,ci=0.95)






plot(var_est)
ls(var_est)
var_est$varresult
pred=predict(var_est,n.ahead = 10, ci = 0.95, dumvar = NULL)
pred$fcst[1]

plot(forecast(var_est,h=200))




plot(z)
acf(x)
diff1x=diff(z[,1],1)
dim(diff1x)
acf(diff1x, na.action = na.omit)  #zeigt nix
pacf(diff1x, na.action = na.omit) #zeigt nix

#ar1model = arima(z[,1], order = c(1,2,1))

#plot(forecast(fit,h=20))
ar1model = arima(z[,1], order = c(1,1,0))
#inspect(ar1model)
ls(ar1model)
pwx=ar1model$residuals
newpwy = filter(y, filter = c(1,-1.7445,.7445), sides =1)


#whitening 
#library(forecast)
ar1model.m=auto.arima(x)
pwx=ar1model.m$residuals    #x  bereinigt um den arima-trend
newpwy <- residuals(Arima(y,model=ar1model.m))  
ccf (pwx,newpwy,na.action=na.omit)

HotLags2.cc(xts(y,Sys.Date()+1:len(y)),xts(x,Sys.Date()+1:len(x)),visual=T)





a = cbind(y,lag3x = lag(x,-3), lag4x=lag(x, -4))
lm(y ~lag3x+lag4x, data = a, na.action = na.omit)  #findet genau die formel von y
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
}
#siehe auch mlomar {timsac}  fur multivar





if (F)
{

y= z[,1]
x=diff(z,1)
mytslars <- tslars(y~x)
summary(mytslars)




plot(y)
(y)[c(376,377,378,379)]

lag(y)[c(376,377,378,379)]
which(ROC(y)==max(ROC(y),na.rm=T))
plot(ROC(y)[c(376,377,378,379)])
plot((y)[c(376,377,378,379)])
plot(mNorm(y))

mPlots(im)

y=im[,11]  #X10Y_3M_SPREAD
head(y)
plot(y)
###########################
#verhinder den 0-durchgang und damit das div-0 - problem von ROC
y=y-min(y)+1   #das wär die Lösung - blos ist das ein zukunfts-vorgriff - sobald neue minima hinzukommen verformt das die ganze Zeitreihe
#jeder normierungs-nenner der zeitlich schwankt verformt die zeitreihe
plot(y)
plot(mNorm(y))
mPlot(10*mNorm(y),runPercentRank(y-SMA(y,200), n = 300, cumulative = FALSE,  exact.multiplier = 0.5))
mPlot(runPercentRank(y, n = 300, cumulative = FALSE, exact.multiplier = 0.5))

#arima zum detrending - um dann mit ccf die lead-lag beziehung abzulesen
#https://onlinecourses.science.psu.edu/stat510/?q=node/75
oilindex = DAX
plot (oilindex, type = "b", main = "Log of Oil Index Series")
expsmoothfit = arima (oilindex, order = c(1,2,2))  #oder auch c(0,2,2)
expsmoothfit # to see the arima results
predicteds = oilindex - expsmoothfit$residuals # predicted values
plot (oilindex, type="l", main = "Exponential Smoothing of Log of Oil Index")
lines (predicteds,col="red")
plot(expsmoothfit$residuals)
fit=auto.arima(oilindex)
plot(forecast(fit,h=20))
#https://onlinecourses.science.psu.edu/stat510/?q=node/79
#VAR(modelle) - darf Multi-dimensional sein

#pfaff
#http://www.jstatsoft.org/v27/i04/paper
#ttp://christophj.github.io/replicating/r/vector-autoregression-var-in-r/
var_est <- VAR(x, p=30, type="none",lag.max=43,ic="AIC")
summary(var_est)
VARselect(x, lag.max = 30, type = "none",season = NULL, exogen = NULL)

var_est <- VAR(EuStockMarkets, p=30, type="both",lag.max=43,ic="AIC")
summary(var_est)
plot(var_est)
pred=predict(var_est,n.ahead = 10, ci = 0.95, dumvar = NULL)
plot(forecast(var_est,h=200))
#hier kann man die optimalen lead-beziehungen abgreifen:
VARselect(EuStockMarkets[,c(2,3)], lag.max = 30, type = "both",season = NULL, exogen = NULL)$selection

#siehe auch fda und mda
#http://cran.r-project.org/web/packages/mda/mda.pdf

#CORElearn

#list.Info(var_est)
#http://christophj.github.io/replicating/r/vector-autoregression-var-in-r/
  
  #https://onlinecourses.science.psu.edu/stat510/?q=node/80
#  plot(EuStockMarkets)
#spec.ar(EuStockMarkets, log="no")
#spec.pgram(diff(EuStockMarkets,1), spans=73, taper=0, log="no")
#abline(v=1/4, lty="dotted")
}


ccf(soi,rec)
maximum values at h =???6,
And, a below average of SOI is associated with a likely above average recruit value about 6 months later.
plot(EuStockMarkets[,1])
head(EuStockMarkets)
EuStockMarkets
DAX=EuStockMarkets[,1]
FTSE=EuStockMarkets[,2]
dax=diff(DAX,1)
ftse=diff(FTSE,1)
plot(dax)
k=ccf(dax,ftse)
HotLags2(dax,ftse)
colnames(prices)
Stoxx=prices[,"EXX50_RI"]
SUP500=prices[,"SUP500"]

#### lowess und loess - glätter 

y=Stoxx
y=data$prices[,data$BENCH]
x=1:len(y)
lowess.y=xts(lowess(y~x,f=0.1)$y,index(y))
loess.y <- loess(y ~ x, data.frame(x=x,y=y),span=0.15,control = loess.control(surface = "direct"))$fitted
purePlot(merge(lowess.y,loess.y,y,SMA(y,101),SMA(loess.y,101)))
################
#detrending durch abziehen des filters oder des arima-forecasts
#vieleiht lassen sich die arima-residuen dann mit einem VAR vorhersagen...

# Filtering with trimmed RM and double window TRM only:
#http://www.statistik.uni-dortmund.de/useR-2008/slides/Schettlinger+Fried+Gather.pdf
library(robfilter)
y2.dw <- dw.filter(ts(Stoxx), online=T,outer.width=41, inner.width=31, method=spl("MED,RM,MTM,TRM,DWMTM,DWTRM"))

filter=xts(y2.dw$level,index(Stoxx))[,"TRM"]
filter=xts(y2.dw$level,index(Stoxx))[,"RM"]
filter=xts(y2.dw$level,index(Stoxx))[,"DWTRM"]
mPlots(merge(filter,Stoxx), filter-Stoxx)

extr=madore.filter(EuStockMarkets)
plot(extr)

mscarm.extr <- mscarm.filter(EuStockMarkets,autocorrelations="automatic")
ls(mscarm.extr)
signal.est

#wlen=5 ;filter =ZLEMA(Stoxx,wlen)
k=HotLags2.cc(na.omit(Stoxx-filter),na.omit(SUP500-filter),n=60,nLag=10,visual=T,main=sprintf("%d ",i))

for(i in 1:51)
{
  stoxx=mROC(na.omit(ROC(Stoxx,i)))
  sup500=mROC(na.omit(ROC(SUP500,i)))
  plot(stoxx)
  plot(sup500)
  k=HotLags2.cc(stoxx,sup500,n=60,nLag=10,visual=T,main=sprintf("%d ",i))
  if (k$wichtigkeit > 0)
  {  print(k)
     sag("%d %f",i,k$wichtigkeit,warte=T)
  }
}
class(EuStockMarkets)
plot(ts(prices[,1:5]))
stocks.spc <- spectrum(EuStockMarkets, kernel("daniell", c(30,50)),     plot = T)
plot(stocks.spc, plot.type = "marginal") # the default type
plot(stocks.spc, plot.type = "coherency")
plot(stocks.spc, plot.type = "phase")



#######################################

if (F)  
{
  #teste einige zeitreihen-libs:
  #http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.170.9102&rep=rep1&type=pdf
  
  #sma-system nur dass als smoother mal ein holtwinters, mal ein runquantile(p)
  #herhalten muss.    
  #als trigger wird mit einem EMA auf quadratischen p-Smooth- Werten garbeitet->große Abweichungen führen zu schnelleren Reaktionen
  #FAZIT:    Beim USDEUR gehts gut - bei anderen gerät er in GegenPhase ... 
  p=data$prices[,2]
  x=p[1000:2050]
  x=p
  plot(p)
  ## Exponential Smoothing
  m2 <- HoltWinters(x,alpha=0.03, gamma = FALSE, beta = FALSE)
  plot(x,type="l")
  hl=x[-1]
  hl[]=fitted(m2)[,1]
  #signal=sign(mROC(hl,20))
  hl[]=runquantile(hl,50,probs=0.20,align="right")
  
  lines(hl, col = "red")
  indi=p-hl  #sehr rauschanfällig
  #summiere die kubischen differenzen zwischen p und smooth -> hohe Werte lassen den
  #EMA damit sehr schneller ansprechen - als kleine Unterschieden
  indi = EMA((p-hl)*(p-hl) *sign(p-hl),20)
  #indi = EMA((p-hl)*(p-hl) *(p-hl),30)
  signal=sign(indi)
  
  
  plotSigPrice(signal=signal,prices=x[-1],indi=list(hl=merge(x[-1],hl),ind=merge(mROC(hl,30),0)))
  ################################################################
  
  hl[]=runquantile(x[-1],150,probs=0.30,align="right")
  
  lines(hl, col = "red")
  indi=p-hl  #sehr rauschanfällig
  indi = EMA((p-hl)*(p-hl) *sign(p-hl),20)
  #indi = EMA((p-hl)*(p-hl) *(p-hl),30)
  signal=sign(indi)
  
  plotSigPrice(signal=signal,prices=x[-1],indi=list(hl=merge(x[-1],hl),ind=merge(mROC(hl,30),0) ))
  
  ############################################################
  library(signalextraction)  #hat sich auf die Fahne geschrieben Wendestellen gut zu finden ...
  x.m=m.to.monthly(x)
  fit<-  dfa(coredata(x.m))
  plot(fit)
  plot(fitted(fit), type="l")
  ls(fit)
  
  #nun so parametrisieren, das trendwechsel früh erkannt werden
  
  for(ci in 1:ncol(data$prices))
  {
    x=data$prices[,ci]
    x.m=m.to.monthly(x)
    
    fit2<-dfa(coredata(x.m), quart = FALSE, d = 0, tpfilter = TRUE,
              lambda = 5, expweight = 1.5, pbd = 1.02,
              pb = 1/6.5, sb = 1/6, n.loops = 10, verbose = 1,
              limamp = 3, i2 = TRUE)
    plot(fit2)
    
    hl=x.m
    hl[]=fitted(fit2)
    plot(x.m)
    lines(hl, col = "red")
    
    signal=sign(mROC(hl,1))
    
    plotSigPrice(signal=signal,prices=x.m,indi=list(hl=merge(x.m,hl),ind=merge(mROC(hl,1),0) ))
  }
  ####################################
  #mit tagesdaten bringt nichts..
  for(ci in 1:ncol(data$prices))
  {
    x=data$prices[,ci]
    
    fit3<-dfa(coredata(x), quart = FALSE, d = 0, tpfilter = TRUE,
              lambda = 5, expweight = 1.5, pbd = 1.02,
              pb = 1/6.5, sb = 1/6, n.loops = 10, verbose = 1,
              limamp = 3, i2 = TRUE)
    plot(fit3)
    
    hl=x
    hl[]=fitted(fit3)
    plot(x)
    lines(hl, col = "red")
    
    signal=sign(EMA(mROC(hl,1),250))
    plotSigPrice(signal=signal,prices=x,indi=list(hl=merge(x,hl),ind=merge(mROC(hl,1),0) ))
    
  }
  #########################
  #http://robjhyndman.com/hyndsight/batch-forecasting/
  
  library(forecast)
  
  retail <- read.csv("http://robjhyndman.com/data/ausretail.csv",header=FALSE)
  retail <-data$prices
  retail <- ts(retail[,-1],f=12)
  
  ns <- ncol(retail)
  h <- 24
  fcast <- matrix(NA,nrow=h,ncol=ns)
  for(i in 1:ns)
    fcast[,i] <- forecast(retail[,i],h=h)$mean
  #That will select an ETS model using the AIC, esti???mate the para???me???ters, and gen???er???ate fore???casts. 
  write(t(fcast),file="ets_fcasts.csv",sep=",",ncol=ncol(fcast))

  #######################################################
  
  ##################################################################
  library(mFilter)
  data(unemp)
  unemp=ts(x.m,frequency=20)
  unemp=ts(x,frequency=1)
  
  plot(unemp)
  opar <- par(no.readonly=TRUE)
  lambda=0.4
  unemp.hp <- mFilter(unemp,filter="HP") # Hodrick-Prescott filter
  print(unemp.hp)
  summary(unemp.hp)
  residuals(unemp.hp)
  fitted(unemp.hp)
  plot(unemp.hp)
  unemp.bk <- mFilter(unemp,filter="BK") # Baxter-King filter
  unemp.cf <- mFilter(unemp,filter="CF") # Christiano-Fitzgerald filter
  unemp.bw <- mFilter(unemp,filter="BW") # Butterworth filter
  unemp.tr <- mFilter(unemp,filter="TR") # Trigonometric regression filter
  par(mfrow=c(2,1),mar=c(3,3,2,1),cex=.8)
  plot(unemp,main="Unemployment Series & Estimated Trend", col=1, ylab="")
  lines(unemp.hp$trend,col=2)
  lines(unemp.bk$trend,col=3)
  lines(unemp.cf$trend,col=4)
  lines(unemp.bw$trend,col=5)
  lines(unemp.tr$trend,col=6)
  
  legend("topleft",legend=c("series", "HP","BK","CF","BW","TR"),
         col=1:6,lty=rep(1,6),ncol=2)
  plot(unemp.hp$cycle,main="Estimated Cyclical Component",
       ylim=c(-2,2.5),col=2,ylab="")
  lines(unemp.bk$cycle,col=3)
  lines(unemp.cf$cycle,col=4)
  lines(unemp.bw$cycle,col=5)
  lines(unemp.tr$cycle,col=6)
  ## legend("topleft",legend=c("HP","BK","CF","BW","TR"),
  ## col=2:6,lty=rep(1,5),ncol=2)
  unemp.cf1 <- mFilter(unemp,filter="CF", drift=TRUE, root=TRUE)
  unemp.cf2 <- mFilter(unemp,filter="CF", pl=8,pu=40,drift=TRUE, root=TRUE)
  unemp.cf3 <- mFilter(unemp,filter="CF", pl=2,pu=60,drift=TRUE, root=TRUE)
  unemp.cf4 <- mFilter(unemp,filter="CF", pl=2,pu=40,drift=TRUE,
                       root=TRUE,theta=c(.1,.4))
  plot(unemp,
       main="Christiano-Fitzgerald filter of unemployment: Trend \n root=TRUE,drift=TRUE",
       col=1, ylab="")
  lines(unemp.cf1$trend,col=2)
  lines(unemp.cf2$trend,col=3)
  lines(unemp.cf3$trend,col=4)
  lines(unemp.cf4$trend,col=5)
  legend("topleft",legend=c("series", "pl=2, pu=32", "pl=8, pu=40",
                            "pl=2, pu=60", "pl=2, pu=40, theta=.1,.4"), col=1:5, lty=rep(1,5), ncol=1)
  plot(unemp.cf1$cycle,
       main="Christiano-Fitzgerald filter of unemployment: Cycle \n root=TRUE,drift=TRUE",
       col=2, ylab="", ylim=range(unemp.cf3$cycle))
  lines(unemp.cf2$cycle,col=3)
  lines(unemp.cf3$cycle,col=4)
  lines(unemp.cf4$cycle,col=5)
  ## legend("topleft",legend=c("pl=2, pu=32", "pl=8, pu=40", "pl=2, pu=60",
  ## "pl=2, pu=40, theta=.1,.4"), col=2:5, lty=rep(1,4), ncol=2)
  par(opar)
  
  ##########################################################################
  
  library(dse)  #multivariat, ... viele modell
  
  
  
}

if (F)
  list_R_functions('MLib/labor_timeSeries.r')
