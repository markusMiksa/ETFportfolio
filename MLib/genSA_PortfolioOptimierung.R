### In diesem File: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#a) Bei MM_GENSA
# findet eine rollierende Portfolio-Optimierung 
# statt -- inkls. Transaktionskosten beachtung...
# es werden unterschiedlichste Optimierungs - Methoden angeboten   - incls. auch CVar
# benutzt wird DeOptim u. GenSA. GenSA konvergiert viel schneller !
# Es gelingt eine coole Optimierungsfunktion optOmega.maxcalmar mit GenSa rollierend über viele Monate zu optimieren.
# Dabei werden transaktionskosten, Faber-Differenzen, minGewichte ...  berücksichtigt -> Sharepe von 0.7 !!! sehr gut !!!!
#
#b) Bei MM_FeatureSelect  versucht er via Optimierer unterschiedlichste Kombinationen von Parametern für ein PNN
# auszubrobieren (binär-Maske maskiert die Feature-Spalten, die Binärmaske entspricht einer 32bit-Int-Zahl)
# Der Optimierer sollte lediglich integer-Zahlen liefern.  Das scheint in der R-Welt ein riesen Problem zu sein.
# Probiere:    genoud(), pso(), optim(method="SANN")
#Hilft alles nichts: daher  runde nun immer, bestrafe "krumme" und gibt Werte die mehrfach ausprobiert werden aus dem 
#Ergebnis-cashe  oldRes zurück
# Dann sollt das auch wieder mit dem schnellen GenSA gehen.
#+~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#vergleich die Optimierungs-Perforamnce von DeOptim mit genSA 
#http://www.r-bloggers.com/a-comparison-of-some-heuristic-optimization-methods/
#-  genSa ist schneller

#http://journal.r-project.org/archive/2013-1/xiang-gubian-suomela-etal.pdf
#Code aus:
#g:\R papers\genSA_optimizer.pdf

#am schönen Beispiel der Portfolio-Omtimierung cvar  
#Risk allocation portfolios
#Mean-risk models were developed in the 1950s for portfolio selection problems. Value-at-Risk (VaR)
#and Conditional Value-at-Risk (CVaR) are the most popular measures of downside risk.


if (F)
{  
  #Rohdatenbeschaffung
  
  
  library("quantmod")
  tickers <- c("GE", "IBM", "JPM", "MSFT", "WMT")
  getSymbols(tickers, from = "2000-12-01", to = "2014-12-31")
  pricess=merge(mNorm(Cl(GE)),mNorm(Cl(IBM)),mNorm(Cl(JPM)),mNorm(Cl(MSFT)),mNorm(Cl(WMT)))
  n=len(tickers)
  P <- NULL
  for(ticker in tickers) {
    tmp <- Cl(to.monthly(eval(parse(text = ticker))))
    P <- cbind(P, tmp)
  }
  
  mPlot(pricess)
  ###############
  
  ls(data)
  data = new.env()
  tickers <-getIndexComposition("^GDAXI")
  
  getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
  for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)    
  bt.prep(data, align='keep.all', dates='1970::')
  data$symbolnames
  head(data[["ADS.DE"]])
  data$weight
  #data$prices = data.info(data)
  data.repair(data$prices)
  
  price=data$prices
  source("MLib/indicators.r")
  Entry.signal<<-rollapplyr(price, 1, roll.Target, by.column = T,Prices=price, maxDD=10,minEQ=20)
  #man kann das Trainingssignal nur bis zur letzten Long-Position benutzen .. am rechten Rand wirds ungewiss weil minEQ nicht mehr geschafft wird.
  
  #MM_TODO... dafür müsst ich jetzt über jedes symbol separat iterieren ...-> Vereinfachung #VX
  if (F)
  {
    rechter.Rand= DateS(last(Entry.signal[Entry.signal>0]))
    train.frame = sprintf("::%s",rechter.Rand)
    #damit sind die Trainings-Soll-Daten vorbereitet
    Train.signal<<-Entry.signal[train.frame,];  #colnames(Train.signal)=c("DAX")
  }
  Train.signal<<-Entry.signal    #VX
  #vorsicht: kein echter pos-plot  - sondern ein entry-plot
  #  plotSigPrice(signal=Entry.signal[train.frame,],prices=mNorm(price[train.frame,]),indi=list(stopLine=merge(price[train.frame,]) ))
  
  data$Train.signal = Train.signal  
  
  save(list=ls(data),file="MData/dax.Rdata",envir=data)
  
  
  
  data=new.env()
  load(file="MData/dax.Rdata",envi=data)
  ls(data)
  data$Train.signal
  data$symbolnames
  #Prices=data.info(data) 
  xyplot(data$prices)
  
  
}
#*****************************************************************
# Code Strategies
#****************************************************************** 
if(F)
{
pricess = data$prices  
mchart(mNorm(pricess))
purePlot(mNorm(pricess))


P=pricess
colnames(P) <- tickers
#####################################################################

R <- diff(log(P))
R <- R[-1,]
mu <- colMeans(R)
sigma <- cov(R)

library("PerformanceAnalytics")


pContribCVaR <- ES(weights = rep(0.2, 5),
                   method = "gaussian", portfolio_method = "component",
                   mu = mu, sigma = sigma)$pct_contrib_ES
}
#g:\R papers\deOptimCVAR.pdf

#The constraint that each asset can contribute at most 5% to total portfolio CVaR risk is
#imposed through the addition of a penalty function to the objective function. As such, we
#allow the search algorithm to consider infeasible solutions. A portfolio which is unacceptable
#for the investor must be penalized enough to be rejected by the minimization process and
#the larger the violation of the constraint, the larger the increase in the value of the objective
#function.

objCVar <- function(w,mu,sigma) {
  fn.call <<- fn.call + 1   #globaler Call-Counter ...
  
  if (sum(w) == 0) { w <- w + 1e-2 }
  w <- w / sum(w)
  CVaR <- ES(weights = w,
             method = "gaussian", portfolio_method = "component",
             mu = mu, sigma = sigma)
  tmp1 <- CVaR$ES
  tmp2 <- max(CVaR$pct_contrib_ES - 0.225, 0)
  out <- tmp1 + 1e3 * tmp2
  return(out)
}


if (F)
{
#We first run DEoptim with the same setting as in Ardia et al.
set.seed(1234)
fn.call <- 0
sink("tmp.txt")
out.DEoptim <- DEoptim(fn = objCVar, lower = rep(0, 5), upper = rep(1, 5),mu=mu,sigma=sigma)
sink(NULL)
fn.call.DEoptim <- fn.call
out.DEoptim$optim$bestval
out.DEoptim$optim$nfeval
cat("DEoptim call functions", fn.call.DEoptim, "times.\n")
#DEoptim call functions 10050 times.

#Nelder-Mead method can be used for further re?nement since this objective function is non-smooth.
#by the use of a local search method. 
out.DEoptim.fur <- optim(par = out.DEoptim$optim$bestmem, fn = obj, method = "Nelder-Mead")
out.DEoptim.fur$value

###################################################################
#Nun mit GenSA
###################################################################
library(GenSA)
set.seed(1234)
fn.call <<- 0
out.GenSA <- GenSA(fn = objCVar, lower = rep(0, 5), upper = rep(1, 5),     control = list(smooth = FALSE, max.call = 3000),mu=mu,sigma=sigma)
fn.call.GenSA <- fn.call
out.GenSA$value
out.GenSA$counts
cat("GenSA call functions", fn.call.GenSA, "times.\n")
#GenSA call functions 3000 times.
wstar.GenSA <- out.GenSA$par
wstar.GenSA <- wstar.GenSA / sum(wstar.GenSA)
rbind(tickers, round(100 * wstar.GenSA, 2))


100 * (sum(wstar.GenSA * mu) - mean(mu))
}
####################################################################




####################################################################
# weiter lesen: in g:\R papers\yollin_slides_PortfolioOpti2013.pdf
######################################################################

#Omega-Perf-Measure:
optOmega = function(x,ret,L)
{
  fn.call <<- fn.call + 1
  
  retu = ret %*% x
  #retu timeSeries or zoo object of asset returns
  #L is the loss threshold that can be specified as zero, return from a benchmark index, or an absolute rate of return - any specified level
  obj = -Omega(retu,L=L,method="simple")
  weight.penalty = 100*(1-sum(x))^2
  return( obj +  weight.penalty )
}  

if (F)
{
n.assets  = 5
wmax = 1
L=0
r=R
lower = rep(0,n.assets)
upper = rep(wmax,n.assets)
res = DEoptim(optOmega,lower,upper,
              control=list(NP=2000,itermax=1000,F=0.2,CR=0.8),
              ret=coredata(r),L=L)

w = cleanWeights(res$optim$bestmem,syms)
w[w!=0]
##################################
fn.call <<- 0
out.GenSA <- GenSA(fn = optOmega, lower = rep(0, 5), upper = rep(1, 5), 
                   control = list(smooth = FALSE, max.call = 9000),ret=coredata(r),L=L)
fn.call.GenSA <- fn.call
out.GenSA$value
out.GenSA$counts
cat("GenSA call functions", fn.call.GenSA, "times.\n")
#GenSA call functions 3000 times.
wstar.GenSA <- out.GenSA$par
wstar.GenSA <- wstar.GenSA / sum(wstar.GenSA)
rbind(tickers, round(100 * wstar.GenSA, 2))
}
#################################################
#Optimization with Additional Constraints

optOmega.maxdd = function(x,ret,L)  #x = portfolio-gewicht  - heute,  ret = return
{
  
  fn.call <<- fn.call + 1
  browser(mP("||"))
  retu <<- ret %*% x      #das skalarprodukt  .. also die portfolio eq
  #objOmega = -Omega(retu,L=L,method="simple")
  #obj = mddx(retu,1)   #maxdrawDown
  retu.xts = R[c(1:shape(ret)),1]; retu.xts[] = retu
  obj= - nval(last(mLogRendite(retu.xts))) / maxDrawdown(retu.xts)
  
  weight.penalty = 100*(1-sum(x))^2
  small.weight.penalty = 100*sum(x[x<0.03])   #hier 
  return( obj + weight.penalty + small.weight.penalty  )
}


if (F)
{
fn.call <<- 0
out.GenSA <- GenSA(fn = optOmega.maxdd, lower = rep(0, 5), upper = rep(1, 5), 
                   control = list(smooth = FALSE, max.call = 3000),ret=coredata(r),L=L)
fn.call.GenSA <- fn.call
out.GenSA$value
out.GenSA$counts
cat("GenSA call functions", fn.call.GenSA, "times.\n")
#GenSA call functions 3000 times.
wstar.GenSA <- out.GenSA$par
wstar.GenSA <- wstar.GenSA / sum(wstar.GenSA)
rbind(tickers, round(100 * wstar.GenSA, 2))
}
######################################################
### mit R-Ratio als Ziel (siehe:  g:\R papers\yollin_slides_PortfolioOpti2013.pdf)
######################################################


optOmega.Rratio = function(x,ret,L)  #x = portfolio-gewicht  - heute,  ret = return
{
  fn.call <<- fn.call + 1
  
  retu <<- ret %*% x      #das skalarprodukt  .. also die portfolio eq
  #objOmega = -Omega(retu,L=L,method="simple")
  #obj = mddx(retu,1)   #maxdrawDown
  retu.xts = R[,1]; retu.xts[] = retu
  #  obj= maxDrawdown(retu.xts)
  #R-Ratio Optimization:
  obj = -CVaR(-retu.xts)/CVaR(retu.xts)
  weight.penalty = 100*(1-sum(x))^2
  small.weight.penalty = 100*sum(x[x<0.03])   #hier 
  return( obj + weight.penalty + small.weight.penalty  )
}
if (F)
{
fn.call <<- 0
out.GenSA <- GenSA(fn = optOmega.Rratio, lower = rep(0, 5), upper = rep(1, 5), 
                   control = list(smooth = FALSE, max.call = 3000),ret=coredata(r),L=L)
fn.call.GenSA <- fn.call
out.GenSA$value
out.GenSA$counts
cat("GenSA call functions", fn.call.GenSA, "times.\n")
#GenSA call functions 3000 times.
wstar.GenSA <- out.GenSA$par
wstar.GenSA <- wstar.GenSA / sum(wstar.GenSA)
rbind(tickers, round(100 * wstar.GenSA, 2))
}
###################################################################
#Nun mit optOmega.maxcalmar mit  GenSA  und rollaplyR
###################################################################


optOmega.maxcalmar = function(x,ret,L, oldweight,faber)  #x = portfolio-gewicht  - heute,  ret = return
{
  fn.call <<- fn.call + 1
  #mP("optOmega.maxcalmar")
  retu <<- ret %*% x      #das skalarprodukt  .. also die portfolio eq
  
  retu.xts = R[c(1:shape(ret)),1]; retu.xts[] = retu
  #obj = -Omega(retu.xts,L=L,method="simple")
  
  #maxCalmar der portfolio-eq der  Vergangenheit...
  price=mLogRendite(retu.xts)
  obj=  -nval(last(price)) / maxDrawdown(retu.xts)  #da hier blos monatsdaten 
  #  zum einsatz kommen, könnt der maxDrawdown natürlich noch größer bei Tagesdaten sein.
  #obj= - maxDrawdown(retu.xts) / nval(last(price)) #da hier blos monatsdaten 
  
  years=shape(retu.xts) / 12
  obj=obj/years
  weight.penalty = 100*(1-sum(x))^2
  small.weight.penalty = 100*sum(x[x<0.03])   #hier 
  
  ##   bestrafe aber auch einen heftige Turnover
  forecast.penality = -mean(faber*x) #pnn.guess()...
  turnover.penality = sum( abs(x - oldweight) ,na.rm=T)/10#MM_Tbremse
  #bevorzuge Gewichte, die den alten Gewichten gleichen
  
  #schau ob die Terme in vernünftigem größenverhältnsi stehen..
  #mP("%f \t %f \t %f \t %f \t %f",obj , weight.penalty ,small.weight.penalty  , turnover.penality,forecast.penality) 
  #browser("2222")  
  
  return( obj + weight.penalty + small.weight.penalty  + turnover.penality + forecast.penality)
}
##################################


rollGenSA.Portfolio<-function(Y,win=60,mode ="maxcalmar")
{
  dolm<-function(r,mode)
  {
    library(GenSA)
    set.seed(1234)
    print(last(r))
    mP("optimize next month")
    fn.call <<- 0
    #TODO hier noch den Pnn-forecast-vector übergeben und in fn mit den weights gewichten.
    #browser(mP("next Optimisation))
    if (mode =="maxcalmar")
    {
      mP("technique")
      price=mLogRendite(r)
      purePlot(price)
      
      #mchart(pricess[fromToS(price)])
      sma = bt.apply.matrix(price, SMA, win)
      faber=last((price-sma)/nval(first(price))*100)
      out.GenSA <- GenSA(fn = optOmega.maxcalmar, lower = rep(0, n), upper = rep(1, n), 
                         control = list(smooth = FALSE, max.call = 3000),
                         ret=coredata(r),L=L,oldweight=oldweight,faber=faber) 
    }
    else
      if(mode=="CVar")
      {
        mu <- colMeans(r)
        sigma <- cov(r)
        
        out.GenSA <- GenSA(fn = objCVar, lower = rep(0, n), upper = rep(1, n),     control = list(smooth = FALSE, max.call = 3000),mu=mu,sigma=sigma)
      }
    fn.call.GenSA <- fn.call
    out.GenSA$value
    print(out.GenSA$counts)
    cat("GenSA call functions", fn.call.GenSA, "times.\n")
    #GenSA call functions 3000 times.
    wstar.GenSA <- out.GenSA$par
    wstar.GenSA <- wstar.GenSA / sum(wstar.GenSA)
    print(rbind(tickers, round(100 * wstar.GenSA, 2)))
    resWeights = round(100 * wstar.GenSA, 2)
    oldweight <<-resWeights/100
  }
  
  oldweight <<- rep(1/n,times=n)
  
  #browser(mP("rollapl"))
  #optimiere über das letzten win-Monate
  weights.xts=rollapplyr(Y, win, dolm, by.column = F,mode=mode)
  #colnames(ret)=  sapply(colnames(Y),function(x) sprintf("%s_weight%d",x,win))
  
  return(weights.xts)
}

if (F)
{
#MM_GENSA
r=na.omit(R)

fromTo(na.omit(R))

#mit r = diff(log(price)) als Vektor gehts hier in die monatliche Gewichtsberechnung
weights= rollGenSA.Portfolio(r,win=60,mode ="maxcalmar")  ### << test.me

turnover.1=weights[,1];turnover.1[]=NA;
turnover.1[]=(rowSums(abs(weights-lag(weights))))
apply.yearly(turnover.1, FUN=sum)*100

100 * (sum(wstar.GenSA * mu) - mean(mu))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
res="turnovers mit #MM_Tbremse

Dez 2001   0.91
Dez 2002 166.52
Dez 2003 143.09
Dez 2004  28.51
Dez 2005  10.08
Dez 2006   0.91
Dez 2007 166.52
Dez 2008 143.09
Dez 2009  28.51
Dez 2010  10.08


ohne bremse
Dez 2001 143.80
Dez 2002 353.36
Dez 2003 382.91
Dez 2004 405.36
Dez 2005 383.10
Dez 2006 143.80
Dez 2007 353.36
Dez 2008 382.91
Dez 2009 405.36
Dez 2010 383.10"
####################################################################
#Portfolio-Auswertung mit sysinvestor
#****************************************************************** 
dates = fromToS(na.omit(weights)) ##checken
colnames(weights)=tickers
data$prices=pricess
#****************************************************************** 
prices = data$prices  
n = len(tickers)  
head(prices)
head(weights)
# find month ends
month.ends = endpoints(prices, 'months')
month.ends = month.ends[month.ends > 0] 
month.ends = month.ends[-len(month.ends)]
period.annual.factor=12 
models = list()

fromTo(prices[month.ends])
fromTo(weights)
# Equal Weight
# Equal Weight

write.csv(weights,"mdata/weights_dax_gensa.csv")
write.csv(prices,"mdata/prices_dax_gensa.csv")

head(na.omit(weights))
month.ends = endpoints(na.omit(weights), 'months')
month.ends = month.ends[month.ends > 0] 

data$weight=na.omit(weights)
data$prices=prices[month.ends,]
fromTo(prices)
dates=fromToS(na.omit(weights))
data$prices = prices[fromToS(na.omit(weights))]
purePlot(mNorm(data$prices))
mean(abs(mNorm(data$prices)-lag(mNorm(data$prices))),na.rm=T)

####################################################################################
######################################################################################

d=data$prices[,"IFX.DE"]
d=d-lag(d)
plot(d)


purePlot(mNorm(data$prices[,28]))
amark("2008-07-21",col="blue")

purePlot(mNorm(data$prices[,20]))
amark("2008-09-29",col="blue")


data$weight[] = NA
data$weight[month.ends,] = 1/n
data$prices
models$equal.weight = bt.run.share(data, clean.signal=T, dates=dates)


data$weight=prices
data$weight[] = NA
data$weight[month.ends,]=weights    #Vorsicht:  checken !!!! nicht zukünftige 
#Gewichte auf vergangen Preise loslassen
fromTo(data$weight[month.ends,])
fromTo(weights)
models$maxcgar = bt.run.share(data, clean.signal=F, dates=dates)
k=compareViewModels(models,prices,alloc=T)


#sysReporting(models)
#signal =iif(dmSlope90 >0 & mSlope90 > 0,1,0)
#tradeResult(models,3, data, signal,frame=dates,reset=T)

##############################################################################################
###################################################################################
#MM_FeatureSelect
###################################################################################
##############################################################################################

#Versuche mit GenSA die relevante feature selection für ein pnn
#der GenSA soll einen eindim-parameter vom typ integer haben

#einlesen der bereitgestellten Trainings-date
train.data <<- read.csv("MData/TrainData.csv")  ;train.data=train.data[,-1]   
#Aufteilung in Target und Features
Target=train.data[,1]
Features=train.data[,-1]
##x sei eine 32 bit integer-Zahl, dann wandel die in ein binär-muster,
#interpretier dies als ein Gatter für 32 slots und gibt ein vector zurück
#der Gatternummern (selectierte Feature-spalten) die einer "1" entsprechen
}


fn.colSelect_<-function(x)
{
  x=round(x,digits=0)
  X=paste(as.integer(intToBits(x)), collapse="") #enthält eine 32-"01"-zeichenkette
  mP("...:%d: %s",x,X)
  
  feature.bit =sapply(str_extract_all(X,"*."),as.numeric)
  i=0;colSelect=c()
  for(feature.i in feature.bit)
  {
    i=i+1
    if (feature.i==1)
      colSelect=c(colSelect,i)
  }
  return(colSelect)
}

fn.colSelect<-cmpfun(fn.colSelect_) #compilier das Teil

#-------- die optimierungs-funktion erhält in x jeweils eine 32-bitzal
opti.features <-function(x)
{
  fn.call <<- fn.call + 1
  #browser(mP("opti.features"))
  #den Parameter auf integer runden
  x=round(x,digits=0)
  #wurde dieser Parameter schon ausprobiert ?
  found.i=which(oldPars==x)
  if (len(found.i)>0)
    return(oldRes[found.i]+0.001)  #extra-penality und nimm das Ergebnis aus dem cashe
  else
    oldPars<<-c(oldPars,x);
  
  colSelect = fn.colSelect(x) 
  #schalte jetzt alle train.data-Spalten aus, die nicht zum feature.selector x passen
  print(fn.call)
  print(colnames(Features)[colSelect])
  train.data.selected =cbind(Target,Features[colSelect])
  #browser()
  ############ trainiere jetzt pnn
  pnn <- pnn::learn(train.data.selected)
  pnn$set[1:shape(train.data.selected), ] #set, the training set.
  #pnn$category.column#  category.column, the column index of categories.
  
  #pnn$categories #categories, the list of found categories.
  #pnn$k #the number of variables.
  #pnn$n#n, the number of observations.
  #pnn <- pnn::smooth(pnn, sigma = 0.72)
  pnn <- pnn::smooth(pnn, sigma = 0.3106217)
  
  #We can now evaluate the performance of our Probabilistic neural network.
  pnn <- pnn::perf(pnn)
  #pnn$observed
  #pnn$guessed
  #pnn$success
  #pnn$fails
  #der Rückgabe-wert für den Optimierer .. penality:
  print(pnn$success_rate)
  penality = nval(1- pnn$success_rate)
  oldRes<<-c(oldRes,penality) #merke dir die gefundenen Ergbnisse
  return(penality )
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(stringr)
if (F) #zeigt den umgang mit binärzahlen
{
  paste(rev(as.integer(intToBits(5))), collapse="") 
  s=paste((as.integer(intToBits(5))), collapse="") 
  
  substr(s,5,5)
  sapply(str_extract_all(s,"*."),as.numeric)
  
  str_length(s)
  strtoi("000101", base = 2)
  
  #mischung aus newton und genetic, kann auch ganzahlige variable mit data.type.int=TRUE
  # http://sekhon.berkeley.edu/rgenoud/genoud.html  tip: http://stackoverflow.com/questions/3234935/non-linear-integer-programming
  # kann auch multicore !
  sag("genoud:",T)
}

if (F)
{
library(rgenoud)
n.features=dim(Features)[2]  #maximal 32 Parameter sind erlaubt
if (n.features > 32)
  sag("pnn-feature-selection allows maximal 32 features",T)
#die integer Zahl die einem bin-string aus n.features einsen entspricht:
maxI=strtoi(str_dup("1",nfeatures), base = 2)

lower=c(1)
upper=c(maxI)
fn.call <<- 0

#cl=makeCluster(detectCores()-1);print(cl)  #aktiviere das parallel-framework

#Problem:  die Parameter sollen reine integers sein
#optimizer aufruf, mit data.type.int=T:  der kann auch reine integers als parameter
#mist:  er endet nicht und testet immer wieder die gleichen parameter, gr=nextfun ruft er auch nicht auf...  ...

#init der globals .. muss vor jedem optimlauf gestartete werden
oldPars<<-c()
fn.call <<- 0
oldRes<<-c()

bestFeatures.code=
  genoud( fn=opti.features, nvars=length(upper),gr=nextfun,
          max.generations =50,hard.generation.limit=T,gradient.check=F,
          data.type.int=T, boundary.enforcement=2,
          Domains=cbind(lower,upper))
}

#nächster Versuch
#http://stackoverflow.com/questions/11110848/how-to-optimize-for-integer-parameters-and-other-discontinuous-parameter-space
#pso ? - particale swarm optimization
################################## optim mit method=SANN
nextfun <- function(x) 
{
  k=0
  mP("nextfun: ######")
  repeat
  {
    k=sample(1:maxI, 1, replace=TRUE)
    ist.k.neu=(len(which(oldPars==4))==0)  #neuer Wert
    if (ist.k.neu)
    {
      break
    }
  }
  print(k)
  return (k)
}  

if (F)
{
#init der globals .. muss vor jedem optimlauf gestartete werden
oldPars<<-c()
fn.call <<- 0
oldRes<<-c()
#... hier wird jetzt brav nextfun aufgerufen .. aber warum terminiert er nicht , manchmal hängt er...
res.opt=optim(fn=opti.features, par=c(1), gr=nextfun, method="SANN", 
              control=list(maxit=100,fnscale=1,trace=10))
#alternativ mit pso - particale swarm optimization .. ich schaffs nicht gr=nextfun aufzurufen
library(pso)
res.opt=psoptim(fn=opti.features, par=c(1),gr=nextfun, lower=1,upper=maxI,
                control=list(maxit=100,maxf=100,fnscale=1,trace=10,hybrid=TRUE))

# integer-params - problem .. ich schaffs nicht gr=nextfun aufzurufen ... 
#~~~~~~~~~~~~~~~ also opti.features um parameter -runden und oldRes-cashen erweitern und wieder mit GenSA probieren:
oldPars<<-c()
fn.call <<- 0
oldRes<<-c()
#~~~~~~~
out.GenSA <- GenSA(fn = opti.features, lower = rep(1), upper = rep( as.numeric(maxI)),     control = list(smooth = FALSE, max.call = 1000))

########## Anzeigen der als beste ausgewählten Features
bestFeatures = fn.colSelect(bestFeatures.code) 
bestFeatures.names = colnames(Features[,bestFeatures])
}
