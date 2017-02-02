#Geh nach #MM2

#MM_MAIN   Hauptprogramm für Eckhards-Allocator
#25.3.2013   gehe nach #MMEM
#    Hier bau ich mein Aktien/Renten - Allocationssystem

#mach voher mmBasicStartup.r
#Einführung in dei Lib des SysInvestors

if (!exists("MM_lesson1"))
{
  
  rm(list=ls())
  print("### load BasicLib ###")
  source("Mlib/InputConfig_Portfolio_TD.R")  #jetzt geht mP("xxx)
  source("MLib/TradeClasses.r")
  source("MLib/LoadDataFuncs.r") 
  source("MLib/portfolioCode.r") #hier wird der gini-teil vom sysinvest weggepatc
  
}

options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)

#source("MLib/SITpatches.r")  #mein Anpassungen dazu

if (F)  #falls reinit- gewünscht anklicken
  rm(MM_lesson1)


#-----------------------------------------------------------------------------------------------
ModelRoot = "O:/R/Nuggets/Eckhard" 
setwd(ModelRoot)

testXLSread<-F
readMyClasses<-T

modelDir="EM3_Maerz_25b"
dataSet = modelDir
##############################################################################

globalPROVIDER = "CSV"
csvDatFormat="%d.%m.%Y"
globalSEP = ";"
globalDEC = ","
globMDataSUB="eMod3"

lookback.len = 60
#periodicity = 'weeks'
periodicity = 'months'
prefix = ''

AllZenariosFname=sprintf("Mdata/%s/AllZenarios.rda",globMDataSUB)
myFrame=""
allmodels = list()
#########################################################
#Run Eckhards Zenario 
########################## ###############################
runEckhard<-function()
{
  parseEckhardDataXls = T
  
  
  #
  #NEU:  Die Fundamentaldaten mit den slopes werden separat nach  data$watch   geladen.
  #Dazu werden sie im xls-Sheet markiert: 
  #Sobald die Fundies kommen wir der ersten von ihnen mit einem vorgestellten "Watch" markiert.
  # "WATCH REX RI slope90"
  #in der entsprechenden  Universe.csv wird eine Zeile mit "############ WATCHLIST ###########"
  #als Markierung geschrieben.
  
  AllZenarios <<- parseEckhardsXlsZenarios (parseEckhardDataXls,AllZenariosFname)
  #Mache die übliche Zeitreihen -Vorverarbeitung (wochen-Tage ...)
  data <<- new.env()
  #liest die universe.csv  
  prepareAllZenarios(AllZenarios, data)
  ls(data)
  
  #head(data$ML_Euz_HY_Ri)
  #meine selbst gerechneten Steigungen
  
  mSlope90 <<- rollRegressionXTS(na.omit(mNorm(data$prices)),win=90)
  dmSlope90 <<-rollRegressionXTS(na.omit(mSlope90),win=30)
  
  rollddRegressionXTS<-function(Y,win=60)
  {
    dolm <- function(Y){(lm.fit(cbind(Intercept=1,index(Y)-index(first(Y))),coredata(Y)))}
    Y=data$prices[,"DAX30"]
    
    l=dolm(Y)
    x=forecast(Y,5)
    ls(x)
    str(x)
    len(x$mean)
    len(Y)
    xts()
    
    price = Y
    k=tail(Y,100)
    
    
    slope90=rollapplyr(Y, win, dolm, by.column = F)
    #pick Dir nur die Steigungen raus 
    ret=slope90[,seq(2,ncol(slope90),2)]
    colnames(ret)=  sapply(colnames(Y),function(x) sprintf("%s_slope%d",x,win))
    return(ret)
  }
  
  ############# InputAssumtions 
  ls(data)
  ia<<-create.ia.mm (data, "months")
  return(AllZenarios)
  #####################################################################################################
  
  
  
  if (F)  #schau dir an Rohdaten und Indikatoren in EC's  xls-File  liegen - sowie die selbst gerechneten Slopes
  {
    signal =iif(dmSlope90 >0 & mSlope90 > 0,1,0)
    equity = lag(sign(signal))*data$prices
    mPlot(na.omit(equity)[,c(1,2,3)],ylog_=F)
    
    pdf= sprintf("Models/%s/%s.pdf",dataSet,"Inputdaten.pdf");
    pdf(file = pdf, width=8.5, height=11) 
    xyplot(data$prices)
    xyplot(data$watch$prices)
    xyplot(mSlope90)
    dev.off()
    
    
    head(dmSlope90,100)
    
    norm_Win(3)
    for(i in c(1:3))
    {
      mPlot(mmerge(scaleTo(data$prices[,i],c(-1,1)), 
                   scaleTo(mSlope90[,i],c(-1,1)),
                   scaleTo(dmSlope90[,i],c(-1,1)),
                   scaleTo(signal[,i],c(-1,1)) ),
            ylog_=F)
      
    }
  }
  
  ########################################################################
  ########################################################################
  
  if (F) #nachbearbeitung der Rohdaten - insbesondere Trennung in normale Zeitreihen und slopes
  {
    
    dim(data$prices)
    data$prices = na.omit(data$prices);dim(data$prices)
    
    norm_Win(2)
    mPlot(data$prices[,1])
    mPlot(mSlope90["2000-05::2001",i],ylog_=F)
    mSlope90["2000-05::2001",i]
    xyplot(mSlope90)
    #slopes = data$symbolnames[grep('slope', data$symbolnames)]
    #ticker = data$symbolnames[-grep('slope', data$symbolnames)]
    
    #data$slopes = data$prices[,slopes]
    #data$prices = data$prices[,ticker]
    
    
    
    data$prices[which(data$prices==0)]
    data$prices[which(!is.finite(data$prices))]
    
    range(data$prices)
    ret= ROC(data$prices)
    ret = na.omit(ret)
    range(ret)
    head(data$prices)
    nprices =(mRendite(ret))
    mPlot(nprices)
    tickers = colnames(data$prices)
    
    xyplot(data$prices)
    xyplot(data$watch$prices)
    
    range(data$prices)
    
    getwd()
    
    zenario = AllZenarios[[1]]
    dataSet = sprintf("%s_%s",modelDir,zenario$zenario)
    dataSetFile = paste(dataSet,"/Data.Rdata",sep="")
    
    filename.pdf =paste(dataSet,"/Data_",zenario$zenarioDir,".pdf",sep="")
    
    ds = sprintf("Models/%s",dataSetFile)
    mP("Save data %s",ds)
    save(data,file=ds)
    
    
    ##schnell noch mal die Rohdaten in ein Pdf-file schreiben
    pdf= sprintf("Models/%s",filename.pdf);
    # put all reports into one pdf file
    mP("Write Data Charts to %s",pdf)
    pdf(file = pdf, width=8.5, height=11) 
    
    xyplot(data$prices)
    xyplot(data$watch$prices)
    
    #ret=mROC(data$prices)
    #MplotNormedPrices(ret )
    x=mPlot(mNorm(data$prices))
    plotCorrelations(data$prices)
    
    dev.off()  
    
  } 
  
  if (F)
  {
    periodicity="months";period.annual.factor=12
    
    models=list()
    #Berechne die unterschiedichen AllocationsModelle für jedes Zenario  und erstell die Reports
    experiment = "passive_month"
    #undebug(runAllocationZenario)
    prices=data$prices
    #purePlot(prices)
    strategie=runAllocationZenario(doLoad=F)  #hier werden die passiv-strategien gerechnet
    models = strategie$models
    allmodels = models
    
    #PASSICE-Ergebnisse!!
    # put all reports into one pdf file
    pdf =sprintf("Models/%s/%s.pdf", dataSet,"passive_month")
    mP("Write Data Charts to %s",pdf)
    pdf(file = pdf, width=8.5, height=11) 
    k=compareViewModels(models,data$prices,alloc=F)  #<<--- mein modell-Performance-zeiger
    custom.period.chart(models)  # .. zeigt monat+jahrs-Return als Balken an
    
    dev.off()
    #dolm <- function(x) coef(lm(USDZAR ~ ., data = as.data.frame(x))))
    
  }
  
  
  if (F)
  {
    #############################
    findFn("portfolio")
    
    library(fPortfolio)
    Data = SMALLCAP.RET[,c("BKE", "GG", "GYMB", "KRON")]
    
    Spec = portfolioSpec()  
    setTargetReturn(Spec) = mean(colMeans(Data))
    Constraints = "minW[1]=0.34"
    efficientPortfolio(Data, Spec, Constraints)
    #########################
    library("tawny")
    data(sp500.subset)
    h <- sp500.subset
    h <-(na.omit(ROC(data$prices)))
    
    mPlot(mNorm(data$prices))
    apply(h,2,hi[which(abs(hi)<0.00000001)]
    )
    
    h=zoo(head(r,160))
    ws.1=0
    # Optimize using a window of length 150
    # (there will be 51 total iterations)
    ws.1 <- optimizePortfolio(h, 150, getCorFilter.RMT() )
    
    # Plot the performance of the resulting optimized portfolio
    pf.1 <- plotPerformance(h, ws.1, len(ws.1), y.min=-0.4, 
                            name='RMT', bg='#fefefe', color='#87abc1')
    
    plot(mRendite(pf.1$daily.returns))
    
    ###########################
    # Generate weights based on the constant correlation shrinkage estimator
    ws.2 <- optimizePortfolio(h, 150, getCorFilter.Shrinkage())
    pf.2 <- plotPerformance(h, ws.2, 150, name='shrinkage',
                            color='#b89c5a', colors=pf.1$colors)
    
    # Compare the performance with a naive equal-weighted portfolio
    ef <- compare.EqualWeighted(h, 150, colors=pf.2$colors)
    # Also compare against the S&P 500
    mf <- compare.Market('^GSPC',200,150, end=end(h), colors=ef$colors)
  }
}
#Erstelle die Alloc-Reports
#  runAllocationZenarioReports()



#########################################################

if(F && readMyClasses)
{
  
  if (F)
  {
    frame=""
    ret=na.omit(diff(log(data$prices[frame])))
    dret = Drawdowns(ret)
    #ret[frame,2,drob=F]
    # identifikation der BENCH-Spalte
    benchId= which(colnames(data$prices)==BENCH)
    Bench =ret[,benchId]
    ############################################################
    if (showUniverse)
      UniverseOverview(ret,frame)
    #############################################################
    
    fromTo(data$prices)
    frame =""
    ret=na.omit(diff(log(data$prices[frame])))
    dret = Drawdowns(ret)
    head(dret)
    z= ret[frame]
    nDays <- length(z[,1])
    # seting learning and testing periods
    testPeriod <- 250
    learningPeriod <- 500
    
    testDates <- (nDays-testPeriod):nDays
    learningDates <- (nDays - testPeriod - learningPeriod):(nDays - testPeriod)
    
    z <- ret[learningDates,]
    zTest <- z[testDates,]
    save(data,file=dataSetFile)
    windows()   
    plot(data[["DAX30"]])
    head( data[["BANKS"]],300)
    head(data[["prices"]],3)
    head(data[["weight"]],3)
    
  } 
  #############################################################################################
  
}

###############################################################################
# Rotational Trading Strategies : ETF Sector Strategy
# http://www.etfscreen.com/sectorstrategy.php
# http://www.etfscreen.com/intlstrategy.php

#jetzt kommt eine ganze Folge von Portfolio-Experimenten:  suche 
# nach dem Wort experiment  


###############################################################################
bt.rotational.trading.test <- function() 
{
  #*****************************************************************
  # Load historical data
  #****************************************************************** 
  if (F)
  {
    load.packages('quantmod')  
    tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU,IWB,IWD,IWF,IWM,IWN,IWO,IWP,IWR,IWS,IWV,IWW,IWZ')  
    
    data <- new.env()
    getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
    for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
    bt.prep(data, align='keep.all', dates='1970::')
  }
  #*****************************************************************
  # Code Strategies
  #****************************************************************** 
  prices = data$prices  
  n = len(tickers)  
  
  # find month ends
  month.ends = endpoints(prices, 'months')
  month.ends = month.ends[month.ends > 0]		
  period.annual.factor=12 
  
  models = list()
  
  #*****************************************************************
  # Code Strategies
  #****************************************************************** 
  dates = '2001::'
  
  # Equal Weight
  data$weight[] = NA
  data$weight[month.ends,] = ntop(prices, n)[month.ends,]	
  models$equal.weight = bt.run.share(data, clean.signal=F, dates=dates)
  
  
  # Rank on 6 month return
  position.score = prices / mlag(prices, 126)	
  #position.score = prices / mlag(prices, 200)   
  # Select Top 2 funds
  data$weight[] = NA
  data$weight[month.ends,] = ntop(position.score[month.ends,], 2)	
  models$top2 = bt.run.share(data, trade.summary=T, dates=dates)
  
  # Seletop Top 2 funds,  and Keep then till they are in 1:6 rank
  data$weight[] = NA
  data$weight[month.ends,] = ntop.keep(position.score[month.ends,], 2, 6)	
  models$top2.keep6 = bt.run.share(data, trade.summary=T, dates=dates)
  
  #*****************************************************************
  # Create Report
  #****************************************************************** 
  allmodels=append(allmodels,models)
  
  k=compareViewModels(models,prices,alloc=T)
  #sysReporting(models)
  tradeResult(models,2, data, signal,frame="2007::2009",reset=T)  #mein komplexer All-In-OnePlot: zeigt die Zeitreihen, ihre Gewichte und auf als Farbänderung auf der Linie auch noch long/short/flat - sowie die GUV Kennzahlen  und dunkel hinterlegt die PortfolioEquity-Kurve
  
  #######################################################################################################################
  #anderes scoring
  #######################################################################################################################
  
  experiment = "ntop month on MdoubleSlope90"
  prices = data$prices  
  n = len(tickers)  
  
  # find month ends
  month.ends = endpoints(prices, 'months')
  month.ends = month.ends[month.ends > 0]  	
  period.annual.factor=12 
  
  models = list()
  
  #*****************************************************************
  # Code Strategies
  #****************************************************************** 
  dates = '2001::'
  
  # Equal Weight
  data$weight[] = NA
  data$weight[month.ends,] = ntop(prices, n)[month.ends,]	
  models$equal.weight = bt.run.share(data, clean.signal=F, dates=dates)
  
  
  # Rank on 6 month return
  #position.score = prices / mlag(prices, 126)	
  
  signal =iif(dmSlope90 >0 & mSlope90 > 0,1,0.001)
  position.score =lag(signal)*mSlope90*10000;
  
  #mPlot(position.score,ylog_=F)
  
  # Select Top 2 funds
  data$weight[] = NA
  
  month.ends = endpoints(position.score, 'months')
  month.ends = month.ends[month.ends > 0]    
  
  data$weight[month.ends,] = ntop(position.score[month.ends,], 2)	
  models$top2 = bt.run.share(data, trade.summary=T, dates=dates)
  
  # Seletop Top 2 funds,  and Keep then till they are in 1:6 rank
  data$weight[] = NA
  data$weight[month.ends,] = ntop.keep(position.score[month.ends,], 2, 6)	
  models$top2.keep6 = bt.run.share(data, trade.summary=T, dates=dates)
  
  # Seletop Top 2 funds,  and Keep then till they are in 1:6 rank
  data$weight[] = NA
  data$weight[month.ends,] = ntop.keep(position.score[month.ends,], 7, 6)  
  models$top7.keep6 = bt.run.share(data, trade.summary=T, dates=dates)
  
  #*****************************************************************
  # Create Report
  #****************************************************************** 
  allmodels=append(allmodels,models)
  
  k=compareViewModels(models,prices,alloc=T)
  #sysReporting(models)
  signal =iif(dmSlope90 >0 & mSlope90 > 0,1,0)
  tradeResult(models,3, data, signal,frame="2007::2009",reset=T)
  
  pdf_Report(experiment)
  dev.off()
  
  getTrades(models$top2.keep6)
  model$trade.summary$trades
  
  #######################################################################################################################
  #scoring auf ek
  #######################################################################################################################
  
  experiment = "ntop month on ekSlope90"
  prices = data$prices  
  n = len(tickers)  
  
  # find month ends
  month.ends = endpoints(prices, 'months')
  month.ends = month.ends[month.ends > 0]    
  period.annual.factor=12 
  
  models = list()
  
  #*****************************************************************
  # Code Strategies
  #****************************************************************** 
  dates = '2001::'
  
  # Equal Weight
  data$weight[] = NA
  data$weight[month.ends,] = ntop(prices, n)[month.ends,]	
  models$equal.weight = bt.run.share(data, clean.signal=F, dates=dates)
  
  
  # Rank on 6 month return
  position.score = prices / mlag(prices, 126)	
  ##signal =iif(dmSlope90 >0 & mSlope90 > 0,1,0.001)
  position.score =lag(signal)*mSlope90*10000;
  
  position.score = data$watch$prices 
  colnames(position.score) = colnames(data$weight)
  
  # Select Top 2 funds
  data$weight[] = NA
  
  month.ends = endpoints(position.score, 'months')
  month.ends = month.ends[month.ends > 0]    
  
  data$weight[month.ends,] = ntop(position.score[month.ends,], 2)	
  models$top2 = bt.run.share(data, trade.summary=T, dates=dates)
  
  # Seletop Top 2 funds,  and Keep then till they are in 1:6 rank
  data$weight[] = NA
  data$weight[month.ends,] = ntop.keep(position.score[month.ends,], 2, 6)	
  models$top2.keep6 = bt.run.share(data, trade.summary=T, dates=dates)
  
  #models=list()
  data$weight[] = NA
  data$weight[month.ends,] = ntop.keep(position.score[month.ends,], 7, 7)  
  models$top7.keep6 = bt.run.share(data, trade.summary=T, dates=dates)
  
  #*****************************************************************
  # Create Report
  #****************************************************************** 
  allmodels=append(allmodels,models)
  
  k=compareViewModels(models,prices,alloc=T)
  #sysReporting(models)
  signal =iif(dmSlope90 >0 & mSlope90 > 0,1,0)
  tradeResult(models,3, data, signal,frame="2007::2009",reset=T)
  
  pdf_Report(experiment)
  dev.off()
  
  #######################################################################################################################
  #scoring auf ek weekly - keine besserung
  #######################################################################################################################
  
  experiment = "ntop week on ekSlope90"
  prices = data$prices  
  n = len(tickers)  
  
  # find month ends
  month.ends = endpoints(prices, 'weeks')
  month.ends = month.ends[month.ends > 0]    
  period.annual.factor=52
  
  models = list()
  
  #*****************************************************************
  # Code Strategies
  #****************************************************************** 
  dates = '2001::'
  
  # Equal Weight
  data$weight[] = NA
  data$weight[month.ends,] = ntop(prices, n)[month.ends,]  
  models$equal.weight = bt.run.share(data, clean.signal=F, dates=dates)
  
  
  # Rank on 6 month return
  #position.score = prices / mlag(prices, 126)	
  #signal =iif(dmSlope90 >0 & mSlope90 > 0,1,0.001)
  #position.score =lag(signal)*mSlope90*10000;
  
  position.score = data$watch$prices 
  colnames(position.score) = colnames(data$weight)
  
  #*****************************************************************
  # MA cross-over strategy   - bringt keine BESSERUNG
  #****************************************************************** 
  if (F)
  {
    sma.fast = bt.apply.matrix(prices,SMA, 2)
    sma.slow = bt.apply.matrix(prices, SMA, 200)
    signal = iif(sma.fast >= sma.slow, 1, 0)
    position.score=signal
  }
  #position.score=signal
  
  #mPlot(position.score,ylog_=F)
  
  # Select Top 2 funds
  data$weight[] = NA
  
  month.ends = endpoints(position.score, 'weeks')
  month.ends = month.ends[month.ends > 0]    
  
  data$weight[month.ends,] = ntop(position.score[month.ends,], 2)	
  models$top2 = bt.run.share(data, trade.summary=T, dates=dates)
  
  # Seletop Top 2 funds,  and Keep then till they are in 1:6 rank
  data$weight[] = NA
  data$weight[month.ends,] = ntop.keep(position.score[month.ends,], 2, 6)	
  models$top2.keep6 = bt.run.share(data, trade.summary=T, dates=dates)
  
  
  data$weight[] = NA
  data$weight[month.ends,] = ntop.keep(position.score[month.ends,], 6, 6)  
  models$top7.keep6 = bt.run.share(data, trade.summary=T, dates=dates)
  
  ################# auf share- statt auf weight - basis ....
  
  # Select Top 2 funds 
  data$weight[] = NA
  data$weight[month.ends,] = ntop(position.score[month.ends,], 2)  
  capital = 100000 
  data$weight[] = (capital / prices) * bt.exrem(data$weight)       
  models$top2share = bt.run(data, type='share', trade.summary=T) 
  
  # Seletop Top 2 funds,  and Keep then till they are in 1:6 rank 
  data$weight[] = NA
  data$weight[month.ends,] = ntop.keep(position.score[month.ends,], 2, 6)  
  capital = 100000 
  data$weight[] = (capital / prices) * bt.exrem(data$weight)       
  models$top2.keep6share = bt.run(data, type='share', trade.summary=T) 
  
  
  
  #*****************************************************************
  # Create Report
  #****************************************************************** 
  allmodels=append(allmodels,models)
  
  k=compareViewModels(models,prices,alloc=T)
  #sysReporting(models)
  signal =iif(dmSlope90 >0 & mSlope90 > 0,1,0)
  tradeResult(models,3, data, signal,frame="2007::2009",reset=T)
  pdf_Report(experiment)
  dev.off()
  
  
  
  ###############################################################################
  # Additional example for Permanent Portfolio
  # that employs:
  # * risk allocation
  # * volatility targeting
  # * makret filter (10 month SMA)
  # to improve strategy perfromance
  ###############################################################################
  #  bt.permanent.portfolio2.test <- function() 
  
  if (F)
  {
    #*****************************************************************
    # Load historical data
    #****************************************************************** 
    load.packages('quantmod')	
    tickers = spl('SPY,TLT,GLD,SHY')
    
    data <- new.env()
    getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
    for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
    
    # extend GLD with Gold.PM - London Gold afternoon fixing prices
    data$GLD = extend.GLD(data$GLD)
    
    bt.prep(data, align='remove.na')
  }   
  #*****************************************************************
  # Setup
  #****************************************************************** 		
  experiment = "bt.permanent.portfolio2.test com01"  
  prices = data$prices   
  n = ncol(prices)
  
  period.ends = endpoints(prices, 'quarters')
  period.ends = period.ends[period.ends > 0]		
  period.ends = c(1, period.ends)
  period.annual.factor=4
  
  # assuming 10c a share commissions
  commission =0.1
  
  models = list()
  
  
  #*****************************************************************
  # Dollar Weighted
  #****************************************************************** 			
  target.allocation = matrix(rep(1/n,n), nrow=1)
  weight.dollar = ntop(prices, n)
  
  data$weight[] = NA
  data$weight[period.ends,] = weight.dollar[period.ends,]
  models$dollar = bt.run.share(data,commission=commission, clean.signal=F)
  
  #*****************************************************************
  # Dollar Weighted + 7% target volatility
  #****************************************************************** 				
  data$weight[] = NA
  data$weight[period.ends,] = target.vol.strategy(models$dollar,
                                                  weight.dollar, 7/100, 21, 100/100)[period.ends,]
  models$dollar.target7 = bt.run.share(data, commission=commission,clean.signal=F)
  
  #*****************************************************************
  # Risk Weighted
  #****************************************************************** 				
  ret.log = bt.apply.matrix(prices, ROC, type='continuous')
  hist.vol = sqrt(252) * bt.apply.matrix(ret.log, runSD, n = 21)	
  weight.risk = weight.dollar / hist.vol
  weight.risk = weight.risk / rowSums(weight.risk)
  
  data$weight[] = NA
  data$weight[period.ends,] = weight.risk[period.ends,]
  models$risk = bt.run.share(data, commission=commission, clean.signal=F)
  
  # risk weighted + 7% target volatility
  data$weight[] = NA
  data$weight[period.ends,] = target.vol.strategy(models$risk,
                                                  weight.risk, 7/100, 21, 100/100)[period.ends,]
  models$risk.target7 = bt.run.share(data, commission=commission,clean.signal=F)
  
  # risk weighted + 5% target volatility
  data$weight[] = NA
  data$weight[period.ends,] = target.vol.strategy(models$risk,
                                                  weight.risk, 5/100, 21, 100/100)[period.ends,]
  models$risk.target5 = bt.run.share(data, commission=commission,clean.signal=F)
  #*****************************************************************
  # Market Filter (tactical): 10 month moving average
  #****************************************************************** 				
  period.ends = endpoints(prices, 'months')
  period.ends = period.ends[period.ends > 0]		
  period.ends = c(1, period.ends)
  
  sma = bt.apply.matrix(prices, SMA, 200)
  weight.dollar.tactical = weight.dollar * (prices > sma)	
  
  data$weight[] = NA
  data$weight[period.ends,] = weight.dollar.tactical[period.ends,]
  models$dollar.tactical = bt.run.share(data, commission=commission, clean.signal=F)
  
  #*****************************************************************
  # Tactical + 7% target volatility
  #****************************************************************** 				
  data$weight[] = NA
  data$weight[period.ends,] = target.vol.strategy(models$dollar.tactical,
                                                  weight.dollar.tactical, 7/100, 21, 100/100)[period.ends,]
  models$dollar.tactical.target7 = bt.run.share(data,  commission=commission,clean.signal=F)
  
  
  #*****************************************************************
  # Create Report
  #****************************************************************** 
  allmodels=append(allmodels,models)
  
  k=compareViewModels(models,prices,alloc=T)
  #sysReporting(models)
  ls(models)
  #signal =iif(dmSlope90 >0 & mSlope90 > 0,1,0)
  #tradeResult(models,3, data, signal,frame="2007::2009",reset=T)
  pdf_Report(experiment)
  dev.off()
  
  #############################################################################
  # maximum SharpeRatio
  #############################################################################
  #*****************************************************************
  # Code Strategies
  #******************************************************************
  experiment = "maximum SharpeRatio"  
  prices = data$prices  
  n = ncol(prices)
  
  models = list()
  
  #*****************************************************************
  # Code Strategies  - die Ergebnisse hier sind schlechter als bei 
  # experiment = "passive_month"  -  alle haben etwas weniger return wie
  #equal weight und dafür etas geringeren MaxDD .. oft nur 2% besser
  #******************************************************************
  # find period ends
  period.ends = endpoints(prices, 'months')
  period.ends = period.ends[period.ends > 0]
  
  n.mom = 180
  n.vol = 60
  n.top = 4        
  momentum = prices / mlag(prices, n.mom)  
  #MMOK
  obj = portfolio.allocation.helper(data$prices, period.ends=period.ends,
                                    lookback.len = n.vol, universe = ntop(momentum[period.ends,], n.top) > 0,
                                    min.risk.fns = list(EW=equal.weight.portfolio,
                                                        RP=risk.parity.portfolio,
                                                        MV=min.var.portfolio,
                                                        MD=max.div.portfolio,
                                                        MC=min.corr.portfolio,
                                                        MC2=min.corr2.portfolio,
                                                        MCE=min.corr.excel.portfolio,
                                                        MS=max.sharpe.portfolio())
  ) 
  
  models = create.strategies(obj, data)$models
  
  #*****************************************************************
  # Create Report
  #******************************************************************    
  strategy.performance.snapshoot(models, T)
  plotbt.custom.report.part2(models$MS)
  # Plot Portfolio Turnover for each strategy
  layout(1)
  barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
  
  
  allmodels=append(allmodels,models)
  
  k=compareViewModels(models,prices,alloc=F)
  #sysReporting(models)
  ls(models)
  #signal =iif(dmSlope90 >0 & mSlope90 > 0,1,0)
  #tradeResult(models,3, data, signal,frame="2007::2009",reset=T)
  pdf_Report(experiment)
  dev.off()
  
  
}

################################# Aufgabe:  Verbinde nTop mit AllocaAnsätzen:
#idee:  setze  via ntop ausgefilterte Gewichte auf NA .. dann sollten sie auch nicht allociert werden.
################################# Aufgabe:  Verbinde nTop mit AllocaAnsätzen:

bt.aaa.test <- function() #MM1
{
  #*****************************************************************
  # Load historical data
  #****************************************************************** 
  load.packages('quantmod')
  
  tickers = spl('SPY,EFA,EWJ,EEM,IYR,RWX,IEF,TLT,DBC,GLD')
  
  data <- new.env()
  getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
  
  
  # contruct another back-test enviroment with split-adjusted prices, do not include dividends
  # http://www.fintools.com/wp-content/uploads/2012/02/DividendAdjustedStockPrices.pdf
  # http://www.pstat.ucsb.edu/research/papers/momentum.pdf
  data.price <- new.env()
  for(i in ls(data)) data.price[[i]] = adjustOHLC(data[[i]], symbol.name=i, adjust='split', use.Adjusted=F)
  bt.prep(data.price, align='keep.all', dates='2004:12::')	
  
  
  # create split and dividend adjusted prices
  for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)							
  bt.prep(data, align='keep.all', dates='2004:12::')
  
  
  # flag to indicate whether to use Total(split and dividend adjusted) or Price(split adjusted) prices
  use.total = FALSE
  
  
  #*****************************************************************
  # Sample Plot of Total and Price only time series
  #******************************************************************	 
  if(F) {
    y = data$prices$TLT
    y.price = data.price$prices$TLT
    y = y / as.double(y[1])
    y.price = y.price / as.double(y.price[1])
    
    plota(y, type='l', ylim=range(y, y.price, na.rm=T))
    plota.lines(y.price, col='red')
    plota.legend('Total,Price', 'black,red')
  }			
  
  
  #*****************************************************************
  # Code Strategies
  #******************************************************************
  prices = data$prices      
  n = ncol(prices)
  
  
  prices4mom = iif(use.total, data$prices, data.price$prices)
  prices4vol = iif(use.total, data$prices, data.price$prices)    
  
  models = list()
  
  # find period ends
  period.ends = endpoints(prices, 'months')
  period.ends = period.ends[period.ends > 0]
  # Adaptive Asset Allocation parameters
  n.top = 5		# number of momentum positions
  n.mom = 6*22	# length of momentum look back
  n.vol = 1*22 	# length of volatility look back
  
  #*****************************************************************
  # Equal Weight
  #******************************************************************
  data$weight[] = NA
  data$weight[period.ends,] = ntop(prices[period.ends,], n)   
  models$equal.weight = bt.run.share(data, clean.signal=F)
  
  #*****************************************************************
  # Volatliliy Position Sizing
  #******************************************************************
  ret.log = bt.apply.matrix(prices4vol, ROC, type='continuous')
  hist.vol = bt.apply.matrix(ret.log, runSD, n = n.vol)
  
  adj.vol = 1/hist.vol[period.ends,]
  
  data$weight[] = NA
  data$weight[period.ends,] = adj.vol / rowSums(adj.vol, na.rm=T)    
  models$volatility.weighted = bt.run.share(data, clean.signal=F)
  
  #*****************************************************************
  # Momentum Portfolio
  #*****************************************************************
  momentum = prices4mom / mlag(prices4mom, n.mom)
  
  data$weight[] = NA
  data$weight[period.ends,] = ntop(momentum[period.ends,], n.top)   
  models$momentum = bt.run.share(data, clean.signal=F)
  
  #*****************************************************************
  # Combo: weight positions in the Momentum Portfolio according to Volatliliy
  #*****************************************************************
  weight = ntop(momentum[period.ends,], n.top) * adj.vol
  
  data$weight[] = NA
  data$weight[period.ends,] = weight / rowSums(weight, na.rm=T)   
  models$combo = bt.run.share(data, clean.signal=F,trade.summary = TRUE)
  
  #*****************************************************************   
  # Adaptive Asset Allocation (AAA)
  # weight positions in the Momentum Portfolio according to 
  # the minimum variance algorithm
  #*****************************************************************   
  weight = NA * prices
  weight[period.ends,] = ntop(momentum[period.ends,], n.top)
  
  for( i in period.ends[period.ends >= n.mom] ) {
    hist = ret.log[ (i - n.vol + 1):i, ]
    
    # require all assets to have full price history
    include.index = count(hist)== n.vol      
    
    # also only consider assets in the Momentum Portfolio
    index = ( weight[i,] > 0 ) & include.index
    n = sum(index)
    
    if(n > 0) {					
      hist = hist[ , index]
      
      # create historical input assumptions
      ia = create.ia(hist)
      s0 = apply(coredata(hist),2,sd)       
      ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
      
      # create constraints: 0<=x<=1, sum(x) = 1
      constraints = new.constraints(n, lb = 0, ub = 1)
      constraints = add.constraints(rep(1, n), 1, type = '=', constraints)       
      
      # compute minimum variance weights				            
      weight[i,] = 0        
      weight[i,index] = min.risk.portfolio(ia, constraints)
    }
  }
  
  # Adaptive Asset Allocation (AAA)
  data$weight[] = NA
  data$weight[period.ends,] = weight[period.ends,]   
  models$aaa = bt.run.share(data, clean.signal=F,trade.summary = TRUE)
  
  
  #*****************************************************************
  # Create Report
  #******************************************************************    
  #pdf(file = 'report.pdf', width=8.5, height=11)
  
  models = rev(models)
  
  png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
  plotbt.custom.report.part1(models)       
  dev.off()		
  
  png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')		               
  plotbt.custom.report.part2(models)       
  dev.off()		
  
  png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
  plotbt.custom.report.part3(models$combo, trade.summary = TRUE)       
  dev.off()		
  
  png(filename = 'plot4.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
  plotbt.custom.report.part3(models$aaa, trade.summary = TRUE)       
  dev.off()               
  clr()
  
}


###############
if (F)
{
  load.packages('quantmod')
  
  tickers = spl('SPY,EFA,EWJ,EEM,IYR,RWX,IEF,TLT,DBC,GLD')
  
  data <- new.env()
  getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
  for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)  						
  bt.prep(data, align='keep.all', dates='2004:12::')
}

if (F)
{
  #*****************************************************************
  # Load historical data for Futures and Forex
  #****************************************************************** 
  data <- new.env()
  #'https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/FinancialInstrument/inst/parser/download.tblox.R?root=blotter'
  getSymbols.TB(env = data, auto.assign = T, download = T)
  
  ls(data)
  bt.prep(data, align='remove.na', dates='1990::')
  save(data,file='FuturesForex.Rdata')
  fromTo(data$prices)
}

if (F)
{
  #*****************************************************************
  # Load historical data for nasdaq 100 stocks
  #****************************************************************** 
  load.packages('quantmod,quadprog')
  #tickers = nasdaq.100.components()
  tickers = spl('ATVI,ADBE,ALTR,AMZN,AMGN,APOL,AAPL,AMAT,ADSK,ADP,BBBY,BIIB,BMC,BRCM,CHRW,CA,CELG,CEPH,CERN,CHKP,CTAS,CSCO,CTXS,CTSH,CMCSA,COST,DELL,XRAY,DISH,EBAY,ERTS,EXPD,ESRX,FAST,FISV,FLEX,FLIR,FWLT,GILD,HSIC,HOLX,INFY,INTC,INTU,JBHT,KLAC,LRCX,LIFE,LLTC,LOGI,MAT,MXIM,MCHP,MSFT,MYL,NTAP,NWSA,NVDA,ORLY,ORCL,PCAR,PDCO,PAYX,PCLN,QGEN,QCOM,RIMM,ROST,SNDK,SIAL,SPLS,SBUX,SRCL,SYMC,TEVA,URBN,VRSN,VRTX,VOD,XLNX,YHOO')
  
  data <- new.env()
  for(i in tickers) {
    try(getSymbols(i, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T), TRUE)
    data[[i]] = try(adjustOHLC(data[[i]], use.Adjusted=T)  	)					
  }
}
if (F)
{
  load.packages('quantmod')  
  tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU,IWB,IWD,IWF,IWM,IWN,IWO,IWP,IWR,IWS,IWV,IWW,IWZ')	
  
  data <- new.env()
  getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
  for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
  bt.prep(data, align='keep.all', dates='1970::')
}



#data$symbolnames=NULL
#data$prices=NULL
#######################################################################################################################
#scoring auf ek  mit AA verbinden  #MMEM
#######################################################################################################################


#technische Stops (z.B.  unteres BBand durchstoßen ) - führen dazu, dass in der weight-
#Matrix das Gewicht auf 0 geht .. oder zu dem Asset mit geringsten Vola wandert.
#MMEM2


#nur zum Test:  ein KristallKugel techStop kennt das Target...

if (!exists("global_Targets") )  #TEST
{
  source("mLib/Now.R")
  global_Targets <<-compute.Targets(data$prices,0.16,0.05)
}


techStops<-function(price)
{  
  #browser()
  price = na.omit(price)
  bb=  BBands(HLC(price),n=150,maType = SMA)
  z<-Cl(price)
  z=ZLEMA(Lo(price),n=5)
  sma=SMA(Cl(price),n=90)
  plot(sma)
  #cudCl = runSum(ifelse(mROC(Cl(price)) > 0,1,-1),n=50)
  rL=runLengthEnc2(iif(mROC(Cl(price))>=0,1,-1))
  
  #plot(runMean(na.omit(cudCl),n=20))
  #lines(scaleTo(Cl(price),c(-2,3)),col="blue")
  sz = z-sma
#  sz=EMA(sz,n=2)
  cudLos = iif( (sz < 0  & rL >3 ) | sz >=0, 1,0)  
#  & cudCl<0
  #purePlot(merge(z,bb[,"dn"]))
  Tstops= iif(sz <0  ,0 ,1) #z < bb[,"dn"] || cudCl <= -3
  #print(head(Tstops))   
  Tstops=EMA(Tstops ,n=100)
  Tstops = iif(Tstops <0.5 | rL< (-4) | z < bb[,"dn"],0, 1) #  & cudLos==0 | rL < (-3)
#Lo(price) < bb[,"dn"] |
  return(Tstops)
  #1 heisst long, 0 heisst stop
}
if (F) #Blick auf die Performance des Stop-Systems
{
#ls(data)
#price = data[["EMBI"]]
#  signal=techStops(data$DAX)
  Tstops <<- lag(m.apply(data, function(x) {techStops(x)}))
#Tstops = global_Targets #TEST
pdf= sprintf("Models/%s/%s.pdf",dataSet,"mWorld_Inputdaten.pdf");
pdf(file = pdf, width=11, height=8.5) 
layout(2)
tail(na.omit(data$DAX))
plot(data$DAX) 
global_xMarker<<-list()
for (i in c(1:len(data$symbolnames)))
  {
    pi=na.omit(data$prices[,i])
    vt=fromTo(pi)
    tsD=sprintf("%s::%s",vt[1],vt[2])
  plotSigPrice(signal=iif(na.omit(Tstops[tsD,i])==0,0,1),prices=mNorm(pi),indi=NULL)#merge(bb[,"dn"],bb[,"up"],z),cudLos)  )
  }
dev.off()

}

if (F)
  {
  Mworld <<- T2$new(PortfolioName = "Mworld", bench="Dax",visual = T, online=T )
  data<<-Mworld$t0data
  
  SektorSpdrUSA<<- T2$new(PortfolioName = "SektorSpdrUSA", bench="Dax",visual = T, online=T )
  data<-SektorSpdrUSA$t0data
  
  
  Mworld2 <<- T2$new(PortfolioName = "Mworld2", bench="Dax",visual = T, online=T )
  data<-Mworld2$t0data
  
  
  Meurope <<- T2$new(PortfolioName = "Meurope", bench="Dax",visual = T, online=T )
  data<-Meurope$t0data
  
  
  data$symbolnames
  
  
  frame="2001::"
  #das gleiche environment wie data- aber beschränkt auf frame
  env.data=data.apply(data, function(x) x[frame,])  #ein auf frame geschrinkter Datenbereich
  df = m.apply(data,function(x) Hi(x)-Cl(x))
  x=m.apply(data, function(x) {print(colnames(x));Hi(x)-Cl(x)}, ignore=c("Zinsen"))
  
  lapply(data$symbolnames, FUN=function(symb) {
         print(symb)
  colnames(data[[symb]])=
    lapply(colnames(data[[symb]]),
           FUN= function(x) paste(sapply(strsplit(x,"\\."),trim),collapse=".") )})
  
  colnames(data$prices)
         
  
  colnames((data[["SX8BP"]]))
  
  ls(data)
  
  data.rm(data,"Gold") 
  data.Info(data,"Test")
  #infos zum Download
  
  data = dataf
  pdata$prices
  
  colnames(data$prices)

}

#MMEM2
if (F)
  {
  em_AA(data, period = "months",n.top=7, k.top=10, version="_MW_stopT", AAignoreStopped=F, useStops=T)
  
  em_AA(data, period = "months",n.top=7, k.top=10, version="_MW_stopF", AAignoreStopped=T, useStops=T)
}

#purePlot(mNorm(data$prices))
#ls(data)
##################################################################
#AssetAllocation
# monatlich, oder wöchentlich:

#Ranking-Indikator berechen -> scoring
#TechStop berechne -> TStop (0,1) , scoring wird zu scoring*TStop
# (( universe = ntop.keep(position.score[period.ends,], n.top, k.top) ))
#- ntop keep k - Selektion an hand des scoring > Universe 
#Die Gewichte für den kommenden Monat kommen jetzt ausschließlich aus
#dem jeweiligen AA-Algorithmus  (zum. max.Sharpe....)  portfolio.allocation.helpe()
#Wende jetzt noch mal die TStop auf die Tages-Gewichte an.
#
#Berechen aus Preisen und Gewichten die Equity ...
#models = create.strategies(obj, data)$models
#Reporte die Ergebnisse .. evtl. auch nach xls - inckls. Transaktionen

##################################################################

em_AA<-function(data, period = "months",n.top=6, k.top=6,version="", AAignoreStopped=T,useStops=T,frame="2001::")
{
  browser()
  tickers =data$symbolnames
  n = len(tickers)  #dim(data$prices[2])
  if (n.top < 1) n.top=n
  if (k.top < 1) k.top=n
  if (k.top < n.top) k.top = n.top
  
  #browser()
  dates = frame
  data$prices = data$prices[dates] 
  prices = data$prices  
  
  n.vol = 60
  #n.top = 7        
  n.mom = 180
  momentum = prices / mlag(prices, n.mom)  
  
  
  mSlope90 <<- rollRegressionXTS(mNorm(data$prices),win=90)
  dmSlope90 <<-rollRegressionXTS(na.omit(mSlope90),win=30)
  
  # Rank on 6 month return
  if (F)
  {  
    position.score = momentum
    experiment="AA ntop month on mom"
  } 
  
  #browser()  
  if (T)
  {
    signal =iif( mSlope90 > 0,1, 0.00000001)
    position.score =lag(signal)*mSlope90*10000;
    
    # position.score = lag(TSIsymbol(data))    *lag(signal)*mSlope90*10000
    experiment="AA mSlope90"
    
  }
  
  if (F)
  {
    ret = mROC(prices)
    #signal<-merge(apply.rolling(ret[,1],FUN="Omega",width=25),apply.rolling(ret[,2],FUN="Omega",width=25))
    omega <- rollapplyr(ret, width=25, FUN="Omega", by.column=T, align = "right")
    omega[!is.finite(omega)]<-0
    #omega[omega < 1.5] <-0 # #zusätzliche Glättung
    #omega[omega > 20]<-20
    
    #lag the data by 1
    signal<-lag(omega,k=1)  #ohne das wirds super "vorhersehend" ?????
    position.score = signal
    
    if (F)
    {
      data$weight[] = NA
      data$weight[] = signal
      dim(signal)
      m.omega = bt.run(data, trade.summary=T)
      plot(m.omega$equity)
    }
    # position.score = lag(TSIsymbol(data))    *lag(signal)*mSlope90*10000
    experiment="AA mSlope90"
    
  }
  
  
  
  if (F)
  {
    signal =iif(dmSlope90 >0 & mSlope90 > 0,1,0.001)
    
    position.score =lag(signal)*mSlope90*10000;
    experiment="AA ntop month on mDoubleSlope90"
  }
  
  if (F)
  {
    position.score = data$watch$prices 
    experiment = "AA ntop month on ekSlope90"
  }
  
  
  if (F)  #SUPER  MMEXPOSURE
  {
    #*****************************************************************
    # Possible Improvements to reduce drawdowns
    #****************************************************************** 
    # Equal Weight
    data$prices=prices
    data$weight = prices
    data$weight[] = ntop(prices, n)
    ew = bt.run(data)  
    # Avoiding severe draw downs
    # http://engineering-returns.com/2010/07/26/rotational-trading-system/
    # Only trade the system when the index is either above the 200 MA or 30 MA
    # Usually these severe draw downs  happen bellow the 200MA average and 
    # the second 30 MA average will help to get in when the recovery happens  
    buy.rule = (ew$equity > SMA(ew$equity,200)) | (ew$equity > SMA(ew$equity,10))
    #buy.rule = (ew$equity > SMA(ew$equity,200))
    buy.rule = ifna(buy.rule, F)
    
    buy.rule = lag(buy.rule)
    
    # Rank using TSI by Frank Hassler, TSI is already smoothed and slow varying, 
    # so SMA will filter will not very effective
    #http://engineering-returns.com/tsi/
    
    position.score = lag(TSIsymbol(data))
    #position.score = bt.apply(data, function(x)  TSI(HLC(x)) )    
    tail(position.score)
    
    
    #position.score.ma = position.score
    #position.score["2009-03"]
    #position.score["2009-2::2009-5",]=NA
    buy.rule["2008-02"]
    position.score[!buy.rule] = NA #klappt nicht
    #du musst schreiben:
    
    position.score[as.Date(index(buy.rule[buy.rule==F])),] = NA
    position.score["2008-02"]
    
    experiment="AA 6ntop6 weekly TSI and buyRule"
    
  }
  
  
  
  #Ausrichtung
  #block=na.omit(merge(data$prices,data$watch$prices ))
  dim (data$prices)
  dim(position.score)
  block=(merge(data$prices,position.score  ))
  #block = block["2001::"]
  
  prices =block[,1:n] 
  
  position.score = block[,(n+1):(2*n)]
  #purePlot(mNorm(prices))
  
  # find month ends
  month.ends = endpoints(prices, 'months')
  month.ends = month.ends[month.ends > 0]    
  week.ends = endpoints(prices, 'weeks')
  week.ends = week.ends[week.ends > 0]    
  
  if (period == "months")
  {
    period.annual.factor=12
    period.ends = month.ends  
  }
  else
    if( period == "weeks")
    {
      period.annual.factor=54
      period.ends = week.ends  
      
    }
  
  #period.endsM = as.Date(index(prices[period.ends])) #MMXX
#  block["2008"]
  #*****************************************************************
  # Code Strategies
  #****************************************************************** 
  
  
  #  purePlot(mNorm(prices))
  #  xyplot(position.score)
  
  #MM2
  
  #### moderner
  data$weight = prices
  data$weight[] = NA
  data$prices = prices
  
  Tstops = prices;  Tstops[] = NA
  #technische Stops  MMEM3

  #Berechnung der dayly-technischen stops.
  #Wenn ein System am Alloc-Tag (periods.ends) augestoppt ist, erhält es ein sehr schlechtes  #position.scoring.
  #Es wird also niemand allokiert der ausgestoppt ist.
  
  mP("Tstops")
  
  #browser()
     if (useStops)
     {
      Tstops = lag(m.apply(data, function(x) {techStops(x)})  )   
      #Tstops = iif(global_Targets <0, 0,1)  #TEST-KRISTALLKUGEL
      TstopsOnPeriods = Tstops[as.Date(index(position.score[period.ends,]))]
       stopped= TstopsOnPeriods * position.score[period.ends,]
   #sollen ausgestoppte Titel nicht bei der AA berücksichtigt werden ?
      if (AAignoreStopped)
           position.score[period.ends,] = stopped 
     }
     else
         Tstops=NULL
  ##########################################################################
  
if (F)
  {
    signal =iif( mSlope90 > 0,1, 0.0000001)
    position.score =lag(signal)*mSlope90*10000;
    
    universe = ntop.keep(position.score[period.ends,],n.top, k.top) > 0
    
    universe = universe & universeMom
    
    experiment="AA ntop month on MomentumSlope90"
  }
  else
    universe = ntop.keep(position.score[period.ends,], n.top, k.top)  
  
  colnames(universe) = colnames(prices)
  
  #universe = ntop.keep(position.score[period.ends,], n, n)  
  #position.score["2008"]
  #position.score[period.ends,]
  #universe["2008"]
  #  universe = ntop.keep(position.score[period.ends,], 26, 26) > 0
  
  #*****************************************************************
  # Code Strategies that rebalance based on maximum deviation
  #****************************************************************** 
  
  # rebalance to target.allocation when portfolio weights are 5% away from target.allocation
  # models$smart5.all = bt.max.deviation.rebalancing(data, buy.hold, target.allocation, 5/100, 0) 
  #fromTo(universe)
  #fromTo(prices)
  #Berechnung der Gewichte .. gem. 
  #  install.packages("Rdonlp2", repos="http://R-Forge.R-project.org")
  obj = portfolio.allocation.helper(prices, 
                                    periodicity = period,
                                    period.ends=period.ends,
                                    lookback.len = n.vol, 
                                    universe=universe,
                                    shrinkage.fns = "ledoit.wolf.shrinkage",
                                    min.risk.fns = list(
                                      EW=equal.weight.portfolio,
                                      RP=risk.parity.portfolio,
                                      MV=min.var.portfolio,
                                      MD=max.div.portfolio,
                                      MC=min.corr.portfolio,
                                      MC1=min.cdar.portfolio,
                                      MC2=min.mad.downside.portfolio,
                                      #MO=max.omega.portfolio,  #läuft nicht, geht wohl nur mit Rdonlp2
                                      #MR=min.risk.portfolio,  #mittelmäig . wie MV
                                      #ML=min.maxloss.portfolio,  schlecht
                                      #MR=min.risk.downside.portfolio,  #genau wie MV
                                      #MP=max.geometric.return.portfolio,  #läuft nicht
                                      
                                      # MC2=min.corr2.portfolio,  #schrott
                                      # MCE=min.corr.excel.portfolio,    #schrott
                                      MS=max.sharpe.portfolio())
  ) 
  
  

  
  #data$weight[] = 0
  #data$execution.price[] = NA  
  #models$buy.hold = bt.run.share(data, clean.signal=T,trade.summary = TRUE)
  #models$buy.hold$trade.summary$trades
  
  #data$weight = prices
  #data$weight[]=NA
  #dim(obj$weights$EW)
  #data$weight[period.ends,] =  obj$weights$EW
  
  #bh = bt.run.share(data, clean.signal=T,  trade.summary = TRUE)
  #bh$trade.summary$trades
  #Tstops=NULL
  #Berechnung der Modell-GUV
  models = create.strategies(obj, data, Tstops=Tstops)$models
  #models$EW$trade.summary$trades
  
  
  experiment=sprintf("%s_%s_n%dk%d_%s",experiment,period,n.top,k.top,version )
  mP("########## experiment %s ########### ",experiment)
  models = rev(models)
  strategy.performance.snapshoot(models, T)
  mP("OOOOK") #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  browser()
  #clr()
  #new_Win(1)
  #purePlot(mNorm(prices))
  #clr()
  #k=compareViewModels(models,prices,alloc=T)
  
  pdf_Report(models,experiment)
  plotbt.custom.report.part1(rev(models))       
  
}
#########################################################################

if (F) #####################################################################
{
  
  # Plot Portfolio Turnover for each strategy
  layout(1)
  barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
  
  ####reporting nach xls schreiben
  reportModels = list(MV=models$MV,MS=models$MS,EW=models$EW)
  writeModelDataXls(reportModels,xlsName=sprintf("%s.xls",experiment))
  
  ######################################
  
  models=list()
  models$combo = bt.aaa.combo(data, period.ends, n.top = n.top,  n.mom = n.mom, n.vol = n.vol)
  experiment = "aa_combo"
  
  ######################################
  ls(models)
  
  
  models = list()
  
  # Equal Weight
  data$weight[] = NA
  data$weight[month.ends,] = ntop(prices, n)[month.ends,]  
  
  models$equal.weight = bt.run.share(data, clean.signal=F, dates=dates)
  
  
  
  #purePlot(position.score)
  
  colnames(position.score) = colnames(data$weight)
  
  # Select Top 2 funds
  data$weight[] = NA
  
  month.ends = endpoints(position.score, 'months')
  month.ends = month.ends[month.ends > 0]    
  
  data$weight[month.ends,] = ntop(position.score[month.ends,], 2)  
  models$top2 = bt.run.share(data, trade.summary=T, dates=dates)
  
  # Seletop Top 2 funds,  and Keep then till they are in 1:6 rank
  data$weight[] = NA
  data$weight[month.ends,] = ntop.keep(position.score[month.ends,], 2, 6)  
  models$top2.keep6 = bt.run.share(data, trade.summary=T, dates=dates)
  
  
  
  data$weight[] = NA
  data$weight[month.ends,] = ntop.keep(position.score[month.ends,], 7, 3)  
  models$top7.keep3 = bt.run.share(data, trade.summary=T, dates=dates)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  use.total=F
  prices4mom = prices #iif(use.total, data$prices, data.price$prices)
  prices4vol = prices# iif(use.total, data$prices, data.price$prices)    
  
  #models = list()
  
  # find period ends
  #period.ends = endpoints(prices, 'months')
  #period.ends = period.ends[period.ends > 0]
  
  # Adaptive Asset Allocation parameters
  n.top = 5  	# number of momentum positions
  n.mom = 6*22	# length of momentum look back
  n.vol = 1*22 	# length of volatility look back
  
  #*****************************************************************
  # Equal Weight
  #******************************************************************
  #data$weight[] = NA
  #data$weight[period.ends,] = ntop(prices[period.ends,], n)   
  #models$equal.weight = bt.run.share(data, clean.signal=F)
  
  period.ends=month.ends
  
  #*****************************************************************
  # Volatliliy Position Sizing
  #******************************************************************
  ret.log = bt.apply.matrix(prices4vol, ROC, type='continuous')
  hist.vol = bt.apply.matrix(ret.log, runSD, n = n.vol)
  
  adj.vol = 1/hist.vol[period.ends,]
  
  data$weight[] = NA
  data$weight[period.ends,] = adj.vol / rowSums(adj.vol, na.rm=T)    
  models$volatility.weighted = bt.run.share(data, clean.signal=F)
  
  #*****************************************************************
  # Momentum Portfolio
  #*****************************************************************
  momentum = prices4mom / mlag(prices4mom, n.mom)
  
  data$weight[] = NA
  data$weight[period.ends,] = ntop(momentum[period.ends,], n.top)   
  models$momentum = bt.run.share(data, clean.signal=F)
  
  
  
  
  #*****************************************************************
  # Combo: weight positions in the Momentum Portfolio according to Volatliliy
  #*****************************************************************
  weight = ntop(momentum[period.ends,], n.top) * adj.vol
  
  data$weight[] = NA
  data$weight[period.ends,] = weight / rowSums(weight, na.rm=T)   
  models$combo = bt.run.share(data, clean.signal=F,trade.summary = TRUE)
}
#*****************************************************************   
# Adaptive Asset Allocation (AAA)
# weight positions in the Momentum Portfolio according to 
# the minimum variance algorithm
#*****************************************************************   
if (F) #old
{
  
  weight = NA * prices
  weight[period.ends,] = ntop(momentum[period.ends,], n.top)
  
  for( i in period.ends[period.ends >= n.mom] ) {
    hist = ret.log[ (i - n.vol + 1):i, ]
    
    # require all assets to have full price history
    include.index = count(hist)== n.vol      
    
    # also only consider assets in the Momentum Portfolio
    index = ( weight[i,] > 0 ) & include.index
    n = sum(index)
    
    if(n > 0) {					
      hist = hist[ , index]
      
      # create historical input assumptions
      ia = create.ia(hist)
      s0 = apply(coredata(hist),2,sd)       
      ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
      
      # create constraints: 0<=x<=1, sum(x) = 1
      constraints = new.constraints(n, lb = 0, ub = 1)
      constraints = add.constraints(rep(1, n), 1, type = '=', constraints)       
      
      # compute minimum variance weights				            
      weight[i,] = 0        
      weight[i,index] = min.risk.portfolio(ia, constraints)
    }
  }
  
  
  # Adaptive Asset Allocation (AAA)
  data$weight[] = NA
  data$weight[period.ends,] = weight[period.ends,]   
  models$aaa = bt.run.share(data, clean.signal=F,trade.summary = TRUE)
  
}



if (F)
{
  #*****************************************************************
  # Create Report
  #****************************************************************** 
  allmodels=append(allmodels,models)
  
  k=compareViewModels(models,prices,alloc=T)
  #sysReporting(models)
  
  #signal =iif(dmSlope90 >0 & mSlope90 > 0,1,0)
  tradeResult(models,3, data, signal,frame="2007::2009",reset=T)
  
  pdf_Report(experiment)
  dev.off()
  #####################################################################################
  #####################################################################################
  
  
}  



bt.aaa.combo <- function
(
  data,
  period.ends,
  n.top = 5,		# number of momentum positions
  n.mom = 6*22,	# length of momentum look back
  n.vol = 1*22 	# length of volatility look back
) 
{
  #*****************************************************************
  # Combo: weight positions in the Momentum Portfolio according to Volatliliy
  #*****************************************************************
  prices = coredata(data$prices)  
  ret.log = bt.apply.matrix(prices, ROC, type='continuous')
  hist.vol = bt.apply.matrix(ret.log, runSD, n = n.vol)   
  adj.vol = 1/hist.vol[period.ends,]
  
  momentum = prices / mlag(prices, n.mom)
  
  weight = ntop(momentum[period.ends,], n.top) * adj.vol
  n.skip = max(n.mom, n.vol)
  
  data$weight[] = NA
  data$weight[period.ends,] = weight / rowSums(weight, na.rm=T)   
  data$weight[1 : n.skip,] = NA 
  bt.run.share(data, clean.signal=F, silent=T)
}

bt.aaa.minrisk <- function
(
  data,
  period.ends,
  n.top = 5,		# number of momentum positions
  n.mom = 6*22,	# length of momentum look back
  n.vol = 1*22 	# length of volatility look back
) 
{
  #*****************************************************************   
  # Adaptive Asset Allocation (AAA)
  # weight positions in the Momentum Portfolio according to 
  # the minimum variance algorithm
  #*****************************************************************   
  prices = coredata(data$prices)  
  ret.log = bt.apply.matrix(prices, ROC, type='continuous')
  #purePlot(mNorm(prices["2013"]))
  momentum = prices / mlag(prices, n.mom)
  
  weight = NA * prices
  weight[period.ends,] = ntop(momentum[period.ends,], n.top)
  n.skip = max(n.mom, n.vol)
  
  for( i in period.ends[period.ends >= n.skip] ) {
    hist = ret.log[ (i - n.vol + 1):i, ]
    
    # require all assets to have full price history
    include.index = count(hist)== n.vol      
    
    # also only consider assets in the Momentum Portfolio
    index = ( weight[i,] > 0 ) & include.index
    n = sum(index)
    
    if(n > 0) {					
      hist = hist[ , index]
      
      # create historical input assumptions
      ia = create.ia(hist)
      s0 = apply(coredata(hist),2,sd)       
      ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
      
      # create constraints: 0<=x<=1, sum(x) = 1
      constraints = new.constraints(n, lb = 0, ub = 1)
      constraints = add.constraints(rep(1, n), 1, type = '=', constraints)       
      
      # compute minimum variance weights				            
      weight[i,] = 0        
      weight[i,index] = min.risk.portfolio(ia, constraints)
    }
  }
  
  # Adaptive Asset Allocation (AAA)
  data$weight[] = NA
  data$weight[period.ends,] = weight[period.ends,]   
  bt.run.share(data, clean.signal=F, silent=T)
}

########################################################################


if (!exists("MM_lesson1"))
{
  AllZenarios= runEckhard()  #lädt die Daten
  
  mSlope90 <<- rollRegressionXTS(mNorm(data$prices),win=90)
  dmSlope90 <<-rollRegressionXTS(na.omit(mSlope90),win=30)
  plot(mSlope90)
}
MM_lesson1="ok"
