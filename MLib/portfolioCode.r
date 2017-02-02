
options( error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)



### Lade alle Titel die portfolios.xls eine Pos zum Portfolio = Name haben

portfolioTicks <-function(Name="MMiksaHot")
{
  mP("portfolioTicks at portfolioCode")
  #MF(portfolioTicks)
  myPortfolio = Name
  #browser()
  wb <- loadWorkbook(sprintf("%s/portfolios.xls",customerData))#, create = TRUE)
  Positions = data.table(wb["Positions"]); setkey(Positions,"Portfolio")
  posList = Positions[which(Positions$Portfolio==myPortfolio)]
  tickList  = cbind( posList$Name, posList$contracts,posList$entryDate,posList$entryPrice)
  if (len(tickList) == 0)
    mP("Sorry - there are no Positions at %s %s %s",myPortfolio," look at ",sprintf("%s/portfolios.xls",customerData))
  colnames(tickList)= c("Name","Contracts","entryDate","entryPrice")
  return (data.frame(tickList))
}

if (F)
{
  # Load workbook (create if not existing)
  #customerData="d:/DataPrismaDemo/CUSTOMER_DATA/Demo_Customer/Input"
  customerData= sprintf("%s/Models",customerData)
  
  pT = portfolioTicks ("RenditePlus")  
  pT$Name
  portfolioTicks ("RenditePlus")$Name
  
  
  wb <- loadWorkbook(sprintf("%s/portfolioList.xls",customerData))#, create = TRUE)
  PortfolioList = data.table(wb["PortfolioList"])
  PortfolioSpec = as.list(PortfolioList[PortfolioList$Portfolio=="RenditePlus"])
  str(PortfolioSpec) 
  
  wb <- loadWorkbook(sprintf("%s/Securities.xls",customerData))#, create = TRUE)
  SecList = data.table(wb["SecList"])
  #tables()
  colnames(SecList)
  setkey(SecList,"Name")
  
  tickers=c()
}

################################################################################################################################
#  Die Ticker werden aus den Portfolio-Stammdaten (PortfolioList.xls, Portfolios.xls, Securities.xls) gelesen: 
#  und zwar aus den Ticks des Portfolios und seiner Watchlists
#  schreibe optional auch eine Universe.csv - datei - wenn model != "" ist.
################################################################################################################################
#MM+  noch fertig schreiben .. 
if (F)
{
  loadCustomerPortfolio(myPortfolio= "RenditePlus", modelDir="RenditePLus_")
  eval(call("loadCustomerPortfolio","XX","yy"))
}



#nicht unbedingt notwendig ...
loadCustomerPortfolio<-function(myPortfolio= "MMiksaHot", modelDir="")
{ 
  #mP("####> %s %s",toString(match.call()),where)
  
  #browser()
  myPortfolio = "RenditePlus"
  # Schau in der PortfolioList nach der Spec des Portfolios
  wb <- loadWorkbook(sprintf("%s/portfolioList.xls",customerData))#, create = TRUE)
  PortfolioList = data.table(wb["PortfolioList"])
  PortfolioSpec = as.list(PortfolioList[PortfolioList$Portfolio==myPortfolio])
  
  mP(PortfolioSpec) 
  watchList = PortfolioSpec$WatchLists
  watchPortfolios= c(myPortfolio,spl(watchList))
  
  wb <- loadWorkbook(sprintf("%s/Securities.xls",customerData))#, create = TRUE)
  SecList = data.table(wb["SecList"])
  #tables()
  colnames(SecList)
  setkey(SecList,"Name")
  
  #schau für das Portfolio und seine Watchlists in der Portfolios.xls#Positions welche Positionen zu dem Portfolio gehören
  tickers = list()
  for(portfolio in watchPortfolios)
  {
    portfolio="DaxMember"   #TEST
    print(portfolio)
    pfType = (PortfolioList[PortfolioList$Portfolio==portfolio])$pfType
    if (pfType == 2) #IndexMember
    {
      indexName = (PortfolioList[PortfolioList$Portfolio==portfolio])$WatchLists
      ticks = getIndexComposition(indexName)       
    }
    else
      ticks= portfolioTicks (portfolio)$Name
    print(ticks)
    for (tick in ticks)
    {
      #  tick = "Rex" #TEST
      ticker=first(SecList[SecList$Name==tick]$YahooTicker)
      if (is.null(tickers[[tick]]) )
      {
        #unique List:   nur wenn das Element noch nicht da ist wird es eingefügt
        # isin=SecList[SecList$Name==tick]$ISIN
        tickers[tick]=tick
      }
    }
    print(tickers)
  }
  tickers = c(tickers)
  unlist(tickers)
  if (modelDir!= "")
  {
    #schreib die stoxxL- Liste als Universe.txt...
    writeIndexComposition(index=index, universeFile=sprintf("Models/%s/universe.csv",modelDir),row.names=F,quote=T)
    
  }
  
  
  if (F)
  {
    writeIndexComposition("^GDAXI","D:\\gdax.csv")
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~
    tickers = list()
    newVal = "Dax"
    tickers[newVal]=newVal 
    newVal = "Rex"
    tickers[newVal]=newVal 
    tickers[["Dax1"]] == "Dax1"
    
    tickers["vw"]
  }
}

parseEckhardsXlsZenarios<-function(parseEckhardDataXls = parseEckhardDataXls,AllZenariosFname=AllZenariosFname)
{
  if (parseEckhardDataXls)
  {
    #lies aus Eckhards xls die Zeitreihen und Zenario-Informationen und schreib Rohdatenreports
    AllZenarios=  importEckhardsXLS(globMDataSUB)
    save(AllZenarios,file=AllZenariosFname) 
  }
  else
    load(AllZenariosFname)
  return (AllZenarios)
}
###############################################################################
# Portfolio Construction and Optimization routines
###############################################################################



#Eckhard liefert seine Rohdaten- in einem besonderen xls-Format .. (mit 5 LeerZeilen am Anfang, ... )
#Normiere sie für die eigene Plattform !!
#
#mach aus diesen Rohdaten folgendes:
#a) Normalisiere die wpNamen und leg pro Wertpapier eine csv an,  und
#b)  ein Universum.csv aller Wertpapiernamen
#c)  einen Ordner pro Zenario mit einer eigenen  Universe.csv die diesem Zenario entspricht
#
#RückgabeWert:  eine Liste aus Listen mit den Zenarien (TeilUniversen ..)
#
importEckhardsXLS <-function(mdataSubDir="eMod1")
{
  #Beispiel:  Einlesen von xls-File#######################
  library(XLConnect)
  
  
  CSVdataPath=sprintf("MData/%s",mdataSubDir)
  dataPath =sprintf("Models/%s",modelDir) #da liegen die xls-files
  mP("read at path %s",dataPath)
  AllUniverse = c()  #das wird die Liste sämtlicher Wertpapiere die in den Samplex.xls- Files vorkommen
  AllZenarios = c() #RückgabeWert:  eine Liste aus Listen mit den Zenarien (TeilUniversen ..)
  #Für alle xls-Files
  xlsPat = glob2rx("*.xls") #wildcard-Transfer
  for (xlsfile in 
       dir(path = dataPath, pattern = xlsPat, all.files = T,
           full.names = F, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE)
  )
  {    
    #xlsfile="Sample5.xls"  #TEST
    #ueberlies die ersten 5 Zeilen
    #browser()
    mP("read %s",xlsfile)
    bm <- readWorksheetFromFile(paste(dataPath, xlsfile, sep="/"), sheet=1, startRow=5, header=TRUE)
    
    #loesch die ersten 4 Zeilen       --- -  früher:   und die erste Spalte
    bm2=bm[c(-1:-4),]
    #bm2=head(bm,10)#Test
    
    #Gib der ersten Spalte einen anderen Namen:   "Index"
    #colnames(bm2)<-c("Index",colnames(bm2)[-1])
    
    head(bm2) #TEST
    colnames(bm2)
    #finde die erste Spalte in der wirklich das Datum steht - also die vorletzte die keinen Col-Bezeichner hat .. (Eckhards-format ...?)
    firstCol  = 0
    for  (cN in colnames(bm2))  
    {
      
      if (substr(cN, 1, 3) == "Col" && !isEmptyCol(bm2,cN))
        firstCol = firstCol+1
      else
        break
    }
    dir =c() #ein neue leer Liste aller schon gesehenen wp-name 
    #ueber alle Spalten ab der zweiten
    zenario = c()
    mP(xlsfile)
    print(head(bm2[,firstCol]))
    head(bm2)
    for ( i in (firstCol+1): dim(bm2)[2])   #firstCol zeigt auf die  Datum-Spalte
    {
      if (!isEmptyCol(bm2,colnames(bm2[i])))
      {
        #i=firstCol+1 #TEST
        #erzeuge einen 2 Spalen-Tabelle
        #mP(" use Col  %d",i)
        bm3 = na.omit(bm2[,c(firstCol,i)])
        #  head(bm3)
        #Falls die nicht leer ist ...
        #browser()
        if (!is.null(bm3))  #MM1
          if ( dim(bm3)[1] > 0)
          {
            #hol den wpNamen als Namen der Spalte 2
            wpName0 = colnames(bm3)[2] 
            
            if (mm.strFind("WATCH",wpName0))
            {
              wpName0= rightOf(wpName0,"WATCH")
              zenario=c(zenario,"############ WATCHLIST ###########")
            }
            wpName = prettyWpName(wpName0)
            
            #setz den 2. spaltenNamen auf den normierten wpNamen
            colnames(bm3)<-c("Index",wpName)
            #das zenario erweitern
            zenario = c(zenario, wpName)
            
            #ist der wpName neu ? (noch nicht in der dir-liste)
            if (! is.def(dir[wpName]))
            {
              #trag ihn in die dir-liste ein
              dir =c(dir,wpName=T) 
              fname = sprintf("%s/%s.csv",CSVdataPath,wpName)
              #schreibe das csv-File der 2 Spaltigen Tabelle (datum,wpName)
              mP("write %s",fname)
              write.table(bm3, fname, sep=";", row.names=FALSE, col.names=T,quote=T,dec=",")
              AllUniverse =c(AllUniverse,wpName)  
            }
          }
      }
    }
    #erzeuge - bei Bedarf ein "Zenario-Dir" mit dem Universe des aktuellen Sample<i>.xls
    sampleDir = sprintf("%s_%s",dataPath,xlsfile)
    if (!file.exists(sampleDir))
      dir.create(sampleDir)
    zen= list(zenarioDir=xlsfile, wplist=list(zenario))
    mP("Run #########################") 
    
    AllZenarios = c(AllZenarios, list(zen))  
    universe = as.matrix(zenario)
    
    colnames(universe) = c("member")
    #schreibe das Universe.csv des Zenarios
    universeFile = sprintf("%s/Universe.csv",sampleDir)
    mP("write %s", universeFile)
    write.table(universe, universeFile, sep=";", row.names=FALSE, col.names=T,dec=",")
    
  }
  
  #schreibe das Gesamt-Universum
  universe = as.matrix(AllUniverse)
  colnames(universe) = c("member")
  #schreibe das Universe.csv des Zenarios
  universeFile = sprintf("%s/Universe.csv",dataPath)
  mP("write %s", universeFile)
  write.table(universe, universeFile, sep=";", row.names=FALSE, col.names=T,dec=",")
  
  return (AllZenarios)
}



#*****************************************************************
# AllZenarios ist eine Liste mit 2 Listen:    DirName, WpListe  - wobei DirName auf einen Ordner mit einer Universe.csv zeigt
# Lade diese Daten ale in ein eigenes environment .. mache die üblichen Vorverabeitungen .. und speicher das data environment
# im dem Order  untder rda.
#*****************************************************************
prepareAllZenarios<-function(AllZenarios,data=new.env())
{
  for (zenario in AllZenarios)
  {  
    #    zenario = AllZenarios[[2]] #Test
    mP("#################################### %s ################################################################## >>>",zenario$zenarioDir)
    
    #Rodaten laden (pwer source von LoadData.R)
    #############################################################################################
    
    #BENCH = "sx5r"
    
    #dataSet sagt global  "LoadData.R" welches universe.csv den Datenload steuert
    #browser()
    dataSet = sprintf("%s_%s",modelDir,zenario$zenarioDir)
    dataSetFile = paste(dataSet,"/Data.Rdata",sep="")
    filename.pdf =paste(dataSet,"/Data_",zenario$zenarioDir,".pdf",sep="")
    showUniverse = F
    
    
    #data=new.env()
    data.weekly <- new.env() 
    data.monthly <- new.env()
    
    #myFrame = "2008::2013"; 
    
    universeFile =sprintf("Models/%s/universe.csv",dataSet)
    mP("read tickers from %s", universeFile)
    
    tickers=read.table(file=universeFile,header=TRUE,as.is=TRUE)
    tickers=as.matrix(tickers[1])
    wli=which(tickers=="############ WATCHLIST ###########")  #wenn im Universe.csv das Wort  "############ WATCHLIST ###########"  werden die folgenden Daten nicht nach data sondern nach data$watch importiert.
    
    fundamentalTicker = c()
    
    if (is.def(wli))
    { 
      
      fundamentalTicker = tickers[index(tickers) > wli]
      
      watch= new.env()
      loadData(fundamentalTicker,watch)
      
      tickers = tickers[index(tickers) < wli]
    }
    
    mP(tickers)
    ###############################################################################
    #debug(loadData)
    mist=NULL
    loadData(tickers,data)
    #BENCH = "Dax"
    
    tickers = ls(data);
    
    #  browser()
    #for(i in ls(data)) cat( i, format(index(data[[i]][1,]), '%d%b%y'), '\n')
    
    #undebug(preprepData)
    #browser()
    pD= preprepData(myFrame,tickers,data,visual=F)   #ret= mROC(data$prices)
    if (is.def(wli))
    {
      pD= preprepData(myFrame,fundamentalTicker,watch,visual=F)   #ret= mROC(data$prices)
      data$watch=watch
    }
    
    
    
    #userStop("nach prepData")
    #MplotNormedPrices(mROC(data$prices))
    #plotNormedPrices(cumsum(mROC(data$prices)))
    
    
    #*****************************************************************
    # Create Report
    #****************************************************************** 
    if (F)
    {
      mP(getwd())
      
      
      xyplot(data$prices)
      if (is.def(data$watch))
        xyplot(data$watch$prices)
      # userStop("-- die Rohdaten ---")
      
      pdf= sprintf("Models/%s/inputDate.pdf",modelDir);
      
      # put all reports into one pdf file
      mP("Write Data Charts to %s",pdf)
      
      pdf(file = pdf, width=8.5, height=11) 
      
      xyplot(data$prices)
      if (is.def(data$watch))
        xyplot(data$watch$prices)
      
      
      #mm.RiskReturnScatter(ret)
      #MplotNormedPrices(ret )
      dev.off()  
    } 
    ret = na.omit(pD)  
    data$ret = ret
    
    save(data,file=sprintf("Models/%s",dataSetFile))
    
    mP(getwd())
    
    if (F)
    {  
      ############################################################################################
      #source("Models/Trade.r")
      
      ############################################################################################
      BENCH=1
      Bench = data$prices[,BENCH]
      head(data$prices,3 ); tail(data$prices,3)
      Auflegedatum=fromTo(data$prices)[1]
      Bisdatum = fromTo(data$prices)[2]     #   format(index(tail(data$prices[""],1)[1,]), '%d%b%y')
      cat("Folgende Zeitreihen konnte nicht geladen werden, weil:  nicht im Internet,  zu kurz (400) oder ohne Close \n", mist)
      frame =""
      ds = sprintf("Models/%s",dataSetFile)
      save(data,file=ds)
      
    }
  }
  return ("ok")
}


###################################################################################################
###################################################################################################


runAllocationZenario<-function(doLoad = T)
{
  for (zenario in AllZenarios)
  {  
    #zenario = AllZenarios[[1]] #Test
    mP("RUN ALLOC #################################### %s ################################################################## >>>",zenario$zenarioDir)
    
    #dataSet sagt global  "LoadData.R" welches universe.csv den Datenload steuert
    #dataSet = sprintf("EM2_Jan13_%s",zenario$zenarioDir)
    dataSet = sprintf("%s_%s",modelDir,zenario$zenarioDir)
    #  load(file = paste(name, '.rda', sep=''))
    datFile = paste("Models/",dataSet,"/Data.Rdata",sep="")
    if (doLoad ) load(file= datFile)
    fromTo(data$prices)
    
    mf = 'min.var.portfolio,min.corr.portfolio,min.corr2.portfolio,max.div.portfolio,min.var.portfolio,risk.parity.portfolio,equal.weight.portfolio'
    #mf = 'min.var.portfolio'
    
    #browser()  
    #data$prices  )= na.omit(data$prices)
    if (F)
      obj = portfolio.allocation.helper(data$prices, periodicity, lookback.len = lookback.len, prefix = prefix,   
                                        min.risk.fns = mf,
                                        custom.stats.fn = 'portfolio.allocation.custom.stats')	
    else
      obj = portfolio.allocation.helper(adjust2positive.definite=T, data$prices, periodicity, lookback.len = lookback.len, prefix = prefix,  
                                        shrinkage.fns = "ledoit.wolf.shrinkage",
                                        #shrinkage.fns="sample.shrinkage",
                                        min.risk.fns = mf, custom.stats.fn = 'portfolio.allocation.custom.stats')  	
    
    
    #obj = portfolio.allocation.helper(data$prices, periodicity, lookback.len = lookback.len, prefix = prefix,
    #                                  min.risk.fns = mf)		
    fname=paste("Models/",dataSet,"/", lookback.len, periodicity, '.bt', '.Rdata', sep='')  
    mP(fname)
    #browser()
    save(obj, file=fname)
    
    #Reporting
    name = zenario$zenarioDir
    fname  =paste("Models/",dataSet,"/",'report.', name, lookback.len, periodicity, sep='')
    mP(fname)
    
    strategie = create.strategies(obj, data)
    
    ls(strategie$models)
    
    #????  custom.report.helper(fname, strategie  )  
    
    models = strategie$models
    
    
  }
  return (strategie)
}


################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#*****************************************************************
# Create summary of strategies report
#*****************************************************************
custom.report.helper <- function(filename, obj) {
  filename.pdf = paste(filename, '.pdf', sep='')
  filename.csv = paste(filename, '.csv', sep='')
  
  
  models = obj$models
  
  #*****************************************************************
  # Create Report
  #****************************************************************** 
  
  doGini = F
  # put all reports into one pdf file
  
  pdf(file = filename.pdf, width=8.5, height=11)
  
  
  # Plot perfromance
  plotbt(models, plotX = T, log = 'y', LeftMargin = 3)      	
  mtext('Cumulative Performance', side = 2, line = 1)
  
  
  # Plot Strategy Statistics  Side by Side
  
  out = plotbt.strategy.sidebyside(models, perfromance.fn = 'custom.returns.kpi', return.table=T)
  
  
  # Plot time series of components of Composite Diversification Indicator
  if (doGini)
  {
    cdi = custom.composite.diversification.indicator(obj)	
    out = rbind(colMeans(cdi, na.rm=T), out)
    rownames(out)[1] = 'Composite Diversification Indicator(CDI)'
  }	
  
  # Portfolio Turnover for each strategy
  y = 100 * sapply(models, compute.turnover, data)
  print(y)
  rownames(out)[1] = 'Portfolio Turnover'		
  
  
  if (F)
    if (doGini)
      # Bar chart in descending order of the best algo by Sharpe Ratio, CAGR, Gini, Herfindahl	
      performance.barchart.helper(out, 'Sharpe,Cagr,RC Gini,RC Herfindahl,Volatility,Portfolio Turnover,Composite Diversification Indicator(CDI)', c(T,T,F,F,F,F,T))
  
  else
    performance.barchart.helper(out, 'Sharpe,Cagr,RC Herfindahl,Volatility,Portfolio Turnover,Composite Diversification Indicator(CDI)', c(T,T,F,F,F,T))
  
  # summary allocation statistics for each model	
  custom.summary.positions(obj$weights)
  
  
  # monhtly returns for each model	
  custom.period.chart(models)
  
  
  # Plot transition maps
  layout(1:len(models))
  for(m in names(models)) {
    plotbt.transition.map(models[[m]]$weight, name=m)
    legend('topright', legend = m, bty = 'n')
  }
  
  
  # Plot transition maps for Risk Contributions
  if (T)
  {
    dates = index(models[[1]]$weight)[obj$period.ends]
    layout(1:len(models))
    for(m in names(models)) {
      plotbt.transition.map(make.xts(obj$risk.contributions[[m]], dates), 
                            name=paste('Risk Contributions',m))
      legend('topright', legend = m, bty = 'n')
    }
  }
  # close pdf
  dev.off()	
  
  
  #*****************************************************************
  # save summary & equity curves into csv file
  #****************************************************************** 
  load.packages('abind')
  write.csv(out, filename.csv,sep=";")
  cat('\n\n', file=filename.csv, append=TRUE)
  
  out = abind(lapply(models, function(m) m$equity))
  colnames(out) = names(models)
  
  write.xts(make.xts(out, index(models[[1]]$equity)), filename.csv)#, append=TRUE)		
}




#*****************************************************************
# Custom Summary function to add consentration statistics (Gini and  Herfindahl)
#
# On the properties of equally-weighted risk contributions portfolios by
# S. Maillardy, T. Roncalliz,  J. Teiletchex (2009)
# A.4 Concentration and turnover statistics, page 22
#*****************************************************************
custom.returns.kpi <- function
(
  bt,  	# backtest object
  trade.summary = NULL
) 
{	
  
  out = list()
  w = bt$period.weight
  rc = bt$risk.contribution
  
  # Average Number of Holdings
  out[[ 'Avg #' ]] =  mean(rowSums(w > 1/1000)) / 100
  #	browser()
  # Consentration stats
  #  out[[ 'W Gini' ]] = mean(portfolio.concentration.gini.coefficient(w), na.rm=T)
  #  out[[ 'W Herfindahl' ]] = mean(portfolio.concentration.herfindahl.index(w), na.rm=T) 
  
  # Consentration stats on marginal risk contributions
  #	out[[ 'RC Gini' ]] = mean(portfolio.concentration.gini.coefficient(rc), na.rm=T)
  #  	out[[ 'RC Herfindahl' ]] = mean(portfolio.concentration.herfindahl.index(rc), na.rm=T) 
  
  out = lapply(out, function(x) if(is.double(x)) round(100*x,1) else x)
  out = c(bt.detail.summary(bt)$System, out)
  
  return( list(System=out))
}


#*****************************************************************
# Create summary of inputs report
#*****************************************************************
custom.input.report.helper <- function(filename, data) {
  filename.pdf = paste(filename, '.pdf', sep='')
  filename.csv = paste(filename, '.csv', sep='')
  
  #*****************************************************************
  # Create Report
  #****************************************************************** 
  # put all reports into one pdf file
  pdf(file = filename.pdf, width=8.5, height=11)
  
  # Input Details
  layout(1:2)
  asset.models = list()
  for(i in data$symbolnames) {
    data$weight[] = NA
    data$weight[,i] = 1  
    asset.models[[i]] = bt.run(data, silent=T)
  }		
  asset.summary = plotbt.strategy.sidebyside(asset.models, return.table=T)
  
  
  # plot correlations
  ret.log = bt.apply.matrix(data$prices, ROC, type='continuous')
  temp = compute.cor(ret.log, 'pearson')
  temp[] = plota.format(100 * temp, 0, '', '%')
  plot.table(temp, smain='Correlation', highlight = TRUE, colorbar = TRUE)	
  
  
  # line plot for each input series
  layout(matrix(1:4,2,2))	
  if( is.null(data$symbol.groups) ) {
    index = order(data$symbolnames)
    for(i in data$symbolnames[index]) 
      plota(data[[i]], type='l', cex.main=0.7,main= i)
  } else {
    index = order(data$symbol.groups)
    for(i in data$symbolnames[index]) 
      plota(data[[i]], type='l', cex.main=0.7, main= paste(i, data$symbol.groups[i], data$symbol.descriptions.print[i], sep=' / ') )
    
    asset.summary = rbind(data$symbol.groups, data$symbol.descriptions.print, asset.summary)
  }
  
  
  # close pdf
  dev.off()	
  
  #*****************************************************************
  # save summary & equity curves into csv file
  #****************************************************************** 
  load.packages('abind')
  write.csv(asset.summary, filename.csv)
  cat('\n\n', file=filename.csv, append=TRUE)
  
  write.table(temp, sep=',',  row.names = , col.names = NA,
              file=filename.csv, append=TRUE)
  
}




################################################################################################################




if (F)
{
  
  
  
  ###############################################################################
  # Custom Report routines
  ###############################################################################
  
  #*****************************************************************
  # Create summary of inputs report
  #*****************************************************************
  custom.input.report.helper <- function(filename, data) {
    filename.pdf = paste(filename, '.pdf', sep='')
    filename.csv = paste(filename, '.csv', sep='')
    
    #*****************************************************************
    # Create Report
    #****************************************************************** 
    # put all reports into one pdf file
    pdf(file = filename.pdf, width=8.5, height=11)
    
    # Input Details
    layout(1:2)
    asset.models = list()
    for(i in data$symbolnames) {
      data$weight[] = NA
      data$weight[,i] = 1	
      asset.models[[i]] = bt.run(data, silent=T)
    }		
    asset.summary = plotbt.strategy.sidebyside(asset.models, return.table=T)
    
    
    # plot correlations
    ret.log = bt.apply.matrix(data$prices, ROC, type='continuous')
    temp = compute.cor(ret.log, 'pearson')
    temp[] = plota.format(100 * temp, 0, '', '%')
    plot.table(temp, smain='Correlation', highlight = TRUE, colorbar = TRUE)	
    
    
    # line plot for each input series
    layout(matrix(1:4,2,2))	
    if( is.null(data$symbol.groups) ) {
      index = order(data$symbolnames)
      for(i in data$symbolnames[index]) 
        plota(data[[i]], type='l', cex.main=0.7,main= i)
    } else {
      index = order(data$symbol.groups)
      for(i in data$symbolnames[index]) 
        plota(data[[i]], type='l', cex.main=0.7, main= paste(i, data$symbol.groups[i], data$symbol.descriptions.print[i], sep=' / ') )
      
      asset.summary = rbind(data$symbol.groups, data$symbol.descriptions.print, asset.summary)
    }
    
    
    # close pdf
    dev.off()	
    
    #*****************************************************************
    # save summary & equity curves into csv file
    #****************************************************************** 
    load.packages('abind')
    write.csv(asset.summary, filename.csv)
    cat('\n\n', file=filename.csv, append=TRUE)
    
    write.table(temp, sep=',',  row.names = , col.names = NA,
                file=filename.csv, append=TRUE)
    
  }
  
  
  #*****************************************************************
  # Create summary of strategies report
  #*****************************************************************
  custom.report.helper <- function(filename, obj) {
    filename.pdf = paste(filename, '.pdf', sep='')
    filename.csv = paste(filename, '.csv', sep='')
    
    
    models = obj$models
    
    #*****************************************************************
    # Create Report
    #****************************************************************** 
    
    doGini = F
    # put all reports into one pdf file
    pdf(file = filename.pdf, width=8.5, height=11)
    
    
    # Plot perfromance
    plotbt(models, plotX = T, log = 'y', LeftMargin = 3)	    	
    mtext('Cumulative Performance', side = 2, line = 1)
    
    
    # Plot Strategy Statistics  Side by Side
    
    out = plotbt.strategy.sidebyside(models, perfromance.fn = 'custom.returns.kpi', return.table=T)
    
    
    # Plot time series of components of Composite Diversification Indicator
    if (doGini)
    {
      cdi = custom.composite.diversification.indicator(obj)	
      out = rbind(colMeans(cdi, na.rm=T), out)
      rownames(out)[1] = 'Composite Diversification Indicator(CDI)'
    }	
    
    # Portfolio Turnover for each strategy
    y = 100 * sapply(models, compute.turnover, data)
    
    rownames(out)[1] = 'Portfolio Turnover'		
    
    if (doGini)
      # Bar chart in descending order of the best algo by Sharpe Ratio, CAGR, Gini, Herfindahl	
      performance.barchart.helper(out, 'Sharpe,Cagr,RC Gini,RC Herfindahl,Volatility,Portfolio Turnover,Composite Diversification Indicator(CDI)', c(T,T,F,F,F,F,T))
    #  else
    #  performance.barchart.helper(out, 'Sharpe,Cagr,RC Herfindahl,Volatility,Portfolio Turnover,Composite Diversification Indicator(CDI)', c(T,T,F,F,F,T))
    
    # summary allocation statistics for each model	
    custom.summary.positions(obj$weights)
    
    
    # monhtly returns for each model	
    custom.period.chart(models)
    
    
    # Plot transition maps
    layout(1:len(models))
    for(m in names(models)) {
      plotbt.transition.map(models[[m]]$weight, name=m)
      legend('topright', legend = m, bty = 'n')
    }
    
    
    # Plot transition maps for Risk Contributions
    if (doGini)
    {
      dates = index(models[[1]]$weight)[obj$period.ends]
      layout(1:len(models))
      for(m in names(models)) {
        plotbt.transition.map(make.xts(obj$risk.contributions[[m]], dates), 
                              name=paste('Risk Contributions',m))
        legend('topright', legend = m, bty = 'n')
      }
    }
    # close pdf
    dev.off()	
    
    
    #*****************************************************************
    # save summary & equity curves into csv file
    #****************************************************************** 
    load.packages('abind')
    write.csv(out, filename.csv,sep=";")
    cat('\n\n', file=filename.csv, append=TRUE)
    
    out = abind(lapply(models, function(m) m$equity))
    colnames(out) = names(models)
    write.xts(make.xts(out, index(models[[1]]$equity)), filename.csv, append=TRUE)		
  }
  
  
  
  ###############################################################################
  #
  # Numerical examples used in the Minimum Correlation Algorithm papaer
  #
  ###############################################################################
  min.corr.paper.numerical.examples <- function() 
  {
    load.packages('quadprog')
    #*****************************************************************
    # create input assumptions
    #*****************************************************************
    n = 3
    ia = list()
    ia$n = 3
    ia$risk = c(14, 18, 22) / 100;
    ia$correlation = matrix(
      c(1, 0.90, 0.85,
        0.90, 1, 0.70,
        0.85, 0.70, 1), nr=3, byrow=T)
    ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
    
    #*****************************************************************
    # create constraints
    #*****************************************************************
    constraints = new.constraints(n)
    # 0 <= x.i <= 1
    constraints = new.constraints(n, lb = 0, ub = 1)
    constraints = add.constraints(diag(n), type='>=', b=0, constraints)
    constraints = add.constraints(diag(n), type='<=', b=1, constraints)
    
    # SUM x.i = 1
    constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		
    
    #*****************************************************************
    # Minimum Variance Portfolio 
    #*****************************************************************
    x = min.var.portfolio(ia, constraints)
    
    sol = solve.QP(Dmat=ia$cov, dvec=rep(0, ia$n), 
                   Amat=constraints$A, bvec=constraints$b, meq=constraints$meq)
    x = sol$solution
    
    round(x,4)
    sqrt(x %*% ia$cov %*% x)
    
    # marginal contributions	
    x %*% ia$cov
    
    #*****************************************************************
    # Maximum Diversification Portfolio 
    #*****************************************************************			
    sol = solve.QP(Dmat=ia$correlation, dvec=rep(0, ia$n), 
                   Amat=constraints$A, bvec=constraints$b, meq=constraints$meq)
    x = sol$solution
    round(x,4)
    
    # marginal contributions
    x %*% ia$correlation
    
    
    # re-scale and normalize weights to sum up to 1
    x = x / sqrt( diag(ia$cov) )
    x = x / sum(x)
    round(x,4)
    sqrt(x %*% ia$cov %*% x)
    
    #*****************************************************************
    # Minimum Correlation Portfolio 
    #*****************************************************************						
    upper.index = upper.tri(ia$correlation)
    cor.m = ia$correlation[upper.index]
    cor.mu = mean(cor.m)
    cor.sd = sd(cor.m)
    
    norm.dist.m = 0 * ia$correlation	
    diag(norm.dist.m) = NA
    norm.dist.m[upper.index] = sapply(cor.m, function(x) 1-pnorm(x, cor.mu, cor.sd))
    norm.dist.m = (norm.dist.m + t(norm.dist.m))
    
    norm.dist.avg = apply(norm.dist.m, 1, mean, na.rm=T)
    
    norm.dist.rank = rank(-norm.dist.avg)
    
    adjust.factor = 1
    adjusted.norm.dist.rank = norm.dist.rank ^ adjust.factor
    
    norm.dist.weight = adjusted.norm.dist.rank / sum(adjusted.norm.dist.rank)
    
    weighted.norm.dist.average = norm.dist.weight %*% ifna(norm.dist.m,0)
    
    final.weight = weighted.norm.dist.average / sum(weighted.norm.dist.average)
    
    x = final.weight
    
    # re-scale and normalize weights to sum up to 1
    x = x / sqrt( diag(ia$cov) )
    x = x / sum(x)
    round(x,4)
    x = as.vector(x)
    sqrt(x %*% ia$cov %*% x)
    
    #*****************************************************************
    # Minimum Correlation 2 Portfolio 
    #*****************************************************************						
    cor.m = ia$correlation
    diag(cor.m) = 0
    
    avg = rowMeans(cor.m)
    cor.mu = mean(avg)
    cor.sd = sd(avg)
    norm.dist.avg = 1-pnorm(avg, cor.mu, cor.sd)
    
    norm.dist.rank = rank(-norm.dist.avg)
    
    adjust.factor = 1
    adjusted.norm.dist.rank = norm.dist.rank ^ adjust.factor
    
    norm.dist.weight = adjusted.norm.dist.rank / sum(adjusted.norm.dist.rank)
    
    weighted.norm.dist.average = norm.dist.weight %*% (1-cor.m)
    final.weight = weighted.norm.dist.average / sum(weighted.norm.dist.average)
    
    x = final.weight
    
    # re-scale and normalize weights to sum up to 1
    x = x / sqrt( diag(ia$cov) )
    x = x / sum(x)
    round(x,4)
    x = as.vector(x)
    sqrt(x %*% ia$cov %*% x)
    
    
    #min.corr.portfolio(ia, constraints)
    #min.corr2.portfolio(ia, constraints)					
  }
  
}


print("########### load portfolioCode.R")
