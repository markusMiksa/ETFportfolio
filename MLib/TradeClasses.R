options(error = quote({
  #  sink(file="error.txt");
  dump.frames()
  
  print(attr(last.dump, "error.message"))
  
  traceback()
  
  #  sink();
}))
options(warn = 1)
version_TradeClassesVer = "22.2.2013"
#
#setwd("O:/R/Nuggets/Eckhard")

#source('MLib/InputConfig_Portfolio_TD.R')
#source('MLib/Trade.R')
#source("MLib/SITpatches.r")
library(msm)

#source('LoadDataFuncs.r')
#library(XLConnect)
#setwd("o:/r/Nuggets/Eckhard")
# Load workbook (create if not existing)
customerData <<-
  sprintf("%s/Models", getwd())   #"o:/r/Nuggets/Eckhard/Models"
#"d:/DataPrismaDemo/CUSTOMER_DATA/Demo_Customer/Input"

#die folgenden 3 globalen Tabellen enthalten nach einem Durchlauf die  entsprechenden
# Portfolio-Konfigurations-xls-Sheets
#PortfolioList = NULL
#Positions = NULL
#SecList = NULL  ##schnellzugriff auf den data.table  mit secStammdaten (aus xls-sheet)
#siehe auch Artikel zu data.table in
#http://www.r-bloggers.com/data-table-rocks-data-manipulation-the-fast-way-in-r/

if (len(data) < 6)
  data = new.env()  #... hierhin laden die T0 bis T2 .. als die Burschen die kein portfolioAlignment brauchen
BENCH = NULL
loadFrame = "2005::"

#-------------------------------------------------------------------------------------------

#############  Gib die Verm?gensaufstellung des Portfolios
# "Name","Contracts","entryDate","entryPrice"

########################################################################################
########################################################################################

if (F)
  ta5 = data.frame(PortfolioTicks_("ta5"))

########################################################################################
#lade aus den xls-sheets (alternative   SQL-DB die Portfoliodaten)
#customerData ist der Pfad zu den xls-sheets
#TODO:  lade aus SQL-DB
########################################################################################

loadPortfolios <-
  function(customerData1 =  sprintf("%s/Models", getwd()))
    #"o:/r/Nuggets/Eckhard/Models"
    
  {
    Positions <<-
      NULL
    SecList <<-
      NULL
    PortfolioList <<- NULL #erzwinge,dass wenn einer - dann
    cat("\nloadPortfolios from xls - files")
    #  if (len(SecList) ==0)  # globale def des Anlageuniversum
    {
      wb <-
        XLConnect::loadWorkbook(sprintf("%s/securities.xls", customerData1))#, create = TRUE)
      SecList <<- data.table(wb["SecList"])
      setkey(SecList, "Name")
      
      
      SecList$Name <<-
        toupper(SecList$Name) #alles keys werden gross geschrieben
      
    }
    #  if (length(PortfolioList)==0)
    {
      wb <-
        XLConnect::loadWorkbook(sprintf("%s/portfolioList.xls", customerData1))#, create = TRUE)
      PortfolioList <<- data.table(wb["PortfolioList"])
    }
    #  if (length(Positions)==0)
    {
      wb <-
        XLConnect::loadWorkbook(sprintf("%s/portfolios.xls", customerData1))#, create = TRUE)
      Positions <<-
        data.table(wb["Positions"])
      setkey(Positions, "Portfolio")
      Positions$Name <<- toupper(Positions$Name)
    }
    signalfile = sprintf("%s/signals.xls", customerData1)
    if (file.exists(signalfile))
    {
      wb <-
        XLConnect::loadWorkbook(sprintf("%s/signals.xls", customerData1))#, create = TRUE)
      Signals <<- data.table(wb["Signals"])
      setkey(Signals, "Signal")
      
    }
    else
      mP("there is no signal.xls  ")
    cat(" <-ok")
  }

#Globales Load
#if (!exists("PortfolioList") || !exists("Positions") || !exists("SecList"))
{
  Positions = NULL
  SecList = NULL
  PortfolioList = NULL #erzwinge,dass wenn einer - dann Alle gelesen werden müssen
  #  browser()
  loadPortfolios(customerData)
}

########################################################################################
########################################################################################

PortfolioTicks <- function(myPortfolio = "MMiksaHot")
{
#  browser()
  if (length(Positions) == 0)
  {
    wb <-
      XLConnect::loadWorkbook(sprintf("%s/portfolios.xls", customerData))#, create = TRUE)
    Positions <<-
      data.table(wb["Positions"])
    setkey(Positions, "Portfolio")
  }
  posList = Positions[which(Positions$Portfolio == myPortfolio)]
  #ISINS und YahooTicker  hinzumischen
  #  wb <- XLConnect::loadWorkbook(sprintf("%s/securities.xls",customerData))#, create = TRUE)
  #  Securities = data.table(wb["SecList"]); setkey(Securities,"Name")
  
  tickList  = cbind(
    posList$Name,
    "T1",
    posList$contracts,
    posList$entryDate,
    posList$entryPrice,
    posList$TrendIndi,
    posList$SwingIndi,
    posList$TsSwitch,
    posList$StopSys,
    posList$QualitySys,
    "noISIN",
    "noTicker",
    "Provider"
  )
  
  #browser() #T!
  setkey(posList, "Name")
  colnames(tickList) = c(
    "Name",
    "Ttype",
    "Contracts",
    "entryDate",
    "entryPrice",
    "TrendIndi" ,
    "SwingIndi",
    "TsSwitch",
    "StopSys",
    "QualitySys",
    "ISIN",
    "Ticker",
    "Provider"
  )
  
  tickList = data.table(tickList)
  if (len(SecList) == 0)
    # globale def des Anlageuniversum
  {
    wb <-
      XLConnect::loadWorkbook(sprintf("%s/securities.xls", customerData))#, create = TRUE)
    SecList <<- data.table(wb["SecList"])
    setkey(SecList, "Name")
  }
  #which (tickList$Name=="EUR")
  dellist = c()
  
  for (y  in 1:nrow(tickList))
  {
    x = toString((tickList[y])$Name)
    r = SecList[Name == x]
    #   r$YahooTicker
    #  str(r)
    ProviderTicker = getProviderTicker(x) #MMX
    
    if (!length(ProviderTicker) && !length(r$ISIN))
    {
      if (!length(PortfolioList[Portfolio == x]$Portfolio))
      {
        dellist = c(dellist, y)
        cat("\n No Symbol at SecList ", x)
      }
      else
      {
        tickList[y]$Ttype = "T2"
      }
      
    }
    else
    {
      cat("\n market ", x)
      
      tickList[y]$ISIN = toString(r$ISIN)
      tickList[y]$Ticker = x
      tickList[y]$Provider = toString(ProviderTicker$provider)
      #browser()
      #MM_TODO hier auch hinterlegen ob ein Papier  market ist - oder watchlist oder Fundamental  - oder auch  "tradeable"
      
    }
  }
  
  #  if (F)
  if (length(dellist) > 0)
    #MMcheck
    tickList = tickList[-dellist]
  return (tickList)
}
if (F)
{
  PortfolioTicks("WorldL1")
  PortfolioTicks("ta5")
  
  Signals$Signal[!is.na(Signals[["SSET1"]])]
}

#PortfolioTicks <- cmpfun(PortfolioTicks_) #compilier das Teil

###### gleich mal alle xls einladen  #MM_TODO
loadPortfolios(customerData)


#--------------------------------------------------------------------------------------------
train <- function()
{
  library(snowfall)
  library("foreach")
  library("doSNOW")
  
  sfInit(parallel = T, cpus = 4)
  registerDoSNOW(makeCluster(4, type = "SOCK"))
  sfLibrary(randomForest)
  
  x <- matrix(runif(500), 7000)
  y <- gl(2, 50)
  rep(250, 4)
  
  rf <-
    foreach(ntree = rep(250, 4),
            .combine = combine,
            .packages = "randomForest") %dopar%
    randomForest(x, y, ntree = ntree)
  rf
  
  sfStop()
}

###############################################################################################


T0 <- setRefClass(
  "T0",
  fields = list(
    name = "character",
    bench = "character",
    #isin = "ANY",
    frame = "character",
    ret = "ANY",
    prices = "ANY",
    #speichert die  mNorm zeitreihen   nö . die gehen nach t0data oder twatch
    tick = "character",
    provider = "character",
    market = "logical",
    #numeric
    tickInfo = "ANY",
    isin = "character",
    longname = "character",
    type = "character",
    t0data = "ANY",
    t0watch = "ANY"
  ),
  
  methods = list(
    ################################################
    save = function(file) {
      'Save the current object on the file
      in R external object format.
      '
      base::save(.self, file = file)
    },
    
    #################################################
    
    mPrint = function()
    {
      cat("T0")
      
      #browser()
      
      mP("name=%s,longname=%s,isin=%s", name, longname, isin)
      mP(
        "tick=%s,frame=%s,bench=%s,provider=%s,market=%s",
        iif(is.def(tick), tick, "no"),
        iif(is.def(frame), frame, "no"),
        iif(is.def(bench), bench, "no"),
        iif(is.def(provider), provider, "no"),
        iif(is.def(market), market, "no")
      )
      mP(
        "%d ret value , %d t0data,  dim prices %s ",
        iif(is.def(ret) , len(ret), 0),
        iif(is.def(t0data) , len(t0data), 0),
        dim(prices)
      )
      if (is.def(prices) &&
          !is.null(colnames(prices)))
        mP(colnames(prices))
      else
        mP("no prices")
      
    },
    #new = function(...)
    #{
    #  cat(" new P0 ")
    #  callSuper(...)
    #},
    
    ################################################
    
    prepData = function(data = .self$t0data,
                        frame = .self$frame,
                        visual = T)
    {
      envir = data
      
      ls(envir)
      #cat("\nprepData ",frame)
      #print(data$prices) ;print(envir$prices)
      # mL(data$prices)
      #bt.prep(envir, align='remove.na', dates=frame)
      
      #cat("\n frame ",frame)
      # .self$t0data = new.env()
      tickers = dataPrices(envir)
      
      cat("\npreData priceTickers: \n")
      print(tickers)
      
      preprepData(
        frame1 = frame,
        visual = visual,
        tickers = tickers,
        data = .self$t0data
      )
      #browser()
      #cat("--------------- after prepDat\n")
      mL(data$prices)
      #BUG
      #schreibt nach data ... data$prices
      .self$ret <-
        na.omit(ROC(envir$prices[frame])[frame])
      
      return(.self$ret)
    }
    
  )
)
## initialize and finalize methods

################################################
T0$methods(
  initialize =      function(name = "",
                             bench = "",
                             market = TRUE,
                             targetData = new.env(),
                             frame = "",
                             visual = T,
                             online = F,
                             isWatch = F,
                             ...) {
    #.self$name = "noName"
    #  callSuper(...)
    
    if (name == "" || is.na(name))
      return(NULL)
    name.org = name
    name = normaliseTickerName(name) #MM_BUG
    
    .self$type = "T0"
    .self$bench = bench
    .self$frame = frame
    .self$name = name
    
    
    if (length(SecList) == 0)
      #evtl. chashe mit SecList laden
    {
      wb <-
        XLConnect::loadWorkbook(sprintf("%s/Securities.xls", customerData))#, create = TRUE)
      SecList <<- data.table(wb["SecList"])
      setkey(SecList, "Name")
    }
    
    if (len(name) < 0 || name == "")
      return("no_name")
    
    .self$name = name
    
    if (isWatch)
      .self$t0watch = targetData
    else
      .self$t0data = targetData
    
    #die gloable BENCH auf den Parameter-Wert bench setzen
    if (length(.self$bench) && length(BENCH) == 0)
      BENCH <<- .self$bench
    if (length(.self$bench) == 0 && length(BENCH) == 0)
      BENCH <<- .self$name
    
    #MM_PROVIDER
    
    if (market)
      # es k?nnen Rohdaten via Datenprovider bezogen werden
    {
      cat("\n init P0 ", .self$name, " market:", market)
      
      tickInfo = as.list(SecList[SecList$Name == name.org])
      .self$isin = tickInfo$ISIN
      .self$longname = tickInfo$LongName
      
      provider = ""
      tick = ""
      doAdjust_ = T #soll  M_adjustOHLC() angewendet werden
      
      #browser()#MM_TODO
      
      
      if (is.def(tickInfo$YahooTicker))
        #default
      {
        provider = "yahoo"
        tick = tickInfo$YahooTicker
      }
      else
        if (is.def(tickInfo$stoxx))
        {
          provider = "stoxx"
          tick = tickInfo$stoxx
        }
      else
        if (is.def(tickInfo$FRED))
        {
          provider = "FRED"
          tick = tickInfo$FRED
        }
      else
        if (is.def(tickInfo$QuandlTicker))
        {
          provider = "Quandl"
          tick = tickInfo$QuandlTicker
        }
      else
        if (is.def(tickInfo$Sonstige))
        {
          provider = "Sonstige"
          tick = tickInfo$Sonstige
        }
      else
        if (is.def(tickInfo$ArivaTicker))
        {
          provider = "Ariva"
          tick = tickInfo$ArivaTicker
        }
      #browser()
      if (provider != "Sonstiges" &&
          is.def(tickInfo$Sonstige))
        #das K?rzel steht in beiden Securities.xls-Spalten
        doAdjust_ = F   #zumeist soll adjustiert werden
      
      # if (.self$name == "Rex")
      #   browser()
      
      #browser()
      
      #debug(mGetTickers)
      
      #browser()
      #marktdaten nacht targetData laden
      if (market && length(.self$name) > 0 &&
          provider != "")
      {
        s1 = sprintf("mGetTickers %s  %s", .self$name, provider)
        sag(s1)
        #  if (.self$name=="USDEUR")
        #undebug(mGetTickers)
        
        #if (.self$bench != "")
        #  quotes = list(c(.self$name,provider,tick),c(.self$bench))
        #else
        #browser()
        quotes = list(c(name, provider, tick))
        # browser(print("mmm1"))
        mGetTickers(
          FredTicker = quotes,
          data = targetData,
          frame1 = .self$frame,
          doAdjust = doAdjust_,
          online = online
        )#, data, frame1)
        #  .self$name = name
        #browser()  #T!  beiTo mGetTickers
        if (F)
        {
          ls(targetData)
        }
        #targetData$SMI_ETF
        #browser(mP("get %s",.self$name))
        if (.self$name %in% ls(targetData))
          dat = tryCatch(
            get(.self$name, envir = targetData),
            error = "MIST"
          )
        else
          dat = NULL
        #mP("CHECK")
        #browser()
        if (len(dat) == 0)
        {
          sag(
            "%s DownLoad-Bug at init To for %s",
            global_PortfolioName,
            toString(quotes),
            warte = T
          )
          #MM_TODO
        }
        #head(dat)
        #if (.self$name =="IFO")
        #     browser()
        #browser()
        #z.b.  fundamentaldaten
        if (!is.na(tickInfo$selectCol) &&
            (is.def(tickInfo$selectCol)))
          # #T! soll nur eine selectierte Zeitreih aus dem Datenpaket dat gew?hlt werden
          
        {
          #falls  - wie bei 0-normierten ifo-Zeitreihen negative Werte auftauchen m?ssen diese positiv gemachten
          #sonst funktioniert mROC bzw. mRendite() nicht ( v?llig falsches Bild der normierten Zeitreihe)
          selCol = as.numeric(tickInfo$selectCol)
          
          if (selCol > 0)
            #es soll nur die eine - in Securities.xls selCol ausgew?hlte spalte gew?hlt werden
          {
            #z.B. die nicht negative  ifo-3- spalte
            #head(dat)
            dat = dat[, selCol]
            datname = unlist(strsplit (colnames(dat), ".", fixed =
                                         T))[1]
            newCol = paste(datname, "Close", sep = ".")
            colnames(dat) = c(newCol)
            #auch fundamentaldaten d?rfen nicht negativ sein- sonst funktioniert die mNorm- nicht
            #weise im environment einer (neuen) Variablen .self$name den Wert dat zu:
            assign(.self$name, dat, envir = targetData)
            
            #browser()
            #  head(.self$t0data$prices)
            #  t0data$prices[,datname]
          }
          
        }
        if (len(dat) == 0 || min(dat, na.rm = T) < 0)
        {
          sag(sprintf(" %s  min(dat) <0", .self$name), T)
          #dat = dat-min(dat,na.rm=T)+1 #sonst stellt sich das mNorm-Bild wie bei ifo fr?her auf den Kopf
          
        }
        else
        {
          dat = m.clean0(dat) #MM_TODO  .. schon beim Dax lässt m.clean0 noch was durch was nicht mNorm kompatibel ist
          dat[dat == 0] <- 0.00000001  #damit mNorm klappt
          
          .self$prices <-
            try(mNorm(dat))
          # Cl(dat) #Cl(data[[.self$name]])
          .self$ret <- try(mROC(dat))
          mL(dat)
          
          
        }
        mBench = .self$bench
        
        if (bench != "" &&
            len(mBench) && mBench != "" &&  !exists(mBench, envir = targetData))
        {
          T0$new(
            name = mBench ,
            targetData = targetData,
            frame = frame,
            visual = visual,
            online = online,
            isWatch = T
          )
        }
        {
          #if (length(mBench) && mBench != "" && exists(mBench,envir=targetData))
          #{
          # benchP = Cl(targetData[[mBench]])
          #  .self$ret = na.omit(ROC(mbox(benchP, .self$prices )))
          #}
          #else
          #  .self$ret <- na.omit(ROC(.self$prices))
          
          #tempData = new.env()
          
          
          #lapply(ls(targetData),FUN=function(x) assign(x, get(x,envir=targetData),envir=tempData))
          
          #preprepData(tickers=ls(tempData),data=tempData,frame1=frame,visual=visual)
          #.self$prices = m.clean0(tempData$prices)
          #.self$prices <- na.omit(mRendite(.self$ret))
          #.self$ret <- na.omit(ROC(tempData$prices))
          
          
        }
        #browser()
        #.self$ret<- na.omit(ROC(na.omit(.self$prices[,grep("Close",colnames(.self$prices),ignore.case=T)])))
        
        
        if (visual)
        {
          #new_Win()
          
          #browser()#T1
          head(.self$prices)
          xyplot(.self$prices)
          
          if (F)
          {
            ers.gnp <-
              ur.ers(
                .self$ret,
                type = "DF-GLS",
                model = "trend",
                lag.max = 4
              )
            plot(ers.gnp)
            title(
              main = .self$name,
              adj = 0,
              outer = TRUE,
              line = -1
            )
          }
          
          chartSeries(.self$prices, name = .self$name)
        }
        
      }
    }
    else
      #no market or no provider, vermutlich ein T2-Objekt .. hol Dir von hier die Zeitreihe
    {
      cat("\n init P0 ", .self$name, " no market:")
      
      .self$isin = "noISIN"
      .self$longname = sprintf("T2Portfolio%s", .self$name)
      
      
      if (market && length(.self$name) > 0)
      {
        sag("no orignial data given")
        .self$prices <-
          NULL # ist auf T0-Ebene nicht bekannt
      }
    }
  },
  finalize = function() {
    cat("finish P0")
  }
)

T0$methods(show = ########################################################
           function(frame = .self$frame,
                    newWin = F,
                    visual = F)
           {
             data = .self$t0data
             cat("show T0  ", .self$name, " at ", frame, "\n")
             
             if (length(.self$bench))
               mBench = .self$bench
             else
               mBench = BENCH
             
             #if (len(mBench) && mBench != ""&& !exists(mBench,envir=data))
             #   T0$new(name = mBench ,targetData = data)
             
             #.self$prices <- Cl(data[[.self$name]])
             #browser()
             if (newWin)
               new_Win()
             #browser()
             Auflegedatum = fromTo(.self$prices)[1]
             
             #XX<<- na.omit(.self$ret[,grep("Close",colnames(.self$ret),ignore.case=T)])
             XX = .self$ret
             try(chart.RiskReturnScatter(XX,
                                         main = sprintf("RiskReturn since %s", Auflegedatum),
                                         colorset = rainbow8equal))
             #plotNormedPrices(cumsum(.self$ret))
             #chart.CumReturns(.self$ret,main=sprintf("%s vs %s",.self$name,mBench),legend.loc="bottom")
             
             
             mPlot(XX)
             #try(xyplot(XX))
             #MplotNormedPrices(XX)
             
             mL(.self$ret)
             .self$mPrint()
           })


T0$methods(getOrgData = #############################################
           function(name = .self$name,
                    frame = .self$frame,
                    newWin = F,
                    visual = F)
           {
             #browser()
             #  data=.self$t0data
             #  typeData = .self$member[[name]]
             #browser()
             prices = na.omit(.self$prices[frame])
             #plot(cumsum(((na.omit(mROC(prices))))))
             if (visual)
               try(mPlot(prices))
             # print(typeData)
             
             
             return(prices)
           })

###############################################################################################
#TrendIndi  SwingIndi  TsSwitch  StopSys	QualitySys
###############################################################################################
T1 <- setRefClass(
  "T1",
  contains = "T0",
  fields = list(
    t1Par = "ANY",
    #die Parameter des Indicators .. werden dynamisch gesetzt
    TrendIndiPar = "ANY",
    SwingIndiPar = "ANY",
    TsSwitchPar = "ANY",
    StopSysPar = "ANY"
    #QualitySysPar= "character"
  )
  ,
  methods = list(
    ########################################################
    save = function(file = NULL) {
      'Save the current object on the file
      in R external object format.
      '
      
      if (is.null(file))
        file = sprintf('T2data/%s.Rdata', PortfolioName)
      base::save(.self, file = file)
    },
    mPrint = function()
      ####################################
    {
      cat("T1")
      callSuper()
    },
    #MMA ################## update des Portfolios
    
    update_t1 = function(Ti,
                         frame = .self$frame,
                         data = .self$t0data)
      #ist ein t1 oder t2 - objekt
    {
      cat("\nupdate_t1 of ", Ti$name)
      #print(str(Ti))
      
      .self$ret = mROC(data$prices)
      #update der technischen Modelle
      thisRet = (Trade1(
        Ti = Ti,
        Ttype = Ti$type,
        frame = frame,
        data = data,
        ret = .self$ret
      ))  #die closings
      # Zuweisen der Guv zum DatenSlot- damit auch eine GuV sich wie ein
      #normales Zeitreihen objekt anf?hlt
      if (Ti$type == "T2")
        assign(Ti$name, dataFrameOfGuv(Ti)  , envir = .self$t0data)
      return (thisRet)
    }
  )
)
############################################################################
## initialize and finalize methods
#TrendIndi = "SMA",SwingIndi="RSI",TsSwitch ="TSI", StopSys="trailingStop1",QualitySys="Kelly",
T1$methods(
  initialize =
    function(name = "",
             bench = "Dax",
             market = TRUE,
             targetData = new.env(),
             frame = "",
             visual = F,
             t1_InitParameter = c(),
             online = F,
             ...) {
      #name="", bench="Dax", market = TRUE, targetData=new.env(),frame="",visual=T, ..
      callSuper(
        name = name,
        bench = bench,
        market = market,
        targetData = targetData,
        frame = frame,
        visual = visual,
        online = online,
        ...
      )
      .self$type = "T1"
      #browser()
      .self$t1Par = t1_InitParameter
      
    }
)

#########################################################################

T1$methods(
  show =
    function(frame = .self$frame,
             newWin = F,
             visual = T)
    {
      #browser()
      data = .self$t0data
      cat("show T1  ", .self$name, " at ", frame, "\n")
      
      if (visual)
        callSuper(frame, newWin, visual = T)
      cat(.self$name , " Benchmark: ", .self$bench)
      print(.self$t1Par)
      .self$t0data
    }
)



###############################################################################################
############# T2

#speicher ein ganzes Portfolio - kann aus T1,T2,T2,... bestehen
#die Zeitreihen der EinzelSerien werden .. einheitlich ausgerichtet in t0data
#kopiert
#die eigene GuV- Kurve wandert dann nach  data
#



T2 <- setRefClass(
  "T2",
  contains = "T1",
  fields = list(
    PortfolioName = "character",
    tickList  = "ANY",
    initPortfolio = "ANY",
    #die zum PortfolioName passenden Zeilen aus der Portfolio.xls
    #t0data="ANY",   # das data-environment wie beim sysinvestor    - ist schon in seiner T0-Vaterklasse
    Portfoliomanager = "character",
    #der AllocaAlgorithmus
    Auflegedatum = "character",
    Bisdatum = "character",
    PortfolioSpec = "ANY",
    #die zeile aus der PortfolioList.xls
    member = "ANY"  #das Environment in das die Portofolio-Daten als T1 gehen
  )
  ,
  methods = list(
    #########################################################
    save = function(file = NULL) {
      'Save the current object on the file
      in R external object format.
      '
      #browser()
      if (is.null(file))
        file = sprintf('T2data/%s.Rdata', PortfolioName)
      base::save(.self, file = file)
    },
    mPrint = function()
    {
      cat("is a T2 portfolios")
      callSuper()
    }
  )
)

T2$methods(getData = ###############################################
           function(name = "",
                    frame = .self$frame,
                    newWin = F,
                    visual = F)
           {
             data = .self$t0data
             typeData = .self$member[[name]]
             prices = na.omit(data[[name]][frame])
             
             if (visual)
             {
               try(MplotNormedPrices((na.omit(mROC(Cl(
                 prices
               ))))))
               print(typeData)
             }
             
             return(prices)
           })


if (F)
{
  ta5 <<- T2$new(PortfolioName = "RenditePlus", bench = "Dax")
  ls(ta5$t0data)
  class(ta5$member[["Dax"]])
  
  ls(ta5$t0data)
  ta5$t0data$prices
  ta5$t0data$weight
  
  ta5$tickList
  #ta5$prices
  ta5$PortfolioName
  ta5$initPortfolio
  ta5$Auflegedatum
  ta5$Bisdatum
  ta5$PortfolioSpec
  dax = ta5$member[["Dax"]]
  dax$type
  str(dax)
  
  ls(dax$item)
  dax$item$prices
  dax$item$mPrint()
  ls(dax$item)
  daxT1 = dax$item
  dax$t1Par
  dax$t0data
  
  Xdata = new.env()
  t0rex <-
    T0$new(
      name = "Rex",
      bench = "Dax",
      targetData = new.env(),
      visual = F
    )
  t0rex$market
  t0rex
  
}
#ta5$t0data$prices
#head(ta5$prices)
#head(ta5$getOrgData("Dax"))


T2$methods(getOrgData = ##############################################
           function(name = "",
                    frame = .self$frame,
                    newWin = F,
                    visual = F)
           {
             if (name == "")
               return(.self$t0data$prices[frame])
             
             member = .self$member[[name]]
             item = member$item
             #  prices =na.omit(item$prices[frame])
             #
             
             return(item$getOrgData(name, frame, newWin, visual))
             
             
           })


###################################################################################
########### f?ge eine neues Objekt ti der Klasse ti$tpye der member-dataTable hinzu
#TrendIndi  SwingIndi  TsSwitch	StopSys	QualitySys
###################################################################################

T2$methods(
  show =
    function(frame = .self$frame,
             newWin = F,
             visual = T)
    {
      cat("show T2  Portfolio ", PortfolioName, " at ", frame, "\n")
      #browser(mP("kkkkkkkkkkkkkkkkk"))
      print(.self$PortfolioSpec)
      #browser()
      dataPrices(.self$t0data)
      mL(.self$t0data$prices)
      
      ret <- .self$ret[frame]#.self$t0data.ret[frame]
      mL(ret)
      if (newWin && visual)
        new_Win()
      if (visual)
      {
        try(chart.RiskReturnScatter(ret,
                                    main = sprintf("RiskReturn since %s", Auflegedatum),
                                    colorset = rainbow8equal))
        
        MplotNormedPrices(ret)
        #browser()
        # mPlot(ret)
        #mPlot(mNorm(.self$t0data$prices[frame]))
      }
      #mL(ret)
      #print("\nSpec:");print(.self$PortfolioSpec)
      print(.self$initPortfolio)
    }
)


###################################################################################
########### f?ge eine neues Objekt ti der Klasse ti$tpye der member-dataTable hinzu
#TrendIndi  SwingIndi  TsSwitch  StopSys	QualitySys
###################################################################################

T2$methods(
  update =
    
    function(global_StartDate_ = Sys.Date(),
             frame = .self$frame,
             newWin = F,
             visual = T)
    {
      global_StartDate <<- global_StartDate_
      cat("update T2  Portfolio ", PortfolioName, " at ", frame, "\n")
      
      #pass1 die t1
      for (tname in ls(.self$member))
      {
        ti = get(tname, envir = .self$member)
        if (ti$type == "T1")
        {
          Ti = ti$item
          Ti$update_t1(Ti, frame, .self$t0data)
        }
      }
      #pass2 die t2 - Portfolios
      for (tname in ls(.self$member))
      {
        ti = get(tname, envir = .self$member)
        
        if (ti$type == "T2")
        {
          Ti = ti$item
          Ti$update_t1(Ti, frame, .self$t0data)
        }
      }
      
    }
)


#str(t2)
###################################################################################
# Initialisiert ein ganzes Portfolios gem den beiden Steuerdateien
#Portfolio.xls und PortfolioList.xls
#   l?dt eine ganze Liste von t1  - Objekten (basierend auf t0)
###################################################################################

## initialize and finalize methods
T2$methods(
  initialize =
    
    function(PortfolioName = "noPortfolioName",
             bench = "",
             market = F,
             frame = "",
             targetData = new.env(),
             visual = F,
             online = F,
             ...) {
      .self$name = sprintf("%s_GUV", PortfolioName) #isPortfolio"
      .self$PortfolioName = PortfolioName
      
      global_PortfolioName <<- PortfolioName
      
      #--liest die Portfolio-spezifikaion aus dem excel-Workbook  "portfolioList.xls"
      
      if (length(PortfolioList) == 0)
      {
        wb <-
          XLConnect::loadWorkbook(sprintf("%s/portfolioList.xls", customerData))#, create = TRUE)
        PortfolioList <<- data.table(wb["PortfolioList"])
      }
      .self$PortfolioSpec = PortfolioList[PortfolioList$Portfolio ==
                                            .self$PortfolioName]
      # browser() #MMX
      if (nrow(.self$PortfolioSpec) == 0)
      {
        
        mP("##### > BUG  Portfolio %s is not defined at PortfolioList.xls",
           .self$PortfolioName)
        mP(sprintf("%s/portfolioList.xls", customerData))
        return(NULL)
        
      }
      print(.self$PortfolioSpec)
      #?bertrage die Benchmarkdefinition
      cat("\n definied Benchmark ", .self$PortfolioSpec$Benchmark)
      #?bernimm Werte aus der Spe (PortfolioList.xls -die evtl. auch ?berschrieben werden k?nnten)
      
      if (is.na(.self$PortfolioSpec$Benchmark))
        .self$PortfolioSpec$Benchmark = ""
      bench = .self$bench = .self$PortfolioSpec$Benchmark
      
      PortfolioSpec <<- .self$PortfolioSpec
    
      
      if (len(bench) > 0  && bench != "")
      {
        mP(
          "overwriting bench vorm PortfolioList.xls %s by arg bench= %s",
          .self$PortfolioSpec$Benchmark,
          bench
        )
        .self$bench = bench
      }
      
      .self$Portfoliomanager = .self$PortfolioSpec$Portfoliomanager
      
      cat("\n PortfolioManager-Algorithmus: ", .self$Portfoliomanager)
      #Aufruf der t1-Init f?r das Portfolioselber - ist ja auch ne t1 zeitreihe die wiederum
      #MM_TODO: - alle Inikatoren (IFO) .. sollten nach $mydata geladen werden
      callSuper(
        name,
        bench = "",
        market = F,
        targetData,
        frame = frame,
        visual = visual,
        t1_InitParameter = c(),
        ...
      )
      
      #scan()
      .self$type = "T2"
      .self$member = new.env()# list()#data.table(name=c("x"),type=(c))
      .self$TrendIndiPar = NULL
      .self$SwingIndiPar = NULL
      .self$TsSwitchPar = NULL
      .self$StopSysPar = NULL
      
      if (length(.self$PortfolioName))
      {
        cat("\n init P2 for ", .self$PortfolioName, "\n")
        #browser()
        .self$initPortfolio <-
          PortfolioTicks(.self$PortfolioName) #liest aus der Portfolio.xls
        .self$tickList <- .self$initPortfolio$Name
        print(.self$tickList)
        #browser() #MMX
        #.self$weights = NULL
        
        #  lapply(.self$tickList, FUN=function(x) mGetTickersNew(toString(x)))#, data= data, From=loadFrame)
        #    for (tick_ in .self$tickList) try(T0$new(market=TRUE,name="Dax"))#, data= data, From=loadFrame)
        
        alltickers = .self$tickList
        tickers = alltickers#[-which(alltickers=="EUR")]
        
        #print("tickers ",tickers)#," all ",alltickers)
        t0data <- new.env()
        #for (tick_ in tickers)
        #{
        #  cat("\ntick ",tick_)
        #}
        
        if (F)
        {
          # getreu dem Konzept von SysInvestor
          loadData(tickers, data = t0data, frame1 = frame)
          #tickers = ls(t0data);
          tickers = dataPrices(t0data)
          .self$t0data.ret <-
            preprepData(
              frame1 = loadFrame,
              visual = F,
              tickers = tickers,
              data = t0data,
              visual = visual
            )
          tickers = t0data$symbolnames
          
          
          for (i in tickers)
            cat(i, format(index(t0data[[i]][1, ]), '%d%b%y'), '\n')
        } else
        {
          #tickers= alltickers
          objectId = 0
          # .self$initPortfolio = data.frame(.self$initPortfolio)
          
          print(tickers)
          # lade die Wertpapier-Zeitreihen der Portfoliobestandteile
          #
          #browser()
          for (tick_ in tickers)
          {
            objectId = objectId + 1
            
            cat("\n make p0 ", tick_, " ", objectId, "\n of all: \n", tickers)
            
            t1_InitParameter = .self$initPortfolio[Name == tick_]
            is_market = length(.self$initPortfolio[Name == tick_]$Ticker) >
              0 && length(.self$initPortfolio[Name == tick_]$Provider) > 0
            
            if (.self$initPortfolio[Name == tick_]$Ttype != "T1")
            {
              is_market = F
              cat("\n no T1 ", tick_)
            }
            # wenn es nicht in der Securities.xls steht ist kein Markt-Titel sonder ein berechnetes Ti - Teill
            #M! at T2-Init
            #browser() #T  in init T2
            #bench soll "" sein, sonst holt jedes t1 sich die gleiche Bench..
            
            InitPortfolio <<- .self$initPortfolio
            #lade lediglich die Zeitreihen ins t0data-Environment
            t1_ <-
              T1$new(
                name = toString(tick_),
                bench = "",
                market = is_market,
                targetData = t0data,
                visual = visual,
                online = online,
                frame = frame,
                t1_InitParameter = t1_InitParameter
              )
            
            #MM_TODO der T2-Init: wie werden Inikatoren (IFO...) von handelbaren Papieren unterschieden ?
            if (.self$initPortfolio[Name == tick_]$Ttype == "T2")
            {
              #MM_TODO der T2-Init wenn ein T2 als Teil des Portfolios geladen werden soll
              cat("\nT2-pricing for ", tick_, "comes later\n")
              #da noch keine gGuV-Kurve f?r das T2-Objekt vorliegt wird hier ein leerer Rahmen angelegt
              #assign(tick_, NULL  ,envir=t0data)
            }
            #  .self$initPortfolio[objectId]$Id=objectId
            # .self$initPortfolio[objectId]$T1=t1_
            #die  t1 Objekt -Liste
            assign(tick_,
                   c(
                     Id = objectId,
                     type = t1_$type,
                     item = t1_
                   ),
                   envir =  .self$member)
            #.self$initPortfolio[Name==tick_]$T1=t1_
          } #<<-for
          
          if (bench != "")
          {
            #browser() #T! initT2 - Bench hinzuladen
            tick_ = bench
            t1_ <-
              T1$new(
                name = tick_,
                bench = "",
                market = is_market,
                targetData = t0data,
                visual = visual,
                online = online,
                frame = frame,
                t1_InitParameter = t1_InitParameter
              )
            assign(tick_,
                   c(
                     Id = objectId,
                     type = t1_$type,
                     item = t1_
                   ),
                   envir =  .self$member)
            #t0data$BENCH=bench
          }
          
          cat("\n preData ->t0data ... ")
          #browser(text="vor t1_$prepData")
          t1_$prepData(data = t0data, visual = visual)  # wochendaten und preise werden erzeugt
          # lapply(t0data, function(x) colnames(x) = normColnameS(x))
          
          #browser()
          
          #evtl. Leerzeichen aus den colnames entfernen
          
          try(lapply(
            t0data$symbolnames,
            FUN = function(symb) {
              #  print(symb)
              colnames(t0data[[symb]]) =
                lapply(
                  colnames(t0data[[symb]]),
                  FUN = function(x)
                    paste(sapply(strsplit(x, "\\."), trim), collapse = ".")
                )
            }
          ))
          
          
          tickers = t0data$symbolnames
          for (i in tickers)
            cat(i, fromToS(Cl(t0data[[i]])), '\n')
        }
        
        cat ("\n", .self$tickList, "\nhas\n", tickers)
        
        t0data$BENCH = bench
        .self$bench = bench
        .self$tickList = tickers
        .self$t0data = t0data
        
        
        
        sag("###### ")
        print(head(t0data$prices))
        #      .self$ret = na.omit(ROC(t0data$prices))
        .self$ret =  mROC(t0data$prices)
        #.self$t0data.nPrices = mRendite(.self$t0data.ret)
        .self$Auflegedatum = fromTo(t0data$prices)[1]
        .self$Bisdatum = fromTo(t0data$prices)[2]
        
        cat("\n member: ")
        #browser()
        
        .self$show(visual = visual)
        
        
        mP("saveRDS %s", sprintf("%s.data", .self$PortfolioName))
        data <<- t0data
       # browser()
        #save(list=ls(t0data),file=sprintf("%s.data",.self$PortfolioName))
        saveRDS(t0data, file = sprintf("%s.rds", .self$PortfolioName))
        try(data.Info(t0data, sprintf("%s.xls", .self$PortfolioName)))
        
      }
    },
  finalize = function() {
    cat("finish P0")
  }
)

if (F)
  DaxMdax <<-
  T2$new(
    PortfolioName = "DaxMdax",
    bench = "DAX",
    visual = F,
    online = F
  )

#########################################################################################################################


#########################################################################################################################
#########################################################################################################################


if (F)
{
  #auf einen Schlag wird das gesamte Portfolio gem.  xls- Steuerung eingeladen
  
  #es werden alle Objekte als t1- Markt- Objekte geladen (d.h. mit P0-Init)   (reine P0-Objekte werden mit BuyHold/NoTrailing  geladen)
  # es gibt dann rohdaten (gem. sysinvestor in  $t0data  und  die t1-Objekte - samt beschreibung und $member
  # dann kann er sortiert nach ti-Typ s?mtliche Modelle updaten
  
  #  Was fehlt:  Hinzuf?gen von t3 - Modellen (ohne Markdaten)
  
  ta5 <- T2$new(PortfolioName = "ta5", visual = F)
  ta5
  ta5$update()
  
  new_Win()
  str(ta5)
  ta5$show()
  ta5$mPrint()
  ta5$member[["Rex"]]
  ls(ta5$member)
  ta5$member[["Dax"]]
  ls(ta5$t0data)
  
  ta5$getData("Dax")
  
  new_Win()
  r =  ta5$getData("Dax", visual = T)
  
  
  
}

if (F)
{
  plot.table.test()
  # CRB = mget.CRB()   geht erst hablautomatisch muss immer noch selber xls->xlsx wandelnd
  tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU')
  tickers = spl('sx5r,sg2r,sv2r,Rex')
  BENCH = "Rex"
  data = new.env()  #das Environment f?r Einzel-Titel - Achtung: T2-Portfolios haben daf?r was eigenes
  
  # w=XLConnect::loadWorkbook(sprintf("%s/Securities.xls",customerData))
  
  windows()
  t1dax <- T1$new(name = "Dax",
                  bench = "Rex",
                  visual = T)
  t0Rex <-
    T0$new(
      name = "Rex",
      bench = "Dax",
      frame = "2006::2009",
      visual = T
    )
  t0xly <- T0$new(name = "")
  
  t0sx5r <- T0$new(name = "sx5r", bench = "Dax")
  ls(t0sx5r)
  t0sx5r$bench
  ls(t0sx5r$t0data)
  str(t0rex)
  t0sx5r$t0data$sx5r
  t0sx5r$bench
  mPlot(t0sx5r$ret)
  #erzeuge eine ganze Liste von t0-Elementen
  data = new.env()
  T0items = lapply(
    tickers,
    FUN =
      function(x) {
        n = sprintf("t0%s", x)
        
        mP("################## NEW   %s     #################", n)
        
        assign(n, T1$new(
          name = x,
          bench = "DAX",
          visual = T
        ), envir = data)
      }
  )
  
  #t0rex <- T0$new(name = "CRB",bench="Rex")
  BENCH
  ls(data)
  
  t0Rex$name
  t0Rex$show()
  #oder noch k?rzer:
  
  t0sx5r
  data$prices
  ls(data)
  t0sx5r$type
  
  
  ls(data)
  ####################################BUG
  #debug(t0Rex$prepData)
  t0Rex$prepData(data, "2006:2008", visual = T)
  ls(data)
  t0Rex$t0data
  t0Rex
  bt.prep(data, align = 'remove.na', dates = "2006")
  data$prices
  
  tail(data[["Rex"]])
  #Vorbereitung:  Ausrichtung und Cl -> data$prices,  weekly, monthly data$symbolnames:
  #t0dax$prepData("")  #kann bisher nicht mehrfach aufgerufen werden
  # alternative:     xx=mReturn(mbox(Cl(Dax),Cl(Rex))) ## geht bisher aber nur f?r 2
  data$prices
  t1 <- T1$new(name = "Dax",
               TrendiIndi = "Omega",
               SwingIndi = "RSI")
  windows()
  t2 <-
    T2$new(
      PortfolioName = "RenditePlus",
      TrendIndi = "Omega",
      SwingIndi = "RSI",
      Stop = "maxDrawDown"
    )
  ta5 <- T2$new(PortfolioName = "ta5", visual = F)
  #Portfolio laden , nach .self$t0data  - erst seine GuV wird dann wieder nach data[] geschrieben
  head(data[["Rex"]])
  
  new_Win()
  t3 <- T2$new(PortfolioName = "AA")
  
  t3$type
  ls(t3$t0data)
  
  Mem = t3$member
  ls(Mem)
  t3$show("2006", T)
  
  print(t3$PortfolioSpec)
  
  
  head(t3$t0data$prices)
  
  ############## # Buy & Hold
  t3$t0data$weight[] = 1
  buy.hold = bt.run(t3$t0data)
  
  t3$t0data$prices
  
  # MA Cross
  sma = bt.apply(t3$t0data, function(x) {
    SMA(Cl(x), 200)
  })
  t3$t0data$weight[] = NA
  t3$t0data$weight[] = iif(t3$t0data$prices >= sma, 1, 0)
  sma.cross = bt.run(t3$t0data, trade.summary = T)
  
  new_Win()
  plotbt.custom.report(sma.cross, buy.hold)
  
  #  aa.omega.test()
  mL(t3$t0data$prices)
  mL(ROC(na.omit(t3$t0data$prices)))
  
  
  
  # compute.drawdowns( portfolio.equity, make.plot = FALSE )
  
  
  #Portfolio von Portfolios:  der return eines T2 wird analog zum P0 in ein t4 integriert
  
  
  
  
  #liest aus den
  l = PortfolioTicks("RenditePlus")$Name
  l1 = l[-which(l == "EUR")]
  l
  l1
  rm(Rex)
  #try(lapply(l, FUN=function(x) T0$new(name=toString(x))))
  #for(i in l)
  #  T0$new(name=toString(i))
  #undebug(rmSymbol)
  #undebug("loadData")
  tickers = c("Daxyy", "Rex")
  t0data = new.env()
  loadData(tickers, data = t0data)
  tickers = ls(t0data)
  tickers = t0data$symbolnames
  
  
  preprepData(frame1 = frame,
              visual,
              tickers = ls(data),
              data = data)
  preprepData(tickers = ls(data),
              data = data,
              frame1 = "")
  for (i in data$symbolnames)
    cat(i, format(index(t0data[[i]][1, ]), '%d%b%y'), '\n')
  .self$ret <- preprepData(loadFrame, visual = T, tickers, data = data)
  
  #debug(mGetTickers)
  mGetTickers(FredTicker = list(c("sx5r", "stoxx")),
              data = data,
              frame1 = loadFrame)#, data, frame1,doAdjust=T)
  
  #x <- xts(rnorm(10), Sys.Date()+1:10)
  #period.apply
  #runSum, runCov, runSD
  
  #getSymbols("USD/EUR",src= "oanda")
  #getSymbols("CHF/EUR",src= "oanda")
  
  #m=makeReturnFrame(c("CHFEUR","USDEUR"))
  
  #m
  #UE= data.frame(USDEUR)
  
}


Save <-
  function(t2)
    #MM_TODO :  komisch: wenn ich in einer Func save rufe .. kann ich das Teil nach nichtmehr mit Load laden
  {
    #save(file=sprintf('T2data/%s.Rdata',t2$PortfolioName),list = "t2",envir= parent.frame())
    # save(t2,file=sprintf('T2data/%s.Rdata',t2$PortfolioName) )
  }

Load <- function(t2String)
{
  load(file = sprintf('T2data/%s.Rdata', t2String),
       envir = parent.frame())
}
#################################################################################
##################################################################################

print("########### load TradeClasses.R")
if (F)
  list_R_functions('MLib/TradeClasses.R')
