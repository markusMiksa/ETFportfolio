################################################################################
#   TSA- BASIS- DEMO 1
################################################################################
#Startup der Umgebung
if (F)
{
rm(list= ls());    
source  ("MLib/TSA.r"); 
define.Globals()
loadPortfolios()

#IndexEuropa
#IndexUSA
#IndexDeutschland

#SektorSpdrUSA

#mchart(scale.one(data$prices))
#mchart(mNorm(data$prices),main="m")

#---------------------------------------
#Risikofreie Position
SAFE= "BARCLAYS"

### Universum auswählen und einladen 

load("stoxx_branchenpure"); 
data$universe="stoxxBra"
define.Globals(); SAFE="BARCLAYS"
  
load("MWorld3");
load("StoxxComp")
define.Globals();
data$universe= "MWorld3"; SAFE="BARCLAYS"


data.info(data); 
#...TIMING  ... viele Timing-Systeme

SAFE="BARCLAYS"
models = run_all_signal(SAFE=SAFE,do.assemble.Signals=F,pdf=T) #rechne alle timing modelle  
load(file=sprintf("models/%s/HuA_F.data",dataSet))
#kommen recht unterschiedlich mit dem Universum zurecht.  SXEBP  (oil+Gas)
m=models.Results(models)  

#...TIMING  .. nur ein gutes signal
all = indi.Generic("signal.lm", global_arg, par=list(win=150),
                   visual=T, TRAINSYM =-1,safe=SAFE,  
                   S.arg=list(ranking.fn="rank.slope300",nTop.q=1.8,kTop.q=1.4,do.assemble.Signals=F),pdfFile="f"  )

models$signal.lm= x2.1=indi.Generic("signal.lm", global_arg, par=list(win=150),visual=T, TRAINSYM =-1,safe=SAFE,pdfFile="F")

models$signal.lm= x2.1=indi.Generic("signal.lm", global_arg, par=list(win=150),visual=T, TRAINSYM =-1,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile)

all = indi.Generic("signal.MA.3", global_arg, par=list(zlemaN=10,slow=60,fast=10),
                   visual=T, TRAINSYM =-1,safe=SAFE,  
                   S.arg=list(ranking.fn="rank.slope300",nTop.q=1.8,kTop.q=1.4,do.assemble.Signals=F),pdfFile="f"  )

all = indi.Generic("signal.MA.3", global_arg, par=list(zlemaN=10,slow=60,fast=10),
                   visual=T, TRAINSYM =-1,safe=SAFE,  
                   S.arg=list(ranking.fn="rank.slope300",nTop.q=1.8,kTop.q=1.4,do.assemble.Signals=F),pdfFile="f"  )

#.. TIMING .. nur ein signal .. weniger gutes System: 
all = indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=200),
                   visual=T, TRAINSYM =-1,safe=SAFE,  
                   S.arg=list(ranking.fn="rank.slope300",nTop.q=1.8,kTop.q=1.4,do.assemble.Signals=F),  experiment="stoxxBraSafeTfaber.baseSslope300"  )

##.. und hier der algorithmisch verbesserte dyn.faber
all = indi.Generic("signal.Faber.dyn.hysterese", global_arg, par=list(sma.w=200),
                   visual=T, TRAINSYM =-1,safe=SAFE,  
                   S.arg=list(ranking.fn="rank.slope300",nTop.q=1.8,kTop.q=1.4,do.assemble.Signals=F),  pdfFile="ff"  )

##.. und hier der eher schlechte
all = indi.Generic("signal.mom", global_arg, par=list(sma.w=200),
                   visual=T, TRAINSYM =-1,safe=SAFE,  
                   S.arg=list(ranking.fn="rank.slope300",nTop.q=1.8,kTop.q=1.4,do.assemble.Signals=F),  pdfFile="ff"  )

##.. und hier der eher schlechte
all = indi.Generic("signal.Price.itp", global_arg, par=list(sma.w=250),
                   visual=T, TRAINSYM =-1,safe=SAFE,  
                   S.arg=list(ranking.fn="rank.slope300",nTop.q=1.8,kTop.q=1.4,do.assemble.Signals=F),  pdfFile="ff"  )



#... SELECTION+ALLOCATION
A.SA (min.risk.fns = A.SA.default$A.arg$min.risk.fns, 
      S.arg=TSA.default$S.arg,
      experiment="A_StoxxBra",  max.product.exposure = 0.8, safe=SAFE) 


#... SELECTION+ALLOCATION
A.SA (min.risk.fns = A.SA.default$A.arg$min.risk.fns, 
                     S.arg=TSA.default$S.arg,
                     experiment="MWorld3",  max.product.exposure = 0.8, safe=SAFE) 


#########################################

#MV ist super  ... wenn Barclays als SAFE da ist !!

A.SA (min.risk.fns = TSA.default$A.arg$min.risk.fns, 
                 S.arg=list(ranking.fn="rank.slope300",nTop.q=1.8,kTop.q=1.4),
                 experiment="euRank.all",  max.product.exposure = 0.8, safe=SAFE) 

#.... nun aber mal ohne SAFE .. die gefilterten bleiben viel stabiler - weil sie timing-scills haben

#und wie sieht das ohne BARCLAYS aus ?
data.rm(data,spl("BARCLAYS")) ;data$universe="stoxxBraNOSAFE"
define.Globals(); SAFE=data$BENCH

A.SA (min.risk.fns = TSA.default$A.arg$min.risk.fns, 
      S.arg=list(ranking.fn="rank.slope300",nTop.q=1.8,kTop.q=1.4),
      experiment="Euro_slope300_noSAFE",  max.product.exposure = 0.8, safe=SAFE, onlyUsr=F) 

#.......................
#Test eines hochgelobten Trend-Stärke-Indikators als Filter

reset.cashed.rank("rank.TSI")
#berechne den TSI
position.score = TSIsymbol(data,n=15) [fromToS(data$prices)]
#normiere seine namen
colnames(position.score) = colnames(data$prices)
#schreib ihn in den cash
x=get.rank( "rank.TSI1",prices=position.score,xtra="write")#schreibt

#so sieht er aus um vergleich zum slope
x=get.rank( "rank.TSI1",prices=data$prices)
x2=get.rank("rank.slope300",prices=data$prices)
sym="SX5R"
mPlots(merge(x[,sym],0),merge(x2[,sym],0))

#aber seine ranking-quality ist viel schlechter
A.SA (min.risk.fns = TSA.default$A.arg$min.risk.fns, 
      S.arg=list(ranking.fn="rank.TSI1",nTop.q=1.8,kTop.q=1.4),
      experiment="euRank.all",  max.product.exposure = 0.8, safe=SAFE, onlyUsr=T) 
#.......................

#.......................
#Test es probMom- Indikators als ranking-score
load("stoxx_branche");
define.Globals(); SAFE="BARCLAYS"

A.SA (min.risk.fns = TSA.default$A.arg$min.risk.fns, 
      S.arg=list(ranking.fn="rank.probMom",nTop.q=1.8,kTop.q=1.4),
      experiment="Euro_probMomALL",  max.product.exposure = 0.8, safe=SAFE, onlyUsr=T,all=T) 

A.SA (min.risk.fns = TSA.default$A.arg$min.risk.fns, 
      S.arg=list(ranking.fn="rank.rsi",nTop.q=1.8,kTop.q=1.4),
      experiment="Euro_probMomALL",  max.product.exposure = 0.8, safe=SAFE, onlyUsr=T,all=T) 

### Universum auswählen und einladen 

#... UNIVERSUMS-Wechsel
load("stoxx_branchenpure");
define.Globals(); SAFE="BARCLAYS"

A.SA (min.risk.fns = TSA.default$A.arg$min.risk.fns, 
      S.arg=TSA.default$S.arg,
      experiment="EuroBranchen",  max.product.exposure = 0.8, safe=SAFE, onlyUsr=F) 
###########################
data$universe=universe="USA"
tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU,IWB,IWD,IWF,IWM,IWN,IWO,IWP,IWR,IWS,IWV,IWW,IWZ')

data=SIT.load(universe = universe,tickers = tickers,offline=F)

A.SA (min.risk.fns = TSA.default$A.arg$min.risk.fns, 
      S.arg=list(ranking.fn="rank.slope300",nTop.q=1.8,kTop.q=1.4),
      experiment="usRank.slope",  max.product.exposure = 0.8, safe=SAFE, onlyUsr=T) 

############

universe="International"
tickers = spl('SPY,QQQ,EEM,IWM,EFA,SHY,TLT,IYR,GLD,    XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU,IWB,IWD,IWF,IWM,IWN,IWO,IWP,IWR,IWS,IWV,IWW,IWZ')
#names(tickers) = spl('S&P 500,Nasdaq 100,Emerging Markets,Russell 2000,EAFE,20 Year Treasury,U.S. Real Estate,Gold')
data=SIT.load(universe,tickers,offline=F)
define.Globals()
data$symbolnames
data$BENCH="SPY"
SAFE="SHY"
purePlot(mNorm(data$prices[,c(data$BENCH,SAFE)]))
tview()

A.SA (min.risk.fns = TSA.default$A.arg$min.risk.fns, 
      S.arg=list(ranking.fn="rank.slope300",nTop.q=1.8,kTop.q=1.4),
      experiment="usRank.slope",  max.product.exposure = 0.8, safe=SAFE, onlyUsr=F) 


######################
#nicht sehr effizent - besser man bleibt einfach auf BENCH  (SAFE ist zu schwach !!)
universe="US_Treashury"
tickers = spl('TLT,IEF,IEI,SHV,SHY')
data=SIT.load(universe,tickers,offline=F)
define.Globals()
data$symbolnames
data$BENCH="IEI"
SAFE="SHY"
purePlot(mNorm(data$prices[,c(data$BENCH,SAFE)]))
tview()


A.SA (min.risk.fns = TSA.default$A.arg$min.risk.fns, 
      S.arg=list(ranking.fn="rank.slope300",nTop=4,kTop=4),
      experiment="internat.slope2",  max.product.exposure = 0.8, safe=SAFE, onlyUsr=F) 

#################
hua=read.HUA.XLS( modelDir="HuA_Jan1", xlsName="AAData.xls",sheet.i = 1,startRow=10,belowColnames=3,visual=F,dec=",",date.format="%d.%m.%Y")

#data.info(data)
euro.indi.n = merge(data$prices,hua)
euro.indi.n= m.ifna.prev(euro.indi.n)
mPlots(euro.indi.n)
euro.indi.n = mNorm(na.omit(euro.indi.n))["1997::"] #wichtig:  die Anfangslag normiert die (fr?hen) Werte
fromToS(euro.indi.n)
#füge Barclays als Rentenndex hinzu
data.add(data,"BARCLAYS",euro.indi.n[,"lbeatreu.index"])
data$symbolnames
data$prices = na.omit(data$prices)  
#data$euro.macros=euro.macros
data$BENCH="SX5R"
SAFE <<- "BARCLAYS"


}
