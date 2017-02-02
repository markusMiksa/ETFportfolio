source("Start.r")

#us-branchen

	
SIT.load__<-function(universe,tickers,update=T)
{
  data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
	bt.prep(data, align='remove.na', dates='1970::')
	save(data,file=universe)
}
if (F)
{

universe="USA"
tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU,IWB,IWD,IWF,IWM,IWN,IWO,IWP,IWR,IWS,IWV,IWW,IWZ')
SIT.load(universe,tickers)

universe="International"
tickers=spl("GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT")
SIT.load(universe,tickers)

tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
tickers = spl('DIA,EEM,EFA,EWH,EWJ,EWT,EWZ,FXI,GLD,GSG,IEF,ILF,IWM,IYR,QQQ,SPY,VNQ,XLB,XLE,XLF,XLI,XLP,XLU,XLV,XLY,XLK,TLT')	


universe="International"
tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
names(tickers) = spl('S&P 500,Nasdaq 100,Emerging Markets,Russell 2000,EAFE,20 Year Treasury,U.S. Real Estate,Gold')
SIT.load(universe,tickers)

}
dasfaf


perfmsg. mit: 

perfmsg. mit: 
   engineering.returns.kpi ()

dfasf

bt.rotational.trading.trades.test() ... auch mit TSI  als selektion-Kriterium testen
#ranking-kriterium mit 

#minVar - geht super - aber nur wenn ein sharpe-starkes SAFE-Asset im Universum ist (BARCLAYS)
Frage:   Kann er andernfalls ein SAFE selber definieren ?
z.B. SAFE =das  LONG-Sym mit der höchsten rolling SHARPE ?  bzw. dem höchsten TSI()
bzw. dem höchsten lm-Winkel...().. mit dem intaktesten Long-Trend ... ?
#siehe em_AA()
#SUPER  MMEXPOSURE

bt.max.deviation.rebalancing()
bt.rebalancing.test ()
bt.min.var.test
bt.meom.test
TSIsymbol()
position.score = lag(TSIsymbol(data))
    
MAImp <- function (x, k1=20, k2=0) 

Probabilistic Momentum() (SIT)
und multi-MOM


################ 
ResultFilter()- dir:
lade die data-Dateien mit den Ergebnissen (gegenstück zu den xls)
.. cbind ein großes  Ergebnisse-Sheet 

mach ein coarseCoding(l1) (es ist nicht so wichtig die absolut beste sharpe zu finden, wenn die anderen faktoren passen)
wenn der sortierung immer noch mehr als ein gewinner da ist, da mach den durchlauf noch mal - mit etwas feinerem 
l1.

Sortiere via coarseCoding "gerundeten" Werte 
Sharpe, MaxDD, Turnover, Cagr    (vieleicht muss man auch nach  avgDD) gehe .. 
             maxDD ist ein statischtisches Einzelereignis - avd nicht...
rolling:
Wo wäre man ausgekommen, wenn man jährlich das modell gewselt hätte ??
