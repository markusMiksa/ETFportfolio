########################################################################
# Berechne scoring-Größe für das ntopK-Ranking
########################################################################


options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink();
}))

options(warn=1)
options(stringsAsFactors = FALSE)


#Ansatz A)
#######################################################################################
#######################################################################################

score.diff<-function(prices,k=100)
{
  score = diff(prices,k) / lag(prices,k)*100; 
  # Rank on 6 month return
  #  score2 = prices / mlag(prices, k)*100  ; colnames(score2) = spl("score2")
  #score = diff(prices,k) / lag(prices,k)*100; 
  
  #colnames(score) = spl("score")
}

score.vol<-function(prices)
{
  ret.log = bt.apply.matrix(prices, ROC, type='continuous')
  hist.vol = bt.apply.matrix(ret.log, runSD, n = 21)
  vol.rank = percent.rank(SMA(percent.rank(hist.vol, 252), 21), 250)
}


############################################################################
#############  Aufbau eines Rankings für das PreiseStück frame
#Es werden für x Kriterien Ranking-Liste bebildet.  Dann wird ausgewertet
#werden wie oft unter den TopN - Titel ist.
#Wer die meisten TopN-Mitgliedschaften hat, kommt in die Auswahl

##########################################################################
score.nK<-function(prices, n=10,     K=7,visual=F)
{
  m=list()
  # browser()
  if (nrow(prices)<2)
    {
     ret = prices
     ret[]=0
     return(NULL)
  }
  else
  ret = mROC(prices)

  m.rendite=unlist(lapply((last(mRendite(ret))),function(x)x))#/abs(compute.max.drawdown(ret))
  m$sharpe=unlist(lapply(ret,function(x) compute.sharpe(x)))
  m$cagr=unlist(lapply(mRendite(ret),function(x) mean(compute.cagr(na.omit(x)),na.rm=T)))
  m.drawdown=unlist(lapply(ret,function(x) abs(compute.max.drawdown(x))))
  
  m$ranking = abs(m.rendite)/abs(m.drawdown)
  #m$ranking["TECDAX"]
  #m.ranking=m.ranking[rev(order(m.ranking))]
  
  m$slopes = unlist(lapply(  c(1:dim(ret)[2]),
                             function(xi) {
                               trainData = data.frame(Y=coredata(mRendite(ret[,xi])),X=as.numeric(index(ret)-index(first(ret[,xi])))) 
                               colnames(trainData) = c("Y","X")
                               mod <- lm(Y ~ X, data = trainData)
                               m=coef(mod)[2]
                               r=summary(mod)$r.squared
                               
                               names(m)=colnames(ret[,xi])[1] 
                               #browser()
                               return(m*r*1000)  #!!!!!!!!!!!!!!!
                             }) )
  
  m$potential=unlist(lapply(mRendite(ret),function(x) {print(colnames(x));  res=100*(max(x)-as.numeric(last(x)))/diff(range(x))}))
  
  m$calmar=unlist(lapply(ret,function(x){  
    eq=mRendite(x)
    cgar=compute.cagr(eq)*100
    calmar = abs(compute.calmar(eq))*sign(cgar) }
    ))
  
  
  #m#Top k jeweils
  #m.k ist jeweils ein kriterium
  top.k= lapply(na.omit(m), function(m.k) m.k[rev(order(m.k))][c(1:n)])
  #zähle jetzt wie oft einzelne titel in den top.k sind
  
  top.k.title=(lapply(strsplit(names(unlist(top.k)),"\\."),function(x)x[2]))
  if(visual)
    print(top.k)
  
  count=list()
  for(tit in colnames(ret))
  {
    for(krit in top.k)
      if (!is.na(krit[tit]))
        count[[tit]] = ifelse(len(count[[tit]])==0,1, count[[tit]]+1 )    
  }
  count=unlist(count)
  m.BestK=na.omit(names(count[rev(order(count))][c(1:K)]))
  
  sel.prices= as.xts(data.frame( lapply(m.BestK, function(x) mRendite(ret[,x]))))
  if(visual)
    mchart(sel.prices)
  #m.BestK$date = as.Date(index(last(prices)))
  
  return(m.BestK)
}

##############################################################
#erzeuge eine scoring matrix mit der dim wie prices
#aus 1,0  - je nachdem ob ein Titel allokiert wird oder nicht
##############################################################
calc.score.nk<-function (prices, n=10, K=7, wlen=150)
{
  m.Score_ <<- prices
  m.Score_[]<<-0
  apply.monthly(prices[,1], FUN=function(x) 
  {
    frame=get.frame(last(x),wlen=wlen) ;
    if (dim(prices[frame])[1] > wlen/2)
    {
      res = tryM(score.nK(prices[frame],n=n, K=K))
      lapply(res, function(x2) m.Score_[frame,x2]<<-1)
    }
  })
  return(m.Score_)
}

if (F)
{
  apply.monthly(prices[,1], FUN=function(x) 
  {frame=get.frame(last(x),wlen=150) ; score.nK(prices[frame],visual = T); browser() })
  
  #erzeuge ein bin-matrix die zeigt wann welche zeitreihe zu den top titeln gehört hat
  m.score=calc.score.nk(prices[],n=10, K=7, wlen=150)
  
  m.score=calc.score.nk(prices[],n=3, K=7, wlen=150)
  
  
}
#------------------------
mcalmar_<-function(eq){  
 # cgar=compute.cagr(eq)*100
#  calmar = abs(compute.calmar(eq))*sign(cgar) 
  
  #browser()
  cgar =last( ROC(eq,(len(eq)-1)))/compute.risk(mROC(eq))
  #plot(eq,main=sprintf("%s %f",colnames(eq),cgar))
  #cgar = -cgar
}
mcalmar<-cmpfun(mcalmar_) #compilier das Teil

#--------------- Target sei x wobei die x die Anzahl der Monate (maximal xmonths) ist die 
# --- ein Titel hintereinander zu den ntopk-besten Titel bezgl. mcalmar zählt ... 
## x wird auf 1 als maximal - wert normiert.
target.rank<-function(prices,xmonths = 6,topn=6,topk=9,m.period="months",visual=F)
{
  mP("rank.target")  
  prices=mNorm(prices)

  p.m=select.dates(prices,m.period)
  p=prices[p.m];
  if (visual) mchart(prices)
  #ich will im portfolio  die mcalmar  der letzten xmonths maximieren: 
  mcalmar <- rollapplyr(p, width=xmonths, FUN="mcalmar", by.column=T, align = "right")
  position.score=clean.matrix(mcalmar) 
  #plot(rowSums(mcalmar))
  #welche titel haben für die letzten xmonths die beste mcalmar 
  ntopK= sign(ntop.keep(position.score,topn=topn,keepn=topk))
  if (visual)View(ntopK)
  #wie oft hintereinander gehört ein titel zu den besten
  rle=bt.apply.matrix(ntopK, runLengthEnc3)   #runLengthEnc3 blickt heftig weit nach vorne!
  if (visual)View(rle)
  #jeder zahl über xmonths auf xmonts setzen - und dann auf 0..1 normieren
  rle=bt.apply.matrix(rle, function(col) iif(col > xmonths, xmonths,col)) / xmonths
  #1 heißt:  der titel gehört xmonths mal hintereinander zu den  ntopk besten titeln 
  #View(rle)
  rle
}

#ich zeige hier:  wenn ich wüsste wer in den kommenden 6 monaten zu den besten ntopk titeln bzgl.
#mcalmar zählen würde eine gute performance machen..
#mit den gewählten parameten ist somit   rt=target.rank() ein sinnvolles Target um
#mit einem classifier ein gutes ranking zu trainieren.

#du hast ne kristallkugel fürs ranking und für die signale .. was kommt dann aus diesem universe  --gar nicht mal SO viel
test.target.rank<-function()
{
  prices=data$prices
  dim(data$prices)
  xmonths = 6;topn=ncol(data$prices)/2; topn=6;topk=topn+topn/2 ; m.period="months";visual=F
  
  rt=target.rank(data$prices,xmonths=xmonths, topn=topn, topk=topk, m.period=m.period,visua=visual)
  #rt =rank.slope300(data$prices)
  #  rt = m.score
  #überprüf mal ob es dir hilft wenn du diesen rank in der assetallocation schon 12 monate
  #voher wüßtest
  data$weight=prices;    data$weight[]=NA#sign(rt)
  data$execution.price=prices;data$execution.price[]=NA
  period.ends=coredata(dates2index(prices,select.dates(prices,"months")))
  
  position.score=lag(rt,-xmonths) #kristallkugel
  signal = bt.apply.matrix(data$prices, function(col) sign(col - SMA(col,200)))
  signal[signal < 0]<-0
#  signal=lag(signal,-xmonths*30) #kristallkugel
  
  rank=ntop.keep(position.score,topn=topn,keepn=topk)
#  universe = 1000000*rank / prices[period.ends,] * signal
 universe=rank#*signal 
#  universe[,"BARCLAYS"] = 1-vec2xts(universe,clean.matrix(rowSums(universe[,-which(colnames(universe)=="BARCLAYS")])))
  
 universe = universe* 1000000/ prices[period.ends,]   
  obj = portfolio.allocation.helper(prices, 
                                    periodicity = "months",
                                    period.ends=period.ends,
                                    lookback.len = 60, 
                                    universe=universe,
                                    shrinkage.fns = "ledoit.wolf.shrinkage",
                                    min.risk.fns  = SET2)#list(EW.full=equal.weight.portfolio))


strat=NULL
  strat=create.strategies(obj, data,commission=0.0005,trade.summary=T)
  
strategy.performance.snapshoot(strat$models, one.page=F,title="Timing 100 single allocated", data=data)
return("ok")
#---------------
signal["2008"]
b=mNorm(data$prices["2008"])
mchart(b);lines(b[,"BARCLAYS"],lwd=4)
View(rt["2008"])
#wenn ich schon früher gewusst hätte das Barclays ab Sep 2008 immer wieder unter den top ist
View(position.score["2008"])
View(rank)
#..........  aber auch dann noch werden topn - titel allokiert - auch wenn einige davon schrott sind -- sie sind ja im Spiel weil andere noch schlechter sind.
}

if (F)
{
  test.target.rank()
  ## die score.nK - besten Zeitreihen jeden Monat finden. 
  #visual 

  apply.monthly(prices[,1], FUN=function(x) 
  {frame=get.frame(last(x),wlen=150) ; score.nK(prices[frame],visual = T); browser() })
  
  #erzeuge ein bin-matrix die zeigt wann welche zeitreihe zu den top titeln gehört hat
  m.score=calc.score.nk(prices[],n=10, K=7, wlen=150)

  m.score=calc.score.nk(prices[],n=3, K=7, wlen=150)
  

}

#######################################################################
##########################   #Ansatz B
#scoreScore():
#Berechne jeden Monat ein Scoring  rankA  aller sym 
#gib die TopN sym bzgl. rankA zurück.
#Erwartung:  die von rankA bestimmten WinnerSym_a  sind im Folgemonat b)
#immerhin noch bei den TopK (k>n) Titeln bzgl. auf das PerformanceKritieriumg x#Calmar. ( Equity/MaxDD).
#Die TopK- Titel von b sind  in b am performantestens (equity/MaxDD) - bei nicht #zu heftigen DrawDowns - also genau die Burschen die man am liebsten schon im #Vormonat a gekauft hätte.

#Alles dreht sich darum ein funktionierendes rankA-Kriterium zu finden !!

#
#scoreScore rollt roll.scoreScore über den Zeitstrahl (täglich, events)
#dieses berechnet für jeden event-Tag (z.B. Monatsende)
#mit scoreT=techScore() eine Scoring-Tabelle - pro sym eine Zeile mit vielen
#SpaltenFaktoren (Kriterien). (die sind coarsecode: faktorisiert und dann auf 0..1 projeziert)
#Das sind einige  verdächtige - evtl. gute evt. nutzlose - Feature:
#TTRs, signale.<>, forecasts, memberships, ...  alles was einem plausibel erscheint.  (jeden Monat eine Tabelle:  pro Sym eine Zeile mit vielen Krit)
#Diesen tabelle soll nun verdichtet werden zum Kriterium rankA.

# Idee: das ist ein typischen Labeling-Problem. (beaufsichtigtes Lernen)
# TrainingsDaten:  rankA sei 1 wenn xcalmar(sym) in b) > topK(xcalmar) ist, -andernfalls 0.
#Mit den Trainingsdaten wird ein Klassifier trainiert - der nach dem training
#das labeling an Hand der Krit-Matrix (a) selber vornimmt und so rankA berechnet.

#Dann werden die beiden scoring-Tabellen scoreTables$a und scoreTables$b
#vom aktuellen Monat (b) und vom Vormonat (a) mit compute.scoreScore()
#miteinander verglichen.
##Die Krit-Spalte xcalmar() dient als Qualitäts-Kriterium für den Monat b.

#
# Dazu kaufe ich im Monat a) die TopN-Titel bezogen auf das (zu definierende) Kriterium "rankA".
#
#
#Ziel ist es aus all den Kriterien ein RankA - Kriterium zu generieren, welches


#######################################################################

#############################################################
#Baut zum Datum atDate aus den preisen und signalen (i.system) eine Tabelle
#
#############################################################

##########################################################################
#versuche nun die scorings zu bewerten
#TODO_MM
##########################################################################
#gibt für jedes Kriteriumg (spalte) der score-Tabellen eine Zahl (1 oder -1) zurück
#je nachdem ob das scoreA des Vormonats zur Realität (scoreB) passt - oder nicht

#checke für jedes Krit:
#summiert über alles sym
#Bilde das Krit-Rank des Symbols 
#gib eine 1 wenn das Ranking zum nTopK bzgl. Target passt oder -1 wenn nicht.

##Damit ein Krit ok ist, muss es bei Target == 1 >= thresh sein, sonst <
compute.scoreScore<-function(thresh=1)  #MM1
{
  #thresh = 12/52
  #View(global_TrainScore)
  #browser()
  global_TrainScore=data.table(global_TrainScore) #sym und target brauch ich nicht
  setkey(global_TrainScore,"Target")
  #der Teil mit den Target=1- Zeilen aus global_TrainScore.
  #Hier sollten alle Kriterien (sind ja schon gerankt 0..1 ) Werte im oberen Bereich haben  , also höher: ( thresh = K/AnzahlSym)
  topK=data.frame(global_TrainScore[global_TrainScore$Target==1])[,-c(1,2)]
  #belowK=data.frame(global_wTrainScore[global_TrainScore$Target==0])[,-c(1,2)]
  #View(topK)  
  
  #Gesamtqualtität der vorliegenden global_TrainScore 
  goodFit = len(which(topK >= thresh)) /  len(which(!is.na(topK)))
  
  topK.lines=dim(topK)[1]
  res =   apply(topK, 2,FUN=function(col){len(which(col >= thresh))/topK.lines})
  #badFit = len(which(belowK < thresh)) /  len(which(!is.na(belowK)))
  #res["ADX"]
  #Die sortierten Qualitätskriterien der Kriterien:
  Res= res[rev(order(res))]
  #mP("Qualitätsgewichte der Kriterien:"); print(Res)
  
  print(goodFit)
  resList = list(trainData.quality=goodFit,krit.quality=Res)
  
  return(resList)
}

############################################################################

coarseCode<-function(colN, score) # --weg von den fliesskommazahlen - hin zu einfachen integers (faktoren) (ranking)
{ if (colN=="sym" )#|| colN=="signal")
  return(score[[colN]])
  #hohe Werte sprechen für gute Kurse
  X=as.numeric(score[[colN]])  #nutzt die implizite faktorisierung von R ..  
  X=scaleTo(X,range(0,1))  #normierung aller Spaltena auf gleichen Werte-Bereich
  if (is.null( strfind(colN,"_")))  #
    return(X)
  else
    return (max(X)-X)
}
###########################################################################
#Baue die Befund-Tabelle global_TrainScore, scoreB ist neu, scoreA ist alt
#hier wird die qualität der score-kriterien bzgl. ihres vorhersagen könnes der
#scoreB$m.BestK - symbole gerlernt
##########################################################################-
prepare.scoreScore<-function(atDate,scoreA, scoreB, n=6,K=12,visual=F)
{
  #ls(scoreA)  m.BestK - die besten sym    und score .. die ganze Matrix
  mP("compute.scoreScore")
  krits=colnames(scoreA$score)
  
  #Welche topN aus a sind auch im nächsten Monat in topK (mit n < K)
  inBeiden=scoreA$m.BestK [ first(scoreA$m.BestK,n)  %in%  scoreB$m.BestK ] 
  
  # mP("................")
  #  browser()
  
  #alle Kriterien die positiv in A  für die Menge inBeiden sind werden belohnt.
  #TODO_MM: 
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  #coarseCode("sharpe", scoreA$score)   #hohe Werte sprechen für gute Kurse
  #jetzt hab ich coarseCode -Matrizen für A und B  
  coarseCoded = data.frame(lapply(krits, function(x){ coarseCode(x, scoreB$score)}) )
  #Wertebereich: 0..1
  target = rep(0,dim(coarseCoded)[1])
  
  #setze im target die "inBeiden" - Werte auf 1
  target [scoreB$score$sym %in% inBeiden] <-1
  #kleb das target an das coaresCoded
  coarseCoded = cbind(target,coarseCoded)
  colnames(coarseCoded) = c("Target",krits)
  #View(coarseCoded)
  #browser()
  #gib nun jedem Krit eine Zahl
  if (!exists("global_TrainScore") || is.null(global_TrainScore))
    global_TrainScore <<-coarseCoded
  else
    global_TrainScore <<- rbind(global_TrainScore,coarseCoded)
  
  #lerne kontinuierlich die Signifikanz Deiner Kriterien besser kennen:
  
  scoreQuality=compute.scoreScore(thresh=K/dim(prices)[2])
  trainData.quality= scoreQuality$trainData.quality 
  krit.quality = scoreQuality$krit.quality
  
  global_trainData.Quality[atDate,1] <<- trainData.quality
  
  if (dim(global_trainData.Quality)[2] == 1)
  {#spalten für die Krierien anhängen
    temp=lapply(krit.quality, function(x) global_trainData.Quality<<-merge(global_trainData.Quality, global_trainData.Quality[,1]))
    colnames(global_trainData.Quality) <<- c("total",names(krit.quality))
  }
  #Werte eintragen
  lapply(names(krit.quality), function(co) global_trainData.Quality[atDate,co] <<-krit.quality[co])
  
  
  #browser()
  if (dim(na.omit(global_trainData.Quality)) [1] > 1)
  { 
    #browser() #MM2
    #norm_Win(2)
    #xyplot(global_trainData.Quality)
    
    #chart.TimeSeries(data.frame(global_trainData.Quality), colorset =getColSet( n=dim(global_trainData.Quality)[2]), legend.loc = "bottomright", period.areas = cycles.dates, period.color = "lightblue",  lwd = 2)
    
    if (visual)
      showScoreScores()
    
    return(coarseCoded)
  }
  
  #event.lines = risk.dates, event.labels = risk.labels, event.color = "red",
  
  #MM_TODO
  #Wenn die trainData.Quality deutlich abnimmt - solle das Lerngedächtnis
  #was alte Daten angeht, verkürzt werden !!!
  #Ausstieg für Testts
  
  
  if (F)
    if(dim(global_TrainScore)[1] > dim(prices)[2]*50)  #50 Monate  
    {
      mP("... ready"); browser();
      stop()
    } #TEST
  
  return("OK")
  
  #-------------- 
}


################################################################################
#über "someTableDate .. eine liste von xts-Objekten .. kann ich Werte beispeisen
# die ich voarb als xts-Objekte für alle zeite schon gerechnet hab (z.B. VaR...)
################################################################################

techScore<-function(prices, atDate, i.system=i.system,K=6,visual=T,someTableData=NULL)
{  
  x=list()
  mP("techScore at %s",atDate)
  #browser()
  if (is.null(i.system))
    last.possible.date =as.Date(index(last(na.omit(prices[,1],))[,1]))
  else
    last.possible.date =as.Date(index(last(na.omit(merge(prices[,1],i.system$Signal[,1])))[,1]))
  
  #das letzte Datum in Signal vor=, atDate
  atDate= fitDate(prices,atDate)
  if (atDate > last.possible.date)
  {
    mP("techScore:  sorry - last possible atDate is  %s",toString(last.possible.date))
    return("")
  }
  wlen=200
  offFrame=get.frame2(prices,atDate,wlen)
  #dim(prices[offFrame])
  firstDate = DateS(first(prices[offFrame]))
  if (offFrame != ""  &&  dim(prices)[1] >= wlen)
    Prices=prices[offFrame]
  else
    return(c())
  #Prices enthält vor atDate genügend Kurese
  #dim(Prices)
  #~~~~~ baue eine fettee Matrix mit Featuren die die Zeitreihe beschreiben sollen ....
  #MM_TODO:   wavelets()  hinzufügen  korrelatoren-und dtw- Bewegungen 
  #(indizes, ifo,...)
  
  #browser()
  #.................................................................
  singleResults=  t(data.frame(lapply(colnames(Prices),function(coln) 
  {
    #browser()
    x$equity = mNorm(Prices[,coln])
    #mP("%s",coln)
    #browser()
    clos=x$equity[c((len(x$equity)-70):len(x$equity)),]
    clos200=x$equity[c((len(x$equity)-200):len(x$equity)),]
    
    RocWlen=mROC(x$equity[c((len(x$equity)-wlen):len(x$equity)),])
    Roc30=mROC(x$equity[c((len(x$equity)-30):len(x$equity)),])
    maxdd=abs(compute.max.drawdown(clos))
    #meq=max(prices[,coln]);if (meq ==0) meq = 0.00000001
    meq=max(clos200);if (meq ==0) meq = 0.00000001
    
    xCalmar = xcalmar(clos,maxdd,15)
    
    if (!is.null(i.system))    
    {
      sysSignals__ <<- c(
        sig.zlema =tryM(as.numeric(i.system$Signal[atDate,coln])) #hier werden timing-system-signale beigefeeded
      )
    }
    else
      sysSignals__ <<- c()
    
    tryM(                
      #browser()
      c(
        sysSignals__,
          #   compute.sharpe(Roc30),
        #last(ROC(x$equity,n=30)*100), #änderungsRate der letzten 30 Tage
        #100*(meq-as.numeric(last(prices[,coln])))/meq, #Potential
        #last(RSI(x$equity,n=2))-50,  #inChannelPos
        #HotLagMom(prices=clos200),      
        maxdd,
        xCalmar,
    #    last(ADX(HLC(to.weekly(clos)),n=7,maType="ZLEMA")[,"ADX"]),
        last(rowSums(sign(TDI(clos))))
  #      last(aroon(HLC(to.weekly(clos))[,-3],n=10)[,"oscillator"])
        #last(na.omit(DPO(clos, n = 10, "SMA", shift = 10/2 + 1, percent = FALSE)))
        , getTableData(listOfXts=someTableData,atDate,coln)) #hier holt er sich aus schon vorbereiteten Zeitreihen die aktuellen Tagesdaten  (z.B slopes90)
      
    )  })) )
  
  rownames(singleResults)=NULL
  singleResults=cbind(colnames(Prices),singleResults,deparse.level = 0)
  colnames(singleResults) = 
    #  c(spl("sym,sigZlema,sharpe,d30,potential,RSI_,HotLagMom,MaxDD_,xCalmar,ADX,TDIs,aroon,dpo") , getTableDataNames(someTableData))
    c("sym",names(sysSignals__), 
      spl("MaxDD_,xCalmar,TDIs") 
     # spl("sharpe,d30,MaxDD_,xCalmar,ADX,TDIs,aroon") 
      , getTableDataNames(someTableData))
  
  #ein  _ hinten heißt:  je höher der Wert desto schlechter - sonst besser
  scoreTable= data.table(singleResults[rev(order(singleResults[,"xCalmar"])),])
  
  #so sieht die ScoreTable aus - bevor sie dann in prepare.scoreScore faktorisiert und damit normiert wird
  #S<<-data.frame(scoreTable)
  #die obersten K-Symbole   bzgl. xCalmar (eher provisorisches Krit.)
  #browser()
  m.BestK=sapply(scoreTable[c(1:K)]$sym,toString)   
  #print(scoreTable)
  
  if(F)  #drucke die ausgewählten sym
  {
    sel.prices= as.xts(data.frame( lapply(m.BestK, function(x) mNorm(Prices[,x]))))
    
    mchart(sel.prices)
    print(m.BestK)  
    View(scoreTable)
  }
  
  return(list(m.BestK = m.BestK,score=scoreTable))
}  

if (F)
  x=techScore(prices,"2012-05-01",i.system=i.system,K=5,visual=T)
if (F)
  x=techScore(prices,DateS(last(prices)),i.system=i.system,K=6)
if (F)
  x=techScore(prices,"2013-02-28",i.system=i.system,K=5,visual=T)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#gib die Liste der Aktien zurück die empfohlen werden
# scoring ideen:
# 1) die titel die am öftesten unter den topK aller Kriterien stehen
# 2) bilde eine gewichtetes score-Summe - und ranke nach der die topK

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
analyse.score<-function(scoreNow=NULL, mode=2, n=6,K=12)
{
  
  
  org.scoreNow = scoreNow
  m.BestK = c()
  
  if (is.null(scoreNow) || scoreNow == "OK")
    return (c())
  
  if (mode == 1)
  {
    #m#Top k jeweils
    #m.k ist jeweils ein kriterium
    
    m = scoreNow[,-c(1)]
    top.k= lapply(na.omit(m[,-1]), function(m.k) m[rev(order(m.k)),1][c(1:n)])
    #zähle jetzt wie oft einzelne titel in den top.k sind
    
    count=list()
    for(tit in scoreNow[,"sym"])
    {
      for(krit in top.k)
      {
        if (contains(krit,tit))
          count[[tit]] = ifelse(len(count[[tit]])==0,1, count[[tit]]+1 ) 
      }
    }
    
    #zu wieviel % passen denn die so ausgewählten Titel zum Target ?
    if (len (count)>1)
    {
    count=unlist(count)
    
      count = count[rev(order(count))]
      m.BestK=na.omit(names(count))[c(1:K)]
      
      scoreNow = data.table(scoreNow)  
      fitness=sum(unlist(lapply(m.BestK, function(x) scoreNow[scoreNow$sym==x]$Target)))
      fitness = fitness/n*100
      mP("fitness 1  %f",fitness)  #sollte im Laufe der Lerness des kweight-vectors besser werden 
      #evtl. läßt sich die fitness durch clevere lernalgorithmen steigern ..
      # browser()
    }
  }
  #Hier einstellen- falls Du mode1 mit mode2 vergleichen willst...
  #es scheint mode2 fast immer bessert zu sein wie mode1
  #mode=2
  #scoreNow=org.scoreNow 
  #..................... Alternative Auswerungsmethode 2: ............
  
  if (mode == 2 && exists("global_trainData.Quality") && dim (na.omit(global_trainData.Quality))[1]> 1)
  {
    mP("analyse.score mode == 2")
    
    shrink=T  #ein steuerungsparameter der sagt wie das ranking gebildet wird
    #damit darf ich ohne große sorge auch schrottkriteriene aufnehmen!
    lastKrit = last(na.omit(global_trainData.Quality[,-1]))
    total=as.numeric(last(na.omit(global_trainData.Quality[,1])))#die grüne qualitäts-marke .. zeigt den mitleren leveln der krit-güte 
    lastKrit.o=lastKrit[,order(lastKrit, decreasing = T)]
    
    if (shrink)
    {
      #wir wollen hiern nur krit die bessert sind als total und zu den top3 krit gehören      
      #top3 Krit:    
      #browser()
      if (len(colnames(lastKrit.o))>4)
        lastKrit.o[ ,seq(4+1,len(colnames(lastKrit.o)))]<-0
      #besser als total
      lastKrit.o[,which(lastKrit.o <= total)]<-0
      #print(lastKrit.o)
    }
    #print(lastKrit.o)
    #bilde das skalarprodukt der sym-krit-gewichte  mit den krit-qualitätsfaktoren 
    k=as.matrix(scoreNow[,-c(1,2)])  #2 spalten löschen
    k=k[,order(colnames(k))]  #die Spalten nach Spaltennamen sortieren
    k[]=as.numeric(k)
    kweight=coredata(lastKrit.o)  # 1 vectorelement löschen
    kweight[]=kweight[order(colnames(kweight))]  #den Vector nach krit-namen sortieren
    
    #das skalargewicht - eine Zahl pro sym
    rank= apply(k,1, function(r) { sum( r * kweight)})
    
    scoreTable = cbind(scoreNow[,c(1,2)], rank)
    scoreTable =scoreTable[order(scoreTable[,"rank"],decreasing=T),]
    
    #ein  _ hinten heißt:  je höher der Wert desto schlechter - sonst besser
    #die obersten K-Symbole
    # browser()
    m.BestK=sapply(scoreTable[c(1:K),"sym"],toString)   
    
    #zu wieviel % passen denn die so ausgewählten Titel zum Target ?
    fitness= sum(scoreTable[c(1:K),"Target"]) / n *100
    mP("fitness 2 %f",fitness)  #sollte im Laufe der Lerness des kweight-vectors besser werden 
    #evtl. läßt sich die fitness durch clevere lernalgorithmen steigern ..
    #  browser()
    
  }
  
  return(m.BestK)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#berechne jeden Monat die neue coreTabelle scoreTable$b und gib ihre TopK
#Sym zurück.  Vergleiche sie mit der des Vormonats.
#Baut die global_TrainScore  und gibt die Liste der bestn Titel pro monat zurück
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
roll.scoreScore<-function(prices_,month.end.dates,i.system, n=6,K=12,someTableData=NULL,visual=F) #erhält immer 200Tage
{
  toDay = DateS(last(prices_))
  scoreT =NULL
  
  if (contains(month.end.dates,toDay)) #trigger event
  {
    #neues Score b berechnen !!!
    scoreT=techScore(prices_,toDay,i.system=i.system,K=K,visual=F,someTableData)
    #ls(scoreT):  m.BestK   und score #MM_B
    #browser()    
    scoreNow = NULL
    scoreTables$a<<-scoreTables$b
    scoreTables$b<<-scoreT
    #berechne global_TrainScore
    if (scoreTables$a != "" && scoreTables$b != "" ) #beide scors gefüllt
      scoreNow=prepare.scoreScore(toDay,scoreTables$a,scoreTables$b,n,K,visual=visual) 
    #welche Aktien werden denn nun vom Scoring empfohlen?
    
    #mP("roll.scoreScore()")
    #browser()
    
    #analysiere die aktuell scoreing-Matrix und schätze die "besten K symbole
    #sortiert danach wie gut sie sind
    
    bestSym = analyse.score(scoreNow,mode=2) #mode=1 ist ein alternative auswertungsmethode ...hier könnten auch nichtlineare mustererkennungsalgorithmen kommen
    
    #schreib die empfehlung als 1 für die topN-Werte nach global_score (xts)
    if ( !is.null(bestSym) && len(which(month.end.dates > toDay))>0)
    {
      
      nextMonth = first(month.end.dates[month.end.dates > toDay])
      
      empfehlungsZeitraum = sprintf("%s::%s",as.Date(toDay)+1,nextMonth)
      #browser()
      #    global_score[empfehlungsZeitraum,unlist(scoreT$m.BestK)] = 1
      
      #poke die global_score-xts-matrix
      global_score[empfehlungsZeitraum,unlist(bestSym[c(1:n)])] <<- 1
      
      # global_score[empfehlungsZeitraum]
      if (visual )
      {
        sel.prices= as.xts(data.frame( lapply(bestSym[c(1:n)], function(x) mNorm(prices[empfehlungsZeitraum,x]))))
        #if(visual)
        mchart(sel.prices) #die charts der topN
        #vergleiche den Mittelwert der topK mit dem Mittelwert aus allen prices
        buyAll=mNorm(prices[empfehlungsZeitraum])
        #alle Preise
        buyAll[,1]=  rowSums(mNorm(prices[empfehlungsZeitraum])) / dim(buyAll)[2]
        sel.prices= as.xts(data.frame( lapply(bestSym[c(1:K)], function(x) mNorm(mNorm(prices[empfehlungsZeitraum,x])))))
        buyAll[,2]=  rowSums(sel.prices[empfehlungsZeitraum]) / K
        
        #lines(buyAll[,1],ylim=c(1,max(buyAll[,1],buyAll[,2])))
        lines(buyAll[,2],lwd="3",col="red")     #der Mittelwert der Auswahl
        lines(buyAll[,1],lwd="3",col="black")    #der Mittelwert von BuyAll 
        
      #  browser()
      }
      return(1)
    }
    else
      return(0)
  }
  
  if (is.null(scoreT)) return(0)
  return(1) #global_TrainScore 
}
################################################################################
#im normalen predict-fall - wenn man ein Ranking braucht 
################################################################################
#noch nicht fertig MM_TODO
predict.score<-function(prices,inDate,i.system,n=6,K=12)
{
  toDay = DateS(last(prices))
  
  scoreT=techScore(prices,toDay,i.system=i.system,K=K,visual=F)
  
  #berechne global_TrainScore
  prepare.scoreScore(scoreT,NULL,n,K) 
  #analyse.....
  return("ok") #global_TrainScore 
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# iteriere über alle Tage und berechne (mit roll.scoreScore())  zum Event (hier ist das Event "at monatsende") den vektor der topN- sym -- also die sym bei denen ich hoffe,
#das sie im Folgemonat immerhin noch zu den TopK-Sym gehören.
#Baue zusätzlich in global_Krit eine große ScoreTrainings-Tabelle.
#in der global_score wird abgelegt ob ein Titel empfohlen wird oder nicht
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scoreScore<-function(prices,i.system, n=6,K=12,someTableData=NULL,visual=F)
{
  month.ends = endpoints(prices, 'months')
  month.ends = month.ends[month.ends > 0]   
  month.end.dates= as.Date(index(prices[month.ends]))
  scoreTables <<- list(a="",b="")
  global_TrainScore <<-NULL
  global_trainData.Quality <<-prices[month.ends,1];  global_trainData.Quality[] <<- NA
  
  last.possible.date =as.Date(index(last(na.omit(merge(prices[,1],i.system$Signal[,1])))[,1]))
  
  possible_prices = prices[sprintf("::%s",last.possible.date)]
  
  adRiskData =F
  
  global_score<<-prices
  global_score[]<<-0
  
  if (visual)
    new_Win(2)
  bestK=rollapplyr(possible_prices,FUN=roll.scoreScore,width=200,by.column=F,
                   month.end.dates=month.end.dates,i.system=i.system,n=n,K=K,someTableData=someTableData  ,visual=visual)
  
}

#......................................................................

if (F)
{
  
  global_objectId <<-paste("TREND","Dax","signal.techStops")  #sagen welches System trainiert 
  i.system=indi.Generic("signal.techStops", global_arg,visual=T, TRAINSYM=-1)  #jeder mit seinen

  #erstellen global_score  - die Empfehlungsmatrix für den Allocator
  scoreScore(prices[],i.system, n=6,K=12) #berechne für jeden Monat das scoring, erstelle global_TrainScore
  scoreScore(prices["::2005"],NULL, n=6,K=12, visual =T)
}  

if (F)
  compute.scoreScore(thresh=12/dim(prices)[2])  #Bewerte die Kriterine in der nur erstellten global_TrainScore:  
#Damit ein Krit ok ist, muss es bei Target == 1 >= thres sein, sonst < 

showScoreScores<-function()
{
  #norm_Win(1)
  chart.TimeSeries(data.frame(global_trainData.Quality), colorset =getColSet( n=dim(global_trainData.Quality)[2]), legend.loc = "bottom", period.areas = cycles.dates, period.color = "lightblue",  lwd = 2)  
  
  
  lastKrit = last(na.omit(global_trainData.Quality))
  lastKrit.o=lastKrit[,order(lastKrit, decreasing = T)]
  print(lastKrit.o)
  return(lastKrit.o)
}


#Ansatz C
###############################################################################################################
###############################################################################################################
########################################################################################
print("########### load score.r")

if (F)
  list_R_functions('MLib/score.r')

if (exists("prices"))mP("Anzahl von Monaten %d in den Daten %s", as.integer(dim(prices)[1]/12),fromTo(prices))

