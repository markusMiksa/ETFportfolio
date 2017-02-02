Q################################################################


options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)
options(error = browser)


# machine learning übersicht:
#http://cran.r-project.org/web/views/MachineLearning.html

#sieh auch den  rminer:  http://cran.r-project.org/web/packages/rminer/rminer.pdf

#NEU:
#http://www.r-bloggers.com/introducing-parallelrandomforest-faster-leaner-parallelized/

#http://jehrlinger.wordpress.com/tag/randomforestsrc/

#install.packages("parallelRandomForest", repos = "c:/Users/markus/Documents/R/win-library/3.0/parallelRandomForest", type="source")


#sehr gute übersicht über tree-classifier:
#http://www.redbrick.dcu.ie/~noel/R_classification.html
#unten gibts Beispiel mit rpart,ctree und weka.j48 (sehr gut auch)

#gute übersicht in verschieden methoden
#http://www.statmethods.net/advstats/cart.html

if(F)
{
  library("parallelRandomForest")
  
  'http://www.thertrader.com/2013/09/24/working-with-intraday-data/'
  libs <- c('sqldf', 'data.table', 'rbenchmark')
  lapply(libs, require, character.only = T)
  
  n <- 1000000
  set.seed(1)
  ldf <- data.frame(id1 = sample(n, n), id2 = sample(n / 1000, n, replace = TRUE), x1 = rnorm(n), x2 = runif(n))
  rdf <- data.frame(id1 = sample(n, n), id2 = sample(n / 1000, n, replace = TRUE), y1 = rnorm(n), y2 = runif(n))
  
  benchmark(replications = 5, order = "user.self",
            noindex.sqldf = (sqldf('select * from ldf as l inner join rdf as r on l.id1 = r.id1 and l.id2 = r.id2')),
            indexed.sqldf = (sqldf(c('create index ldx on ldf(id1, id2)',
                                     'select * from main.ldf as l inner join rdf as r on l.id1 = r.id1 and l.id2 = r.id2')))
  )
  
  benchmark(replications = 5, order = "user.self",
            noindex.table = {
              ldt <- data.table(ldf)
              rdt <- data.table(rdf)
              merge(ldt, rdt, by = c('id1', 'id2'))
            },
            indexed.table = {
              ldt <- data.table(ldf, key = 'id1,id2')
              rdt <- data.table(rdf, key = 'id1,id2')
              merge(ldt, rdt, by = c('id1', 'id2'))
            }
  )
  #("R","h")])
  
}
# Rattle is Copyright (c) 2006-2013 Togaware Pty Ltd.

#============================================================
# Rattle Zeitstempel: 2013-08-29 10:13:29 x86_64-w64-mingw32 

# Rattle Version 2.6.26 Benutzer 'markus'

# Exportieren Sie diese Log-Textansicht in eine Datei anhand des Buttons 
# Exportieren oder dem Tools-Men?, um ein Log ?ber alle Aktivit?ten zu speichern. Dies vereinfacht Wiederholungen. Exportieren 
# in die Datei 'myrf01.R' erm?glicht z. B., dass man in der R-Konsole den 
# Befehl source('myrf01.R') eingeben kann, um den Ablauf automatisch zu 
# wiederholen. Ggf. m?chten wir die Datei f?r unsere Zwecke bearbeiten. Wir k?nnen diese aktuelle Textansicht auch direkt 
# bearbeiten, um zus?tzliche Informationen aufzuzeichnen, bevor exportiert wird. 

# Beim Speichern und Laden von Projekten bleibt dieses Log ebenfalls erhalten.
crs<-new.env()
library(rattle)

# Diese Log zeichnet ?blicherweise die Schritte zum Erstellen eines Modells auf. Mit sehr 
# wenig Aufwand kann es aber verwendet werden, um eine neue Datenreihe anzulegen. Die logische Variable 
# 'building' wird verwendet, um zwischen dem Erstellen von Umwandlungen, wenn ein Modell erstellt 
# wird und dem Verwenden von Umwandlungen, wenn eine Datenreihe angelegt wird, umzuschalten.

building <- TRUE
scoring  <- ! building

# Das Paket Colorspace wird verwendet, um die in den Grafiken vorhandenen Farben zu erzeugen.

library(colorspace)

# Die allgemeine Fehlerquote berechnen

overall <- function(x)
{
  if (nrow(x) == 2) 
    cat((x[1,2] + x[2,1]) / sum(x)) 
  else
    cat(1 - (x[1,rownames(x)]) / sum(x))
} 
##############################################################################################################
# 
##############################################################################################################
classifier.prepare<-function(crs,train.data,dimTarget=1,today=0)  
{
  if (!exists("dont_use_last") || len(dont_use_last)==0)
    dont_use_last <<- 100
  
  if (today==0) today=DateS(last(train.data)[,1])
  if ( dimTarget >1)
  {
    sag("mehrspaltige Targets sind noch nicht zu ende implementiert",warte=T)
    dimTarget=1
  }
  #colnames(train.data)[1]=c("Target") 
  crs$today=today
  crs$dimTarget=dimTarget
  
  mP("%s TRAIN  classifier.randomForest.fit %d %d",today,dim(train.data)[1],dim(train.data)[2])
  #if (as.character(today)=="2002-09-30")
  #  browser(print("xxx1"))
  crs$seed <- 42 
 # library(caret)
#  highCorr.500 <- findCorrelation(tail(train.data,500), 0.90)
  #============================================================
  # Rattle Zeitstempel: 2013-08-29 10:13:57 x86_64-w64-mingw32 
  # Einen R-Datenrahmen laden
  crs$dataset <- na.omit(train.data)
  # Eine einfache Zuammenfassunf (Struktur) der Datenreihe anzeigen
  #str(crs$dataset)
  #============================================================
  # Rattle Zeitstempel: 2013-08-29 10:13:59 x86_64-w64-mingw32 
  # Die Auswahl des Benutzers beachten 
  # Die Training-/Validierungs-/Test-Datenreihen erstellen
  
  set.seed(crs$seed) 
  crs$nobs <- nrow(crs$dataset) # 1220 observations 
#ich will möglichs nicht mit aktuellen Daten Trainieren !!! weil:  eigentich hab ich für die neuesten Daten gar keine Targets !!!!!
  crs$sample <- crs$train <- 1:max((crs$nobs-150),0.7*crs$nobs)#sample(nrow(crs$dataset), 0.7*crs$nobs) # 854 observations
  crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 
                          as.integer(0.5*(crs$nobs-len(crs$train)) ))#0.15*crs$nobs) # 183 observations
  crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 183 observations
  
  # Die folgenden Variablenauswahl wurde entdeckt.
  crs$input<-colnames(crs$dataset)[- (1:dimTarget)]
  crs$numeric<-crs$input
  crs$categoric <- NULL
  crs$target  <- colnames(crs$dataset)[1:dimTarget]
  crs$risk    <- NULL
  crs$ident   <- NULL
  crs$ignore  <- NULL
  crs$weights <- NULL
  
  crs$dataset[1,1]= ifelse( crs$dataset[2,1]==-1 , 1,-1)   #zwangserzeugung von 2 Klassen
  Target <- crs$dataset[crs$train,1:dimTarget,drop=F]
  #dim(Target)
  #dim(crs$dataset)
  
  #============================================================
  # Rattle Zeitstempel: 2013-08-29 10:14:04 x86_64-w64-mingw32 
  # Random Forest 
  # Das Paket 'randomForest' stellt die Funktion 'randomForest' zur Verf?gung.
  #View(crs$dataset[crs$sample,c(crs$target,crs$input)])
  require(randomForest, quietly=TRUE)
  # Das Random Forest-Modell erstellen
  set.seed(crs$seed)
  mP("---- pre forest   %s",  today)
 
  
  #if (today == "2004-12-31")
  #  browser(mP("stop#"))
  tdata=crs$dataset[crs$train,c( crs$target,crs$input)]
  #Target=tail(Target,dim(tdata)[1])
  #gegen den Bug:   Need at least two classes to do classification.
  #gibt es is.infinite-Werte ?
  if (sum(apply(tdata,2,FUN=function(col) len(col[is.infinite(col)]))) >0)
    no=foreach(col.i = 1:ncol(tdata)) %do%  { tdata[is.infinite(tdata[,col.i]),col.i]<-0 }
  
  if (sum(apply(Target,2,FUN=function(col) len(col[is.infinite(col)]))) > 0)
    no=foreach(col.i=1:ncol(Target)) %do% {Target[is.infinite(Target[,col.i]),col.i]<-0 }
  
  crs$Target=Target
  crs$tdata=tdata
  return(crs)
}

###############################################################################
classifier.randomForest.fit<-function(crs,train.data,dimTarget=dimTarget,ntree=500, importance=T ,remove_highCorr=F,today=0)
{
  library(caret)
  # Ein vordefinierter Wert wird verwendet, um den zuf?lligen Startwert  zur?ck-zusetzen, damit die Ergebnisse wiederholt werden k?nnen  mP("fit")
  crs=classifier.prepare(crs,train.data,dimTarget=dimTarget,today=today)
  highCorr=NULL
  
  if (remove_highCorr)
  {
  descrCorr <- cor(na.omit(crs$tdata[,-(1:dimTarget)]))# die eingentlich - trainings-daten
  highCorr <- findCorrelation(descrCorr, 0.999)+dimTarget
  #damit auch predict damit arbeiten kann
  }
  if (!is.null(highCorr) && len(highCorr)>0 )
     {
    crs$input=colnames(crs$tdata[,- highCorr])
      mP("remove highCorr %d",length(highCorr))
      print(colnames(crs$tdata[,highCorr]))
      print(highCorr)
       crs$highCorr=highCorr
  }
  else
   { 
     crs$input=colnames(crs$tdata)
     highCorr=crs$highCorr=NULL
  }
  crs$input = crs$input[- (1:dimTarget)]
                       
  #options(error=recover)
  #debug(randomForest:::randomForest.reStruct)
  #as.factor(P) -.. macht das Teil zum Klassifikator
  #y=cbind(as.factor(crs$Target[,1]),as.factor(crs$Target[,2]))   
  temp <- try(randomForest(y=as.factor(crs$Target[,1])  ,
                           type="classification",
                           #    proximity=T,
                           #    do.trace=5,
                           x=crs$tdata[,crs$input], 
                           ntree=ntree,
                           mtry=4,
                           importance=importance,
                           na.action=na.roughfix,
                           replace=FALSE)  )
 #http://www.r-bloggers.com/my-intro-to-multiple-classification-with-random-forests-conditional-inference-trees-and-linear-discriminant-analysis/

 #fit.lda <- lda(grouping=as.factor(crs$Target[,1]) ,  x=coredata(crs$tdata[,crs$input]))
#  pred.lda=predict(fit.lda, crs$tdata[,crs$input])$class
 
 
 
        mP("#m1")
  if(inherits(temp, 'try-error')) 
  {
    mP("#m1.5")
    #http://stackoverflow.com/questions/14143450/r-randomforest-for-classification
    # It is because there are more than 32 levels for one of your variable. Levels means #distinct values for one variables. Remove that variable and try again
    #View(tdata)
    mP("#################### stop NA at fit  ######################### ")
    #  browser()
    wichtigkeit=xts(data.frame(cbind(model.err=0,t.w=0)),as.Date(crs$today))
    
    return(wichtigkeit)
  }
  crs$rf <- temp
  # Die Textausgabe des Modells 'Random Forest' erstellen
  model.err= crs$model.err = last(crs$rf$err.rate)[1]
  print(crs$rf)
  mP("----- after forest - err %f",crs$model.err)
  #browser()
  ls(crs$rf)
  last(crs$rf$err.rate)
  # Die Wicktigkeit der Variablen auflisten
  #rn <- round(importance(crs$rf), 2)
  #rn[order(rn[,3], decreasing=TRUE),]
  if (F)
  {
    # The `pROC' package implements various AUC functions.
    require(pROC, quietly=TRUE)
    # Calculate the Area Under the Curve (AUC).
    roc(crs$rf$y, crs$rf$votes)
    # Calculate the AUC Confidence Interval.
    ci.auc(crs$rf$y, crs$rf$votes)
  }
  if (T)
  {
    # Die Wicktigkeit der Variablen auflisten
    rn <- round(randomForest::importance(crs$rf), 2)
    wichtigkeit =rn[order(rn[,3], decreasing=TRUE),]
    
    #View(crs$wichtigkeit)
  }
  # Ben?tigte Zeit: 7.25 Sek
  #============================================================
  #
  # Die Modellleistung auswerten 
  # Eine Fehlermatrix f?r das Modell Random Forest erstellen
  # Die Antwort vom Modell Random Forest erhalten
  #gibt die Wahrscheinlichkeit f?r die in Frage kommenden Kategorien
  #crs$pr <- predict(crs$rf, type="vote",newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))
  
 check.classifier.1(crs,crs$rf,visual=T,mode=1)
  
  #wie groß ist die confidence für das winner-signal
  pr.conf <- rowMax(predict(crs$rf, type="vote",newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])))
  #sagt: welche Kategorie gewinnt
  temp <-  predict(crs$rf,newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))
  #mP("#m2")
  if(inherits(temp, 'try-error')) 
  {
    mP("#m2.")
    wichtigkeit=xts(data.frame(cbind(model.err=0,t.w=0)),as.Date(crs$today))
    return(wichtigkeit)
  }
  #mP("#m3")
  
  crs$pr=temp
  crs$m.target <- merge( predict(crs$rf,newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])),pr.conf)
  colnames(crs$m.target)=c("signal","confidence")
  
  
  if (F)
  {
    colnames(crs$dataset)
    first(Target)
    last(Target)
    prices=crs$dataset[crs$sample,"EXX50_RI.1"]
    
    mPlot(prices,crs$Target)
    dim(prices)
    dim(crs$Target)
    plot(prices)
    plot(crs$Target)
    
    k=plotSigPrice(signal=sign(crs$Target),prices=prices)
    mPlots(k,crs$dataset[,"EXX50_RI.1"],merge(crs$Target,0))
  }
  # Generate the confusion matrix showing counts.
  #mP("#m4")
  #dim(crs$dataset[crs$test,])
  #shape(crs$pr)
  #k=table(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])[,crs$target], crs$pr,
  #      dnn=c("Ist", "Vorausgesagt"))
  #print(k)
  #mP("#m5")
  # Generate the confusion matrix showing percentages.
  #k=round(100*table(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])[,crs$target], crs$pr, 
  #               dnn=c("Ist", "Vorausgesagt"))/length(crs$pr))
  #print(k)
  overall <- function(x)
  {
    if (nrow(x) == 2) 
      cat((x[1,2] + x[2,1]) / sum(x)) 
    else
      cat(1 - (x[1,rownames(x)]) / sum(x))
  } 
  if (F)
    XTab=overall(table(crs$pr, na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])[,crs$target],       dnn=c("Predicted", "Actual")))
  
  Wichtigkeit=crs$tdata[1,-1];Wichtigkeit[]=NA  #langer vector
  
  w=(wichtigkeit[,1]+wichtigkeit[,2])/2
  t.w=t(w)
  if(len(model.err)==0 || is.na(model.err))
    model.err=-1
  if (len(t.w)==0)
  {mP("#m9")
   t.w=NA  
  }
  
  Wichtigkeit[,crs$input] = t.w
  t.w=Wichtigkeit
  
  #  einzeiliges xts mit model.err und wichtigkeit aller variablen
  wichtigkeit=xts(data.frame(cbind(model.err=model.err,pred.err=crs$pred.err,t.w=t.w)),as.Date(crs$today))
  
  #print(tail(sort(data.frame(wichtigkeit),decreasing=F),30))
  #dim(wichtigkeit)
  mP("#rf.fit -->")
  return(wichtigkeit)
}

###############################################################################################################
# hol Dir die classification vom randomForest f?r newData (dataCloud) 
#das Ergebnis wird ein 2 dim  xts aus signal und signalsicherheit
###############################################################################################################

m.rowSums<-function(p,temp)
{
  temp[]=0
#  browser()
  if (is.null(p) || shape(p)==0)
    return(temp)
  res = p;res[]=0
  if (is.null(ncol(p)) || ncol(p)==1)
    {res = xts(p,as.Date(names(p)))
     return(res)
  }
  if (ncol(p)>1)
    res = rowSums(p)
  res
}
#############################################################################################


classifier.randomForest.predict<-function(crs, newData,just.sign=F, visual=T)
{
  mP("#M1 predict")
  #browser()
  newData= na.omit(newData)
  if (len(crs$highCorr) >0)   #lass gegebenenfalls einige spalten weg
    newData=newData[,- crs$highCorr]
  #today=DateS(last(newData)[,1])
  #if (visual)mP(" ..  classifier.randomForest.predict   %s .. %s ",DateS(newData[1]),today)
  if (F && today == "2004-08-02")  
    browser(mP("stop"))
  
  # dim(t(newData))
  # is.na(t(newData))
  # Die Antwort vom Modell Random Forest erhalten
  #gibt die Wahrscheinlichkeit f?r die in Frage kommenden Kategorien
  
  temp<- try(predict(crs$rf, type="vote",newdata=newData))
  if(inherits(temp, 'try-error')) 
  {
   # mP("#M1b")
    
    mP("1 invalid result at classifier.randomForest.predict")
    
    temp=try(cbind(signal=0,confidence=0, model.err=0))
    crs$m.target = temp
    crs$m.target=xts(temp,as.Date(today))
    sag("not ready11",warte=T)
    return (crs$m.target)
    #return(NA)
  }
  #mP("#M2")
  # browser()
  #crs$pr2 = temp
  if(just.sign)
  {#alles negativen bzw. alle positiven signal-confidences separat addieren und dann vergleichen
    tv=nval(colnames(temp))
    
    temp2=try(cbind(m.rowSums(temp[,tv >=0],temp),m.rowSums(temp[,tv <0],temp)))
    temp2=try(cbind(sum(temp[,tv >=0]),sum(temp[,tv <0])))
    temp=temp2
  }
  pr.conf=rowMax(temp)
  #sagt: welche Kategorie gewinnt
  crs$pr <- pred <-  predict(crs$rf,newdata=newData,type="class")  
  pred.val=factor2val(pred) #as.numeric(levels(pred)[pred]) #factor2val
  
  #levels(x)
  #rückkodierung des factors in einen Zahl
  temp=try(cbind(signal=pred.val,confidence=pr.conf,model.err=crs$model.err))
  
  #mP("#M3")
  if(inherits(temp, 'try-error')) 
  {
    mP("invalid result at classifier.randomForest.predict")
    temp=try(cbind(signal=0,confidence=0, model.err=0))
    mP("#M3b")    
  }
  
  crs$m.target = temp
  #mP("#M4")
  
  crs$m.target=xts(crs$m.target,index(newData)) #????
  
  #factor -> integer  umrechnen
  #signal = as.numeric(levels(last(crs$pr)))[last(crs$pr)]
  if (F && visual)
  {
    mP("     predict: ")
    print(crs$m.target)
  }
  #mP("#M5")
  
  return (crs$m.target)
}
############################################################################################
if (F)
{
  if (is.null(data$crs))  data$crs= new.env()
  
  crs=data$crs
  classifier.prepare(crs,data$train.data,dimTarget=1)  
  
  #-----------------  2 Tree-Views --------------------------------
  
  crs$rf=randomForest(as.factor(crs$Target) ~ .,
                      data=crs$tdata,
                      ntree=500,
                      mtry=4,
                      na.action=na.roughfix,
                      replace=FALSE)  
  
  last(crs$rf$err.rate)
  pred <-  predict(crs$rf,newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))
  print("rainForest")
  table((coredata(crs$Target)[crs$test]),coredata(pred))
  
  #  plot(crs$rf)
  #...............................................................
  library(rpart)  
  crs$rpart <- rpart(as.factor(crs$Target) ~ .,
                     data=crs$tdata,
                     method="class",
                     parms=list(split="information"),
                     control=rpart.control(usesurrogate=0, 
                                           maxsurrogate=0))
  
  # Eine Textansicht des Modells Entscheidungsstruktur erstellen
  summary(crs$rpart)
  
  
  pred <-  predict(crs$rpart,newdata=na.omit(crs$dataset[crs$train, c(crs$input, crs$target)]),type="class")  
  class.pred <- table(pred,crs$dataset[crs$train,crs$target])
  print(class.pred)
  pred.err=1-sum(diag(class.pred))/sum(class.pred)
  mP("rpart.model.err = %f",pred.err)
  
  pred <-  predict(crs$rpart,newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),type="class")  
  
  class.pred <- table(pred,crs$dataset[crs$test,crs$target])
  print(class.pred)
  pred.err=1-sum(diag(class.pred))/sum(class.pred)
  mP("rpart.pred.err = %f",pred.err)
  
  
  printcp(fit) 
  fit=crs$rpart
  fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
  crs$rpart$variable.importance
  ls(crs$rpart)
  crs$rpart$cptable
  plotcp(fit) 
  print(crs$rpart)
  printcp(crs$rpart)
  
  # create attractive postscript plot of tree
  new_Win(1)
  plot(fit, uniform=TRUE,
       main="Classification Tree for rpart")
  text(fit, use.n=TRUE, all=TRUE, cex=.8)
  
  post(fit, file = "rpart.ps",
       title = "Classification Tree for rpart")
  
  cat("\n")
  
  if (F)
  {
    pred <-  predict(crs$rpart,newdata=na.omit(crs$dataset[crs$test, c(crs$target,crs$input)]),type="class")  
    # check predicted classes against original class labels
    table((coredata(crs$Target)[crs$test]),coredata(pred))
  }
  
  # We use the rpart.plot package.
  report.name=sprintf("%sTree1_%s.pdf",path,sym)
  pdf(file = paste(report.name,'.pdf',sep=""), width=8.5, height=11)
  fancyRpartPlot(crs$rpart, main="Entscheidungsstruktur trainBest.csv $ EXX50_RI.EXX50_RI_sumTarget")
  dev.off() 
  
  #................................................................
  library(party)
  
  crs$ct <- ctree(as.factor(crs$Target) ~ .,    data=crs$tdata,
                  controls = ctree_control(minsplit=30, minbucket=10, maxdepth=5))
  
  check.classifier(crs,crs$ct)
  pred <-  predict(crs$ct,newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),type="class")  
  
  class.pred <- table(pred,crs$dataset[crs$test,crs$target])
  print(class.pred)
  pred.err=1-sum(diag(class.pred))/sum(class.pred)
  mP("rpart.pred.err = %f",pred.err)
  
  
  report.name=sprintf("%sTree2_%s.pdf",path,sym)
  pdf(file = paste(report.name,'.pdf',sep=""), width=28.5, height=11)  
  plot(crs$ct)#, ip_args=list(pval=FALSE), ep_args=list(digits=0))
  dev.off()
  #.....................................................................
  
  
  
  library(RWeka) #http://www.r-bloggers.com/r-talks-to-weka-about-data-mining/
  #super.. besser wie randomForest -- aber wo sind die variablen-wichtigkeiten ?
  crs$j48 <- J48(as.factor(crs$Target) ~ .,   data=crs$tdata)
  summary(crs$j48)
  print(crs$j48)
  report.name=sprintf("%sTree3_%s.pdf",path,sym)
  pdf(file = paste(report.name,'.pdf',sep=""), width=280.5, height=11)  
  plot(crs$j48)
  dev.off()
  
  #schrott
  smo=SMO(as.factor(crs$Target) ~ .,   data=crs$tdata,  control = Weka_control(K = list("RBFKernel", G = 2)) )
  summary(smo)
  
}
################################################################################
check.classifier.1<-function(crs,fit,mode=1,just.sign=T,visual=F)
{
  #hier gmodels und caret - per-test und roc einfügen...
  
  check.set=list(crs$test,crs$validate,crs$train)[[mode]]
  pred <-  predict(fit,newdata=na.omit(crs$dataset[check.set, c(crs$target,crs$input )]),type="class")  
  pred.val=factor2val(pred) #as.numeric(levels(pred)[pred]) #factor2val
  class.pred <- table(pred.val,crs$dataset[check.set,crs$target],dnn=c("Ist", "Vorausgesagt"))
  print(class.pred)
  if (just.sign)
    comp=apply(cbind(pred.val,crs$dataset[check.set,crs$target]),1,FUN=function(r)ifelse(sign(r[1])==sign(r[2]),0,1))
  else
    comp=apply(cbind(pred.val,crs$dataset[check.set,crs$target]),1,FUN=function(r)ifelse(r[1]==r[2],0,1))
  pred.err=sum(comp)/length(comp)*100
  mP("num %d, model.err 0 %f  ,   pred.err = %f",len(comp),crs$model.err,pred.err)
crs$pred.err=pred.err
}
#..............................................................................

check.classifier<-function(crs,fit,visual=T,mode=1)
{
  check.classifier.1(crs,fit,mode=3)
  check.classifier.1(crs,fit,mode=1)
  
  #browser(mP("wait"))
  #.................
  #library(gmodels)
  #CrossTable(pred,crs$dataset[crs$test,crs$target])
  library(irr)
  kappa=kappa2(coredata(merge(pred,crs$dataset[crs$test,crs$target])))
  mP("kappa ")
  print(kappa)
  if (visual)
  {
    # The `pROC' package implements various AUC functions.
    require(pROC, quietly=TRUE)
    # Calculate the Area Under the Curve (AUC).
    plot(roc(crs$rf$y, crs$rf$votes))
   
    # Calculate the AUC Confidence Interval.
   print("auc")
    ci.auc(crs$rf$y, crs$rf$votes)
  }
  
}


####################################################################################
#variablenwichtigkeit für rpart - trees 
#siehe: 
#http://www.redbrick.dcu.ie/~noel/R_classification.html
#und 
#http://stats.stackexchange.com/questions/6478/how-to-measure-rank-variable-importance-when-using-cart-specifically-using
#nicht nötig weil es nun gibt: crs$rpart$variable.importance
importance.rpart <- function(mytree) {
  
  # Calculate variable importance for an rpart classification tree
  
  # NOTE!! The tree *must* be based upon data that has the response (a factor)
  #        in the *first* column
  
  # Returns an object of class 'importance.rpart'
  
  # You can use print() and summary() to find information on the result
  
  delta_i <- function(data,variable,value) {
    # Calculate the decrease in impurity at a particular node given:
    
    #  data -- the subset of the data that 'reaches' a particular node
    #  variable -- the variable to be used to split the data
    #  value -- the 'split value' for the variable
    
    current_gini <- gini(data[,1])
    size <- length(data[,1])
    left_dataset <- eval(parse(text=paste("subset(data,",paste(variable,"<",value),")")))
    size_left <- length(left_dataset[,1])
    left_gini <- gini(left_dataset[,1])
    right_dataset <- eval(parse(text=paste("subset(data,",paste(variable,">=",value),")")))
    size_right <- length(right_dataset[,1])
    right_gini <- gini(right_dataset[,1])
    # print(paste("     Gini values: current=",current_gini,"(size=",size,") left=",left_gini,"(size=",size_left,"), right=", right_gini,"(size=",size_right,")"))
    current_gini*size-length(left_dataset[,1])*left_gini-length(right_dataset[,1])*right_gini
  }
  
  gini <- function(data) {
    # Calculate the gini value for a vector of categorical data
    numFactors = nlevels(data)
    nameFactors = levels(data)
    proportion = rep(0,numFactors)
    for (i in 1:numFactors) {
      proportion[i] = sum(data==nameFactors[i])/length(data)
    }
    1-sum(proportion**2)
  }
  
  frame <- mytree$frame
  splits <- mytree$splits
  allData <- eval(mytree$call$data)
  
  output <- ""
  finalAnswer <- rep(0,length(names(allData)))
  names(finalAnswer) <- names(allData)
  
  d <- dimnames(frame)[[1]]
  # Make this vector of length = the max nodeID
  # It will be a lookup table from frame-->splits
  index <- rep(0,as.integer(d[length(d)]))
  total <- 1
  for (node in 1:length(frame[,1])) {
    if (frame[node,]$var!="<leaf>") {
      nodeID <- as.integer(d[node])
      index[nodeID] <- total
      total <- total + frame[node,]$ncompete + frame[node,]$nsurrogate+ 1
    }
  }
  
  for (node in 1:length(frame[,1])) {
    if (frame[node,]$var!="<leaf>") {
      nodeID <- as.integer(d[node])
      output <- paste(output,"Looking at nodeID:",nodeID,"\n")
      output <- paste(output," (1) Need to find subset","\n")
      output <- paste(output,"   Choices made to get here:...","\n")
      data <- allData
      if (nodeID%%2==0) symbol <- "<"
      else symbol <- ">="
      i <- nodeID%/%2
      while (i>0) {
        output <- paste(output,"    Came from nodeID:",i,"\n")
        variable <- dimnames(splits)[[1]][index[i]]
        value <- splits[index[i],4]
        command <- paste("subset(allData,",variable,symbol,value,")")
        output <- paste(output,"      Applying command",command,"\n")
        data <- eval(parse(text=command))
        if (i%%2==0) symbol <- "<"
        else symbol <- ">="
        i <- i%/%2
      }
      output <- paste(output,"   Size of current subset:",length(data[,1]),"\n")
      
      output <- paste(output," (2) Look at importance of chosen split","\n")
      variable <- dimnames(splits)[[1]][index[nodeID]]	
      value <- splits[index[nodeID],4]
      best_delta_i <- delta_i(data,variable,value)
      output <- paste(output,"   The best delta_i is:",format(best_delta_i,digits=3),"for",variable,"and",value,"\n")
      finalAnswer[variable] <- finalAnswer[variable] + best_delta_i
      
      output <- paste(output,"                   Final answer: ",paste(finalAnswer,collapse=" "),"\n")
      
      output <- paste(output," (3) Look at importance of surrogate splits","\n")
      ncompete <- frame[node,]$ncompete
      nsurrogate <- frame[node,]$nsurrogate
      if (nsurrogate>0) {
        start <- index[nodeID]
        for (i in seq(start+ncompete+1,start+ncompete+nsurrogate)) {
          variable <- dimnames(splits)[[1]][i]
          value <- splits[i,4]
          best_delta_i <- delta_i(data,variable,value)
          output <- paste(output,"   The best delta_i is:",format(best_delta_i,digits=3),"for",variable,"and",value,"and agreement of",splits[i,3],"\n")
          finalAnswer[variable] <- finalAnswer[variable] + best_delta_i*splits[i,3]
          output <- paste(output,"                   Final answer: ",paste(finalAnswer[2:length(finalAnswer)],collapse=" "),"\n")
        }
      }
    }
  }
  result <- list(result=finalAnswer[2:length(finalAnswer)],info=output)
  class(result) <- "importance.rpart"
  result
}
print.importance.rpart <- function(self) {
  print(self$result)
}
summary.importance.rpart <- function(self) {
  cat(self$info)
}

######################

print("########### classifier_randomForest.r")
#sfSource("MLib/EM4.R")

if (F)
  list_R_functions('MLib/classifier_randomForest.r')


