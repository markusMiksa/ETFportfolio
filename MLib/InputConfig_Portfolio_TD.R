#TESTFILTER
#update.packages(checkBuilt=TRUE, ask=FALSE)


options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)


init.libraries <-function () 
{
  
  #C:\Program Files\R\R-3.0.1
  #c:\Users\markus\Documents\R\win-library\
  
  
  #install.packages('rgarch')#macht nen bug sp?ter weil xts first,last ?berdeckt nicht mehr vorh.und r.3
  try(detach("package:xts"),TRUE)
  #require(xts) 
  install.packages("gdata")
  #install.packages('quantmod') 
  #install.packages('RODBC')
  #library(zoo)
  install.packages('TTR')  
  install.packages('debug')  
  install.packages('RODBC')
  #install.packages('qmao')  #nicht unter 3
  
  #macht in excel aus einem umgeleiteten Dir
  #="install.packages('"&TRIM(MID(A8;SEARCH("<DIR> ";A8)+5;LEN(A8)))&"') "
  install.packages('gtools')
  
  install.packages('abind') 
  install.packages('Rcpp') 
  install.packages('acepack') 
  install.packages('AER') 
  install.packages('akima') 
  install.packages('alr3') 
  install.packages('anchors') 
  install.packages('ape') 
  install.packages('aplpack') 
  install.packages('backtest') 
  install.packages('BayesX') 
  install.packages('bdsmatrix') 
  install.packages('biglm') 
  install.packages('BiocInstaller') 
  install.packages('bit') 
  install.packages('bitops') 
  install.packages('car') 
  install.packages('caTools') 
  install.packages('chron') 
  install.packages('cmprsk') 
  install.packages('coda') 
  install.packages('coin') 
  install.packages('colorRamps') 
  install.packages('colorspace') 
  install.packages('CompQuadForm') 
  install.packages('coxme') 
  install.packages('cubature') 
  install.packages('data.table') 
  install.packages('DBI') 
  install.packages('Deducer') 
  install.packages('DeducerExtras') 
  install.packages('DeducerPlugInScaling') 
  install.packages('Defaults') 
  install.packages('degreenet') 
  install.packages('deldir') 
  install.packages('DEoptim') 
  install.packages('devtools') 
  install.packages('dichromat') 
  install.packages('digest') 
  install.packages('diptest') 
  install.packages('dlm') 
  install.packages('dlmodeler') 
  install.packages('doSMP') 
  install.packages('doSMP1') 
  install.packages('doSNOW') 
  install.packages('dtw') 
  install.packages('dyn') 
  install.packages('dynlm') 
  install.packages('e1071') 
  install.packages('Ecdat') 
  install.packages('effects') 
  install.packages('ellipse') 
  install.packages('ergm') 
  install.packages('evaluate') 
  install.packages('events') 
  install.packages('expsmooth') 
  install.packages('FactorAnalytics') 
  install.packages('fAsianOptions') 
  install.packages('fAssets') 
  install.packages('fBasics') 
  install.packages('fCopulae') 
  install.packages('fExoticOptions') 
  install.packages('fExtremes') 
  install.packages('fftw') 
  install.packages('fGarch') 
  install.packages('fgui') 
  install.packages('fields') 
  install.packages('fImport') 
  install.packages('flexmix') 
  install.packages('fMultivar') 
  install.packages('fNonlinear') 
  install.packages('fOptions') 
  install.packages('foreach') 
  install.packages('forecast') 
  install.packages('formatR') 
  install.packages('Formula') 
  install.packages('fPortfolio') 
  install.packages('fRegression') 
  install.packages('fSeries') 
  install.packages('fTrading') 
  install.packages('fUnitRoots') 
  install.packages('gam') 
  install.packages('gbm') 
  install.packages('gclus') 
  install.packages('gdata') 
  install.packages('gee') 
  install.packages('geoR') 
  install.packages('geoRglm') 
  install.packages('ggplot2') 
  install.packages('gpclib') 
  install.packages('gplots') 
  install.packages('gtable') 
  install.packages('gtools') 
  install.packages('gWidgets') 
  install.packages('gWidgetsRGtk2') 
  install.packages('gWidgetstcltk') 
  install.packages('hash') 
  install.packages('heatmap.plus') 
  install.packages('helpr') 
  install.packages('hergm') 
  install.packages('hexbin') 
  install.packages('HH') 
  install.packages('highlight') 
  install.packages('Hmisc') 
  install.packages('HSAUR') 
  install.packages('ineq') 
  install.packages('inline') 
  install.packages('ipred') 
  install.packages('irr') 
  install.packages('irutils') 
  install.packages('ISwR') 
  install.packages('iterators') 
  install.packages('itertools') 
  install.packages('its') 
  install.packages('jbryer-irutils-95e4309') 
  install.packages('JGR') 
  install.packages('Kendall') 
  install.packages('kernlab') 
  install.packages('KFAS') 
  install.packages('klaR') 
  install.packages('knitr') 
  install.packages('labeling') 
  install.packages('latentnet') 
  install.packages('latticeExtra') 
  install.packages('lawstat') 
  install.packages('leaps') 
  install.packages('lgtdl') 
  install.packages('lme4') 
  install.packages('lmtest') 
  install.packages('locfit') 
  install.packages('logspline') 
  install.packages('lpSolve') 
  install.packages('mapproj') 
  install.packages('maps') 
  install.packages('maptools') 
  install.packages('matrixcalc') 
  install.packages('MatrixModels') 
  install.packages('maxLik') 
  install.packages('mboost') 
  install.packages('mclust') 
  install.packages('MCMCpack') 
  install.packages('memoise') 
  install.packages('MEMSS') 
  install.packages('mhsmm') 
  install.packages('mice') 
  install.packages('misc3d') 
  install.packages('miscTools') 
  install.packages('mitools') 
  install.packages('mix') 
  install.packages('mlbench') 
  install.packages('mlmRev') 
  install.packages('mlogit') 
  install.packages('mnormt') 
  install.packages('modeltools') 
  install.packages('MPV') 
  install.packages('MSBVAR') 
  install.packages('msm') 
  install.packages('multcomp') 
  install.packages('munsell') 
  install.packages('mvbutils') 
  install.packages('mvna') 
  install.packages('mvnormtest') 
  install.packages('mvtnorm') 
  install.packages('network') 
  install.packages('networkDynamic') 
  install.packages('networksis') 
  install.packages('nor1mix') 
  install.packages('np') 
  install.packages('numDeriv') 
  install.packages('nws') 
  install.packages('openNLP') 
  install.packages('PairTrading') 
  install.packages('parser') 
  install.packages('party') 
  install.packages('PBSmapping') 
  install.packages('penalized') 
  install.packages('PerformanceAnalytics') 
  install.packages('dplyr') 
  
    install.packages('plm') 
  install.packages('plyr') 
  install.packages('png') 
  install.packages('polspline') 
  install.packages('polycor') 
  install.packages('pomp') 
  install.packages('PortfolioAnalytics') 
  install.packages('prodlim') 
  install.packages('proto') 
  install.packages('pscl') 
  #install.packages('qmao') 
  install.packages('quadprog') 
  install.packages('quantmod') 
  install.packages('quantreg') 
  install.packages('R2wd') 
  install.packages('RandomFields') 
  install.packages('randomForest') 
  install.packages('randtoolbox') 
  install.packages('RANN') 
  install.packages('RArcInfo') 
  install.packages('rbenchmark') 
  install.packages('Rcmdr') 
  install.packages('RColorBrewer') 
  install.packages('rcom') 
  install.packages('Rcpp') 
  install.packages('RcppArmadillo') 
  install.packages('Rdonlp2') 
  install.packages('relevent') 
  install.packages('relimp') 
  install.packages('reshape') 
  install.packages('reshape2') 
  install.packages('revoIPC') 
  install.packages('RExcelInstaller') 
  install.packages('rgarch') 
  install.packages('rgdal') 
  install.packages('rgenoud') 
  install.packages('rgeos') 
  install.packages('rgl') 
  install.packages('Rglpk') 
  install.packages('rj') 
  install.packages('rj.gd') 
  install.packages('rJava') 
  install.packages('rlecuyer') 
  install.packages('rmeta') 
  install.packages('rms') 
  install.packages('rngWELL') 
  install.packages('robustbase') 
  install.packages('ROCR') 
  install.packages('RODBC') 
  install.packages('roxygen2') 
  install.packages('rpanel') 
  install.packages('rparallel') 
  install.packages('RQuantLib') 
  install.packages('rscproxy') 
  install.packages('Rserve') 
  install.packages('Rsolnp') 
  install.packages('rsprng') 
  install.packages('RSQLite') 
  install.packages('RthroughExcelWorkbooksInstaller') 
  install.packages('RUnit') 
  install.packages('sampleSelection') 
  install.packages('sandwich') 
  install.packages('scales') 
  install.packages('scatterplot3d') 
  install.packages('sem') 
  install.packages('sfsmisc') 
  install.packages('sgeostat') 
  install.packages('shapefiles') 
  install.packages('shapes') 
  install.packages('sinartra') 
  install.packages('slam') 
  install.packages('sm') 
  install.packages('sn') 
  install.packages('sna') 
  install.packages('snow') 
  install.packages('snowfall') 
  install.packages('snowFT') 
  install.packages('sp') 
  install.packages('spam') 
  install.packages('SparseM') 
  install.packages('spatstat') 
  install.packages('spdep') 
  install.packages('splancs') 
  install.packages('stabledist') 
  install.packages('statmod') 
  install.packages('statnet') 
  install.packages('stockPortfolio') 
  install.packages('stringkernels') 
  install.packages('stringr') 
  install.packages('strucchange') 
  install.packages('subplex') 
  install.packages('subselect') 
  install.packages('SuppDists') 
  install.packages('survey') 
  install.packages('survival') 
  install.packages('systemfit') 
  install.packages('tcltk2') 
  install.packages('TeachingDemos') 
  install.packages('testthat') 
  install.packages('timeDate') 
  install.packages('timeSeries') 
  install.packages('tkrplot') 
  install.packages('tpr') 
  install.packages('tripack') 
  install.packages('truncnorm') 
  install.packages('truncreg') 
  install.packages('trust') 
  install.packages('TSA') 
  install.packages('tsDyn') 
  install.packages('tseries') 
  install.packages('tseriesChaos') 
  install.packages('TTR') 
  install.packages('ttrTests') 
  install.packages('tweedie') 
  install.packages('twsInstrument') 
  install.packages('urca') 
  install.packages('vars') 
  install.packages('vcd') 
  install.packages('VGAM') 
  install.packages('VIM') 
  install.packages('xlsReadWrite') 
  install.packages('xlsx') 
  install.packages('xlsxjars') 
  install.packages('XML') 
  install.packages('xtable') 
  install.packages('xts') 
  install.packages('Zelig') 
  install.packages('zoo') 
  install.package("gplots")
  install.package("XLConnect")
  
  install.packages("futile")
  install.packages("Futile.logger")
  install.packages("BurStFin")
  install.packages("reshape")
  install.packages("sos")
  install.packages("PortfolioAnalytics")
  install.packages("NMOF") 
  install.packages("pracma")
  
  install.packages("rChart")
  install.packages("Quandl")
  install.packages("doParallel")
  install.packages("foreach")
  install.packages("MASS")
  install.packages("grnn")
  install.packages("xtsExtra", repos="http://R-Forge.R-project.org")
  install.packages("rattle")
  install.packages("rattle", dependencies = c("Depends", "Suggests"))
  #daraus.folgt="
  #package 'sets' successfully unpacked and MD5 sums checked
#package 'Rsymphony' successfully unpacked and MD5 sums checked
#package 'relations' successfully unpacked and MD5 sums checked
 # package 'TSP' successfully unpacked and MD5 sums checked
  #package 'igraphdata' successfully unpacked and MD5 sums checked
#  package 'jpeg' successfully unpacked and MD5 sums checked
#  package 'clue' successfully unpacked and MD5 sums checked
#  package 'igraph0' successfully unpacked and MD5 sums checked
#  package 'oz' successfully unpacked and MD5 sums checked
#  package 'randomSurvivalForest' successfully unpacked and MD5 sums checked
#  package 'glmnet' successfully unpacked and MD5 sums checked
#  package 'pmmlTransformations' successfully unpacked and MD5 sums checked
#  package 'seriation' successfully unpacked and MD5 sums checked
#  package 'igraph' successfully unpacked and MD5 sums checked
#  package 'flexclust' successfully unpacked and MD5 sums checked
#  package 'isa2' successfully unpacked and MD5 sums checked
#  package 'trimcluster' successfully unpacked and MD5 sums checked
#  package 'prabclus' successfully unpacked and MD5 sums checked
#  package 'tclust' successfully unpacked and MD5 sums checked
#  package 'DAAG' successfully unpacked and MD5 sums checked
#  package 'waveslim' successfully unpacked and MD5 sums checked
#  package 'CircStats' successfully unpacked and MD5 sums checked
#  package 'clv' successfully unpacked and MD5 sums checked
#  package 'pmml' successfully unpacked and MD5 sums checked
#  package 'ada' successfully unpacked and MD5 sums checked
#  package 'amap' successfully unpacked and MD5 sums checked
#  package 'arules' successfully unpacked and MD5 sums checked
#  package 'arulesViz' successfully unpacked and MD5 sums checked
#  package 'biclust' successfully unpacked and MD5 sums checked
#  package 'cairoDevice' successfully unpacked and MD5 sums checked
#  package 'cba' successfully unpacked and MD5 sums checked
#  package 'descr' successfully unpacked and MD5 sums checked
#  package 'doBy' successfully unpacked and MD5 sums checked
#  package 'fpc' successfully unpacked and MD5 sums checked
#  package 'ggdendro' successfully unpacked and MD5 sums checked
#  package 'hmeasure' successfully unpacked and MD5 sums checked
#  package 'latticist' successfully unpacked and MD5 sums checked
#  package 'odfWeave' successfully unpacked and MD5 sums checked
#  package 'playwith' successfully unpacked and MD5 sums checked
#  package 'rggobi' successfully unpacked and MD5 sums checked
#  package 'RGtk2Extras' successfully unpacked and MD5 sums checked
#  package 'rpart.plot' successfully unpacked and MD5 sums checked
#  package 'Snowball' successfully unpacked and MD5 sums checked
#  Warning in install.packages :
#  package 'verification' successfully unpacked and MD5 sums checked
#  package 'weightedKmeans' successfully unpacked and MD5 sums checked
#  package 'rattle' successfully unpacked and MD5 sums checked"
  
  install.packages("caret", dependencies = c("Depends", "Suggests"))
#  daraus.folgt=
#    "package 'tensorA' successfully unpacked and MD5 sums checked
#  package 'energy' successfully unpacked and MD5 sums checked
#  package 'BRugs' successfully unpacked and MD5 sums checked
#  package 'compositions' successfully unpacked and MD5 sums checked
#  package 'robCompositions' successfully unpacked and MD5 sums checked
#  package 'R2WinBUGS' successfully unpacked and MD5 sums checked
#  package 'plotmo' successfully unpacked and MD5 sums checked
#  package 'plotrix' successfully unpacked and MD5 sums checked
#  package 'partykit' successfully unpacked and MD5 sums checked
#  package 'locfdr' successfully unpacked and MD5 sums checked
#  package 'flsa' successfully unpacked and MD5 sums checked
#  package 'pcaPP' successfully unpacked and MD5 sums checked
#  package 'mvoutlier' successfully unpacked and MD5 sums checked
#  package 'glasso' successfully unpacked and MD5 sums checked
#  package 'entropy' successfully unpacked and MD5 sums checked
#  package 'fdrtool' successfully unpacked and MD5 sums checked
#  package 'arm' successfully unpacked and MD5 sums checked
#  package 'Boruta' successfully unpacked and MD5 sums checked
#  package 'bst' successfully unpacked and MD5 sums checked
#  package 'C50' successfully unpacked and MD5 sums checked
#  package 'Cubist' successfully unpacked and MD5 sums checked
#  package 'earth' successfully unpacked and MD5 sums checked
#  package 'elasticnet' successfully unpacked and MD5 sums checked
#  package 'evtree' successfully unpacked and MD5 sums checked
#  package 'extraTrees' successfully unpacked and MD5 sums checked
#  package 'fastICA' successfully unpacked and MD5 sums checked
#  package 'foba' successfully unpacked and MD5 sums checked
#  package 'glmnet' successfully unpacked and MD5 sums checked
#  package 'hda' successfully unpacked and MD5 sums checked
#  package 'HDclassif' successfully unpacked and MD5 sums checked
#  package 'HiDimDA' successfully unpacked and MD5 sums checked
#  package 'kknn' successfully unpacked and MD5 sums checked
#  package 'kohonen' successfully unpacked and MD5 sums checked
#  package 'KRLS' successfully unpacked and MD5 sums checked
#  package 'lars' successfully unpacked and MD5 sums checked
#  package 'LogicReg' successfully unpacked and MD5 sums checked
#  package 'mda' successfully unpacked and MD5 sums checked
#  package 'neuralnet' successfully unpacked and MD5 sums checked
#  package 'nodeHarvest' successfully unpacked and MD5 sums checked
#  package 'obliqueRF' successfully unpacked and MD5 sums checked
#  package 'pamr' successfully unpacked and MD5 sums checked
#  package 'partDSA' successfully unpacked and MD5 sums checked
#  package 'penalizedLDA' successfully unpacked and MD5 sums checked
#  package 'pls' successfully unpacked and MD5 sums checked
#  package 'protoclass' successfully unpacked and MD5 sums checked
#  package 'qrnn' successfully unpacked and MD5 sums checked
#  package 'quantregForest' successfully unpacked and MD5 sums checked
#  package 'relaxo' successfully unpacked and MD5 sums checked
#  package 'rFerns' successfully unpacked and MD5 sums checked
#  package 'rocc' successfully unpacked and MD5 sums checked
#  package 'rrcov' successfully unpacked and MD5 sums checked
#  package 'RRF' successfully unpacked and MD5 sums checked
#  package 'rrlda' successfully unpacked and MD5 sums checked
#  package 'RSNNS' successfully unpacked and MD5 sums checked
#  package 'sda' successfully unpacked and MD5 sums checked
#  package 'SDDA' successfully unpacked and MD5 sums checked
#  package 'sparseLDA' successfully unpacked and MD5 sums checked
#  package 'spls' successfully unpacked and MD5 sums checked
#  package 'stepPlr' successfully unpacked and MD5 sums checked
#  package 'superpc' successfully unpacked and MD5 sums checked
#  package 'caret' successfully unpacked and MD5 sums checked"
  
  install.packages("Rcmdr")
  #library(Rcmdr)  #startet die Gui
  
  install.packages("TSclust")   #zeitreihen-abst?nde
  install.packages("GenSA")
  install.packages("GenSA")
  #install.packages("SmarterPoland")
  #gui
  install.packages("Deducer", dependencies = c("Depends", "Suggests"))
  install.packages("rminer", dependencies = c("Depends", "Suggests"))
  
  ##################
  #sehr interessant:
  
  #  http://www.unstarched.net/r/dbm/
  #  #https://r-forge.r-project.org/softwaremap/trove_list.php
  #  https://r-forge.r-project.org/R/?group_id=1721
  
  
  #install.packages("TEATIME", source = "http://r-forge.r-project.org")
  #install.packages("dbm")
  #install.packages("tsanalysis", source = "http://r-forge.r-project.org")
  install.packages("RCurl")
  install.packages("googleVis")
  install.packages("date")
  install.packages("dbm","https://r-forge.r-project.org/R/?group_id=1721")
  install.packages("fastICA") 
  install.packages("sqldf") #http://www.r-bloggers.com/how-to-select-and-merge-r-data-frames-with-sql/ 
  install.packages("mvtnorm")
  install.packages("FinancialInstrument")
  install.packages("SmarterPoland")
  
  install.packages('knitr', dependencies = TRUE)
  #  install.packages("fasttime")
  install.packages("rattle", repos="http://rattle.togaware.com", type="source")
  install.packages("RWeka",dependencies=T) 
  install.packages("RcppArmadillo")
  install.packages("digest")
  install.packages("roxygen2")
  
  install.packages("magic")

  install.packages("varSelRF")
  install.packages("tabplot")
  install.packages("tabplotd3")
  
  install.packages("gmodels")
  install.packages("irr")

  install.packages("R.utils")
  
}

library(lib.loc = .Library)
.libPaths()
library(gdata)
library(XLConnect)
#library(lib.loc = .libPaths())

##Einf?hrung:  http://freecode.com/articles/creating-charts-and-graphs-with-gnu-r

#library("tabplot")
#library("tabplotd3")

#library(RGtk2 )
library(RWeka)
library(knitr)
library("RCurl")
library(magic)
#library("GTrendsR")

library(PerformanceAnalytics)
#data(edhec)
#charts.PerformanceSummary(edhec[,c(1,13)])

library(forecast)
library(gplots)
library(heatmap.plus)
require(ggplot2)
#require(XLConnect)
library(methods)
library(data.table)
library(RColorBrewer)
library(xts)
#library(SmarterPoland)

#library(plyr)

library(vars)
library (RODBC)
library(stringr) #file:///C:/Users/markus/Documents/R/win-library/2.14/stringr/html/00Index.html
require(quantmod)

#require(PortfolioAnalytics)
require(lattice)
require(latticeExtra)
require(colorRamps)
library(XML)
library(hash)
library(chron)
library(timeDate)
#library(fUnitRoots)  mach PerformanceAnalytics kaputt
library(compiler)
library(TTR)
#library(futile)
#library(PerformanceAnalytics)
library("stringr")
library("caTools")
library("snowfall")
#library(tawny)

#library(futile.logger) #http://cartesianfaith.wordpress.com/2013/03/10/better-logging-in-r-aka-futile-logger-1-3-0-released/
if (F)
{
  tableplot(iris)
}

if (F)
{
  #  library(tawny)
  #  library(fractalrock)
  prices <- getPortfolioPrices(LETTERS[1:10], 100)
  returns <- apply(prices,2,Delt)[-1,]
  s <- cov.shrink(as.xts(returns,order.by=index(prices)[-1]))
  flog.threshold(WARN, name='tawny')
  s <- cov.shrink(as.xts(returns,order.by=dates[-1]))
  flog.info("Got covariance matrix")
}

#library(tawny)
library("sos")    
#findFn("hurst exponent")  
#hurst(brown72)                    #in pracma   # 0.7385   # 0.720
#hurstexp(brown72, d = 128)

library(NMOF) 
library(pracma)

library(urca)  #johansen ca.co
#lade charts.PerformanceSummaryX  . die j?hrliche Balkengrafik des Retunrs
#library(qmao)
library(foreach)
library(parallel)
##### 1.1.2014
library("varSelRF")



options(show.error.messages = TRUE)
#con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))

#con = gzcon(file('../frameworks/SystematicInvestorToolbox/code.r', 'rb'))
con = gzcon(file('SysInvestor/code.r', 'rb'))

source(con)
print("########### load SysInvestor code")
close(con) 
source("MLib/SITpatches.r")


last = xts::last    #wird leider von data.table ungut ?beladen!!


options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink();
}))



### end of script
#########################################################################
source("MLib/InputConfig_Portfolio_TD1.r")
source("MLib/InputConfig_Portfolio_TD2.r")
source("MLib/InputConfig_Portfolio_TD3.r")
source("MLib/InputConfig_Portfolio_TD4.r")
source("MLib/InputConfig_Portfolio_TD5.r")

source("MLib/CustomerReport.r")
source("MLib/Gui.r")
source("MLib/attention.r")
print("########### load InputConfig_Portfolio_TD.R")

if (F)
{
  list_R_functions('MLib/InputConfig_Portfolio_TD.R')
  list_R_functions()
  list_R_functions(search="mP")
  list_R_functions('MLib/now.r')
  list_SIT_functions()
  
}