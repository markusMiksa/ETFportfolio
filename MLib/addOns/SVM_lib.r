#Gute Chanse mit SVM - Klassifikatoren Timing-Modelle robust zu verbinden.  Sehr viel versprechende Ergebnisse  (so gut wie Garch) 
#mit  #MM!SVM_RSI  s.u.  und einer Eingangsschicht von RSI-Detektoren.

#MM!SVM

#Trading with Support Vector Machines (SVM)   #MMA
#
#Einleitung:  http://quantumfinancier.wordpress.com/2010/05/21/application-of-svms/
#Einleitung=
"From wikipedia: given a set of training examples, each marked as belonging to one of two categories, an 
SVM training algorithm builds a model that predicts whether a new example falls into one category or the other. 
Intuitively, an SVM model is a representation of the examples as points in space, mapped so that the examples of the 
separate categories are divided by a clear gap that is as wide as possible. New examples are then mapped into that same space 
and predicted to belong to a category based on which side of the gap they fall on.

In concrete terms, the training algorithm separate the data into up days and down days then look at the predictors value. 
It then creates a set of rules dividing the data; these rules minimize the classification error while also maximizing the margin of 
safety, thus giving the model more room, resulting (hopefully) in a greater accuracy. Based on this set of rules, the algorithm classifies 
new data in a category. Note that this is not a numeric prediction (i.e. next day return should be xx%) 
it is a binary factor prediction, thus allowing us to derive a probability along with the prediction. It comes in 
handy when we want a confidence based system.

This is a nice technique, but it has its drawbacks. First of all, it is a parameter dependant technique. 
The effectiveness of the svm is mostly determined by two parameters. It is usually recommended to test different values 
for the pair and to retain the pair that performs best during cross-validation for the model. This can become quite 
computationally annoying. Without getting to technical (or into details), if we want a non-linear classification algorithm, 
we have to choose the type of kernel function we want; there is several.
"
#http://www.quintuitive.com/2012/11/30/trading-with-support-vector-machines-svm/
#https://gist.github.com/ivannp/4180399

#SVM-Test-Code   #MM!SVM1
#http://www.quintuitive.com/2012/11/30/trading-with-support-vector-machines-svm/

#Very nice! Using the 5 lagged daily returns shows similar performance to the ARMA+GARCH strategy, which 
#I found very promising. If you wonder why I am so 
#excited about this fact, it’s because here we are in the area where ARMA+GARCH is best, and yet, SVMs show comparable performance.

#http://www.r-bloggers.com/trading-with-support-vector-machines-svm/


if (F)
{
  require(e1071)
  require(quantmod)
  require(parallel)
  
  source("e1071.R")
  
  tt = get( getSymbols( "^GSPC", from="1900-01-01" ) )
  
  rets = na.trim( ROC( Cl( tt ), type="discrete" ) )
  
  # only the first two features so that we may see some results in reasonable time
  data = svmFeatures( tt )[,c(1,2)]
  
  rets = rets[index(data)]
  data = data[index(rets)]
  
  stopifnot( NROW( rets ) == NROW( data ) )
  
  fore = svmComputeForecasts(
    data=data,
    history=500,
    response=rets,
    cores=8,
    trace=T,
    modelPeriod="days",
    startDate="1959-12-28",
    endDate="1959-12-31",
    featureSelection="all" )
  
}
########################################
#Trading with SVMs: Performance
#To get a feeling of SVM performance in trading, I run different setups on the S&P 500 historical data from … the 50s. T
#http://www.quintuitive.com/2012/12/13/trading-with-svms-performance/

#findFn("backtest")
###########> SVM-LiB

#check the svm-lib:  (http://caret.r-forge.r-project.org/index.html)?

svmComputeOneForecast = function(   #MMA_svm_feature_select
  id,
  data,
  response,
  startPoints,
  endPoints,
  len,
  history=500,
  trace=FALSE,
  kernel="radial",
  gamma=10^(-5:-1),
  cost=10^(0:2),
  sampling="cross",
  seed=1234,
  featureSelection=c("add", "prune", "all"),
  cross=10)
{
  # Determine the forecast length
  startIndex = startPoints[id]
  endIndex = endPoints[id]
  
  forecastLength = endIndex - startIndex + 1
  
  # A row in the data is responsible for the corresponding value in the
  # response. Thus, to forecast day X, we train the model on the previous
  # *history* days, and then use the features for day X to forecast.
  xtsData = data[index(data)[(startIndex-history):(startIndex-1)]]
  xtsResponse = response[index(response)[(startIndex-history):(startIndex-1)]]
  
  # Convert the input data and response to a matrix and a vector, respectively
  xx = as.matrix( coredata( xtsData ) )
  yy = as.vector( coredata( xtsResponse ) )
  
  # We need to set the seed to have reprodcible results
  set.seed( seed )
  
  if(featureSelection[1] == "add") {
    # We add the features one by one, until we cannot improve the error
    best = NULL
    bestPerf = 1e9
    
    # Maintained sorted, the content are the column indexes in the original matrix
    features = c()
    availableFeatures = seq(1,ncol(xx))
    
    # Use greedy approach to add features
    repeat {
      bestColIdToAdd = 0L
      # print( features )
      for(colId in 1:length(availableFeatures)) {
        # Get the matrix for the current tunning and tune
        zz = xx[,sort(c(features, availableFeatures[colId]))]
        # print(paste(sep="", "trying adding feature ", availableFeatures[colId]))
        newSvm = tune( svm,
                       train.x=zz,
                       train.y=yy,
                       ranges=list( gamma=gamma, cost=cost ),
                       tunecontrol=tune.control( sampling=sampling, cross=cross ),
                       kernel=kernel )
        # Check the performance improvement
        newPerf = round(newSvm$best.performance, 8)
        # print( paste( sep="", "new performance=", newPerf ) )
        if(newPerf < bestPerf) {
          # print( paste( sep="", "old performance=", bestPerf, ", new performance=", newPerf ) )
          best = newSvm
          bestPerf = newPerf
          bestColIdToAdd = colId
        }
      }
      
      if(bestColIdToAdd > 0) {
        # print( paste( sep="", "improvement, adding feature ", availableFeatures[bestColIdToAdd] ) )
        
        # Found an improvement, update the features
        features = sort(c(features, availableFeatures[bestColIdToAdd]))
        availableFeatures = availableFeatures[-bestColIdToAdd]
        
        # Exit if no features left
        if(length(availableFeatures) == 0) break
      } else {
        
        # No improvements, done
        break
      }
    }
  } else {
    # Train the SVM
    # ss = svm( x=xx, y=yy, kernel=kernel, gamma=gamma[1], cost=cost[1] )
    best = tune( svm,
                 train.x=xx,
                 train.y=yy,
                 ranges=list( gamma=gamma, cost=cost ),
                 tunecontrol=tune.control( sampling=sampling, cross=cross ),
                 kernel=kernel )
    
    # print( "gotBest" )
    # print( paste( sep="", "performance=", round( best$best.performance, 6 ) ) )
    
    # An array to keep track of the original participating features (by index)
    features = seq(1,ncol(xx))
    
    # print( length( features ) )
    
    # Use greedy approach to prune features
    if(featureSelection[1] == "prune") {
      repeat {
        bestColIdToRemove = 0L
        # print( features )
        for(colId in 1:ncol(xx)) {
          # Remove column colId
          zz = xx[,-colId]
          
          # print( paste( sep="", "trying without feature ", colId ) )
          
          # Tune with the reduced number of columns
          newBest = tune( svm,
                          train.x=zz,
                          train.y=yy,
                          ranges=list( gamma=gamma, cost=cost ),
                          tunecontrol=tune.control( sampling=sampling, cross=cross ),
                          kernel=kernel )
          # print( paste( sep="", "new performance=", round( newBest$best.performance, 6 ) ) )
          if(round( newBest$best.performance, 6 ) < round( best$best.performance, 6)) {
            best = newBest
            bestColIdToRemove = colId
            # print( paste( sep="", "old performance=", round( best$best.performance, 6 ),
            #              ", new performance=", round( newBest$best.performance, 6 ) ) )
          }
        }
        
        if(bestColIdToRemove > 0) {
          # Found an improvement
          xx = xx[,-bestColIdToRemove]
          features = features[-bestColIdToRemove]
          
          # print( paste( sep="", "improvement, removed feature ", bestColIdToRemove ) )
          
          # Break if there is only a single feature left
          if(length(features) == 1) break
        } else {
          # No improvements, done
          break
        }
      }
    }
  }
  
  # print( paste( sep="", "final features: (", paste( sep=",", collapse=",", features ), ")" ) )
  
  # Predict using the SVM, use only the remaining features
  xtsNewData = data[index(data)[startIndex:endIndex]]
  newData = as.matrix( coredata( xtsNewData[,features] ) )
  fore = predict( best$best.model, newData )
  
  if( trace ) {
    str = paste( sep="",
                 "\n", index(response)[startIndex], "\n",
                 "=======================\n",
                 "   from: ", head(index(xtsResponse),1),
                 ", to: ", tail(index(xtsResponse),1),
                 ", length: ", length(index(xtsResponse)),
                 "\n   new data: from: ", head(index(xtsNewData), 1),
                 ", to: ", tail(index(xtsNewData), 1),
                 ", length: ", NROW(xtsNewData),
                 "\n   forecast length: ", forecastLength,
                 "\n   best model performance: ", round( best$best.performance, 6 ),
                 "\n   best model features: (", paste( collapse=",", features), ")",
                 "\n   best model gamma: ", best$best.model$gamma,
                 "\n   best model cost: ", best$best.model$cost,
                 "\n   forecasts: ",
                 paste( collapse=", ", round( fore, 6 ) ),
                 "\n" )
    cat( sep="", str )
  }
  
  return( list( index=startIndex,
                forecasts=fore,
                performance=best$best.performance,
                features=features,
                gamma=best$best.model$gamma,
                cost=best$best.model$cost ) )
}

svmComputeForecasts = function(
  data,
  response,
  history=500,
  modelPeriod="days",
  modelPeriodMultiple=1,
  trace=TRUE,
  startDate,
  endDate,
  kernel="radial",
  gamma=10^(-5:-1),
  cost=10^(0:2),
  sampling="cross",
  cross=10,
  featureSelection=c("add", "prune", "all"),
  cores)
{
  require( e1071 )
  
  stopifnot( NROW( data ) == NROW( response ) )
  
  len = NROW( response )
  
  # Determine the starting index
  if( !missing( startDate ) )
  {
    startIndex = max( len - NROW( index( data[paste( sep="", startDate, "/" )] ) ) + 1,
                      history + 2 )
  }
  else
  {
    startIndex = history + 2
  }
  
  # Determine the ending index
  if( missing( endDate ) )
  {
    lastIndex = len
  }
  else
  {
    lastIndex = NROW( index( data[paste( sep="", "/", endDate )] ) )
  }
  
  if( startIndex > lastIndex )
  {
    return( NULL )
  }
  
  modelPeriod = tolower( modelPeriod[1] )
  
  forecasts = rep( NA, len )
  gammas = rep( NA, len )
  costs = rep( NA, len )
  performances = rep( NA, len )
  features = rep( "", len )
  
  # Get the interesting indexes
  periods = index(data)[startIndex:lastIndex]
  
  # Compute the end points for each period (day, week, month, etc)
  endPoints = endpoints( periods, modelPeriod, modelPeriodMultiple )
  
  # Compute the starting points of each period, relative to the *data* index
  startPoints = endPoints + startIndex
  
  # Remove the last start point - it's outside
  length(startPoints) = length(startPoints) - 1
  
  # Make the end points relative to the *data* index
  endPoints = endPoints + startIndex - 1
  
  # Remove the first end point - it's always zero
  endPoints = tail( endPoints, -1 )
  
  stopifnot( length( endPoints ) == length( startPoints ) )
  
  if( missing( cores ) ) {
    cores = 1
  }
  
  res = mclapply( seq(1,length(startPoints)),
                  svmComputeOneForecast,
                  data=data,
                  response=response,
                  startPoints=startPoints,
                  endPoints=endPoints,
                  len=len,
                  history=history,
                  trace=TRUE,
                  kernel=kernel,
                  gamma=gamma,
                  cost=cost,
                  featureSelection=featureSelection,
                  mc.cores=cores )
  for( ll in res )
  {
    # Prepare the indexes 
    ii = ll[["index"]]
    jj = ii + NROW( ll[["forecasts"]] ) - 1
    
    # Copy the output
    forecasts[ii:jj] = ll[["forecasts"]]
    gammas[ii:jj] = ll[["gamma"]]
    costs[ii:jj] = ll[["cost"]]
    performances[ii:jj] = ll[["performance"]]
    
    # Encode the participating features as a bit mask stored in a single
    # integer. This representation limits us to max 32 features.
    features[ii:jj] = sum( 2^( ll[["features"]] - 1 ) )
  }
  
  sigUp = ifelse( forecasts >= 0, 1, 0 )
  sigUp[is.na( sigUp )] = 0
  
  sigDown = ifelse( forecasts < 0, -1, 0 )
  sigDown[is.na( sigDown)] = 0
  
  # forecasts[is.na( forecasts )] = 0
  
  sig = sigUp + sigDown
  
  res = merge( reclass( sig, response ),
               reclass( sigUp, response ),
               reclass( sigDown, response ),
               na.trim( reclass( forecasts, response ) ),
               reclass( performances, response ),
               reclass( gammas, response ),
               reclass( costs, response ),
               reclass( features, response ),
               all=F )
  colnames( res ) = c( "Indicator", "Up", "Down", "Forecasts", "Performance", "Gamma", "Cost", "Features" )
  
  return( res )
}

svmFeatures = function(series)
{
  require(PerformanceAnalytics)
  
  close = Cl(series)
  
  rets = na.trim(ROC(close, type="discrete"))
  
  # 1-day, 2-day, 3-day, 5-day, 10-day, 20-day and 50-day returns
  res = merge(na.trim(lag(rets, 1)),
              na.trim(lag(ROC(close, type="discrete", n=2), 1)),
              na.trim(lag(ROC(close, type="discrete", n=3), 1)),
              na.trim(lag(ROC(close, type="discrete", n=5), 1)),
              na.trim(lag(ROC(close, type="discrete", n=10), 1)),
              na.trim(lag(ROC(close, type="discrete", n=20), 1)),
              na.trim(lag(ROC(close, type="discrete", n=50), 1)),
              all=FALSE)
  
  # Add mean, median, sd, mad, skew and kurtosis
  res = merge(res,
              xts(na.trim(lag(rollmean(rets, k=21, align="right"),1))),
              xts(na.trim(lag(rollmedian(rets, k=21, align="right"),1))),
              xts(na.trim(lag(rollapply(rets, width=21, align="right", FUN=sd),1))),
              xts(na.trim(lag(rollapply(rets, width=21, align="right", FUN=mad),1))),
              xts(na.trim(lag(rollapply(rets, width=21, align="right", FUN=skewness),1))),
              xts(na.trim(lag(rollapply(rets, width=21, align="right", FUN=kurtosis),1))),
              all=FALSE)
  
  # Add volume with a lag of two
  res = merge(res, xts(na.trim(lag(Vo(series),2))), all=FALSE)
  
  colnames(res) = c("ROC.1", "ROC.2", "ROC.3", "ROC.5", "ROC.10", "ROC.20", "ROC.50",
                    "MEAN", "MEDIAN", "SD", "MAD", "SKEW", "KURTOSIS",
                    "VOLUME")
  
  return(res)
}

#############################################
#There is still a long list of topics to explore, just to give you an idea, in no particular order:

#Add other features. Mostly thinking of adding some Fed-related series, this data goes back to 1960, so it’s coming soon.:)
#Try other svm parameters: other regressions, other classifications, other kerenls, etc. This is more like a stability test.
#Try other error functions. The default is to use the mean square error, but in the case of regression, why not use Sharpe Ratio (in
#-sample)? The regression case is simpler, since we have the actual returns – check the input of tune.control.
#Try longer periods instead of days. Weekly is a start, but ideally I’d like to implement two or three day periods.
#Vary the loopback period.
#Use more classes with classification: large days, medium days, etc."

#############################################
#obiger Author verweist auf svm-Classifikation mit rsi-Indikatoren:

#code in
#http://quantumfinancier.wordpress.com/2010/06/26/support-vector-machine-rsi-system/
#############################################

#Support Vector Machine RSI System     #MM!SVM_RSI

#Posted by Quantum Financier on June 26, 2010
#Better late than never, as promised, the R code for the SVM system discussed in a previous post.

#Beschreibung 
#http://quantumfinancier.wordpress.com/2010/06/10/svm-classification-using-rsi-from-various-lengths/
#Here is the system :

SVMClassifModel = function(data, targets, returns, lookback = 252, ktype = "C-svc", crossvalid = 10, C = 10) {
  # Construct a predictive model using support vector machine
  # Input data must be lagged one period to avoid look-ahead bias
  # Print predictions and confidence, accuracy, equity curves plot, and performance statistics v.s. benchmark
  
  # Libraries
  require(kernlab)
  require(quantmod)
  
  # Make sure targets is a factor (for classification)
  targets = as.factor(targets)
  data$targets = as.factor(data$targets)
  
  # Generate indexes for backtest
  idx = data.frame(targets = lookback:(nrow(data)-1))
  
  # Isolate index to be used later
  inx = index(returns[idx$targets])
  
  # Prediction function to be used for backtesting
  pred1pd = function(t) {
    # Train model
    model = trainSVM(data[(t-lookback):t, ], ktype, C, crossvalid)
    # Prediction
    pred = predict(model, data[t+1, -1], type="prob")
    # Print for user inspection
    print(pred)
  }
  
  # backtest by looping over the calendar previously generated
  preds = sapply(idx$targets, pred1pd)
  # print output
  print(preds)
  print(max.col(preds))
  preds = data.frame(t(rbind(mle = max.col(t(preds)), preds)))
  print(preds)
  print(summaryStats((returns[idx$targets] * (preds$mle*2-3)), returns[idx$targets], comp = TRUE))
  
  #Equity curves
  equity = xts(cumprod((returns[idx$targets] * (preds$mle*2-3))+1), inx)
  Benchmark = xts(cumprod(returns[idx$targets] + 1), inx)
  
  # y axis values range
  yrngMin = abs(min(equity, Benchmark))
  yrngMax = abs(max(equity, Benchmark))
  
  # Plot curves
  chartSeries(equity, log.scale = TRUE, name='Equity Curves', yrange=c(yrngMin, yrngMax))
  addTA(Benchmark, on=1, col='gold')
}

trainSVM = function(data, ktype, C, crossvalid) {
  # Return a trained svm model
  trainedmodel = ksvm(targets ~ ., data = data, type = ktype, kernel="rbfdot", kpar=list(sigma=0.05), C = C, prob.model = TRUE, cross = crossvalid)
}

featureGen = function(sym, returns) {
  # Return a data frame to be used as input by the SVM system
  
  # Targets vector
  targets = coredata(returns)
  targets[targets>=0] = 1
  targets[targets<0] = -1
  targets = as.factor(targets)
  
  #RSIs
  rsi2 = RSI(Cl(sym), 2 )
  rsi3 = RSI(Cl(sym), 3 )
  rsi4 = RSI(Cl(sym), 4 )
  rsi5 = RSI(Cl(sym), 5 )
  rsi6 = RSI(Cl(sym), 6 )
  rsi7 = RSI(Cl(sym), 7 )
  rsi8 = RSI(Cl(sym), 8 )
  rsi9 = RSI(Cl(sym), 9 )
  rsi10 = RSI(Cl(sym), 10 )
  rsi11 = RSI(Cl(sym), 11 )
  rsi12 = RSI(Cl(sym), 12 )
  rsi13 = RSI(Cl(sym), 13 )
  rsi14 = RSI(Cl(sym), 14 )
  rsi15 = RSI(Cl(sym), 15 )
  rsi16 = RSI(Cl(sym), 16 )
  rsi17 = RSI(Cl(sym), 17 )
  rsi18 = RSI(Cl(sym), 18 )
  rsi19 = RSI(Cl(sym), 19 )
  rsi20 = RSI(Cl(sym), 20 )
  rsi21 = RSI(Cl(sym), 21 )
  rsi22 = RSI(Cl(sym), 22 )
  rsi23 = RSI(Cl(sym), 23 )
  rsi24 = RSI(Cl(sym), 24 )
  rsi25 = RSI(Cl(sym), 25 )
  rsi26 = RSI(Cl(sym), 26 )
  rsi27 = RSI(Cl(sym), 27 )
  rsi28 = RSI(Cl(sym), 28 )
  rsi29 = RSI(Cl(sym), 29 )
  rsi30 = RSI(Cl(sym), 30 )
  
  # lagged RSIs to correspond RSI with target period
  rsi2 = Lag(rsi2, 1)
  rsi3 = Lag(rsi3, 1)
  rsi4 = Lag(rsi4, 1)
  rsi5 = Lag(rsi5, 1)
  rsi6 = Lag(rsi6, 1)
  rsi7 = Lag(rsi7, 1)
  rsi8 = Lag(rsi8, 1)
  rsi9 = Lag(rsi9, 1)
  rsi10 = Lag(rsi10, 1)
  rsi11 = Lag(rsi11, 1)
  rsi12 = Lag(rsi12, 1)
  rsi13 = Lag(rsi13, 1)
  rsi14 = Lag(rsi14, 1)
  rsi15 = Lag(rsi15, 1)
  rsi16 = Lag(rsi16, 1)
  rsi17 = Lag(rsi17, 1)
  rsi18 = Lag(rsi18, 1)
  rsi19 = Lag(rsi19, 1)
  rsi20 = Lag(rsi20, 1)
  rsi21 = Lag(rsi21, 1)
  rsi22 = Lag(rsi22, 1)
  rsi23 = Lag(rsi23, 1)
  rsi24 = Lag(rsi24, 1)
  rsi25 = Lag(rsi25, 1)
  rsi26 = Lag(rsi26, 1)
  rsi27 = Lag(rsi27, 1)
  rsi28 = Lag(rsi28, 1)
  rsi29 = Lag(rsi29, 1)
  rsi30 = Lag(rsi30, 1)
  
  # Data frame
  data = data.frame(targets, rsi2, rsi3, rsi4, rsi5, rsi6, rsi7, rsi8, rsi9, rsi10, rsi11, rsi12, rsi13, rsi14, rsi15, rsi16, rsi17, rsi18, rsi19, rsi20, rsi21, rsi22, rsi23, rsi24, rsi25, rsi26, rsi27, rsi28, rsi29, rsi30)
  # names(data) = c("targets", "data")
  
  # Results
  return(data)
}

##################### joshua ulrichs kürzere Variante ################

featureGen2 = function(sym, n=30, steps=1) {
  # Return a data frame to be used as input by the SVM system
  targets = as.factor(ifelse(dailyReturn(Cl(sym), type='log')>=0,1,-1))
  data = data.frame(targets)
  
  # lagged RSIs to correspond RSI with target period
  for(i in seq.int(2,n,steps)) {
    data[,paste("rsi",i,sep="")] = as.numeric(Lag(RSI(Cl(sym),i),1))
  }
  rownames(data) = index(sym)
  
  # Results
  return(data)
}


summaryStats = function(x, bmk, comp = FALSE) {
  #Required library
  require(PerformanceAnalytics)
  
  #Compute stats of interest for strategy
  cumRetx = Return.cumulative(x)
  annRetx = Return.annualized(x, scale=252)
  sharpex = SharpeRatio.annualized(x, scale=252)
  winpctx = length(x[x > 0])/length(x[x != 0])
  annSDx = sd.annualized(x, scale=252)
  maxDDx = maxDrawdown(x)
  avDDx = mean(Drawdowns(x))
  
  if(comp == TRUE) {
    #Compute stats of interest for benchmark
    cumRetbmk = Return.cumulative(bmk)
    annRetbmk = Return.annualized(bmk, scale=252)
    sharpebmk = SharpeRatio.annualized(bmk, scale=252)
    winpctbmk = length(bmk[bmk > 0])/length(bmk)
    annSDbmk = sd.annualized(bmk, scale=252)
    maxDDbmk = maxDrawdown(bmk)
    avDDbmk = mean(Drawdowns(bmk))
    #Return result vectors
    Benchmark = c(cumRetbmk, annRetbmk, sharpebmk, winpctbmk, annSDbmk, maxDDbmk, avDDbmk)
    Strategy = c(cumRetx, annRetx, sharpex, winpctx, annSDx, maxDDx, avDDx)
    nms = c("Cumulative Return", "Annualized Return", "Annualized Sharpe Ratio", "Winning Percentage", "Annualized Volatility", "Maximum Drawdown", "Average Drawdown")
    result = data.frame(Strategy, Benchmark, row.names = nms)
  } else {
    #Return result vectors
    nms = c("Cumulative Return", "Annualized Return", "Annualized Sharpe Ratio", "Winning Percentage", "Annualized Volatility", "Maximum Drawdown", "Average Drawdown")
    Strategy = c(cumRetx, annRetx, sharpex, winpctx, annSDx, maxDDx, avDDx)
    result = data.frame(Strategy, row.names = nms)
  }
  return(result)
}

#Here is the harness used to use the system. Don’t forget to change the first two line of the code and replace with your directory.
#############################################################

if (F)
{
  #For example:     
  #setwd(“C:\Users\John Doe\Documents”)
  #source(“SVM System”)
  
  #setwd("INPUT DIRECTORY")
  #source("NAME OF THE RSI SYSTEM FILE IN THE FOLDER")
  require(quantmod)
  require(PerformanceAnalytics)
  
  # Load data with quantmod
  
  #getSymbols('SPY', from='2000-06-01')
  #returns = dailyReturn(Cl(SPY), type='log')
  
  getSymbols('^GDAXI', from='2000-06-01')
  prices = GDAXI[,-5]
  period.ends = endpoints(prices, 'months')
  hist.prices = prices[period.ends, ]
  cl=Cl(prices)
  returns = dailyReturn(cl, type='log')
  
  #returns = mROC(hist.prices)

  rr = rollapplyr(prices, width=100, FUN=mean)
  mPlot(rr,prices)
  
  clI = seq(1:len(cl))
  
  
  
  bewegungsrichtung_<-function(x )
  {   
   # browser()
  # if(len(x) > 3)      
       ri=  sign( last((diff(x,len(x)-1))))
  #  else
  #    ri=0
    
  return(ri)
  }
  bewegungsrichtung<-cmpfun(bewegungsrichtung_) 
  
  cl=cl["2006"]
  
  cl[ cl==max(cl)]
  
  rr = rollapplyr(na.omit(cl), width=10, FUN=bewegungsrichtung)
  plota(cl)
  plota.lines(cl,col="orange")
  plota.lines(cl,col=ifelse(rr>=0 ,"black","red"),lwd=2)
  
  
  
  #nur wenn die Bewegungsrichtung vom ShortMA und LongMA übereinstimmen ist dies die Trendrichtung.
#  (an sonsten flat)
#Bewegungsrichtung von c ist:  
  
  
  
  #Ecken produzieren
  md=2
  lows= Lo(prices)[HighLows(Lo(prices),maxdd=md,visual=T)$lows]
  highs= Hi(prices)[HighLows(Hi(prices),maxdd=md,visual=T)$highs]
  
  hsm=SMA(highs,n=3)
  
  mPlots(hsm,cl)
  
  sign(lows-lag(lows))
  
  
  
  s1=runLengthEnc(ifelse(sign(highs-lag(highs,1))<0,0,1))
  
  xyplot(mmerge(s1,cl,mROC(cl),ROC(cl,3)),grid=T)
  
  mPlots(cl,mRendite(na.omit(ROC(cl,10))))
  
  
  enc.startidx <- c(0, enc.endidx[1:(length(enc.endidx)-1)],length(prices))  # starting indices
  if (is.na(enc$values[1]))
    enc$values[1] = -1*enc$values[2]
  
  
  
  s2=ifelse(sign(highs-lag(highs,2))<0,0,1)
  s3=ifelse(sign(highs-lag(highs,3))<0,0,1)
  s4=ifelse(sign(lows-lag(lows,1))<0,0,1)
  s5=ifelse(sign(lows-lag(lows,2))<0,0,1)
  s6=ifelse(sign(lows-lag(lows,3))<0,0,1)
  
  compar = mmerge(highs,lows)
  chighs=compar[,1]
  clows= compar[,2]
  chighs-lag(clows,4)
  
  s7=na.omit(ifelse(sign(chighs-lag(clows,1))<0,0,1))
  s8=na.omit(ifelse(sign(chighs-lag(clows,2))<0,0,1))
  s9=na.omit(ifelse(sign(chighs-lag(clows,3))<0,0,1))
  s10=na.omit(ifelse(sign(chighs-lag(clows,4))<0 ,0,1))
  
  indi=mmerge(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10)
  indi=mmerge(s7,s8,s9,s10)
  indi=mmerge(s9)
  HLindi=indi
  HLindi[]=rowSums(indi)+1
  HLindi=na.omit(runSum(na.omit(HLindi),5))
  
  
  plot(HLindi)
  lines(HLindi,type="h",col="lightblue")
  lines(scaleTo(cl,range(HLindi)),col="blue")
  mPlot(mNorm(cl,HLindi))
  Pre(highs, lag)
  
  
  rr = rollapplyr(dl1, width=20, FUN=mean)
  rr = rollapplyr(dl1, width=20, FUN=mean)
  
  lows
  
  
  
  ?TTR
  runDif
  
  justEx = mmerge(lows,highs)
  
  mPlot(difEx)
  rr = rollapplyr(difEx, width=20, FUN=mean)
  
  mPlot(rr,scaleTo(Cl(prices),range(na.omit(rr))),ylog_=F)
  
  plot(justEx)
  rr = rollapplyr(prices, width=510, FUN=mean)
  
  
  mPlot(rr,prices)
  
  
  
  #Hurst .. nahe 0.5 zeigt Chaos .. also naher Trendbruch
  #stochRSI <- stoch( RSI(ttrc[,"Close"])
  #          WPR(HLC, n=14)
  
  hurst(Cl(prices))
  hurst(prices)
  
  HurstRoc = rollapplyr(mROC(Cl(prices)), width=50, FUN=hurst) #MMA schade  - Hurst sieht verrauscht aus.
  #rhurst <- rollapplyr(Cl(prices)["2008::2009"], width=50, FUN=hurstexp)
  
  mPlots(HurstRoc,Cl(prices))  
  
  mPlots(na.omit(HurstRoc),cl)
  
  
  hurstexp(Cl(hist.prices), d = 50, display = TRUE)
  hurstexp(mROC(Cl(hist.prices)), d = 50, display = TRUE)
  
  rhurst <- rollapplyr(Cl(prices), width=50, FUN=hurst)
  oHurst = rhurst
  mhurst = rhurst
  tail(mhurst,200)
  mPlot(mhurst)
  p =Cl(prices)
  p[] = 0.5
  
  
  mPlot(mhurst,scaleTo(Cl(prices),range(na.omit(mhurst))),p,ylog_=F)
  
####################################

# Generate data frame of data and targets


data = featureGen(cl, returns)
tail(data)
targets = coredata(returns)
targets[targets>=0] = 1
targets[targets<0] = -1
targets = as.factor(targets)

# Run the system
SVMClassifModel(data[30:nrow(data),], targets[30:length(targets)], returns, lookback = 252, ktype = "C-svc", crossvalid = 10, C = 60)
}

