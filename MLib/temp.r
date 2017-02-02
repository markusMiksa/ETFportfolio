performance.numbers.bad<-function(this.model,frame="", data=NULL,visual=T)
{
  if (is.null(data))
    return("sorry -no data given")
  model=clone(this.model)
  fromToS(model$eq)
  
  #patche die Zeiten
  frame="2006::2009"
#  model$eq=model$eq[frame];model$equity=model$equity[frame];model$ret=model$ret[frame];model$weight=model$weight[frame];
  #prices=data$prices[frame,]
  #model$dates.index=NULL
  model$dates.index=dates2index(data$prices,as.Date(index(data$prices[frame])))
#  model$weight
#mdata=clone(data)
#mdata$prices=mdata$prices[frame]
  
  model.i=bt.run  (
    data,					# enviroment with symbols time series
    trade.summary = T, 	# flag to create trade summary
    do.lag = 1, 		# lag signal
    do.CarryLastObservationForwardIfNA = T, 
    type = c('weight', 'share'),
    silent = F,
    capital = 100000,
    commission = global_commission,
    weight = model$weight[frame],
    dates = model$dates.index	
  ) 
model.i$trade.summary$trades

  #bt = bt.summary(model$weight, mROC(prices), model$type, prices, model$capital,global_commission)
#  bt$dates.index = dates.index 
  
   #model$trade.summary = bt.trade.summary(data, bt)
  
#  if(  !is.null(model$trade.summary))
#  model$trade.summary$trades=model$trade.summary$trades
  #### die sit-summary
  ds=bt.detail.summary(model.i, model.i$trade.summary) #######################
  #ds$Trade=append(ds$Trade, list(Turnover= sprintf("%.1f",100 *compute.turnover(model,data))))
  temp=list2matrix(ds)
  if (visual)    
    plot.table(temp, keep_all.same.cex = TRUE)
  purePlot(merge(mNorm(data$prices[frame,data$BENCH]),model.i$equity))
  
if (visual)
  {
    plotbt.transition.map(model.i$weight, x.highlight = T)
    norm_Win()
    temp = plotbt.monthly.table(model.i$equity)
    plotbt.holdings.time(model.i$weight)
  }   
  if (F&& !is.null(model$trade.summary)) 
  {
    
    ntrades = min(20, nrow(model$trade.summary$trades))
    temp = last(model$trade.summary$trades, ntrades)
    if( ntrades == 1 ) temp = model.i$trade.summary$trades
    print( temp )
    print( model$trade.summary$stats )
    layout(c(1,rep(2,10)))
    make.table(1,1)
    a = matrix(names(models)[1],1,1)
    cex = plot.table.helper.auto.adjust.cex(a)
    draw.cell(a[1],1,1, text.cex=cex,frame.cell=F)
    if (visual)
      plot.table( temp )
    
  }
}