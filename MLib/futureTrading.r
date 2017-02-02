
if (F)
{
  #  bt.mebanefaber.modified.mn.test()
   
  ###############################################################################
  # One of the biggest challenges for a market neutral strategy is your shorts ripping when a market 
  # bottoms and all of the (expensive/low momentum) stocks rip straight up.  That is why most factor 
  # based long short portfolios rarely survive – they are long and short the wrong things at market 
  # bottoms.  
  # 
  # Below is french fama momentum data that shows high and low momentum stocks back to the 1920s.  
  # Hi mo beats both the market and low mo.  One would think a market neutral portfolio would be 
  # really low risk, but in reality it has massive drawdowns in the 1920s and 2009.  
  # 
  # One way to rectify this situation is to simply short less the more the market goes down.  
  # Kind of makes sense as you think about it and is probably just prudent risk management.  
  # 
  # So the modified strategy below starts 100% market neutral, and depending on the drawdown bucket 
  # will reduce the shorts all the way to zero once the market has declined by 50% 
  # (in 20% steps for every 10% decline in stocks).
  #
  # http://www.mebanefaber.com/2013/10/30/the-problem-with-market-neutral-and-an-answer/
  ###############################################################################
  
  #*****************************************************************
  # Modified MN
  # The modified strategy below starts 100% market neutral, and depending on the drawdown bucket 
  # will reduce the shorts all the way to zero once the market has declined by 50%
  # (in 20% steps for every 10% decline in stocks)
  #*****************************************************************  
  
  compute.drawUp <- function(x,w=500) 
  {   
    res=x/runmin(x,k=w,align="right") - 1
    res=xts2xts(x,res)
  }
  
  compute.drawDown <- function(x,w=500) 
  {   
    res=x/runmax(c(1,x),k=w,align="right")[-1] - 1
    res=xts2xts(x,res)
  }
  
  
  models = list()
  mdata=clone(data)
  
  p=data$prices[,"SX5R"]
  faber = p-SMA(p,300)
  HI=p; HI[faber < 0] = NA;HI=m.ifna.prev(HI)
  LO=p; LO[faber > 0]=  NA;LO = m.ifna.prev(LO)
  HI=LO=p
  
  mdata$symbolnames = spl("HI,LO")
  mdata$prices=merge(HI,LO);colnames(mdata$prices)=spl("HI,LO")
  
  
  mdata$weight = mdata$prices;  mdata$weight[] = NA
  mdata$weight$HI[]=1; 
  mdata$weight$LO[]=0; 
  
  models$SX5R = bt.run.share(mdata, clean.signal=T,commission=global_commission)
  strategy.Performance.snapshoot(models,state="T",data=mdata,commission=global_commission)    
  
  
  mdata$weight = mdata$prices;  mdata$weight[] = NA
  mdata$weight$HI[]=0; mdata$weight$HI[faber > 0] = 1
  mdata$weight$LO[]=0; mdata$weight$LO[faber <0] = -1
  
  models$faber = bt.run.share(mdata, clean.signal=T,commission=global_commission)
  strategy.Performance.snapshoot(models,state="T",data=mdata,commission=global_commission)    
  
  
  #mdata$weight$HI[]=0; mdata$weight$HI[faber > 0,] = 1
  #mdata$weight$LO[] =0; mdata$weight$LO[faber <0,] = -1
  #models$MKT.NEUTRAL = bt.run.share(mdata, clean.signal=F)
  #strategy.Performance.snapshoot(models,state="T",data=mdata,commission=global_commission)    
  
  
  
  #Long-Future-Trading
  market.up = scaleTo( compute.drawUp(p,w=20),c(0,100))
  market.up=EMA(market.up,2)
  market.drawup.10.step = 10 * floor(market.up / 10)
  long.allocation = 100 - market.drawup.10.step *2
  long.allocation[ long.allocation < 0 ] = 0
  
  
  mdata$weight$HI[] = long.allocation / 100

  models$HI = bt.run.share(mdata, clean.signal=T,trade.summary=T,commission=global_commission)
  strategy.Performance.snapshoot(models,state="T",data=mdata,commission=global_commission)    
  
  
  market.drawdown = -100 * compute.drawDown(p,w=100)#compute.drawdown(p)
  market.drawdown = scaleTo( compute.drawUp(p,w=20),c(0,100))
  market.drawdown=EMA(market.drawdown,2)
  
  
  market.drawdown.10.step = 10 * floor(market.drawdown / 10)
  
  short.allocation = market.drawdown.10.step * 4
  short.allocation[ short.allocation < 0 ] = 0
  
  colnames(  mdata$prices)
  colnames(mdata$weight)   
  dim(  mdata$prices)
  dim(mdata$weight)   
  
  
  #mdata$weight$HI[]=0 
  #mdata$weight$HI[] =0
  
  mdata$weight$LO[faber <0] = -1 * short.allocation / 100
  mdata$weight$LO[mdata$weight$LO <  -1] =-1
  models$Modified.HILO = bt.run.share(mdata, clean.signal=F, trade.summary=T,commission=global_commission)
  
  strategy.Performance.snapshoot(models,state="T",data=mdata,commission=global_commission)    
  
  
  
  
  
  
  
}
