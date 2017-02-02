#### vom sysinvestor,  bt.stop.test.r

#*****************************************************************
# Stops
#****************************************************************** 
# fixed stop: exit trade once price falls below % from entry price
fixed.stop <- function(weight, price, tstart, tend, pstop) {
  index = tstart : tend
  if(weight > 0)
    price[ index ] < (1 - pstop) * price[ tstart ]
  else
    price[ index ] > (1 + pstop) * price[ tstart ]
}

# trailing stop: exit trade once price falls below % from max price since start of trade
trailing.stop <- function(weight, price, tstart, tend, pstop) {
  index = tstart : tend
  if(weight > 0) {
    temp = price[ index ] < (1 - pstop) * cummax(price[ index ])
  } else {
    temp = price[ index ] > (1 + pstop) * cummin(price[ index ])
  }
  return( temp )	
}	

# trailing stop: exit trade once price either
# - falls below % from max price since start of trade OR
# - rises above % from entry price
trailing.stop.profit.target <- function(weight, price, tstart, tend, pstop, pprofit) {
  index = tstart : tend
  if(weight > 0) {
    temp = price[ index ] < (1 - pstop) * cummax(price[ index ])
    
    # profit target
    temp = temp | price[ index ] > (1 + pprofit) * price[ tstart ]
  } else {
    temp = price[ index ] > (1 + pstop) * cummin(price[ index ])
    
    # profit target
    temp = temp | price[ index ] < (1 - pprofit) * price[ tstart ]		
  }
  return( temp )	
}		

if (F)
{
#*****************************************************************
# Exit using fixed stop
#****************************************************************** 
data$weight[] = NA
data$weight[] = custom.stop.fn(coredata(buy.signal), coredata(prices), fixed.stop, 
                               pstop = 1/100)
models$ma.cross.fixed.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)

#*****************************************************************
# Exit using trailing stop
#****************************************************************** 
data$weight[] = NA
data$weight[] = custom.stop.fn(coredata(buy.signal), coredata(prices), trailing.stop, 
                               pstop = 1/100)
models$ma.cross.trailing.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)

#*****************************************************************
# Exit using trailing stop or profit target
#****************************************************************** 		
data$weight[] = NA
data$weight[] = custom.stop.fn(coredata(buy.signal), coredata(prices), trailing.stop.profit.target, 
                               pstop = 1/100, pprofit = 1.5/100)
models$ma.cross.trailing.stop.profit.target = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
}
