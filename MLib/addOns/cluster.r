
show_cluster<-function(prices)
{
  #*****************************************************************
  # Create Clusters
  #****************************************************************** 
  # compute returns
  ret = prices / mlag(prices) - 1
  ret = na.omit(ret)		
  
  # setup period and method to compute correlations
  dates = '2012::2012'
  method = 'pearson'	# kendall, spearman
  
  correlation = cor(ret[dates], method = method)    
  dissimilarity = 1 - (correlation)
  distance = as.dist(dissimilarity)
  
  # find 4 clusters      
  xy = cmdscale(distance)
  fit = kmeans(xy, 4, iter.max=100, nstart=100)
  
  fit$cluster
  
  #*****************************************************************
  # Create Plot
  #****************************************************************** 	
  load.packages('cluster')
#  png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
  clusplot(xy, fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F, 
           main = paste('Major Market Clusters over', dates), sub='')
 # dev.off()	
  
  
#  png(filename = 'plot2.png', width = 800, height = 800, units = 'px', pointsize = 12, bg = 'white')			
  #layout(matrix(1:8,nc=2))
  par( mar = c(2, 2, 2, 2) )
  clr()
  for(icluster in 2:12)
    clusplot(xy, kmeans(xy, icluster, iter.max=100, nstart=100)$cluster, color=TRUE, shade=F,   
             labels=3, lines=0, plotchar=F, main=icluster, sub='')
#  dev.off()	
  
}

if (F)
  show_cluster(prices)