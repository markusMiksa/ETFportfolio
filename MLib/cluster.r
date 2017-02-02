
options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)
##############################################################################################################


sit.cluster<-function(data,frame,n.given=NULL,visual=T)
{
  #*****************************************************************
  # Create Clusters
  #hier könnt man ja auch mal im return/riks-Raum clustern..
  #****************************************************************** 
  # compute returns
  
  if (visual)
    mP("sit.cluster %s",frame)
  ret = data$prices / mlag(data$prices) - 1
  ret = na.omit(ret)		
  
  # setup period and method to compute correlations
  dates = frame#'2013::2014'
  method = 'pearson'	# kendall, spearman
  
  correlation = cor(ret[dates], method = method)    
  dissimilarity = 1 - (correlation)
  distance = na.omit(as.dist(dissimilarity))
  
  # get first 2 pricipal componenets
  xy = cmdscale(distance)
  
  #*****************************************************************
  # Determine number of clusters
  #****************************************************************** 
  n = ncol(data$prices)
  n1 = ceiling(n*2/3)
  
  # percentage of variance explained by clusters
  p.exp = rep(0,n1)
  
  # minimum correlation among all components in each cluster	
  min.cor = matrix(1,n1,n1)  
  
  for (i in 2:n1) {
    fit = kmeans(xy, centers=i, iter.max=100, nstart=100)
    p.exp[i] = 1- fit$tot.withinss / fit$totss
    
    for (j in 1:i) {
      index = fit$cluster == j
      min.cor[i,j] = min(correlation[index,index])
    }
  }
  
  # minimum number of clusters that explain at least 90% of variance
  
  nc90=min(which(p.exp > 0.9))
  
  # minimum number of clusters such that correlation among all components in each cluster is at least 40%
  # will not always work
  #min(which(apply(min.cor[-1,],1,min,na.rm=T) > 0.4)) + 1
  
  # number of clusters based on elbow method
  nc=find.maximum.distance.point(p.exp[-1]) + 1
  
  
  #*****************************************************************
  # Create Plot
  #****************************************************************** 	
  load.packages('cluster')
  
  if (len(n.given))
  {
    fit = kmeans(xy, n.given, iter.max=100, nstart=100)
    if (visual)
      clusplot(xy, fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F, 
               main = sprintf('Major Market Clusters over %s given %d  Clusters',dates,n.given))#dev.off()  
  }
  else
  {
    #png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
    fit90 = kmeans(xy, nc90, iter.max=100, nstart=100)
    if (visual)
      clusplot(xy, fit90$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F, 
               main = sprintf('Major Market Clusters90 over %s %d  Clusters',dates,nc90))
    #dev.off()	
    
    #png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
    fit = kmeans(xy, nc, iter.max=100, nstart=100)
    if (visual)
      clusplot(xy, fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F, 
               main = sprintf('Major Market Clusters over %s %d  Clusters',dates,nc))#dev.off()	
    
  }
  
  # http://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Clustering/Expectation_Maximization_(EM)
  if (F)
  {
    load.packages('mclust')
    fitBIC = mclustBIC(xy)
    plot(fitBIC, legendArgs = list(x = "topleft"))
    
    fit <- summary(fitBIC, data = xy)
    mclust2Dplot(data = xy, what = "density", identify = TRUE, parameters = fit$parameters, z = fit$z)	
  }
  list(fit90=fit,nc90=nc90, fit=fit,nc=nc )
}
#.......................................... zeig mal jährlich wie's sich clustert...


check.cluster.history<-function()
{
  years=as.character(as.Date(Dates.year.ends(index(data$prices["2002::",1]))))
  years=xts(years,as.Date(years))
  rollapplyr( merge(years,lag(years))[-1],width=1, by.column=F,FUN=
                function(x) 
                {
                  frame=sprintf("%s::%s",x[,2],x[,1])
                  nC= sit.cluster(data,frame,visual=F)
                  nC$nc90
                  nC$nc
                  
                  sit.cluster(data,frame,n.given=nC$nc+1)
                 # browser()
                 nC$nc
                }
  )
}
if (F)
  check.cluster.history()

