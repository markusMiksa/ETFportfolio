# Copyright: Marc Wildi
# 15.01.2012
# http://blog.zhaw.ch/idp/sefblog
library(signalextraction)
#library(imdfa-master)
#https://github.com/swayland/imdfa
#http://blog.zhaw.ch/idp/sefblog/index.php?/archives/278-Regularization-Enhancing-the-Decay-Term-of-the-Troka.html 
#_____________________________________________________________


#gdp_q<-gdp
#x<-xh

# New 2012-code: computes spectral estimates based on DFT
spec_comp<-function(insamp,x,d,GDP_T,gdp_q,publication_lag)
{
  #insamp<-260-anf+1
  #insamp<-len
  #  K<-(insamp-1)/2
  weight_func<-NULL#matrix(nrow=K+1,ncol=dim(x)[2])
  
  if (d==1)
  {
    K<-length(periodogram_bp(diff(x[1:insamp,1]), 1, insamp-1)$fourtrans)-1
    weight_func<-1:(K+1)
    if (!GDP_T)
    {
      weight_func<-periodogram_bp(diff(x[1:insamp,1]), 1, insamp-1)$fourtrans
    } else
    {
      weight_func<-periodogram_bp(diff(gdp_q[1:as.integer(insamp/3)]), 1, as.integer(insamp/3)-1)$fourtrans    
      weight_func<-c(weight_func,rep(0,K+1-length(weight_func)))
      weight_func<-weight_func*exp(-1.i*(0:K)*pi*publication_lag/K)
    }
    # explaining variables                            ts.plot(x[1:insamp,1])
    if (length(weight_func)>1)
    {
      for (j in 2:length(x[1,]))  #j<-2
      {
        # Since the data is integrated one uses the pseudo-periodogram: diff(data) and d=1
        weight_func<-cbind(weight_func,periodogram_bp(diff(x[1:insamp,j]), 1, insamp-1)$fourtrans)
      }
    }
  } else
  {
    weight_func<-periodogram_bp((x[1:insamp,1]), 0, insamp)$fourtrans
    K<-length(periodogram_bp(x[1:insamp,1], 0, insamp)$fourtrans)-1
    if (!GDP_T)
    {
      weight_func<-periodogram_bp(x[1:insamp,1], 0, insamp)$fourtrans
    } else
    {
      weight_func<-periodogram_bp(gdp_q[1:as.integer(insamp/3)], 0, as.integer(insamp/3))$fourtrans    
      weight_func<-c(weight_func,rep(0,K+1-length(weight_func)))
      weight_func<-weight_func*exp(-1.i*(0:K)*pi*publication_lag/K)      
    }
    
    
    # explaining variables                            ts.plot(x[1:insamp,1])
    if (length(weight_func)>1)
    {
      for (j in 2:length(x[1,]))  #j<-2
      {
        # Since the data is integrated one uses the pseudo-periodogram: diff(data) and d=1
        weight_func<-cbind(weight_func,periodogram_bp((x[1:insamp,j]), 0, insamp)$fourtrans)
      }
    }
    
  }
  dimnames(weight_func)[[2]]<-dimnames(x)[[2]]
  #weight_func[,1]<-periodogram_bp(diff(gdp[1:insamp]), 1, insamp-1)$fourtrans
  # if i1<-T then weight_constraint imposes corresponding values of amplitude functions in frequency zero
  
  ts.plot(abs(weight_func)[,1])
  return(list(weight_func=weight_func))
}






# DFT (old code but still in use for new 2012-version...)
periodogram_bp <- function(x, dd, n.pg)
{
  ## Preparations
  n.fit  <- length(x)
  xx     <- x[((n.fit-n.pg+1):n.fit)]
  npg2   <- (n.pg/2)
  perall <- 0*0:npg2
  fourtrans<-perall
  
  ## Case without a seasonal component
  if (dd < 3)
  {
    for (j in (1:npg2)) #j<-1
    {
      fourtrans[j+1] <- xx%*%exp((1:(2*npg2))*1.i*j*pi/npg2)
      term2 <- (1-exp(j*1.i*pi/npg2))^(dd)
      fourtrans[j+1] <- fourtrans[j+1]/(1-min(dd,1)+min(1,dd)*term2)
      
      perall[j+1] <- abs(fourtrans[j+1])^2
    }
  }
  
  ## Case with a seasonal component, special treatment for Pi/6
  if (dd >= 3)
  {
    for (j in (1:npg2)[(-npg2/6)*(1:6)])
    {
      fourtrans[j+1] <- xx%*%exp((1:(2*npg2))*1.i*j*pi/npg2)
      term2 <- abs(1-exp(j*1.i*pi/npg2))^2
      term3 <- abs(1-exp(12*j*1.i*pi/npg2))^2
      perall[j+1] <- abs(fourtrans[j+1])/(term2*term3)
    }
    perall[(npg2/6)*(1:6)+1] <- max(perall)*100000
  }
  
  ## Output
  return(list(perall=perall,fourtrans=fourtrans))
}







# New 2012 code: is needed for implementing spectral matrix in new parametrization including regularization
spec_mat_comp<-function(weight_func,L,Lag)
{
  K<-length(weight_func[,1])-1
  weight_h<-weight_func
  # Frequency zero receives half weight
  weight_h[1,]<-weight_h[1,]*0.5
  # Extract DFT target variable (first column)
  weight_target<-weight_h[,1]
  # Rotate all DFT's such that weight_target is real (rotation does not alter mean-square error)
  weight_h<-weight_h*exp(-1.i*Arg(weight_target))
  weight_target<-weight_target*exp(-1.i*Arg(weight_target))
  # DFT's explaining variables (target variable can be an explaining variable too)
  weight_h_exp<-as.matrix(weight_h[,2:(dim(weight_h)[2])])
  spec_mat<-as.vector(t(as.matrix(weight_h_exp[1,])%*%t(as.matrix(rep(1,L)))))
  for (j in 1:(K))
  {
    omegak<-j*pi/K
    exp_vec<-exp(1.i*omegak*((0:(L-1))-Lag))
    spec_mat<-cbind(spec_mat,as.vector(t(as.matrix(weight_h_exp[j+1,])%*%t(as.matrix(exp_vec)))))
  }
  dim(spec_mat)
  return(list(spec_mat=spec_mat))
}








# New 2012-code: Computes regularization matrices and expresses parameters in central-deviance form

mat_func<-function(i1,i2,L,weight_h_exp,lambda_decay,lambda_cross,lambda_smooth,Lag)
{
  if (Lag>(L-1)/2)
  {
    print("Lag larger than L/2!!!!! Will be trimmed automtically to L/2 (symmetric filter)")
    Lag<-as.integer(L/2)
  }
  lambda_smooth<-abs(lambda_smooth)
  lambda_cross<-abs(lambda_cross)
  lambda_decay<-abs(lambda_decay) 
  # The smoothness and decay regularization are conveniently (rightly) implemented on original parameters
  # The Q_smooth and Q_decay matrices address regularizations for original unconstrained parameters (Therefore dimension L^2)
  # At the end, the matrix des_mat is used to map these regularizations to central-deviance parameters 
  # accounting for first order constraints!!!!!!!!!!!!!!!!!!!
  Q_smooth<-matrix(data=rep(0,((L)*length(weight_h_exp[1,]))^2),nrow=(L)*length(weight_h_exp[1,]),ncol=(L)*length(weight_h_exp[1,]))
  Q_decay<-matrix(data=rep(0,((L)*length(weight_h_exp[1,]))^2),nrow=(L)*length(weight_h_exp[1,]),ncol=(L)*length(weight_h_exp[1,]))
  # Cross-sectional regularization if dimension>1
  if ((length(weight_h_exp[1,])>1))
  {
    # The cross-sectional regularization is conveniently implemented on central-deviance parameters. The regularization is expressed on the
    # unconstrained central-deviance parameters (dimension L), then mapped to the original (unconstrained) parameters (dimension L) with Q_centraldev_original
    # and then maped back to central-deviance with constraint (dim L-1) with des_mat (mathematically unnecessarily complicate but more convenient to implement in code).
    Q_cross<-matrix(data=rep(0,((L)*length(weight_h_exp[1,]))^2),nrow=(L)*length(weight_h_exp[1,]),ncol=(L)*length(weight_h_exp[1,]))
    Q_centraldev_original<-matrix(data=rep(0,((L)*length(weight_h_exp[1,]))^2),nrow=(L)*length(weight_h_exp[1,]),ncol=(L)*length(weight_h_exp[1,]))
  }
  for (i in 1:L)
  {
    # For symmetric filters or any historical filter with Lag>0 the decay must be symmetric about b_max(0,Lag) zunehmen
    Q_decay[i,i]<-lambda_decay*(1+lambda_decay)^(2*abs(i-1-max(0,Lag)))        
    if(i==1)
    {
      Q_smooth[i,i:(i+2)]<-lambda_smooth*c(1,-2,1)        
    } else
    {
      if(i==2)
      {
        Q_smooth[i,(i-1):(i+2)]<-lambda_smooth*c(-2,5,-4,1)        
      } else
      {
        if(i==L)
        {
          Q_smooth[i,(i-2):i]<-lambda_smooth*c(1,-2,1)        
        } else
        {
          if(i==L-1)
          {
            Q_smooth[i,(i-2):(i+1)]<-lambda_smooth*c(1,-4,5,-2)        
          } else
          {
            Q_smooth[i,(i-2):(i+2)]<-lambda_smooth*c(1,-4,6,-4,1)                      
          }
        }
      }
    }
  }
  
  if (length(weight_h_exp[1,])>1)
  {
    for (j in 1:max(1,(length(weight_h_exp[1,])-1)))   #j<-1
    {
      Q_smooth[j*L+1:L,j*L+1:L]<-Q_smooth[1:L,1:L]
      Q_decay[j*L+1:L,j*L+1:L]<-Q_decay[1:L,1:L]        
    }
    diag(Q_centraldev_original[1:L,1:L])<-rep(1,L)
    diag(Q_centraldev_original[1:L,L+1:L])<-rep(-1,L)
    for (i in 2:length(weight_h_exp[1,]))
    {
      diag(Q_centraldev_original[(i-1)*L+1:L,1:L])<-rep(1,L)
      diag(Q_centraldev_original[(i-1)*L+1:L,(i-1)*L+1:L])<-rep(1,L)
      diag(Q_centraldev_original[1:L,(i-1)*L+1:L])<-rep(-1,L)
    }
    
    Q_centraldev_original<-solve(Q_centraldev_original)
    diag(Q_cross[L+1:((length(weight_h_exp[1,])-1)*L),L+1:((length(weight_h_exp[1,])-1)*L)])<-
      lambda_cross*rep(1,((length(weight_h_exp[1,])-1)*L))
  }
  
  # Here we implement the matrix which links central-deviance parameters and original parameters
  # The i2 restriction is not implemented (correctly) yet!!!!!
  
  if (i2)
  {
    if (i1)
    {
      # First and second order restrictions
      des_mat<-matrix(data=rep(0,(L-2)*L*(length(weight_h_exp[1,]))^2),nrow=(L-2)*length(weight_h_exp[1,]),ncol=(L)*length(weight_h_exp[1,]))
      for (i in 1:(L-2))
      {
        des_mat[i,i+(0:(length(weight_h_exp[1,])-1))*L]<-1
        des_mat[i,L-1+(0:(length(weight_h_exp[1,])-1))*L]<--(L-i)
        des_mat[i,L+(0:(length(weight_h_exp[1,])-1))*L]<-(L-i-1)
        
      }
      for (j in 1:max(1,(length(weight_h_exp[1,])-1)))
      {
        for (i in 1:(L-2))                                                          #reg_mat[600,600
        {
          des_mat[i+(j)*(L-2),i]<--1
          des_mat[i+(j)*(L-2),i+j*L]<-1
          des_mat[i+(j)*(L-2),L-1]<-(L-i)
          des_mat[i+(j)*(L-2),L]<--(L-i-1)
          des_mat[i+(j)*(L-2),L-1+j*L]<--(L-i)
          des_mat[i+(j)*(L-2),L+j*L]<-(L-i-1)
          
        }
      }
    } else
    {
      des_mat<-matrix(data=rep(0,(L-1)*L*(length(weight_h_exp[1,]))^2),nrow=(L-1)*length(weight_h_exp[1,]),ncol=(L)*length(weight_h_exp[1,]))
      for (i in 1:(L-1))
      {
        des_mat[i,i+(0:(length(weight_h_exp[1,])-1))*L]<-1
        des_mat[i,L+(0:(length(weight_h_exp[1,])-1))*L]<--(i-1)/(L-1)
        
      }
      for (j in 1:max(1,(length(weight_h_exp[1,])-1)))
      {
        for (i in 1:(L-1))
        {
          des_mat[i+(j)*(L-1),i]<--1
          des_mat[i+(j)*(L-1),i+j*L]<-1
          des_mat[i+(j)*(L-1),L]<-(i-1)/(L-1)
          des_mat[i+(j)*(L-1),L+j*L]<--(i-1)/(L-1)
        }
      }
    }
  } else
  {
    if (i1)                            #lambda_cross=0
    {
      des_mat<-matrix(data=rep(0,(L-1)*L*(length(weight_h_exp[1,]))^2),nrow=(L-1)*length(weight_h_exp[1,]),ncol=(L)*length(weight_h_exp[1,]))
      # The cross-sectional regularization can be directly implemented into reg_mat (it adresses the central deviance parameters)
      for (i in 1:(L-1))  #i<-1
      {
        # The i1-constraint is imposed on b_max(0,Lag) (instead of b_L as in original procedure) in order to avoid a conflict with the exponential decay requirement
        if (Lag<1)
        {
          des_mat[i,i+1+(0:(length(weight_h_exp[1,])-1))*L]<-1
          des_mat[i,1+(0:(length(weight_h_exp[1,])-1))*L]<--1
        } else
        {
          # Lag cannot be larger than (L-1)/2 (symmetric filter)
          des_mat[i,ifelse(i<Lag+1,i,i+1)+(0:(length(weight_h_exp[1,])-1))*L]<-1
          des_mat[i,Lag+1+(0:(length(weight_h_exp[1,])-1))*L]<--1
        }
      }
      if (max(1,(length(weight_h_exp[1,])-1))>1)
      {
        for (j in 1:max(1,(length(weight_h_exp[1,])-1)))   #j<-1
        {
          for (i in 1:(L-1))
          {
            # The i1-constraint is imposed on b_max(0,Lag) (instead of b_L as in original procedure) in order to avoid a conflict with the exponential decay requirement
            if (Lag<1)
            {
              des_mat[i+j*(L-1),i+1]<--1
              des_mat[i+j*(L-1),1]<-1
              des_mat[i+j*(L-1),i+1+j*L]<-1
              des_mat[i+j*(L-1),1+j*L]<--1
            } else
            {
              # Lag cannot be larger than (L-1)/2 (symmetric filter)
              des_mat[i+j*(L-1),ifelse(i<Lag+1,i,i+1)]<--1
              des_mat[i+j*(L-1),Lag+1]<-1
              des_mat[i+j*(L-1),ifelse(i<Lag+1,i,i+1)+j*L]<-1
              des_mat[i+j*(L-1),Lag+1+j*L]<--1
            }
            # The cross sectional regularization is implemented directly into reg_mat (it addresses central deviance parameters!)          
            #            reg_mat[i+(j)*(L-1),i+(j)*(L-1)]<-lambda_cross
          }
        }             # det(reg_mat)         lambda_cross<-0.3
      }
    } else
    {
      des_mat<-matrix(data=rep(0,(L)*L*(length(weight_h_exp[1,]))^2),nrow=(L)*length(weight_h_exp[1,]),ncol=(L)*length(weight_h_exp[1,]))
      for (i in 1:(L))
      {
        des_mat[i,i+(0:(length(weight_h_exp[1,])-1))*L]<-1
      }       #dim(reg_mat[359,])
      if (max(1,(length(weight_h_exp[1,])-1))>1)
      {
        
        for (j in 1:max(1,(length(weight_h_exp[1,])-1)))
        {
          for (i in 1:(L))
          {
            des_mat[i+(j)*(L),i]<--1
            des_mat[i+(j)*(L),i+j*L]<-1
            # bei symmetrischen Filtern bzw. wenn Lag>0 muss der decay symmetrisch um b_0 zunehmen          
            #            reg_mat[i+(j)*(L),i+(j)*(L)]<-lambda_cross
          }
        }  
      }
    }  
  }
  reg_mat<-0*des_mat
  return(list(des_mat=des_mat))     
}
















# New 2012 I-MDFA code: might crash if dimension is 1 (univariate setting): I did not test this!!!
# Generalizes old code (below) when regularization parameters are set to zero
# This new function can deal with very richly parametrized designs (high-dimensional multivariate with long lag structures)
# Regularization affect/control smoothness, rate of decay and cross-sectional similarity of filter parameters/weights

mdfa_analytic_new<-function(K,L,lambda,weight_func,Lag,Gamma,expweight,cutoff,i1,i2,weight_constraint,lambda_cross,lambda_decay,lambda_smooth)
{
  # In order to enhance numerical speed this call could be done outside (as long as L and Lag are fixed)
  spec_mat<-spec_mat_comp(weight_func,L,Lag)$spec_mat
  # weighting of amplitude function in stopband
  omega_Gamma<-as.integer(cutoff*K/pi)
  if ((K-omega_Gamma+1)>0)
  {
    expweight_vec<-c(rep(1,omega_Gamma),(1:(K-omega_Gamma+1))^(expweight/2))
    weight_h<-weight_func*expweight_vec
  } else
  {
    expweight_vec<-rep(1,K+1)
    weight_h<-weight_func* expweight_vec
  }
  #ts.plot(abs(weight_h))       ts.plot(Gamma)
  # Frequency zero receives half weight
  weight_h[1,]<-weight_h[1,]*0.5
  # DFT target variable
  weight_target<-weight_h[,1]
  # Rotate all DFT's such that weight_target is real (rotation does not alter mean-square error)
  weight_h<-weight_h*exp(-1.i*Arg(weight_target))
  weight_target<-Re(weight_target*exp(-1.i*Arg(weight_target)))
  # DFT's explaining variables: target variable can be an explaining variable too
  weight_h_exp<-as.matrix(weight_h[,2:(dim(weight_h)[2])])
  
  # The spectral matrix is inflated in stopband: effect of expweight
  spec_mat<-t(t(spec_mat)*expweight_vec) #dim(spec_mat)
  
  #lambda_decay<-1         lambda_smooth<-6      lambda_cross<-100
  # Define Designmatrix
  if (i2)
  {
    if (i1)
    {
      # First and second order restrictions
      # Compute design matrix and regularization matrix
      
      mat_obj<-mat_func(i1,i2,L,weight_h_exp,lambda_decay,lambda_cross,lambda_smooth,Lag)
      
      des_mat<-mat_obj$des_mat
      reg_mat<-mat_obj$reg_mat           #min(reg_mat)
      
      mat_x<-des_mat%*%spec_mat
      X_new<-t(Re(mat_x))+sqrt(1+Gamma*lambda)*1.i*t(Im(mat_x))
      # xtx can be written either in Conj(X_new)%*%X_new or as below:
      xtx<-t(Re(X_new))%*%Re(X_new)+t(Im(X_new))%*%Im(X_new)
      # The filter restrictions (i1<-T) appear as constants on the right hand-side of the equation: 
      #   they correspond to lag orders L-2 and L-1 # (b(L-1) and b(L-2) are constrained)
      #   They `multiply' the data-matrix X': (y-X*beta-constraints)*X'
      xtxy<-apply(t(Re(X_new))%*%((L-1)*Re(exp(1.i*(L-2-Lag)*pi*(0:(K))/(K))*t(t(weight_h_exp)*weight_constraint))-
                                    (L-2)*Re(exp(1.i*(L-1-Lag)*pi*(0:(K))/(K))*t(t(weight_h_exp)*weight_constraint)))+
                    t(Im(X_new))%*%(sqrt(1+Gamma*lambda)*((L-1)*Im(exp(1.i*(L-2-Lag)*pi*(0:(K))/(K))*t(t(weight_h_exp)*weight_constraint))-
                                                            (L-2)*Im(exp(1.i*(L-1-Lag)*pi*(0:(K))/(K))*t(t(weight_h_exp)*weight_constraint)))),1,sum)
      X_inv<-solve(xtx)      
      bh<-as.vector(X_inv%*%(((t(Re(X_new)*weight_target))%*%Gamma)-xtxy))
      # the last two filter weights are functions of the previous ones through the first and second order restrictions
      b<-matrix(nrow=L,ncol=length(weight_h_exp[1,]))
      bhh<-t(des_mat)%*%bh
      for (k in 1:L) #k<-1
      {      #dim(t(des_mat))
        b[k,]<-bhh[(k)+(0:(length(weight_h_exp[1,])-1))*L]
      }   #b_new<-b
    } else
    {
      # Only second order restriction
      # Compute design matrix and regularization matrix      
      mat_obj<-mat_func(i1,i2,L,weight_h_exp,lambda_decay,lambda_cross,lambda_smooth,Lag)
      
      des_mat<-mat_obj$des_mat
      reg_mat<-mat_obj$reg_mat
      
      mat_x<-des_mat%*%spec_mat
      X_new<-t(Re(mat_x))+sqrt(1+Gamma*lambda)*1.i*t(Im(mat_x))
      # xtx can be written either in Conj(X_new)%*%X_new or as below:
      xtx<-t(Re(X_new))%*%Re(X_new)+t(Im(X_new))%*%Im(X_new)
      X_inv<-solve(xtx)      
      bh<-as.vector(X_inv%*%((t(Re(X_new)*weight_target))%*%Gamma))
      # the last filter coefficient is a function of the previous ones through the second order restriction
      b<-matrix(nrow=L,ncol=length(weight_h_exp[1,]))
      bhh<-t(des_mat)%*%bh
      for (k in 1:L) #k<-1
      {      #dim(t(des_mat))
        b[k,]<-bhh[(k)+(0:(length(weight_h_exp[1,])-1))*L]
      }
    }
  } else
  {
    if (i1)
    {
      # Only first order restriction
      # Compute design matrix and regularization matrix
      mat_obj<-mat_func(i1,i2,L,weight_h_exp,lambda_decay,lambda_cross,lambda_smooth,Lag)           # lambda_smooth<-0.4
      
      des_mat<-mat_obj$des_mat
      reg_mat<-mat_obj$reg_mat
      mat_x<-des_mat%*%spec_mat
      X_new<-t(Re(mat_x))+sqrt(1+Gamma*lambda)*1.i*t(Im(mat_x))
      # xtx can be written either in Conj(X_new)%*%X_new or as below:
      xtx<-t(Re(X_new))%*%Re(X_new)+t(Im(X_new))%*%Im(X_new)
      xtxy<-apply(t(Re(X_new))%*%Re(exp(1.i*(L-1-Lag)*pi*(0:(K))/(K))*t(t(weight_h_exp)*weight_constraint))+
                    t(Im(X_new))%*%(sqrt(1+Gamma*lambda)*Im(exp(1.i*(L-1-Lag)*pi*(0:(K))/(K))*t(t(weight_h_exp)*weight_constraint))),1,sum)
      # MA-Filterweights                            det(xtx+mean(diag(xtx))*reg_mat)
      X_inv<-solve(xtx)      
      bh<-as.vector(X_inv%*%(((t(Re(X_new)*weight_target))%*%Gamma)-xtxy))
      # the last weight is a function of the previous ones through the first order restriction
      b<-matrix(nrow=L,ncol=length(weight_h_exp[1,]))
      bhh<-t(des_mat)%*%bh
      for (k in 1:L) #k<-1
      {      #dim(t(des_mat))
        b[k,]<-bhh[(k)+(0:(length(weight_h_exp[1,])-1))*L]
      }        #b_new-b        apply(b,2,mean)
    } else
    {
      # No restrictions
      # Compute design matrix and regularization matrix        dim(t(Re(X_new)*Re(weight_target)))   
      
      mat_obj<-mat_func(i1,i2,L,weight_h_exp,lambda_decay,lambda_cross,lambda_smooth,Lag)
      
      des_mat<-mat_obj$des_mat
      reg_mat<-mat_obj$reg_mat
      mat_x<-des_mat%*%spec_mat
      X_new<-t(Re(mat_x))+sqrt(1+Gamma*lambda)*1.i*t(Im(mat_x))
      # xtx can be written either in Conj(X_new)%*%X_new or as below:
      xtx<-t(Re(X_new))%*%Re(X_new)+t(Im(X_new))%*%Im(X_new)
      X_inv<-solve(xtx)      
      bh<-as.vector(X_inv%*%((t(Re(X_new)*weight_target))%*%Gamma))
      # the last weight is a function of the previous ones through the first order restriction
      b<-matrix(nrow=L,ncol=length(weight_h_exp[1,]))
      bhh<-t(des_mat)%*%bh
      for (k in 1:L) #k<-1
      {      #dim(X_inv)
        b[k,]<-bhh[(k)+(0:(length(weight_h_exp[1,])-1))*L]
      }
    }
  }                              
  
  # The target Y in the frequency-domain is the real vector weight_target*Gamma (both vectors are real)  
  # The Regression estimate (the smoother) of Y is the following expression:
  trt<-((X_new)%*%(X_inv%*%t(Re(X_new))))%*%(weight_target*Gamma)
  # This expression is identical to trt computed below if lambda=0 (otherwise trth is identical to Re(trt)+1.i*sqrt(1+lambda*Gamma)*Im(trt))
  # The projection matrix is therefore:
  Proj_mat<-((X_new)%*%(X_inv%*%t(Re(X_new))))              #dim(Proj_mat)
  # The residual projection matrix is
  res_mat<-diag(rep(1,dim(Proj_mat)[1]))-Proj_mat
  # DFA criterion: first possibility (all three variants are identical)
  sum(abs(res_mat%*%(weight_target*Gamma))^2)
  #  ts.plot(abs(res_mat%*%(weight_target*Gamma))^2)
  #  acf(abs(res_mat%*%(weight_target*Gamma)))
  # Residuals
  resi<-res_mat%*%(weight_target*Gamma)
  # DFA criterion: second possibility
  t(Conj(resi))%*%resi
  t((weight_target*Gamma))%*%(t(Conj(res_mat))%*%(res_mat))%*%(weight_target*Gamma)
  
  #  sum(diag(res_mat))
  #  sum(diag(t(res_mat%*%Conj(res_mat))))
  
  # The interesting `effective degrees of freedom' used here emphasizes an unbiased estimate of the mean-square residual
  #    See  http://en.wikipedia.org/wiki/Degrees_of_freedom_(statistics) (effective degrees of freedom: the expression tr((I-H)(I-H)')
  #    Note that res_mat=I-H see above
  #    Then (Y-Y^)(Y-Y^)/tr((I-H)(I-H)') is an unbiased estimate of the mean-square residual error (in our case Y=weight_target*Gamma. see above)
  #    This correcting `effective degrees of freedom' can then be used to implement a generalized AIC, see below
  #    Neither of the other proposed definitions of `effective degrees of freedom' on Wiki-site tackle this problem (in particular the trace of the smoothing operator is not what we want!!!!)
  degrees_freedom<-2*Re(sum(diag(t(Conj(res_mat))%*%(res_mat))))-1
  #  degrees_freedom<-K-Re(sum(diag(Proj_mat)))
  # DFA Criterion: third possibility (here an additional normalization by 1/(2*(K+1)^2))
  rever<-sum(abs(Gamma*weight_target-trt)^2)/(2*(K+1)^2)
  
  
  #ts.plot(b)
  # Transferfunction
  trffkt<-matrix(nrow=K+1,ncol=length(weight_h_exp[1,]))
  trffkth<-trffkt
  trffkt[1,]<-apply(b,2,sum)
  trffkth[1,]<-trffkt[1,]
  #  b<-scale(b,center=T,scale=F)
  
  for (j in 1:length(weight_h_exp[1,]))
  {
    for (k in 0:(K))#k<-1
    {
      trffkt[k+1,j]<-(b[,j]%*%exp(1.i*k*(0:(L-1))*pi/(K)))
    }
  }
  #  trt<-apply(((trffkt)*exp(1.i*(0-Lag)*pi*(0:(K))/K))*weight_h_exp,1,sum)
  #  rever<-sum(abs(Gamma*weight_target-Re(trt)-1.i*sqrt(1+lambda*Gamma)*Im(trt))^2)/(2*(K+1)^2)
  aic<-ifelse(degrees_freedom<K+1&degrees_freedom>1,log(rever)+2*(K-degrees_freedom)/(K)+2*(K-degrees_freedom)*(K-degrees_freedom+1)/(K*(degrees_freedom-1)),NA)
  aic<-ifelse(degrees_freedom<K+1&degrees_freedom>1,log(rever)+2*(K-degrees_freedom+1)/(degrees_freedom-2),NA)
  # ts.plot(abs(trffkt))   exp(1.i*(0-Lag)*pi*(0:(K))/K)*
  return(list(b=b,trffkt=trffkt,rever=rever,degrees_freedom=degrees_freedom,aic=aic))
}












# Residual code from earlier implementations: probably obsolete
rest_func<-function()
{
  #   dim(Proj_mat)  dim(xtx)  dim(xtxxt) dim(X_inv)    dim(reg_mat)    dim(X_new) dim(Di)     dim(t(svd_obj$v))          dim(t(svd_obj$u))
  #   sum(diag(X_inv))   sum(diag(xtx))   sum(diag(xtx+mean(diag(xtx))*reg_mat))  sum(mean(diag(xtx))*diag(reg_mat))  det((xtx+mean(diag(xtx))*reg_mat)/220)
  # Compute degrees of freedom: (K+1)-rank(Id-Proj_mat)
  #   Proj_mat projects Y (the target) on a sub-space of the (K+1)-dimensional space, see (8.1.8) in Hamilton, p.201
  #  xtxxt<-Re(X_inv)%*%t(Re(X_new))+Im(X_inv)%*%t(Im(X_new))
  xtxxt<-X_inv%*%(t(Re(X_new))+t(Im(X_new)))  
  #  Proj_mat<-(Re(X_new))%*%Re(xtxxt)+(Im(X_new))%*%Im(xtxxt)            #svd(xtxxt)$d     lambda<-18
  Proj_mat<-(Re(X_new)+Im(X_new))%*%xtxxt                               #svd(Proj_mat)$d
  
  xtxxt<-X_inv%*%t(Conj(X_new))
  #  Proj_mat<-(Re(X_new))%*%Re(xtxxt)+(Im(X_new))%*%Im(xtxxt)            #svd(xtxxt)$d     lambda<-18
  Proj_mat<-X_new%*%xtxxt                               #svd(Proj_mat)$d
  abs(sum(diag(((X_new)%*%(X_inv%*%t(Re(X_new)))))))
  # There are various possibilities for deriving the number of degrees of freedom
  # The following variant is the Satterthwaite approximation (last of the three expressions), see http://en.wikipedia.org/wiki/Degrees_of_freedom_(statistics)
  #  degrees_freedom<-sum(diag(t(Proj_mat)%*%Proj_mat))^2/(sum(diag((t(Proj_mat)%*%Proj_mat)%*%(t(Proj_mat)%*%Proj_mat))))                             
  
  # Another variant on wiki-page
  #  degrees_freedom<-sum(diag(t(Proj_mat)%*%Proj_mat))
  # Standard variant:
  
  degrees_freedom<-2*sum(diag(Proj_mat))
  # Alternative: tr(D^2(D^2+reg)^{-1}), where D is the diagonal matrix in svd-decomposition of X: book by James E. Gentle http://books.google.ch/books?id=Pbz3D7Tg5eoC&pg=PA292&lpg=PA292&dq=effective+degrees+of+freedom+shrinkage&source=bl&ots=MXtFVrsGqJ&sig=oLK3YhQSklkJl6VP6wj3x9jxg3g&hl=de&sa=X&ei=1N72TqyAMoiE-waVtNXJAQ&sqi=2&ved=0CFsQ6AEwBg#v=onepage&q=effective%20degrees%20of%20freedom%20shrinkage&f=false
  #  svd_obj <- svd(t(X_new))
  #  Di <- diag(svd_obj$d)
  #  degrees_freedom<-sum(diag(Di^2%*%(solve(Di^2+mean(diag(xtx))*reg_mat))))
  
}






# `Old' 2011 code
mdfa_analytic_old<-function(K,L,lambda,weight_func,Lag,Gamma,expweight,cutoff,i1,i2,weight_constraint)
{
  # weighting of amplitude function in stopband
  omega_Gamma<-as.integer(cutoff*K/pi)
  if ((K-omega_Gamma+1)>0)
  {
    weight_h<-weight_func*(c(rep(1,omega_Gamma),(1:(K-omega_Gamma+1))^(expweight/2)))
  } else
  {
    weight_h<-weight_func*rep(1,K+1)
  }
  #ts.plot(abs(weight_h))       ts.plot(Gamma)
  # Frequency zero receives half weight
  weight_h[1,]<-weight_h[1,]*0.5
  # DFT target variable
  weight_target<-weight_h[,1]
  # Rotate all DFT's such that weight_target is real (rotation does not alter mean-square error)
  weight_h<-weight_h*exp(-1.i*Arg(weight_target))
  weight_target<-weight_target*exp(-1.i*Arg(weight_target))
  # DFT's explaining variables: target variable can be an explaining variable too
  # but it hasn't to be one (counter-example: GDP)
  weight_h_exp<-as.matrix(weight_h[,2:(dim(weight_h)[2])])
  # Regression weights for filter coefficient b_0 i.e. exp(1.i*0)
  X<-Re(exp(1.i*(0-Lag)*pi*(0:(K))/(K))*weight_h_exp)+sqrt(1+Gamma*lambda)*1.i*Im(exp(1.i*(0-Lag)*pi*(0:(K))/(K))*weight_h_exp)
  
  if (i2)
  {
    if (!i1)
    {
      # Restriction i2 is true but i1 is false i.e. L-1 freely determined parameters
      for (l in 2:(L-1))          #l<-L L<-21
      {
        # Regression weights explaining variables
        # Second order restriction is implemented: (l-1)/(L-1)
        X<-cbind(X,Re(exp(1.i*(l-1-Lag)*pi*(0:(K))/(K))*weight_h_exp)-((l-1)/(L-1))*Re(exp(1.i*(L-1-Lag)*pi*(0:(K))/(K))*weight_h_exp)+
                   sqrt(1+Gamma*lambda)*1.i*(Im(exp(1.i*(l-1-Lag)*pi*(0:(K))/(K))*weight_h_exp)-((l-1)/(L-1))*Im(exp(1.i*(L-1-Lag)*pi*(0:(K))/(K))*weight_h_exp)))
      }
      xtx<-t(Re(X))%*%Re(X)+t(Im(X))%*%Im(X)
      # Solution
      bh<-as.vector(solve(xtx)%*%((t(Re(X)*Re(weight_target)))%*%Gamma))
      # the last filter coefficient is a function of the previous ones through the second order restriction
      b<-matrix(nrow=L,ncol=length(weight_h_exp[1,]))
      for (k in 1:length(weight_h_exp[1,])) #k<-1
      {
        bhh<-bh[k+length(weight_h_exp[1,])*(0:(L-2))]
        b[,k]<-c(bhh,-(1:(L-2))%*%bhh[2:(L-1)]/(L-1))
      }
      b_old<-b
    } else
    {
      X<-X-(L-1)*Re(exp(1.i*(L-2-Lag)*pi*(0:(K))/(K))*weight_h_exp)+(L-2)*Re(exp(1.i*(L-1-Lag)*pi*(0:(K))/(K))*weight_h_exp)+
        sqrt(1+Gamma*lambda)*1.i*((-1)*(L-1)*Im(exp(1.i*(L-2-Lag)*pi*(0:(K))/(K))*weight_h_exp)+(L-2)*Im(exp(1.i*(L-1-Lag)*pi*(0:(K))/(K))*weight_h_exp))
      # First and second order restriction: L-2 freely determined parameters
      if (L-2>1)
      {
        for (l in 2:(L-2))          #l<-2 L<-21
        {
          # First and second order constraints are implemented
          X<-cbind(X,Re(exp(1.i*(l-1-Lag)*pi*(0:(K))/(K))*weight_h_exp)-(L-l)*Re(exp(1.i*(L-2-Lag)*pi*(0:(K))/(K))*weight_h_exp)+
                     (L-l-1)*Re(exp(1.i*(L-1-Lag)*pi*(0:(K))/(K))*weight_h_exp)+
                     sqrt(1+Gamma*lambda)*1.i*(Im(exp(1.i*(l-1-Lag)*pi*(0:(K))/(K))*weight_h_exp)-(L-l)*Im(exp(1.i*(L-2-Lag)*pi*(0:(K))/(K))*weight_h_exp)+
                                                 (L-l-1)*Im(exp(1.i*(L-1-Lag)*pi*(0:(K))/(K))*weight_h_exp)))
        }
      }
      xtx<-t(Re(X))%*%Re(X)+t(Im(X))%*%Im(X)
      xtxy<-apply(t(Re(X))%*%((L-1)*Re(exp(1.i*(L-2-Lag)*pi*(0:(K))/(K))*t(t(weight_h_exp)*weight_constraint))-
                                (L-2)*Re(exp(1.i*(L-1-Lag)*pi*(0:(K))/(K))*t(t(weight_h_exp)*weight_constraint)))+
                    t(Im(X))%*%(sqrt(1+Gamma*lambda)*((L-1)*Im(exp(1.i*(L-2-Lag)*pi*(0:(K))/(K))*t(t(weight_h_exp)*weight_constraint))-
                                                        (L-2)*Im(exp(1.i*(L-1-Lag)*pi*(0:(K))/(K))*t(t(weight_h_exp)*weight_constraint)))),1,sum)
      bh<-as.vector(solve(xtx)%*%(((t(Re(X)*Re(weight_target)))%*%Gamma)-xtxy))
      # the last two filter weights are functions of the previous ones through the first and second order restrictions
      b<-matrix(nrow=L,ncol=length(weight_h_exp[1,]))
      for (k in 1:length(weight_h_exp[1,])) #k<-4
      {
        bhh<-bh[k+length(weight_h_exp[1,])*(0:(L-3))]
        b[,k]<-c(bhh,(L-1)*weight_constraint[k]-(L-(1:(L-2)))%*%bhh,-(L-2)*weight_constraint[k]+(L-(2:(L-1)))%*%bhh)
      }
      b_old<-b   #b_new  b_old
    }
  } else
  {
    # No filter constraint imposed
    if (!i1)
    {
      for (l in 2:L)          #l<-2  L<-21
      {
        X<-cbind(X,(Re(exp(1.i*(l-1-Lag)*pi*(0:(K))/(K))*weight_h_exp)+
                      sqrt(1+Gamma*lambda)*1.i*Im(exp(1.i*(l-1-Lag)*pi*(0:(K))/(K))*weight_h_exp)))
        #        X_y<-cbind(X_y,(Re(exp(1.i*(l-1-Lag)*pi*(0:(K))/(K))*weight_h_exp)+
        #        1.i*Im(exp(1.i*(l-1-Lag)*pi*(0:(K))/(K))*weight_h_exp)))
      }
      xtx<-t(Re(X))%*%Re(X)+t(Im(X))%*%Im(X)
      # MA-Filterweights
      bh<-as.vector(solve(xtx)%*%((t(Re(X)*Re(weight_target)))%*%Gamma))
      b<-matrix(nrow=L,ncol=length(weight_h_exp[1,]))
      for (k in 1:length(weight_h_exp[1,]))
      {
        bhh<-bh[k+length(weight_h_exp[1,])*(0:(L-1))]
        b[,k]<-bhh
      }
      b_old<-b
    } else
    {
      # Only first order constraint imposed
      X<-X-Re(exp(1.i*(L-1-Lag)*pi*(0:(K))/(K))*weight_h_exp)-sqrt(1+Gamma*lambda)*1.i*Im(exp(1.i*(L-1-Lag)*pi*(0:(K))/(K))*weight_h_exp)
      for (l in 2:(L-1))          #l<-2 L<-21
      {
        X<-cbind(X,Re(exp(1.i*(l-1-Lag)*pi*(0:(K))/(K))*weight_h_exp)-Re(exp(1.i*(L-1-Lag)*pi*(0:(K))/(K))*weight_h_exp)+
                   sqrt(1+Gamma*lambda)*1.i*Im(exp(1.i*(l-1-Lag)*pi*(0:(K))/(K))*weight_h_exp)-sqrt(1+Gamma*lambda)*1.i*Im(exp(1.i*(L-1-Lag)*pi*(0:(K))/(K))*weight_h_exp))
      }
      xtx<-t(Re(X))%*%Re(X)+t(Im(X))%*%Im(X)
      xtxy<-apply(t(Re(X))%*%Re(exp(1.i*(L-1-Lag)*pi*(0:(K))/(K))*t(t(weight_h_exp)*weight_constraint))+
                    t(Im(X))%*%(sqrt(1+Gamma*lambda)*Im(exp(1.i*(L-1-Lag)*pi*(0:(K))/(K))*t(t(weight_h_exp)*weight_constraint))),1,sum)
      # MA-Filterweights
      bh<-as.vector(solve(xtx)%*%(((t(Re(X)*Re(weight_target)))%*%Gamma)-xtxy))
      # the last weight is a function of the previous ones through the first order restriction
      b<-matrix(nrow=L,ncol=length(weight_h_exp[1,]))
      for (k in 1:length(weight_h_exp[1,]))
      {
        bhh<-bh[k+length(weight_h_exp[1,])*(0:(L-2))]
        b[,k]<-c(bhh,weight_constraint[k]-sum(bhh))
      }
      b_old<-b           #b_new
    }
  }
  #ts.plot(b)
  # Transferfunction
  trffkt<-matrix(nrow=K+1,ncol=length(weight_h_exp[1,]))
  trffkth<-trffkt
  trffkt[1,]<-apply(b,2,sum)
  trffkth[1,]<-trffkt[1,]
  
  for (j in 1:length(weight_h_exp[1,]))
  {
    for (k in 0:(K))#k<-1
    {
      trffkt[k+1,j]<-(b[,j]%*%exp(1.i*k*(0:(L-1))*pi/(K)))
    }
  }
  trt<-apply(((trffkt)*exp(1.i*(0-Lag)*pi*(0:(K))/K))*weight_h_exp,1,sum)
  rever<-sum(abs(Gamma*weight_target-Re(trt)-1.i*sqrt(1+lambda*Gamma)*Im(trt))^2)/(2*(K+1)^2)
  # ts.plot(abs(trffkt))   exp(1.i*(0-Lag)*pi*(0:(K))/K)*
  return(list(b=b,trffkt=trffkt,rever=rever))
}





# Old univariate 2011 code
dfa_analytic<-function(K,L,lambda,weight_func,Lag,Gamma,expweight,cutoff,i1,i2)
{
  #  omega_Gamma<-which(Gamma[length(Gamma):1]>0.01)
  #  omega_Gamma<-length(Gamma)+1-omega_Gamma[1]
  omega_Gamma<-as.integer(cutoff*K/pi)
  #  weight_h<-weight_func*(c(rep(1,omega_Gamma),1+exp(expweight*log(abs(1-exp(1.i*pi*(1:(K-omega_Gamma+1)/(length(weight_func)-1))))))))
  if ((K-omega_Gamma+1)>0)
  {
    weight_h<-weight_func*(c(rep(1,omega_Gamma),(1:(K-omega_Gamma+1))^(expweight)))
  } else
  {
    weight_h<-weight_func*rep(1,K+1)
  }
  # First order filter restriction: assigning a `large' weight to frequency zero
  if (i1)
    weight_h[1]<-max(1.e+10,weight_h[1])
  
  X<-exp(-1.i*Lag*pi*(0:(K))/(K))*rep(1,K+1)*sqrt(weight_h)
  X_y<-exp(-1.i*Lag*pi*(0:(K))/(K))*rep(1,K+1)
  if (i2)
  {
    # Second order restriction: time shift in frequency zero vanishes
    for (l in 2:(L-1))          #l<-L<-21
    {
      X<-cbind(X,(cos((l-1-Lag)*pi*(0:(K))/(K))-((l-1)/(L-1))*cos((L-1-Lag)*pi*(0:(K))/(K))+
                    sqrt(1+Gamma*lambda)*1.i*(sin((l-1-Lag)*pi*(0:(K))/(K))-((l-1)/(L-1))*sin((L-1-Lag)*pi*(0:(K))/(K))))*sqrt(weight_h))
      X_y<-cbind(X_y,(cos((l-1-Lag)*pi*(0:(K))/(K))-((l-1)/(L-1))*cos((L-1-Lag)*pi*(0:(K))/(K))+
                        1.i*(sin((l-1-Lag)*pi*(0:(K))/(K))-((l-1)/(L-1))*sin((L-1-Lag)*pi*(0:(K))/(K)))))
    }
    xtx<-t(Re(X))%*%Re(X)+t(Im(X))%*%Im(X)
    # MA-Filterweights
    b<-as.vector(solve(xtx)%*%(t(Re(X_y))%*%(Gamma*weight_h)))
    # the last weight is a function of the previous ones through the second order restriction
    b<-c(b,-sum(b*(0:(length(b)-1)))/(length(b)+1))
  } else
  {
    for (l in 2:L)          #l<-L<-21
    {
      X<-cbind(X,(cos((l-1-Lag)*pi*(0:(K))/(K))+
                    sqrt(1+Gamma*lambda)*1.i*sin((l-1-Lag)*pi*(0:(K))/(K)))*sqrt(weight_h))
      X_y<-cbind(X_y,(cos((l-1-Lag)*pi*(0:(K))/(K))+
                        1.i*sin((l-1-Lag)*pi*(0:(K))/(K))))
    }
    xtx<-t(Re(X))%*%Re(X)+t(Im(X))%*%Im(X)
    # MA-Filterweights
    b<-as.vector(solve(xtx)%*%(t(Re(X_y))%*%(Gamma*weight_h)))
  }
  #ts.plot(b)
  # Transferfunction
  trffkt<-1:(K+1)
  trffkt[1]<-sum(b)
  for (k in 1:(K))#k<-1
  {
    trffkt[k+1]<-(b%*%exp(-1.i*k*(0:(length(b)-1))*pi/(K)))
  }
  # ts.plot(abs(trffkt))
  return(list(b=b,trffkt=trffkt))
}





# 2011-code
filter_comp<-function(name_sel,x,len,rat,b_sym,ord,lambda,weight_func,Lag,
                      expweight,cutoff,i1,i2,insamp,which_min,K,trffkt,Gamma)
{
  name_min<-c(dimnames(x)[[2]][1],name_sel[which_min,1:which_min])
  weight_constraint_min<-rep(0,which_min)
  L<-as.integer(insamp/(rat*which_min))   #L<-35
  # In practice, L does not have to be larger than the length of the symmetric filter  2*ord+1
  L<-min(L,2*ord+1)
  i_mdfa_min<-mdfa_analytic(K,L,lambda,
                            weight_func[1:(K+1),name_min],Lag,Gamma,expweight,cutoff,i1,i2,weight_constraint_min)
  
  i_mdfa_min$rever
  b_min<-i_mdfa_min$b
  trffkt_min<-i_mdfa_min$trffkt
  ts.plot(cbind(Gamma,abs(trffkt_min))[,1:(which_min+1)],lty=1:(which_min+1))
  xf<-(1:len)*0
  xf_sym<-(1:len)*0
  
  for (i in L:len) #i<-20
  {
    if (i>ord&i<len-ord)
      xf_sym[i]<-b_sym%*%x[(i:(i-ord)),1]+b_sym[2:length(b_sym)]%*%x[((i+1):(i+ord)),1]
    xf[i]<-0
    for (j in 2:(which_min+1))  #j<-2
      xf[i]<-xf[i]+i_mdfa_min$b[,j-1]%*%x[i:(i-L+1),name_min[j]]
  }
  return(list(xf_sym=xf_sym,xf=xf,trffkt=trffkt_min,trffkt_sym=trffkt,b=b_min))
}












#2011 code
rev_seqh<-function(leadmax,endrev,x,len,ord,insamp,weight_func,K,lambda,Gamma,expweight,cutoff,comb_fil,i1,i2,rat,minser)
{
  
  name_sel<-matrix(ncol=dim(x)[2]-1,nrow=dim(x)[2]-1)
  rev_sel<-name_sel
  xf_comb<-matrix(nrow=len,ncol=leadmax+endrev+1)
  xf_min<-xf_comb
  xf_fin<-xf_comb
  # In the loop best estimates are computed for Lag/leads between -leadmax and leadmax+endrev
  # For Lag=-leadmax+endrev the symmetric filter can be replicated perfectly by I-MDFA (with a delay equal to endrev)
  
  for (Lag in -leadmax:endrev)#Lag<--leadmax
  {
    # Selection of variables: the idea goes as follows
    
    
    #  if (Lag<(10))
    #  {
    if (comb_fil)
    {
      
      #        write.table(name_sel,file="C:\\wia\\Projekte\\2011\\oecd\\data\\name_sel.txt",sep="\t")
      gt<-read.table("C:\\wia\\Projekte\\2011\\oecd\\data\\name_sel.txt")
      tr<-matrix(nrow=8,ncol=8)
      for (i in 1:8)
        tr[,i]<-as.character(gt[,i])
      name_sel<-tr
      xf_combh<-matrix(nrow=len,ncol=(dim(x)[[2]]-1))
      for (k_comb in 1:(dim(x)[[2]]-1)) #k_comb<-1
      {
        if ((endrev+leadmax)>0)
        {
          lambda_h<-lambda*(endrev+1-Lag)/(endrev+leadmax)
        } else
        {
          lambda_h<-lambda
        }
        filt<-filter_comp(name_sel,x,len,rat,b_sym,ord,lambda_h,
                          weight_func,Lag,expweight,cutoff,i1,i2,insamp,k_comb,K,trffkt,Gamma)
        xf_combh[,k_comb]<-filt$xf
        #  xf_combh[1:(len/rat),]<-0
      }
      xf_comb[,Lag+leadmax+1]<-apply(xf_combh[,minser:(dim(x)[[2]]-1)],1,mean)
    }
    
    #ts.plot(scale(xf_comb[,5:9]))
    
    #    xf_comb[1:(len-(Lag+leadmax)),Lag+leadmax+1]<-apply(xf_combh,1,mean)[(Lag+leadmax+1):len]
    # ts.plot(scale(xf_comb))
    #    xf_fin[,Lag+leadmax+1]<-apply(xf_combh,1,mean)
    
    #  } else
    #  {
    # We here fix the variables:
    
    #      rat<-3
    
    which_min<-3
    name_sel[3,1:3]<-c( "USA.LOREKP07.IXOBSA.2.M" ,"USA.LOCNDS07.MLSA.2.M" ,"USA.LOCNSH07.IXOB.2.M")
    # Filtering the data: note that filter parameters are applied to full data
    # Thus if insamp<len then filtered data from t=insamp+1,...,len is out of sample
    if ((endrev+leadmax)>0)
    {
      lambda_h<-lambda*(endrev-Lag)/(endrev+leadmax)
    } else
    {
      lambda_h<-lambda
    }
    
    filt_min<-filter_comp(name_sel,x,len,rat,b_sym,ord,lambda_h,
                          weight_func,Lag,expweight,cutoff,i1,i2,insamp,which_min,K,trffkt,Gamma)
    
    xf_min[,Lag+leadmax+1]<-filt_min$xf
    #    xf_min[1:(len-(Lag+leadmax)),Lag+leadmax+1]<-filt_min$xf[(Lag+leadmax+1):len]
    #    xf_fin[,Lag+leadmax+1]<-filt_min$xf
    xf_sym<-filt_min$xf_sym
    b_min<-filt_min$b
    
    trffkt_min<-filt_min$trffkt
    trffkt_sym<-filt_min$trffkt_sym
    
    #  }
    if (comb_fil)
    {                        #-leadmax:endrev
      if ((Lag+leadmax)>0)
      {
        xf_fin[,Lag+leadmax+1]<-((Lag+leadmax)*xf_min[,Lag+leadmax+1]+(leadmax+endrev-(Lag+leadmax))*xf_comb[,Lag+leadmax+1])/(leadmax+endrev)
      } else
      {
        xf_fin[,Lag+leadmax+1]<-xf_comb[,Lag+leadmax+1]
      }
    } else
    {
      xf_fin[,Lag+leadmax+1]<-xf_min[,Lag+leadmax+1]
    }
    ##############
    xf_fin[,Lag+leadmax+1]<-xf_comb[,Lag+leadmax+1]
    
    #xf_min<-scale(xf_min)
    #  ts.plot(cbind((xf_sym),(xf_comb[,leadmax+endrev+1])),lty=1:2)
    print(Lag)
  }
  return(list(xf_sym=xf_sym,xf_fin=xf_fin))
}











# 2011 code
rev_seq<-function(leadmax,endrev,x,len,ord,insamp,weight_func,K,lambda,Gamma,expweight,cutoff,comb_fil,i1,i2,rat,minser)
{
  
  name_sel<-matrix(ncol=dim(x)[2]-1,nrow=dim(x)[2]-1)
  rev_sel<-name_sel
  xf_comb<-matrix(nrow=len,ncol=leadmax+endrev+1)
  xf_min<-xf_comb
  xf_fin<-xf_comb
  # In the loop best estimates are computed for Lag/leads between -leadmax and leadmax+endrev
  # For Lag=-leadmax+endrev the symmetric filter can be replicated perfectly by I-MDFA (with a delay equal to endrev)
  
  for (Lag in -leadmax:endrev)#Lag<--6
  {
    # Selection of variables: the idea goes as follows
    
    #  if (Lag<(10))
    #  {
    if (comb_fil)
    {
      for (k_sel in 1:(length(x[1,])-1))  #k_sel<-2     Lag<--leadmax+15-1
      {
        # filter length real-time filter
        L<-as.integer(insamp/(rat*k_sel))
        
        weight_funch<-weight_func
        for (j in 1:(k_sel)) #j<-1
        {
          ret<-1.e+90
          for (ih in 2:(length(weight_funch[1,])))   #ih<-2
          {
            weight_constrainth<-rep(0,j)
            if (j==1)
            {
              #weight_func<-cbind(weight_funch[,1],weight_func[,name_sel[k_sel,1:(j-1)]],weight_funch[,ih])  weight_constraint<-weight_constrainth
              i_mdfa<-mdfa_analytic(K,L,lambda,weight_funch[,c(1,ih)],Lag,Gamma,expweight,cutoff,i1,i2,weight_constrainth)
            } else
            {
              i_mdfa<-mdfa_analytic(K,L,lambda,cbind(weight_funch[,1],
                                                     weight_func[,name_sel[k_sel,1:(j-1)]],weight_funch[,ih]),Lag,Gamma,expweight,cutoff,i1,i2,weight_constrainth)
            }
            if (i_mdfa$rever<ret)
            {
              ret<-i_mdfa$rever
              rev_sel[k_sel,j]<-ret
              name_sel[k_sel,j]<-dimnames(weight_funch)[[2]][ih]#name_sel[k_sel,]
              ih_min<-ih
            }
          }
          weight_funch<-weight_funch[,-ih_min]#dimnames(weight_funch)
        }
      }
      
      ## pick the `best' solution (for insamp=260 and lead<--4 a single series PMI is selected only)
      which_min<-which(diag(rev_sel)==min(diag(rev_sel)))
      #  ts.plot(abs(cbind(trffkt_sym,trffkt_min)),lty=1:(which_min+1))
      
      # I personally prefer to combine different filters
      # The idea is the following:
      # 1. for k_comb=1 in the loop below the best filter based on a single time series is computed
      # 2. for k_comb=2 the best filter based on two time series is computed
      # 3. This goes on until all time series are selected
      # 4. At the end, all filters are aggregated
      
      xf_comb<-matrix(nrow=len,ncol=(dim(x)[[2]]-1))
      for (k_comb in 1:(dim(x)[[2]]-1)) #k_comb<-8
      {
        if ((endrev+leadmax)>0)
        {
          lambda_h<-lambda*(endrev-Lag)/(endrev+leadmax)
        } else
        {
          lambda_h<-lambda
        }
        filt<-filter_comp(name_sel,x,len,rat,b_sym,ord,lambda_h,
                          weight_func,Lag,expweight,cutoff,i1,i2,insamp,k_comb,K,trffkt,Gamma)
        xf_comb[,k_comb]<-filt$xf
        #  xf_combh[1:(len/rat),]<-0
      }
    }
    #    xf_comb[1:(len-(Lag+leadmax)),Lag+leadmax+1]<-apply(xf_combh,1,mean)[(Lag+leadmax+1):len]
    # ts.plot(scale(xf_comb))
    #    xf_fin[,Lag+leadmax+1]<-apply(xf_combh,1,mean)
    
    #  } else
    #  {
    # We here fix the variables:
    
    #      rat<-3
    
    
    which_min<-3
    name_sel[3,1:3]<-c( "USA.LOREKP07.IXOBSA.2.M" ,"USA.LOCNDS07.MLSA.2.M" ,"USA.LOCNSH07.IXOB.2.M")
    # Filtering the data: note that filter parameters are applied to full data
    # Thus if insamp<len then filtered data from t=insamp+1,...,len is out of sample
    if ((endrev+leadmax)>0)
    {
      lambda_h<-lambda*(endrev-Lag)/(endrev+leadmax)
    } else
    {
      lambda_h<-lambda
    }
    
    filt_min<-filter_comp(name_sel,x,len,rat,b_sym,ord,lambda_h,
                          weight_func,Lag,expweight,cutoff,i1,i2,insamp,which_min,K,trffkt,Gamma)
    
    xf_min[,Lag+leadmax+1]<-filt_min$xf
    #    xf_min[1:(len-(Lag+leadmax)),Lag+leadmax+1]<-filt_min$xf[(Lag+leadmax+1):len]
    #    xf_fin[,Lag+leadmax+1]<-filt_min$xf
    xf_sym<-filt_min$xf_sym
    b_min<-filt_min$b
    
    trffkt_min<-filt_min$trffkt
    trffkt_sym<-filt_min$trffkt_sym
    
    #  }
    if (comb_fil)
    {                        #-leadmax:endrev
      if ((Lag+leadmax)>0)
      {
        xf_fin[,Lag+leadmax+1]<-((Lag+leadmax)*xf_min[,Lag+leadmax+1]+(leadmax+endrev-(Lag+leadmax))*xf_comb[,Lag+leadmax+1])/(leadmax+endrev)
      } else
      {
        xf_fin[,Lag+leadmax+1]<-xf_comb[,Lag+leadmax+1]
      }
    } else
    {
      xf_fin[,Lag+leadmax+1]<-xf_min[,Lag+leadmax+1]
    }
    ##############
    xf_fin[,Lag+leadmax+1]<-xf_comb[,Lag+leadmax+1]
    
    #xf_min<-scale(xf_min)
    #  ts.plot(cbind((xf_sym),(xf_comb[,leadmax+endrev+1])),lty=1:2)
    print(Lag)
  }
  
  return(list(xf_sym=xf_sym,xf_fin=xf_comb))
}








#2011 code
vintage<-function(len,i_start,endrev,leadmax,xf_fin,xf_sym)
{
  imdfa_vint<-matrix(nrow=len,ncol=(len+1-i_start-endrev-leadmax))
  imdfa_vint[1:i_start,1]<-xf_fin[(endrev)+1:i_start,(endrev+leadmax)+1]
  ts.plot(cbind(xf_sym,imdfa_vint[,1]),lty=1:2)
  
  
  for (j in 0:(len-i_start-endrev-leadmax)) #j<-0(len-i_start-endrev-leadmax)
  {
    imdfa_vint[1:(j+i_start),j+1]<-xf_fin[(endrev+leadmax)+1:(j+i_start),(endrev+leadmax)+1]
    for (k in 1:(endrev+leadmax))#k<-2
    {
      imdfa_vint[k+(j+i_start),j+1]<-xf_fin[endrev+leadmax+(j+i_start),(endrev+leadmax+1)-k]
    }
  }
  dim(imdfa_vint)
  return(imdfa_vint=imdfa_vint)
}






# 2011 code
vint_plot<-function(len,i_start,ord,leadmax,imdfa_vint,xf_sym)
{
  inc<-50
  require(graphics)
  colo<-rainbow(inc)
  
  # Last bunch towards the sample end
  j<-as.integer((len-i_start)/inc)
  ind<-(i_start+ord+leadmax+((j-1)*inc)):len
  ymin=min(imdfa_vint[ind,(((j-1)*inc+1):length(imdfa_vint[1,]))],na.rm=T)
  ymax=max(imdfa_vint[ind,(((j-1)*inc+1):length(imdfa_vint[1,]))],na.rm=T)
  plot(rep(xf_sym[len],length(ind)),ylim=c(ymin,ymax),type="l",
       axes=F,col="black",xlab="Levels",ylab="")
  for (k in 1:length(ind))
    lines(imdfa_vint[ind,((j-1)*inc)+k],col=colo[k],lty=k)
  axis(1,at=1:length(ind),labels=xdate[ind])
  axis(2)
  box()
  for (j in as.integer((len-i_start)/inc):1) #j<-17
  {
    x11()
    if (i_start+ord+leadmax+(j*inc)<len)
    {
      ind<-i_start+ord-1+leadmax+(((j-1)*inc+1):(j*inc))
      ymin=min(apply(cbind(xf_sym[leadmax+ind],imdfa_vint[ind,(((j-1)*inc+1):(j*inc))]),1,min,na.rm=T))
      ymax=max(apply(cbind(xf_sym[leadmax+ind],imdfa_vint[ind,(((j-1)*inc+1):(j*inc))]),1,max,na.rm=T))
      plot(xf_sym[leadmax+ind],ylim=c(ymin,ymax),type="l",
           axes=F,col="black",xlab="Levels",ylab="")
      for (k in 1:inc)
        lines(imdfa_vint[ind,((j-1)*inc)+k],col=colo[k],lty=k)
      axis(1,at=1:inc,labels=xdate[ind])
      axis(2)
      box()
    }
  }
}







# The following is all 2011-code
#-----------------------------------------------------------
# I-MDFA vs. I-DFA: Filter performances
# Simulate a GDP setting where GDP cannot be observed i.e. only some of the components are observed
simulation_IMDFA<-function()
{
  # Definition of the (symmetric) Signal with cutoff lambda1
  lambda1<-pi/6
  ord<-L
  # Coefficients
  b<-c(lambda1/pi,(1/pi)*sin(lambda1*1:ord)/(1:ord))
  b<-b/(sum(b)+sum(b[2:(ord+1)]))
  #ts.plot(b[1:min(1000,ord)])
  # Transferfunction
  len1<-len/2
  trffkt<-0:len1
  for (k in 0:len1)#k<-0
  {
    omegak<-k*pi/len1
    trffkt[k+1]<-b%*%(cos(omegak*0:ord)*c(1,rep(2,ord)))
    
  }
  
  Gamma<-trffkt
  ts.plot(Gamma)
  
  
  K<-len/2
  L<-10
  Lag<-0
  expweight<-0
  lambda<-0
  cutoff<-pi/6
  
  # Time series
  ar1<-0.8
  ar2<-0.99
  set.seed(1)
  eps<-rnorm(10000)
  #model<-list(ar=ar1)
  y1l<-rep(0,10000)
  for (i in 1:(10000-1)) #i<-1
  {
    y1l[i+1]<-ar1*y1l[i]+eps[i+1]
  }
  eps<-rnorm(10000)
  zl1<-rep(0,10000)
  for (i in 1:(10000-1))
  {
    zl1[i+1]<-ar2*zl1[i]+eps[i+1]
  }
  eps<-rnorm(10000)
  zl2<-rep(0,10000)
  for (i in 1:(10000-1))
  {
    zl2[i+1]<-ar2*zl2[i]+eps[i+1]
  }
  eps<-rnorm(10000)
  zl3<-rep(0,10000)
  for (i in 1:(10000-1))
  {
    zl3[i+1]<-ar2*zl3[i]+eps[i+1]
  }
  y1<-y1l[(10000-3*len):10000]
  y2<-diff(cumsum(y1)+10*zl1[(10000-3*len):10000])
  y3<-diff(cumsum(y1)+10*zl2[(10000-3*len):10000])
  y4<-diff(cumsum(y1)+10*zl3[(10000-3*len):10000])
  
  y1<-y1[2:length(y1)]
  ts.plot(cbind(y1,y2),lty=1:2)
  ts.plot(apply(cbind(y1,y2,y3),2,cumsum),lty=1:3)
  cor(y1,y2)
  acf(y1)
  acf(y2)
  
  # y is made of four different components (y~GDP)
  y<-y1+y2+y3+y4
  ts.plot(y)
  # This setting replicates univariate DFA: explaining variable is identical
  # with target variable y
  x<-cbind(y,y)
  # In this setting all subseries are used
  x1<-cbind(y,y1,y2,y3,y4)
  # In this setting one uses information from y1 and y2 only
  x2<-cbind(y,y1,y2)
  
  ts.plot(scale(cbind(apply(apply(cbind(y1,y2,y3),2,cumsum),1,sum),cumsum(y))),lty=1:2)
  
  weight_func<-matrix(nrow=K+1,ncol=dim(x)[2])
  weight_func[,1]<-periodogram_bp(x[1:len,1], 0, len)$fourtrans
  if (length(weight_func[1,])>1)
  {
    for (j in 2:length(x[1,]))
    {
      weight_func[,j]<-periodogram_bp(x[1:len,j], 0, len)$fourtrans
    }
  }
  # if i1<-T then weight_constraint imposes corresponding values of amplitude functions in frequency zero
  weight_constraint<-c(1,rep(0,length(x[1,])-2))
  
  weight_func1<-matrix(nrow=K+1,ncol=dim(x1)[2])
  weight_func1[,1]<-periodogram_bp(x1[(len+1):(2*len),1], 0, len)$fourtrans
  if (length(weight_func1[1,])>1)
  {
    for (j in 2:length(x1[1,]))
    {
      weight_func1[,j]<-periodogram_bp(x1[(len+1):(2*len),j], 0, len)$fourtrans
    }
  }
  # if i1<-T then weight_constraint1 imposes corresponding values of amplitude functions in frequency zero
  weight_constraint1<-c(1,rep(0,length(x1[1,])-2))
  
  weight_func2<-matrix(nrow=K+1,ncol=dim(x2)[2])
  weight_func2[,1]<-periodogram_bp(x2[(len+1):(2*len),1], 0, len)$fourtrans
  if (length(weight_func2[1,])>1)
  {
    for (j in 2:length(x2[1,]))
    {
      weight_func2[,j]<-periodogram_bp(x2[(len+1):(2*len),j], 0, len)$fourtrans
    }
  }
  # if i1<-T then weight_constraint2 imposes corresponding values of amplitude functions in frequency zero
  weight_constraint2<-c(1,rep(0,length(x2[1,])-2))
  
  
  i1<-F
  i2<-F
  L_mdfa<-10
  i_mdfa<-mdfa_analytic(K,L_mdfa,lambda,weight_func,Lag,Gamma,expweight,cutoff,i1,i2,weight_constraint)
  i1<-F
  i2<-F
  #weight_constraint1<-1
  i_mdfa1<-mdfa_analytic(K,L,lambda,weight_func1,Lag,Gamma,expweight,cutoff,i1,i2,weight_constraint1)
  i1<-F
  i2<-F
  L_mdfa<-10
  i_mdfa2<-mdfa_analytic(K,L_mdfa,lambda,weight_func2,Lag,Gamma,expweight,cutoff,i1,i2,weight_constraint2)
  i1<-F
  i2<-F
  L<-10
  dfa<-dfa_analytic(K,L,lambda,abs(weight_func[,1])^2,Lag,Gamma,expweight,cutoff,i1,i2)
  
  b_RTSE<-dfa$b
  trffkt_RTSE<-dfa$trffkt
  par(mfrow=c(2,1))
  ts.plot(abs(trffkt_RTSE),ylim=c(0,max(abs(trffkt_RTSE))))
  ts.plot(abs(i_mdfa1$trffkt),lty=1:dim(x)[2])
  par(mfrow=c(2,1))
  ts.plot(Arg(i_mdfa$trffkt)[2:(K+1),]/((1:(K))*pi/K),lty=1:2)
  ts.plot(Arg(trffkt_RTSE)[2:(K+1)]/((1:(K))*pi/K),lty=1:2)
  
  # All filters
  
  xf_sym<-L:len
  xf_idfa<-xf_sym
  xf_imdfa<-xf_sym
  xf_imdfa1<-xf_sym
  xf_imdfa2<-xf_sym
  
  for (i in L:len) #i<-L
  {
    xf_sym[i-L+1]<-b[1:ord]%*%y[len+(i:(i-ord+1))]+b[2:ord]%*%y[len+((i+1):(i+ord-1))]
    xf_imdfa[i-L+1]<-0
    for (j in 2:length(x[1,]))  #j<-2
      xf_imdfa[i-L+1]<-xf_imdfa[i-L+1]+i_mdfa$b[,j-1]%*%x[len+(i:(i-L_mdfa+1)),j]
    xf_imdfa1[i-L+1]<-0
    for (j in 2:length(x1[1,]))  #j<-2
      xf_imdfa1[i-L+1]<-xf_imdfa1[i-L+1]+i_mdfa1$b[,j-1]%*%x1[len+(i:(i-L+1)),j]
    xf_imdfa2[i-L+1]<-0
    for (j in 2:length(x2[1,]))  #j<-2
      xf_imdfa2[i-L+1]<-xf_imdfa2[i-L+1]+i_mdfa2$b[,j-1]%*%x2[len+(i:(i-L+1)),j]
    
    xf_idfa[i-L+1]<-dfa$b%*%x[len+(i:(i-L+1)),1]
    #  xf_idfa[i-L+1]<-dfa$b%*%(x[i:(i-L+1),2]+x[i:(i-L+1),2])/1.6
  }
  
  
  
  ts.plot(cbind(xf_sym,xf_imdfa,xf_idfa),lty=1:3)
  
  mean((xf_sym-xf_imdfa)^2)
  mean((xf_sym-xf_imdfa1)^2)
  mean((xf_sym-xf_imdfa2)^2)
  mean((xf_sym-xf_idfa)^2)
  
  #-----------------------------------------------
  # Simulation out of sample
  # y is made of y1,y2,...,y5
  # one can try to include partial information only (in the example, y1,y2,y3)
  
  ar1<-0.8
  ar2<-0.99
  set.seed(10)
  anzsim<-100
  imdfa_os<-1:anzsim
  imdfa_is<-1:anzsim
  idfa_os<-1:anzsim
  
  for (ksim in 1:anzsim)
  {
    eps<-rnorm(10000)
    #model<-list(ar=ar1)
    y1l<-rep(0,10000)
    for (i in 1:(10000-1)) #i<-1
    {
      y1l[i+1]<-ar1*y1l[i]+eps[i+1]
    }
    eps<-rnorm(10000)
    zl1<-rep(0,10000)
    for (i in 1:(10000-1))
    {
      zl1[i+1]<-ar2*zl1[i]+eps[i+1]
    }
    eps<-rnorm(10000)
    zl2<-rep(0,10000)
    for (i in 1:(10000-1))
    {
      zl2[i+1]<-ar2*zl2[i]+eps[i+1]
    }
    eps<-rnorm(10000)
    zl3<-rep(0,10000)
    for (i in 1:(10000-1))
    {
      zl3[i+1]<-ar2*zl3[i]+eps[i+1]
    }
    eps<-rnorm(10000)
    zl4<-rep(0,10000)
    for (i in 1:(10000-1))
    {
      zl4[i+1]<-ar2*zl3[i]+eps[i+1]
    }
    y1<-y1l[(10000-3*len):10000]
    y2<-diff(cumsum(y1)+6*zl1[(10000-3*len):10000])
    y3<-diff(cumsum(y1)+6*zl2[(10000-3*len):10000])
    y4<-diff(cumsum(y1)+6*zl3[(10000-3*len):10000])
    y5<-diff(cumsum(y1)+6*zl4[(10000-3*len):10000])
    
    y1<-y1[2:length(y1)]
    
    y<-y1+y2+y3+y4+y5
    x<-cbind(y,y1,y2,y3)
    
    
    
    weight_func<-matrix(nrow=K+1,ncol=dim(x)[2])
    weight_func[,1]<-periodogram_bp(x[1:len,1], 0, len)$fourtrans
    if (length(weight_func[1,])>1)
    {
      for (j in 2:length(x[1,]))
      {
        weight_func[,j]<-periodogram_bp(x[1:len,j], 0, len)$fourtrans
      }
    }
    weight_constraint<-c(1,0)
    x1<-cbind(y,y1,y2,y3)
    weight_func1<-matrix(nrow=K+1,ncol=dim(x1)[2])
    weight_func1[,1]<-periodogram_bp(x1[(len+1):(2*len),1], 0, len)$fourtrans
    if (length(weight_func1[1,])>1)
    {
      for (j in 2:length(x1[1,]))
      {
        weight_func1[,j]<-periodogram_bp(x1[(len+1):(2*len),j], 0, len)$fourtrans
      }
    }
    weight_constraint<-c(1,rep(0,length(x[1,])-2))
    #weight_func[1,]<-100000
    
    i1<-F
    i2<-F
    L_mdfa<-8
    i_mdfa<-mdfa_analytic(K,L_mdfa,lambda,weight_func,Lag,Gamma,expweight,cutoff,i1,i2,weight_constraint)
    i1<-F
    i2<-F
    weight_constraint1<-1
    i_mdfa1<-mdfa_analytic(K,L,lambda,weight_func1,Lag,Gamma,expweight,cutoff,i1,i2,weight_constraint1)
    i1<-F
    i2<-F
    dfa<-dfa_analytic(K,L,lambda,abs(weight_func[,1])^2,Lag,Gamma,expweight,cutoff,i1,i2)
    
    b_RTSE<-dfa$b
    trffkt_RTSE<-dfa$trffkt
    # All filters
    
    xf_sym<-L:len
    xf_idfa<-xf_sym
    xf_imdfa<-xf_sym
    xf_imdfa1<-xf_sym
    
    for (i in L:len) #i<-L
    {
      xf_sym[i-L+1]<-b[1:ord]%*%y[len+(i:(i-ord+1))]+b[2:ord]%*%y[len+((i+1):(i+ord-1))]
      xf_imdfa[i-L+1]<-0
      for (j in 2:length(x[1,]))  #j<-2
        xf_imdfa[i-L+1]<-xf_imdfa[i-L+1]+i_mdfa$b[,j-1]%*%x[len+(i:(i-L_mdfa+1)),j]
      xf_imdfa1[i-L+1]<-0
      for (j in 2:length(x1[1,]))  #j<-2
        xf_imdfa1[i-L+1]<-xf_imdfa1[i-L+1]+i_mdfa1$b[,j-1]%*%x1[len+(i:(i-L+1)),j]
      
      xf_idfa[i-L+1]<-dfa$b%*%x[len+(i:(i-L+1)),1]
      #  xf_idfa[i-L+1]<-dfa$b%*%(x[i:(i-L+1),2]+x[i:(i-L+1),2])/1.6
    }
    print(ksim)
    
    imdfa_os[ksim]<-mean((xf_sym-xf_imdfa)^2)
    imdfa_is[ksim]<-mean((xf_sym-xf_imdfa1)^2)
    idfa_os[ksim]<-mean((xf_sym-xf_idfa)^2)
  }
  mean(imdfa_os)
  mean(imdfa_is)
  mean(idfa_os)
}







zpc_opt<-function(parm,al)
{
  
  
  maxpol<-al$maxpol
  maxamp<-al$maxamp                                
  K<-al$K
  Gamma<-al$Gamma                    #wh1-weight_funchh-weight_h
  weight_funchh<-al$weight_funchh     #ts.plot(abs(weight_funchh)[,2:9])
  opt<-al$opti
  expweight<-al$expweight
  lambda<-al$lambda
  cutoff<-al$cutoff
  omega_Gamma<-as.integer(cutoff*K/pi)
  weight_hh<-weight_funchh*(c(rep(1,omega_Gamma),(1:(K-omega_Gamma+1))^(expweight/2)))
  # Rotation of DFT's such such that DFT of target is real (rotation does not alter mean-square error)
  weight_h<-weight_hh*exp(-1.i*Arg(weight_hh[,1]))
  
  parme<-matrix(parm,nrow=4)
  trzpc<-matrix(nrow=K+1,ncol=length(weight_funchh[1,])-1)
  trzpc_agg<-rep(0,K+1)
  for (i in 1:(length(weight_funchh[1,])-1))     #Lag<-0
  {
    # Argument
    parme[1,i]<-min(abs(parme[1,i]),lb)
    parme[1,i]<-max(abs(parme[1,i]),ub)
    # Length of zero
    parme[2,i]<-min(1,abs(parme[2,i]))
    # length of pole
    parme[3,i]<-min(maxpol,abs(parme[3,i]))
    parme[4,i]<-abs(parme[4,i])
    # zpc transferfunction
    Z<-parme[2,i]*exp(1.i*(pi/parme[1,i]))
    P<-parme[3,i]*exp(1.i*(pi/parme[1,i]))
    trzpc[,i]<-parme[4,i]*((1-Z*exp(1.i*pi*(0:K)/K))*(1-Conj(Z)*exp(1.i*pi*(0:K)/K)))/
      ((1-P*exp(1.i*pi*(0:K)/K))*(1-Conj(P)*exp(1.i*pi*(0:K)/K)))
    #    trzpc[,i]<-parme[4,i]*((1-Z*exp(1.i*pi*(0:K)/K))*(1-Conj(Z)*exp(1.i*pi*(0:K)/K)))/
    #    ((1-P*exp(1.i*pi*(0:K)/K))*(1-Conj(P)*exp(1.i*pi*(0:K)/K)))
    trzpc_agg<-trzpc_agg+trzpc[,i]*weight_h[,i+1]  #ts.plot(abs(weight_h)[,2:9],lty=1:9)
  }          #ts.plot(abs(trzpc_agg),lty=1:9)
  
  # ZPC are thought as a complement to MA-filters: If MA are OK then ZPC should be an identity
  # Therefore:
  #    -too large a value of the amplitude is to be avoided
  #    -higher maximum of amplitude in high-frequency stop-band than in lowpass is to be avoided
  maxamp_zpc<-max(apply(abs(trzpc),2,max))
  maxamp_zpc_high<-apply(abs(trzpc)[as.integer(cutoff*K/pi+1):(K+1),],2,max)
  maxamp_zpc_low<-apply(abs(trzpc)[-(as.integer(cutoff*K/pi+1):(K+1)),],2,max)
  # ts.plot(abs(trzpc))
  
  #  if (sum(c(maxamp_zpc>maxamp,maxamp_zpc_high>20))>0)
  #  {
  #    crit<-1.e+90
  #  } else
  #  {
  # Same criterion as I-MDFA but positive phase only
  crit<-sum(abs(Gamma*weight_h[,1]-Re(exp(-Lag*1.i*pi*(0:K)/K)*trzpc_agg)-
                  (Arg(exp(-Lag*1.i*pi*(0:K)/K)*trzpc_agg)>0)*sqrt(1+Gamma*lambda)*1.i*Im(exp(-Lag*1.i*pi*(0:K)/K)*trzpc_agg))^2)/(2*(K+1)^2)
  # Criterion where imaginary part of real-time filter is emphasized (where phase is positive)
  lenc<-(length(weight_funchh[1,])-1)
  ph_pos<-(Arg(exp(-Lag*1.i*pi*(0:K)/K)*trzpc)>0)    
  Rep<-Re(trzpc*exp(-Lag*1.i*pi*(0:K)/K))*Re(weight_h[,1+1:lenc])-(1+lambda*Gamma*ph_pos)*Im(trzpc*exp(-Lag*1.i*pi*(0:K)/K))*Im(weight_h[,1+1:lenc])
  Imp<-Re(trzpc*exp(-Lag*1.i*pi*(0:K)/K))*Im(weight_h[,1+1:lenc])+(1+lambda*Gamma*ph_pos)*Im(trzpc*exp(-Lag*1.i*pi*(0:K)/K))*Re(weight_h[,1+1:lenc])
  crit<-sum(abs(Gamma*weight_h[,1]-Rep-1.i*Imp)^2)/(2*(K+1)^2)
  # Same criterion as I-MDFA
  crit<-sum(abs(Gamma*weight_h[,1]-Re(exp(-Lag*1.i*pi*(0:K)/K)*trzpc_agg)-
                  sqrt(1+Gamma*lambda)*1.i*Im(exp(-Lag*1.i*pi*(0:K)/K)*trzpc_agg))^2)/(2*(K+1)^2)
  
  #  }
  #ts.plot(cbind(abs(weight_h[,1]),abs(weight_func[,1])),lty=1:2)
  # ts.plot(cbind(abs(Gamma*weight_h[,1]),abs(trzpc_agg)),lty=1:2)
  # ts.plot(cbind(abs(Gamma*weight_h[,1]),abs(apply(wh1[,2:9]*trzpc,1,sum))),lty=1:2)
  if (opti)
  {
    return(crit=crit)
  } else
  {
    return(list(crit=crit,trzpc=trzpc,trzpc_agg=trzpc_agg))
  }
}







zpc_opt_single<-function(parm_single,al)
{
  
  
  maxpol<-al$maxpol
  maxamp<-al$maxamp                                
  K<-al$K
  Gamma<-al$Gamma                    #wh1-weight_funchh-weight_h
  weight_funchh<-al$weight_funchh     #ts.plot(abs(weight_funchh)[,2:9])
  opt<-al$opti
  expweight<-al$expweight
  lambda<-al$lambda
  cutoff<-al$cutoff
  omega_Gamma<-as.integer(cutoff*K/pi)
  weight_hh<-weight_funchh*(c(rep(1,omega_Gamma),(1:(K-omega_Gamma+1))^(expweight/2)))
  # Rotation of DFT's such such that DFT of target is real (rotation does not alter mean-square error)
  weight_h<-weight_hh*exp(-1.i*Arg(weight_hh[,1]))
  
  trzpc<-rep(0,K+1)
  trzpc_agg<-trzpc
  parm_single[1]<-min(abs(parm_single[1]),lb)
  parm_single[1]<-max(abs(parm_single[1]),ub)
  # Length of zero
  parm_single[2]<-min(1,abs(parm_single[2]))
  # length of pole
  parm_single[3]<-min(maxpol,abs(parm_single[3]))
  parm_single[4]<-abs(parm_single[4])
  # zpc transferfunction
  Z<-parm_single[2]*exp(1.i*(pi/parm_single[1]))
  P<-parm_single[3]*exp(1.i*(pi/parm_single[1]))
  trzpc<-parm_single[4]*((1-Z*exp(1.i*pi*(0:K)/K))*(1-Conj(Z)*exp(1.i*pi*(0:K)/K)))/
    ((1-P*exp(1.i*pi*(0:K)/K))*(1-Conj(P)*exp(1.i*pi*(0:K)/K)))
  
  
  for (i in 1:(length(weight_funchh[1,])-1))     #Lag<-0
  {
    trzpc_agg<-trzpc_agg+trzpc*weight_h[,i+1]  #ts.plot(abs(weight_h)[,2:9],lty=1:9)
  }          #ts.plot(abs(trzpc_agg),lty=1:9)
  
  # ZPC are thought as a complement to MA-filters: If MA are OK then ZPC should be an identity
  # Therefore:
  #    -too large a value of the amplitude is to be avoided
  #    -higher maximum of amplitude in high-frequency stop-band than in lowpass is to be avoided
  #  maxamp_zpc<-max(apply(abs(trzpc),2,max))
  #  maxamp_zpc_high<-apply(abs(trzpc)[as.integer(cutoff*K/pi+1):(K+1),],2,max)
  #  maxamp_zpc_low<-apply(abs(trzpc)[-(as.integer(cutoff*K/pi+1):(K+1)),],2,max)
  # ts.plot(abs(trzpc))
  
  #  if (sum(c(maxamp_zpc>maxamp,maxamp_zpc_high>20))>0)
  #  {
  #    crit<-1.e+90
  #  } else
  #  {
  # Same criterion as I-MDFA but positive phase only
  crit<-sum(abs(Gamma*weight_h[,1]-Re(exp(-Lag*1.i*pi*(0:K)/K)*trzpc_agg)-
                  (Arg(exp(-Lag*1.i*pi*(0:K)/K)*trzpc_agg)>0)*sqrt(1+Gamma*lambda)*1.i*Im(exp(-Lag*1.i*pi*(0:K)/K)*trzpc_agg))^2)/(2*(K+1)^2)
  # Criterion where imaginary part of real-time filter is emphasized (where phase is positive)
  lenc<-(length(weight_funchh[1,])-1)
  ph_pos<-(Arg(exp(-Lag*1.i*pi*(0:K)/K)*trzpc)>0)    
  Rep<-Re(trzpc*exp(-Lag*1.i*pi*(0:K)/K))*Re(weight_h[,1+1:lenc])-(1+lambda*Gamma*ph_pos)*Im(trzpc*exp(-Lag*1.i*pi*(0:K)/K))*Im(weight_h[,1+1:lenc])
  Imp<-Re(trzpc*exp(-Lag*1.i*pi*(0:K)/K))*Im(weight_h[,1+1:lenc])+(1+lambda*Gamma*ph_pos)*Im(trzpc*exp(-Lag*1.i*pi*(0:K)/K))*Re(weight_h[,1+1:lenc])
  crit<-sum(abs(Gamma*weight_h[,1]-Rep-1.i*Imp)^2)/(2*(K+1)^2)
  # Same criterion as I-MDFA
  crit<-sum(abs(Gamma*weight_h[,1]-Re(exp(-Lag*1.i*pi*(0:K)/K)*trzpc_agg)-
                  sqrt(1+Gamma*lambda)*1.i*Im(exp(-Lag*1.i*pi*(0:K)/K)*trzpc_agg))^2)/(2*(K+1)^2)
  
  #  }
  #ts.plot(cbind(abs(weight_h[,1]),abs(weight_func[,1])),lty=1:2)
  # ts.plot(cbind(abs(Gamma*weight_h[,1]),abs(trzpc_agg)),lty=1:2)
  # ts.plot(cbind(abs(Gamma*weight_h[,1]),abs(apply(wh1[,2:9]*trzpc,1,sum))),lty=1:2)
  if (opti)
  {
    return(crit=crit)
  } else
  {
    return(list(crit=crit,trzpc=trzpc,trzpc_agg=trzpc_agg))
  }
}








zpc_opt_single_angle<-function(parm_single,al)
{
  
  
  maxpol<-al$maxpol
  maxamp<-al$maxamp                                
  K<-al$K
  Gamma<-al$Gammah                    #wh1-weight_funchh-weight_h
  weight_funchh<-al$weight_funchh     #ts.plot(abs(weight_funchh)[,2:9])
  opti<-al$opti
  expweight<-al$expweight
  lambda<-al$lambda
  cutoff<-al$cutoff
  omega_Gamma<-as.integer(cutoff*K/pi)
  weight_hh<-weight_funchh*(c(rep(1,omega_Gamma),(1:(K-omega_Gamma+1))^(expweight/2)))
  # Rotation of DFT's such such that DFT of target is real (rotation does not alter mean-square error)
  weight_h<-weight_hh*exp(-1.i*Arg(weight_hh[,1]))
  
  trzpc<-rep(0,K+1)
  trzpc_agg<-trzpc
  #  parm_single[1]<-min(abs(parm_single[1]),lb)
  #  parm_single[1]<-max(abs(parm_single[1]),ub)
  parm_single[1]<-al$arg
  # Length of zero
  parm_single[2]<-min(1,abs(parm_single[2]))
  # length of pole
  parm_single[3]<-min(maxpol,abs(parm_single[3]))
  parm_single[4]<-abs(parm_single[4])
  # zpc transferfunction
  # Frequency zero or larger than zero
  if (parm_single[1]>2*K)
  {
    Z<-parm_single[2]
    P<-parm_single[3]
  } else
  {
    Z<-parm_single[2]*exp(1.i*(pi/parm_single[1]))
    P<-parm_single[3]*exp(1.i*(pi/parm_single[1]))
  }
  trzpc<-parm_single[4]*((1-Z*exp(1.i*pi*(0:K)/K))*(1-Conj(Z)*exp(1.i*pi*(0:K)/K)))/
    ((1-P*exp(1.i*pi*(0:K)/K))*(1-Conj(P)*exp(1.i*pi*(0:K)/K)))
  #ts.plot(abs(trzpc))
  
  for (i in 1:(length(weight_funchh[1,])-1))     #Lag<-0
  {
    trzpc_agg<-trzpc_agg+trzpc*weight_h[,i+1]  #ts.plot(abs(weight_h)[,2:9],lty=1:9)
  }          #ts.plot(abs(trzpc_agg),lty=1:9)
  
  # ZPC are thought as a complement to MA-filters: If MA are OK then ZPC should be an identity
  # Therefore:
  #    -too large a value of the amplitude is to be avoided
  #    -higher maximum of amplitude in high-frequency stop-band than in lowpass is to be avoided
  #  maxamp_zpc<-max(apply(abs(trzpc),2,max))
  #  maxamp_zpc_high<-apply(abs(trzpc)[as.integer(cutoff*K/pi+1):(K+1),],2,max)
  #  maxamp_zpc_low<-apply(abs(trzpc)[-(as.integer(cutoff*K/pi+1):(K+1)),],2,max)
  # ts.plot(abs(trzpc))
  
  #  if (sum(c(maxamp_zpc>maxamp,maxamp_zpc_high>20))>0)
  #  {
  #    crit<-1.e+90
  #  } else
  #  {
  # Same criterion as I-MDFA but positive phase only
  crit<-sum(abs(Gamma*weight_h[,1]-Re(exp(-Lag*1.i*pi*(0:K)/K)*trzpc_agg)-
                  (Arg(exp(-Lag*1.i*pi*(0:K)/K)*trzpc_agg)>0)*sqrt(1+Gamma*lambda)*1.i*Im(exp(-Lag*1.i*pi*(0:K)/K)*trzpc_agg))^2)/(2*(K+1)^2)
  # Criterion where imaginary part of real-time filter is emphasized (where phase is positive)
  lenc<-(length(weight_funchh[1,])-1)
  ph_pos<-(Arg(exp(-Lag*1.i*pi*(0:K)/K)*trzpc)>0)    
  Rep<-Re(trzpc*exp(-Lag*1.i*pi*(0:K)/K))*Re(weight_h[,1+1:lenc])-(1+lambda*Gamma*ph_pos)*Im(trzpc*exp(-Lag*1.i*pi*(0:K)/K))*Im(weight_h[,1+1:lenc])
  Imp<-Re(trzpc*exp(-Lag*1.i*pi*(0:K)/K))*Im(weight_h[,1+1:lenc])+(1+lambda*Gamma*ph_pos)*Im(trzpc*exp(-Lag*1.i*pi*(0:K)/K))*Re(weight_h[,1+1:lenc])
  crit<-sum(abs(Gamma*weight_h[,1]-Rep-1.i*Imp)^2)/(2*(K+1)^2)
  # Same criterion as I-MDFA
  crit<-sum(abs(Gamma*weight_h[,1]-Re(exp(-Lag*1.i*pi*(0:K)/K)*trzpc_agg)-
                  sqrt(1+Gamma*lambda)*1.i*Im(exp(-Lag*1.i*pi*(0:K)/K)*trzpc_agg))^2)/(2*(K+1)^2)
  
  #  }
  #ts.plot(cbind(abs(weight_h[,1]),abs(weight_func[,1])),lty=1:2)
  # ts.plot(cbind(abs(Gamma*weight_h[,1]),abs(trzpc_agg)),lty=1:2)
  # ts.plot(cbind(abs(Gamma*weight_h[,1]),abs(apply(wh1[,2:9]*trzpc,1,sum))),lty=1:2)
  if (opti)
  {
    return(crit=crit)
  } else
  {
    return(list(crit=crit,trzpc=trzpc,trzpc_agg=trzpc_agg))
  }
}







