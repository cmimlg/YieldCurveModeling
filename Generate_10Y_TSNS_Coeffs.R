.libPaths("/home/admin123/R/x86_64-pc-linux-gnu-library/3.2")
library(xts)
library(mvtnorm)
library(ggplot2)
setwd("/home/admin123/Yield_Curve_Modelling")
fp = "Src_2_2006_to_Present_Treasury_Data.csv"
data<-read.csv(fp,header=TRUE,sep=",")
data1<-data.matrix(data[,-1])
#rownames(data1)<-as.Date(data[,1],format="%Y-%m-%d")
rownames(data1)<-data[,1]
str(data1)
data2<-as.xts(data1,descr='my new xts object')
term<-c(1/12,3/12,6/12,12/12,24/12,36/12,60/12,84/12,120/12,240/12,360/12)
####################################
## Section1: Fit Nelson-Siegel Model
###################################

fb1<-function(lambda,term){
  
  fb1<-as.numeric((1 - exp(-lambda * term))/(lambda * term))
  return(fb1)
}
fb2<-function(lambda,term){ 
  #cat("In fb2: lambda = ", lambda, ". term= ", term, "\n")
  fb2<-as.numeric((1 - exp(-lambda * term))/(lambda * term) - 
                    exp(-lambda * term))
  #cat("Value returned is ", fb2,"\n")
  return(fb2)
}

## Estimate beta's of NS model - assume lambda known
NS_beta_hat<-function(rate,term,lambda){
  #cat("In est. beta, term = ", term, " lambda = " , lambda, '\n')
  fit<-lm(rate~1+fb1(lambda=lambda,term=term)+fb2(lambda=lambda,term=term))
  beta<-coef(fit)
  NaBeta<-na.omit(beta)
  if(length(NaBeta)<3) beta<-c(0,0,0)
  names(beta)<-c("beta_0","beta_2","beta_3")
  result<-list(par=beta,residual=resid(fit))
  return(result)
}

## Fit NS model - uses grid search for lambda
Fit_Nelson_Siegel<-function(rate,term){
  lambdaValues<-seq(term[1],term[length(term)],length.out = 100)
  par<-matrix(0,nrow(rate),4)
  colnames(par)<-c("beta_0","beta_1","beta_2","lambda")
  for(r in 1:nrow(rate)){
    Iteration<-matrix(0,length(lambdaValues),5)
    colnames(Iteration)<-c("beta_0","beta_1","beta_2","lambda","SSR")
    for(i in 1:length(lambdaValues)){
      lambdaStar<-optimize(fb2, interval=c(0.00001,1),term=lambdaValues[i],maximum=TRUE)$maximum
      NS_beta_star<-NS_beta_hat(rate=as.numeric(rate[r,]),term=term,lambda=lambdaStar)
      beta_star<-NS_beta_star$par
      SSR<-sum(NS_beta_star$residual^2)
      Iteration[i,]<-c(beta_star,lambdaStar,SSR)
      if(beta_star[1]<0)Iteration[i,]<-c(beta_star,lambdaStar,1e+10)
      if(beta_star[1]>30)Iteration[i,]<-c(beta_star,lambdaStar,1e+10)
    }
    row_with_min_ssr<-which.min(Iteration[,5])
    par[r,]<-Iteration[row_with_min_ssr,1:4]
  }
     
  return(par)
}

fit_NS<-Fit_Nelson_Siegel(rate=data2,term=term)
fit_NS = as.data.frame(fit_NS)
fit_NS$Date = as.Date(data[,1])
print(ggplot(fit_NS, aes(Date, beta_0)) + geom_line() +
   xlab("Date") + ylab(expression(beta[0])) +
     ggtitle(expression(paste(beta[0], " Versus Time"))))

print(ggplot(fit_NS, aes(Date, beta_1)) + geom_line() +
        xlab("Date") + ylab(expression(beta[1])) +
        ggtitle(expression(paste(beta[1], " Versus Time"))))

print(ggplot(fit_NS, aes(Date, beta_2)) + geom_line() +
        xlab("Date") + ylab(expression(beta[2])) +
        ggtitle(expression(paste(beta[2], " Versus Time"))))


fp.ns.results = "/home/admin123/Yield_Curve_Modelling/10Y_NS_Results.csv"
df.ns.results = as.data.frame(fit_NS)
write.csv(df.ns.results, fp.ns.results, row.names = FALSE)
## Code upto this point was used for ICCIDS paper
########### 
## Section 2: Fit Dynamic Nelson Siegel with Full Likelihood
###########

## Return negative log-likelihood of DNS Model when lambda is known
DNS_likelihood<-function(theta,rate,term,lambda){
  theta01<-theta[1]
  theta02<-theta[2]
  theta03<-theta[3]
  theta11<-theta[4]
  theta12<-theta[5]
  theta13<-theta[6]
  sigma1_sq<-exp(theta[7])
  sigma2_sq<-exp(theta[8])  
  
  theta0<-c(theta01,theta02,theta03)
  Z<-diag(c(theta11,theta12,theta13))
  #print(Z)
  b<-matrix(NA,nrow=nrow(rate),ncol=3)
  b[1,]<-Fit_Nelson_Siegel(rate=rate[1,],term=term)[1:3]
  #b[1,]<-c(3.7,-3.7,-4.8)
  
  phi<-matrix(NA,nrow=length(term),ncol=3)
  phi[,1]<-1
  phi[,2]<-fb1(lambda=lambda,term=term)
  phi[,3]<-fb2(lambda=lambda,term=term)
  
  m<-nrow(phi)
  Sigma_1<-sigma1_sq^2*solve(t(phi)%*%phi)

  l<-dmvnorm(rate[1,],mean=phi%*%b[1,],sigma=diag(sigma1_sq,nrow=length(term)),log=TRUE)
  
  for(t in 2:nrow(rate)){
    
    b[t,]=theta0+Z%*%b[t-1,]
    
    Rt<-Z%*%Sigma_1%*%Z+diag(sigma2_sq,nrow = 3)
    K_tilde<-diag(sigma1_sq,nrow = m)
    St<- K_tilde + phi%*%Rt%*%t(phi)
    #print(St)
    #p<-dmvnorm(b[t,],mean=b[t-1,],sigma=diag(sigma2_sq,nrow=3),log=TRUE)   
    #f<-dmvnorm(rate[t,],mean=phi%*%b[t,],sigma=diag(sigma1_sq,nrow=length(term)),log=TRUE)
    f<-dmvnorm(rate[t,],mean=phi%*%b[t,],sigma=St,log=TRUE)
     l<-l+f #+p          
  }
  
  ## prior on static parameters
  prior<-sum(dcauchy(theta,log=TRUE))
  l<-l+prior
  return(-l)
}

DNS_likelihood(theta=c(4.7,-2.3,-2.5,-0.9,0.9,0.9,0.01,0.001)
               ,rate=data2[1:10,]
               ,term=term
               ,lambda=0.35)


## This function estimates the static parameters of DNS when lambda is known
DNS_theta_hat<-function(rate,term,lambda){
  theta.init<-c(0,0,0,1,1,1,0.01,0.001)
  fit<-optim(theta.init,DNS_likelihood,rate=rate,term=term,lambda=lambda
             ,method="BFGS",control=list(maxit=1000))
  par<-fit$par
  value<-fit$value
  return(list(par=par,value=value))
}

system.time(DNS_theta_hat(rate=data2[1:30,],term=term,lambda=5))

## This function estimates the static parameters of DNS including lambda
## It uses grid search for lambda
DNS<-function(rate,term){
  
  lambdaValues<-seq(term[1],term[length(term)],length.out = 30)
  Iteration<-matrix(0,length(lambdaValues),10)
  for(i in 1:length(lambdaValues)){
    lambdaStar<-optimize(fb2, interval=c(0.01,1),term=lambdaValues[i],maximum=TRUE)$maximum
    DNS_theta_star<-DNS_theta_hat(rate=rate,term=term,lambda=lambdaStar)
    Iteration[i,1:8]<-DNS_theta_star$par
    Iteration[i,9]<-lambdaStar
    Iteration[i,10]<-DNS_theta_star$value
    row_with_min_value<-which.min(Iteration[,10])
  }  
  return(Iteration[row_with_min_value,])
}

system.time(DNS_par<-DNS(rate=data2[1:10,],term=term))

