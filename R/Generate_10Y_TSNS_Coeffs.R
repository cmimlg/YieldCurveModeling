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




