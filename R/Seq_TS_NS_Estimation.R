.libPaths("/home/admin123/R/x86_64-pc-linux-gnu-library/3.2")
library(vars)
library(ggplot2)
setwd("~/Yield_Curve_Modelling")
fp = "10Y_NS_Results.csv"
df = read.csv(fp)
df = df[order(df$Date),]
dates = as.Date(as.character(df$Date), format="%Y-%m-%d")
years = format(dates, "%Y")
req.cols = setdiff(names(df), "Date")
df = df[, req.cols]

term<-c(1/12,3/12,6/12,12/12,24/12,36/12,60/12,84/12,120/12,240/12,360/12)
term.str = sapply(round(term, digits = 3), as.character)
col.names = paste("Term = ", term.str, "yrs")



calc_yield = function(tau, beta_0, beta_1, beta_2, lambda) {
  term_1 = ((1 -exp(-(lambda * tau)))/(lambda * tau))* beta_1
  term_2 = (((1 -exp(-(lambda * tau)))/(lambda * tau)) - exp(-(lambda*tau)))* beta_2
  y_tau = beta_0 + term_1 + term_2
  
  return(y_tau)
}

vs = VARselect(df, lag.max = 30, type = "const")
cat("Optimal lag (initial) based on BIC is", vs$selection[3], "\n")

block_size = 250
index_start = 1
index_end = index_start + block_size - 1
N = nrow(df)
NS.pred.mat = matrix(0, (N - block_size), 11)

while (index_end <=  N-1) {
  df.trng = df[index_start:index_end,]
  if(index_start %% block_size == 0) {
    cat("Day Number ", index_start,", so determining optimal lag again!", "\n")
    vs = VARselect(df, lag.max = 30, type = "const")
    cat("Optimal lag (update) based on BIC is", vs$selection[3], "\n")
  }
  ns.fit <- VAR(df.trng, p = vs$selection[3], type = "const")
  pred.test = predict(ns.fit, n.ahead = 1)
  beta_0 = pred.test$fcst$beta_0[1]
  beta_1 = pred.test$fcst$beta_1[1]
  beta_2 = pred.test$fcst$beta_2[1]
  lambda = pred.test$fcst$lambda[1]
  yields = sapply(term, calc_yield, beta_0, beta_1, beta_2, lambda)
 
  NS.pred.mat[index_start,] = yields
  
  index_start = index_start + 1
  index_end = index_start + block_size - 1
}


df.NS.est = as.data.frame(NS.pred.mat)
names(df.NS.est) = c("Mo_1", "Mo_3", "Mo_6", "Yr_1", "Yr_2", "Yr_3", "Yr_5", "Yr_7",
                     "Yr_10", "Yr_20", "Yr_30")
date.start.index = block_size + 1
df.NS.est$Date = dates[date.start.index: nrow(df)]
fp.NS.est = "10Y_NS_Estimates.csv"
write.csv(df.NS.est, fp.NS.est, row.names = FALSE)
# req.cols = c("Mo_1", "Mo_3", "Mo_6", "Yr_1", "Yr_2", "Yr_3", "Yr_5", "Yr_7",
#              "Yr_10", "Yr_20", "Yr_30")
# 
# fp.day500.sample = "day_500_sample.csv"
# df.day500.sample = df.NS.est[500,req.cols]
# df.day500.sample$method = "TSNS"
# write.table(df.day500.sample, fp.day500.sample,
#             sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
# 
# fp.day1000.sample = "day_1000_sample.csv"
# df.day1000.sample = df.NS.est[1000,req.cols]
# df.day1000.sample$method = "TSNS"
# write.table(df.day1000.sample, fp.day1000.sample,
#             sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
# 
# fp.day1500.sample = "day_1500_sample.csv"
# df.day1500.sample = df.NS.est[1500,req.cols]
# df.day1500.sample$method = "TSNS"
# write.table(df.day1500.sample, fp.day1500.sample, sep = ",", append = TRUE, 
#             row.names = FALSE, col.names = FALSE)
# 
# 
# fp.day2000.sample = "day_2000_sample.csv"
# df.day2000.sample = df.NS.est[2000,req.cols]
# df.day2000.sample$method = "TSNS"
# write.table(df.day2000.sample, fp.day2000.sample,
#             sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
# 
# 
fp = "Src_2_2006_to_Present_Treasury_Data.csv"
df.td = read.csv(fp)
df.td = df.td[order(df.td$Date),]
req.cols = setdiff(names(df.td), "Date")
df.td = df.td[, req.cols]


mat.act.yields = as.matrix(df.td[(block_size + 1):nrow(df.td),])
mat.err.yields = NS.pred.mat - mat.act.yields
err.term.df = as.data.frame(mat.err.yields)
sq.err.df = as.data.frame(apply(err.term.df,2, FUN = function(x) return (x^2)))
sq.err.df$Date = dates[date.start.index: nrow(df.td)]
names(sq.err.df) = c("Mo_1", "Mo_3", "Mo_6", "Yr_1", "Yr_2", "Yr_3", "Yr_5", "Yr_7",
                     "Yr_10", "Yr_20", "Yr_30")
# fp.err.MVTS = "10Y_Error_TSNS.csv"
# write.csv(sq.err.df, fp.err.MVTS, row.names = FALSE)
rmse.term.err = apply(mat.err.yields, 2, FUN = function(x) { sqrt(sum(x^2)/nrow(mat.err.yields))})
rmse.df = as.data.frame(rmse.term.err)

fp.MV.RMSE = "10Y_RMSE_TSNS_Results.csv"
write.csv(rmse.df, fp.MV.RMSE, row.names = FALSE)