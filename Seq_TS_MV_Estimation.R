.libPaths("/home/admin123/R/x86_64-pc-linux-gnu-library/3.2")
library(vars)
setwd("/home/admin123/Yield_Curve_Modelling/")
fp = "Src_2_2006_to_Present_Treasury_Data.csv"
df = read.csv(fp)
df = df[order(df$Date),]
dates = as.Date(as.character(df$Date), format="%Y-%m-%d")
years = format(dates, "%Y")
df = df[,-1]
term<-c(1/12,3/12,6/12,12/12,24/12,36/12,60/12,84/12,120/12,240/12,360/12)
term.str = sapply(round(term, digits = 3), as.character)
col.names = paste("Term = ", term.str, "yrs")
vs = VARselect(df, lag.max = 30, type = "const")
cat("Optimal lag (initial) based on BIC is", vs$selection[3], "\n")



block_size = 250
index_start = 1
index_end = index_start + block_size - 1
N = nrow(df)
MV.pred.mat = matrix(0, (N-block_size), 11)

while (index_end <=  N-1) {
  df.trng = df[index_start:index_end,]
  
  if(index_start %% block_size == 0) {
    cat("Day Number ", index_start,", so determining optimal lag again!", "\n")
    vs = VARselect(df, lag.max = 30, type = "const")
    cat("Optimal lag (update) based on BIC is", vs$selection[3], "\n")
  }
  mv.fit <- VAR(df.trng,p = vs$selection[3], type = "const")
  
  if (index_start %% 50 == 0) {
    cat("Processing record:", index_start, "\n")  
  }
  
  pred.test = predict(mv.fit, n.ahead = 1)
  Mo_1 = pred.test$fcst$Mo_1[1]
  Mo_3= pred.test$fcst$Mo_3[1]
  Mo_6 = pred.test$fcst$Mo_6[1]
  Yr_1 = pred.test$fcst$Yr_1[1]
  Yr_2 = pred.test$fcst$Yr_2[1]
  Yr_3 = pred.test$fcst$Yr_3[1]
  Yr_5 = pred.test$fcst$Yr_5[1]
  Yr_7 = pred.test$fcst$Yr_7[1]
  Yr_10 = pred.test$fcst$Yr_10[1]
  Yr_20 = pred.test$fcst$Yr_20[1]
  Yr_30 = pred.test$fcst$Yr_30[1]
  yields = c(Mo_1, Mo_3, Mo_6, Yr_1, Yr_2, Yr_3, Yr_5, Yr_7, Yr_10, Yr_20, Yr_30)
  
  MV.pred.mat[index_start,] = yields
  
  index_start = index_start + 1
  index_end = index_start + block_size - 1
  

}

df.MV.est = as.data.frame(MV.pred.mat)
names(df.MV.est) = c("Mo_1", "Mo_3", "Mo_6", "Yr_1", "Yr_2", "Yr_3", "Yr_5", "Yr_7",
                     "Yr_10", "Yr_20", "Yr_30")
date.start.index = block_size + 1
df.MV.est$Date = dates[date.start.index: nrow(df)]
fp.MV.est = "10Y_MV_Estimates.csv"
write.csv(df.MV.est, fp.MV.est, row.names = FALSE)
req.cols = c("Mo_1", "Mo_3", "Mo_6", "Yr_1", "Yr_2", "Yr_3", "Yr_5", "Yr_7",
             "Yr_10", "Yr_20", "Yr_30")
fp.day500.sample = "day_500_sample.csv"
df.day500.sample = df.MV.est[500, req.cols]
df.day500.sample$method = "MVTS"
write.table(df.day500.sample, fp.day500.sample,
            sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)

fp.day1000.sample = "day_1000_sample.csv"
df.day1000.sample = df.MV.est[1000, req.cols]
df.day1000.sample$method = "MVTS"
write.table(df.day1000.sample, fp.day1000.sample,
            sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)

fp.day1500.sample = "day_1500_sample.csv"
df.day1500.sample = df.MV.est[1500, req.cols]
df.day1500.sample$method = "MVTS"
write.table(df.day1500.sample, fp.day1500.sample, sep = ",", append = TRUE, 
            row.names = FALSE, col.names = FALSE)


fp.day2000.sample = "day_2000_sample.csv"
df.day2000.sample = df.MV.est[2000, req.cols]
df.day2000.sample$method = "MVTS"
write.table(df.day2000.sample, fp.day2000.sample,
            sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)

#include actuals
fp.day500.sample = "day_500_sample.csv"
df.day500.sample = df[500, req.cols]
df.day500.sample$method = "actual"
write.table(df.day500.sample, fp.day500.sample,
            sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)

fp.day1000.sample = "day_1000_sample.csv"
df.day1000.sample = df[1000, req.cols]
df.day1000.sample$method = "actual"
write.table(df.day1000.sample, fp.day1000.sample,
            sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)

fp.day1500.sample = "day_1500_sample.csv"
df.day1500.sample = df[1500, req.cols]
df.day1500.sample$method = "actual"
write.table(df.day1500.sample, fp.day1500.sample, sep = ",", append = TRUE, 
            row.names = FALSE, col.names = FALSE)


fp.day2000.sample = "day_2000_sample.csv"
df.day2000.sample = df[2000, req.cols]
df.day2000.sample$method = "actual"
write.table(df.day2000.sample, fp.day2000.sample,
            sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)





mat.act.yields = as.matrix(df[(block_size + 1):N,])
mat.err.yields = MV.pred.mat - mat.act.yields
err.term.df = as.data.frame(mat.err.yields)
sq.err.df = as.data.frame(apply(err.term.df,2, FUN = function(x) return (x^2)))
sq.err.df$Date = dates[date.start.index: nrow(df)]
names(sq.err.df) = c("Mo_1", "Mo_3", "Mo_6", "Yr_1", "Yr_2", "Yr_3", "Yr_5", "Yr_7",
                     "Yr_10", "Yr_20", "Yr_30")
fp.err.MVTS = "10Y_Error_MVTS.csv"
write.csv(sq.err.df, fp.err.MVTS, row.names = FALSE)
rmse.term.err = apply(mat.err.yields, 2, FUN = function(x) { sqrt(sum(x^2)/nrow(mat.err.yields))})
rmse.df = as.data.frame(rmse.term.err)

fp.MV.RMSE = "10Y_RMSE_MV_TS_Results.csv"
write.csv(rmse.df, fp.MV.RMSE, row.names = FALSE)
