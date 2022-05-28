.libPaths("/home/admin123/R/x86_64-pc-linux-gnu-library/3.2")
library(ustyc)
library(dplyr)
df = getYieldCurve()
df.data = df$df
fp = "Yield_Curve_Data_From_1990.csv"
setwd("~/Yield_Curve_Modelling")
write.csv(df.data, fp, row.names = FALSE)
df.clean = df.data
df.clean = na.omit(df.data)
req.cols = setdiff(names(df.clean), "BC_30YEARDISPLAY")
df.clean = df.clean[req.cols]
names(df.clean) = c("Mo_1", "Mo_3", "Mo_6", "Yr_1", "Yr_2", "Yr_3", "Yr_5", "Yr_7",
                     "Yr_10", "Yr_20", "Yr_30")
dates = rownames(df.clean)
df.clean$Date = as.Date(dates, format="%Y-%m-%d")
df.subset = filter(df.clean, Date >= "2006-01-01")
ord.cols = c("Date", "Mo_1", "Mo_3", "Mo_6", "Yr_1", "Yr_2", "Yr_3", "Yr_5", "Yr_7",
             "Yr_10", "Yr_20", "Yr_30")
df.subset = df.subset[ord.cols]
fp.2 = "Src_2_2006_to_Present_Treasury_Data.csv"
write.csv(df.subset, fp.2, row.names = FALSE)