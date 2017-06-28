.libPaths("/home/admin123/R/x86_64-pc-linux-gnu-library/3.2")
library(ggplot2)
library(tidyr)
setwd("/home/admin123/Yield_Curve_Modelling/")
fp = "Src_2_2006_to_Present_Treasury_Data.csv"
df = read.csv(fp)
df = df[order(df$Date),]
dates = as.Date(as.character(df$Date), format="%Y-%m-%d")
dates = format(dates, format="%b %d, %Y")
sample.dates = c(dates[500], dates[1000], dates[1500], dates[2000])
df = NULL

fp.500 = "day_500_sample.csv"
df.500 = read.csv(fp.500, header = FALSE)
col.names = c("Mo_1",  "Mo_3",  "Mo_6",  "Yr_1",  "Yr_2",
              "Yr_3",  "Yr_5",  "Yr_7",  "Yr_10", "Yr_20", "Yr_30", "method")
names(df.500) = col.names
d500.title = paste("Estimates for", sample.dates[1])
df.m500 = gather(df.500, term, yield, -method)
df.m500$term = factor(df.m500$term, ordered = TRUE, levels = c("Mo_1",  "Mo_3",  "Mo_6",  "Yr_1",  "Yr_2",
                                                               "Yr_3",  "Yr_5",  "Yr_7",  "Yr_10", "Yr_20", "Yr_30"))
p <-ggplot(df.m500, aes(term, y = yield, color = method, group = method)) +
  stat_smooth(se=FALSE)+
  ggtitle(d500.title) + labs(x="Term",y="Yield (%)")
print(p)
fp.save = "/home/admin123/Yield_Curve_Modelling/Conference/plots/d500_sample.png"
ggsave(fp.save, p)
fp.1000 = "day_1000_sample.csv"
df.1000 = read.csv(fp.1000, header = FALSE)
col.names = c("Mo_1",  "Mo_3",  "Mo_6",  "Yr_1",  "Yr_2",
              "Yr_3",  "Yr_5",  "Yr_7",  "Yr_10", "Yr_20", "Yr_30", "method")
names(df.1000) = col.names

df.m1000 = gather(df.1000, term, yield, -method)
d1000.title = paste("Estimates for", sample.dates[2])
df.m1000$term = factor(df.m1000$term, ordered = TRUE)
levels(df.m1000$term) = c("Mo_1",  "Mo_3",  "Mo_6",  "Yr_1",  "Yr_2",
                         "Yr_3",  "Yr_5",  "Yr_7",  "Yr_10", "Yr_20", "Yr_30")

p <-ggplot(df.m1000, aes(term, y = yield, color = method, group = method)) +
  stat_smooth(se= FALSE) +
  ggtitle(d1000.title) + labs(x="Term",y="Yield (%)")
print(p)
fp.save = "/home/admin123/Yield_Curve_Modelling/Conference/plots/d1000_sample.png"
ggsave(fp.save, p)

fp.1500 = "day_1500_sample.csv"
df.1500 = read.csv(fp.1500, header = FALSE)
col.names = c("Mo_1",  "Mo_3",  "Mo_6",  "Yr_1",  "Yr_2",
              "Yr_3",  "Yr_5",  "Yr_7",  "Yr_10", "Yr_20", "Yr_30", "method")
names(df.1500) = col.names

d1500.title = paste("Estimates for", sample.dates[3])
df.m1500 = gather(df.1500, term, yield, -method)
df.m1500$term = factor(df.m1500$term, ordered = TRUE)
levels(df.m1500$term) = c("Mo_1",  "Mo_3",  "Mo_6",  "Yr_1",  "Yr_2",
                          "Yr_3",  "Yr_5",  "Yr_7",  "Yr_10", "Yr_20", "Yr_30")
p <-ggplot(df.m1500, aes(term, y = yield, color = method, group = method)) +
  stat_smooth(se = FALSE)+
  ggtitle(d1500.title) + labs(x="Term",y="Yield (%)")
print(p)
fp.save = "/home/admin123/Yield_Curve_Modelling/Conference/plots/d1500_sample.png"
ggsave(fp.save, p)
fp.2000 = "day_2000_sample.csv"
df.2000 = read.csv(fp.2000, header = FALSE)
col.names = c("Mo_1",  "Mo_3",  "Mo_6",  "Yr_1",  "Yr_2",
              "Yr_3",  "Yr_5",  "Yr_7",  "Yr_10", "Yr_20", "Yr_30", "method")
names(df.2000) = col.names
d2000.title = paste("Estimates for", sample.dates[4])
df.m2000 = gather(df.2000, term, yield, -method)
df.m2000$term = factor(df.m2000$term, ordered = TRUE)
levels(df.m2000$term) = c("Mo_1",  "Mo_3",  "Mo_6",  "Yr_1",  "Yr_2",
                          "Yr_3",  "Yr_5",  "Yr_7",  "Yr_10", "Yr_20", "Yr_30", "method")
p <-ggplot(df.m2000, aes(term, y = yield, color = method, group = method)) + 
  stat_smooth(se= FALSE, position = "jitter") +
  ggtitle(d2000.title) + labs(x="Term",y="Yield (%)")
print(p)
fp.save = "/home/admin123/Yield_Curve_Modelling/Conference/plots/d2000_sample.png"
#ggsave(fp.save, p)