library(dplyr)
library(ggplot2)
library(fitdistrplus)
library(logspline)
library(car)
library(ltm)
library(ggpubr)


#Hurricanes
hurricanes <- read.csv("~\\NumofHurricanes-Separated.csv")
hurricanes

hurricanes.type4 <- data.frame(hurricanes$Decade, hurricanes$Type4)
hurricanes.type5 <- data.frame(hurricanes$Decade, hurricanes$Type5)

hurricanes.type4
hurricanes.type5

ggplot(hurricanes.type4, aes(x=hurricanes.Decade, y = hurricanes.Type4))+
  geom_col(color = "darkred")

ggplot(hurricanes.type5, aes(x=hurricanes.Decade, y = hurricanes.Type5))+
  geom_col(color = "darkred")

qqPlot(hurricanes.type4$hurricanes.Type4, dist="norm", col="red")
qqPlot(hurricanes.type4$hurricanes.Type4, dist="pois", lambda=5.62, col="red")
qqPlot(hurricanes.type4$hurricanes.Type4, dist="nbinom", size=16, prob=0.72, col="red")

qqPlot(hurricanes.type5$hurricanes.Type5, dist="norm", col="red")
qqPlot(hurricanes.type5$hurricanes.Type5, dist="pois", lambda=5.62, col="red")
qqPlot(hurricanes.type5$hurricanes.Type5, dist="nbinom", size=16, prob=0.72, col="red")

#AMO
amo_monthly.10yrLP <- read.table("~\\dataset.txt", quote="\"", comment.char="")



AMO_ts <- amo_monthly.10yrLP
AMO_ts <- AMO_ts[1:146,]
names(AMO_ts)<-c("Year", c(seq(1,12)))
head(AMO_ts)
dim(AMO_ts)
Years <- AMO_ts$Year
Months <- AMO_ts[2:13]
AMO_ts_mean <- rowMeans(Months, na.rm=FALSE)
AMO_ts_mean <- data.frame(Years, AMO_ts_mean)
AMO_ts_mean_colored <- AMO_ts_mean %>%
  mutate(pos = AMO_ts_mean >=0 )


ggplot(AMO_ts_mean_colored, aes(x = Years, y = AMO_ts_mean, fill = pos ))+
  geom_col(position = "identity")

#Correlation
year.wise <- read.csv("~\\YearwiseHurricanes.csv", quote="\"", comment.char="")
year.wise <- year.wise[1:146,]

plot1 <- ggplot(year.wise, aes(x = Year, y = Type4))+
  geom_col(position = "identity")

plot1

plot2 <- ggplot(year.wise, aes(x = Year, y = Type5))+
  geom_col(position = "identity")

plot2

ALL <- data.frame(year.wise, AMO_ts_mean_colored$AMO_ts_mean)
ALL

correlation.amo.type4.pearson <- cor.test(ALL$AMO_ts_mean_colored.AMO_ts_mean, ALL$Type4)
correlation.amo.type4.pearson

correlation.amo.type5.pearson <- cor.test(ALL$AMO_ts_mean_colored.AMO_ts_mean, ALL$Type5)
correlation.amo.type5.pearson
