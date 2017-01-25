LungCancerDeaths <-read.csv("Z:/Team Zeus Project/Data Sources/Lung cancer deaths by month and region.csv")

library(plyr)

## Turn month into number - JAY
revalue(LungCancerDeaths$Group.1, c("January" = "01")) -> aggLCD$Group.1
revalue(LungCancerDeaths$Group.1, c("February" = "02")) -> aggLCD$Group.1
revalue(LungCancerDeaths$Group.1, c("March" = "03")) -> aggLCD$Group.1
revalue(LungCancerDeaths$Group.1, c("April" = "04")) -> aggLCD$Group.1
revalue(LungCancerDeaths$Group.1, c("May" = "05")) -> aggLCD$Group.1
revalue(LungCancerDeaths$Group.1, c("June" = "06")) -> aggLCD$Group.1
revalue(LungCancerDeaths$Group.1, c("July" = "07")) -> aggLCD$Group.1
revalue(LungCancerDeaths$Group.1, c("August" = "08")) -> aggLCD$Group.1
revalue(LungCancerDeaths$Group.1, c("September" = "09")) -> aggLCD$Group.1
revalue(LungCancerDeaths$Group.1, c("October" = "10")) -> aggLCD$Group.1
revalue(LungCancerDeaths$Group.1, c("November" = "11")) -> aggLCD$Group.1
revalue(LungCancerDeaths$Group.1, c("December" = "12")) -> aggLCD$Group.1

#combine - GARETH
aggLCD$Date <- paste(aggLCD$Group.2,aggLCD$Group.1,sep="-")

aggLCD <- aggregate(LungCancerDeaths$Deaths~LungCancerDeaths$Date, FUN = sum)
aggLCD

#add date reference - GARETH
aggLCD$Date.reference<-seq(1,168,by=1)

## PLOT the Time Series - GARETH
df_Time_series<-ts(aggLCD$`LungCancerDeaths$Deaths`)
plot(df_Time_series, xlab="Time in Months", ylab="Number Of Deaths", main="Number of Lung Cancer Deaths. Jan 2001-Dec 2015")

##use simple exponential smoothing to make forecasts for the time series - ALEX
LCDseriesforecasts <- HoltWinters(df_Time_series, beta=FALSE, gamma=FALSE, l.start=1318.374)
LCDseriesforecasts
#NOTE: The output of HoltWinters() tells us that the estimated value of the alpha parameter is about 0.132
#This is close to zero, telling us that the forecasts are based on both recent and less recent observations.
#By default, HoltWinters() just makes forecasts for the same time period covered by our original time series.

##PLOT exponential smoothing against original - ALEX
plot(LCDseriesforecasts,xlab="Time in Months", ylab="Number Of Deaths", main="Number of Lung Cancer Deaths with forecast for same period")

##Install "forecast" package & "ggplot2"
install.packages("forecast")
library("forecast")
library("ggplot2")

##Forecast for next 192 months (Jan2015-Dec2030) 
LCDseriesforecasts2 <- forecast.HoltWinters(LCDseriesforecasts, h=192)
##Plot forecast2 against original 
plot(LCDseriesforecasts2,xlab="Time in Months", ylab="Number Of Deaths", main="Number of Lung Cancer Deaths Jan 2001-Dec 2030", col="blue")
#Here the forecasts for Jan2015-Dec2030 are plotted as a blue line,
#the 80% prediction interval as the inner shaded area, and the 95% prediction interval as the outer shaded area


