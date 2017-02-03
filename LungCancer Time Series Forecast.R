### Abbreviations ###
# LCD = Lung Cancer Deaths
# AG_LCD = Aggregate on Lung Cancer Deaths
# TS_LCD = Time Series on Lung Cancer Deaths
# FS_LCD = Forcast Series on Luncg Cancer Deaths
# F24_LCD = 24 months forcast on Lung Cancer Deaths

LCD <-read.csv("Z:/Team Zeus Project/Data Sources/Lung cancer deaths by month and region.csv")

library(plyr)

## Turn month into number - JAY
revalue(LCD$Month, c("January" = "01")) -> LCD$Month
revalue(LCD$Month, c("February" = "02")) -> LCD$Month
revalue(LCD$Month, c("March" = "03")) -> LCD$Month
revalue(LCD$Month, c("April" = "04")) -> LCD$Month
revalue(LCD$Month, c("May" = "05")) -> LCD$Month
revalue(LCD$Month, c("June" = "06")) -> LCD$Month
revalue(LCD$Month, c("July" = "07")) -> LCD$Month
revalue(LCD$Month, c("August" = "08")) -> LCD$Month
revalue(LCD$Month, c("September" = "09")) -> LCD$Month
revalue(LCD$Month, c("October" = "10")) -> LCD$Month
revalue(LCD$Month, c("November" = "11")) -> LCD$Month
revalue(LCD$Month, c("December" = "12")) -> LCD$Month

#combine - GARETH
LCD$Date <- paste(LCD$Year,LCD$Month,sep="-")

#Aggregate by Date to remove region split
AG_LCD <- aggregate(LCD$Deaths~LCD$Date, FUN = sum)
AG_LCD

#add date reference - GARETH
AG_LCD$Date.reference<-seq(1,168,by=1)

## PLOT the Time Series - GARETH
TS_LCD<-ts(AG_LCD$`LCD$Deaths`)
plot(TS_LCD, xlab="Time in Months", ylab="Number Of Deaths", main="Number of Lung Cancer Deaths. Jan 2001-Dec 2015")

##use simple exponential smoothing to make forecasts for the time series - ALEX
FS_LCD <- HoltWinters(TS_LCD, beta=FALSE, gamma=FALSE, l.start=1318.374)
FS_LCD
#NOTE: The output of HoltWinters() tells us that the estimated value of the alpha parameter is about 0.132
#This is close to zero, telling us that the forecasts are based on both recent and less recent observations.
#By default, HoltWinters() just makes forecasts for the same time period covered by our original time series.

##PLOT exponential smoothing against original - ALEX
plot(FS_LCD,xlab="Time in Months", ylab="Number Of Deaths", main="Number of Lung Cancer Deaths with forecast for same period")

##Install "forecast" package & "ggplot2"
install.packages("forecast")
library("forecast")
library("ggplot2")

##Forecast for next 24 months (Jan2015-Dec2016) 
F24_LCD <- forecast.HoltWinters(FS_LCD, h=24)
##Plot forecast2 against original 
plot(F24_LCD,xlab="Time in Months", ylab="Number Of Deaths", main="Number of Lung Cancer Deaths Jan 2001-Dec 2030", col="blue")
#Here the forecasts for Jan2015-Dec2016 are plotted as a blue line,
#the 80% prediction interval as the inner shaded area, and the 95% prediction interval as the outer shaded area
