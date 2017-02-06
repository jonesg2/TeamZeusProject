library(ggplot2)
library(reshape2)

## <----- Description of data source: Table of cancer survival rates of BOTH genders in 2014 ------>

## Import data for cancer stages ## 
head(CSV_2014 <- read.csv("Z:/Team Zeus Project/Stages/Cancer_Stage_14.csv")) ## 2014 Data
head(CSV_2013 <- read.csv("Z:/Team Zeus Project/Stages/Cancer_Stage_13.csv")) ## 2013 Data
head(CSV_2012 <- read.csv("Z:/Team Zeus Project/Stages/Cancer_Stage_12.csv")) ## 2012 Data

## Melt data frames & delete NA rows
#2014
MELT_2014 <- melt(CSV_2014)
DF_2014 <- subset(MELT_2014, value!="NA")
DF_2014["Year"] <- NA ## Simply puts NA into column when column is created
DF_2014$Year <- 2014  ## Inserting year into column ready for merging the three datasets
DF_2014

#2013
MELT_2013 <- melt(CSV_2013)
DF_2013 <- subset(MELT_2013, value!="NA")
DF_2013["Year"] <- NA ## Simply puts NA into column when column is created
DF_2013$Year <- 2013  ## Inserting year into column ready for merging the three datasets
DF_2013

#2012
MELT_2012 <- melt(CSV_2012)
DF_2012 <- subset(MELT_2012, value!="NA")
DF_2012["Year"] <- NA ## Simply puts NA into column when column is created
DF_2012$Year <- 2012  ## Inserting year into column ready for merging the three datasets
DF_2012

# Combine three datasets
DF_ALL <- rbind(DF_2012,DF_2013, DF_2014)
DF_ALL

# Aggregate
AGG_ALL <- aggregate(DF_ALL$value~(DF_ALL$Cancer.site + DF_ALL$variable), FUN=mean)
names(AGG_ALL)[1]<-paste("Cancer") 
names(AGG_ALL)[2]<-paste("Stage") 
names(AGG_ALL)[3]<-paste("Value")
AGG_ALL

# Plot graph 
ggplot(data = AGG_ALL, aes(x= AGG_ALL$Stage, y= AGG_ALL$Value, group = AGG_ALL$Cancer, colour = AGG_ALL$Cancer)) +
  geom_line() +
  geom_point( size=4, shape=21, fill="white") +
  ylim(0,105) +
  labs(x="Stage of Cancer", y="Survival Rate %", colour = "Cancer Type") +
  ggtitle("Mean Cancer Survival Rates (2012-2014)")
