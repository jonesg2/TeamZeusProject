DR2001to2012 <- read.csv("Z:/Team Zeus Project/Data Sources/DR2001to2012.csv")

#Removal of Age 75+ results
DR2001to2012.age<-DR2001to2012[DR2001to2012$AGE != "75-79", ]
DR2001to2012.age<-DR2001to2012[DR2001to2012$AGE != "80-84", ]
DR2001to2012.age<-DR2001to2012[DR2001to2012$AGE != "85+  ", ]

#Convert NDTHS to numeric removing commer spacing
DR2001to2012.age$NDTHS <-as.numeric(as.character(gsub(",","",DR2001to2012.age$NDTHS)))

#Subset the data for Number of Deaths per year
DR2001to2012.agelungYRNDTHS<- subset(DR2001to2012.age, select = c("YR", "NDTHS"))

#Aggregate
AggregateDR2001to2012.agelungYRNDTHS<-aggregate(DR2001to2012.agelungYRNDTHS$NDTHS, by=list(DR2001to2012.agelungYRNDTHS$YR), FUN=sum)
summary(AggregateDR2001to2012.agelungYRNDTHS)

#Plot of deaths by year
plot(AggregateDR2001to2012.agelungYRNDTHS$Group.1, AggregateDR2001to2012.agelungYRNDTHS$x,
     xlab = "Year",
     ylab = "Number of Deaths",
    main = "Total Number of Deaths")

#rename columns to facilitate merge
colnames(AggregateDR2001to2012.agelungYRNDTHS)<-c("YEAR", "NDTHS")
