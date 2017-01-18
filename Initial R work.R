#Import Data set
DR2001to2012 <- read.csv("Z:/Team Zeus Project/Data Sources/DR2001to2012.csv") 
#please check which letter your shared drive is mapped to. If it is not Z then you will need to change the above code to the letter your have mapped yours to. 

#create data frame
DFDR2001to2012<-data.frame(DR2001to2012)

#Subset the data set by ICD-10 codes relating to Lung Cancer
DR2001to2012.subC340 <- DFDR2001to2012[DFDR2001to2012$ICD.10 == "C340", ]
DR2001to2012.subC341 <- DFDR2001to2012[DFDR2001to2012$ICD.10 == "C341", ]
DR2001to2012.subC342 <- DFDR2001to2012[DFDR2001to2012$ICD.10 == "C342", ]
DR2001to2012.subC343 <- DFDR2001to2012[DFDR2001to2012$ICD.10 == "C343", ]
DR2001to2012.subC348 <- DFDR2001to2012[DFDR2001to2012$ICD.10 == "C348", ]
DR2001to2012.subC349 <- DFDR2001to2012[DFDR2001to2012$ICD.10 == "C349", ]

#Merge data set to one data frame
DR2001to2012.subClung <- rbind(DR2001to2012.subC340, 
                               DR2001to2012.subC341,
                               DR2001to2012.subC342,
                               DR2001to2012.subC343,
                               DR2001to2012.subC348,
                               DR2001to2012.subC349
)
#Backup copy before conversion
DR2001to2012.subClung2<-DR2001to2012.subClung

#normalise class of age to subset
DR2001to2012.subClung$AGE <-as.character(DR2001to2012.subClung$AGE)

#Removal of Age 75+ results
DR2001to2012.subClung3<-DR2001to2012.subClung[DR2001to2012.subClung$AGE != "75-79", ]
DR2001to2012.subClung3<-DR2001to2012.subClung3[DR2001to2012.subClung3$AGE != "80-84", ]
DR2001to2012.subClung3<-DR2001to2012.subClung3[DR2001to2012.subClung3$AGE != "85+  ", ]
summary(DR2001to2012.subClung3)

#rename data set for reference
DR2001to2001.subclungagerestricted<-DR2001to2012.subClung3

#Convert NDTHS to numeric removing commer spacing
DR2001to2001.subclungagerestricted$NDTHS <-as.numeric(as.character(gsub(",","",DR2001to2001.subclungagerestricted$NDTHS)))

#Subset the data for Number of Deaths per year
DR2001to2021.subClungYRNDTHS<- subset(DR2001to2001.subclungagerestricted, select = c("YR", "NDTHS"))

#Aggregate
summary(DR2001to2021.subClungYRNDTHS)
AggregateDR2001to2021.subClungYRNDTHS<-aggregate(DR2001to2021.subClungYRNDTHS$NDTHS, by=list(DR2001to2021.subClungYRNDTHS$YR), FUN=sum)
summary(AggregateDR2001to2021.subClungYRNDTHS)

#Plot of deaths by year
plot(AggregateDR2001to2021.subClungYRNDTHS$Group.1, AggregateDR2001to2021.subClungYRNDTHS$x,
     xlab = "Year",
     ylab = "Number of Deaths",
     main = "Number of Deaths where cause of death is registered as Lung Cancer")

colnames(AggregateDR2001to2021.subClungYRNDTHS)<-c("YEAR", "NDTHS")

#--PLOTTING TOTAL NUMBER OF PREMATURE DEATHS

#creating an age restricted data set for all registered deaths.
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


#---PLOTTING WHAT PROPORTION OF DEATHS REGISTERED AS LUNG CANCER

#merge total deaths and lung cancer deaths
DR2001to2012.merge1<-merge(AggregateDR2001to2021.subClungYRNDTHS, AggregateDR2001to2012.agelungYRNDTHS, by="YEAR")
head(DR2001to2012.merge1)

colnames(DR2001to2012.merge1)<-c("YEAR", "NDTHSLUNG", "NDTHS")

#Adding proportion column
proportionofdeaths<- DR2001to2012.merge1$NDTHSLUNG / DR2001to2012.merge1$NDTHS
DR2001to2012.merge1$PROPDTHS<-proportionofdeaths

#Plot of deaths by year
plot(DR2001to2012.merge1$YEAR, DR2001to2012.merge1$PROPDTHS,
     xlab = "Year",
     ylab = "Proportion of Deaths",
     main = "Proportion of Deaths with cause of death is Lung Cancer")

#---PLOTTING LUNG CANCER DEATHS AS PROPORTION OF POPULATION

#Import Populations Data set
Population2001to2015 <- read.csv("Z:/Team Zeus Project/Data Sources/Population2001to2015.csv")
#Please check which letter your shared drive is mapped to. If it is not Z then you will need to change the above code to the letter your have mapped yours to. 

#Merge population stats with previous aggregate
DR2001to2012.merge2<- merge(DR2001to2012.merge1, Population2001to2015, by = "YEAR")

# add proportion pf population column
proportionofpopulation<- DR2001to2012.merge2$NDTHSLUNG / DR2001to2012.merge2$POP
DR2001to2012.merge2$PROPPOP<-proportionofpopulation

#Plot of deaths by year
plot(DR2001to2012.merge2$YEAR, DR2001to2012.merge2$PROPPOP,
     xlab = "Year",
     ylab = "Proportion ",
     main = "Proportion of population with cause of death is Lung Cancer ")
