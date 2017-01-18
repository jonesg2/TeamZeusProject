#Import Data set
DR2001to2012 <- read.csv("Z:/Team Zeus Project/DR2001to2012.csv")

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

