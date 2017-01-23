#Import Data set
DR2001to2012 <- read.csv("Z:/Team Zeus Project/Data Sources/DR2001to2012.csv") 
#please check which letter your shared drive is mapped to. If it is not Z then you will need to change the above code to the letter your have mapped yours to. 

#Data currently covers all causes of death but we want just data concerned with lung cancer which is ICD-10 codes c340 - c349 
#To subset the data set i created individual sections for each ICD-10 code relating to Lung Cancer and then merge the results into one data frame.

DR2001to2012.subC340 <- DR2001to2012[DR2001to2012$ICD.10 == "C340", ]
DR2001to2012.subC341 <- DR2001to2012[DR2001to2012$ICD.10 == "C341", ]
DR2001to2012.subC342 <- DR2001to2012[DR2001to2012$ICD.10 == "C342", ]
DR2001to2012.subC343 <- DR2001to2012[DR2001to2012$ICD.10 == "C343", ]
DR2001to2012.subC348 <- DR2001to2012[DR2001to2012$ICD.10 == "C348", ]
DR2001to2012.subC349 <- DR2001to2012[DR2001to2012$ICD.10 == "C349", ]

#Merge data set to one data frame
DR2001to2012.subClung <- rbind(DR2001to2012.subC340, 
                               DR2001to2012.subC341,
                               DR2001to2012.subC342,
                               DR2001to2012.subC343,
                               DR2001to2012.subC348,
                               DR2001to2012.subC349)

#removing the above individual subsets as no longer required
rm(DR2001to2012.subC340, 
      DR2001to2012.subC341,
      DR2001to2012.subC342,
      DR2001to2012.subC343,
      DR2001to2012.subC348,
      DR2001to2012.subC349)


#Backup copy before conversion
#DR2001to2012.subClung2<-DR2001to2012.subClung

#Next step was to restrict the age ranges as we need ages less than 75 years old and current data contains information on deaths up to ages 85+.
#Initial steps was to use the above method but the data type is currently a factor which will not subset.
#The next step changes the data type to a character which will allow a subset.

#normalise class of age to subset

DR2001to2012.subClung$AGE <-as.character(DR2001to2012.subClung$AGE)

#Removal of Age 75+ results by selecting a subset the data not including the unwanted ages ranges. i have repeated the code for the 3 ages ranges not wanted so the result will only include the ages wanted.
DR2001to2001.subclungagerestricted<-DR2001to2012.subClung[DR2001to2012.subClung$AGE != "75-79", ]
DR2001to2001.subclungagerestricted<-DR2001to2012.subClung[DR2001to2012.subClung$AGE != "80-84", ]
DR2001to2001.subclungagerestricted<-DR2001to2012.subClung[DR2001to2012.subClung$AGE != "85+  ", ]
summary(DR2001to2001.subclungagerestricted)

#Convert NDTHS to numeric removing commer spacing
DR2001to2001.subclungagerestricted$NDTHS <-as.numeric(as.character(gsub(",","",DR2001to2001.subclungagerestricted$NDTHS)))

#Remove column X as duplicate   of index
DR2001to2001.subclungagerestricted<-DR2001to2001.subclungagerestricted[-1]

#Number of lung cancer deaths per year LCD=Lung cancer Deaths
LCD.byyear<-aggregate(DR2001to2001.subclungagerestricted$NDTHS~DR2001to2001.subclungagerestricted$YR, FUN=sum)
colnames(LCD.byyear)<-c("YEAR", "NDTHSLUNG")
LCD.byyear

#creating an age restricted data set for all registered deaths.
#Removal of Age 75+ results
DR2001to2012.age<-DR2001to2012[DR2001to2012$AGE != "75-79", ]
DR2001to2012.age<-DR2001to2012[DR2001to2012$AGE != "80-84", ]
DR2001to2012.age<-DR2001to2012[DR2001to2012$AGE != "85+  ", ]

#Convert NDTHS to numeric removing commer spacing
DR2001to2012.age$NDTHS <-as.numeric(as.character(gsub(",","",DR2001to2012.age$NDTHS)))

#Subset the data for Number of Deaths per year
#DR2001to2012.agelungYRNDTHS<- subset(DR2001to2012.age, select = c("YR", "NDTHS"))

#Number of premature deaths per year(all causes)
TD.byyear<-aggregate(DR2001to2012.age$NDTHS~DR2001to2012.age$YR, FUN=sum)
colnames(TD.byyear)<-c("YEAR", "TotalNDTHS")
TD.byyear

#merge total deaths and lung cancer deaths
DR2001to2012.merge<-merge(LCD.byyear, TD.byyear, by="YEAR")
head(DR2001to2012.merge)

#Adding proportion column
proportionofdeaths<- DR2001to2012.merge$NDTHSLUNG / DR2001to2012.merge$TotalNDTHS
DR2001to2012.merge$PROPDTHS<-proportionofdeaths
head(DR2001to2012.merge)

#Import Populations Data set
Population2001to2015 <- read.csv("Z:/Team Zeus Project/Data Sources/Population2001to2015.csv")
#Please check which letter your shared drive is mapped to. If it is not Z then you will need to change the above code to the letter your have mapped yours to. 

#Merge population stats with previous aggregate
DR2001to2012.merge<- merge(DR2001to2012.merge, Population2001to2015, by = "YEAR")

#Add proportion pf population column
proportionofpopulation<- DR2001to2012.merge$NDTHSLUNG / DR2001to2012.merge$POP
DR2001to2012.merge$PROPPOP<-proportionofpopulation
head(DR2001to2012.merge)

#The following to be worked on in GGPLOT
#Plot LCD.byyear TOTAL NUMBER OF PREMATURE DEATHS BY LUNG CANCER
#Plot WHAT PROPORTION OF DEATHS REGISTERED AS LUNG CANCER
#plot total deaths by year
#Plot Lung Cancer deaths as proportion of populatoin by year
