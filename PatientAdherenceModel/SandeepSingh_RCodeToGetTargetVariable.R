rm(list=ls())
setwd("I:/INSOFE_Singh1/Insofe_Project")
PatientData<-read.csv("DataPatient_Adherence_Batch24.csv", header = TRUE,sep = )
str(PatientData)
summary(PatientData)

#To remove fre>1 of actgpi from original data
#Removed all the Unique records based on ActGpi
tf1 <- table(PatientData$ActGPI)
#tf1 <- as.data.frame(tf1)
#df[df$col1 %in% names(tdf)[tdf>1],]

PatientData1<-PatientData[PatientData$ActGPI %in% names(tf1)[tf1>1],]

#COnverted date(factor) to date format
Purchased_Date <- as.Date(PatientData$Date, format = "%d-%m-%Y")
PatientData$Date <- NULL 
PatientData1 <- cbind(PatientData, Purchased_Date)

#Arranging the dates in ascending order
PatientData2 <- PatientData1[order(as.Date(PatientData1$Purchased_Date, format="%d-%m-%Y")),]
#write.csv(PatientData2,"Target2.csv")

PatientData3 <- PatientData2[order((PatientData2$PatientID)),]
#write.csv(PatientData3,"Target3.csv")

PatientData4 <- PatientData3[order((PatientData3$ActGPI)),]
#write.csv(PatientData4,"Target4.csv")

PatientData5 <- PatientData4[order((PatientData4$Medication)),]
#write.csv(PatientData5,"Target5.csv")

#Adding a column Duedata
Duedate <- PatientData5$Purchased_Date + PatientData5$For_How_Many_Days
Duedate <- data.frame(Duedate)
PatientData6 <- cbind(PatientData5, Duedate)
#write.csv(PatientData6,"Target6.csv")

#Concatenating ID and Medication
PatientData6$IdMedication <- paste(PatientData6$PatientID,"-",PatientData6$Medication)
#write.csv(PatientData6,"Target7.csv")
PatientData6$PatientID <- NULL
PatientData6$Medication <- NULL
#PatientData6$ActGPI <- NULL
FinalPatientData <- PatientData6[c(14,1:13)]
#write.csv(FinalPatientData,"Target7.csv")


#Getting the target 
FinalPatientData$NoOfDaysDue <- 0
for(i in 1:nrow(FinalPatientData) )
{
  
  if(FinalPatientData$IdMedication[i+1] == FinalPatientData$IdMedication[i])
  {
    FinalPatientData$NoOfDaysDue[i+1] <- as.data.frame(FinalPatientData$Duedate[i] - FinalPatientData$Purchased_Date[i+1])
  }
  else
  {
    FinalPatientData$NoOfDaysDue[i+1] <- 0
  }
  
}
#write.csv(FinalPatientData, "FPD.csv")

#sum(is.na(FinalPatientData$NoOfDaysDue))


#1 - adherent
#0 - non adherent
for(i in 1:nrow(FinalPatientData))
{
  if(FinalPatientData$NoOfDaysDue[i]>=-3)
  {
    FinalPatientData$Category[i] <- '1' 
  }
  else
  {
    FinalPatientData$Category[i] <- '0'
  }
}

str(FinalPatientData)
FinalPatientData$NoOfDaysDue <- as.integer(FinalPatientData$NoOfDaysDue)
FinalPatientData$Category <- as.integer(FinalPatientData$Category)

write.csv(FinalPatientData,"Final.csv")


  
  
  
  
  
  