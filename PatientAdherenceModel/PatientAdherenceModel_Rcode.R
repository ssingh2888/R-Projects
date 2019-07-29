rm(list=ls())
setwd("I:/INSOFE_Singh1/Insofe_Project")
PatientData<-read.csv("DataPatient_Adherence_Batch24.csv", header = TRUE,sep = )
str(PatientData)
summary(PatientData)
summary(PatientData$Date)

#Purchased_Date <- as.factor(PatientData$Date)
Purchased_Date <- as.Date(PatientData$Date, format = "%d-%m-%Y")
Purchased_Date <- data.frame(Purchased_Date)
str(Purchased_Date)
str(PatientData$For_How_Many_Days)

#PatientData1 == with purchased date in date format
PatientData$Date <- NULL 
PatientData1 <- cbind(PatientData, Purchased_Date)

#TO add purchased_date and for_how_many_days
duedate <- PatientData1$Purchased_Date + PatientData1$For_How_Many_Days
duedate <- data.frame(duedate)

#PatientData2 == with duedate
PatientData2 <- cbind(PatientData1, duedate)
#write.csv(PatientData2,"1.csv")


#we cannot sort the date directly.
#date has to be sorted wrt ActGpi
PurchasedDateInAscOrder <- PatientData2[order(as.Date(PatientData2$Purchased_Date, format="%d-%m-%Y")),]
write.csv(PurchasedDateInAscOrder,"3.csv")


#second purchased date of tablet 281 - due date of that particular tab gives adherent or not.
#ActGPI_Unique <- unique(PatientData$ActGPI)
#data.frame(ActGPI_Unique)

library(plyr)
arrange(PatientData,desc(PatientData$ActGpi))

#Checking for null values
count<- sum(is.na(PurchasedDateInAscOrder))
count

#Extracting record of 1001 patient
patientId_1001 <- subset(PurchasedDateInAscOrder, PurchasedDateInAscOrder$PatientID=="1001") 
patientId_1001 <- data.frame(patientId_1001)
write.csv(patientId_1001,"patientId_1001.csv")
duplicated(patientId_1001$ActGPI)
unique(patientId_1001$ActGPI)
patientId_1001_count <- table(patientId_1001$ActGPI)
patientId_1001_count <- data.frame(patientId_1001_count)
Frequency_1_1001 <- subset(patientId_1001_count, patientId_1001_count$Freq>1)
str(patientId_1001$ActGPI)

#
ToRemoveTabFreqOnce <- table(PurchasedDateInAscOrder$ActGPI)
ToRemoveTabFreqOnce <- data.frame(ToRemoveTabFreqOnce)
write.csv(ToRemoveTabFreqOnce,"FrequencyOfEachDrug.csv")

dataWithFreq <- cbind(PurchasedDateInAscOrder,ToRemoveTabFreqOnce)

table(ToRemoveTabFreqOnce$Freq>1)
ToRemoveTabFreqOnceFromData <- subset(PurchasedDateInAscOrder,ToRemoveTabFreqOnce$Freq>1)
ToRemoveTabFreqOnceFromData <- data.frame(ToRemoveTabFreqOnceFromData)
table(ToRemoveTabFreqOnceFromData$ActGPI)
table(PurchasedDateInAscOrder$PatientID)


#write a forloop and give condition if 2nd purchase date-due date >0 then non-adherent
#There are 121 Unique values in ActGpi column

for(i in ActGpi Unique values)
{
  if(2nd purch date- duedate >0)
  {
    cat("Non adhere")
  }
  else
  {
    cat("adhere")
  }
}













#PatientID-1001 to 1059 
#ACTGPI - outliers

boxplot(PatientData$ActGPI)
hist(PatientData$ActGPI)
new <- subset(PatientData, PatientData$ActGPI<550)
summary(new)


#AmountPaid - Outliers
new1 <- subset(new,new$AmountPaid<150)
summary(new1)
par(mfrow=c(1,2))
hist(new$ActGPI)
hist(PatientData$ActGPI)
boxplot(new$ActGPI)
boxplot(PatientData$ActGPI)

PatientData$ActGPI[PatientData$ActGPI>550] <- NA
mean(PatientData$ActGPI, na.rm = TRUE)
summary(PatientData$ActGPI)

PatientData$AmountPaid[PatientData$AmountPaid>150] <- NA
mean(PatientData$AmountPaid, na.rm = TRUE)
summary(PatientData$AmountPaid)

#Patient data without outliers
PatientData1<- na.omit(PatientData)

# split the data into train and test data sets
#rows=seq(1,nrow(PatientData),1)
#set.seed(123)
#trainRows=sample(rows,(70*nrow(PatientData))/100)
#train = PatientData[trainRows,] 
#test = PatientData[-trainRows,] 

#Imputing NAs
summary(PatientData)
afterkNN <- kNN(PatientData, variable = c(3,11), k=5)

?kNN()

library(car)
vif()


