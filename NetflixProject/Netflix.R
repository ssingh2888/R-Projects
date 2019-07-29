rm(list=ls())
setwd("H:/TBS/R Projects/Project Information-20181018")
CD <- read.csv("cleanedData.csv", sep = ",")
#class(CD$Genres1)

colnames(CD)[3] <- "DirectorName"
#CD$ï..Title <- NULL

MatrixData <- CD[,1:7]

# readinteger <- function()
# {
#   n <- readline(prompt="Enter themovie ID: ")
#   if(!grepl("^[0-99999]+$",n))
#   {
#     return(readinteger())
#   }
#   
#   return(as.integer(n))
# }
# 
# n<-readinteger()
#print(readinteger())
#n<-767

n=285 #Try with different Movie IDs

for(x in 1:nrow(CD))
{
  if(n==CD[x,1])
  {
    vector <- as.data.frame(CD[x,])
  }
  else
  {
    x+1
  }
}
# class(vector)
# class(MatrixData)



##TO Compare vector and main data
# 
# i<-1
# for (i in 1:nrow(MatrixData)){
#   for(j in 1:ncol(vector))
#     
#   if(as.logical(colSums(MatrixData[i,2:7]==vector))){
#     resultmatrix <- as.data.frame(as.logical(colSums(vector == MatrixData[i,2:7])))
#   }
#   else
#   {
#     i+1
#   }
# }

#######Practice########
#install.packages("compare")

# test <- head(MatrixData)
# vector
#install.packages("sqldf")

# require(sqldf)
# a1NotIna2 <- sqldf('SELECT * FROM a1 EXCEPT SELECT * FROM a2')
# 
# for(i in 1:nrow(test))
# {
#   if(vector[,1:6] == test[i,2:7])
#   {
#     print("yes")
#   }
#   
# }
#as.logical(colSums(movie.raw.data[4000,]==v1))
# vtest<-as.vector(MatrixData[2,])
# vtest
vector
# #is.na(vector)
# as.vector(colSums(MatrixData[1,]==vector, na.rm = FALSE))
# #x=1 
# while (x <= 15){ 
#   vx<- as.vector(colSums(MatrixData[x,]==vector))
#   assign(paste("v",x,sep=""),vx)
#   x=x+1 
# } 

# resultmatrix<-rbind(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11)
#colnames(resultmatrix)<-c(1:51)

#Null values to be replaced by Zero so that it will 

# code to test vector similarity as.vector(colSums(movie.raw.data[1,]==v1))
#create a matrix with resuts from selected movie to the movie database
EmptyMatrix <- matrix(NA, ncol=7, nrow=4803)#create empty matrix
for(x in 1:nrow(MatrixData)){
  
  EmptyMatrix[x,] <- as.vector(colSums(MatrixData[x,1:7]==vector))
  x=x+1 
}#loop to compare each vector to the movie database and then paste it as a row in the resultmatrix

rankvector<-as.vector(rowSums(EmptyMatrix,na.rm=TRUE))
rankvector1 <- as.data.frame(rankvector)

resultvector <-as.vector(tail(sort.int(rankvector, partial=length(rankvector) - 4), 6))
tail(sort.int(rankvector, partial = length(rankvector)-4),6)
resultvector3<- as.data.frame(resultvector2)
positionvector<- which(rankvector %in% resultvector)

for (i in positionvector){
  print(MatrixData[i,1:2])
}





