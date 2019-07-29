rm(list=ls())
setwd("H:/TBS/R Projects/Project Information-20181018")
movie_raw_data <- read.csv("movie_raw_data.csv", sep = ",")
#write.csv("MovieData", movie_raw_data)
version
#creating selection vector
n<-27205
for(x in 1:nrow(movie_raw_data))
{
  
  if(n==movie_raw_data[x,1])          
  {
    vtest <- as.data.frame(movie_raw_data[x,])
  }
  else
  {
    x+1
  }
}
class(vtest)
vtest<- as.matrix(vtest)
#create test vector
#vtest<-as.vector(movie_raw_data[2,])
#vtest
#create a matrix with resuts from selected movie to the movie database
comparisonmatrix <- matrix(NA, ncol=34, nrow=4945)#create empty matrix
for(x in 1:nrow(movie_raw_data)){
  
  comparisonmatrix[x,] <- as.vector(colSums(movie_raw_data[x,1:34]==vtest))
  x=x+1 
}#loop to compare each vector to the movie database and then paste it as a row in the resultmatrix
#Perform numerical variable analysis
Popularityvector<-as.vector(movie_raw_data$Adjusted_popularity)
Votevector<-as.vector(movie_raw_data$Adjusted_vote_parameter)
Yearvector<-as.vector(movie_raw_data$Year)
resultmatrix<-cbind(comparisonmatrix,Popularityvector,Votevector,Yearvector)
#Add every row and sort the results
rankvector<-as.vector(rowSums(resultmatrix,na.rm=TRUE))
resultvector<-as.vector(tail(sort.int(rankvector, partial=length(rankvector) - 4), 6))
positionvector<- which(rankvector %in% resultvector)
for (i in positionvector){
  print(movie_raw_data[i,1:2])
}