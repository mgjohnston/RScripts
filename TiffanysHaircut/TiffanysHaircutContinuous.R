set.seed(1)
waitTime<-function(){
  tiffany<- runif(1)*15
  barber<- runif(3)*15
  tiffanyWanted <- sample(c(T,F,F,F),1)
  
  if (tiffanyWanted){
    time <- tiffany + 15
  }
  else(
    if (all(tiffany < barber)){
      time <- tiffany + 15
    }
    else{
      time <- tiffany
      
    }
  )
  return(time)
}
meanWait <- NULL
for(j in 1:1000){
waitTimed<-NULL
for (i in 1:1000){
  waitTimed<-rbind(waitTimed,waitTime())
}
meanWait<-c(meanWait,mean(waitTimed))
}
mean(meanWait)
hist(meanWait)