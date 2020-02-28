barbers<-as.data.frame(expand.grid(1:15, 1:15, 1:15, 1:15, c(F,T)))
returnWait <- function(x){
  tiffany<- unlist(x[1])
  barber<- unlist(x[2:4])
  tiffanyWanted <- unlist(x[5])
  if (tiffanyWanted){
    time <- tiffany + 15
  }
  else{
    if (all(tiffany < barber)){
      time <- tiffany + 15
    }
    else{
      time <- tiffany
    }
  }
  return(time)
}
waitTimesWant<-apply(barbers[barbers$Var5==T,], 1, returnWait)
waitTimesNoWant<-apply(barbers[barbers$Var5==F,], 1, returnWait)

waitTimes <- c(waitTimesWant,waitTimesNoWant,waitTimesNoWant,waitTimesNoWant)

mean(waitTimes)
hist(waitTimes)