library(ggplot2)

battle<-function(n1,n2){
  living <- n1
  dead <- n2
  surviving <- T
  while (surviving == T){
    if (runif(1) < 0.5) {
      living <- living
      dead <- dead - 1
    }else{
      living <- living - 1
      dead <- dead + 1
    }
    if (living < 1 || dead < 1){
      surviving = F
    }
  }
return(cbind(living,dead))
}


result2<-NULL
for (j in seq(1,450,10)){
  for (k in 1:20){
    result<-NULL
    for (i in 1:100){
      result<-rbind(result,battle(j,k))
    }
    result2<-rbind(result2, c(living=j,dead=k,livewin=100*sum(result[,1] != 0)/i))
  }
}
result2<-as.data.frame(result2)
ggplot(result2, aes(x=living,y=dead,fill=livewin>50))+geom_raster(interpolate=F)      
