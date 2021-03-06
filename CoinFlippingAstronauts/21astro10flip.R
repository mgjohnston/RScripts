set.seed(1)
library(tidyverse)
library(reshape2)
nCr <- function(n,r) factorial(n)/(factorial(r)*factorial(n-r))
binom<-function(success,flips,prob) nCr(flips,success)*prob^success*(1-prob)^(flips-success)
possibilities <- function(n) distinct(as.data.frame(t(combn(rep(c(F,T),n),n,NULL,T))))
likelihood <- function(x, n,prob=0.5) prob^x*(1-prob)^(n-x)
splitter <- function(values, N){
  inds = c(0, sapply(1:N, function(i) which.min(abs(cumsum(as.numeric(values)) - sum(as.numeric(values))/N*i))))
  dif = diff(inds)
  re = rep(1:length(dif), times = dif)
  return(split(values, re))
}

people<-21
mintosses <- ceiling(log2(people))
maxtosses<-10
result<-NULL

for (i in mintosses:maxtosses){
  for (j in seq(0.01,0.5,0.05)){
    a <- possibilities(i)
    a <- cbind(a,sum=apply(a,1,sum))
    a <- cbind(a, chance=sapply(a$sum,likelihood,n=i,prob=j))
    split<-splitter(a$chance,people)
    result<-rbind(result,c(i,j,sum(abs(sapply(split, sum)-1/people)),all(abs(sapply(split, sum)-1/people)<0.001)))
  }
}

result<-as.data.frame(result)
ggplot(result, aes(x=V1, y=V2, fill=(V4==1)))+geom_raster(interpolate=F)
head(result[result$V4==1,],1)
a <- possibilities(10)
a <- cbind(a,sum=apply(a,1,sum))
a <- cbind(a, chance=sapply(a$sum,likelihood,n=10,prob=.46))
split<-splitter(a$chance,people)
sapply(split, sum)
