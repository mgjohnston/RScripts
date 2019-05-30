One Small Step For Man, One Giant Coin Flip For Mankind
================
Matthew Johnston
30 May 2019

Solution
--------

With 3 or 5 astronauts it is possible to equally split them with a fair or unfair coin!

The minimum flips for 3 astronauts is 4, while for 5 it is 6. The more flips you do the greater your accuracy.

Riddler Classic
---------------

From Dean Ballard, one small step for man, one giant problem for The Riddler:

Sometime in the near future people from Earth will land on Mars. Someone will step out of the landing pod and become the first human to leave his or her footprints on another planet.

Imagine two astronauts sitting in the pod, both of whom would love to take that first step. But they would also like to decide which of them gets the honor in a fair manner, so they flip a coin. Despite the change in gravity, this method is fair as long as the coin is fair. If there were four astronauts in the landing pod, then they could flip a fair coin twice, assigning the four possible outcomes - heads-heads, heads-tails, tails-heads and tails-tails - to each of the four astronauts. This would also be fair as long as the coins were fair.

But what if there were three astronauts in the landing pod? Then our fair coin doesn't work so well. We could, for example, assign three of the four possible outcomes - say heads-heads, heads-tails and tails-heads - to each of the astronauts. Then, if the outcome were tails-tails, they could simply start over again with two more flips. This would give an ever-increasing probability that a fair decision would eventually be made, but that could take a long time, and the required number of flips would be unknown. And there's a planet to walk on!

Another approach, however, is to use an "unfair coin" - one in which the probabilities of heads and tails are not equal. Is it possible to make a fair choice among three astronauts with a fixed number of flips of an unfair coin?

You are able to set the coin's probability of heads to any number you like between 0 and 1. You may flip the coin as many times as you like, as long as that is some known, fixed number. And, you may assign any combination of possible outcomes to each of the three astronauts.

Extra credit: What if there were five astronauts?

Three astronauts
----------------

If we consider every permuation as different, i.e. \[H,T\] is different from \[T,H\], then the minimum number of coinflips for three astronauts will be 2 \[`ceiling(log2(3))`\].

We can then sample through uneven coins between 0.01 to fair, from 2 tosses to 10. And see at which point there is a reasonable degree of accuracy.

``` r
people<-3
mintosses <- ceiling(log2(people))
maxtosses<-10
result<-NULL

for (i in mintosses:maxtosses){
  for (j in seq(0.01,0.5,0.05)){
  a <- possibilities(i)
  a <- cbind(a,sum=apply(a,1,sum))
  a <- cbind(a, chance=sapply(a$sum,likelihood,n=i,prob=j))
  split<-splitter(a$chance,people)
  result<-rbind(result,c(i,j,sum(abs(sapply(split, sum)-1/people)),all(abs(sapply(split, sum)-1/people)<0.01)))
  }
}

result<-as.data.frame(result)
result2<-result
ggplot(result, aes(x=V1, y=V2, fill=(V4==1)))+geom_raster(interpolate=F)
```

![](README_figs/README-cars1-1.png)

In this coarse overview, 5 flips is the minimum. Around a weighting of 0.4. Let's zoom in:

``` r
people<-3
mintosses <- ceiling(log2(people))
maxtosses<-5
result<-NULL

for (i in mintosses:maxtosses){
  for (j in seq(0.3,0.5,0.001)){
  a <- possibilities(i)
  a <- cbind(a,sum=apply(a,1,sum))
  a <- cbind(a, chance=sapply(a$sum,likelihood,n=i,prob=j))
  split<-prive(a$chance,people)
  result<-rbind(result,c(i,j,sum(abs(sapply(split, sum)-1/people)),all(abs(sapply(split, sum)-1/people)<0.01)))
  }
}

result<-as.data.frame(result)
ggplot(result, aes(x=V1, y=V2, fill=(V4==1)))+geom_raster(interpolate=F)
```

![](README_figs/README-fine2-1.png)

We can see that the minimum number of coin flips is 4, with a weighting of 0.387:

``` r
head(result[result$V4==1,],1)
```

    ##     V1    V2         V3 V4
    ## 490  4 0.387 0.01914032  1

We give astronaut 1:

FTTF, FFTF, FTFT, FTTT, FTFF

astronaut 2:

FTFT, FFFT,FFFF,TFTF

astronaut 3:

TFFT,TFFF,TTFT,TTFF,TTTF,TTTT,TFTT

``` r
  a <- possibilities(4)
  a <- cbind(a,sum=apply(a,1,sum))
  a <- cbind(a, chance=sapply(a$sum,likelihood,n=4,prob=0.387))
  invisible(split<-prive(a$chance,people))
```

    ## [1] 0.0001468275

``` r
  print(split)
```

    ## $`1`
    ## [1] 0.05627855 0.08914406 0.05627855 0.03552985 0.08914406
    ## 
    ## $`2`
    ## [1] 0.05627855 0.08914406 0.14120234 0.05627855
    ## 
    ## $`3`
    ## [1] 0.03552985 0.05627855 0.08914406 0.03552985 0.05627855 0.03552985
    ## [7] 0.02243075

``` r
  print(a)
```

    ##       V1    V2    V3    V4 sum     chance
    ## 1  FALSE  TRUE FALSE  TRUE   2 0.05627855
    ## 2  FALSE  TRUE FALSE FALSE   1 0.08914406
    ## 3  FALSE  TRUE  TRUE FALSE   2 0.05627855
    ## 4  FALSE  TRUE  TRUE  TRUE   3 0.03552985
    ## 5  FALSE FALSE  TRUE FALSE   1 0.08914406
    ## 6  FALSE FALSE  TRUE  TRUE   2 0.05627855
    ## 7  FALSE FALSE FALSE  TRUE   1 0.08914406
    ## 8  FALSE FALSE FALSE FALSE   0 0.14120234
    ## 9   TRUE FALSE  TRUE FALSE   2 0.05627855
    ## 10  TRUE FALSE  TRUE  TRUE   3 0.03552985
    ## 11  TRUE FALSE FALSE  TRUE   2 0.05627855
    ## 12  TRUE FALSE FALSE FALSE   1 0.08914406
    ## 13  TRUE  TRUE FALSE  TRUE   3 0.03552985
    ## 14  TRUE  TRUE FALSE FALSE   2 0.05627855
    ## 15  TRUE  TRUE  TRUE FALSE   3 0.03552985
    ## 16  TRUE  TRUE  TRUE  TRUE   4 0.02243075

Taking this approach, I normalised each number of tosses to itself to find the best and worst weighting

``` r
m<-spread(result[,1:3], "V1", "V3")[,-1]
data2<-melt(as.data.frame(cbind(weight=result[,2][1:dim(m)[1]],t(t(m)/colMax(m)))), id = "weight")
ggplot(data2, aes(x=variable, y=weight, fill=(value)))+geom_raster(interpolate=F)
```

![](README_figs/README-pressure5-1.png)

Without normalising, you can see that the accuracy increases with the more flips.

``` r
ggplot(result, aes(x=V1, y=V2, fill=(V3)))+geom_raster(interpolate=F)
```

![](README_figs/README-pressure5-2-1.png)

``` r
ggplot(result2, aes(x=V1, y=V2, fill=(V3)))+geom_raster(interpolate=F)
```

![](README_figs/README-pressure5-2-2.png) \#\# How about number of successes?

It would be quite difficult for the astronauts to remember exactly how the coin tosses went down, was it TTFT or TFTT?! So it may instead be easier to count how many "Heads" were achieved?

``` r
mintosses <- people-1
result<-NULL
maxtosses<-100


for (i in mintosses:maxtosses){
  for (j in seq(0.2,0.5,0.01)){
    a<-NULL
    for (k in 0:i){
      a<-rbind(a,c(k,binom(k,i,j)))
    }
    split<-splitter(a[,2],people)
    result<-rbind(result,c(i,j,sum(abs(sapply(split, sum)-1/people)),all(abs(sapply(split, sum)-1/people)<0.01)))
  }
}
result<-as.data.frame(result)
ggplot(result, aes(x=V1, y=V2, fill=(V4==1)))+geom_raster(interpolate=F)
```

![](README_figs/README-successes6-1.png)

Surprisingly, we can see you only need 6 tosses to get an acceptable split:

We give astronaut 1:

0,1 heads

astronaut 2:

2 heads

astronaut 3:

3, 4, 5, 6 heads

on a coin that gives heads 34% of the time.

``` r
head(result[result$V4==1,],1)
```

    ##     V1   V2          V3 V4
    ## 139  6 0.34 0.009592924  1

``` r
i=6
j=0.34
a<-NULL
for (k in 0:i){
  a<-rbind(a,c(k,binom(k,i,j)))
}
split<-splitter(a[,2],people)
print(a)
```

    ##      [,1]        [,2]
    ## [1,]    0 0.082653950
    ## [2,]    1 0.255475846
    ## [3,]    2 0.329021922
    ## [4,]    3 0.225994856
    ## [5,]    4 0.087316194
    ## [6,]    5 0.017992428
    ## [7,]    6 0.001544804

``` r
print(split)
```

    ## $`1`
    ## [1] 0.08265395 0.25547585
    ## 
    ## $`2`
    ## [1] 0.3290219
    ## 
    ## $`3`
    ## [1] 0.225994856 0.087316194 0.017992428 0.001544804

5 Astronauts
------------

We can do the same process for 5 astronauts:

``` r
people<-5
mintosses <- ceiling(log2(people))
maxtosses<-10
result<-NULL

for (i in mintosses:maxtosses){
  for (j in seq(0.35,0.45,0.01)){
    a <- possibilities(i)
    a <- cbind(a,sum=apply(a,1,sum))
    a <- cbind(a, chance=sapply(a$sum,likelihood,n=i,prob=j))
    split<-splitter(a$chance,people)
    result<-rbind(result,c(i,j,sum(abs(sapply(split, sum)-1/people)),all(abs(sapply(split, sum)-1/people)<0.01)))
  }
}

result<-as.data.frame(result)
ggplot(result, aes(x=V1, y=V2, fill=(V4==1)))+geom_raster(interpolate=F)
```

![](README_figs/README-5%20astro8-1.png)

``` r
head(result[result$V4==1,],1)
```

    ##    V1  V2       V3 V4
    ## 39  6 0.4 0.027264  1

And get 6 flips with a 40% Heads coin.

``` r
mintosses <- people-1
result<-NULL
maxtosses<-1000


for (i in seq(mintosses,maxtosses,25)){
  for (j in seq(0.2,0.5,0.01)){
    a<-NULL
    for (k in 0:i){
      a<-rbind(a,c(k,dbinom(k,i,j)))
    }
    split<-splitter(a[,2],people)
    result<-rbind(result,c(i,j,sum(abs(sapply(split, sum)-1/people)),all(abs(sapply(split, sum)-1/people)<0.01)))
  }
}
result<-as.data.frame(result)
ggplot(result, aes(x=V1, y=V2, fill=(V4==1)))+geom_raster(interpolate=F)
```

![](README_figs/README-5%20astro210-1.png)

And we see we need ~280 coin tosses!

``` r
head(result[result$V4==1,],1)
```

    ##      V1  V2         V3 V4
    ## 362 279 0.4 0.02926958  1

What if they only had a fair coin?
----------------------------------

We could just flip a fair coin the minimum number of times each and wait until only one astronaut got all heads.

``` r
coinToss<- function(prob=0.5){
  return(runif(1)<prob)
}

multipleToss <- function(tosses, prob=0.5){
  return(sum(sapply(rep(prob,tosses), coinToss)))
}

players <- 3
tosses <- ceiling(log2(players))
fairness <- 0.5
winner<-NULL
reps<-1000

for(j in 1:reps){
  result<-NULL
  while(!sum(result[,2]==tosses)==1){
    result<-NULL
    for (i in 1:players){
      result <- rbind(result,c(player=i,wins=multipleToss(tosses, fairness)))
    }
  }
  winner<-c(winner,which(result[,2]==tosses))
}

table(winner)
```

    ## winner
    ##   1   2   3 
    ## 320 350 330

``` r
hist(winner)
```

![](README_figs/README-fliptowin12-1.png)

Finally, my colleague suggested just taking a dice...

Thanks
------

Thanks to StackOverflow (<https://stackoverflow.com/questions/46431527/split-a-vector-into-chunks-such-that-sum-of-each-chunk-is-approximately-constant/>) where I got the functions Prive and Splitter.

and for the riddle: <https://fivethirtyeight.com/features/one-small-step-for-man-one-giant-coin-flip-for-mankind/> .
