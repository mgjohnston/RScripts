uncorrpval<-0.00001;

getPackages <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      library( i , character.only = TRUE )
    }
  }
}

packageList <- c("tidyverse","reshape2","readxl","RColorBrewer","ggrepel","ggbeeswarm", "gridExtra")

getPackages(packageList)

SEpredict <- function(model,xmin, xmax, padding = TRUE){
  ##make SE ribbon
  if (padding) {pad<-0.05} else {pad<-0}
  xval <- seq(floor(xmin*(1-pad)),ceiling(xmax*(1+pad)),.05)
  fit <- predict(model,data.frame(Size=xval), se.fit=T, type="link")
  pcrit <- qnorm(1-pval/2)
  yval <-  model$family$linkinv(fit$fit)
  upper <- model$family$linkinv(fit$fit+fit$se.fit)
  lower <- model$family$linkinv(fit$fit-fit$se.fit)
  upperci <-model$family$linkinv(fit$fit+pcrit*fit$se.fit)
  lowerci <-model$family$linkinv(fit$fit-pcrit*fit$se.fit)
  return(data.frame(xval, yval, upper, lower, upperci, lowerci))
}

SEpredict_linear <- function(model,xmin, xmax, padding = TRUE){
  ##make SE ribbon
  if (padding) {pad<-0.05} else {pad<-0}
  xval <- seq(floor(xmin*(1-pad)),ceiling(xmax*(1+pad)),.05)
  fit <- predict(model,data.frame(Size=xval), se.fit=T, type="response")
  pcrit <- qnorm(1-pval/2)
  yval <-  (fit$fit)
  upper <- (fit$fit+fit$se.fit)
  lower <- (fit$fit-fit$se.fit)
  upperci <-(fit$fit+pcrit*fit$se.fit)
  lowerci <-(fit$fit-pcrit*fit$se.fit)
  return(data.frame(xval, yval, upper, lower, upperci, lowerci))
}


ratePredict <- function(model, weight, actualRate = Inf){
  fit <- predict(model,data.frame(Size=weight), se.fit=T, type="link")
  pcrit <- qnorm(1-pval/2)
  upperci <-model$family$linkinv(fit$fit+pcrit*fit$se.fit)
  lowerci <-model$family$linkinv(fit$fit-pcrit*fit$se.fit)
  yval <-  model$family$linkinv(fit$fit)
  bound <- if (actualRate >= upperci) {upperci} else if (actualRate >= yval) {yval} else {lowerci}
  return(c(bound, yval))
}


#### Import Data
data <- read_excel("//nbi-cfs2/shared/Research-Groups/Christine-Faulkner/Matthew Johnston/Scripts/Mobility/JJmobcomppapf.xlsx")
head(data)

#### Reorganise data into a useful format (wide to long)
sizemob<-data
sizemob$value<-sizemob$GFPmobility
sizemob$Effector<-sizemob$ID
head(sizemob)

#####################
#### Generate a Standard curve
#####################

#### Subset the data to just the points marked as a standard curve (SD)
sizemobsd<-(sizemob[sizemob$Type=="SD"& sizemob$Size<100,])

sizemobexp<-(sizemob[sizemob$Type!="SD",])
numberOfComparisons <- length(levels(factor(sizemobexp$Effector)))
pval <- uncorrpval/numberOfComparisons
alpha <- 1 - pval
#### Make a linear fit of the SD
mod<-glm(value~Size,family=quasipoisson(link = "log"), data = sizemobsd)

#### Visualise this line
plot(sizemobsd$Size,sizemobsd$value)
abline(mod)

#### Make a pretty graph of Standard Curve
sizemobsd<-(sizemob[sizemob$Type=="SD",])
xmin <- min(sizemobsd$Size)
xmax <- max(sizemobsd$Size)
bestfit<-SEpredict(mod,xmin,xmax)

ggStandard2 <- ggplot(sizemobsd, aes(x=Size, y=value))+
  geom_ribbon(data=bestfit, aes(x=xval, y=yval, ymin=lowerci, ymax=upperci), alpha=0.1, fill="red")+
  geom_ribbon(data=bestfit, aes(x=xval, y=yval, ymin=lower, ymax=upper), alpha=0.2, fill="blue")+
  geom_line(data=bestfit, aes(x=xval, y=yval, ymin=lower, ymax=upper), size=.5)+
  geom_count(shape=1)+
  xlim(25,55)+
  theme_bw()+
  theme(
    text=element_text(size=14),
    legend.position ='none'
    #legend.position = c(.99, .99),
    #legend.justification = c("right", "top"),
    #legend.box.just = "right",
    #legend.margin = margin(6, 6, 6, 6),
    #legend.text = element_text(size=8),
    #legend.background = element_rect(colour = 'black',  linetype='solid')
  )+
  xlab("Size (kDa)")+
  ylab("GFP spread (cells)")+
  labs(size = "Point Density (n)"
  )+
  stat_summary(fun.y = "mean", geom="point", size=3, shape=16, color="#FE34DA")
ggStandard2
ggsave("stanplan.png", ggStandard2, device = "png", width=60*2.8, height=40*2.8,  unit="mm", dpi=600)


#####################
#### Compare Experimental data to SD
#####################
expResults <- NULL
for(i in levels(factor(sizemobexp$Effector))){
  subdata<-sizemobexp[sizemobexp$Effector==i,]
  movement <- sum(subdata$value)
  observations <- length(subdata$value)
  predRate <- ratePredict(mod, data.frame(Size=subdata$Size[1]), movement/observations, FALSE)
  x<- movement
  t<- observations
  r<-predRate[1]
  poisson<- poisson.test(x=x,T=t,r=r, conf.level = alpha)
  
  sem <- sd(subdata$value)/sqrt(observations)
  
  rate <- unlist(poisson[5])
  upper <- poisson[4]$conf.int[2]
  lower <- poisson[4]$conf.int[1]
  Size <- subdata$Size[1]
  Effector <- subdata$Effector[1]
  signif <- poisson[3] < pval
  rateDiff <- rate - predRate[2]
  relRateDiff <- (rate) / (predRate[2])
  
  Type <- subdata$Type[1]
  
  expResults<-rbind(expResults,data.frame(Effector,Type, Size, predRate=predRate[2], rate, rateDiff, relRateDiff, lower, upper, signif,sem))
  
}

expResults$signif<-factor(expResults$signif, levels=c("TRUE","FALSE"))
xmin <- 27
xmax <- max(expResults$Size)*1.05
bestfit<-SEpredict(mod,xmin,xmax, padding = FALSE)


facts <- interaction(expResults$signif, expResults$Type)

myColors<-c("#7570B3","#1B9E77","#3b34fe","")
names(myColors) <- c("FALSE.EXP","FALSE.IM","TRUE.EXP","" )
myShapes <- c(16,15,17,0)
names(myShapes) <- c( "FALSE.EXP","FALSE.IM","TRUE.EXP","")
colScale <- scale_colour_manual(name = "Hello",values = myColors, labels=c("Significant\nMobility" ,"Expected\nMobility","Immobile", ""), drop=T)
shapeScale <- scale_shape_manual(name="Hello", values = myShapes, labels=c("Significant\nMobility" , "Expected\nMobility","Immobile", ""), drop=T)


ggResult1 <- ggplot(expResults, aes(x=Size, y=rate))+
  geom_point(data=sizemobexp, aes(x=Size, y=value), alpha=0.1)+
  geom_ribbon(data=bestfit, aes(x=xval, y=yval, ymin=lowerci, ymax=upperci), alpha=0.1, fill="red")+
  geom_ribbon(data=bestfit, aes(x=xval, y=yval, ymin=lower, ymax=upper), alpha=0.2, fill="blue")+
  geom_line(data=bestfit, aes(x=xval, y=yval, ymin=lower, ymax=upper), size=1)+
  #geom_count(data=sizemobexp, aes(x=Size, y=value), shape=1, alpha=0.2)+
  geom_pointrange(aes(shape=facts,ymin=rate-sem,ymax=rate+sem, color=facts), size=.75)+
  theme_bw()+
  theme(
    text=element_text(size=18),
    #legend.position = 'none'
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(-6, 6, 6, 6),
    legend.title = element_blank(),
    legend.text = element_text(size=11),
    legend.background = element_rect(colour = 'black',  linetype='solid')
  )+
  xlab("Size (kDa)")+
  ylab("GFP spread (cells)")+
  colScale+
  shapeScale
  #geom_text_repel(aes(label=Effector),size=4) #Toggle this line for labels
ggResult1

ggsave("moballlegcor.png", ggResult1, device = "png", width=60*2.8, height=40*2.8,  unit="mm", dpi=600)

#### Import Data
data <- read_excel("//nbi-cfs2/shared/Research-Groups/Christine-Faulkner/Matthew Johnston/Scripts/Mobility/JJmobonlypapf.xlsx")
head(data)

#### Reorganise data into a useful format (wide to long)
sizemob<-data
sizemob$value<-sizemob$GFPmobility
sizemob$Effector<-sizemob$ID
head(sizemob)

#####################
#### Generate a Standard curve
#####################

#### Subset the data to just the points marked as a standard curve (SD)
sizemobsd<-(sizemob[sizemob$Type=="SD"& sizemob$Size<100,])

sizemobexp<-(sizemob[sizemob$Type=="EXP",])
numberOfComparisons <- length(levels(factor(sizemobexp$Effector)))
pval <- uncorrpval/numberOfComparisons
alpha <- 1 - pval
#### Make a linear fit of the SD
mod<-glm(value~Size,family=quasipoisson(link = "log"), data = sizemobsd)

#### Visualise this line
plot(sizemobsd$Size,sizemobsd$value)
abline(mod)

#### Make a pretty graph of Standard Curve
sizemobsd<-(sizemob[sizemob$Type=="SD",])
xmin <- min(sizemobsd$Size)
xmax <- max(sizemobsd$Size)
bestfit<-SEpredict(mod,xmin,xmax, belowZero = FALSE)



#####################
#### Compare Experimental data to SD
#####################
expResults <- NULL
for(i in levels(factor(sizemobexp$Effector))){
  subdata<-sizemobexp[sizemobexp$Effector==i,]
  movement <- sum(subdata$value)
  observations <- length(subdata$value)
  predRate <- ratePredict(mod, data.frame(Size=subdata$Size[1]), movement/observations, FALSE)
  x<- movement
  t<- observations
  r<-predRate[1]
  poisson<- poisson.test(x=x,T=t,r=r, conf.level = alpha)
  sem <- sd(subdata$value)/sqrt(observations)
  
  
  rate <- unlist(poisson[5])
  upper <- poisson[4]$conf.int[2]
  lower <- poisson[4]$conf.int[1]
  Size <- subdata$Size[1]
  Effector <- subdata$Effector[1]
  signif <- poisson[3] < pval
  rateDiff <- rate - predRate[2]
  relRateDiff <- (rate) / (predRate[2])
  
  
  expResults<-rbind(expResults,data.frame(Effector, Size, predRate=predRate[2], rate, rateDiff, relRateDiff, lower, upper, signif,sem))
  
}

expResults$signif<-factor(expResults$signif, levels=c("TRUE","FALSE"))
xmin <- 27
xmax <- max(expResults$Size)*1.05
bestfit<-SEpredict(mod,xmin,xmax, belowZero = FALSE, padding = FALSE)

myColors<-c("#7570B3","#1B9E77","#3b34fe","")
names(myColors) <- c("FALSE","","TRUE")
myShapes <- c(15,16,1)
names(myShapes) <- c("FALSE","","TRUE")
colScale <- scale_colour_manual(name = "Effector",values = myColors, labels=c("Significant\nMobility","Expected\nMobility"), drop=FALSE)
shapeScale <- scale_shape_manual(name="Effector", values = myShapes, labels=c("Significant\nMobility","Expected\nMobility"), drop=FALSE)




ggResult1mobo <- ggplot(expResults, aes(x=Size, y=rate))+
  #geom_point(data=sizemobexp, aes(x=Size, y=value), alpha=0.1)+
  geom_ribbon(data=bestfit, aes(x=xval, y=yval, ymin=lowerci, ymax=upperci), alpha=0.1, fill="red")+
  geom_ribbon(data=bestfit, aes(x=xval, y=yval, ymin=lower, ymax=upper), alpha=0.2, fill="blue")+
  geom_line(data=bestfit, aes(x=xval, y=yval, ymin=lower, ymax=upper), size=1)+
  #geom_count(data=sizemobexp, aes(x=Size, y=value), shape=1, alpha=0.2)+
  geom_pointrange(aes(shape=signif,ymin=rate-sem,ymax=rate+sem, color=signif), size=3/2.8)+
  theme_bw()+
  theme(
    text=element_text(size=18),
    legend.position = 'none'
    #legend.position = c(.99, .99),
    #legend.justification = c("right", "top"),
    #legend.box.just = "right",
    #legend.margin = margin(-6, 6, 6, 6),
    #legend.title = element_blank(),
    #legend.text = element_text(size=11),
   # legend.background = element_rect(colour = 'black',  linetype='solid')
  )+
  xlab("Size (kDa)")+
  ylab("GFP spread (cells)")+
  colScale+
  shapeScale 
#geom_text_repel(aes(label=Effector),size=5) #Toggle this line for labels
ggResult1mobo

ggsave("mob.png", ggResult1mobo, device = "png", width=60*2.8, height=40*2.8,  unit="mm", dpi=600)


tddata <- read_excel("//nbi-cfs2/shared/Research-Groups/Christine-Faulkner/Matthew Johnston/Scripts/Mobility/JJtdpapf.xlsx")
tddata$Effector<-tddata$ID
tdmobmovesd<-(tddata[tddata$Type=="SD",])
tdmobmoveexp<-(tddata)
numberOfComparisons <- length(levels(factor(tdmobmoveexp$Effector)))
pval <- uncorrpval/numberOfComparisons
alpha <- 1 - pval

mod2<-lm(tDmove~1, data = tdmobmovesd)
bestfit2<-SEpredict_linear(mod2,25,55,belowZero=FALSE, padding = FALSE)

tomsd<-ggplot(tdmobmovesd, aes(x=Size, y=tDmove)) +
  geom_ribbon(data=bestfit2, aes(x=xval, y=yval, ymin=lowerci, ymax=upperci), alpha=0.1, fill="red")+
  geom_ribbon(data=bestfit2, aes(x=xval, y=yval, ymin=lower, ymax=upper), fill= "blue", alpha=0.2)+
  geom_line(data=bestfit2, aes(x=xval, y=yval), size=1)+
  geom_count(alpha=0.1)+
  stat_summary(fun.y = "mean", geom="point", size=3, shape=16, color="#FE34DA")+
  theme_bw()+
  theme(
    text=element_text(size=14),
    legend.position = 'none'
    #legend.background = element_blank(),
    #legend.background = element_rect(colour = 'black',  linetype='solid')
    #legend.key = element_blank(),
    #legend.position = c(.01, .99),
    #legend.justification = c("left", "top"),
    #legend.box.just = "right",
    #legend.margin = margin(6, 6, 6, 6),
    #legend.text = element_text(size=8)
  ) +
  xlab("Size of GFP fusion (kDa)")+
  ylab("Tomato spread (cells)")+
  labs(
    size = "Point Density (n)"
  )
tomsd
ggsave("tomsd.png", tomsd, device = "png", width=60*2.8, height=40*2.8,  unit="mm", dpi=600)



bestfit2<-SEpredict(mod2,30,55, padding = FALSE)


poisson <- poisson.test(x=sum(tdmobmovesd$tDmove),T=length(tdmobmovesd$tDmove), conf.level=alpha)
rate <- unlist(poisson[5])
upper <- poisson[4]$conf.int[2]
lower <- poisson[4]$conf.int[1]
standardTdData <- data.frame(rate, upper, lower)


tdmobmoveexpJoin <- left_join(tdmobmoveexp, expResults, by="Effector", suffix = c("", ".y"))

expResults2 <- NULL
for(i in levels(factor(tdmobmoveexpJoin$Effector))){
  subdata<-tdmobmoveexpJoin[tdmobmoveexpJoin$Effector==i,]
  subdata
  movement <- sum(subdata$tDmove)
  observations <- length(subdata$tDmove)
  actualRate <- movement/observations
  predRateTD <- if (actualRate >= standardTdData$upper) {standardTdData$upper} else if (actualRate >= standardTdData$rate) {standardTdData$rate} else {standardTdData$lower}
  x<- movement
  t<- observations
  r<-predRateTD
  poisson<- poisson.test(x=x,T=t,r=r, conf.level = alpha)
  
  rateTD <- unlist(poisson[5])
  upper <- poisson[4]$conf.int[2]
  lower <- poisson[4]$conf.int[1]
  Size <- subdata$Size[1]
  Effector <- subdata$Effector[1]
  signif <- poisson[3] < pval
  
  rateDiffGFP <- subdata$rateDiff[1]
  relRateDiffGFP <- subdata$relRateDiff[1]
  predRateGFP <- subdata$predRate[1]
  rateGFP <- subdata$rate[1]
  Type <- subdata$Type[1]
  expResults2<-rbind(expResults2,data.frame(Effector, Type, Size, standardTdData$rate, rateTD, predRateGFP, rateGFP, rateDiffGFP, relRateDiffGFP, lower, upper, signif))
  
}
expResults2$signif<-factor(expResults2$signif, levels=c("TRUE","FALSE"))

myColors <- c("deeppink3", "cyan3", "grey")
names(myColors) <- c("FALSE","","TRUE")
myShapes <- c(15,16,1)
names(myShapes) <- c("FALSE","","TRUE")
colScale <- scale_colour_manual(name = "Effector",values = myColors, labels=c("Significant\nMobility","Expected\nMobility"), drop=FALSE)
shapeScale <- scale_shape_manual(name="Effector", values = myShapes, labels=c("Significant\nMobility","Expected\nMobility"), drop=FALSE)


tomgraph<-ggplot(expResults2, aes(x=relRateDiffGFP, y=rateTD)) +
  geom_ribbon(aes(ymin=standardTdData$lower, ymax = standardTdData$upper, x=relRateDiffGFP), alpha=0.1, fill="red") +
  geom_line(aes(y=standardTdData$rate, x=relRateDiffGFP), size=1)+
  geom_pointrange(aes(shape=signif,ymin=lower,ymax=upper, color=signif), size=1)+
  theme_bw()+
  ylim(0,8)+
  theme(
    text=element_text(size=14),
    legend.position = 'none'
    #legend.position = c(.99, .99),
    #legend.justification = c("right", "top"),
    #legend.box.just = "right",
    #legend.margin = margin(-6, 6, 6, 6),
    #legend.title = element_blank(),
    #legend.text = element_text(size=11),
    #legend.background = element_rect(colour = 'black',  linetype='solid')
  )+
  xlab("Relative Effector-GFP Hypermobility")+
  ylab("Tomato spread (cells)")+
  colScale+
  shapeScale
  #scale_x_log10()
  #geom_text_repel(aes(label=Effector))
tomgraph

ggsave("tomplotplan2.png", tomgraph, device = "png", width=60*2.8, height=40*2.8,  unit="mm", dpi=600)




#svg(filename = "mobplot.svg", width = 6, height = 4)

ggsave("mobplotalllab.png", ggResult1, device = "png", width=60*2.8, height=40*2.8,  unit="mm", dpi=600)

ggsave("mobplotalllab.png", ggResult1, device = "png", width=60*2.8, height=40*2.8,  unit="mm", dpi=600)

ggsave("moblab.png", ggResult1, device = "png", width=60*2.8, height=40*2.8,  unit="mm", dpi=600)

ggsave("mob.png", ggResult1, device = "png", width=60*2.8, height=40*2.8,  unit="mm", dpi=600)

ggsave("stan.png", ggResult1, device = "png", width=60*2.8, height=40*2.8,  unit="mm", dpi=600)

ggsave("tomsd.png", ggResult1, device = "png", width=60*2.8, height=40*2.8,  unit="mm", dpi=600)


ggsave("mobplotalllab.png", ggResult1, device = "png", width=60*2.8, height=40*2.8,  unit="mm", dpi=600)

ggsave("mobplotalllab.png", ggResult1, device = "png", width=60*2.8, height=40*2.8,  unit="mm", dpi=600)


