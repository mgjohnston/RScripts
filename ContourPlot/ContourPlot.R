##Declare functions

#Midpoint of contour for directlabels package

not.extreme<- function(d,...)
{
  d$dist.from.center <- sqrt(d$x^2 + d$y^2)
  d$hjust<- 1
  d$vjust<- 1
  d$rot<- -45
  gapply(d, function(d, ...) d[which.min(d$dist.from.center), ])
}

#Install and/or load required packages
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

#Install and load package(s)
packageList <- c("ggplot2", "directlabels","RColorBrewer")
getPackages(packageList)

##Create similar data

area <- seq(0.2,0.9,0.02)
df<-NULL
for(i in area){
  for(j in area){
    z <- i*j
    x <- i
    y <- j
    df<-rbind(df, cbind(x,y,z))
  }
}
df<-as.data.frame(df)

##Plot data
myPal<-rev(brewer.pal(5,"Spectral"))

p<-ggplot(df, aes(x=x,y=y,z=z, fill=z))+
  geom_raster(interpolate = TRUE)+geom_contour(color="black")+
  scale_fill_gradientn(colours=myPal, guide="colorbar")+
  geom_dl(aes(label=..level..), method="not.extreme", stat="contour")
p

#Save plot
ggsave("plot.png",p, width=6.75, height=6, device="png")
