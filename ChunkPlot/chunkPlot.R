#Install and load package(s)
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

packageList <- c("tidyverse")

getPackages(packageList)

##Import  data
data<-structure(list(Gene = structure(c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 
                                        4L, 5L, 5L, 6L, 6L, 7L, 7L, 8L, 8L, 9L, 9L, 10L, 10L, 11L, 11L, 
                                        12L, 12L), .Label = c("A", "B", "C", "D", "E", "F", "G", "H", 
                                                              "I", "J", "K", "L"), class = "factor"), Genotype = structure(c(2L, 
                                                                                                                             1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 
                                                                                                                             1L, 2L, 1L, 2L, 1L, 2L, 1L), .Label = c("Col", "WT"), class = "factor"), 
                     T1_D = c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 
                              1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L), T1_SD = c(2L, 
                                                                                         2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                                         2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), T2_D = c(5L, 6L, 7L, 8L, 
                                                                                                                                   5L, 6L, 7L, 8L, 5L, 6L, 7L, 8L, 5L, 6L, 7L, 8L, 5L, 6L, 7L, 
                                                                                                                                   8L, 5L, 6L, 7L, 8L), T2_SD = c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                                                                                                                  2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                                                                                                                  2L, 2L)), class = "data.frame", row.names = c(NA, -24L))
head(data) 

#Make data in useful format

df<-gather(data, "stage_Type", "value",c(3:6))%>%separate(stage_Type,c("stage","Type"),"_")%>%spread(key=Type, value=value)

head(df)

#Subset list of genes of interest or take all genes

# g<-c("C","E","F")
g<-levels(df$Gene)

#Subset data and plot graph(s) in groups of max. 6
chunk <- 6

for(h in split(g,cumsum(0:(length(g)-1)%%chunk==0))){
  df %>%
  filter(Gene %in% h) %>%
  {ggplot(.,aes(x=stage, y=D, group=Genotype, color=Genotype, shape=Genotype))+
    geom_point(position = position_dodge(width=0.2))+
    geom_line(position = position_dodge(width=0.2), aes(linetype=Genotype))+
    geom_errorbar(aes(ymin=D-SD,ymax=D+SD), width=0.3,position = position_dodge(width=0.2))+
    facet_wrap(~Gene)+
    ylab("FPKM")+
    xlab("Developmental Stage")} %>%
  print
}