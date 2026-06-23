########################## Median analysis of raw data
library(dplyr)
library(agricolae)
library(fitdistrplus)
library(goftest)
library(ggplot2)
library(ggspatial)
library( rstatix)

rm(list=ls())    #  remove all previous variables and value

##### load district descriptive data
Dpath <- "C:/Users/Owner/Compactness/Papercode/Process data"
setwd(Dpath)
Fname <-"Compiled_data.csv"
CDx <- read.csv( file=Fname)
nrow( CDx)

###################################################################
### Polsby Medians by Session
####################################################################
PlotData <- CDx

Global.median <- median( PlotData$Polsby)

Plot <- ggplot(PlotData, aes(x = as.factor(SESSN) , y = Polsby)) +
  geom_boxplot(aes( fill= as.factor(SESSN))) +
  labs( x = "Session", fill="") +
  geom_hline(yintercept=Global.median, linetype="dashed",color = "red", size=1)+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
    theme(text = element_text(size=12)) +
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )
Plot
setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
ggsave( file="CDSESSN_polsby_box.eps", plot=Plot, width=6, height=3, units="in")

#### Violin chart
Plot <- ggplot(PlotData, aes(x = as.factor(SESSN) , y = Polsby)) +
  geom_violin(aes( fill= as.factor(SESSN)), scale='count', draw_quantiles=c(0.25,0.5,0.75)) +
  labs( x = "Session", fill="") +
  geom_hline(yintercept=Global.median, linetype="dashed",color = "red", size=1)+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  theme(text = element_text(size=12), legend.position = "none") +
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )
Plot

setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
ggsave( file="CDSESSN_polsby_box.eps", plot=Plot, width=6, height=3, units="in")

## Calc ranges
max( CDx$Polsby[CDx$SESSN==118]) - min(CDx$Polsby[ CDx$SESSN==118])
max( CDx$Polsby[CDx$SESSN==113]) - min(CDx$Polsby[ CDx$SESSN==113])
max( CDx$Polsby[CDx$SESSN==108]) - min(CDx$Polsby[ CDx$SESSN==108])


Test <- kruskal.test( Polsby ~ SESSN, data = PlotData)  #### NULL is no difference in medians
Test  ###Result  Not all  the same

Mtest <- Median.test(  PlotData$Polsby, PlotData$SESSN, simulate.p.value= TRUE)
Mtest$statistics  ### 118 is diff

Wilcoxtest <- pairwise_wilcox_test( Polsby~SESSN, data =PlotData)
Wilcoxtest   ### they ae all diff


Medians <- CDx%>%group_by(ST, SESSN)%>%summarise( STmed=median(Polsby),STmin=min(Polsby))

Plot <- ggplot(Medians, aes(x = as.factor(SESSN) , y = STmin)) +
  geom_boxplot(aes( fill= as.factor(SESSN))) +
  labs( x = "Session", fill="") +
  geom_hline(yintercept=Global.median, linetype="dashed",color = "red", size=1)+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  theme(text = element_text(size=12)) +
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )
Plot

Test <- kruskal.test( STmed ~ SESSN, data = Medians)  #### NULL is no difference in medians
Test  ###Result  Not all  the same

Mtest <- Median.test(  Medians$STmed, Medians$SESSN, simulate.p.value= TRUE)
Mtest$statistics  ### 118 is diff

MediansX <- Medians[ Medians$ST!='MT',]
Wilcoxtest <- pairwise_wilcox_test( STmed ~ SESSN, data=MediansX)


#####################################################
### Polsby Medians by ST
####################################################
Global.median <- median( CDx$Polsby)
Medians<- CDx %>% group_by(ST)%>%summarise( Pmedian= median( Polsby))
PlotData <- left_join( CDx, Medians)

kruskal.test( Polsby ~ ST, data = PlotData)  #### NULL is no diffrence in medians


Plot <- ggplot(PlotData, aes(x =reorder(ST, Pmedian) , y = Polsby)) +
  geom_boxplot(aes( fill= as.factor( CReq))) +
  labs(title='Polsby Popper', y = "Polsby Popper score", x = "State in order of increasing district compactness", fill="Compactness \n requirement") +
  geom_hline(yintercept=Global.median, linetype="dashed",color = "red", size=1)+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  scale_fill_discrete(labels = c("None", "Required"))+
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90)) +
  theme( plot.title = element_text(vjust = -10, hjust = 0.5) ) +
  #theme(legend.position = "inside", legend.position.inside = c(0.9, 0.1)) +
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )
Plot
#setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
#ggsave( file="CDST_polsby_box.eps", plot=Plot, width=6, height=3, units="in")

#### Moody Median test
Mtest <- Median.test(  PlotData$Polsby, PlotData$ST, simulate.p.value= TRUE)
Mtest$statistics
States <- row.names( Mtest$groups)
Mgroups <- data.frame( States, Mtest$groups)
colnames( Mgroups)<- c("ST", "Pmedian", "Groups")
#Mgroups <- dplyr:: select(  all_of(Mgroups, States, groups))
#colnames( Mgroups)<- c( "ST", "Groups")
PlotData <- left_join( PlotData, Mgroups)

Plot <- ggplot(PlotData, aes(x =reorder(ST, Pmedian) , y = Polsby)) +
  geom_boxplot(aes( fill= as.factor( Groups))) +
  labs(y = "Polsby Popper score", x = "ST in order of increasing district compactness", fill="Indifferent \n medians") +
  geom_hline(yintercept=Global.median, linetype="dashed",color = "red", size=1)+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  #scale_fill_manual(values=c('blue',"purple", "red" ), labels=c("A","Either", "B"))+
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 90, size=8)) +
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )
Plot

setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
ggsave( file="CDST_polsby_box.eps", plot=Plot, width=6, height=3, units="in")

#########################################################################
###### Wilcox test Polsby
##########################################################################
Wilcoxtest <- pairwise_wilcox_test( Polsby~ST, data =PlotData)
Wilcoxtest

Plotdata1 <- Wilcoxtest%>%select( 'group1', 'group2','p.adj')
Plotdata1 <- Plotdata1 %>% mutate( Sig = ifelse( p.adj < 0.05,1,0))
Counts<-     Plotdata1%>%group_by(group1)%>%summarise( Nbad = sum( Sig ))
colnames( Counts)<- c( "ST","Nbad")
Counts2<-  Plotdata1%>%group_by(group2)%>%summarise( Nbad = sum( Sig ))
colnames( Counts2)<- c( "ST","Nbad")
Countsall <- merge( Counts, Counts2, by="ST")
#Plotdata1 <- left_join(Plotdata1, Counts)

Countsall <- Countsall %>% mutate( Nbad.tot = Nbad.x + Nbad.y)

print( Countsall[ Countsall$Nbad.tot>0,c(1,4)])
Nbad <- sum( Plotdata1$Sig)
Pbad <- Nbad/nrow(Plotdata1)
print( paste( Nbad ," Significantly different state pairs;",Pbad," of all state pairs"))

States <- unique(PlotData$ST)
Testout <- data.frame()
for( S in States){
  Base <- filter( PlotData, ST!=S)
  Group <- filter( PlotData, ST==S)
  Test <- wilcox.test( Base$Polsby, Group$Polsby)
  if( Test$p.value < 0.05) { print( paste( S, Test$p.value))}
  Tdata <- data.frame( S, Test$p.value)
  Testout <- rbind( Testout, Tdata)
}
sum( Testout$Test.p.value<0.05)
sum( Testout$Test.p.value<0.05) / nrow( Testout)
Testout[ Testout$Test.p.value<0.05,]

####################################################
### Polsby by Compactness and other variables
###################################################
Crq <- CDx%>%filter( CReq==1)
NCrq <- CDx%>%filter( CReq==0)

Mtest <- Median.test(  CDx$Polsby, CDx$CReq, simulate.p.value= TRUE)
Mtest$statistics

Mtest <- Median.test(  CDx$Polsby, CDx$Coast, simulate.p.value= TRUE)
Mtest$statistics

###################################################################
### Reock Medians by Session
######################################################################
PlotData <- CDx

Global.median <- median( PlotData$Reock)

Plot <- ggplot(PlotData, aes(x = as.factor(SESSN) , y = Reock)) +
  geom_boxplot(aes( fill= as.factor(SESSN))) +
  labs( x = "Session", fill="") +
  geom_hline(yintercept=Global.median, linetype="dashed",color = "red", size=1)+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  theme(text = element_text(size=12)) +
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )
Plot
setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
ggsave( file="CDSESSN_reock_box.eps", plot=Plot, width=6, height=3, units="in")

Plot <- ggplot(PlotData, aes(x = as.factor(SESSN) , y = Reock)) +
  geom_violin(aes( fill= as.factor(SESSN)), scale='count', draw_quantiles=c(0.25,0.5,0.75)) +
  labs( x = "Session", fill="") +
  geom_hline(yintercept=Global.median, linetype="dashed",color = "red", size=1)+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  theme(text = element_text(size=12), legend.position = "none") +
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )
Plot
setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
ggsave( file="CDSESSN_reock_box.eps", plot=Plot, width=6, height=3, units="in")

## Calc ranges
max( CDx$Reock[CDx$SESSN==118]) - min(CDx$Reock[ CDx$SESSN==118])
max( CDx$Reock[CDx$SESSN==113]) - min(CDx$Reock[ CDx$SESSN==113])
max( CDx$Reock[CDx$SESSN==108]) - min(CDx$Reock[ CDx$SESSN==108])

Test <- kruskal.test( Reock ~ SESSN, data = PlotData)  #### NULL is no difference in medians
Test  ###Result  Not all  the same

Mtest <- Median.test(  PlotData$Reock, PlotData$SESSN, simulate.p.value= TRUE)
Mtest$statistics  ### 118 is diff

Wilcoxtest <- pairwise_wilcox_test( Reock~SESSN, data =PlotData)
Wilcoxtest   ### they ae all diff

Medians <- CDx%>%group_by(ST, SESSN)%>%summarise( STmed=median(Reock),STmin=min(Reock))

Plot <- ggplot(Medians, aes(x = as.factor(SESSN) , y = STmin)) +
  geom_boxplot(aes( fill= as.factor(SESSN))) +
  labs( x = "Session", fill="") +
  geom_hline(yintercept=Global.median, linetype="dashed",color = "red", size=1)+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  theme(text = element_text(size=12)) +
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )
Plot

Test <- kruskal.test( STmed ~ SESSN, data = Medians)  #### NULL is no difference in medians
Test  ###Result  Not all  the same

Mtest <- Median.test(  Medians$STmed, Medians$SESSN, simulate.p.value= TRUE)
Mtest$statistics  ### 118 is diff

MediansX <- Medians[ Medians$ST!='MT',]
MediansX <- MediansX[, c( 'SESSN', 'ST', 'STmed')]
colnames( MediansX)<- c( 'SESSN', 'ST', 'Medians')
Wilcoxtest <- pairwise_wilcox_test( Medians ~ SESSN, data=MediansX)
Wilcoxtest


#####################################################
### Reock by ST - 
####################################################
PlotData <- CDx

Global.median <- median(PlotData$Reock)
Medians<- PlotData %>% group_by(ST)%>%summarise( Pmedian= median( Reock))
PlotData <- left_join( PlotData, Medians)

Plot <- ggplot(PlotData, aes(x =reorder(ST, Pmedian) , y = Reock)) +
  geom_boxplot(aes( fill= as.factor( CReq))) +
  labs(title='Reock', y = "Reock score", x = "State in order of increasing district compactness", fill="Compactness \n requirement") +
  geom_hline(yintercept=Global.median, linetype="dashed",color = "red", size=1)+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  scale_fill_discrete(labels = c("None", "Required"))+
  theme( plot.title = element_text(vjust = -10, hjust = 0.5) ) +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90)) +
  #theme(legend.position = "inside", legend.position.inside = c(0.9, 0.1)) +
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )
Plot
#setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
#ggsave( file="CDST_reock_box.eps", plot=Plot, width=6, height=3, units="in")



setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
ggsave( file="CDST_reock_box.eps", plot=Plot, width=6, height=3, units="in")

#######################################################################
###### Wilcox test Reock
######################################################################
kruskal.test( Reock ~ ST, data = PlotData) 

States <- unique(PlotData$ST)
Testout <- data.frame()
for( S in States){
  Base <- filter( PlotData, ST!=S)
  Group <- filter( PlotData, ST==S)
  Test <- wilcox.test( Base$Reock, Group$Reock)
  if( Test$p.value < 0.05) { print( paste( S, Test$p.value))}
  Tdata <- data.frame( S, Test$p.value)
  Testout <- rbind( Testout, Tdata)
}
sum( Testout$Test.p.value<0.05)
sum( Testout$Test.p.value<0.05)/nrow( Testout)




Wilcoxtest <- pairwise_wilcox_test( Reock~ST, data =PlotData)
Wilcoxtest

Plotdata1 <- Wilcoxtest%>%select( 'group1', 'group2','p.adj')
Plotdata1 <- Plotdata1 %>% mutate( Sig = ifelse( p.adj < 0.05,1,0))
Counts<-     Plotdata1%>%group_by(group1)%>%summarise( Nbad = sum( Sig ))
colnames( Counts)<- c( "ST","Nbad")
Counts2<-  Plotdata1%>%group_by(group2)%>%summarise( Nbad = sum( Sig ))
colnames( Counts2)<- c( "ST","Nbad")
Countsall <- merge( Counts, Counts2, by="ST")
#Plotdata1 <- left_join(Plotdata1, Counts)

Countsall <- Countsall %>% mutate( Nbad.tot = Nbad.x + Nbad.y)

print( Countsall[ Countsall$Nbad.tot>0,c(1,4)])
Nbad <- sum( Plotdata1$Sig)
Pbad <- Nbad/nrow(Plotdata1)
print( paste( Nbad ," Significantly different state pairs;",Pbad," of all state pairs"))


Medians<- PlotData %>% group_by(ST)%>%summarise( Pmedian= median( Reock))
colnames( Medians)<- c('group1','Pmedian')
Plotdata1 <- left_join( Plotdata1, Medians)
colnames( Medians)<- c('group2','Pmedian2')
Plotdata1 <- left_join( Plotdata1, Medians)

Plotdata2 <- data.frame( Plotdata1$group2, Plotdata1$group1, Plotdata1$p.adj, Plotdata1$Sig, Plotdata1$Pmedian2, Plotdata1$Pmedian)
colnames( Plotdata2)<- colnames(Plotdata1)
Plotdata <- rbind( Plotdata1, Plotdata2)

ggplot(Plotdata, aes(x = reorder(group1, Pmedian), y = reorder(group2, Pmedian2), fill = as.factor( Sig )) ) +
  geom_tile() +
  labs( x="In order of increasing district compactness", y="In order of increasing district compactness", fill="", title="Wilcox paired comparison test - Reock")+
  scale_fill_manual( values=c("lightblue","red" ),labels=c("Similar","Different"))+
  theme(text = element_text(size=14), axis.text.x = element_text(angle = 90))+
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )

####################################################
### Reock by Compactness and other variables
###################################################
Crq <- CDx%>%filter( CReq==1)
NCrq <- CDx%>%filter( CReq==0)

Mtest <- Median.test(  CDx$Reock, CDx$CReq, simulate.p.value= TRUE)
Mtest$statistics

Mtest <- Median.test(  CDx$Reock, CDx$Coast, simulate.p.value= TRUE)
Mtest$statistics

