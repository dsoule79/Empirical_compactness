##### Explore outliers from model

library(dplyr)
library(fitdistrplus)
library(ggplot2, svglite)
library(sf)
library(ggspatial)
library( corrgram)
library(betareg)
library( rstatix)
library( agricolae)
rm(list=ls())    #  remove all previous variables and value
options(digits = 4)

###################################################################
### Get district  data
##################################################################
Dpath <- "C:/Users/Owner/Compactness/Papercode/Process data"
setwd(Dpath)
Fname <-"Compiled_data.csv"
CDx <- read.csv( file=Fname)

###############################################################
#### Use regression model to calc relative compactness
#################################################################
Dpath <- "C:/Users/Owner/Compactness/Papercode/Process data"
setwd(Dpath)
Fname <-"ModelParams_Pby_Crq_Best.csv"
Model <- read.csv( file=Fname)
Model

Intercept <- Model$Estimate[1]
Coeff_Cnt_Pby_Avg <- Model$Estimate[2]
Coeff_RSTPolsby <- Model$Estimate[3]
Phi_intercept <- Model$Estimate[4]

CDx <- CDx%>% mutate( Mu = Intercept + Coeff_Cnt_Pby_Avg * Cnt_Pby_Avg + Coeff_RSTPolsby * RSTPolsby )
CDx <- CDx%>% mutate( Phi = Phi_intercept )
CDx <- CDx%>% mutate( Pdistrict = pbetar( Polsby,Mu,Phi))

PPecdf <- ecdf( CDx$Polsby)
CDx <- CDx%>%mutate( PPrank = PPecdf(Polsby))  ### empirical probability as is

##############################################
#### Calc states- Sessn that are outliers on an overall map basis
########################################
ST_SESSN.dat <- data.frame( character(), character(), numeric(), numeric(), numeric(), numeric(), numeric(), numeric() )
Tnames <- c( "SESSN","ST","Pstate", "MinDisPval", "MinDisPolsby", "STpolsby","Cnt_Pby_Avg","Max_RSTPolsby")
SNs <- unique( CDx$SESSN)
STs <- unique ( CDx$ST)

for ( Sn in SNs){
  for( St in STs){
    STdata <- filter( CDx, SESSN==Sn & ST==St)
    if( nrow(STdata) < 2) next
    MinDpolsby <- min( STdata$Polsby)
    MinDpval <- min( STdata$Pdistrict)
    Ptest <- ks.test( STdata$Pdistrict, punif, alternative='greater')$p.value
    ST.test <- data.frame( Sn, St, Ptest, MinDpval, MinDpolsby, STdata$STpolsby[1], STdata$Cnt_Pby_Hvg[1], max(STdata$RSTPolsby) )
    colnames(ST.test)<- Tnames
    ST_SESSN.dat <- rbind( ST_SESSN.dat, ST.test)
  }
}
ST_SESSN.dat[ST_SESSN.dat$Pstate<0.01,] 

##############################################
### Load known gerrymanders
##############################################
Gerrys <- read.csv( file="Interval_Gerrymanders_plus.csv")
Gerry.dat <- left_join( ST_SESSN.dat, Gerrys)

I <- which( Gerry.dat$SESSN==113 & Gerry.dat$ST=="IL")
Gerry.dat$Gerryd[I]<- 1

###############################
### ROC analysis
################################
IDaccurate <- function(T){ 
  ID <- ""
  Indx <- which( Gdat$Metric<T & Gdat$GD==1)
  for ( i in Indx ){
    ID <- paste( ID, paste0( Gdat$SESSN[i],Gdat$ST[i]))
    }
  return( ID)
}

IDwrong <- function(T){ 
  ID <- ""
  Indx <- which( Gdat$Metric<T & Gdat$GD==0)
  for ( i in Indx ){
    ID <- paste( ID, paste0( Gdat$SESSN[i],Gdat$ST[i]))
  }
  return( ID)
}


Gdat<- Gerry.dat
Gdat <- Gdat%>%select( c( SESSN, ST, Pstate, MinDisPval, Gerryd, Splitd))
Gdat <- filter( Gdat, SESSN!=108)
Gdat$Metric <- Gdat$MinDisPval   ### Test metric for ROC analysis
#Gdat$Metric <- Gdat$Pstate
Gdat$GD <- Gdat$Gerryd

Thresholds <- seq( 0,1,0.001)
Nts <- length( Thresholds)
N <- nrow( Gdat)
Nout <- sum( Gdat$GD)
Ngood <- N - Nout
ROC <- matrix( NA, Nts,7)
colnames(ROC) <- c( "Accuracy","Precison","TPR","FPR", "Threshold", "Ntrue","Nwrong")
States <- matrix( NA, Nts,2)
colnames( States) <- c( "Accurate","FalsePositive")

print( paste(N, Nout))

for( i in 1:Nts){
  T <- Thresholds[i]
  Nflagged <- length( which( Gdat$Metric<T))
  Nflagged.accurate <- length( which( Gdat$Metric<T & Gdat$GD==1))
  Nflagged.wrong <- length( which( Gdat$Metric<T & Gdat$GD==0))
  Npassed.accurate <- length( which( Gdat$Metricl>T & Gdat$GD==0))
  ROC[i,1] <- Accuracy <- ( Nflagged.accurate + Npassed.accurate) / N
  ROC[i,2] <- Precision <- Nflagged.accurate/Nflagged
  if ( is.na(Precision)) { ROC[i,2] <- Precision <- 0}
  ROC[i,3] <- TruePos <- Nflagged.accurate/Nout
  ROC[i,4] <- FalsePos <- Nflagged.wrong/Ngood
  ROC[i,5] <- T
  ROC[i,6] <- Nflagged.accurate
  ROC[i,7] <- Nflagged.wrong
  if( Nflagged.accurate >0) States[i,1]<- IDaccurate(T)
  if( Nflagged.wrong>0) States[i,2]<- IDwrong(T)
  }
ROC <- as.data.frame( ROC)
States <- as.data.frame( States)
Rocdata <- cbind( ROC, States) 
F1name <- paste0("ROCdata-MinDis-",Fname)
F1name <- paste0("ROCdata-STpval-",Fname)
write.csv( file=F1name, Rocdata)

ggplot( data=ROC, aes( x=FPR, y=TPR)) +
      geom_line() +
      geom_abline( color="blue")
      

ggplot( data=ROC, aes( x=Threshold, y=Precison)) + geom_line() +
        theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )

N <- nrow( ROC)
AUC <- 0
for ( i in 1:(N-1)){
  if((ROC$FPR[i+1] - ROC$FPR[i])>0 ){
    AUC <- AUC + 0.5 *(ROC$FPR[i+1] - ROC$FPR[i]) * (ROC$TPR[i+1]+ROC$TPR[i])
  }}
AUC

###########################################
### Choose threshold and analyze 
###########################################
ROC[1:30,]

Tdis <- 0.016
Tst <- 0.027
Tdat <- Gdat
Tdat <- Tdat %>%mutate( STflag = ifelse( Pstate<Tst,"ST bias",""))
Tdat <- Tdat %>%mutate( MinDflag = ifelse( MinDisPval<Tdis,"Extreme district",""))
Tdat <- Tdat%>% mutate( Flagged= ifelse( (STflag =='ST bias')| (MinDflag =="Extreme district"),1,0) )
#Tdat <-Tdat%>%select(-Gerryd)

Indx <- which( Tdat$Flagged==1 | Tdat$GD==1)
ROCanalysis <- Tdat[Indx,]
ROCanalysis

Dpath <- "C:/Users/Owner/Compactness/Papercode/Paper data"
setwd(Dpath)
Fname2<- paste0( 'ROCanalysis' ,'-',Fname)
Fname2
write.csv( file=Fname2,ROCanalysis, row.names = FALSE )

##########################################################################
#### plot QQ plot
#################################################
### 50% point KS 118
### 99% point MI118, CA118

ST <- "OR"
SN <- "118"

Plotdata_all <- NA
STdata <- CDx[ CDx$ST==ST & CDx$SESSN==SN,]
Bins <- hist( STdata$Pdistrict, breaks=seq(0,1,0.05), plot=FALSE)
Plotdata<- data.frame( Bins$mids, Bins$counts, paste("118",ST))
colnames(Plotdata)<- c( "Bin","Count", "ST")
Totcount <- sum( Plotdata$Count)
Plotdata <- Plotdata%>%mutate( Delta.per = Count/ Totcount)
Plotdata <- Plotdata%>%mutate( Cum.Count = Count)
for ( i in 2:nrow(Plotdata)) { Plotdata$Cum.Count[i] <- Plotdata$Cum.Count[i] +  Plotdata$Cum.Count[i-1] }
Plotdata <- Plotdata%>%mutate( Cum.per = Cum.Count/ Totcount)
Plotdata_all <- rbind( Plotdata_all, Plotdata)
     
Plotdata_all <- Plotdata_all[-1,]

Ptest <- ks.test( STdata$Pdistrict, punif, alternative='greater')$p.value
Title <- paste(ST, SN,"-", format(round(Ptest,4), nsmall=4), "- Pval map compactness")
Fname <- paste0( 'PvalQQ-',ST, SN,".svg")

Plot <- ggplot(Plotdata_all, aes( x = Bin)) +              
  geom_bar( aes(y=Delta.per), position = "dodge", stat="identity", color ="grey")+
  geom_line( aes(y=Cum.per), color ="red") +
  geom_line( aes( y= Bin), color ="blue") +
  scale_x_continuous( breaks=seq(0,1,0.2))+
  scale_y_continuous( breaks=seq(0,1,0.2))+
    labs( x="Adjusted compactness percentile",
          y ="Frequency of districts",
          title=Title) +
  theme(text = element_text(size=12)) +
  #theme(legend.position="inside",legend.position.inside = c(0.1,0.9)) +
  theme(plot.margin =  unit(c(0,0.2,0,0.2),"cm") )

Plot

#############################################################
### Plot outliers by state and Session
#########################################################
SN <- "118"
STdata <- filter( ST_SESSN.dat, SESSN== SN)
States <-STdata$ST[ STdata$STpval < 0.05]
#States2 <- unique( Outliers$ST[ Outliers$P<0.05 & Outliers$SESSN== SN] )
#States <- union( States, States2)
Shapedata <- select( CDs, ST, SESSN, DISTRICT,  geometry)
Shapedata <- filter( Shapedata, SESSN== SN)
Plotdata <- left_join( Shapedata, Outliers)
Plotdata <- Plotdata %>% mutate( Out = ifelse( P <=0.05,1,0))
Plotdata <- left_join( Plotdata, STdata)

for( State in States) {
  Pdata <- filter( Plotdata, ST==State )
  #Title <- paste( SN, State,"Min district pval=", format(min(Pdata$P),digits=3) )
  Title <- paste( SN, State )
  Fname <- paste0( "Outliers-",SN,"-",State,".svg")
  Plot <- ggplot(Pdata) +
    geom_sf( aes(fill = as.factor(Out)) ,colour="white", linewidth=0.5) +
    #facet_grid( cols=vars(SESSN))+
    scale_fill_manual(values=c('lightblue',"red" ), labels=c("Normal","Outlier"))+
    labs( title= Title, fill="Outlier @ 5%" ) +
    theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )
  ggsave( file=Fname, plot=Plot, width=6, height =6, units="in")
  
  Bins <- hist( Pdata$P, breaks=seq(0,1,0.05), plot=FALSE)
  QQdata<- data.frame( Bins$mids, Bins$counts)
  colnames(QQdata)<- c( "Bin","Count")
  Totcount <- sum( QQdata$Count)
  QQdata <- QQdata%>%mutate( Delta.per = Count/ Totcount)
  QQdata <- QQdata%>%mutate( Cum.Count = Count)
  for ( i in 2:nrow(QQdata)) { QQdata$Cum.Count[i] <- QQdata$Cum.Count[i] +  QQdata$Cum.Count[i-1] }
  QQdata <- QQdata%>%mutate( Cum.per = Cum.Count/ Totcount)
  Title <- paste(State, SN,"-", format(round(Pdata$STpval[1],4), nsmall=4), "- Pval map compactness")
  Fname <- paste0( 'PvalQQ-',State, SN,".svg")
  Plot <- ggplot(QQdata, aes( x = Bin)) +              
    geom_bar( aes(y=Delta.per), position = "dodge", stat="identity", color ="grey")+
    geom_line( aes(y=Cum.per), color ="red") +
    geom_line( aes( y= Bin), color ="blue") +
    scale_x_continuous( breaks=seq(0,1,0.1))+
    scale_y_continuous( breaks=seq(0,1,0.1))+
    labs( x="Probability of district compactness", y ="Frequency of districts", title = Title) +
    theme(text = element_text(size=14)) +
    theme(legend.position="inside",legend.position.inside = c(0.1,0.9)) +
    theme(plot.margin =  unit(c(0,0.2,0,0.2),"cm") )
  Plot
  ggsave( file=Fname, plot=Plot, width=6, height = 6, units="in")
}

##############################################
#### Calc states that are outliers on an overall state basis
########################################
Test.data <- data.frame( character(), numeric())
Tnames <- c( "ST","STpval")
STs <- unique ( CDx$ST)


for( St in STs){
  STdata <- filter( CDx, ST==St)
  Ptest <- ks.test( STdata$P, punif, alternative='greater')$p.value
  ST.test <- data.frame( St, Ptest)
  colnames(ST.test)<- Tnames
  Test.data <- rbind( Test.data, ST.test)
}

print( Test.data[ Test.data$STpval<=0.05, ])
hist( Test.data$STpval, breaks=seq(0,1,0.05))

setwd("C:/Users/Owner/OneDrive/Redistricting/Analysis/Beta Regressions")
write.csv( file="STpvalsbyST.csv", Test.data, row.names = FALSE )



#################################
### Plot State with outlier by SESSN & ST
#################################
St <- "MS"
Sn <- "118"
Shapedata <- select( CDs, ST, SESSN, DISTRICT,  geometry)
Shapedata <- filter( Shapedata, SESSN== Sn & ST==St )
Plotdata <- left_join( Shapedata, Ranks)
Plotdata <- Plotdata %>% mutate( Out = ifelse( P<0.05,1,0))

STpval <- select( ST_SESSN.dat, SESSN, ST, STpval)
Plotdata <- left_join( Plotdata, STpval)

Title <- paste( Sn, St )
Title <- paste( Sn, St , "ST bias pval =", format(Plotdata$STpval[1], digits=3))
Subtitle <- paste( "ST Polsby=", format(Plotdata$STpolsby[1],digits=2), "County avg Polsby=", format(Plotdata$CSTavg[1],digits=2) )
Fname <- paste0( "Map outliers",Sn,"-",St,".svg")
Plot <- ggplot(Plotdata) +
  geom_sf( aes(fill = as.factor(Out)) ,colour="white", linewidth=0.5) +
  scale_fill_manual(values=c('lightblue',"red" ), labels=c("Others","Outlier"))+
  labs( title= Title,subtitle=Subtitle, fill="" ) +
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )
Plot
ggsave( file=Fname, plot=Plot, width=6, height = 6, units="in")



########### Plot state with outlier
Iplot <- 879

Indx <- which( Ranks$SESSN=="118" & Ranks$P <0.05 )

for( Iplot in Indx){
  St <- Ranks$ST[Iplot]
  #St <- "IL"
  Sn <- Ranks$SESSN[Iplot]
  #Sn <- "118"
  Ds <- Ranks$DISTRICT[Iplot]
  print( paste( St, Sn, Ds))
  Dpolsby <- Ranks$PolsbyW[Iplot]
  Pasis <- Ranks$ASISpercentile[Iplot]
  Padj <- Ranks$P[Iplot]
  Shapedata <- select( CDs, ST, SESSN, DISTRICT,  geometry)
  Shapedata <- filter( Shapedata, SESSN== Sn & ST==St )
  Plotdata <- left_join( Shapedata, Ranks)
  Plotdata <- Plotdata %>% mutate( Out = ifelse(  DISTRICT==Ds,1,0))
  
  Title <- paste( Sn, St, Ds, "As is percentile=", format(Pasis, digits=2),"Adjusted =", format(Padj, digits=2) )
  Subtitle <- paste( "ST Polsby=", format(Plotdata$STpolsby[1],digits=2), "County avg Polsby=", format(Plotdata$CSTavg[1],digits=2), "District polsby=", format(Dpolsby,digits=2) )
  Fname <- paste0( "Percentile-",Sn,"-",St,"-", Ds,".svg")
  Plot <- ggplot(Plotdata) +
    geom_sf( aes(fill = as.factor(Out)) ,colour="white", linewidth=0.5) +
    scale_fill_manual(values=c('lightblue',"red" ), labels=c("Others","ADJ district"))+
    labs( title= Title,subtitle=Subtitle, fill="" ) +
    theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )
  Plot
  ggsave( file=Fname, plot=Plot, width=6, units="in")
}


##############################################
#### Calc states- Sessn that are outliers on an overall map basis
########################################
Test.data <- data.frame( character(), character(), numeric(), numeric(), numeric())
Tnames <- c( "SESSN","ST","STpval","STpolsby", "CSTavg")
SNs <- unique( CDx$SESSN)
STs <- unique ( CDx$ST)

for ( Sn in SNs){
  for( St in STs){
    STdata <- filter( CDx, SESSN==Sn & ST==St)
    if( nrow(STdata) < 2) next
    Ptest <- ks.test( STdata$P, punif, alternative='greater')$p.value
    ST.test <- data.frame( Sn, St, Ptest, STdata$STpolsby[1], STdata$CSTavg[1] )
    colnames(ST.test)<- Tnames
    Test.data <- rbind( Test.data, ST.test)
  }
}

ST_SESSN.dat <- Test.data

### ST pal histogram
Plot <- ggplot( data=Test.data, aes( x=STpval))+
  geom_histogram(  aes( y= 100*..count../sum( ..count..)), breaks=seq(0,1,0.05), color="lightgrey") +
  labs( y="Percent of observations", x="Pval for state map is compact" )+
  coord_cartesian(xlim = c(0, 1.0), ylim=c(0,15)) +
  theme(text = element_text(size=14))+
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )
Plot
ggsave( file="ST pval hist.svg", plot=Plot, width=6, height=6, units="in")

print( paste( length( which( Test.data$STpval<0.05)),"maps below 5% of", nrow(Test.data)))
length( which( Test.data$STpval<0.05))/ nrow( Test.data)

print( paste( length( which( Test.data$STpval<0.01)),"maps below 1% of", nrow(Test.data)))
length( which( Test.data$STpval<0.01))/ nrow( Test.data)

#### Calc number of individual outliers
CDx <- CDx%>% mutate( Dout = ifelse( P<0.05,1,0))
Dists.out <- CDx%>%group_by( SESSN, ST)%>%summarise( min(P), sum(Dout), mean(PolsbyW))
ST_SESSN.dat <- left_join( ST_SESSN.dat, Dists.out)
colnames( ST_SESSN.dat)<- c( "SESSN","ST","STpval","STpolsby","CSTavg","MinDP","Dout","AvgDcompact")

ST_SESSN.dat <- ST_SESSN.dat[ order( ST_SESSN.dat$ST),]
ST_SESSN.dat <- ST_SESSN.dat[,c(1,2,4,5,3,7,6,8)]

write.csv( file="ST-SESSN-data.csv", ST_SESSN.dat, row.names = FALSE )

Indx <- which( ST_SESSN.dat$STpval<0.01 | ST_SESSN.dat$Dout>0)
write.csv( file="ST-SESSN-data-outliers.csv", ST_SESSN.dat[Indx,], row.names = FALSE )

### ST pal histogram
Plot <- ggplot( data=Test.data, aes( x=STpval))+
  geom_histogram(  aes( y= 100*..count../sum( ..count..)), breaks=seq(0,1,0.05), color="lightgrey") +
  labs( y="Percent of observations", x="Pval for state map is compact" )+
  coord_cartesian(xlim = c(0, 1.0), ylim=c(0,15)) +
  theme(text = element_text(size=14))+
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )
Plot
ggsave( file="ST pval hist.svg", plot=Plot, width=6, height=6, units="in")

print( paste( length( which( Test.data$STpval<0.05)),"maps below 5% of", nrow(Test.data)))
length( which( Test.data$STpval<0.05))/ nrow( Test.data)

print( paste( length( which( Test.data$STpval<0.01)),"maps below 1% of", nrow(Test.data)))
length( which( Test.data$STpval<0.01))/ nrow( Test.data)



