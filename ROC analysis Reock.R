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
Fname <-"ModelParams_Reock_Crq_Null.csv"
Model <- read.csv( file=Fname)
Model

Intercept <- Model$Estimate[1]
Phi <- Model$Estimate[2]

CDx <- CDx%>% mutate( Mu = Intercept )
CDx <- CDx%>% mutate( Pdistrict = pbetar( Reock,Mu,Phi))

##############################################
#### Calc states- Sessn that are outliers on an overall map basis
########################################
ST_SESSN.dat <- data.frame( character(), character(), numeric(), numeric(), numeric(), numeric() )
Tnames <- c( "SESSN","ST","Pstate","MinDisPval", "MinDisReock", "STReock")
SNs <- unique( CDx$SESSN)
STs <- unique ( CDx$ST)

for ( Sn in SNs){
  for( St in STs){
    STdata <- filter( CDx, SESSN==Sn & ST==St)
    if( nrow(STdata) < 2) next
    MinDisReock <- min( STdata$Reock)
    MinDpval <- min( STdata$Pdistrict)
    Ptest <- ks.test( STdata$Pdistrict, punif, alternative='greater')$p.value
    ST.test <- data.frame( Sn, St, Ptest, MinDpval, MinDisReock, STdata$STreock[1] )
    colnames(ST.test)<- Tnames
    ST_SESSN.dat <- rbind( ST_SESSN.dat, ST.test)
  }
}

ST_SESSN.dat[ ST_SESSN.dat$Pstate<0.01,]

#ST_SESSN.dat[ ST_SESSN.dat$MinDisPval<0.002,]

##############################################
### Load known gerrymanders
##############################################
Gerrys <- read.csv( file="Interval_Gerrymanders_plus.csv")
Gerry.dat <- left_join( ST_SESSN.dat, Gerrys)

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
#F1name <- paste0("ROCdata-STpval-",Fname)
write.csv( file=F1name, Rocdata)

ggplot( data=ROC, aes( x=FPR, y=TPR)) +
      geom_line() +
      geom_abline( color="blue")
      
ggplot( data=ROC, aes( x=Threshold, y=Precison)) + geom_line() +
        theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )

###########################################
### Choose threshold and analyze 
###########################################
ROC[1:30,]
ROC[31:80,]
ROC[81:140,]

Tdis <- 0.002
Tst <- 0.000
Tdat <- Gdat
Tdat <- Tdat %>%mutate( STflag = ifelse( Pstate<Tst,"ST bias",""))
Tdat <- Tdat %>%mutate( MinDflag = ifelse( MinDisPval<Tdis,"Extreme district",""))
Tdat <- Tdat%>% mutate( Flagged= ifelse( Pstate<Tst | MinDisPval<Tdis ,1,0) )
#Tdat <-Tdat%>%select(-Gerryd)

Indx <- which( Tdat$Flagged==1 | Tdat$GD==1)
ROCanalysis <- Tdat[Indx,]
ROCanalysis

Dpath <- "C:/Users/Owner/Compactness/Papercode/Paper data"
setwd(Dpath)
Fname2<- paste0( 'ROCanalysis' ,'-',Fname)
Fname2
write.csv( file=Fname2,ROCanalysis, row.names = FALSE )


