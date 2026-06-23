#################################################################################
###  Load district master data with three decades of data
###  Perform Beta regressions
###  Analyze residuals
###  export coefficients of best model for further use
#################################################################################

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

###############################################################
### Functions
#################################################################
Format_summary <- function( Model) {
  X <- summary( Model)
  Mean <- X$coefficients$mean
  Precision <- X$coefficients$precision
  row.names(Precision) <- paste0( 'Phi:',row.names(Precision) )
  Pmodel <- rbind( Mean, Precision)
  Model_parms<-  data.frame( c( X$pseudo.r.squared, AIC(Model), BIC(Model)), rep(NA,3), rep(NA,3), rep(NA,3))
  row.names( Model_parms)<- c( 'Rsq', 'AIC','BIC')
  colnames(Model_parms) <- colnames( Pmodel)
  Pmodel <- rbind( Pmodel,Model_parms)
  
}

BIC <- function( Fit){
  BIC <- ( Fit$df.null- Fit$df.residual)*log( Fit$n)- 2*Fit$loglik
  return( BIC)
}

###################################################################
### Get district  data
##################################################################
Dpath <- "C:/Users/Owner/Compactness/Papercode/Process data"
setwd(Dpath)
Fname <-"Compiled_data.csv"
CDx <- read.csv( file=Fname)

Reg_var <-c( "ST","SESSN","Reock", "STreock","STLarea" , "STLarea_Nd",     
             "POP_REP", "Pop_Den","LPop_Den","Nd","LNd",
             "Cnt_Rk_Avg", "Cnt_Rk_Gvg", "Cnt_Rk_Hvg","Cnt_Rk_Min", "Cnt_Rk_Med",
             "S108", "S113", "S118", "Decade", "RSTPerim" ,   
             "RCoast" , "Coast",
             "STreock2", "Cnt_Rk_Avg2", "Cnt_Rk_Gvg2", "Cnt_Rk_Hvg2", "Cnt_Rk_Med2",
             "Cnt_Rk_SDI" , 
             "CReq"  )

Modeldata <- select( CDx, all_of(Reg_var))

##############################################
### Remove states without compact req
############################################
nrow(Modeldata)
Modeldata <- Modeldata%>%filter( CReq==1)
nrow(Modeldata)


########################################################
### Beta regressions
########################################################
setwd("C:/Users/Owner/Compactness/Papercode/Paper data")  #### location of model summaries

################################################################################
### Beta regression ALL
##############################################################################
Id <- make.link('identity')      ### will be used by Betareg to overwrite the logit link function
Ws <- 1/Modeldata$Nd
Beta_ALL <-
  betareg( Reock ~ 1, 
           weights = Ws,
           data = Modeldata,
           link = Id
  )

Means_base <- Beta_ALL$coefficients$mean[1] 

#################################################################
#### Compare regression without one Session
##############################################################
Testsummary<- data.frame()

for ( S in unique( Modeldata$SESSN)){
  Testdata <- filter( Modeldata, SESSN!=S)
  Id <- make.link('identity')      ### will be used by Betareg to overwrite the logit link function
  Ws <- 1/Testdata$Nd
  Beta_Test <-
    betareg( Reock ~ 1, 
             weights = Ws,
             data = Testdata,
             link = Id
    )
  Means_test <- Beta_Test$coefficients$mean[1] 
  Compare <- data.frame( SESSN=Modeldata$SESSN, ST=Modeldata$ST,  Test=Means_test, Base=Means_base)
  Compare <- Compare%>% mutate( Diff = abs( Test - Base )) 
  Compare <- Compare%>% mutate( Pdiff = Diff/Test )
  #summary( Compare)
  Compare_ST <-Compare %>% group_by( SESSN, ST)%>% summarise( MaxDiff = max( Diff), MaxPdiff = max(Pdiff))
  Indx<- which( Compare_ST$MaxPdiff>0.05 )
  print( paste( S,length( Indx), max( Compare_ST$MaxPdiff) ) )
  if (length( Indx)>0) {
    Testout <- data.frame( Test=paste0('X-',S), Compare_ST[Indx,] )
    Testsummary <- rbind( Testsummary, Testout)
    }
  Testmaxes <- data.frame(Test=paste0('X',S), SESSN="Maxes",ST='',MaxDiff=max(Compare_ST$MaxDiff), MaxPdiff=max(Compare_ST$MaxPdiff) )
  Testsummary <- rbind( Testsummary, Testmaxes)
  } ### end loop

#### With each state missing
for ( S in unique( Modeldata$ST)){
  Testdata <- filter( Modeldata, ST!=S)
  Id <- make.link('identity')      ### will be used by Betareg to overwrite the logit link function
  Ws <- 1/Testdata$Nd
  Beta_Test <-
    betareg( Reock ~ 1, 
             weights = Ws,
             data = Testdata,
             link = Id
    )
  Means_test <- Beta_Test$coefficients$mean[1] 
  Compare <- data.frame( SESSN=Modeldata$SESSN, ST=Modeldata$ST,  Test=Means_test, Base=Means_base)
  Compare <- Compare%>% mutate( Diff = abs( Test - Base )) 
  Compare <- Compare%>% mutate( Pdiff = Diff/Test )
  #summary( Compare)
  Compare_ST <-Compare %>% group_by( SESSN, ST)%>% summarise( MaxDiff = max( Diff), MaxPdiff = max(Pdiff))
  Indx<- which( Compare_ST$MaxPdiff>0.05 )
  print( paste( S,length( Indx), max( Compare_ST$MaxPdiff) ) )
  if (length( Indx)>0) {
    Testout <- data.frame( Test=paste0('X-',S), Compare_ST[Indx,] )
    Testsummary <- rbind( Testsummary, Testout)
    }
  Testmaxes <- data.frame(Test=paste0('X',S), SESSN="Maxes",ST='',MaxDiff=max(Compare_ST$MaxDiff), MaxPdiff=max(Compare_ST$MaxPdiff) )
  Testsummary <- rbind( Testsummary, Testmaxes)
} ### end loop

write.csv( file="Reock_reg_sensitivity.csv", Testsummary)
