library(dplyr)
library(fitdistrplus)
library(ggplot2, svglite)
library(sf)
library(ggspatial)
library( corrgram)
library(betareg)
library( rstatix)
library( agricolae)
library( tidyr)
rm(list=ls())    #  remove all previous variables and value
options(digits = 4)

###################################################################
### Get Mid decade district  data
##################################################################
Dpath <- "C:/Users/Owner/Compactness/Papercode/Process data"
setwd(Dpath)
Fname <-"Compiled_Mid_decade_data.csv"
CDx <- read.csv( file=Fname)
colnames( CDx)[[7]] <- 'Polsby'

###############################################################
#### Use Polsby regression model to calc relative compactness
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

CDx <- CDx%>% mutate( PP_Mu = Intercept + Coeff_Cnt_Pby_Avg * Cnt_Pby_Avg + Coeff_RSTPolsby * RSTPolsby)
CDx <- CDx%>% mutate( PP_Phi = Phi_intercept )
CDx <- CDx%>% mutate( PP_Pdistrict = pbetar( Polsby,PP_Mu,PP_Phi))
CDx <- CDx%>% mutate( PP_Delta = Polsby - PP_Mu)          ### Higher scores more compact than expected

PPecdf <- ecdf( CDx$Polsby)
CDx <- CDx%>%mutate( PPrank = PPecdf(Polsby))  ### empirical probability as is


###############################################################
#### Use Reock regression model to calc relative compactness
#################################################################
Fname <-"ModelParams_Reock_Crq_Null.csv"
Model <- read.csv( file=Fname)
Model

Intercept <- Model$Estimate[1]
Phi_intercept <- Model$Estimate[2]

CDx <- CDx%>% mutate( Rk_Mu = Intercept )
CDx <- CDx%>% mutate( Rk_Phi = Phi_intercept )
CDx <- CDx%>% mutate( Rk_Pdistrict = pbetar( Reock,Rk_Mu,Rk_Phi))
CDx <- CDx%>% mutate( Rk_Delta = Reock - Rk_Mu)          ### Higher scores more compact than expected

RKecdf <- ecdf( CDx$Reock)
CDx <- CDx%>%mutate( RKrank = RKecdf(Reock))  ### empirical probability as is

#################################################
### Find values
############################################
St <- 'MO'  ### State
D <-  5    ### District

I <- which( CDx$ST==St & CDx$DISTRICT==D)
print( I)

t( CDx[I,] %>% select( ST, DISTRICT, Polsby, PPrank, PP_Pdistrict,STpolsby, Reock, RKrank, Rk_Pdistrict, STreock ))

I <- which( CDx$ST==St )
Dataout <- CDx %>% select( ST, DISTRICT, Polsby, PPrank, PP_Pdistrict,STpolsby, Reock, RKrank, Rk_Pdistrict, STreock )
write.csv(file="MO-Middecade data.csv", Dataout[I,])


######################################################
### Find district outliers
###################################################

PP_outliers <- CDx%>%filter( PP_Pdistrict <= 0.016) %>% select( ST, DISTRICT, Polsby, PPrank, PP_Pdistrict, Reock, RKrank, Rk_Pdistrict )
nrow( PP_outliers)
PP_outliers

PP_outliers <- PP_outliers   %>%mutate(Dis = paste( ST,sprintf("%02d", DISTRICT) ) )
PPdata <- data.frame( Metric = 'PP',  District=PP_outliers$Dis, PP_outliers$PP_Pdistrict)
colnames( PPdata)<- c('Metric',"District", 'Relative compactness')

RK_outliers <- CDx%>%filter( Rk_Pdistrict <= 0.002) %>% select( ST, DISTRICT, Polsby, PPrank, PP_Pdistrict, Reock, RKrank, Rk_Pdistrict )
nrow( RK_outliers)
RK_outliers

RK_outliers <- RK_outliers   %>%mutate(Dis = paste( ST,sprintf("%02d", DISTRICT) ) )
RKdata <- data.frame( Metric = 'RK',  District=RK_outliers$Dis, RK_outliers$Rk_Pdistrict)
colnames( RKdata)<- c('Metric',"District", 'Relative compactness')

Outlier_data <- rbind( PPdata, RKdata )
write.csv( file="Doutliers_Mid_decade.csv", Outlier_data, row.names = FALSE)

##############################################
#### Calc states- Sessn that are Polsby outliers on an overall map basis
########################################
ST_SESSN.dat <- data.frame( character(), numeric(), numeric(), numeric(), numeric(), numeric(), numeric() )
Tnames <- c( "ST","Pstate", "MinDisPval", "MinDisPolsby", "STpolsby","Cnt_Pby_Avg","Max_RSTPolsby")
STs <- unique ( CDx$ST)

  for( St in STs){
    STdata <- filter( CDx, ST==St)
    if( nrow(STdata) < 2) next
    MinDpolsby <- min( STdata$Polsby)
    MinDpval <- min( STdata$PP_Pdistrict)
    Ptest <- ks.test( STdata$PP_Pdistrict, punif, alternative='greater')$p.value
    ST.test <- data.frame( St, Ptest, MinDpval, MinDpolsby, STdata$STpolsby[1], STdata$Cnt_Pby_Avg[1], max(STdata$RSTPolsby) )
    colnames(ST.test)<- Tnames
    ST_SESSN.dat <- rbind( ST_SESSN.dat, ST.test)
  }

#ST_SESSN.dat[ST_SESSN.dat$Pstate<0.023,] 

ST_SESSN.dat 

PP_ST_SESSN.dat <- ST_SESSN.dat %>% select( ST, Pstate, MinDisPval)
PP_ST_SESSN.dat <- PP_ST_SESSN.dat %>% mutate( Metric = 'PP')

##############################################
#### Calc states- Sessn that are Reock outliers on an overall map basis
########################################
ST_SESSN.dat <- data.frame( character(), numeric(), numeric(), numeric(), numeric(), numeric() )
Tnames <- c( "ST","Pstate", "MinDisPval", "MinDisReock", "STreock","Cnt_Rk_Avg")
STs <- unique ( CDx$ST)

for( St in STs){
  STdata <- filter( CDx, ST==St)
  if( nrow(STdata) < 2) next
  MinDreock <- min( STdata$Reock)
  MinDpval <- min( STdata$Rk_Pdistrict)
  Ptest <- ks.test( STdata$Rk_Pdistrict, punif, alternative='greater')$p.value
  ST.test <- data.frame( St, Ptest, MinDpval, MinDreock, STdata$STreock[1], STdata$Cnt_Rk_Avg[1] )
  colnames(ST.test)<- Tnames
  ST_SESSN.dat <- rbind( ST_SESSN.dat, ST.test)
}

ST_SESSN.dat 

RK_ST_SESSN.dat <- ST_SESSN.dat %>% select( ST, Pstate, MinDisPval)
RK_ST_SESSN.dat <- RK_ST_SESSN.dat %>% mutate( Metric = 'RK')

###########################################################
#### Output combined results
#######################################################

Mid_decade_scores <- rbind( PP_ST_SESSN.dat, RK_ST_SESSN.dat)

write.csv( file='Mid_decade_scores.csv', Mid_decade_scores)
