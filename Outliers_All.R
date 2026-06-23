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
### Get district  data
##################################################################
Dpath <- "C:/Users/Owner/Compactness/Papercode/Process data"
setwd(Dpath)
Fname <-"Compiled_data.csv"
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

######################################################
### Find district outliers
###################################################

PP_outliers <- CDx%>%filter( PP_Pdistrict <= 0.01)%>%dplyr::select(SESSN, ST, DISTRICT, Polsby, PPrank, PP_Pdistrict, Reock, RKrank, Rk_Pdistrict )
nrow( PP_outliers)
PP_outliers

PP_outliers <- PP_outliers%>%mutate(Dis = paste( SESSN ,ST,sprintf("%02d", DISTRICT) ) )
PPdata <- data.frame( Metric = 'PP',  District=PP_outliers$Dis, PP_outliers$PP_Pdistrict)
colnames( PPdata)<- c('Metric',"District", 'Relative compactness')

RK_outliers <- CDx%>%filter( Rk_Pdistrict <= 0.01) %>% dplyr::select( SESSN, ST, DISTRICT, Polsby, PPrank, PP_Pdistrict, Reock, RKrank, Rk_Pdistrict )
nrow( RK_outliers)
RK_outliers

RK_outliers <- RK_outliers%>%mutate(Dis = paste( SESSN,ST,sprintf("%02d", DISTRICT) ) )
RKdata <- data.frame( Metric = 'RK',  District=RK_outliers$Dis, RK_outliers$Rk_Pdistrict)
colnames( RKdata)<- c('Metric',"District", 'Relative compactness')

Outlier_data <- rbind( PPdata, RKdata )
write.csv( file="Doutliers01_All.csv", Outlier_data, row.names = FALSE)

##############################################
#### Calc states- Sessn that are Polsby outliers on an overall map basis
########################################
ST_SESSN.dat <- data.frame( character(), character(), numeric(), numeric(), numeric(), numeric(), numeric(), numeric() )
Tnames <- c( "SESSN","ST","Pstate", "MinDisPval", "MinDisPolsby", "STpolsby","Cnt_Pby_Avg","Max_RSTPolsby")
STs <- unique ( CDx$ST)
SNs <- unique( CDx$SESSN)

for ( Sn in SNs){ 
  for( St in STs){
    STdata <- filter( CDx, SESSN==Sn & ST==St)
    if( nrow(STdata) < 2) next
    MinDpolsby <- min( STdata$Polsby)
    MinDpval <- min( STdata$PP_Pdistrict)
    Ptest <- ks.test( STdata$PP_Pdistrict, punif, alternative='greater')$p.value
    ST.test <- data.frame( Sn, St, Ptest, MinDpval, MinDpolsby, STdata$STpolsby[1], STdata$Cnt_Pby_Avg[1], max(STdata$RSTPolsby) )
    colnames(ST.test)<- Tnames
    ST_SESSN.dat <- rbind( ST_SESSN.dat, ST.test)
    }}
  
ST_SESSN.dat[ST_SESSN.dat$Pstate<0.01,] 

PP_ST <- ST_SESSN.dat[ST_SESSN.dat$Pstate<0.01,] %>% dplyr::select( SESSN, ST, Pstate, MinDisPval) 

#### ouput mid decade state data
MidDecadeSTs <- c("CA", "TX",	"NC",	"MO",	"VA",	"FL")

PP_ST_SESSN.dat <- ST_SESSN.dat %>% filter( SESSN=='118' & ST %in% MidDecadeSTs) %>%select( ST, Pstate, MinDisPval)
PP_ST_SESSN.dat <- PP_ST_SESSN.dat %>% mutate( Metric = 'PP')


##############################################
#### Calc states- Sessn that are Reock outliers on an overall map basis
########################################
ST_SESSN.dat <- data.frame( character(), character(), numeric(), numeric(), numeric(), numeric(), numeric() )
Tnames <- c( "SESSN", "ST","Pstate", "MinDisPval", "MinDisReock", "STreock","Cnt_Rk_Avg")
STs <- unique ( CDx$ST)
SNs <- unique( CDx$SESSN)

for ( Sn in SNs){ 
for( St in STs){
  STdata <- filter( CDx, SESSN==Sn & ST==St)
  if( nrow(STdata) < 2) next
  MinDreock <- min( STdata$Reock)
  MinDpval <- min( STdata$Rk_Pdistrict)
  Ptest <- ks.test( STdata$Rk_Pdistrict, punif, alternative='greater')$p.value
  ST.test <- data.frame( Sn, St, Ptest, MinDpval, MinDreock, STdata$STreock[1], STdata$Cnt_Rk_Avg[1] )
  colnames(ST.test)<- Tnames
  ST_SESSN.dat <- rbind( ST_SESSN.dat, ST.test)
}}

ST_SESSN.dat[ST_SESSN.dat$Pstate<0.01,] 

RK_ST <- ST_SESSN.dat[ST_SESSN.dat$Pstate<0.01,] %>% dplyr::select( SESSN, ST, Pstate, MinDisPval) 

#### ouput mid decade state data
MidDecadeSTs <- c("CA", "TX",	"NC",	"MO",	"VA",	"FL")

RK_ST_SESSN.dat <- ST_SESSN.dat %>% filter( SESSN=='118' & ST %in% MidDecadeSTs) %>%select( ST, Pstate, MinDisPval)
RK_ST_SESSN.dat <- RK_ST_SESSN.dat %>% mutate( Metric = 'PP')

Previous_Mid_decade_scores <- rbind( PP_ST_SESSN.dat, RK_ST_SESSN.dat)
write.csv( file='MidDecade_prev_scores.csv',Previous_Mid_decade_scores  )


PP_ST$Metric <- "PP"
RK_ST$Metric <- "RK"
STdata <- rbind( PP_ST, RK_ST)
write.csv( file="SToutliers01_All.csv", STdata, row.names = FALSE)


