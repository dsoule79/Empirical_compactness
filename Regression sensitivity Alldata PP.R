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

CDx <- CDx%>% mutate( PP_Mu = Intercept + Coeff_Cnt_Pby_Avg * Cnt_Pby_Avg + Coeff_RSTPolsby * RSTPolsby )
CDx <- CDx%>% mutate( PP_Phi = Phi_intercept )
CDx <- CDx%>% mutate( PP_Pdistrict = pbetar( Polsby,PP_Mu,PP_Phi))

###############################################################
#### Use Polsby regression model to calc relative compactness
#################################################################
Dpath <- "C:/Users/Owner/Compactness/Papercode/Process data"
setwd(Dpath)
Fname <-"ModelParams_Pby_All_Best.csv"
Model <- read.csv( file=Fname)
Model

Intercept <- Model$Estimate[1]
Coeff_Cnt_Pby_Avg <- Model$Estimate[2]
Coeff_RSTPolsby <- Model$Estimate[3]
Phi_intercept <- Model$Estimate[4]

CDx <- CDx%>% mutate( PPall_Mu = Intercept + Coeff_Cnt_Pby_Avg * Cnt_Pby_Avg + Coeff_RSTPolsby * RSTPolsby )
CDx <- CDx%>% mutate( PPall_Phi = Phi_intercept )
CDx <- CDx%>% mutate( PPall_Pdistrict = pbetar( Polsby,PP_Mu,PP_Phi))

CDx <- CDx%>% mutate( PP_Delta = PP_Mu - PPall_Mu)    

mean( abs( CDx$PP_Delta))
median( CDx$PP_Delta)
min( CDx$PP_Delta)
max( CDx$PP_Delta)

t( CDx[ CDx$PP_Delta==min(CDx$PP_Delta),])

#######################################################################

Median.test( CDx$PP_Delta, CDx$CReq)

###############################################

Plot <- ggplot(data=CDx, aes( x=PP_Delta, fill=as.factor(CReq) )) +
  geom_histogram( binwidth = 0.01, position='dodge',aes( y = after_stat(count / tapply(count, group, sum)[group] * 100)))+
  scale_fill_discrete(labels = c("None", "Required"))+
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )+
  labs(y = "Percentage of districts in group", fill="Compactness \n requirement", x='Compactness increase')

Plot

setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
ggsave( file="PPall_compare_hist.eps", plot=Plot, width=6, height=3, units="in")                
                  
