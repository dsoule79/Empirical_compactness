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

CDx <- CDx%>%mutate( RST_component = Coeff_RSTPolsby * RSTPolsby ) 
CDx <- CDx%>%mutate( CntAVG_component = Coeff_Cnt_Pby_Avg * Cnt_Pby_Avg )
CDx <- CDx%>% mutate( Mu = Intercept + Coeff_Cnt_Pby_Avg * Cnt_Pby_Avg + Coeff_RSTPolsby * RSTPolsby )
CDx <- CDx%>% mutate( Phi = Phi_intercept )
CDx <- CDx%>% mutate( Pdistrict = pbetar( Polsby,Mu,Phi))
CDx<- CDx%>% mutate( RP = ifelse( RSTPerim==0,0,1))
CDx<- CDx%>% mutate( RP10 = ifelse( RSTPerim>=0.1,1,0))

PPecdf <- ecdf( CDx$Polsby)
CDx <- CDx%>%mutate( PPrank = PPecdf(Polsby))  ### empirical probability as is

Plotdata <- CDx%>% filter( CReq==1)

Plot <- ggplot(Plotdata, aes(x = as.factor(RP) , y = Polsby)) +
  geom_boxplot() +
  #labs(y = "Residual", x = "Session", fill="") +
  theme(text = element_text(size=10)) +
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )
Plot

Mtest <- Median.test(  Plotdata$Polsby, Plotdata$RP, simulate.p.value= TRUE)
Mtest$statistics 

median( Plotdata$Polsby )

median( Plotdata$Cnt_Pby_Avg )

median( Plotdata$STpolsby)


sum( CDx$RP)
sum( CDx$RP10)





Plot <- ggplot( CDx) +
         geom_point( aes( x=CntAVG_component, y=RST_component, color=as.factor( SESSN))) +
         labs( x="County Avg PP", y="ST PP * ration district on border ", 
               title="Contributions to expected compactness", color="Session") +
        theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )
Plot

setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
ggsave( file="PP_contribution.eps", plot=Plot, width=3, height=6, units="in")
