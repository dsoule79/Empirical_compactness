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
CDx <- CDx%>% mutate( PP_Delta = Polsby - PP_Mu)          ### Higher scores more compact than expected

PPecdf <- ecdf( CDx$Polsby)
CDx <- CDx%>%mutate( PPrank = PPecdf(Polsby))  ### empirical probability as is

hist( CDx$PP_Delta)
Phigher <- sum( CDx$PP_Delta>0.01)/ nrow( CDx)
Plower  <- sum( CDx$PP_Delta< (-0.01))/ nrow( CDx)
Psame <- 1 - Phigher - Plower

print( paste(Plower, Psame, Phigher))

sum( CDx$PP_Pdistrict <0.5) / nrow(CDx)
sum( CDx$Rk_Pdistrict <0.5) / nrow(CDx)


###############################################################
#### Use Reock regression model to calc relative compactness
#################################################################
Fname <-"ModelParams_Reock_Crq_Null.csv"
Model <- read.csv( file=Fname)
Model

Intercept <- Model$Estimate[1]
Phi_intercept <- Model$Estimate[2]

CDx <- CDx%>% mutate( Rk_Mu = Intercept   )
CDx <- CDx%>% mutate( Rk_Phi = Phi_intercept )
CDx <- CDx%>% mutate( Rk_Pdistrict = pbetar( Reock,Rk_Mu,Rk_Phi))
CDx <- CDx%>% mutate( Rk_Delta = Reock - Rk_Mu)          ### Higher scores more compact than expected

RKecdf <- ecdf( CDx$Reock)
CDx <- CDx%>%mutate( RKrank = RKecdf(Reock))  ### empirical probability as is


hist( CDx$Rk_Delta)
Phigher <- sum( CDx$Rk_Delta>0.01)/ nrow( CDx)
Plower  <- sum( CDx$Rk_Delta< (-0.01))/ nrow( CDx)
Psame <- 1 - Phigher - Plower

print( paste(Plower, Psame, Phigher))

####################################################
### Load GIS data
###################################################

Shape1 <- read_sf('CD118raw.shp'  )
Shape2 <- read_sf( 'CD113raw.shp' )
Shape3 <- read_sf( 'CD108raw.shp' )

Shapes <- rbind( Shape1, Shape2)
Shapes <- rbind( Shapes, Shape3)
nrow( Shapes)

#######################################################################
### Add descriptive data to Shapes
#################################################################

Data <- CDx%>%select( SESSN, STATEFP,ST, DISTRICT, Polsby, PPrank, PP_Pdistrict, Cnt_Pby_Avg, RSTPolsby, Reock, RKrank, Rk_Pdistrict)
Data$DISTRICT <- sprintf("%02d", Data$DISTRICT) 
Data$SESSN <- sprintf("%03d", Data$SESSN)
Data$STATEFP <- sprintf("%02d", Data$STATEFP)
CDs <- left_join(Shapes, Data)

###################################################################################################

Sn <- "118"
St <- "NE"

Plotdata <- CDs%>%filter( SESSN==Sn & ST==St)
Plotdata <- Plotdata%>%mutate( Flag=ifelse( DISTRICT=="03",1,0))

Plot <- ggplot(Plotdata, aes( fill=as.factor(Flag))) +
  geom_sf( colour="white", linewidth=0.5) + 
  scale_fill_manual( values = c( "lightblue", 'darkblue'), labels=c('Other districts','District 3')) +
  theme(legend.title = element_blank()) +
  labs(title="Nebraska 118th session map") +
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )
Plot

setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
ggsave( file="NE_118.eps", plot=Plot, width=6, height=3, units="in")


median( CDx$Polsby)

t( Plotdata[3,])

###################################################################################################

Sn <- "118"
St <- "MD"

Plotdata <- CDs%>%filter( SESSN==Sn & ST==St)
Plotdata <- Plotdata%>%mutate( Flag=ifelse( DISTRICT=="02",1,0))

Plot <- ggplot(Plotdata, aes( fill=as.factor(Flag))) +
  geom_sf( colour="white", linewidth=0.5) + 
  scale_fill_manual( values = c( "lightblue", 'darkblue'), labels=c('Other districts','District 2')) +
  theme(legend.title = element_blank()) +
  labs(title="Maryland 118th session map") +
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )
Plot

setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
ggsave( file="MD_118.eps", plot=Plot, width=6, height=3, units="in")


median( CDx$Polsby)

t( Plotdata[ Plotdata$DISTRICT=="02",])






################################################################
## Find examples
#########################################################

Dis_median <- median( CDx$Polsby)

Data <- CDx%>%filter( Polsby>=Dis_median & PP_Pdistrict <=0.5)%>%select( SESSN, ST, DISTRICT, STpolsby, Cnt_Pby_Avg, Polsby, PP_Pdistrict)

Data

median( CDx$STpolsby)

