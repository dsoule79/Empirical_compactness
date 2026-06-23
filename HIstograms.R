##########################################
### Plot histograms 
### Compare ST with compactness and without requirement
##########################################
library(ggplot2)
library( scales)
library(fitdistrplus)
library( goftest)
library( twosamples)
library( dgof)
library(dplyr)

rm(list=ls())    #  remove all previous variables and value

##############################################
##### AD test for Beta
##### kSamples: Provides the k-sample version of the test, used to determine if multiple independent samples come from the same (unspecified) distribution.
##############################################
Beta_test <- function( Data){
  Fit <- fitdist(Data, "beta")
  shape1_est <- Fit$estimate["shape1"]
  shape2_est <- Fit$estimate["shape2"]
  ad_test <- ad.test(Data, null = "pbeta", shape1 = shape1_est, shape2 = shape2_est)
  mean <- shape1_est/(shape1_est+shape2_est)
  phi <- shape1_est+shape2_est
  return(list( AD_Pval=ad_test$p.value, mean=mean, Phi=phi ))
  }

##### load district descriptive data
Dpath <- "C:/Users/Owner/Compactness/Papercode/Process data"
setwd(Dpath)
Fname <-"Compiled_data.csv"
CDx <- read.csv( file=Fname)
nrow( CDx)
nrow( CDx[ CDx$CReq==1,])
nrow( CDx[ CDx$CReq==0,])

############################################
#### Plot Polsby Histogram woth density curve
##########################################
Beta_test(CDx$Polsby)
Beta_test(CDx$Polsby[ CDx$CReq==1])
Beta_test(CDx$Polsby[ CDx$CReq==0])

median(CDx$Polsby[ CDx$CReq==1] )
median(CDx$Polsby[ CDx$CReq==0] )

Median.test( CDx$Polsby, CDx$CReq)

ks.test(CDx$Polsby[ CDx$CReq==1],CDx$Polsby[ CDx$CReq==0] )
twosamples::ad_test(CDx$Polsby[ CDx$CReq==1],CDx$Polsby[ CDx$CReq==0] )

Beta.fit <- fitdist( CDx$Polsby[ CDx$CReq==1], "beta")
Beta.parms <- Beta.fit$estimate
Steps<- seq(0,1.0,0.001) + 0.025
Bdensity <- dbeta( Steps , Beta.parms[1], Beta.parms[2])
Bdensity <- 31*Bdensity / sum( Bdensity) 
Pbeta <-  pbeta( Steps , Beta.parms[1], Beta.parms[2])
Bdist <- data.frame( Steps, Bdensity)
Bdist <- Bdist%>%filter( Steps<=0.8)

Plot <- ggplot() +
  geom_histogram(data=CDx, aes( x = Polsby, fill = as.factor(CReq),y = after_stat(count / tapply(count, group, sum)[group] * 100  )), 
                 position = "dodge", 
                 breaks = seq(0, 0.8, by = 0.05)) + 
  #scale_y_continuous(labels = scales::percent, limits=c(0,0.11)) +
  scale_fill_discrete(labels = c("None", "Required"))+
  #geom_line(data=Bdist, aes( x = Steps, y = Bdensity), size=1, color="blue") +
  theme(legend.position = "inside", legend.position.inside = c(0.8, 0.8)) +
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )+
  labs(y = "Percent of districts", x = "District Polsby Popper", fill='Compactness \nrequirement')
Plot

setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
ggsave( file="CDhist_split_polsby.eps", plot=Plot, width=3, height=3, units="in")


#####################################################################
#### Plot District Reock histogram
######################################################################
Beta_test(CDx$Reock)
Beta_test(CDx$Reock[ CDx$CReq==1])
Beta_test(CDx$Reock[ CDx$CReq==0])

median(CDx$Reock[ CDx$CReq==1] )
median(CDx$Reock[ CDx$CReq==0] )

Median.test( CDx$Reock, CDx$CReq)

ks.test(CDx$Reock[ CDx$CReq==1],CDx$Reock[ CDx$CReq==0] )
ad_test(CDx$Reock[ CDx$CReq==1],CDx$Reock[ CDx$CReq==0] )

Beta.fit <- fitdist( CDx$Reock[ CDx$CReq==1], "beta")
Beta.parms <- Beta.fit$estimate
Steps<- seq(0,1.0,0.001) + 0.025
Bdensity <- dbeta( Steps , Beta.parms[1], Beta.parms[2])
Bdensity <- 31*Bdensity / sum( Bdensity) 
Pbeta <-  pbeta( Steps , Beta.parms[1], Beta.parms[2])
Bdist <- data.frame( Steps, Bdensity)
Bdist <- Bdist%>%filter( Steps<=0.8)

Plot <- ggplot() +
  geom_histogram(data=CDx, aes( x = Reock, fill = as.factor(CReq),y = after_stat(count / tapply(count, group, sum)[group] * 100  )), 
                 position = "dodge", 
                 breaks = seq(0, 0.8, by = 0.05)) + 
  #scale_y_continuous(labels = scales::percent, limits=c(0,0.11)) +
  scale_fill_discrete(labels = c("None", "Required"))+
  #geom_line(data=Bdist, aes( x = Steps, y = Bdensity), size=1, color="blue") +
  theme(legend.position = "inside", legend.position.inside = c(0.8, 0.8)) +
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )+
  labs(y = "Percent of districts", x = "District Reock", fill='Compactness \nrequirement')
Plot

setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
ggsave( file="CDhist_split_reock.eps", plot=Plot, width=3, height=3, units="in")



#########################################################
### Reock fit
############################################################

#### Try fitting distributions
Data <- CDx$Reock
Norm.fit <-  fitdist( Data, "norm")
Fstat <- gofstat(Norm.fit)

Lnorm.fit <- fitdist( Data, "lnorm")
X<- gofstat(Lnorm.fit)
Fstat <- cbind( Fstat, X)

Beta.fit <- fitdist( Data, "beta")
X<- gofstat(Beta.fit)
Fstat <- cbind( Fstat, X)   ### Note only beta is fail to reject say it is good fit

Logistic.fit <- fitdist(Data, "logis")
X<- gofstat(Logistic.fit)
Fstat <- cbind( Fstat, X) 
colnames( Fstat)<- c("Normal", "LogNorm","Beta","Logistic")

print( Fstat)
