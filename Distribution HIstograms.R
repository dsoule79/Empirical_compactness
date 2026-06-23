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
#### Plot Polsby Histogram with density curve
##########################################
Beta_test(CDx$Polsby)
Beta_test(CDx$Polsby[ CDx$CReq==1])

Beta.fit <- fitdist( CDx$Polsby, "beta")
Beta.parms <- Beta.fit$estimate

bw<- 0.05
Plot <- ggplot(CDx, aes(x = Polsby)) +
          geom_histogram(aes(y = after_stat(density * bw)), 
                 binwidth = bw, fill = "lightblue", color = "black") +
          stat_function(fun = function(x) dbeta(x,Beta.parms[1], Beta.parms[2] ) * bw, 
                color = "red", linewidth = 1) +
          scale_y_continuous(labels = scales::percent) +
          theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )+
          labs(y = "Percent of districts", x = "District Polsby Popper score")
Plot

setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
ggsave( file="CDhist_polsby_beta.eps", plot=Plot, width=6, height=3, units="in")


#####################################################################
#### Plot District Reock histogram with density curve
######################################################################
Beta_test(CDx$Reock)
Beta_test(CDx$Reock[ CDx$CReq==1])

Beta.fit <- fitdist( CDx$Reock, "beta")
Beta.parms <- Beta.fit$estimate

bw<- 0.05
Plot <- ggplot(CDx, aes(x = Reock)) +
  geom_histogram(aes(y = after_stat(density * bw)), 
                 binwidth = bw, fill = "lightblue", color = "black") +
  stat_function(fun = function(x) dbeta(x,Beta.parms[1], Beta.parms[2] ) * bw, 
                color = "red", linewidth = 1) +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )+
  labs(y = "Percent of districts", x = "District Reock score")
Plot

setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
ggsave( file="CDhist_reock_beta.eps", plot=Plot, width=6, height=3, units="in")

#########################################################
### Polsby fit
############################################################

#### Try fitting distributions
Data <- CDx$Polsby
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
