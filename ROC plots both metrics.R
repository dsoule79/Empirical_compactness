##########################################
### ROC combined graphs
#######################################
library(dplyr)
library(fitdistrplus)
library(ggplot2, svglite)
rm(list=ls())    #  remove all previous variables and value
options(digits = 4)

####################################
### Get ROC analysis data from both tests
#####################################
Dpath <- "C:/Users/Owner/Compactness/Papercode/Process data"
setwd(Dpath)
Fname <- "ModelParams_Pby_Crq_Best.csv"
Fname <- "ModelParams_Reock_Crq_Null.csv"

Dtest_fname <- paste0("ROCdata-MinDis-",Fname)
STtest_fname <- paste0("ROCdata-STpval-",Fname)

Dis_ROC <- read.csv( file=Dtest_fname)
ST_ROC <- read.csv( file=STtest_fname)

Dis_ROC['Metric'] <-"Min District"
ST_ROC['Metric'] <- 'ST Bias'
ROC_data <- rbind( Dis_ROC, ST_ROC)

############################################################
#### Plot ROC chart both
#########################################################
Plot <- ggplot( data=ROC_data, aes( x=FPR, y=TPR)) +
  geom_line( aes(color=as.factor(Metric))) +
  geom_abline( color="blue") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))+ 
  scale_x_continuous(breaks = seq(0, 1, by = 0.1))+ 
 labs( x='False positive Rate', y='True positive rate', color='Test')+
  labs( title="Test ROC curve")+
  theme(legend.position="inside",legend.position.inside = c(0.8,0.2)) +
  theme(plot.margin =  unit(c(0,0.2,0,0.2),"cm") )

Plot
setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
ggsave( file="ROC_RK.eps", plot=Plot, width=3, height=3, units="in")

##########################################################
### Plot precision curves
############################################################
Plotdata <- ROC_data[ ROC_data$Threshold < 0.5,]

Plot <- ggplot( data=Plotdata, aes( x=Threshold, y=Precison)) +
  geom_line( aes(color=as.factor(Metric))) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))+ 
  scale_x_continuous(breaks = seq(0, 1, by = 0.1))+ 
  labs( x='Threshold', y='True positive rate', color='Test')+
  labs( title="Test Precision")+
  theme(legend.position="inside",legend.position.inside = c(0.8,0.8)) +
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )

Plot

setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
ggsave( file="Precision_RK.eps", plot=Plot, width=3, height=3, units="in")

###############################################################
### AUC calc
################################################################
ROC <- Dis_ROC
ROC <- ST_ROC

N <- nrow( ROC)
AUC <- 0
for ( i in 1:(N-1)){
  if((ROC$FPR[i+1] - ROC$FPR[i])>0 ){
  AUC <- AUC + 0.5 *(ROC$FPR[i+1] - ROC$FPR[i]) * (ROC$TPR[i+1]+ROC$TPR[i])
  }}
AUC


