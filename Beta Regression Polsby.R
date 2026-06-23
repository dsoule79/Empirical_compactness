#################################################################################
###  Load district master data with three decades of data
###  Perform Beta regressions
###  Analyze residuals
###  export coefficients of best model for further use
#################################################################################

library(dplyr)
library( stringr)
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

Reg_var <-c( "ST","SESSN","Polsby", "STpolsby","STLarea" , "STLarea_Nd",     
             "POP_REP", "Pop_Den","LPop_Den","Nd","LNd",
             "Cnt_Pby_Avg", "Cnt_Pby_Gvg", "Cnt_Pby_Hvg","Cnt_Pby_Min", "Cnt_Pby_Med",
             "S108", "S113", "S118", "Decade", "RSTPerim" , 'RSTPolsby',  
             "RCoast" , "Coast",
             "STposlby2", "Cnt_Pby_Avg2", "Cnt_Pby_Gvg2", "Cnt_Pby_Hvg2", "Cnt_Pby_Med2",
             "Cnt_Pby_SDI" , 
             "CReq"  )

Modeldata <- select( CDx, all_of(Reg_var))

##############################################
### Remove states without compact req
############################################
nrow(Modeldata)
Modeldata <- Modeldata%>%filter( CReq==1)
nrow(Modeldata)

#################################
## Add state indicator variables
###############################
States <- unique( Modeldata$ST)

for( ST in States){
  NewCol <- rep(0,nrow(Modeldata))
  Indx <- which( Modeldata$ST== ST)
  NewCol[Indx]<-1
  #print( paste( ST, sum(NewCol)))
  Modeldata <- cbind( Modeldata, NewCol)
  colnames(Modeldata)[[ncol(Modeldata)]] <- ST
}

########################################################
### Beta regressions
########################################################
setwd("C:/Users/Owner/Compactness/Papercode/Process data")  #### location of model summaries

################################################################################
### Beta regression versus a constant - base case
##############################################################################
Id <- make.link('identity')      ### will be used by Betareg to overwrite the logit link function
Ws <- 1/Modeldata$Nd
Beta_NOvars <-
  betareg( Polsby ~ 1, 
           weights = Ws,
           data = Modeldata,
           link = Id
  )

summary(Beta_NOvars)
AIC( Beta_NOvars)
BIC( Beta_NOvars)

Summary <- Format_summary( Beta_NOvars)
write.csv( file="ModelParams_Pby_Cqr_Null.csv", Summary)

################################################################################
### Beta regression versus states 
##############################################################################
### Find median state and remove to not over specify model
######################################################################
ST_Med_Pby <- Modeldata%>%group_by(ST)%>%summarise(STMed=median(Polsby))
Overall_Med <- median( ST_Med_Pby$STMed)
Delta <- abs( ST_Med_Pby$STMed - Overall_Med)
I <- which( Delta == min( Delta))
ST_w_Med <- ST_Med_Pby$ST[I]
ST_w_Med 
St_I <- which( States==ST_w_Med)

VarsF <- paste( States[-St_I], collapse="+")
Formula <- as.formula( paste( "Polsby ~ ", VarsF))

Id <- make.link('identity')      ### will be used by Betareg to overwrite the logit link function
Ws <- 1/Modeldata$Nd
Beta_STonly <-
  betareg( Formula, 
           weights = Ws,
           data = Modeldata,
           link = Id
  )

summary(Beta_STonly)
AIC( Beta_STonly)
BIC( Beta_STonly)

Summary <- Format_summary( Beta_STonly)
write.csv( file="ModelParams_Pby_All_STonly.csv", Summary, row.names = FALSE)


############################################################################
#### Additive variable selection Mean only
########################################################################
Id <- make.link('identity')      ### will be used by Betareg to overwrite the logit link function
Ws <- 1/Modeldata$Nd


Vars <- c(Reg_var[-c(1:3,31, 22, 12)], States)   ####  Explanatory variables
#Vars <- States
 
Formula.string<- "Polsby ~ RSTPolsby + Cnt_Pby_Avg +"
N_mean_params <- 2 + str_count(Formula.string, "\\+")### including intercept and test variable is formula +2
N_mean_params 

Vs <- Vars
Nv <- length(Vs)
Result <- matrix( NA, Nv, 6, dimnames = list( Vs,c( "BIC","AIC","R^2","Percision","Coeff","PvalCoeff")))
for(i in 1:Nv) {
  FS<- paste(Formula.string,Vs[i])
  Formula <- as.formula( FS)
  Betamodel <- betareg(Formula, weights = Ws,link=Id,link.phi=Id, data = Modeldata )
  X <- summary( Betamodel)
  Coeffs <- X$coefficients$mean
  Per <- X$coefficients$precision
  Ncoeffs <- nrow( Coeffs)
  Result[i,1]<- BIC(Betamodel)
  Result[i,2]<- AIC(Betamodel)
  Result[i,3]<- Betamodel$pseudo.r.squared
  Result[i,4]<- Per[1,1]
  Result[i,5]<- Coeffs[N_mean_params,1]
  Result[i,6]<- Coeffs[N_mean_params,4]
}
Result[ order(Result[,1]),]
Reg_var

write.csv( file="Pby_iter_All_IL_Decade.csv",Result[ order(Result[,1]),] )

#####################################################################################
### Additive with best mean model and percision
#######################################################################################
Id <- make.link('identity')      ### will be used by Betareg to overwrite the logit link function
Ws <- 1/Modeldata$Nd

Formula.string<- "Polsby ~  Cnt_Pby_Hvg + RSTPolsby | "
N_phi_params <- 2  ### Including  intercept and test variable ie formula +2

Vs <- c( Reg_var[ -c(1:3, 31)], States)
Nv <- length(Vs)
Result <- matrix( NA, Nv, 5, dimnames = list( Vs,c( "BIC","AIC","R^2","Pre_Coeff","Pval_Pre_Coeff")))
for(i in 1:Nv) {
  FS<- paste(Formula.string,Vs[i])
  Formula <- as.formula( FS)
  Betamodel <- betareg(Formula, weights = Ws,link=Id,link.phi=Id, data = Modeldata )
  X <- summary( Betamodel)
  Coeffs <- X$coefficients$mean
  Per <- X$coefficients$precision
  Ncoeffs <- nrow( Coeffs)
  Result[i,1]<- BIC(Betamodel)
  Result[i,2]<- AIC(Betamodel)
  Result[i,3]<- Betamodel$pseudo.r.squared
  Result[i,4]<- Per[N_phi_params,1]
  Result[i,5]<- Per[N_phi_params,4]
}
Result[ order(Result[,1]),]
Reg_var
write.csv( file="Reock_iter_pre_Crq_all1.csv",Result[ order(Result[,1]),] )

#############################################################################################
####  Beta best model
#######################################################################################

Id <- make.link('identity')      ### will be used by Betareg to overwrite the logit link function
Ws <- 1/Modeldata$Nd


Beta_best <-
  betareg( Polsby ~ Cnt_Pby_Avg + RSTPolsby   , 
           weights = Ws,
           data = Modeldata,
           link = Id,
           link.phi = Id
  )

summary(Beta_best)
AIC( Beta_best)
BIC( Beta_best)

Summary <- Format_summary( Beta_best)
write.csv( file="ModelParams_Pby_Crq_Best.csv", Summary)

###############################################################################
#### Model analysis
###########################################################################
setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
StdResids <- residuals( Beta_best, type="pearson")
Fit <- Beta_best$fitted.values
Resid <- Beta_best$residuals
NC <- ncol( Modeldata)
RegData <- cbind( Modeldata, Fit,Resid, StdResids)
colnames( RegData)[[NC+1]]<-"Fit"
colnames( RegData)[[NC+2]]<-"Resid"
colnames( RegData)[[NC+3]]<-"StdResid"

#RegData <- RegData %>% mutate( Phi = Beta_best$coefficients$precision[1] + Beta_best$coefficients$precision[2]*Cnt_Pby_SDI)

#######################################################################################
### Residual plots
###############################################################################

Plot <- ggplot(RegData, aes(x = Resid)) + 
  geom_histogram( aes(y = after_stat(count)/sum(after_stat(count))), binwidth = 0.05, color="white") + 
  scale_y_continuous(labels = scales::percent) +
    labs(y = "Percentage of districts", x="Model residual") 

Plot
#ggsave( file="Beta_Pby_Crq_resid_hist.eps", plot=Plot, width=6, height=3, units="in")

Plot <- ggplot(RegData, aes(x =Fit , y = Resid)) +
  geom_point( aes(color=as.factor(SESSN))) +
  labs(y = "Model Residual", x = "Fitted value") +
  geom_smooth(method = "lm") +
  scale_x_continuous( breaks=seq(0,1,0.1))+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  theme(text = element_text(size=10)) +
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )
Plot
ggsave( file="Beta_Pby_Crq_resid_fitted.eps", plot=Plot, width=6, height=3, units="in")

Plot <- ggplot(RegData, aes(x =Cnt_Pby_Avg , y = Resid)) +
  geom_point( aes(color=as.factor(SESSN))) +
  labs(y = "Model Residual") +
  geom_smooth(method = "lm") +
  scale_x_continuous( breaks=seq(0,1,0.1))+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  theme(text = element_text(size=10)) +
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )
Plot
ggsave( file="Beta_Pby_Crq_resid_STLarea.eps", plot=Plot, width=6, height=3, units="in")

Plot <- ggplot(RegData, aes(x =RSTPolsby , y = Resid)) +
  geom_point( aes(color=as.factor(SESSN))) +
  labs(y = "Model Residual") +
  geom_smooth(method = "lm") +
  scale_x_continuous( breaks=seq(0,1,0.1))+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  theme(text = element_text(size=10)) +
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )
Plot



###################################################################
### residuals by Session
######################################################################
PlotData <- RegData

Plot <- ggplot(PlotData, aes(x = as.factor(SESSN) , y = Resid)) +
  geom_boxplot(aes( fill= as.factor(SESSN))) +
  labs(y = "Residual", x = "Session", fill="") +
  geom_hline(yintercept=0, linetype="dashed",color = "red", linewidth=1)+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  theme(text = element_text(size=10)) +
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )
Plot
ggsave( file="Beta_Pby_All_resid_SESSN.eps", plot=Plot, width=6, height=3, units="in")

Plot <- ggplot(PlotData, aes(x = as.factor(SESSN) , y = Resid)) +
  geom_violin(aes( fill= as.factor(SESSN)), draw_quantiles = c(0.25,0.5,0.75)) +
  labs(y = "Residual", x = "Session", fill="") +
  geom_hline(yintercept=0, linetype="dashed",color = "red", linewidth=1)+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  theme(text = element_text(size=12), legend.position = "none") +
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )
Plot
ggsave( file="Beta_Pby_All_resid_SESSN.eps", plot=Plot, width=6, height=3, units="in")

max(PlotData$Resid[ PlotData$SESSN==108]) - min(PlotData$Resid[ PlotData$SESSN==108])
max(PlotData$Resid[ PlotData$SESSN==113]) - min(PlotData$Resid[ PlotData$SESSN==113])
max(PlotData$Resid[ PlotData$SESSN==118]) - min(PlotData$Resid[ PlotData$SESSN==118])

Test <- kruskal.test( Resid ~ SESSN, data = RegData)  #### NULL is no difference in medians
Test  ###Result  Not all  the same

Mtest <- Median.test(  RegData$Resid, RegData$SESSN, simulate.p.value= TRUE)
Mtest$statistics 

Wilcoxtest <- wilcox_test( Resid~SESSN, data =RegData)
Wilcoxtest

####################################################
#### Testing sessn median residuals 
#####################################################
MedResid <- RegData %>% group_by(SESSN,ST)%>%summarise( Res_med= median( Resid))
MedResid <- as.data.frame( MedResid)

Test <- kruskal.test( Res_med ~ SESSN, data = MedResid )  #### NULL is no difference in medians
Test  ###Result  Not diff

Mtest <- Median.test(  MedResid$Res_med, MedResid$SESSN, simulate.p.value= TRUE)
Mtest$statistics  ## Nt diff

Wilcoxtest <- wilcox_test( Res_med~SESSN, data = MedResid )
Wilcoxtest  ### Not diff

PlotData <- MedResid

Plot <- ggplot(PlotData, aes(x = as.factor(SESSN) , y = Res_med )) +
  geom_boxplot(aes( fill= as.factor(SESSN))) +
  labs(y = "Residual", x = "Session", fill="") +
  geom_hline(yintercept=0, linetype="dashed",color = "red", linewidth=1)+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  theme(text = element_text(size=10)) +
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )
Plot
#ggsave( file="Beta_Reock_Crq_resid_SESSN.eps", plot=Plot, width=6, height=3, units="in")

#####################################################
### Check resids for no ST correlations
####################################################
Medians<- RegData %>% group_by(ST)%>%summarise( Pmedian= median( Polsby), PST=first( STpolsby))
PlotData <- left_join( RegData, Medians)

#### Moody Median test
Mtest <- Median.test(  RegData$Resid, RegData$ST, simulate.p.value= TRUE)
Mtest$statistics
States <- row.names( Mtest$groups)
Mgroups <- data.frame( States, Mtest$groups)
Mgroups <- select( Mgroups, States, groups)
colnames( Mgroups)<- c( "ST", "Groups")
PlotData <- left_join( PlotData, Mgroups)

Plot <- ggplot(PlotData, aes(x =reorder(ST, Pmedian) , y = Resid)) +
  geom_boxplot(aes( fill= as.factor( Groups))) +
  labs(y = "Residual", x = "ST in order of increasing district compactness", fill="Groups of \n indifferent \n medians") +
  geom_hline(yintercept=0, linetype="dashed",color = "red", linewidth=1)+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  #scale_fill_manual(values=c('blue',"purple", "red" ), labels=c("A","Either", "B"))+
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90)) +
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )

Plot
ggsave( file="Resid_Pby_Crq_ST.eps", plot=Plot, width=6, height=3, units="in")

Plot <- ggplot(PlotData, aes(x =reorder(ST, PST) , y = Resid)) +
  geom_boxplot(aes( fill= as.factor( Groups))) +
  labs(y = "Residual", x = "ST in order of increasing state compactness", fill="Groups of \n indifferent \n medians", title="Beta regression residual box plot") +
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  #scale_fill_manual(values=c('blue',"purple", "red" ), labels=c("A","Either", "B"))+
  theme(text = element_text(size=14), axis.text.x = element_text(angle = 90)) +
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )

Plot

###### Wilcox test 
Wilcoxtest <- wilcox_test( Resid~ST, data =RegData)
Wilcoxtest[ Wilcoxtest$p.adj.signif!='ns',]
Wilcox_states <- unique( c( Wilcoxtest$group1, Wilcoxtest$group2) )
length( Wilcox_states)
Wilcox_states

############ Wilcox test versus total population
States <- unique(RegData$ST)
Testout <- data.frame()
for( S in States){
  Base <- filter( RegData, ST!=S)
  Group <- filter( RegData, ST==S)
  Test <- wilcox.test( Base$Resid, Group$Resid)
  if( Test$p.value < 0.05) { print( paste( S, Test$p.value))}
  Tdata <- data.frame( S, Test$p.value)
  Testout <- rbind( Testout, Tdata)
}

sum(Testout$Test.p.value < 0.05)
  
write.csv( file="Polsby Creq Residual ST wilcox test.csv", Testout, row.names = FALSE)
  
########################################################################
### Regress st on resids
##################################################

Id <- make.link('identity')      ### will be used by Betareg to overwrite the logit link function
Ws <- 1/Modeldata$Nd


Vars <- States

Formula.string<- "Resid ~ "
N_mean_params <- 2 + str_count(Formula.string, "\\+")### including intercept and test variable is formula +2
N_mean_params 

Vs <- Vars
Nv <- length(Vs)
Result <- matrix( NA, Nv, 6, dimnames = list( Vs,c( "BIC","AIC","R^2","Percision","Coeff","PvalCoeff")))
for(i in 1:Nv) {
  FS<- paste(Formula.string,Vs[i])
  Formula <- as.formula( FS)
  Betamodel <- lm(Formula, weights = Ws,link=Id,link.phi=Id, data = Modeldata )
  X <- summary( Betamodel)
  Coeffs <- X$coefficients$mean
  Per <- X$coefficients$precision
  Ncoeffs <- nrow( Coeffs)
  Result[i,1]<- BIC(Betamodel)
  Result[i,2]<- AIC(Betamodel)
  Result[i,3]<- Betamodel$pseudo.r.squared
  Result[i,4]<- Per[1,1]
  Result[i,5]<- Coeffs[N_mean_params,1]
  Result[i,6]<- Coeffs[N_mean_params,4]
}
Result[ order(Result[,1]),]
Reg_var

