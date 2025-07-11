#################################################################################
###  Load district master data with three decaeds of data
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


### Get district  data
Dpath <- "C:/Users/Owner/Compactness/Papercode/Process data"
setwd(Dpath)

Shapes <- read_sf("District_master.shp")
nrow(Shapes)
CDx <- st_drop_geometry(Shapes)

#################################
## Add state indicator variables
###############################
States <- unique( CDx$ST)

Modeldata <- CDx
for( ST in States){
  NewCol <- rep(0,nrow(Modeldata))
  Indx <- which( Modeldata$ST== ST)
  NewCol[Indx]<-1
  #print( paste( ST, sum(NewCol)))
  Modeldata <- cbind( Modeldata, NewCol)
  colnames(Modeldata)[[ncol(Modeldata)]] <- ST
}

##############################################
### Remove outliers - skip if doing intial regression
### Use list of SESSN-ST-DISTRICT in removals file 
############################################
nrow(Modeldata)
Removals <- read.csv( file="Removals.csv")
Removals$DISTRICT <- sprintf( "%02d",Removals$DISTRICT )
Removals$SESSN <- as.character(Removals$SESSN)
Modeldata <- left_join( Modeldata, Removals )
Modeldata <- Modeldata %>% filter( is.na(Dscore))
Modeldata <- Modeldata %>% select( -"X", -"STpval",-"Dscore")
nrow(Modeldata)

########################################################
### Beta regressions
########################################################
BIC <- function( Fit){
  BIC <- ( Fit$df.null- Fit$df.residual)*log( Fit$n)- 2*Fit$loglik
  return( BIC)
}

################################################################################
### Beta regression versus a constant - base case
##############################################################################
Id <- make.link('identity')      ### will be used by Betareg to overwrite the logit link function
Ws <- 1/Modeldata$Nd
Beta_NOvars <-
  betareg( PolsbyW ~ 1, 
           weights = Ws,
           data = Modeldata,
           link = Id
  )

summary(Beta_NOvars)
AIC( Beta_NOvars)
BIC( Beta_NOvars)

###############################################################
#### Beta regressin with All variables
###  To avoid correlation issues need to do state indicators separate from explanatory variables
##############################################################
Id <- make.link('identity')      ### will be used by Betareg to overwrite the logit link function
Ws <- 1/Modeldata$Nd

Vars <- colnames( Modeldata)[c(15:30, 34)]   ####  Explanatory variables
Vars <- colnames( Modeldata)[c(37:79)]                 ####  State indicators


Vars <- Vars[-c(6:8)]
VarsF <- paste( Vars, collapse="+")
Formula <- as.formula( paste( "PolsbyW ~", VarsF))

Beta_all <-
  betareg( Formula , 
           weights = Ws,
           data = Modeldata,
           link = Id,
           link.phi = Id
  )

summary(Beta_all)
X <- summary( Beta_all)
write.csv( file="Beta ALL w all vars1.csv", X$coefficients$mean)

AIC( Beta_all)
BIC( Beta_all)

#############################################################################################
####  Beta best model
#######################################################################################

Id <- make.link('identity')      ### will be used by Betareg to overwrite the logit link function
Ws <- 1/Modeldata$Nd
Beta_best <-
  betareg( PolsbyW ~  RSTPolsby + CSTavg , 
           weights = Ws,
           data = Modeldata,
           link = Id,
           link.phi = Id
  )

summary(Beta_best)
AIC( Beta_best)
BIC( Beta_best)

Pnames <- c( "Intercept", "RSTPolsby","CSTavg","Phi")
X <- summary( Beta_best)
Parameters<- c( X$coefficients$mean[1:3], X$coefficients$precision[1])
Pvalues<- c( X$coefficients$mean[10:12], X$coefficients$precision[4])
Pmodel<- data.frame( Pnames,Parameters, Pvalues)
setwd("C:/Users/Owner/Compactness/Papercode/Process data")
#write.csv( file="ModelParams_ALL.csv", Pmodel)
write.csv( file="ModelParams_X.csv", Pmodel)

############################################################################
#### Additive variable selection Mean only
########################################################################
Id <- make.link('identity')      ### will be used by Betareg to overwrite the logit link function
Ws <- 1/Modeldata$Nd

Vars <- colnames( Modeldata)[c(15:30, 34)]   ####  Explanatory variables

Formula.string<- "PolsbyW ~ RSTPolsby + CSTavg +"
Vs <- Vars
Vs <- Vars[-c(11,14)]
Nv <- length(Vs)
Result <- matrix( NA, Nv, 7, dimnames = list( Vs,c( "BIC","AIC","R^2","Per", "PvalPer","Coeff","PvalCoeff")))
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
  Result[i,5]<- Per[1,4]
  Result[i,6]<- Coeffs[Ncoeffs,1]
  Result[i,7]<- Coeffs[Ncoeffs,4]
}
Result[ order(Result[,1]),]

write.csv( file="Beta All add a ST.csv",Result[ order(Result[,1]),] )

#####################################################################################
### Additive with best mean model and percision
#######################################################################################
Id <- make.link('identity')      ### will be used by Betareg to overwrite the logit link function
Ws <- 1/Modeldata$Nd

Vars <- colnames( Modeldata)[15:36]
Formula.string<- "PolsbyW~ RSTPolsby + CSTavg | "

Vs <- Vars
Nv <- length(Vs)
Result <- matrix( NA, Nv, 4, dimnames = list( Vs,c( "BIC","R^2", "Coeff","Pval")))
for(i in 1:Nv) {
  FS<- paste(Formula.string,Vs[i])
  Formula <- as.formula( FS)
  Betamodel <- betareg(Formula, weights = Ws,link=Id, link.phi='identity', data = Modeldata )
  Result[i,1]<- BIC(Betamodel)
  Result[i,2]<- Betamodel$pseudo.r.squared
  X <- summary(Betamodel)
  Result[i,3]<- X$coefficients$precision[2,1]
  Result[i,4]<- X$coefficients$precision[2,4]
}
Result[ order(Result[,1]),]

###############################################################################
#### Model analysis
######################################################################
setwd("C:/Users/Owner/Compactness/Papercode/Paperdata")

StdResids <- residuals( Beta_best, type="pearson")
Fit <- Beta_best$fitted.values
Resid <- Beta_best$residuals
NC <- ncol( Modeldata)
RegData <- cbind( Modeldata, Fit,Resid, StdResids)
colnames( RegData)[[NC+1]]<-"Fit"
colnames( RegData)[[NC+2]]<-"Resid"
colnames( RegData)[[NC+3]]<-"StdResid"

#######################################################################################
### Residual plots
###############################################################################

Plot <- ggplot(RegData, aes(x = Resid)) + 
  geom_histogram( aes(y = after_stat(count)/sum(after_stat(count))), binwidth = 0.05, color="white") + 
  scale_y_continuous(labels = scales::percent) +
    labs(y = "Percentage of districts", x="Poslby Popper residual") 

Plot
ggsave( file="Beta_ALL_resid_hist.eps", plot=Plot, width=6, height=3, units="in")

Plot <- ggplot(RegData, aes(x =Fit , y = Resid)) +
  geom_point( aes(color=SESSN)) +
  labs(y = "PP Residual", x = "Fitted PP") +
  geom_smooth(method = "lm") +
  scale_x_continuous( breaks=seq(0,1,0.1))+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  theme(text = element_text(size=10)) +
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )
Plot
ggsave( file="Beta_ALL_resid_fitted.eps", plot=Plot, width=6, height=3, units="in")

Plot <- ggplot(RegData, aes(x =CSTavg , y = Resid)) +
  geom_point( aes(color=SESSN)) +
  labs(y = "PP Residual", x = "CSTavg") +
  geom_smooth(method = "lm") +
  scale_x_continuous( breaks=seq(0,1,0.1))+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  theme(text = element_text(size=10)) +
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )
Plot
ggsave( file="Beta_ALL_resid_CSTavg.eps", plot=Plot, width=6, height=3, units="in")

Plot<- ggplot(RegData, aes(x =RSTPolsby , y = Resid)) +
  geom_point( aes(color=SESSN)) +
  labs(y = "PP Residual", x = "RSTPoslby") +
  geom_smooth(method = "lm") +
  scale_x_continuous( breaks=seq(0,1,0.1))+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  theme(text = element_text(size=10)) +
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )
Plot
ggsave( file="Beta_ALL_resid_RSTPolsby.eps", plot=Plot, width=6, height=3, units="in")
###################################################################
### residuals by Session
######################################################################
Medians<- RegData %>% group_by(ST)%>%summarise( Pmedian= median( PolsbyW), PCT=first(CSTavg), PST=first(STpolsby))
PlotData <- left_join( RegData, Medians)


Plot <- ggplot(PlotData, aes(x = SESSN , y = Resid)) +
  geom_boxplot(aes( fill= SESSN)) +
  labs(y = "PP Residual", x = "Session", fill="") +
  geom_hline(yintercept=0, linetype="dashed",color = "red", linewidth=1)+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  theme(text = element_text(size=10)) +
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )
Plot
ggsave( file="Beta_ALL_resid_SESSN.eps", plot=Plot, width=6, height=3, units="in")

STanova <- aov(Resid ~ SESSN, data = RegData)
summary(STanova)   ### SESSN is significant

Test <- kruskal.test( Resid ~ SESSN, data = RegData)  #### NULL is no difference in medians
Test  ###Result  Not all  the same

Mtest <- Median.test(  RegData$Resid, RegData$SESSN, simulate.p.value= TRUE)
Mtest$statistics 

Wilcoxtest <- wilcox_test( Resid~SESSN, data =RegData)
Wilcoxtest   ### they ae all diff

#####################################################
### Check resids for no ST correlations
####################################################
Medians<- RegData %>% group_by(ST)%>%summarise( Pmedian= median( PolsbyW), PCT=first(CSTavg), PST=first(STpolsby))
PlotData <- left_join( RegData, Medians)

STanova <- aov(Resid ~ ST, data = RegData)
summary(STanova)   ### ST is significant

Test <- kruskal.test( Resid ~ ST, data = RegData)  #### NULL is no diffrence in medians
Test  ###Result  Not all  the same

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
  labs(y = "PP Residual", x = "ST in order of increasing district compactness", fill="Groups of \n indifferent \n medians") +
  geom_hline(yintercept=0, linetype="dashed",color = "red", linewidth=1)+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  #scale_fill_manual(values=c('blue',"purple", "red" ), labels=c("A","Either", "B"))+
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90)) +
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )

Plot
ggsave( file="Resid_ALL_Moody.eps", plot=Plot, width=6, height=3, units="in")

Plot <- ggplot(PlotData, aes(x =reorder(ST, PST) , y = Resid)) +
  geom_boxplot(aes( fill= as.factor( Groups))) +
  labs(y = "PP Residual", x = "ST in order of increasing state compactness", fill="Groups of \n indifferent \n medians", title="Beta regression residual box plot") +
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  scale_y_continuous( breaks=seq(-1,1,0.1))+
  #scale_fill_manual(values=c('blue',"purple", "red" ), labels=c("A","Either", "B"))+
  theme(text = element_text(size=14), axis.text.x = element_text(angle = 90)) +
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )

Plot

###### Wilcox test 
Wilcoxtest <- wilcox_test( Resid~ST, data =RegData)
Wilcoxtest
write.csv( file="Beta ALL ST wilcox test.csv", Wilcoxtest)

Plotdata1 <- Wilcoxtest%>%select( 'group1', 'group2','p.adj')
Plotdata1 <- Plotdata1 %>% mutate( Sig = ifelse( p.adj < 0.05,1,0))
Counts<-     Plotdata1%>%group_by(group1)%>%summarise( Nbad = sum( Sig ))
colnames( Counts)<- c( "ST","Nbad")
Counts2<-  Plotdata1%>%group_by(group2)%>%summarise( Nbad = sum( Sig ))
colnames( Counts2)<- c( "ST","Nbad")
Countsall <- merge( Counts, Counts2, by="ST")
#Plotdata1 <- left_join(Plotdata1, Counts)

Countsall <- Countsall %>% mutate( Nbad.tot = Nbad.x + Nbad.y)

print( Countsall[ Countsall$Nbad.tot>0,c(1,4)])
Nbad <- sum( Plotdata1$Sig)
Pbad <- Nbad/nrow(Plotdata1)
print( paste( Nbad ," Significantly different state pairs;",Pbad," of all state pairs"))

Wilcoxtest[ Wilcoxtest$p.adj.signif!="ns",]

Medians<- RegData %>% group_by(ST)%>%summarise( Pmedian= median( PolsbyW))
colnames( Medians)<- c('group1','Pmedian')
Plotdata1 <- left_join( Plotdata1, Medians)
colnames( Medians)<- c('group2','Pmedian2')
Plotdata1 <- left_join( Plotdata1, Medians)

Plotdata2 <- data.frame( Plotdata1$group2, Plotdata1$group1, Plotdata1$p.adj, Plotdata1$Sig, Plotdata1$Pmedian2, Plotdata1$Pmedian)
colnames( Plotdata2)<- colnames(Plotdata1)
Plotdata <- rbind( Plotdata1, Plotdata2)

Plot <- ggplot(Plotdata, aes(x = reorder(group1, Pmedian), y = reorder(group2, Pmedian2), fill = as.factor( Sig )) ) +
  geom_tile() +
  labs( x="In order of increasing district compactness", y="In order of increasing district compactness", fill="")+
  scale_fill_manual( values=c("lightblue","red" ),labels=c("Similar","Different"))+
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))+
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )

Plot
ggsave( file="Resid_ALL_MWtest.eps", plot=Plot, width=6, height=4, units="in")

