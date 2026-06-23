#################################################################################
###  Load district master data with three decades of data
###  Load Beta regression model coefficients
###  Calculate individual district adjusted compactness score
###  Calculate State mape Pvalues
###  Identify 1% outliers
#################################################################################

library(dplyr)
library(ggplot2, svglite)
library(sf)
library(ggspatial)
library(betareg)
library( rstatix)
library( agricolae)
rm(list=ls())    #  remove all previous variables and value

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
Fname <-"ModelParams_Pby_All_Decade_IL_SDI.csv"
Model <- read.csv( file=Fname)
Model

Intercept <- Model$Estimate[1]
Coeff_Cnt_Pby_Avg <- Model$Estimate[2]
Coeff_RSTPolsby <- Model$Estimate[3]
Coeff_Decade <- Model$Estimate[4]
Phi_intercept <- Model$Estimate[6]
Phi_Coeff_SDI <- Model$Estimate[7]

#CDx <- CDx%>% mutate( PP_Mu = Intercept + Coeff_Cnt_Pby_Avg * Cnt_Pby_Avg + Coeff_RSTPolsby * RSTPolsby)

### With decade                                       
CDx <- CDx%>% mutate( PP_Mu = Intercept + Coeff_Cnt_Pby_Avg * Cnt_Pby_Avg + Coeff_RSTPolsby * RSTPolsby + Coeff_Decade * Decade )

CDx <- CDx%>% mutate( PP_Phi = Phi_intercept )
CDx <- CDx%>% mutate( PP_Phi = Phi_intercept  +Phi_Coeff_SDI * Cnt_Pby_SDI)

CDx <- CDx%>% mutate( PP_Pdistrict = pbetar( Polsby,PP_Mu,PP_Phi))
CDx <- CDx%>% mutate( PP_Delta = Polsby - PP_Mu)          ### Higher scores more compact than expected

#CDx%>%select( ST, DISTRICT, Polsby, PP_Pdistrict )
PPecdf <- ecdf( CDx$Polsby)
CDx <- CDx%>%mutate( PPrank = PPecdf(Polsby))  ### empirical probability as is





### Calc State map compactness Pvalue
Test.data <- data.frame( character(), character(),numeric(), numeric(), numeric(), numeric(), numeric(),numeric())
Tnames <- c( "SESSN","ST","STpval","STpolsby","STavgDistPP", "CSTavg","NDist_outliers", "Min Dist P")
SNs <- unique( CDs$SESSN)
STs <- unique ( CDs$ST)

for ( Sn in SNs){
  for( St in STs){
    STdata <- filter( CDs, SESSN==Sn & ST==St)
    if( nrow(STdata) < 2) next
    Ptest <- ks.test( STdata$Dscore, punif, alternative='greater')$p.value
    Nd <- length( which(STdata$Dscore<0.01))
    ST.test <- data.frame( Sn, St, Ptest, STdata$STpolsby[1], mean(STdata$PolsbyW), STdata$CSTavg[1], Nd, min( STdata$Dscore) )
    colnames(ST.test)<- Tnames
    Test.data <- rbind( Test.data, ST.test)
  }
}

ST_SESSN.dat <- Test.data

CDs <- left_join( CDs, Test.data[,1:3])

### Find individual district outliers
setwd("C:/Users/Owner/Compactness/Papercode/Paperdata")
CDx <- st_drop_geometry(CDs)

Outlier1 <- filter( CDx, Dscore<0.01)
nrow(Outlier1)
write.csv( file="Outliers_District.csv", Outlier1)

#### Find state map outliers
ST_SESSN_outliers <- filter( Test.data, STpval<0.01)
nrow(ST_SESSN_outliers )
write.csv( file="Outliers_ST.csv",ST_SESSN_outliers )

### Find District outliers not also a ST map outlier
Outlier2<- filter( CDx, STpval>0.01 & Dscore < 0.01) 
nrow( Outlier2)
write.csv( file="Outliers_DisXST.csv", Outlier2)

### Find any State map or seperate district as outlier to remove
Removals <- filter( CDx, STpval<0.01 | Dscore < 0.01) 
nrow(Removals)
Removals <- select( Removals, "SESSN","ST","DISTRICT","STpval","Dscore")
setwd("C:/Users/Owner/Compactness/Papercode/Process data")
write.csv( file="Removals.csv", Removals)

#############
### Outlier histograms
###########
setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper new")
Ntest <- nrow( Test.data)
Breaks <- seq(0,1,0.01)
SThist <- hist( Test.data$STpval, breaks= Breaks)
P_STs <- SThist$counts/sum( SThist$counts)
Mids <- SThist$mids
Plotdata <- data.frame(Breaks[-1] ,P_STs)
colnames( Plotdata) <- c("Pval","Percent")
Plotdata <- filter( Plotdata, Pval <= 0.05)

Plot <- ggplot( Plotdata,aes( y=Percent, x=as.factor(Pval)) ) +
  geom_bar(stat='identity') +
  scale_y_continuous(labels = scales::percent, limits =c(0,0.15)) +
  labs(y = "Percentage of Maps", x="ST map Pvalue") +
  theme(text = element_text(size=10))+
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )
Plot

#ggsave( file="Outliers_STmap_hist.eps", plot=Plot, width=2.8, height=2, units="in")

Breaks <- seq(0,1,0.01)
SThist <- hist( CDs$Dscore, breaks= Breaks)
P_STs <- SThist$counts/sum( SThist$counts)
Mids <- SThist$mids
Plotdata <- data.frame(Breaks[-1] ,P_STs)
colnames( Plotdata) <- c("Pval","Percent")
Plotdata <- filter( Plotdata, Pval <= 0.05)

Plot <- ggplot( Plotdata,aes( y=Percent, x=as.factor(Pval)) ) +
  geom_bar(stat='identity') +
  scale_y_continuous(labels = scales::percent, limits =c(0,0.15)) +
  labs(y = "Percentage of Districts", x="District geography adjusted percentile") +
  theme(text = element_text(size=10))+
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )
Plot

#ggsave( file="Outliers_Dist_hist.eps", plot=Plot, width=2.8, height=2, units="in")

##################
### Compare ST pval to AVG compactness
###########################

Plot <- ggplot( data=ST_SESSN.dat, aes(y=STpval,x=STavgDistPP) ) +
  geom_point()+
  geom_smooth(method=lm, color="blue") +
  coord_cartesian(xlim = c(0, 0.6), ylim=c(0,1)) +
  labs( y="ST map bias Pval", x="Avg District compactness in ST") +
  theme(text = element_text(size=14))+
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") )
Plot
#ggsave( file="ST pval v Avg Compactness.svg", plot=Plot, width=6, height=6, units="in")

Indx <- which( ST_SESSN.dat$STpval<0.01)
ST_SESSN.dat[Indx,]

###################################################################
### Plots
###################################################################

### Highlight one district in a state map
St <- "IA"
Sn <- "118"
Ds <- "04"
Plotdata <- filter( CDs, ST==St & SESSN==Sn)
Plotdata <- Plotdata %>% mutate( Out = ifelse(  DISTRICT==Ds,1,0))

#Title <- paste( Sn, St, Ds, "Adjusted =", format(Padj, digits=2) )
#Subtitle <- paste( "ST Polsby=", format(Plotdata$STpolsby[1],digits=2), "County avg Polsby=", format(Plotdata$CSTavg[1],digits=2), "District polsby=", format(Dpolsby,digits=2) )
Fname <- paste0( "Percentile-",Sn,"-",St,"-", Ds,".eps")
Plot <- ggplot(Plotdata) +
  geom_sf( aes(fill = as.factor(Out)) ,colour="white", linewidth=0.5) +
  scale_fill_manual(values=c('lightblue',"darkblue" ), labels=c("Others","ADJ district"))+
  #labs( title= Title,subtitle=Subtitle, fill="" ) +
  labs( fill="" ) +
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )
Plot
#setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper new")
#ggsave( file=Fname, plot=Plot, width=6, units="in")

st_drop_geometry(Plotdata)  #### Print data for state 


#################################
### Plot District outliers in a state
#################################
St <- "TX"
Sn <- "113"
Plotdata <- filter( CDs, ST==St & SESSN==Sn)
Plotdata <- Plotdata %>% mutate( Out = ifelse( Dscore<0.01,1,0))

Title <- paste( Sn, St )
Fname <- paste0( "Map outliers",Sn,"-",St,".svg")
Plot <- ggplot(Plotdata) +
  geom_sf( aes(fill = as.factor(Out)) ,colour="white", linewidth=0.5) +
  scale_fill_manual(values=c('lightblue',"red" ), labels=c("Others","Outlier"))+
  labs( title= Title, fill="" ) +
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )
Plot
st_drop_geometry(Plotdata)  #### Print data for state 
#ggsave( file=Fname, plot=Plot, width=6, height = 6, units="in")

##################################################
#### Plot District geographically adjusted percentiles
#####################################################
St <- "TX"
Sn <- "113"
Plotdata <- filter( CDs, ST==St & SESSN==Sn)
Plotdata <- Plotdata %>% mutate( Out = ifelse( Dscore<0.01,1,0))

Title <- paste( Sn, St )
Fname <- paste0( "Map outliers",Sn,"-",St,".svg")
Plot <- ggplot(Plotdata) +
  geom_sf( aes(fill = Dscore ) ,colour="white", linewidth=0.5) +
  scale_fill_gradient2(midpoint=0.5, low="red", mid="white",high="blue" )+
  labs( title= Title ) +
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )
Plot
st_drop_geometry(Plotdata)  #### Print data for state 
#ggsave( file=Fname, plot=Plot, width=6, height = 6, units="in")



