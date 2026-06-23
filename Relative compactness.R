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

#################################################
### Find values
############################################
Sn <- 113   ### Session
St <- 'NC'  ### State
D <-  12    ### District

I <- which( CDx$SESSN==Sn & CDx$ST==St & CDx$DISTRICT==D)
print( I)

t( CDx[I,] %>% select( SESSN, ST, DISTRICT, Polsby, PPrank, PP_Pdistrict,STpolsby, Reock, RKrank, Rk_Pdistrict, STreock ))

I <- which( CDx$SESSN==Sn & CDx$ST==St )
Dataout <- CDx %>% select( SESSN, ST, DISTRICT, Polsby, PPrank, PP_Pdistrict,STpolsby, Reock, RKrank, Rk_Pdistrict, STreock )
write.csv(file="MO118 data.csv", Dataout[I,])

###########################################
### impact of small changes
##########################################

pbetar( 0.39,CDx$Rk_Mu[1],CDx$Rk_Phi[1])
pbetar( 0.29,CDx$Rk_Mu[1],CDx$Rk_Phi[1])
pbetar( 0.19,CDx$Rk_Mu[1],CDx$Rk_Phi[1])

Values <- seq(0.3931, 0.0931, -0.1)
Nvs <- length( Values)
Deltas <- data.frame()
for( i in (1:(Nvs-1)) ){
  P1 <- pbetar( Values[i],CDx$Rk_Mu[1],CDx$Rk_Phi[1]) 
  P2 <- pbetar( Values[i+1],CDx$Rk_Mu[1],CDx$Rk_Phi[1]) 
  Delta <- P1 - P2
  Data <- data.frame( V1=Values[i], V2=Values[i+1], P1=P1, P2=P2, D=Delta)
  Deltas<- rbind( Deltas, Data)
}
Deltas

##########################################################
setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
######################################################
### Find outliers
###################################################

PP_outliers <- CDx%>%filter( PP_Pdistrict <= 0.01) %>% select( SESSN, ST, DISTRICT, Polsby, PPrank, PP_Pdistrict, Reock, RKrank, Rk_Pdistrict )
nrow( PP_outliers)
PP_outliers

PP_outliers <- PP_outliers   %>%mutate(Dis = paste( SESSN,ST,sprintf("%02d", DISTRICT) ) )
PPdata <- data.frame( Metric = 'PP',  District=PP_outliers$Dis, PP_outliers$PP_Pdistrict)
colnames( PPdata)<- c('Metric',"District", 'Relative compactness')

RK_outliers <- CDx%>%filter( Rk_Pdistrict <= 0.01) %>% select( SESSN, ST, DISTRICT, Polsby, PPrank, PP_Pdistrict, Reock, RKrank, Rk_Pdistrict )
nrow( RK_outliers)
RK_outliers

RK_outliers <- RK_outliers   %>%mutate(Dis = paste( SESSN,ST,sprintf("%02d", DISTRICT) ) )
RKdata <- data.frame( Metric = 'RK',  District=RK_outliers$Dis, RK_outliers$Rk_Pdistrict)
colnames( RKdata)<- c('Metric',"District", 'Relative compactness')

Outlier_data <- rbind( PPdata, RKdata )
write.csv( file="Doutliers01.csv", Outlier_data, row.names = FALSE)

############################################################
### Relative compactness histograms
#########################################################
Plot <- ggplot() +
  geom_histogram(data=CDx, aes( x = PP_Pdistrict, y = after_stat(count / sum(count))), 
                 position = "dodge",
                 color = 'black',
                 fill= 'darkgrey',
                 breaks = seq(0, 1, by = 0.1)) + 
  scale_y_continuous(labels = scales::percent, limits=c(0,0.15)) +
  geom_hline(yintercept = 0.1, color = "red", linetype = "dashed", size = 1) +
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )+
  labs(y = "Percent of districts", x = "Relative PP compactness",title="Polsby Popper relative compactness")+
  theme(text = element_text(size = 10))
Plot

ggsave( file="PP_Relcomp_hist.eps", plot=Plot, width=3.2, height=3, units="in")

#### Reock

Plot <- ggplot() +
  geom_histogram(data=CDx, aes( x = Rk_Pdistrict, y = after_stat(count / sum(count))), 
                 position = "dodge",
                 color = 'black',
                 fill= 'darkgrey',
                 breaks = seq(0, 1, by = 0.1)) + 
  scale_y_continuous(labels = scales::percent, limits=c(0,0.15)) +
  geom_hline(yintercept = 0.1, color = "red", linetype = "dashed", size = 1) +
    theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )+
  labs(y = "Percent of districts", x = "Relative Reock compactness", title="Reock relative compactness")+
  theme(text = element_text(size = 10))
Plot
ggsave( file="Rk_Relcomp_hist.eps", plot=Plot, width=3.2, height=3, units="in")



##################################################################
#### Polsby Cumulative prob plot with 0.1 deltas and observed values
###################################################################
Cnames <- c( "PP", "Pval","District")
Points<- as.data.frame( matrix(NA,3,3) )
colnames( Points) <- Cnames

Data <- CDx%>%select( SESSN, ST, DISTRICT, Polsby, PP_Mu, PP_Phi )
colnames( Data) <- c( 'SESSN', 'ST', 'DISTRICT', 'Score', 'Mu', 'Phi')
I1 <- which( Data$SESSN=="118" & Data$ST=="LA" & Data$DISTRICT==2)
PP <- seq( 0.01,0.99,0.001)
Pval <- pbetar( PP,Data$Mu[I1],Data$Phi[I1])
Plotdata1 <- data.frame( PP, Pval, rep( "118 LA Dis. 2", length( PP)))

colnames( Plotdata1) <- Cnames
M1A <- max ( which( Plotdata1$Pval<= 0.5))
Plotdata1[M1A,]
M1B <- max ( which( Plotdata1$PP<= Plotdata1$PP[M1A]-0.1))
Plotdata1[M1B,]
Plotdata <- Plotdata1
Points[1,1] <- Data$Score[I1]
Points[1,2] <- pbetar( Data$Score[I1],Data$Mu[I1],Data$Phi[I1])

I2 <- which( Data$SESSN=="118" & Data$ST=="TX" & Data$DISTRICT==32)
PP <- seq( 0.01,0.99,0.001)
Pval <- pbetar( PP,Data$Mu[I2],Data$Phi[I2])
Plotdata2 <- data.frame( PP, Pval, rep( "118 TX Dis. 32", length( PP)))
colnames( Plotdata2) <- Cnames
Plotdata <- rbind( Plotdata, Plotdata2)
M2A <- max ( which( Plotdata2$Pval<= 0.5))
Plotdata2[M2A,]
M2B <- max ( which( Plotdata2$PP<= Plotdata2$PP[M2A]-0.1))
Plotdata2[M2B,]
Points[2,1] <- Data$Score[I2]
Points[2,2] <- pbetar( Data$Score[I2],Data$Mu[I2],Data$Phi[I2])


I3 <- which( Data$SESSN=="118" & Data$ST=="NE" & Data$DISTRICT==3)
PP <- seq( 0.01,0.99,0.001)
Pval <- pbetar( PP,Data$Mu[I3],Data$Phi[I3])
Plotdata3 <- data.frame( PP, Pval, rep( "118 NE Dis. 3", length( PP)))
colnames( Plotdata3) <- Cnames
Plotdata <- rbind( Plotdata, Plotdata3)
M3A <- max ( which( Plotdata3$Pval<= 0.5))
Plotdata3[M3A,]
M3B <- max ( which( Plotdata3$PP<= Plotdata3$PP[M3A]-0.1))
Plotdata3[M3B,]
Points[3,1] <- Data$Score[I3]
Points[3,2] <- pbetar( Data$Score[I3],Data$Mu[I3],Data$Phi[I3])
Points$District<- "Actual value"

Plot <- ggplot( data=Plotdata)+
  geom_line(aes( x=PP, y=Pval, color=District)) +
  #scale_color_manual(values = c( "blue", "red")) +
  geom_line( data=Plotdata1[M1A:M1B,],aes( x=PP, y=Pval, color=District), linewidth = 2 )+
  geom_line( data=Plotdata2[M2A:M2B,],aes( x=PP, y=Pval, color=District), linewidth = 2 )+
  geom_line( data=Plotdata3[M3A:M3B,],aes( x=PP, y=Pval, color=District), linewidth = 2 )+
  geom_point( data=Points, aes( x=PP, y=Pval, color=District), size=3 ) +
  labs( x="District Poslby Popper score", y="Cumulative beta probability")+
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") ) +
  theme( legend.position = c(0.75,0.2)) +
  theme(text = element_text(size = 10))
Plot

ggsave( file="PP_Deltas.eps", plot=Plot, width=3.1, height=4, units="in")

##################################################################
#### Reock Cumulative prob plot with 0.1 deltas and observed values
###################################################################
Cnames <- c( "PP", "Pval","District")
Points<- as.data.frame( matrix(NA,3,3) )
colnames( Points) <- Cnames

Data <- CDx%>%select( SESSN, ST, DISTRICT, Reock, Rk_Mu, Rk_Phi )
colnames( Data) <- c( 'SESSN', 'ST', 'DISTRICT', 'Score', 'Mu', 'Phi')
I1 <- which( Data$SESSN=="118" & Data$ST=="CO" & Data$DISTRICT==1)
PP <- seq( 0.01,0.99,0.001)
Pval <- pbetar( PP,Data$Mu[I1],Data$Phi[I1])
Plotdata1 <- data.frame( PP, Pval, rep( "118 CO Dis. 1", length( PP)))

colnames( Plotdata1) <- Cnames
M1A <- max ( which( Plotdata1$Pval<= 0.5))
Plotdata1[M1A,]
M1B <- max ( which( Plotdata1$PP<= Plotdata1$PP[M1A]-0.1))
Plotdata1[M1B,]
Plotdata <- Plotdata1
Points[1,1] <- Data$Score[I1]
Points[1,2] <- pbetar( Data$Score[I1],Data$Mu[I1],Data$Phi[I1])

I2 <- which( Data$SESSN=="118" & Data$ST=="WV" & Data$DISTRICT==1)
PP <- seq( 0.01,0.99,0.001)
Pval <- pbetar( PP,Data$Mu[I2],Data$Phi[I2])
Plotdata2 <- data.frame( PP, Pval, rep( "118 WV Dis. 1", length( PP)))
colnames( Plotdata2) <- Cnames
Plotdata <- rbind( Plotdata, Plotdata2)
M2A <- max ( which( Plotdata2$Pval<= 0.5))
Plotdata2[M2A,]
M2B <- max ( which( Plotdata2$PP<= Plotdata2$PP[M2A]-0.1))
Plotdata2[M2B,]
Points[2,1] <- Data$Score[I2]
Points[2,2] <- pbetar( Data$Score[I2],Data$Mu[I2],Data$Phi[I2])


I3 <- which( Data$SESSN=="118" & Data$ST=="FL" & Data$DISTRICT==17)
PP <- seq( 0.01,0.99,0.001)
Pval <- pbetar( PP,Data$Mu[I3],Data$Phi[I3])
Plotdata3 <- data.frame( PP, Pval, rep( "118 FL Dis. 17", length( PP)))
colnames( Plotdata3) <- Cnames
Plotdata <- rbind( Plotdata, Plotdata3)
M3A <- max ( which( Plotdata3$Pval<= 0.5))
Plotdata3[M3A,]
M3B <- max ( which( Plotdata3$PP<= Plotdata3$PP[M3A]-0.1))
Plotdata3[M3B,]
Points[3,1] <- Data$Score[I3]
Points[3,2] <- pbetar( Data$Score[I3],Data$Mu[I3],Data$Phi[I3])
#Points$District<- "Actual value"
Points$District[1]<- "118 CO Dis. 1"
Points$District[2]<- "118 WV Dis. 1"
Points$District[3]<- "118 FL Dis. 17"

Plot <- ggplot( data=Plotdata)+
  #geom_line(aes( x=PP, y=Pval, color=District)) +
  geom_line(aes( x=PP, y=Pval)) +
  #scale_color_manual(values = c( "blue", "red")) +
  geom_line( data=Plotdata1[M1A:M1B,],aes( x=PP, y=Pval), linewidth = 2 )+
  geom_line( data=Plotdata2[M2A:M2B,],aes( x=PP, y=Pval), linewidth = 2 )+
  geom_line( data=Plotdata3[M3A:M3B,],aes( x=PP, y=Pval), linewidth = 2 )+
  geom_point( data=Points, aes( x=PP, y=Pval, color=District), size=3 ) +
  labs( x="District Reock score", y="Cumulative beta probability")+
  theme(plot.margin =  unit(c(0.1,0.2,0.1,0.2),"cm") ) +
  theme( legend.position = c(0.75,0.2)) +
  theme(text = element_text(size = 10))
Plot

ggsave( file="Rk_Deltas.eps", plot=Plot, width=3.1, height=4, units="in")

#####################################################################
### Mapping plot
###################################################################

Plotdata <- CDx%>%select( GEOID, SESSN, ST, DISTRICT, Polsby, PP_Pdistrict )
Plotdata <- Plotdata%>%mutate( Pair_code = paste0(SESSN, GEOID))
PPecdf <- ecdf( Plotdata$Polsby)
Plotdata <- Plotdata%>%mutate( Emp = PPecdf(Polsby))
Plotdata <- Plotdata%>%mutate( Change_rank = PP_Pdistrict - Emp )
Plotdata <- Plotdata%>%mutate( Direction = case_when(
                                            (Change_rank < -0.05) ~ 'Decrease',
                                            (Change_rank > 0.05) ~ 'Increase',
                                            TRUE ~ 'Small change'
))



Plotdata <- Plotdata%>%filter( SESSN==118)
Plotdata <-Plotdata%>% select(Pair_code, Polsby, PP_Pdistrict, Direction )

Pdata_long <- Plotdata  %>%
  pivot_longer(cols = c(Polsby, PP_Pdistrict), names_to = "Condition", values_to = "Value")

Pdata_long <- Pdata_long %>%filter( Direction!='Small change')
Pdata_long$Condition[ Pdata_long$Condition=='PP_Pdistrict'] <- 'Relative compactness'

# 3. Plot
ggplot(Pdata_long, aes(x = Condition, y = Value, group = Pair_code, color=as.factor(Direction))) +
  geom_line() + # Lines connecting pairs
  scale_color_manual(values=c('red','blue','white')) +
  geom_point(aes(), size = 1) + # Points for each value
  labs( x='', color="Ranking \n change")+
  theme( legend.position = c(0.1,0.9))+
  theme_minimal()




