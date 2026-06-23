######################################################
#### Compile data from multple census files
#### Remove single district states 
#### Calculate various explanatory factors
###################################################

library(dplyr)
rm(list=ls())    #  remove all previous variables and value
Dpath <- "C:/Users/Owner/Compactness/Papercode/Process data"
setwd(Dpath)

#### Get mid decade data use for out of sample test
Fileout <- 'Compiled_Mid_decade_data.csv'
CDx <- read.csv("Mid_decade_stats.csv")

State <- read.csv( "Statenames.csv")                  ### Update state codes with real names
#State$STATEFP<- sprintf( "%02d", State$STATEFP)
CDx<- left_join( CDx, State, by='STATEFP')

################  Add state populations
Pfiles <- c( '2000','2010','2020')
SESSNs <- c( 118)
PopData <- data.frame(
  STATE = character(0),
  POP_REP = numeric(0),
  SESSN = character(0),
  stringsAsFactors = FALSE
  )

for (i in 3) {
  Fname <- paste0('Apportionment',Pfiles[i],'.csv')
  Pdata <- read.csv( Fname)
  Pdata['SESSN']<- SESSNs[i]
  PopData <- rbind( PopData,Pdata)
}
CDx <- left_join( CDx, PopData, by=c( 'STATE'='STATE'))
CDx <- CDx%>%mutate( Pop_Den = 10e6*ST_POP/STarea)

### Rename column and remove single dist states
CDx <- CDx%>% mutate( Nd = REPs)
CDx <- CDx%>%select( -REPs)
CDx <- CDx %>% filter( Nd>1) 

#### Get county data and summarize by state
Cdata <- read.csv( "County2023.csv")

Cdata <- Cdata%>%mutate(Log_Polsby = log(PolsbyW))  
Cdata <- Cdata%>%mutate(Inv_Polsby = 1/PolsbyW)
Cdata <- Cdata%>%mutate(Log_Reock = log(Reock))
Cdata <- Cdata%>%mutate(Inv_Reock = 1/Reock)
Cdata <- Cdata%>%mutate(Log_ReockX = log(ReockX))
Cdata <- Cdata%>%mutate(Inv_ReockX = 1/ReockX)

CSTdata <- Cdata%>%group_by(STATEFP)%>% summarise( Cnt_Pby_Avg=mean(PolsbyW),Cnt_Pby_Med=median(PolsbyW), Cnt_Pby_Gvg=mean(Log_Polsby),Cnt_Pby_Hvg=mean(Inv_Polsby), Cnt_Pby_Min=min(PolsbyW),Cnt_Pby_SD=sd(PolsbyW),
                                                  Cnt_Rk_Avg=mean(Reock),Cnt_Rk_Med=median(Reock),Cnt_Rk_Gvg=mean(Log_Reock),Cnt_Rk_Hvg=mean(Inv_Reock), Cnt_Rk_Min=min(Reock), Cnt_Rk_SD=sd(Reock),
                                                  Cnt_RkX_Avg=mean(ReockX),Cnt_RkX_Med=median(ReockX),Cnt_RkX_Gvg=mean(Log_ReockX),Cnt_RkX_Hvg=mean(Inv_ReockX), Cnt_RkX_Min=min(ReockX), Cnt_RkX_SD=sd(ReockX))

CSTdata$Cnt_Pby_Gvg <- exp(CSTdata$Cnt_Pby_Gvg)  ## geometric avg
CSTdata$Cnt_Rk_Gvg <- exp(CSTdata$Cnt_Rk_Gvg)
CSTdata$Cnt_RkX_Gvg <- exp(CSTdata$Cnt_RkX_Gvg)
CSTdata$Cnt_Pby_Hvg <- 1/CSTdata$Cnt_Pby_Hvg    ## harmonic avg
CSTdata$Cnt_Rk_Hvg <- 1/CSTdata$Cnt_Rk_Hvg
CSTdata$Cnt_RkX_Hvg <- 1/CSTdata$Cnt_RkX_Hvg

CDx <- left_join( CDx, CSTdata, by=c('STATEFP'='STATEFP') )

#### Calculate other parameters
CDx <- CDx%>%mutate( LNd = log( Nd))
CDx$STarea <- as.numeric(CDx$STarea)
CDx <- CDx%>%mutate( STLarea= log(STarea))
CDx <- CDx%>%mutate( STLarea_Nd = STLarea/Nd)
CDx <- CDx%>%mutate( LPop_Den = log(Pop_Den))
#CDx <- CDx%>%mutate( S108 = ifelse( SESSN=="108", 1,0))
#CDx <- CDx%>%mutate( S113 = ifelse( SESSN=="113", 1,0))
CDx <- CDx%>%mutate( S118 = 1)
CDx <- CDx%>%mutate( Decade = 2)
#CDx$Decade[ CDx$SESSN=="113"] <- 1
#CDx$Decade[ CDx$SESSN=="118"] <- 2
CDx <- CDx %>% mutate( RSTPerim = DSTperim/Dperimeter )
CDx$Coast_len <- as.numeric( CDx$Coast_len)
CDx <- CDx %>% mutate( RSTPolsby = STpolsby*DSTperim/Dperimeter )
CDx <- CDx %>% mutate( RCoast = Coast_len/Dperimeter)
CDx <- CDx %>% mutate( Coast = ifelse( RCoast>0.1,1,0))

CDx <- CDx %>% mutate( STposlby2 = STpolsby^2)
CDx <- CDx %>% mutate( STreock2 = STreock^2)
CDx <- CDx %>% mutate( Cnt_Pby_Avg2 = Cnt_Pby_Avg^2)
CDx <- CDx %>% mutate( Cnt_Pby_Gvg2 = Cnt_Pby_Gvg^2)
CDx <- CDx %>% mutate( Cnt_Pby_Hvg2 = Cnt_Pby_Hvg^2)
CDx <- CDx %>% mutate( Cnt_Pby_Med2 = Cnt_Pby_Med^2)
CDx <- CDx %>% mutate( Cnt_Rk_Avg2 = Cnt_Rk_Avg^2)
CDx <- CDx %>% mutate( Cnt_Rk_Gvg2 = Cnt_Rk_Gvg^2)
CDx <- CDx %>% mutate( Cnt_Rk_Hvg2 = Cnt_Rk_Hvg^2)
CDx <- CDx %>% mutate( Cnt_Rk_Med2 = Cnt_Rk_Med^2)
CDx <- CDx %>% mutate( Cnt_RkX_Avg2 = Cnt_RkX_Avg^2)
CDx <- CDx %>% mutate( Cnt_RkX_Gvg2 = Cnt_RkX_Gvg^2)
CDx <- CDx %>% mutate( Cnt_RkX_Hvg2 = Cnt_RkX_Hvg^2)
CDx <- CDx %>% mutate( Cnt_RkX_Med2 = Cnt_RkX_Med^2)

CDx <- CDx %>% mutate( Cnt_Pby_SDI = 1/Cnt_Pby_SD)
CDx <- CDx %>% mutate( Cnt_Rk_SDI = 1/Cnt_Rk_SD)
CDx <- CDx %>% mutate( Cnt_RkX_SDI = 1/Cnt_RkX_SD)

CDx$Darea <- CDx$Darea/10e6              
CDx$STarea <- CDx$STarea/10e6

#########################################
### Compare districts in a state with compactness requirement to others
#######################################
ST_with_compactness_req <- c('AL','AZ','CA','CO','FL','HI','ID','IA',
                             'KS','ME','MI','MN','MS','MO','MT','NE','NM','NY',
                             'NC','OH','OK','PA','RI','SC','UT','VA','WA',
                             'WV')


CDx <- CDx %>%mutate( CReq=ifelse( ST %in% ST_with_compactness_req,1,0))

write.csv( file=Fileout, CDx, row.names=FALSE)
print(paste( 'Wrote', nrow(CDx), 'to', Fileout))
#colnames(CDx)

##################################################################
### Multicolinjearity check
##################################################################
Reg_var <-c( "Polsby", "Reock", "STpolsby",  "STreock","STLarea" , "STLarea_Nd",     
             "POP_REP", "LPop_Den","LNd",
             "Cnt_Pby_Avg","Cnt_Pby_Min",
             "Cnt_Rk_Avg", "Cnt_Rk_Min",
             "S108", "S113", "S118", "Decade", "RSTPerim" ,   
             "RSTPolsby", "RCoast" , "Coast",
             "Cnt_Pby_SDI",  "Cnt_Rk_SDI" , 
             "CReq"  )

RegData <- select( CDx, all_of(Reg_var))

CorrData <- data.frame(
  Var1 = character(0),
  Var2 = character(0),
  Corr = numeric(0),
  stringsAsFactors = FALSE
)

Nvs <- length(Reg_var)
for (i in 1:(Nvs-1)) {
  for (j in (i+1):Nvs){
    if (i==j) next
    Corr <- list( Var1=Reg_var[i], Var2=Reg_var[j], Corr=cor( RegData[,i], RegData[,j]))
  CorrData <- rbind( CorrData, Corr)
    }}  
 
 write.csv( file="Correlations.csv", CorrData, row.names = FALSE) 

High_corrs <- CorrData%>%filter( abs(Corr)>0.7)  

High_corrs

CorrData[ order( CorrData$Corr),]

colnames(RegData)

