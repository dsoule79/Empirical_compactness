################################################
### Compare compactness state by state to confirm clipping OK across different census years
################################################

library(dplyr)
rm(list=ls())    #  remove all previous variables and value

### Get district  data
Dpath <- "C:/Users/Owner/Compactness/Papercode/Process data"
setwd(Dpath)

Dpath <- "C:/Users/Owner/Compactness/Papercode/Process data"
setwd(Dpath)
Fname <-"Compiled_data.csv"
CDx <- read.csv( file=Fname)


##### Add CD118 state compactness
Data <- CDx%>%group_by( SESSN,ST)%>%summarise( STPP = mean(STpolsby), STRK=mean(STreock))
Data118 <- Data[ Data$SESSN=='118',]
Data118 <- data.frame( Data118 )
Data118 <- select( Data118, "ST","STPP", "STRK")
colnames( Data118)<- c( "ST","STPP118", "STRK118")
Data <- left_join( Data, Data118)
Data <- Data %>% mutate( Delta_PP = STPP - STPP118, Delta_RK= STRK-STRK118 )
Data <- Data[ Data$SESSN!='118',]
Dataout <- filter( Data, abs(Delta_PP)>0.01 | abs(Delta_RK)>0.01 )
Dataout

