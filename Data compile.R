######################################################
#### Compile data from multple census files
#### Remove single district states and any water regions
#### Calculate various explanatory factors
###################################################

library(dplyr)
library(sf)
rm(list=ls())    #  remove all previous variables and value

### Get three decades data
Dpath <- "C:/Users/Owner/Compactness/Papercode/Process data"
setwd(Dpath)

print("Reading CD118")
Shapes <- read_sf("CD118.shp")
#Shapes <- st_transform( Shapes, crs ="EPSG:4326")      ### transform to WGS 84 coordiante system
Shapes <- Shapes%>%filter(DISTRICT != "ZZ")
nrow(Shapes)
CDs <- Shapes

print("Reading CD113")
Shapes <- read_sf("CD113.shp")
#Shapes <- st_transform( Shapes, crs ="EPSG:4326")      ### transform to WGS 84 coordiante system 
Shapes <- Shapes%>%filter(DISTRICT != "ZZ")
nrow(Shapes)
CDs <- rbind( CDs, Shapes)

print("Reading CD108")
Shapes <- read_sf("CD108.shp")
#Shapes <- st_transform( Shapes, crs ="EPSG:4326")      ### transform to WGS 84 coordiante system 
Shapes <- Shapes%>%filter(DISTRICT != "ZZ")
nrow(Shapes)
CDs <- rbind( CDs, Shapes)
nrow(CDs)

State <- read.csv( "Statenames.csv")                  ### Update state codes with real names
State$STATEFP<- sprintf( "%02d", State$STATEFP)
CDs<- left_join( CDs, State, by='STATEFP')

CDx <- st_drop_geometry(CDs)   ### Note runs much faster without geometry will add back later

### Remove single dist states
Ndists <- CDx%>%group_by( SESSN, ST) %>%summarise(Count=n())
Onedist <- Ndists%>%filter( Count==1)
nrow(Onedist)
ST108 <- Onedist%>%filter( SESSN=="108")
Indx1 <- which( CDx$SESSN=="108" & CDx$ST %in% ST108$ST)
ST113 <- Onedist%>%filter( SESSN=="113")
Indx2 <- which( CDx$SESSN=="113" & CDx$ST %in% ST113$ST)
ST118 <- Onedist%>%filter( SESSN=="118")
Indx3 <- which( CDx$SESSN=="118" & CDx$ST %in% ST118$ST)
Indx <- c( Indx1,Indx2,Indx3)
length(Indx)
CDx <- CDx[-Indx,]
Shape_Indx <- Indx
nrow(CDx)

#### Get county data
Cdata <- read.csv( "County2023.csv")
nrow(Cdata)
Cdata <- Cdata %>%filter( STATEFP!= 11)     ### remove DC as single county state
nrow(Cdata)
CSTdata <- Cdata%>%group_by(STATEFP)%>% summarise( CSTavg=mean(PolsbyW),CSTmed=median(PolsbyW), CSTmin=min(PolsbyW))
Cdata <- left_join(Cdata, CSTdata)
Cdata <- Cdata %>% mutate( Cdev = abs(PolsbyW-CSTmed))
Cdevdata <- Cdata%>%group_by(STATEFP)%>%summarise( CSTmad=mean(Cdev), CSTsd=sd(PolsbyW) )
CSTdata <- left_join(CSTdata, Cdevdata)
CSTdata$STATEFP <- sprintf("%02d",CSTdata$STATEFP )

###### Update ST data
CDx <- CDx%>%mutate( STLarea = log( STarea), STLperim = log(STperim))
STdata <- CDx%>%group_by(ST, SESSN)%>%summarise( Nd=n(), LNd= log( n()))
CDx <- left_join( CDx, STdata)
CDx <- CDx%>%mutate( STLAD = STLarea/Nd)
CDx <- CDx%>%mutate( S108 = ifelse( SESSN=="108", 1,0))
CDx <- CDx%>%mutate( S113 = ifelse( SESSN=="113", 1,0))
CDx <- CDx%>%mutate( S118 = ifelse( SESSN=="118", 1,0))
CDx <- CDx%>%mutate( Decade = 0)
CDx$Decade[ CDx$SESSN=="108"] <- -2
CDx$Decade[ CDx$SESSN=="113"] <- -1
CDx$DSTperim <- as.numeric( CDx$DSTperim)
CDx <- CDx %>% mutate( RST = DSTperim/Dperimeter )
CDx <- CDx %>% mutate( RSTPolsby = STpolsby*DSTperim/Dperimeter )
CDx <- CDx %>% mutate( RCST = Coast_len/Dperimeter)
CDx <- CDx %>% mutate( Coast = ifelse( RCST>0.1,1,0))
CDx <- left_join( CDx, CSTdata)
CDx <- CDx %>% mutate( STposlby2 = STpolsby^2)
CDx <- CDx %>% mutate( CSTavg2 = CSTavg^2)
CDx <- CDx %>% mutate( CSTsdI = 1/CSTsd)
CDx <- CDx %>% mutate( CSTmadI = 1/CSTmad)

#### Create and output master data file with geometry
CDshapes  <- CDs %>% select( "STATEFP","DISTRICT","SESSN","geometry")
CD <- left_join(CDx, CDshapes)

CD$Dperimeter <- as.integer( CDx$Dperimeter)
CD$STperim <- as.integer( CDx$STperim)
CD$Darea <- CD$Darea/10e6              ### Note shapefile format can not accomodate very large numbers
CD$STarea <- CD$STarea/10e6

st_write( CD,"District_master.shp", append=FALSE )

