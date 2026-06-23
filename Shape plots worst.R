
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(lwgeom)
library(betareg)
rm(list=ls())    #  remove all previous variables and value

##### load district descriptive data
Dpath <- "C:/Users/Owner/Compactness/Papercode/Process data"
setwd(Dpath)
Fname <-"Compiled_data.csv"
CDx <- read.csv( file=Fname)
CDx$GEOID <- as.character( sprintf("%04d", CDx$GEOID))
CDx$SESSN <- as.character( sprintf("%03d", CDx$SESSN))

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

##########################################
### Combine with geo data
##############################################################
Shapes118 <- read_sf("CD118raw.shx")
Shapes1 <- Shapes118%>%select( GEOID, SESSN, geometry)
Shapes113 <- read_sf("CD113raw.shx")
Shapes2 <- Shapes113%>%select( GEOID, SESSN, geometry)
Shapes <- rbind( Shapes1, Shapes2)
Shapes108 <- read_sf("CD108raw.shx")
Shapes3 <- Shapes108%>%select( GEOID, SESSN, geometry)
Shapes <- rbind( Shapes,Shapes3)
CDs <- left_join( Shapes, CDx)
CDr <- CDs%>% select( SESSN, ST, DISTRICT, Polsby, Reock, CReq)

#################################################
### Find and plot lowest compactness
###############################################
I1 <- which( CDs$Polsby==min( CDs$Polsby, na.rm=TRUE))
I2 <- which( CDs$Reock==min( CDs$Reock, na.rm=TRUE )  )

CDr[I1,]
CDr[I2,]


Plotdata <- CDs%>% filter( SESSN=='108', ST=='MD')
Plotdata <- Plotdata %>% mutate( Outlier = if_else( DISTRICT==5,1,0))
Plot <- ggplot(Plotdata) +
  geom_sf(aes(fill = as.factor(Outlier) )) +
  scale_fill_manual(values = c("1" = "red", "0" = "lightblue")) +
  #labs( title="North Carolina congressional map for session 113")+
  labs( title="Maryland congressional map for session 108")+
  theme( legend.position = 'none')+
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )
Plot
setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
ggsave( file="108MD05.eps", plot=Plot, width=6, height=3, units="in")
ggsave( file="113NC12.eps", plot=Plot, width=6, height=3, units="in")



#########################################################
###  Find worst cases
####################################################
CDr <- CDs%>% filter( CReq==1)

I1 <- which( CDr$Polsby==min( CDr$Polsby[ CDr$SESSN=='118']) )
I2 <- min(which( CDr$Polsby==min( CDr$Polsby[ CDr$SESSN=='113']) ) )
I3 <- which( CDr$Polsby==min( CDr$Polsby[ CDr$SESSN=='108']) )

Worst <- c( I1, I2, I3)

Plotdata <- CDr %>% select( GEOID, SESSN, ST, DISTRICT,Polsby, Reock )

Plot <- ggplot(Plotdata[ (Plotdata$ST=="NC" & Plotdata$SESSN=="113"),]) +
  geom_sf(fill = "#69b3a2", color = "black") +
  #facet_grid( rows=vars(SESSN) )+
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )
Plot

Plot1


I1 <- which( CDr$Polsby==min( CDr$Polsby) )
I2 <- which( CDr$Reock==min( CDr$Reock) )



dDpath <- "C:/Users/Owner/Compactness/Census/"
setwd(Dpath)
Blaugreen <- "#69b3a2"

Shapes118 <- read_sf("CD118/CD118.shx")
Shapes118 <- st_transform( Shapes118, crs ="EPSG:4326")      ### transform to WGS 84 coordiante system same as coastline file
nrow(Shapes118)
colnames( Shapes118)[[4]]<- "CDSESSN"
Shapes118 <- Shapes118%>%select( STATEFP, CDSESSN, geometry)

Shapes113 <- read_sf("CD113/tl_2013_us_cd113.shx")
Shapes113 <- st_transform( Shapes113, crs ="EPSG:4326")      ### transform to WGS 84 coordiante system same as coastline file
#Shapes113 <- st_set_precision(Shapes, 1e-3)
nrow(Shapes113)
Shapes113 <- Shapes113%>%select( STATEFP, CDSESSN, geometry)
Shapes113 <- Shapes113%>%mutate(Style="113")

Shapes108 <- read_sf("CD108/tl_2010_us_cd108.shx")
Shapes108 <- st_transform( Shapes108, crs ="EPSG:4326")      ### transform to WGS 84 coordiante system same as coastline file
#Shapes <- st_set_precision(Shapes, 1e-3)
colnames( Shapes108)[[1]]<- "STATEFP"
nrow(Shapes108)
Shapes108 <- Shapes108%>%select( STATEFP, CDSESSN, geometry)
Shapes108 <- Shapes108%>%mutate(Style="108")

Dpath <- "C:/Users/Owner/Compactness/Census/"
setwd(Dpath)
print("Reading CD113")
Shapes113C <- read_sf("CD113/CD113.shp")
Shapes113C <- st_transform( Shapes113C, crs ="EPSG:4326")      ### transform to WGS 84 coordiante system 
Shapes113C <- Shapes113C%>%filter(DISTRICT != "ZZ")
nrow(Shapes113C)
colnames( Shapes113C)[[4]]<- "CDSESSN"
Shapes113C <- Shapes113C%>%select( STATEFP, CDSESSN, geometry)
Shapes113C <- Shapes113C%>%mutate(Style="113 Clipped")

Shapes <- rbind( Shapes108, Shapes113)
Shapes <- rbind( Shapes, Shapes113C)

State <- read.csv( "Statenames.csv")
State$STATEFP<- sprintf( "%02d", State$STATEFP)
Shapes<- left_join( Shapes, State, by='STATEFP')

#### MI 108, 113
setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper new")
Plotdata <- Shapes %>% filter( ST=="MI" )

Plot <- ggplot(Plotdata) +
        geom_sf(fill = "#69b3a2", color = "black") +
        facet_grid( cols=vars(Style) )+
        theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )
Plot
ggsave( file="MI108-113.eps", plot=Plot, width=6, height = 3, units="in")

###############################################
### plot metric example
#############################################

Delta <- CDr$Reock - CDr$Polsby
I <- which( Delta == max(Delta, na.rm=TRUE))
CDr[I,]



Plotdata <- CDs%>% filter( SESSN=='118', ST=='PA', DISTRICT==12)
Bounding_circle <- st_minimum_bounding_circle(Plotdata$geometry[1] )
Centroid <- st_centroid(Plotdata$geometry[1] )
Perim <- st_perimeter(Plotdata$geometry[1] )
Radius <- Perim/2/pi
Perim_circle <- st_buffer( Centroid, dist=Radius)

Plot <- ggplot() +
  geom_sf( data = Plotdata, fill="lightblue") +
  geom_sf( data = Bounding_circle, fill=NA, color='blue', size=1) +
  geom_sf( data = Perim_circle, fill=NA, color='red', size=1) +
  #geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = r), data = circles)
  #scale_fill_manual(values = c("1" = "red", "0" = "lightblue")) +
    labs( title="118th session PA  district 12")+
  theme( legend.position = 'none')+
  theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )
Plot

setwd("C:/Users/Owner/OneDrive/Redistricting/Compactness paper revision/Latex revision")
ggsave( file="118PA12.eps", plot=Plot, width=6, height=3, units="in")

####################################################################
#### Plot 10 worst
#######################################################################
setwd("C:/Users/Owner/Compactness/Papercode/Worst plots")

CDplot <- CDs
#CDplot <- CDs%>% filter( SESSN=='118')


PPWorst_indx <- order( CDplot$PP_Pdistrict)
RKWorst_indx <- order( CDplot$Rk_Pdistrict)


for( i in PPWorst_indx[1:10]){
  Sn <- CDplot$SESSN[i]
  St <- CDplot$ST[i]
  Ds <- CDplot$DISTRICT[i]
  Score <- sprintf("%.4f", CDplot$PP_Pdistrict[i])
  Title <- paste( Sn, St, Ds )  
  Subtitle <- paste( " Rel. Polsby Popper=", Score) 
  print( Title )
  Pfile <- paste0( Sn, St,Ds,"PP.svg")
  Plotdata <- CDplot%>% filter( SESSN==Sn, ST==St)
  Plotdata <- Plotdata %>% mutate( Outlier = if_else( DISTRICT==Ds,1,0))
  Plot <- Plot_neighbors(Plotdata, Title, Subtitle)
  ggsave( file=Pfile, plot=Plot, width=6, height=3, units="in")
  #ggsave( file=Pfile, plot=Plot)
  }

for( i in RKWorst_indx[1:10]){
  Sn <- CDplot$SESSN[i]
  St <- CDplot$ST[i]
  Ds <- CDplot$DISTRICT[i]
  Score <- sprintf("%.4f", CDplot$Rk_Pdistrict[i])
  Title <- paste( Sn, St, Ds) 
  Subtitle <- paste("Rel. Reock=", Score) 
  print( Title)
  Pfile <- paste0( Sn, St,Ds,"RK.svg")
  Plotdata <- CDplot%>% filter( SESSN==Sn, ST==St)
  Plotdata <- Plotdata %>% mutate( Outlier = if_else( DISTRICT==Ds,1,0))
  Plot <- Plot_neighbors(Plotdata, Title, Subtitle)
  ggsave( file=Pfile, plot=Plot, width=6, height=3, units="in") 
  #ggsave( file=Pfile, plot=Plot)
  }

##########################################
### Plot worst by state
####################################
Sn <- '118'
CDplot <- CDs%>% filter( SESSN==Sn)

PPWorst_indx <- order( CDplot$PP_Pdistrict)
Threshold <- max( CDplot$PP_Pdistrict[PPWorst_indx], na.rm=TRUE)
CDplot <- CDplot%>%mutate(Outlier=if_else(PP_Pdistrict <= Threshold, 1,0 ) )
.Sts <- unique( CDplot$ST[PPWorst_indx[1:10]])

for( S in Sts[1]){
  St <- S
  Title <- paste("Worst Polsby Popper", Sn, St )  
  print( Title)
  Pfile <- paste0( Sn, St,Ds,"PP.svg")
  Plotdata <- CDplot%>% filter( SESSN==Sn, ST==St)
  Plot <- ggplot(Plotdata) +
    geom_sf(aes(fill = as.factor(Outlier) )) +
    scale_fill_manual(values = c("1" = "red", "0" = "lightblue")) +
    #labs( title="North Carolina congressional map for session 113")+
    labs( title=Title)+
    theme( legend.position = 'none')+
    theme(plot.margin =  unit(c(0.1,0.1,0.1,0.1),"cm") )
  Plot
  #ggsave( file=Pfile, plot=Plot, width=6, height=3, units="in")
  
}


##################################################################
### Outlier and neighbors
#####################################################################
# ============================================================
#  Plot an outlier congressional district and its neighbors
#  Inputs:  an sf dataframe with columns:
#             state    – state abbreviation / name
#             district – district identifier
#             geometry – sf geometry (polygons)
#             outlier  – 1 for the flagged district, 0 otherwise
# ============================================================

library(sf)
library(ggplot2)
library(dplyr)

# ------------------------------------------------------------
# 0.  EXAMPLE DATA  (comment out / replace with your own sf object)
# ------------------------------------------------------------
# Uncomment the three lines below to download TX congressional
# districts on the fly for a self-contained demo.
#
 install.packages("tigris")          # only needed once
 library(tigris)
 options(tigris_use_cache = TRUE)
 districts_sf <- congressional_districts(state = "TX", cb = TRUE, year = 2022) |>
  st_transform(4326) |>
   rename(district = CD118FP) |>
   mutate(state   = "TX",
          outlier = if_else(district == "35", 1L, 0L))  # TX-07 as demo outlier

# If you already have your sf object loaded, just make sure it is
# named  districts_sf  and has the four columns above, then run
# everything from section 1 onward.

###########################################################################
### Plot function
 ##########################################################################
 Plot_neighbors <- function( districts_sf, Title, Subtitle){  
   
   #Title <- paste( districts_sf$SESSN, districts_sf$ST, districts_sf$DISTRICT[districts_sf$Outlier==1]  )
# ------------------------------------------------------------
# 1.  IDENTIFY OUTLIER AND TOUCHING NEIGHBOURS
# ------------------------------------------------------------

# The single outlier district
outlier_district <- districts_sf |>
  filter(Outlier == 1)
 

# All districts that share a border (or touch) the outlier.
# st_touches() returns a sparse list of indices.
touch_idx <- st_touches(outlier_district, districts_sf)[[1]]

neighbor_districts <- districts_sf |>
  slice(touch_idx)

# Combine outlier + neighbours into one plotting layer
focal_districts <- bind_rows(
  outlier_district  |> mutate(role = "Outlier"),
  neighbor_districts |> mutate(role = "Neighbor")
)

# ------------------------------------------------------------
# 2.  BOUNDING BOX  (with a small padding buffer)
# ------------------------------------------------------------

PAD <- 0.25

bbox <- st_bbox(focal_districts)
xlim <- c(bbox["xmin"] - PAD, bbox["xmax"] + PAD)
ylim <- c(bbox["ymin"] - PAD, bbox["ymax"] + PAD)

# FIX: modify the bbox object directly, then convert to sfc
padded_bbox        <- bbox
padded_bbox["xmin"] <- xlim[1]
padded_bbox["xmax"] <- xlim[2]
padded_bbox["ymin"] <- ylim[1]
padded_bbox["ymax"] <- ylim[2]

bbox_poly <- st_as_sfc(padded_bbox)

background_districts <- districts_sf |>
  st_intersection(bbox_poly)
# ------------------------------------------------------------
# 3.  DISTRICT LABEL POSITIONS  (centroid of each focal district)
# ------------------------------------------------------------

label_pts <- focal_districts |>
  mutate(
    centroid = st_centroid(geometry),
    lon      = st_coordinates(centroid)[, 1],
    lat      = st_coordinates(centroid)[, 2]
  ) |>
  st_drop_geometry()

# ------------------------------------------------------------
# 4.  PLOT
# ------------------------------------------------------------

p <- ggplot() +
  
  # -- faint grey background for all districts in the window
  geom_sf(
    data  = background_districts,
    fill  = "grey92",
    color = "white",
    linewidth = 0.3
  ) +
  
  # -- neighbors: light blue fill, visible border
  geom_sf(
    data      = focal_districts |> filter(role == "Neighbor"),
    fill      = "#90CAF9",   # Material Blue 200
    color     = "#1565C0",
    linewidth = 0.6,
    alpha     = 0.7
  ) +
  
  # -- outlier: bold red/orange fill
  geom_sf(
    data      = focal_districts |> filter(role == "Outlier"),
    fill      = "#EF5350",   # Material Red 400
    color     = "#B71C1C",
    linewidth = 0.9,
    alpha     = 0.85
  ) +
  
  # -- district number labels
  geom_text(
    data    = label_pts,
    aes(x = lon, y = lat, label = DISTRICT,
        fontface = if_else(role == "Outlier", "bold", "plain")),
    size    = 3.2,
    color   = "black"
  ) +
  
  # -- zoom to focal area
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  
  # -- colour legend entries (manual, since fill comes from two layers)
  annotate("rect", xmin = -Inf, xmax = -Inf,
           ymin = -Inf, ymax = -Inf,
           fill = NA, color = NA) +   # placeholder so scale renders
  
  scale_fill_identity() +
  
  # -- labels & theme
  labs(
    title    = Title,
    subtitle = Subtitle,
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 12),
    plot.subtitle   = element_text(size = 10, color = "grey40"),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text       = element_text(size = 8),
    plot.margin     = margin(1, 1, 1, 1)
  )

return(p)

}

# ------------------------------------------------------------
# 5.  SAVE  (PNG + PDF for maximum compatibility)
# ------------------------------------------------------------

OUT_DIR <- "."   # change to your preferred output folder

ggsave(
  filename = file.path(OUT_DIR, "outlier_district_map.png"),
  plot     = p,
  width    = 8,
  height   = 7,
  dpi      = 300,
  bg       = "white"
)

ggsave(
  filename = file.path(OUT_DIR, "outlier_district_map.pdf"),
  plot     = p,
  width    = 8,
  height   = 7
)

message("Saved:  outlier_district_map.png  and  outlier_district_map.pdf")
