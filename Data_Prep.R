##setup and libraries
library(readr)
library(tidyverse)
library(sp)
library(adehabitatLT)
library(gganimate)
library(gifski)
library(png)

## import tracks and select species of interest 

all_gps <- read_csv("Data/all_gps.csv", col_types = cols(DATE.TIME = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                                         END.TIME = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                                         RING = col_character(), START_TIME = col_datetime(format = "%Y-%m-%d %H:%M:%S"), X1 = col_skip()))

# removes pre 2002 tracks, selects only albatross, fixes the "season" so that it isn't just year

alb_GPS <- all_gps %>%
  mutate(YEAR=substr(DATE.TIME, 1,4),
         MONTH=substr(DATE.TIME,6,7)) %>%
  filter(SPECIES %in% c("BBA", "GHA", "LMSA"), YEAR >2002) %>%
  mutate(SEASON = ifelse(MONTH=="01", as.numeric(YEAR)-1, YEAR))

# quick plot

world <- maps::map("world", ".", exact = FALSE, plot = FALSE, fill = TRUE) %>% fortify()

ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_path() + 
  coord_map("azequalarea", orientation=c(-55, -45, 0),xlim=c(5, -80), ylim=c(-35,-70)) +
  geom_point(data=alb_GPS, aes(LON, LAT, group=SPECIES, color=SPECIES), size=0.5) + 
  theme_bw() +
  theme(legend.position="bottom") 


## interpolates tracks to 5 minute intervals
tointerp <- alb_GPS
tointerp <- tointerp %>%
  mutate(Species_track_stage_season_ring = paste(TRACKID, SPECIES,STAGE, SEASON, RING, sep="-"))

id <- tointerp[,13]

tointerp <- tointerp[-which(duplicated(paste(tointerp$DATE.TIME, id))),]

loc <- coordinates(as.matrix(tointerp[,c(6,7)], nrow=length(tointerp)))

alb_GPSltraj <- as.ltraj(xy=loc, 
                       id=tointerp$Species_track_stage_season_ring, date=tointerp$DATE.TIME,
                       typeII=T)

interp <- redisltraj(l=alb_GPSltraj, u=300, type="time")

# converts back to DF
df_interp <- ld(interp)

# selects columns required and merges back
df_interpm <- df_interp %>%
  dplyr::select(long=x,lat=y,DATETIME=date, Species_track_stage_season_ring=id)

# separates out extra information again
alb_gps_interp <- df_interpm %>%
  separate(col=Species_track_stage_season_ring, into = c("TRACKID", "SPECIES", "STAGE", "SEASON", "RING"), sep="-")

# quick plot
ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_path() + 
  coord_map("azequalarea", orientation=c(-55, -45, 0),xlim=c(5, -80), ylim=c(-35,-70)) +
  geom_point(data=alb_gps_interp, aes(long, lat, group=SPECIES, color=SPECIES), size=0.5) + 
  theme_bw() +
  facet_wrap(~SEASON+STAGE) +
  theme(legend.position="bottom") 

# save file/export

# write.csv(alb_gps_interp, file=paste("alb_gps_interp_", Sys.Date(), ".csv", sep=""), row.names = F)

#alb_gps_interp <- read_csv("alb_gps_interp_2019-05-29.csv")

## import deployment data for BBA and GHA

BBA_GHA_deployments <- read_csv("Data/BBA_GHA_deployments.csv", 
                                col_types = cols(Deployed = col_datetime(format = "%d-%b-%Y %H:%M"), 
                                                 Retrieved = col_datetime(format = "%d-%b-%Y %H:%M"), 
                                                 Return1 = col_datetime(format = "%d-%b-%Y %H:%M"), 
                                                 Ring = col_character(), 
                                                 Start1 = col_datetime(format = "%d-%b-%Y %H:%M")))

# only choose birds who have TDRs
BBA_GHA_deployments <- BBA_GHA_deployments %>%
  filter(is.na(TDR_no)==F)

# only take tracks from birds with TDRs
alb_gps_interp_TDR <- alb_gps_interp %>%
  filter(RING %in% BBA_GHA_deployments$Ring) %>%
  filter(SEASON == 2009) # known that all the GHA/BBA dives are in 2010 (so, 2009 season)

# quick plots
# all birds
ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_path() + 
  coord_map("azequalarea", orientation=c(-55, -45, 0),xlim=c(-34, -48), ylim=c(-44,-57)) +
  geom_point(data=alb_gps_interp_TDR, 
             aes(long, lat, group=SPECIES, color=TRACKID), size=0.5) + 
  theme_bw() +
  facet_wrap(~SPECIES) +
  ggtitle("2009, BBA/GHA, Brood Guard") +
  theme(legend.position = "none")

# just black brows by track
ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_path() + 
  coord_map("azequalarea", orientation=c(-55, -45, 0),xlim=c(-30, -50), ylim=c(-42,-58)) +
  geom_point(data=subset(alb_gps_interp_TDR, SPECIES=="BBA"), 
             aes(long, lat, group=SPECIES), size=0.5) + 
  theme_bw() +
  facet_wrap(~TRACKID) +
  theme(legend.position="bottom") +
  ggtitle("2009, BBA, Brood Guard")

# just grey heads by track
ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_path() + 
  coord_map("azequalarea", orientation=c(-55, -45, 0),xlim=c(-30, -50), ylim=c(-42,-58)) +
  geom_point(data=subset(alb_gps_interp_TDR, SPECIES=="GHA"), 
             aes(long, lat, group=SPECIES), size=0.5) + 
  theme_bw() +
  facet_wrap(~TRACKID) +
  theme(legend.position="bottom") +
  ggtitle("2009, GHA, Brood Guard")

## Import DIVE DATA

BBA_GHA_Dive_Events <- read_csv("Data/BBA_GHA_Dive_Events.csv", 
                                col_types = cols(StartT = col_datetime(format = "%m/%d/%y %H:%M:%S"), 
                                                 deepestT = col_datetime(format = "%m/%d/%y %H:%M:%S"), 
                                                 endT = col_datetime(format = "%m/%d/%y %H:%M:%S")))

BBA_GHA_Dive_Events <- BBA_GHA_Dive_Events %>%
  separate(ID, c("TDR_no", "dateID", "SPECIES", "Ring"), sep="_") %>%
  filter(!doubt>1)
  

## REINTERPOLATE TRACKS TO 1 SECOND
alb_GPS_TDR <- alb_GPS %>%
  filter(TRACKID %in% alb_gps_interp_TDR$TRACKID) %>%
  filter(RING %in% BBA_GHA_Dive_Events$Ring)


tointerp <- alb_GPS_TDR
tointerp <- tointerp %>%
  mutate(Species_track_stage_season_ring = paste(TRACKID, SPECIES,STAGE, SEASON, RING, sep="-"))

id <- tointerp[,13]

tointerp <- tointerp[-which(duplicated(paste(tointerp$DATE.TIME, id))),]

loc <- coordinates(as.matrix(tointerp[,c(6,7)], nrow=length(tointerp)))

alb_gps_1sec_TDRltraj <- as.ltraj(xy=loc, 
                         id=tointerp$Species_track_stage_season_ring, date=tointerp$DATE.TIME,
                         typeII=T)

interp <- redisltraj(l=alb_gps_1sec_TDRltraj, u=1, type="time")

# converts back to DF
df_interp <- ld(interp)

# selects columns required and merges back
df_interpm <- df_interp %>%
  dplyr::select(long=x,lat=y,DATETIME=date, Species_track_stage_season_ring=id)

# selects test tracks - separates out extra information again
alb_gps_1sec_TDR <- df_interpm %>%
  separate(col=Species_track_stage_season_ring, into = c("TRACKID", "SPECIES", "STAGE", "SEASON", "RING"), sep="-")


#### can you write a function to "check" timepoints? 

time_pointx <- "2010-01-08 23:44:14"
ringnumberx <-"1320592"

is_diving <- function(time_point1, ringnumber1) {
  subDEf <- subset(BBA_GHA_Dive_Events, Ring==as.character(ringnumber1))
  subDEf$timepoint <- as.POSIXct(time_point1)
  subDEf <- as.data.frame(subDEf)
  subDE2f <- subDEf %>%
    dplyr::mutate(DIVE = case_when(as.POSIXct(timepoint)>= as.POSIXct(StartT) & as.POSIXct(timepoint)<= as.POSIXct(endT) ~ TRUE,
                                   as.POSIXct(timepoint)< as.POSIXct(StartT) ~ FALSE, 
                                   as.POSIXct(timepoint) > as.POSIXct(endT) ~ FALSE))
  return(any(subDE2f$DIVE))
}

is_diving(time_point1=time_pointx, ringnumber1 = ringnumberx)

is_diving(alb_gps_1sec_TDR$DATETIME[1], alb_gps_1sec_TDR$RING[1])

checknums <- seq(1,1700000, by=10000)

output <- list()
alb_gps_1sec_GHA <- subset(alb_gps_1sec_TDR, SPECIES=="GHA")

for (i in 950000:nrow(alb_gps_1sec_GHA)){
  output[i] <- is_diving(alb_gps_1sec_GHA$DATETIME[i], alb_gps_1sec_GHA$RING[i])
  if(i %in% checknums) {print(i)}
}

GHAdives<- unlist(output)
table(GHAdives)

GHA_gps_1sec_dive <- alb_gps_1sec_GHA 
GHA_gps_1sec_dive$dive <- as.vector(GHAdives)
#write.csv(GHA_gps_1sec_dive, file="GHA_gps_1sec_dive.csv", row.names = F)

## IMPORT SO YOU DON'T HAVE TO RE RUN THE ABOVE CODE
GHA_gps_1sec_dive <- read_csv("GHA_gps_1sec_dive.csv", 
                                      col_types = cols(DATETIME = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

#write.csv(alb_gps_1sec_TDR2, file="alb_gps_1sec_TDR2.csv", row.names = F)

alb_gps_1sec_TDR2 <- read_csv("alb_gps_1sec_TDR2.csv", 
                                      col_types = cols(DATETIME = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

dive_2sec <- alb_gps_1sec_TDR2 %>% filter(dive==TRUE)

GHA_gps_1sec_dive_true <- GHA_gps_1sec_dive %>% filter(dive==TRUE)

############

##### okay so the black brows have the date wrong half the time
BBA_Dive_Events <- BBA_GHA_Dive_Events %>%
  filter(SPECIES=="BBA") %>%
  separate(StartT,c("SYear", "SMonth", "SDay"), sep="-") %>%
  separate(SDay, c("SDay", "STime"), sep=" ") %>%
  separate(endT,c("EYear", "EMonth", "EDay"), sep="-") %>%
  separate(EDay, c("EDay", "ETime"), sep=" ") %>%
  mutate(S_Date_Fixed = case_when(
    SMonth == "01" ~ paste(SYear, SMonth, SDay, sep="-"),
    SMonth != "01" ~ paste(SYear, SDay, SMonth,sep="-")),
    E_Date_Fixed = case_when(
    EMonth == "01" ~ paste(SYear, SMonth, SDay, sep="-"),
    EMonth != "01" ~ paste(SYear, SDay, SMonth,sep="-"))) %>%
  mutate(S_DateTime = paste(S_Date_Fixed, STime, sep=" "),
         E_DateTime = paste(E_Date_Fixed, ETime, sep=" ")) 
  
#### Function JUST FOR BBA

is_divingBBA <- function(time_point1, ringnumber1) {
  subDEB <- subset(BBA_Dive_Events, Ring==as.character(ringnumber1))
  subDEB$timepoint <- as.POSIXct(time_point1)
  subDEB <- as.data.frame(subDEB)
  subDE2B <- subDEB %>%
    dplyr::mutate(DIVE = case_when(as.POSIXct(timepoint)>= as.POSIXct(S_DateTime) & as.POSIXct(timepoint)<= as.POSIXct(E_DateTime) ~ TRUE,
                                   as.POSIXct(timepoint) < as.POSIXct(S_DateTime) ~ FALSE, 
                                   as.POSIXct(timepoint) > as.POSIXct(E_DateTime) ~ FALSE))
  return(any(subDE2B$DIVE))
}

alb_gps_1sec_TDR_BBA <- alb_gps_1sec_TDR %>% filter(SPECIES=="BBA")

outputBBA <- list()  
checknums <- seq(1,1700000, by=10000)

for (i in 7750:nrow(alb_gps_1sec_TDR_BBA)){
  outputBBA[i] <- is_divingBBA(alb_gps_1sec_TDR_BBA$DATETIME[i], alb_gps_1sec_TDR_BBA$RING[i])
  if(i %in% checknums) {print(i)}
}

vecBBA<- unlist(outputBBA)
table(vecBBA)

alb_gps_1sec_TDR_BBA_dive <- alb_gps_1sec_TDR_BBA
alb_gps_1sec_TDR_BBA_dive$dive <- as.vector(vecBBA)

# write.csv(alb_gps_1sec_TDR_BBA_dive, file="alb_gps_1sec_TDR_BBA_diveJUN5.csv", row.names = F)

alb_gps_1sec_TDR_BBA_dive <- read_csv("alb_gps_1sec_TDR_BBA_diveJUN5.csv", 
                                      col_types = cols(DATETIME = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

alb_gps_1sec_TDR_BBA_dive <- alb_gps_1sec_TDR_BBA_dive %>% filter(dive == T)

## plot now

ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_path() + 
  coord_map("azequalarea", orientation=c(-55, -45, 0),xlim=c(-34, -48), ylim=c(-44,-57)) +
  geom_point(data=alb_gps_interp_TDR, 
             aes(long, lat, group=SPECIES), size=0.5, alpha=0.2, color="grey") + 
  geom_point(data=GHA_gps_1sec_dive_true, 
             aes(long, lat, group=SPECIES, color=TRACKID), size=0.75) + 
  geom_point(data=alb_gps_1sec_TDR_BBA_dive, 
             aes(long, lat, group=SPECIES, color=TRACKID), size=0.75) +
  theme_bw() +
  facet_wrap(~SPECIES) +
  ggtitle("2009, BBA/GHA, Brood Guard") +
  theme(legend.position = "none")

########### what's up with that weird BBA
ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_path() + 
  coord_map("azequalarea", orientation=c(-55, -45, 0),xlim=c(-34, -42), ylim=c(-46,-56)) +
  geom_point(data=subset(alb_gps_interp_TDR, str_detect(TRACKID, "IGOT0011")), 
             aes(long, lat, group=SPECIES), size=0.5, alpha=0.2, color="grey") + 
  geom_point(data=subset(alb_gps_1sec_TDR_BBA_dive, str_detect(TRACKID, "IGOT0011")), 
             aes(long, lat, group=SPECIES, color=TRACKID), size=0.75) +
  theme_bw() +
  ggtitle("2009, BBA, Brood Guard") 

# okay so the weird one is this trackID: IGOT0011_20100106171100, siri zoom in
ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_path() + 
  coord_map("azequalarea", orientation=c(-55, -45, 0),xlim=c(-38, -40), ylim=c(-46.5,-49)) +
  geom_point(data=subset(alb_gps_interp_TDR, str_detect(TRACKID, "IGOT0011")), 
             aes(long, lat, group=SPECIES), size=0.75, alpha=0.5, color="grey") + 
  geom_point(data=subset(alb_gps_1sec_TDR_BBA_dive, str_detect(TRACKID, "IGOT0011")), 
             aes(long, lat, group=SPECIES, color=TRACKID), size=0.75) +
  theme_bw() +
  ggtitle("2009, BBA, Brood Guard") 

## shit goes weird after this point: 46.60672 and time 2010-01-07 17:11:56
test_bird2 <- subset(alb_gps_interp_TDR, str_detect(TRACKID, "IGOT0011")) 
test_bird <- test_bird2 %>%
  filter(DATETIME>"2010-01-07 17:00:00") %>%
  filter(DATETIME<"2010-01-09 11:00:00")

test_bird2 <- subset(alb_gps_interp_TDR, str_detect(TRACKID, "IGOT0011")) 
test_bird_context <- test_bird2 %>%
  filter(DATETIME>"2010-01-07 12:00:00") %>%
  filter(DATETIME<"2010-01-09 12:00:00")

## plot with the lead in and out  
ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_path() + 
  coord_map("azequalarea", orientation=c(-55, -45, 0),xlim=c(-38, -40), ylim=c(-46.5,-48.5)) +
  geom_point(data=test_bird_context, 
             aes(long, lat, group=SPECIES), size=0.75, alpha=0.5, color="grey") + 
  geom_point(data=subset(alb_gps_1sec_TDR_BBA_dive, str_detect(TRACKID, "IGOT0011")), 
             aes(long, lat, group=SPECIES, color=TRACKID), size=0.75) +
  theme_bw()  +
  ggtitle("Weird Bird Why: BBA, Brood Guard, IGOT0011_20100106171100")  +
  theme(legend.position = "none")
  
  # this bird just hangs out without going far for like ... a long time
difftime("2010-01-07 17:00:00","2010-01-09 11:00:00", units="hours")
# THIS BIRD HANGS OUT FOR 42 HOURS on the water????? what are you doing bird... 


## are all the tracks leaving gthe island right away? zoomy zoom, yep they are.
ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_path() + 
  coord_map("azequalarea", orientation=c(-55, -45, 0),xlim=c(-37, -38.5), ylim=c(-53.5,-54.5)) +
  geom_point(data=alb_gps_interp_TDR, 
             aes(long, lat, group=SPECIES), size=0.5, alpha=0.2, color="grey") + 
  theme_bw() +
  facet_wrap(~SPECIES) +
  ggtitle("2009, BBA/GHA, Brood Guard") +
  theme(legend.position = "none")

max(test_bird2$DATETIME) - min(test_bird2$DATETIME)

##### cheeky test of animation
dive_2sec <- alb_gps_1sec_TDR2 %>% filter(dive==TRUE)

ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_path() + 
  coord_map("azequalarea", orientation=c(-55, -45, 0),xlim=c(-34, -48), ylim=c(-44,-57)) +
  geom_point(data=alb_gps_interp_TDR, 
             aes(long, lat, group=TRACKID, color=TRACKID), size=0.5, alpha=0.2) + 
  geom_point(data=subset(dive_2sec, SPECIES=="GHA"), 
             aes(long, lat, group=TRACKID, color=TRACKID), size=2) + 
  geom_point(data=alb_gps_1sec_TDR_BBA_dive, 
             aes(long, lat, group=TRACKID, color=TRACKID), size=2) +
  theme_bw() +
  facet_wrap(~SPECIES) +
  ggtitle("2009, {frame_time}, BBA/GHA, Brood Guard") +
  theme(legend.position = "none") +
  transition_time(DATETIME) + shadow_trail(distance=1, max_frames = Inf, exclude_layer = NULL)


## why are those dots not moving
ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_path() + 
  coord_map("azequalarea", orientation=c(-55, -45, 0),xlim=c(-34, -48), ylim=c(-44,-57)) +
  geom_point(data=alb_gps_interp_TDR, 
             aes(long, lat, group=TRACKID, color=TRACKID), size=0.5, alpha=0.2) + 
  geom_point(data=subset(dive_2sec, SPECIES=="GHA"), 
             aes(long, lat, group=TRACKID, color=TRACKID), size=2) + 
  geom_point(data=alb_gps_1sec_TDR_BBA_dive, 
             aes(long, lat, group=TRACKID, color=TRACKID), size=2) +
  theme_bw() +
  facet_wrap(~SPECIES) +
  ggtitle("2009, BBA/GHA, Brood Guard") +
  theme(legend.position = "none") 


