library(tidyverse)


##### Import file names, create empty dfs, import and combine all immersion data from the two logger types

filenamesNEW <- list.files("Data/Immersion/with_TDR_2/Corrected_Files")

imm_dataNEW <- data.frame(check = as.character(),
                       datetime = as.character(),
                       XX = as.double(),
                       secs = as.double(),
                       wet_dry = as.character(),
                       GLS_id = as.character(),
                       SPECIES = as.character(),
                       Ring = as.character())

for (i in 1:length(filenamesNEW)){
  GLS_id <- strsplit(filenamesNEW[i], "_")[[1]][1]
  SPECIES <- strsplit(filenamesNEW[i], "_")[[1]][2]
  Ring <- strsplit(filenamesNEW[i], "_")[[1]][3]
  
  input <- read_csv(paste("Data/Immersion/with_TDR_2/Corrected_Files/", filenamesNEW[i], sep=""), 
                    col_names = c("check", "datetime", "XX", "secs", "wet_dry"))
  
  input$GLS_id <- GLS_id
  input$SPECIES <- SPECIES
  input$Ring <-Ring
  
  imm_dataNEW <- rbind(imm_dataNEW, input)
}

imm_dataNEW <- imm_dataNEW %>% mutate(datetime=as.POSIXct(datetime, format="%d/%m/%y %H:%M:%S"))


## LMA files
filenamesLMA <- list.files("Data/Immersion/with_TDR_2/LMA")


imm_dataLMA <- data.frame(check = as.character(),
                          datetime = as.character(),
                          XX = as.double(),
                          secs = as.double(),
                          wet_dry = as.character(),
                          GLS_id = as.character(),
                          SPECIES = as.character(),
                          Ring = as.character())


for (i in 1:length(filenamesLMA)){
  GLS_id <- strsplit(filenamesLMA[i], "_")[[1]][1]
  SPECIES <- strsplit(filenamesLMA[i], "_")[[1]][2]
  Ring <- strsplit(filenamesLMA[i], "_")[[1]][3]
  
  input <- read_csv(paste("Data/Immersion/with_TDR_2/LMA/", filenamesLMA[i], sep=""), 
                    col_names = c("check", "datetime", "XX", "secs", "wet_dry"))
  
  input$GLS_id <- GLS_id
  input$SPECIES <- SPECIES
  input$Ring <-Ring
  
  imm_dataLMA <- rbind(imm_dataLMA, input)
}

imm_dataLMA$datetime <- as.POSIXct(imm_dataLMA$datetime, 
                                   format = "%d/%m/%y %H:%M:%S")


## bring in matches to metadata
bba_gha_matching <- read_csv("Data/Immersion/bba_gha_matching.csv", 
                             col_types = cols(GLS_id = col_character(), Notes = col_skip(),
                                              Ring = col_character(), trip_end = col_datetime(format = "%d/%m/%Y %H:%M"), 
                                              trip_start = col_datetime(format = "%d/%m/%Y %H:%M")))




bba_gha_matching <- bba_gha_matching %>%
  unite(SPP_Ring_GLS,c(SPECIES, Ring, GLS_id), sep="_",remove=F)

# remove anything suspect (nothing here)
imm_dataNEW <- imm_dataNEW %>%
  filter(check =="ok") %>%
  unite(SPP_Ring_GLS,c(SPECIES, Ring, GLS_id), sep="_",remove=F)



## including LMSA
Master_Birds_test <- read_csv("Data/Immersion/Master_Birds_test.csv", 
                              col_types = cols(END_DATE = col_datetime(format = "%d/%m/%Y %H:%M"), 
                                               NOTES = col_skip(), START_DATE = col_datetime(format = "%d/%m/%Y %H:%M"), 
                                              X15 = col_skip()))

Master_Birds_test <- Master_Birds_test %>%
  unite(SPP_Ring_GLS,c(SPECIES, RING, GLS_ID), sep="_",remove=F) %>%
  rename(trip_start=START_DATE, trip_end = END_DATE, all_detects=ALL_DETECTS, n_dives=N_DIVES)


## dives per day
Master_Birds_test %>%
  filter(!is.na(trip_start)) %>%
  mutate(trip_hours = trip_end-trip_start) %>% 
  mutate(trip_days = as.numeric(trip_hours)/24) %>%
  mutate(divesperday = n_dives/as.numeric(trip_days)) %>% 
  mutate(nondiver = case_when(n_dives == 0 ~1, 
                           n_dives > 0 ~ 0)) %>%
  group_by(SPECIES) %>%
  summarise(mediandivesperday = median(divesperday, na.rm=T),
            meandivesperday = mean(divesperday, na.rm=T),
            sddivesperday = sd(divesperday, na.rm=T),
            divestotal = sum(n_dives),
            nondivers = sum(nondiver),
            birdstotal= length(unique(ID)))
  



imm_dataLMA <- imm_dataLMA %>%
  filter(check =="ok"|check=="SUSPECT") %>%
  unite(SPP_Ring_GLS,c(SPECIES, Ring, GLS_id), sep="_",remove=F)


# attach together and calculate metrics

imm_prop_wetNEW <- imm_dataNEW %>%
  left_join(bba_gha_matching, by = "SPP_Ring_GLS") %>% 
  dplyr::select(check, datetime, secs, wet_dry, XX, SPP_Ring_GLS, TDR_ID, TRACKID, trip_start, trip_end, all_detects, n_dives, SEASON, STAGE) %>%
  mutate(datetime = as.POSIXct(datetime, format="%d/%m/%y %H:%M:%S")) %>%
  filter((datetime > trip_start) & (datetime < trip_end)) %>%
  mutate(triplength = trip_end - trip_start) %>%
  mutate(triplength = triplength/24) %>%
  mutate(tripseconds = triplength*24*60*60) %>% 
  group_by(SPP_Ring_GLS) %>%
  filter(wet_dry=="wet") %>% 
  summarise(total_imm=sum(secs), triplength=unique(triplength),tripseconds=unique(tripseconds)) %>%
  mutate(total_imm_days = total_imm/60/60/24) %>% 
  mutate(proportion_trip_wet = total_imm_days/triplength)



imm_prop_wetLMA <- imm_dataLMA %>%
  left_join(Master_Birds_test, by = "SPP_Ring_GLS") %>% 
  dplyr::select(check, datetime, secs, wet_dry, XX, SPP_Ring_GLS, TRACKID, trip_start, trip_end, all_detects, n_dives, SEASON, STAGE) %>%
  mutate(datetime = as.POSIXct(datetime, format="%d/%m/%y %H:%M:%S")) %>%
  filter((datetime > trip_start) & (datetime < trip_end)) %>%
  mutate(triplength = trip_end - trip_start) %>%
  mutate(tripseconds = triplength*24*60*60) %>% 
  group_by(SPP_Ring_GLS) %>%
  filter(wet_dry=="wet") %>% 
  summarise(total_imm=sum(secs), triplength=unique(triplength),tripseconds=unique(tripseconds)) %>%
  mutate(total_imm_days = total_imm/60/60/24) %>% 
  mutate(proportion_trip_wet = total_imm_days/triplength)



## bring in dive data and compute seconds under water 

BBA_GHA_Dive_Events <- read_csv("/Users/lbentley/Dropbox/PhD/Chapters/4. Dive-Depth/Diving_Albatross/Data/BBA_GHA_Dive_Events.csv", 
                                col_types = cols(StartT = col_datetime(format = "%m/%d/%y %H:%M:%S"), 
                                                 deepestT = col_datetime(format = "%m/%d/%y %H:%M:%S"), 
                                                 endT = col_datetime(format = "%m/%d/%y %H:%M:%S")))

BBA_GHA_Dive_Events <- BBA_GHA_Dive_Events %>%
  separate(ID, c("TDR_no", "dateID", "SPECIES", "Ring"), sep="_")


##### okay so the black brows have the date wrong half the time
BBA_GHA_Dive_Events2 <- BBA_GHA_Dive_Events %>%
  #filter(SPECIES=="BBA") %>%
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
         E_DateTime = paste(E_Date_Fixed, ETime, sep=" ")) %>%
  mutate(Ring=ifelse(Ring=="12543054", "1254305", Ring))


dive_seconds <- BBA_GHA_Dive_Events2 %>%
  unite(SPP_Ring, c(SPECIES, Ring)) %>% 
  group_by(SPP_Ring) %>% 
  filter(doubt==0) %>%
  summarise(diveseconds=sum(dur), ndive=n())


### bring in LMA dive data 
LMSA_Dives <- read_csv("Data/LMSA/LMSA_Dives.csv", 
                       col_types = cols(StartT = col_datetime(format = "%m/%d/%y %H:%M:%S"), 
                                        endT = col_datetime(format = "%m/%d/%y %H:%M:%S")))

# addtional pieces are just the date, ignore
dive_secondsLMA <- LMSA_Dives %>%
  separate(ID, into=c("TDR", "SPECIES", "Ring"), sep="_") %>%
  unite(SPP_Ring, c(SPECIES, Ring)) %>% 
  group_by(SPP_Ring) %>% 
  filter(doubt==0) %>%
  summarise(diveseconds=sum(dur), ndive=n())

## combine dives with immersion for BBA/GHA and for LMA

immersion_secondsNEW <- imm_prop_wetNEW

immersion_secondsNEW <- immersion_secondsNEW %>%
  separate(SPP_Ring_GLS, c("SPP", "Ring", "GLS"), sep = "_") %>%
  unite("SPP_Ring", SPP, Ring)


dive_prop_wet_prop_tripNEW <- immersion_secondsNEW %>%
  left_join(dive_seconds, by="SPP_Ring") %>%
  mutate(diveseconds=ifelse(is.na(diveseconds), 0, diveseconds)) %>%
  mutate(ndive=ifelse(is.na(ndive), 0, ndive)) %>%
  dplyr::select(SPP_Ring, ndive, diveseconds, total_imm, tripseconds) %>% 
  mutate(prop_wet_diving = diveseconds/total_imm, prop_trip_wet = total_imm/tripseconds) %>% 
  separate(SPP_Ring, into=c("SPECIES", "Ring"), remove=F) %>%
  mutate(imm_hour = total_imm/60/60, triphours = tripseconds/60/60, 
         tripdays = tripseconds/60/60/24, divehours = diveseconds/60/60)

# LMA

immersion_secondsLMA <- imm_prop_wetLMA %>%
  separate(SPP_Ring_GLS, c("SPP", "Ring", "GLS"), sep = "_") %>%
  unite("SPP_Ring", SPP, Ring)


dive_prop_wet_prop_tripLMA <- immersion_secondsLMA %>%
  left_join(dive_secondsLMA, by="SPP_Ring") %>%
  mutate(diveseconds=ifelse(is.na(diveseconds), 0, diveseconds)) %>%
  mutate(ndive=ifelse(is.na(ndive), 0, ndive)) %>%
  mutate(dive_days = diveseconds/60/60/24) %>%
  dplyr::select(SPP_Ring, ndive, diveseconds, total_imm_days, dive_days, total_imm, tripseconds) %>% 
  mutate(prop_wet_diving = diveseconds/total_imm, prop_trip_wet = total_imm/tripseconds) %>% 
  separate(SPP_Ring, into=c("SPECIES", "Ring"), remove=F) %>%
  mutate(imm_hour = total_imm/60/60, triphours = tripseconds/60/60, 
         tripdays = tripseconds/60/60/24, divehours = diveseconds/60/60)


dive_prop_wet_prop_tripLMA$Ring <- factor(dive_prop_wet_prop_tripLMA$Ring, 
                                       levels = dive_prop_wet_prop_tripLMA$Ring[order(dive_prop_wet_prop_tripLMA$tripdays)])

## as absolute hours 
## BBA/GHA
dive_prop_wet_prop_tripNEW %>%
  filter(Ring !="1148957") %>% 
  ggplot(aes(x=reorder(Ring, triphours))) + 
  geom_bar(aes(x=reorder(Ring, triphours), y=triphours),fill="light grey", stat="identity") +
  geom_bar(aes(x=reorder(Ring, triphours), y=imm_hour), fill="light blue", stat="identity") +
  geom_bar(aes(x=reorder(Ring, triphours), y=divehours), fill="black", stat="identity") +
  theme_bw(base_family="Arial", base_size=14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans='sqrt',name= "Hours", breaks=c(0, 1, 5, 10, 20, 40, 60, 80), 
                     labels=c("0", "1", "5", "10", "20", "40", "60", "80")) +
  scale_x_discrete(name="Bird ID") +
  facet_wrap(~SPECIES,nrow = 1,scales = "free_x")

# calculate percentages
dive_prop_wet_prop_tripNEW %>%
  filter(Ring !="1148957") %>% 
  mutate(proptripdiving=diveseconds/tripseconds) %>%
  group_by(SPECIES) %>%
  summarise(meanproptripdiving = mean(proptripdiving),
            meanproptripwet = mean(prop_trip_wet),
            sdproptripwet = sd(prop_trip_wet),
            meantriplength = mean(tripdays),
            sdtripdays = sd(tripdays),
            npercalc=n())


## LMA

dive_prop_wet_prop_tripLMA %>%
  mutate(SPECIES="LMA") %>%
  ggplot(aes(x=reorder(Ring, triphours))) + 
  geom_bar(aes(x=Ring, y=tripdays),fill="light grey", stat="identity") +
  geom_bar(aes(x=Ring, y=total_imm_days), fill="light blue", stat="identity") +
  geom_bar(aes(x=Ring, y=dive_days), fill="black", stat="identity") +
  theme_bw(base_family="Arial", base_size=14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans='sqrt',name= "Days", breaks=c(0, 5, 10, 15, 20), 
                     labels=c("0", "5", "10", "15", "20")) +
  scale_x_discrete(name="Bird ID") +
  facet_wrap(~SPECIES,nrow = 1,scales = "free_x")


dive_prop_wet_prop_tripLMA %>%
  group_by(SPECIES) %>%
  summarise(meanproptripwet = mean(prop_trip_wet),
            sdproptripwet = sd(prop_trip_wet),
            meantriplength = mean(tripdays),
            sdtripdays = sd(tripdays))

##### COMBINE IMMERSION WITH GPS AND PLOT

# BRING IN GPS
all_gps <- read_csv("Data/all_gps.csv", col_types = cols(DATE.TIME = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                                         END.TIME = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                                         RING = col_character(), START_TIME = col_datetime(format = "%Y-%m-%d %H:%M:%S"), X1 = col_skip()))

alb_GPS_immer <- all_gps %>%
  mutate(YEAR=substr(DATE.TIME, 1,4),
         MONTH=substr(DATE.TIME,6,7)) %>%
  mutate(SEASON = ifelse(MONTH=="01", as.numeric(YEAR)-1, YEAR)) %>% 
  filter(TRACKID %in% Master_Birds_test$TRACKID)


tointerp <- alb_GPS_immer
tointerp <- tointerp %>%
  mutate(Species_track_stage_season_ring = paste(TRACKID, SPECIES,STAGE, SEASON, RING, sep="-"))

id <- tointerp[,12]

tointerp <- tointerp[-which(duplicated(paste(tointerp$DATE.TIME, id))),]

loc <- coordinates(as.matrix(tointerp[,c(6,7)], nrow=length(tointerp)))

imm_gps_1sec_ltraj <- as.ltraj(xy=loc, 
                               id=tointerp$Species_track_stage_season_ring, date=tointerp$DATE.TIME,
                               typeII=T)

interp <- redisltraj(l=imm_gps_1sec_ltraj, u=1, type="time")

# converts back to DF
df_interp <- ld(interp)

# selects columns required and merges back
df_interpm <- df_interp %>%
  dplyr::select(long=x,lat=y,DATETIME=date, Species_track_stage_season_ring=id)

# separates out extra information again
imm_gps_1sec <- df_interpm %>%
  separate(col=Species_track_stage_season_ring, into = c("TRACKID", "SPECIES", "STAGE", "SEASON", "RING"), sep="-")

## join to wet/dry, then extnd down

immdataNEW_tojoin <- imm_dataNEW %>%
  dplyr::select(datetime, secs, wet_dry, SPECIES, Ring) %>%
  mutate(DATETIME=as.character(datetime))


imm_gps_joined <- imm_gps_1sec %>%
  mutate(DATETIME=as.character(DATETIME)) %>%
  rename(Ring=RING) %>%
  left_join(immdataNEW_tojoin,by=c("SPECIES", "Ring", "DATETIME"))

wet_points <- imm_gps_joined %>% 
  fill(wet_dry) %>% 
  filter(wet_dry == "wet") %>%
  mutate(DATETIME = as.POSIXct(DATETIME, format= "%Y-%m-%d %H:%M:%S", tz="GMT"))#%>%
  #slice(which(row_number() %% 5 == 1))

dive_2sec <- dive_2sec %>% rename(Ring = RING)
imm_gps_5min <- imm_gps_5min %>% rename(Ring=RING)


## dive events STARTS ONLY

dive_2sec %>%
  filter()

alldivesT_GPS1sec_GHABBA$RING <- as.character(alldivesT_GPS1sec_GHABBA$RING)

## dive events w location of start point

imm_gps_1sec$testdate <- as.character(imm_gps_1sec$DATETIME)

startdive_locations <- alldives_combined %>%
  mutate(RING=Ring) %>%
  mutate(testdate=as.character(S_DateTime)) %>%
  left_join(imm_gps_1sec, by=c("testdate","RING", "SPECIES")) %>%
  dplyr::select(SPECIES, TDR_no, RING, S_DateTime, E_DateTime, dur, depth, doubt, 
         testdate, dive_ID, long, lat, TRACKID, STAGE, SEASON) %>%
  mutate(DATETIME = as.POSIXct(testdate, format = "%Y-%m-%d %H:%M:%S"))



## Plot tracks and immersion
ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_path() + 
  coord_map("azequalarea", orientation=c(-55, -45, 0),xlim=c(-34, -48), ylim=c(-44,-57)) +
  geom_point(data=subset(imm_gps_5min, Ring=="1436905"), 
             aes(long, lat, group=TRACKID), size=0.7, alpha=0.5, color="grey") + 
  geom_point(data=subset(wet_points, Ring=="1436905"), 
             aes(long, lat, group=TRACKID), size=2, color="lightblue") + 
  geom_point(data=subset(dive_2sec, Ring=="1436905"), 
             aes(long, lat, group=TRACKID), size=2.5, pch = 4) +
  theme_bw(base_family="Hoefler Text") +
  theme(legend.position = "right")

ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_path() + 
  coord_map("azequalarea", orientation=c(-55, -45, 0),xlim=c(-34, -48), ylim=c(-44,-57)) +
  geom_point(data=subset(imm_gps_5min, Ring=="1320592"), 
             aes(long, lat, group=TRACKID), size=0.7, alpha=0.5, color="grey") + 
  geom_point(data=subset(wet_points, Ring=="1320592"), 
             aes(long, lat, group=TRACKID), size=2, color="lightblue") + 
  geom_point(data=subset(dive_2sec, Ring=="1320592"), 
             aes(long, lat, group=TRACKID), size=2.5, pch = 4) +
  theme_bw(base_family="Hoefler Text") +
  theme(legend.position = "right")


ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_path() + 
  coord_map("azequalarea", orientation=c(-55, -45, 0),xlim=c(-36, -44), ylim=c(-52,-56)) +
  geom_point(data=subset(imm_gps_5min, Ring=="1320592"), 
             aes(long, lat, group=TRACKID), size=0.7, alpha=0.5, color="grey") + 
  geom_point(data=subset(wet_points, Ring=="1320592"), 
             aes(long, lat, group=TRACKID), size=2, color="lightblue") + 
  geom_point(data=subset(dive_2sec, Ring=="1320592"), 
             aes(long, lat, group=TRACKID), size=2.5, pch = 4) +
  theme_bw(base_family="Hoefler Text") +
  theme(legend.position = "right")

# all the birds
ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_path() + 
  coord_map("azequalarea", orientation=c(-55, -45, 0),xlim=c(-34, -48), ylim=c(-44,-57)) +
  geom_point(data=subset(imm_gps_5min), 
             aes(long, lat, group=TRACKID), size=0.7, alpha=0.5, color="grey") + 
  geom_point(data=subset(wet_points), 
             aes(long, lat, group=TRACKID), size=2, color="lightblue") + 
  geom_point(data=subset(dive_2sec), 
             aes(long, lat, group=TRACKID), size=2.5, pch = 4) +
  theme_bw(base_family="Hoefler Text") +
  theme(legend.position = "right")


## get zeniths for wet points

zenithsIMM <- zenith(solar(wet_points$DATETIME), wet_points$long, wet_points$lat)

wet_points$zenith <- zenithsIMM

wet_points <- wet_points %>%
  mutate(light=ifelse(zenith<96, "twilight", "night")) %>%
  mutate(light=ifelse(zenith<90, "day", light))

# all birds wet/dry/night
ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_path() + 
  coord_map("azequalarea", orientation=c(-55, -45, 0),xlim=c(-34, -48), ylim=c(-44,-57)) +
  geom_point(data=subset(imm_gps_5min), 
             aes(long, lat, group=TRACKID), size=0.7, alpha=0.5, color="grey") + 
  geom_point(data=subset(wet_points, light=="day"), 
             aes(long, lat, group=TRACKID), size=2, color="lightblue") + 
  geom_point(data=subset(wet_points, light=="twilight"), 
             aes(long, lat, group=TRACKID), size=1, color="mediumblue", alpha=0.8) + 
  geom_point(data=subset(wet_points, light=="night"), 
             aes(long, lat, group=TRACKID), size=1, color="darkblue", alpha=0.8) + 
  geom_point(data=subset(dive_2sec), 
             aes(long, lat, group=TRACKID), size=2.5, pch = 4) +
  theme_bw(base_family="Hoefler Text") +
  theme(legend.position = "right") +
  facet_wrap(~SPECIES)


## all birds wet/dry night with only start locations of dives

ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_path() + 
  coord_map("azequalarea", orientation=c(-55, -45, 0),xlim=c(-34, -48), ylim=c(-44,-57)) +
  geom_point(data=subset(imm_gps_5min, imm_gps_5min$TRACKID %in% wet_points$TRACKID), 
             aes(long, lat, group=TRACKID), size=0.7, alpha=0.5, color="grey") + 
  geom_point(data=subset(wet_points, light=="day"), 
             aes(long, lat, group=TRACKID), size=1, color="lightblue") + 
  geom_point(data=subset(wet_points, light=="twilight"), 
             aes(long, lat, group=TRACKID), size=1, color="mediumblue", alpha=0.8) + 
  geom_point(data=subset(wet_points, light=="night"), 
             aes(long, lat, group=TRACKID), size=1, color="darkblue", alpha=0.8) + 
  geom_point(data=subset(startdive_locations, SPECIES!="LMSA"), 
             aes(long, lat, group=TRACKID), size=2.5, pch = 4) +
  theme_bw(base_family="Hoefler Text") +
  theme(legend.position = "right") +
  facet_wrap(~SPECIES)


# reduce freqeucny of immm data
wet_points_plot  <- wet_points %>%
  filter(row_number() %% 3 == 0)

## by bird
ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_path() + 
  coord_map("azequalarea", orientation=c(-55, -45, 0),xlim=c(-34, -48), ylim=c(-44,-57)) +
  geom_point(data=subset(imm_gps_5min, TRACKID %in% mapbirds$TRACKID), 
             aes(long, lat, group=TRACKID), size=0.7, alpha=0.5, color="grey") + 
  geom_point(data=subset(wet_points_plot, light=="day"), 
             aes(long, lat, group=TRACKID), size=1, color="lightblue") + 
  geom_point(data=subset(wet_points_plot, light=="twilight"), 
             aes(long, lat, group=TRACKID), size=1, color="mediumblue", alpha=0.8) + 
  geom_point(data=subset(wet_points_plot, light=="night"), 
             aes(long, lat, group=TRACKID), size=1, color="darkblue", alpha=0.8) + 
  geom_point(data=subset(startdive_locations, SPECIES!="LMSA"), 
             aes(long, lat, group=TRACKID), size=2.5, pch = 4) +
  theme_bw(base_family="Hoefler Text") +
  theme(legend.position = "right") +
  facet_wrap(~TRACKID)


## zeniths for start point of dive only

zenithsSTART <- zenith(solar(startdive_locations$DATETIME), startdive_locations$long, startdive_locations$lat)
startdive_locations$zeniths <- zenithsSTART

# which dives are day/twlight/dark
## A COUPLE HAVE BEEN DROPPED FOR NO LOCS.(reported as missing in manuscript)
startdive_locations %>%
  mutate(light=ifelse(zeniths<96, "twilight", "night")) %>%
  mutate(light=ifelse(zeniths<90, "day", light)) %>% View()
  group_by(SPECIES, light) %>%
  summarise(n())

wet_points %>%
  group_by(SPECIES, light) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = light, values_from=n) %>%
  mutate(total=sum(day, night, twilight)) %>%
  mutate(prop_day = day/total,
         prop_night = night/total,
         prop_twi = twilight/total)

## means and sd
wet_points %>%
  group_by(SPECIES, Ring, light) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = light, values_from=n) %>%
  mutate(total=sum(day, night, twilight)) %>%
  mutate(prop_day = day/total,
         prop_night = night/total,
         prop_twi = twilight/total) %>%
  group_by(SPECIES) %>%
  mutate(mean_prop_day=mean(prop_day),
         sd_prop_day=sd(prop_day),
         mean_prop_night=mean(prop_night),
         sd_prop_night=sd(prop_night),
         mean_prop_twi=mean(prop_twi),
         sd_prop_twi=sd(prop_twi)) %>% View()




# plot of immersion over trip length
dive_prop_wet_prop_tripNEW %>%
  filter(Ring !="1148957") %>% 
  ggplot(aes(x=reorder(Ring, triphours))) + 
  geom_bar(aes(x=reorder(Ring, triphours), y=triphours),fill="light grey", stat="identity") +
  geom_bar(aes(x=reorder(Ring, triphours), y=imm_hour), fill="light blue", stat="identity") +
  geom_bar(aes(x=reorder(Ring, triphours), y=divehours), fill="black", stat="identity") +
  theme_bw(base_family="Hoefler Text") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(name= "Hours", breaks=c(0, 20, 40, 60, 80), 
                     labels=c("0", "20", "40", "60", "80")) +
  scale_x_discrete(name="Bird ID") +
  facet_wrap(~SPECIES,nrow = 1,scales = "free_x")

## plot immersion events by light
wet_points %>%
  mutate(light2=case_when(light=="day" ~"aday",
                          light=="night" ~"cnight",
                          light=="twilight" ~"btwilight")) %>%
  ggplot(aes(x = SPECIES,fill = light2)) + 
  geom_bar(position = "fill") +
  theme_bw() +
  scale_fill_manual(values = c("lightblue", "grey50", "midnightblue"),
                    labels = c("day", "twilight", "night"),
                    name ="Light") +
  scale_y_continuous(name="Time Immersed (Proportion)") +
  scale_x_discrete(name="Species")
  
### by individual
wet_points %>%
  filter(Ring!="1320030")%>%
  mutate(light2=case_when(light=="day" ~"aday",
                          light=="night" ~"cnight",
                          light=="twilight" ~"btwilight")) %>%
  ggplot(aes(x = Ring,fill = light2)) + 
  geom_bar(position = "fill") +
  theme_bw(base_family="Arial", base_size=16) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("lightblue", "grey50", "midnightblue"),
                    labels = c("day", "twilight", "night"),
                    name ="Light") +
  scale_y_continuous(name="Proportion of Time Immersed") +
  scale_x_discrete(name="Bird ID") + 
  facet_wrap(~SPECIES, scale='free_x')

### calculate average trip time wet in day/night/twilight


zenithsIMMALL <- zenith(solar(as.POSIXct(imm_gps_joined$DATETIME, format="%Y-%m-%d %H:%M:%S", tz= "GMT")), 
                        imm_gps_joined$long, imm_gps_joined$lat)

imm_gps_joined$zenith <- zenithsIMMALL

imm_gps_joined2 <- imm_gps_joined %>%
  group_by(TRACKID) %>%
  arrange(TRACKID, DATETIME) %>%
  fill(wet_dry) %>% 
  mutate(light=ifelse(zenith<96, "twilight", "night")) %>%
  mutate(light=ifelse(zenith<90, "day", light)) %>%
  mutate(wet_dry = ifelse(is.na(wet_dry), "dry", wet_dry))

imm_gps_joined2 %>% 
  mutate(wetdrydaynight = paste(light, wet_dry)) %>% 
  mutate(wetdrydaynight = ifelse(str_detect(wetdrydaynight, "twilight"), 
                                 paste("f", wetdrydaynight), wetdrydaynight)) %>%
  filter(Ring!=("1147587")) %>%
  ggplot(aes(x = Ring,fill = wetdrydaynight)) + 
  geom_bar(position = "fill") +
  theme_bw(base_family="Arial", base_size=16) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("gold", "gold2", 
                               "gray53", "gray73",
                               "dodgerblue3", "dodgerblue4"),
                    labels = c("day, dry", "day, immersed", 
                               "twilight, dry", "twilight, immersed", 
                               "night, dry", "night, immersed"),
                    name ="Light/Immersion Conditions") +
  scale_y_continuous(name="Proportion of Foraging Trip") +
  scale_x_discrete(name="Bird ID") + 
  facet_wrap(~SPECIES, scale='free_x')
