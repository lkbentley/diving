library(tidyverse)
library(readr)
library(sp)
library(adehabitatLT)
library(SGAT)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)


# map for manuscript

## GPS tracks to plot
 ## brdlist

Master_Birds_test <- read_csv("Data/Immersion/Master_Birds_test.csv", 
                              col_types = cols(END_DATE = col_datetime(format = "%d/%m/%Y %H:%M"), 
                                               NOTES = col_skip(), START_DATE = col_datetime(format = "%d/%m/%Y %H:%M"), 
                                               X15 = col_skip()))

Master_Birds_test <- Master_Birds_test %>%
  unite(SPP_Ring_GLS,c(SPECIES, RING, GLS_ID), sep="_",remove=F) %>%
  rename(trip_start=START_DATE, trip_end = END_DATE, all_detects=ALL_DETECTS, n_dives=N_DIVES)



# BRING IN GPS

all_gps <- read_csv("Data/adult_gps_speedfilter10lox_linearinterpol.csv", 
                                                      col_types = cols(DATE.TIME = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                                                       END.TIME = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                                                       START.TIME = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                                                       RING = col_character(),
                                                                       X1 = col_skip()))

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
# run if u = 300
#imm_gps_5min <- df_interpm %>%
 # separate(col=Species_track_stage_season_ring, into = c("TRACKID", "SPECIES", "STAGE", "SEASON", "RING"), sep="-")

#write.csv(imm_gps_5min, "imm_gps_5min.csv", row.names = F)

imm_gps_5min <- read_csv("imm_gps_5min.csv", 
                         col_types = cols(DATETIME = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                          RING = col_character()))

# run if u = 1
#imm_gps_1sec <- df_interpm %>%
 # separate(col=Species_track_stage_season_ring, into = c("TRACKID", "SPECIES", "STAGE", "SEASON", "RING"), sep="-")

#write.csv(imm_gps_1sec, "imm_gps_1sec.csv", row.names = F)

imm_gps_1sec <- read_csv("imm_gps_1sec.csv", 
                     col_types = cols(DATETIME = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                      RING = col_character()))


immdataNEW_tojoin <- imm_dataNEW %>%
  dplyr::select(datetime, secs, wet_dry, SPECIES, Ring) %>%
  rename(DATETIME=datetime)



imm_gps_joined <- imm_gps_1sec %>%
  rename(Ring=RING) %>%
  left_join(immdataNEW_tojoin)

wet_points <- imm_gps_joined %>% 
  fill(wet_dry) %>% 
  filter(wet_dry == "wet") #%>%
# slice(which(row_number() %% 5 == 1))


imm_gps_5min <- imm_gps_5min %>% rename(Ring=RING)




 ## attach to immdata (in immersion_corrected script)
immdataNEW_tojoin <- imm_dataNEW %>%
  dplyr::select(datetime, secs, wet_dry, SPECIES, Ring) %>%
  rename(DATETIME=datetime)


imm_gps_joined <- imm_gps_1sec %>%
  rename(Ring=RING) %>%
  left_join(immdataNEW_tojoin)


wet_points <- imm_gps_joined %>% 
  fill(wet_dry) %>% 
  filter(wet_dry == "wet") 


imm_gps_1sec$testdate <- as.character(imm_gps_1sec$DATETIME)

startdive_locations <- alldives_combined %>%
  mutate(RING=Ring) %>%
  mutate(testdate=as.character(S_DateTime)) %>%
  left_join(imm_gps_1sec, by=c("testdate","RING", "SPECIES")) %>%
  dplyr::select(SPECIES, TDR_no, RING, S_DateTime, E_DateTime, dur, depth, doubt, 
                testdate, dive_ID, long, lat, TRACKID, STAGE, SEASON) %>%
  mutate(DATETIME = as.POSIXct(testdate, format = "%Y-%m-%d %H:%M:%S"))




## get zeniths for wet points

zenithsIMM <- zenith(solar(wet_points$DATETIME), wet_points$long, wet_points$lat)

wet_points$zenith <- zenithsIMM

wet_points <- wet_points %>%
  mutate(light=ifelse(zenith<96, "twilight", "night")) %>%
  mutate(light=ifelse(zenith<90, "day", light))


## birds to map
mapbirds<-Master_Birds_test %>%
  filter(MAP==1) %>%
  dplyr::select(TRACKID)


# reduce freqeucny of immm data
wet_points_plot  <- wet_points %>%
  dplyr::mutate(TOKEEP = ifelse(DATETIME %in% startdive_locations$DATETIME == T, 1, 0)) %>%
  dplyr::mutate(TOKEEP2 = ifelse(row_number() %% 5 == 0, 1, 0)) %>%
  filter(TOKEEP==1|TOKEEP2==1)


world <- maps::map("world", ".", exact = FALSE, plot = FALSE, fill = TRUE) %>% fortify()


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

## all
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
  theme_bw() +
  #theme(legend.position = "right")  +
  facet_wrap(~SPECIES)


imm_gps_5minplot<-subset(imm_gps_5min, TRACKID %in% mapbirds$TRACKID)

gpspointsplot <- st_as_sf(imm_gps_5minplot, coords = c("long", "lat"), 
                  crs = 4326, agr = "constant")

wet_points_plotsf <- st_as_sf(wet_points_plot, coords = c("long", "lat"), 
                              crs = 4326, agr = "constant")

startdive_locations_sf <- st_as_sf(subset(startdive_locations, !is.na(long)), coords = c("long", "lat"), 
                              crs = 4326, agr = "constant")


# the bonding box polygon in long/lat projection, i.e. axis-aligned
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"
crsLAEA <- "+proj=laea +lon_0=-45 +lat_0=-75 +datum=WGS84 +units=m +no_defs"

bb <- st_sfc(
  st_polygon(list(cbind(
    c(-48,-34, -34, -48, -48), # x-coordinates (longitudes) of points A,B,C,D
    c(-56, -56, -44, -44, -56)     # y-coordinates (latitudes) of points A,B,C,D
  ))),
  crs = crsLONGLAT)

# now in in LAEA projection
laeabb <- st_transform(bb, crs = crsLAEA)

# the extent of the bounding box in the new projection
b <- st_bbox(laeabb)
b


worldne <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = worldne) +
  geom_sf(fill = "darkolivegreen4", alpha=0.5) +
  geom_sf(data = gpspointsplot, size=0.8, alpha=0.8, color="grey") +
  geom_sf(data = subset(wet_points_plotsf, light=="day"), size=0.8, alpha=0.3, color="lightblue") +
  geom_sf(data = subset(wet_points_plotsf, light=="twilight"), size=0.8, alpha=0.3, color="mediumblue") +
  geom_sf(data = subset(wet_points_plotsf, light=="night"), size=0.8, alpha=0.3, color="darkblue") +
 # geom_sf(data = subset(wet_points_plotsf, (TRACKID == "EOMN0029_20100107204800")), size=3, alpha=0.3, color="pink") + ## TESTING LINE  
  geom_sf(data = startdive_locations_sf, size=2.5, pch=4, color="firebrick3", alpha=0.8) +
  coord_sf(crs = "+proj=laea +datum=WGS84 +lon_0=-45 +lat_0=-76", xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  #coord_sf(xlim=c(-48, -34), ylim=c(-57,-44), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.2, style = "ticks") +
  annotation_north_arrow(location = "bl", which_north = "true", width=unit(0.7, "cm"), height = unit(0.7, "cm"),
                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                        style = north_arrow_orienteering) +

  xlab("Longitude") + ylab("Latitude") +
  theme(text = element_text(size=14), panel.grid.major = element_line(color = gray(0.1), linetype = "solid",
                                        size = 0.1), panel.background = element_rect(fill = "white")) + 
  facet_wrap(~SPECIES) ## WRAP BY SPECIES OR TRACK ID


## trip lengths ## DIVES/DAY NOT RIGHT FOR LMSA -- AKIKO PROVIDED CORRECT NUMBERS IN EMAIL
Master_Birds_test %>%
  filter(!is.na(trip_start)) %>%
  mutate(trip_days = (trip_end-trip_start)/24) %>% 
  mutate(divesperday = n_dives/as.numeric(trip_days)) %>%
  mutate(nondiver = case_when(n_dives == 0 ~1, 
                              n_dives > 0 ~ 0)) %>%
  group_by(SPECIES) %>%
  summarise(meantriplength = mean(trip_days),
            sdtriplength = sd(trip_days),
            meandivespertrip = mean(n_dives, na.rm=T),
            sddivespertrip = sd(n_dives, na.rm=T),
            mediandivesperday = median(divesperday, na.rm=T),
            meandivesperday = mean(divesperday, na.rm=T),
            sddivesperday = sd(divesperday, na.rm=T),
            divestotal = sum(n_dives),
            nondivers = sum(nondiver),
            birdstotal= length(unique(ID))) %>% View()

                       
#### summary table for supplementary

imm_gps_joined2 %>% group_by(TRACKID, wet_dry) %>%
  summarize(n=n()) %>% group_by(TRACKID) %>%
  pivot_wider(names_from = "wet_dry", values_from = "n") %>%
  mutate(totaltime = dry+wet) %>%
  mutate(dryhr= dry/60/60,
         wethr= wet/60/60,
         totalhr =totaltime/60/60) %>%
  mutate(dryd= dry/60/60/24,
         wetd= wet/60/60/24,
         totald =totaltime/60/60/24)

Master_Birds_test %>% left



                   