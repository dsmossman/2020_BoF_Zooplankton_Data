##########
# Initialization
##########

library(tidyverse)
library(ggpubr)
library(oce)

setwd("~/Documents/BoF2020_Cruise/Raw_Data/Glider")

Glider_Instruments = read.csv("Glider_Instruments.csv")
Glider_Instruments$c_pitch..rad.[is.na(Glider_Instruments$c_pitch..rad.)] = 0
Glider_Instruments$m_pitch..rad.[is.na(Glider_Instruments$m_pitch..rad.)] = 0
Glider_Instruments = na.omit(Glider_Instruments[, c(1:4, 10, 31, 69)])
names(Glider_Instruments) = c("Date",
                              "Depth_m",
                              "Latitude",
                              "Longitude",
                              "c_pitch",
                              "m_pitch",
                              "profile_id")
Glider_Instruments$c_pitch = Glider_Instruments$c_pitch * 180/pi
Glider_Instruments$m_pitch = Glider_Instruments$m_pitch * 180/pi

Glider_Instruments[which(startsWith(Glider_Instruments$Date, "2020-09-19")), 1] = "19-Sep"
Glider_Instruments[which(startsWith(Glider_Instruments$Date, "2020-09-20")), 1] = "20-Sep"
Glider_Instruments[which(startsWith(Glider_Instruments$Date, "2020-09-21")), 1] = "21-Sep"
Glider_Instruments[which(startsWith(Glider_Instruments$Date, "2020-09-24")), 1] = "24-Sep"
Glider_Instruments[which(startsWith(Glider_Instruments$Date, "2020-09-25")), 1] = "25-Sep"
Glider_Instruments[which(startsWith(Glider_Instruments$Date, "2020-09-26")), 1] = "26-Sep"

attach(Glider_Instruments)
Glider_19 = subset(Glider_Instruments, Date == "19-Sep")
distance_19 = as.data.frame(geodDist(Glider_19$Longitude, Glider_19$Latitude, alongPath = T))
names(distance_19) = "Distance_km"
Glider_20 = subset(Glider_Instruments, Date == "20-Sep")
distance_20 = as.data.frame(geodDist(Glider_20$Longitude, Glider_20$Latitude, alongPath = T))
names(distance_20) = "Distance_km"
Glider_21 = subset(Glider_Instruments, Date == "21-Sep")
distance_21 = as.data.frame(geodDist(Glider_21$Longitude, Glider_21$Latitude, alongPath = T))
names(distance_21) = "Distance_km"
Glider_24 = subset(Glider_Instruments, Date == "24-Sep")
distance_24 = as.data.frame(geodDist(Glider_24$Longitude, Glider_24$Latitude, alongPath = T))
names(distance_24) = "Distance_km"
Glider_25 = subset(Glider_Instruments, Date == "25-Sep")
distance_25 = as.data.frame(geodDist(Glider_25$Longitude, Glider_25$Latitude, alongPath = T))
names(distance_25) = "Distance_km"
Glider_26 = subset(Glider_Instruments, Date == "26-Sep")
distance_26 = as.data.frame(geodDist(Glider_26$Longitude, Glider_26$Latitude, alongPath = T))
names(distance_26) = "Distance_km"
detach(Glider_Instruments)

distances = rbind(distance_19,
                  distance_20,
                  distance_21,
                  distance_24,
                  distance_25,
                  distance_26)
Glider_Instruments = cbind(Glider_Instruments, distances)

OB_Glider_Data = Glider_Instruments[which(
  Glider_Instruments$Date == "19-Sep" |
    Glider_Instruments$Date == "20-Sep" | Glider_Instruments$Date == "26-Sep"
),]
GMB_Glider_Data = Glider_Instruments[which(
  Glider_Instruments$Date == "21-Sep" |
    Glider_Instruments$Date == "24-Sep" | Glider_Instruments$Date == "25-Sep"
),]

##########
# Depth and Pitch Profiles
##########

OB_Depth_Profiles = ggplot() +
  
  geom_path(data = OB_Glider_Data, aes(x = Distance_km, y = Depth_m), size = 0.3) +
  
  theme_bw() +
  
  scale_y_reverse() +
  
  labs(x = NULL, y = "Depth (m)", title = "Owen Basin Glider Dive Profiles by Date") +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  
  facet_wrap( ~ Date, scales = "free_x")

# print(OB_Depth_Profiles)

OB_Pitch_Profiles = ggplot() +
  
  geom_path(data = OB_Glider_Data, aes(x = Distance_km, y = m_pitch), size = 0.3) +
  
  scale_y_continuous(breaks = c(-30, 0, 30)) +
  
  theme_bw() +
  
  labs(x = "Distance Traveled (km)", y = "Pitch (degrees)", title = NULL) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  
  facet_wrap( ~ Date, scales = "free_x") +
  
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

# print(OB_Pitch_Profiles)

# print(ggarrange(OB_Depth_Profiles, OB_Pitch_Profiles, nrow = 2))


GMB_Depth_Profiles = ggplot() +
  
  geom_path(data = GMB_Glider_Data, aes(x = Distance_km, y = Depth_m), size = 0.3) +
  
  theme_bw() +
  
  scale_y_reverse() +
  
  labs(x = NULL, y = "Depth (m)", title = "Grand Manan Basin Glider Dive Profiles by Date") +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  
  facet_wrap( ~ Date, scales = "free_x")

# print(GMB_Depth_Profiles)

GMB_Pitch_Profiles = ggplot() +
  
  geom_path(data = GMB_Glider_Data, aes(x = Distance_km, y = m_pitch), size = 0.3) +
  
  scale_y_continuous(breaks = c(-30, 0, 30)) +
  
  theme_bw() +
  
  labs(x = "Distance Traveled (km)", y = "Pitch (degrees)", title = NULL) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  
  facet_wrap( ~ Date, scales = "free_x") +
  
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

# print(GMB_Pitch_Profiles)

# print(ggarrange(GMB_Depth_Profiles, GMB_Pitch_Profiles, nrow = 2))

##########
# Downcast Angles
##########


OB_Downcast_IDs = OB_Glider_Data$profile_id %in% c(1600533105, 1600535126, 1600536546, 1600538203, 1600540065, 1600542112, 1600543948, 1600545739, 1600547449,
                    1600603784, 1600605390, 1600607486, 1600609399, 1600611599, 1600613794,
                    1601122482, 1601124510, 1601126683, 1601127710, 1601130025, 1601131887)

OB_Downcasts = OB_Glider_Data[OB_Downcast_IDs,]
OB_Downcasts$profile_id = as.factor(OB_Downcasts$profile_id)

GMB_Downcast_IDs = GMB_Glider_Data$profile_id %in% c(1600694472, 1600696131, 1600698685, 1600700960, 1600703305, 1600705642, 1600707857,
                     1600957895, 1600959355, 1600961869, 1600964503, 1600966943, 1600969453, 1600971917, 1600974148, 1600976274,
                     1601047930, 1601049464, 1601051967, 1601054388, 1601056970, 1601059523, 1601061902)

GMB_Downcasts = GMB_Glider_Data[GMB_Downcast_IDs,]
GMB_Downcasts$profile_id = as.factor(GMB_Downcasts$profile_id)

OB_Downcast_Boxplots = ggplot() +
  
  geom_boxplot(data = OB_Downcasts, aes(x = profile_id, y = m_pitch)) +
  
  theme_bw() +
  
  scale_x_discrete(labels = c("Dive 1", "Dive 2", "Dive 3", "Dive 4", "Dive 5", "Dive 6", "Dive 7", "Dive 8", "Dive 9", "Dive 10",
                              "Dive 11", "Dive 12", "Dive 13", "Dive 14", "Dive 15", "Dive 16", "Dive 17", "Dive 18", "Dive 19", "Dive 20", "Dive 21")) +
  
  labs(x = "Dive #", y = "Pitch (degrees)", title = "Owen Basin Glider Pitch Boxplots") +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  
  theme(axis.text.x=element_text(color = "black", size=9, angle=45, vjust=.8, hjust=0.8))

# print(OB_Downcast_Boxplots)

OB_Downcast_Pitch_Profiles = ggplot() +
  
  geom_path(data = OB_Downcasts, aes(x = m_pitch, y = Depth_m)) +
  
  scale_y_reverse() +
  
  theme_bw() +
  
  labs(x = "Pitch (degrees)", y = "Depth (m)", title = "Owen Basin Glider Pitch Over Depth") +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  
  facet_wrap(~ profile_id)

print(OB_Downcast_Pitch_Profiles)

##########
# Testing Omissions of Certain Data Points
##########

test_data_frame = OB_Downcasts

for(i in 1:length(test_data_frame$m_pitch)){
  if(test_data_frame$m_pitch[i] > -15){ # -15 seems a good cutoff to get rid of areas at beginning/ends of dives, when glider is starting to pitch back up
    test_data_frame$m_pitch[i] = NA
  }
  else{
    next
  }
}
test_data_frame = na.omit(test_data_frame)

ggplot() +
  
  geom_boxplot(data = test_data_frame, aes(x = profile_id, y = m_pitch)) +
  
  theme_bw() +
  
  scale_x_discrete(labels = c("Dive 1", "Dive 2", "Dive 3", "Dive 4", "Dive 5", "Dive 6", "Dive 7", "Dive 8", "Dive 9", "Dive 10",
                              "Dive 11", "Dive 12", "Dive 13", "Dive 14", "Dive 15", "Dive 16", "Dive 17", "Dive 18", "Dive 19", "Dive 20", "Dive 21")) +
  
  labs(x = "Dive #", y = "Pitch (degrees)", title = "Owen Basin Glider Pitch Boxplots") +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  
  theme(axis.text.x=element_text(color = "black", size=9, angle=45, vjust=.8, hjust=0.8))

ggplot() +
  
  geom_path(data = test_data_frame, aes(x = m_pitch, y = Depth_m)) +
  
  scale_y_reverse() +
  
  theme_bw() +
  
  labs(x = "Pitch (degrees)", y = "Depth (m)", title = "Owen Basin Glider Pitch Over Depth") +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  
  facet_wrap(~ profile_id)


