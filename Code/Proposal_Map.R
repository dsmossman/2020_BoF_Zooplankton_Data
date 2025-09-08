# Set working directory and libraries
rm(list = ls())

setwd("C:/Users/dmossman/Box/2022 MSc Thesis Work/")
library(tidyverse)
library(sp)
library(sf)
library(ggplot2)
library(ggsn)
library(dplyr)
library(marmap)
library(ggpubr)
library(readr)
library(cowplot)


##### 

## Bathymetric and coastline data
bathy = fortify(getNOAA.bathy(-70, -60, 40, 50))
bathy$z[bathy$z >= 0] = 0
bathy$z = abs(bathy$z)

gshhs.f.b = "Code/gshhg-bin-2.3.7/GSHHS_shp/f/GSHHS_f_L1.shp"

# read in coastline data for specific region
lims = c(xmin=-67,xmax=-60,ymin=44,ymax=50)
sf_use_s2(F)
reg = read_sf(gshhs.f.b) %>% st_make_valid() %>% st_crop(lims)

# Load in cage spreadsheet
Sept2020_Cruise_Leg_2_Cage_Log = read_csv("Raw_Data/Spreadsheets/Sept2020_Cruise_Leg_2_Cage_Log.csv")

# Import GPS data from openCPN for multinet
Sept_19_GPS = na.omit(read_csv("Processed_Data/Multinet/Sept_19_GPS.csv"))
Sept_19_GPS = Sept_19_GPS[, 2:length(Sept_19_GPS)]
Sept_20_GPS = na.omit(read_csv("Processed_Data/Multinet/Sept_20_GPS.csv"))
Sept_20_GPS = Sept_20_GPS[, 2:length(Sept_20_GPS)]
Sept_21_GPS = na.omit(read_csv("Processed_Data/Multinet/Sept_21_GPS.csv"))
Sept_21_GPS = Sept_21_GPS[, 2:length(Sept_21_GPS)]
Sept_24_GPS = na.omit(read_csv("Processed_Data/Multinet/Sept_24_GPS.csv"))
Sept_24_GPS = Sept_24_GPS[, 2:length(Sept_24_GPS)]
Sept_25_GPS = na.omit(read_csv("Processed_Data/Multinet/Sept_25_GPS.csv"))
Sept_25_GPS = Sept_25_GPS[, 2:length(Sept_25_GPS)]
Sept_26_GPS = na.omit(read_csv("Processed_Data/Multinet/Sept_26_GPS.csv"))
Sept_26_GPS = Sept_26_GPS[, 2:length(Sept_26_GPS)]

Multinet_GPS_Data = rbind(Sept_19_GPS,
                          Sept_20_GPS,
                          Sept_21_GPS,
                          Sept_24_GPS,
                          Sept_25_GPS,
                          Sept_26_GPS)
Multinet_Labels = aggregate.data.frame(Multinet_GPS_Data[, 1:2],
                                       list(Multinet_GPS_Data$Track, Multinet_GPS_Data$Date),
                                       mean)
colnames(Multinet_Labels)[1:2] = c("Track", "Date")

# Set up useable data frames
Cage_Data = na.omit(data.frame(Sept2020_Cruise_Leg_2_Cage_Log[, c(1:3, 7:8)]))
names(Cage_Data) = c("Station_Number",
                     "Cage_Number",
                     "Date",
                     "Latitude",
                     "Longitude")
row.names(Cage_Data) = NULL

# Import glider data
Glider_Data = read_csv("Raw_Data/Glider/Glider_Instruments.csv")
Glider_Data = Glider_Data[, c(1, 3:4)]
colnames(Glider_Data) = c("Date", "Latitude", "Longitude")

# Reformatting and parsing glider data
Glider_Data$Date = as.character(Glider_Data$Date)
Glider_Data[which(startsWith(Glider_Data$Date, "2020-09-19")), 1] = "19-Sep"
Glider_Data[which(startsWith(Glider_Data$Date, "2020-09-20")), 1] = "20-Sep"
Glider_Data[which(startsWith(Glider_Data$Date, "2020-09-21")), 1] = "21-Sep"
Glider_Data[which(startsWith(Glider_Data$Date, "2020-09-24")), 1] = "24-Sep"
Glider_Data[which(startsWith(Glider_Data$Date, "2020-09-25")), 1] = "25-Sep"
Glider_Data[which(startsWith(Glider_Data$Date, "2020-09-26")), 1] = "26-Sep"

Glider_Startpoints = aggregate(Glider_Data[, 2:3], list(Glider_Data$Date), first)
colnames(Glider_Startpoints)[1] = c("Date")
Glider_Endpoints = aggregate(Glider_Data[, 2:3], list(Glider_Data$Date), last)
colnames(Glider_Endpoints)[1] = c("Date")

# Function to convert degree minutes to decimal degrees
DD_Conversion = function(dataframe) {
  for (d in 4:ncol(dataframe)) {
    for (a in 1:nrow(dataframe)) {
      degrees = as.numeric(substr(dataframe[a, d], start = 1, stop = 2))
      minutes = as.numeric(substr(
        dataframe[a, d],
        start = 4,
        stop = nchar(dataframe[a, d])
      ))
      
      dataframe[a, d] = degrees + minutes / 60
    }
    dataframe[, d] = as.numeric(dataframe[, d])
  }
  return(dataframe)
}

# Degree minute to decimal degrees conversion
Cage_Data = DD_Conversion(Cage_Data)
Cage_Data$Longitude = -Cage_Data$Longitude

# Owen Basin-specific data formatting
OB_cages = Cage_Data[which(Cage_Data$Longitude < -66.5), ]

OB_tracks = Multinet_GPS_Data[which(Multinet_GPS_Data$Lon < -66.5), ]
OB_labels = Multinet_Labels[which(Multinet_Labels$Lon < -66.5), ]

OB_Glider_Data = Glider_Data[which(
  Glider_Data$Date == "19-Sep" |
    Glider_Data$Date == "20-Sep" | Glider_Data$Date == "26-Sep"
),]
OB_Glider_Startpoints = Glider_Startpoints[which(
  Glider_Startpoints$Date == "19-Sep" |
    Glider_Startpoints$Date == "20-Sep" |
    Glider_Startpoints$Date == "26-Sep"
),]
OB_Glider_Endpoints = Glider_Endpoints[which(
  Glider_Endpoints$Date == "19-Sep" |
    Glider_Endpoints$Date == "20-Sep" |
    Glider_Endpoints$Date == "26-Sep"
),]

#####

# Full plot latitudes/longitudes
lons = c(-67,-66.2)
lats = c(44.45, 44.95)

GMBCritArea = data.frame(c(44+49/60, 44+47/60, 44+40/60, 44+33/60, 44+29/60, 44+29/60, 44+42/60),
                  -c(66+27/60,66+17/60,66+17/60,66+22/60,66+30/60,66+37/60,66+37/60))
colnames(GMBCritArea) = c("Latitude", "Longitude")

Full_Plot = ggplot() +
  
  geom_contour_filled(
    data = bathy,
    aes(x = x, y = y, z = z),
    colour = NA,
    breaks = c(seq(
      from = 0, to = 250, by = 10
    )),
    show.legend = FALSE
  ) +
  scale_fill_grey(start = 0.9, end = 0.3) +
  
  geom_sf(data=reg,fill="gray25",color="black") +
  
  coord_sf(xlim = lons, ylim = lats, expand = FALSE) +
  
  geom_polygon(data = GMBCritArea, aes(x = Longitude, y = Latitude),
               color = "black", fill = "red", alpha = 0.3) +
  
  annotate("text", x = -66.75, y = 44.87, label = "Owen\nBasin", size = 5) +
  annotate("text", x = -66.49, y = 44.6, label = "Grand Manan\nBasin", color = "white", size = 5) +
  annotate("text", x = -66.8, y = 44.71, label = "Grand Manan\nIsland", color = "white", size = 5) +
  
  # geom_path(
  #   data = Glider_Data,
  #   aes(x = Longitude, y = Latitude, colour = Date),
  #   alpha = 1,
  #   size = 2,
  #   show.legend = FALSE
  # ) +
  # 
  # scale_colour_manual(
  #   values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"),
  #   aesthetics = c("colour")
  # ) +
  
  ggsn::scalebar(
    data = reg,
    location = "topright",
    dist = 2,
    dist_unit = "nm",
    transform = TRUE,
    model = "WGS84",
    anchor = c(x = -66.25, y = 44.93),
    height = 0.004,
    st.dist = 0.005,
    st.size = 5
  ) +
  
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Depth (m)",
       title = NULL) +
  
  theme_bw() + 
  
  theme(panel.grid = element_blank(),
        axis.text=element_text(size=20),
        axis.title = element_text(size = 20)) +
  scale_y_continuous(breaks = c(44.50, 44.55, 44.60, 44.65, 44.70, 44.75, 44.8, 44.85, 44.90)) +
  scale_x_continuous(breaks = c(-66.9, -66.8, -66.7, -66.6, -66.5, -66.4, -66.3))

print(Full_Plot)

canada = map_data("world2", region = "Canada")

Inset_Map = ggplot() + geom_polygon(
  data = canada,
  aes(x = long, y = lat, group = group),
  fill = "grey",
  color = "black"
) +
  coord_fixed(xlim = c(290, 305), ylim = c(40, 55), ratio = 1.3, expand = FALSE) +
  annotate(
    geom = "rect",
    ymax = 47,
    ymin = 43,
    xmax = 297,
    xmin = 292,
    colour = "red",
    fill = NA
  ) +
  theme_bw() +
  
  labs(x = NULL,
       y = NULL,
       title = NULL) +
  
  theme_bw() + 
  
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.background = element_rect(fill = "white"))
  
print(Inset_Map)

Poster_Plot = ggdraw() +
  draw_plot(Full_Plot) +
  draw_plot(Inset_Map, x = 0.2, y = 0.13, width = 0.2, height = 0.2)
print(Poster_Plot)
ggsave(filename = "Visuals/Thesis_Study_Area_Plot.png")

#####

lons = c(-66.82,-66.75)
lats = c(44.83, 44.90)

Zoomed_Plot = ggplot() +
  
  geom_contour_filled(
    data = bathy,
    aes(x = x, y = y, z = z),
    colour = NA,
    breaks = c(seq(
      from = 0, to = 200, by = 10
    )),
    show.legend = FALSE
  ) +
  scale_fill_grey(start = 0.9, end = 0.3) +
  
  coord_quickmap(xlim = lons, ylim = lats, expand = TRUE) +
  
  geom_path(
    data = Glider_Data[which(Glider_Data$Date == "20-Sep"),],
    aes(x = Longitude, y = Latitude, colour = Date),
    alpha = 1,
    linewidth = 0.9,
    show.legend = FALSE
  ) +
  
  scale_colour_manual(
    values = c("#56B4E9"),
    aesthetics = c("colour")
  ) +
  
  geom_path(
    data = OB_tracks[which(OB_tracks$Date == "20-Sep"),],
    aes(x = Lon, y = Lat, group = Track),
    alpha = 1,
    linewidth = 0.7,
    arrow = arrow(length = unit(8, "points"), ends = "last"),
    show.legend = FALSE
  ) +
  
  geom_point(
    data = OB_cages[which(OB_cages$Date == "20-Sep"),],
    aes(x = Longitude, y = Latitude),
    fill = "grey",
    shape = 21,
    alpha = 0.7,
    show.legend = FALSE
  ) +
  
  geom_text(
    data = OB_labels[which(OB_labels$Date == "20-Sep"),],
    aes(x = Lon, y = Lat, label = Track),
    hjust = "left",
    vjust = "top",
    size = 5
  ) +
  
  ggsn::scalebar(
    data = reg,
    location = "topleft",
    dist = 0.5,
    dist_unit = "nm",
    transform = TRUE,
    model = "WGS84",
    anchor = c(x = -66.818, y = 44.9),
    height = 0.0002,
    st.dist = 0.0005,
    st.size = 4
  ) +
  
  labs(x = NULL,
       y = NULL,
       fill = "Depth (m)",
       title = NULL) +
  
  theme_bw() + 
  
  theme(panel.grid = element_blank(),
        axis.text=element_text(size=10),
        axis.title = element_text(size = 10))  +
  
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Depth (m)",
       title = NULL)

print(Zoomed_Plot)
ggsave(filename = "Visuals/Thesis_Single_Track_Plot.png")

ggarrange(
  Full_Plot,
  Zoomed_Plot,
  labels = "AUTO",
  ncol = 2,
  legend = NULL,
  common.legend = FALSE
)

#####
lons = c(-67,-66.2)
lats = c(44.45, 44.95)

GMB1 = Cage_Data %>% 
  filter(Station_Number %in% c(10,13,17)) %>%
  reframe(Latitude = mean(Latitude),Longitude=mean(Longitude))
GMB2 = Cage_Data %>% 
  filter(Station_Number %in% c(9,16)) %>%
  reframe(Latitude = mean(Latitude),Longitude=mean(Longitude))
GMB3 = Cage_Data %>% 
  filter(Station_Number %in% c(8,12,15)) %>%
  reframe(Latitude = mean(Latitude),Longitude=mean(Longitude))
GMB_Stations = rbind(GMB1,GMB2,GMB3)

OB1 = Cage_Data %>% 
  filter(Station_Number %in% c(5,20)) %>%
  reframe(Latitude = mean(Latitude),Longitude=mean(Longitude))
OB2 = Cage_Data %>% 
  filter(Station_Number %in% c(3,7,18)) %>%
  reframe(Latitude = mean(Latitude),Longitude=mean(Longitude))
OB3 = Cage_Data %>% 
  filter(Station_Number %in% c(4,6,19)) %>%
  reframe(Latitude = mean(Latitude),Longitude=mean(Longitude))
OB_Stations = rbind(OB1,OB2,OB3)

Glider_Plot =
ggplot() +
  
  geom_contour_filled(
    data = bathy,
    aes(x = x, y = y, z = z),
    colour = NA,
    breaks = c(seq(
      from = 0, to = 250, by = 10
    )),
    show.legend = FALSE
  ) +
  scale_fill_grey(start = 0.9, end = 0.3) +
  
  geom_sf(data=reg,fill="gray25",color="black") +
  
  coord_sf(xlim = lons, ylim = lats, expand = FALSE) +
  
  geom_point(data=Cage_Data, aes(x=Longitude,y=Latitude),
             shape=21,
             color="black",
             fill="white") +
  
  geom_path(
    data = Glider_Data,
    aes(x = Longitude, y = Latitude,group=Date),
    color="black",
    alpha = 1,
    linewidth = 0.5,
    show.legend = FALSE
  ) +
  
  geom_text(data=GMB_Stations,aes(x=Longitude,y=Latitude,label=c("GMB1","GMB2","GMB3")),
            size=4) +
  geom_text(data=OB_Stations,aes(x=Longitude,y=Latitude,label=c("OB1","OB2","OB3")),
            size=4) +
  
  ggsn::scalebar(
    data = reg,
    location = "topright",
    dist = 2,
    dist_unit = "nm",
    transform = TRUE,
    model = "WGS84",
    anchor = c(x = -66.25, y = 44.92),
    height = 0.002,
    st.dist = 0.002,
    st.size = 5
  ) +
  
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Depth (m)",
       title = NULL) +
  
  theme_bw() + 
  
  theme(panel.grid = element_blank(),
        axis.text=element_text(size=10),
        axis.title = element_text(size = 10)) +
  scale_y_continuous(breaks = c(44.50, 44.55, 44.60, 44.65, 44.70, 44.75, 44.8, 44.85, 44.90)) +
  scale_x_continuous(breaks = c(-66.9, -66.8, -66.7, -66.6, -66.5, -66.4, -66.3))

ggarrange(Glider_Plot, Zoomed_Plot, ncol=2,labels="AUTO")
