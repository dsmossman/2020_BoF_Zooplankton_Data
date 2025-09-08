# Set working directory and libraries
setwd("~/Documents/BoF2020_Cruise")
library(tidyverse)
library(maptools)
library(ggplot2)
library(ggsn)
library(dplyr)
library(marmap)
library(mapdata)
library(ggpubr)
library(readr)
# install.packages('rgeos', type='source')
# install.packages('rgdal', type='source')

# Bathymetric and coastline data
bathy = fortify(getNOAA.bathy(lon1 = -70, lon2 = -60, lat1 = 40, lat2 = 50))
# If you get an "argument is of length 0" error, reinstall rgdal and raster packages
bathy$z[bathy$z >= 0] = 0
bathy$z = abs(bathy$z)

gshhs.f.b = 'R_Code/gshhg-bin-2.3.7/gshhs_f.b'

xlims = c(-67, -66)
ylims = c(44, 45)

# read in coastline data for specific region and convert to tabular format for ggplot
reg = getRgshhsMap(gshhs.f.b, xlim = xlims, ylim = ylims) %>%
  fortify()

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

# Grand Manan Basin-specific data formatting
GMB_cages = Cage_Data[which(Cage_Data$Longitude > -66.5), ]

GMB_tracks = Multinet_GPS_Data[which(Multinet_GPS_Data$Lon > -66.5), ]
GMB_labels = Multinet_Labels[which(Multinet_Labels$Lon > -66.5), ]

GMB_Glider_Data = Glider_Data[which(
  Glider_Data$Date == "21-Sep" |
    Glider_Data$Date == "24-Sep" | Glider_Data$Date == "25-Sep"
),]
GMB_Glider_Startpoints = Glider_Startpoints[which(
  Glider_Startpoints$Date == "21-Sep" |
    Glider_Startpoints$Date == "24-Sep" |
    Glider_Startpoints$Date == "25-Sep"
),]
GMB_Glider_Endpoints = Glider_Endpoints[which(
  Glider_Endpoints$Date == "21-Sep" |
    Glider_Endpoints$Date == "24-Sep" |
    Glider_Endpoints$Date == "25-Sep"
),]

# ----------------------------------------- #

# Owen Basin latitudes/longitudes
lons = c(-66.85,-66.75)
lats = c(44.82, 44.91)

# Owen Basin station number labels
OB_labels$Track = OB_cages$Station_Number[!duplicated(OB_cages$Station_Number)]
colnames(OB_labels)[1] = "Station_Number"

# Owen Basin plot

OB_Plots = ggplot() +
  
  geom_contour_filled(
    data = bathy,
    aes(x = x, y = y, z = z),
    colour = NA,
    breaks = c(seq(
      from = 0, to = 200, by = 10
    ))
  ) +
  scale_fill_grey(start = 0.9, end = 0.3) +
  
  geom_path(
    data = OB_Glider_Data,
    aes(x = Longitude, y = Latitude, colour = Date),
    alpha = 1,
    size = 0.5,
    show.legend = FALSE
  ) +
  
  scale_colour_manual(
    values = c("#E69F00", "#56B4E9", "#009E73"),
    aesthetics = c("colour")
  ) +
  
  geom_point(
    data = OB_Glider_Startpoints,
    aes(x = Longitude, y = Latitude),
    fill = c("#E69F00", "#56B4E9", "#009E73"),
    shape = 22,
    alpha = 1
  ) +
  
  geom_point(
    data = OB_Glider_Endpoints,
    aes(x = Longitude, y = Latitude),
    fill = c("#E69F00", "#56B4E9", "#009E73"),
    shape = 24,
    alpha = 1
  ) +
  
  geom_path(
    data = OB_tracks,
    aes(x = Lon, y = Lat, group = Track),
    alpha = 1,
    size = 0.4,
    arrow = arrow(length = unit(8, "points"), ends = "last")
  ) +
  
  geom_point(
    data = OB_cages,
    aes(x = Longitude, y = Latitude),
    fill = "grey",
    shape = 21,
    alpha = 0.7
  ) +
  
  geom_text(
    data = OB_labels,
    aes(x = Lon, y = Lat, label = Station_Number),
    hjust = "left",
    vjust = "top"
  ) +
  
  ggsn::scalebar(
    data = reg,
    location = "topleft",
    dist = 1,
    dist_unit = "nm",
    transform = TRUE,
    model = "WGS84",
    anchor = c(x = -66.85, y = 44.91),
    height = 0.002,
    st.dist = 0.003,
    st.size = 3
  ) +
  
  coord_quickmap(xlim = lons, ylim = lats) +
  labs(x = NULL,
       y = NULL,
       fill = "Depth (m)",
       title = "Owen Basin Transects") +
  theme_bw() + theme(panel.grid = element_blank(),
                     plot.title = element_text(size = 14, hjust = 0.5)) +
  facet_wrap( ~ Date) + theme(panel.spacing.x = unit(30, units = "points"))


# Grand Manan Basin latitudes/longitudes
lons = c(-66.58,-66.4)
lats = c(44.66, 44.8)

# Grand Manan Basin station labels
GMB_labels$Track = GMB_cages$Station_Number[!duplicated(GMB_cages$Station_Number)]
colnames(GMB_labels)[1] = "Station_Number"

GMB_Plots = ggplot() +
  
  geom_contour_filled(
    data = bathy,
    aes(x = x, y = y, z = z),
    colour = NA,
    breaks = c(seq(
      from = 0, to = 200, by = 10
    ))
  ) +
  scale_fill_grey(start = 0.9, end = 0.3) +
  
  geom_path(
    data = GMB_Glider_Data,
    aes(x = Longitude, y = Latitude, colour = Date),
    alpha = 1,
    size = 0.5,
    show.legend = FALSE
  ) +
  
  scale_colour_manual(values = c("#F0E442", "#0072B2", "#D55E00")) +
  
  geom_point(
    data = GMB_Glider_Startpoints,
    aes(x = Longitude, y = Latitude),
    fill = c("#F0E442", "#0072B2", "#D55E00"),
    shape = 22,
    alpha = 1
  ) +
  
  geom_point(
    data = GMB_Glider_Endpoints,
    aes(x = Longitude, y = Latitude),
    fill = c("#F0E442", "#0072B2", "#D55E00"),
    shape = 24,
    alpha = 1
  ) +
  
  geom_path(
    data = GMB_tracks,
    aes(x = Lon, y = Lat, group = Track),
    alpha = 1,
    size = 0.4,
    arrow = arrow(length = unit(8, "points"), ends = "last")
  ) +
  
  geom_point(
    data = GMB_cages,
    aes(x = Longitude, y = Latitude),
    fill = "grey",
    shape = 21,
    alpha = 0.7
  ) +
  
  geom_text(
    data = GMB_labels,
    aes(x = Lon, y = Lat, label = Station_Number),
    hjust = "left",
    vjust = "bottom"
  ) +
  
  ggsn::scalebar(
    data = reg,
    location = "topleft",
    dist = 1,
    dist_unit = "nm",
    transform = TRUE,
    model = "WGS84",
    anchor = c(x = -66.58, y = 44.80),
    height = 0.003,
    st.dist = 0.004,
    st.size = 3
  ) +
  
  coord_quickmap(xlim = lons, ylim = lats) +
  labs(x = NULL,
       y = NULL,
       fill = "Depth (m)",
       title = "Grand Manan Basin Transects") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 14, hjust = 0.5)) +
  facet_wrap( ~ Date)



ggarrange(
  OB_Plots,
  GMB_Plots,
  ncol = 1,
  legend = "bottom",
  common.legend = TRUE
)
