# Set working directory and libraries
setwd("~/Documents/BoF2020_Cruise")
library(tidyverse)
library(geosphere)

# Import and format glider data
Glider_Data = read_csv("Raw_Data/Glider/Glider_Instruments.csv")
Glider_Data = Glider_Data[, c(1, 4, 3)]
colnames(Glider_Data) = c("Date", "Longitude", "Latitude")

Glider_Data$Date = as.character(Glider_Data$Date)
Glider_Data[which(startsWith(Glider_Data$Date, "2020-09-19")), 1] = "19-Sep"
Glider_Data[which(startsWith(Glider_Data$Date, "2020-09-20")), 1] = "20-Sep"
Glider_Data[which(startsWith(Glider_Data$Date, "2020-09-21")), 1] = "21-Sep"
Glider_Data[which(startsWith(Glider_Data$Date, "2020-09-24")), 1] = "24-Sep"
Glider_Data[which(startsWith(Glider_Data$Date, "2020-09-25")), 1] = "25-Sep"
Glider_Data[which(startsWith(Glider_Data$Date, "2020-09-26")), 1] = "26-Sep"

# Parse glider data by date
GD_19 = Glider_Data[which(Glider_Data$Date == "19-Sep"),]
GD_20 = Glider_Data[which(Glider_Data$Date == "20-Sep"),]
GD_21 = Glider_Data[which(Glider_Data$Date == "21-Sep"),]
GD_24 = Glider_Data[which(Glider_Data$Date == "24-Sep"),]
GD_25 = Glider_Data[which(Glider_Data$Date == "25-Sep"),]
GD_26 = Glider_Data[which(Glider_Data$Date == "26-Sep"),]

# Get lengths of glider tracks by date in nm

dist_19 = sum(distHaversine(GD_19[,2:3]))/1852
dist_20 = sum(distHaversine(GD_20[,2:3]))/1852
dist_21 = sum(distHaversine(GD_21[,2:3]))/1852
dist_24 = sum(distHaversine(GD_24[,2:3]))/1852
dist_25 = sum(distHaversine(GD_25[,2:3]))/1852
dist_26 = sum(distHaversine(GD_26[,2:3]))/1852

# Average track length in nm

mean_dist = sum(dist_19,dist_20,dist_21,dist_24,dist_25,dist_26)/6
