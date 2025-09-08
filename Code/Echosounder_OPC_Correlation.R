# Last updated: 24 Oct 2022

#####
setwd("C:/Users/dmossman/Box/2022 MSc Thesis Work/")

## Libraries

library(tidyverse)
library(oce)
library(ocedata)
library(opcr)
library(R.utils)
library(readxl)
library(rstatix)
library(emmeans)
library(modelr)
library(ggpubr)
library(pracma)
library(R.matlab)
library(zoo)

## Project Structure
sourceDirectory('Code/Hansen Zooplankton Code and Sample Data/src',
                modifiedOnly = F)

function_dir = 'Code/Hansen Zooplankton Code and Sample Data/src/'
create_dir(function_dir)
processed_dir = 'Processed_Data/'
create_dir(processed_dir)
data_dir = 'Raw_Data/'
create_dir(data_dir)
figure_dir = 'Visuals/'
create_dir(figure_dir)
report_dir = getwd()
create_dir(report_dir)

#####

## LOADING IN FILES

load(paste0(processed_dir, "OPC_Data_Full.rda"))
load(paste0(processed_dir, "OPC_Data_C5_Estimate.rda"))
Sept19 = readMat(paste0(processed_dir, "19Sept_Differencing_Data.mat"))
Sept20 = readMat(paste0(processed_dir, "20Sept_Differencing_Data.mat"))
Sept21 = readMat(paste0(processed_dir, "21Sept_Differencing_Data.mat"))
Sept24 = readMat(paste0(processed_dir, "24Sept_Differencing_Data.mat"))
Sept25 = readMat(paste0(processed_dir, "25Sept_Differencing_Data.mat"))
Sept26 = readMat(paste0(processed_dir, "26Sept_Differencing_Data.mat"))
Sept21_Seafloor_Corrected = readMat(paste0(processed_dir, "21Sept_Differencing_Data_Seafloor_Corrected.mat"))
Sept2020 = list(Sept19, Sept20, Sept21, Sept24, Sept25, Sept26, Sept21_Seafloor_Corrected)
rm(Sept19, Sept20, Sept21, Sept24, Sept25, Sept26, Sept21_Seafloor_Corrected)
names(Sept2020) = c("Sept19", "Sept20", "Sept21", "Sept24", "Sept25", "Sept26", "Sept21_Seafloor_Corrected")

## Glider times are recorded in UTC
## OPC times are also recorded in UTC

matlab2POS = function(x, timez = "UTC") {
  days = x - 719529 	# 719529 = days from 1-1-0000 to 1-1-1970
  secs = days * 86400 # 86400 seconds in a day
  # This next string of functions is a complete disaster, but it works.
  # It tries to outsmart R by converting the secs value to a POSIXct value
  # in the UTC time zone, then converts that to a time/date string that 
  # should lose the time zone, and then it performs a second as.POSIXct()
  # conversion on the time/date string to get a POSIXct value in the user's 
  # specified timezone. Time zones are a goddamned nightmare.
  date_time = as.POSIXct(strftime(as.POSIXct(secs, origin = '1970-1-1', 
                                             tz = 'UTC'), format = '%Y-%m-%d %H:%M:%S', 
                                  tz = 'UTC', usetz = FALSE), tz = timez)
  return(date_time)
}

for(j in 1:size(Sept2020, 2)) {
  Sept2020[[j]]$"Output"[[1]] = matlab2POS(Sept2020[[j]]$"Output"[[1]])
}


dates = c("19", "20", "21", "24", "25", "26")



## Determine which dive is closest to each good OPC cast

good_casts = c(5, 7, 8, 13, 14, 15, 16, 17, 21, 22, 23, 27:33, 35, 36)

OPC_Data_Good = OPC_Data_Full %>% filter(OPC_cast_num %in% good_casts)
OPC_Start_Times = OPC_Data_Good %>% group_by(OPC_cast_num) %>% summarise(start_time = as.POSIXct(first(time_UTC))) ## these are in UTC
OPC_Start_Times$dive_index = NA

OPC_Start_Times$dive_index[1] = which.min(abs(OPC_Start_Times$start_time[1] - Sept19_Output[[1]]))

OPC_Start_Times$dive_index[2] = which.min(abs(OPC_Start_Times$start_time[2] - Sept20_Output[[1]]))
OPC_Start_Times$dive_index[3] = which.min(abs(OPC_Start_Times$start_time[3] - Sept20_Output[[1]]))

OPC_Start_Times$dive_index[4] = which.min(abs(OPC_Start_Times$start_time[4] - Sept21_Output[[1]]))
OPC_Start_Times$dive_index[5] = which.min(abs(OPC_Start_Times$start_time[5] - Sept21_Output[[1]]))
OPC_Start_Times$dive_index[6] = which.min(abs(OPC_Start_Times$start_time[6] - Sept21_Output[[1]]))
OPC_Start_Times$dive_index[7] = which.min(abs(OPC_Start_Times$start_time[7] - Sept21_Output[[1]]))
OPC_Start_Times$dive_index[8] = which.min(abs(OPC_Start_Times$start_time[8] - Sept21_Output[[1]]))

OPC_Start_Times$dive_index[9] = which.min(abs(OPC_Start_Times$start_time[9] - Sept24_Output[[1]]))
OPC_Start_Times$dive_index[10] = which.min(abs(OPC_Start_Times$start_time[10] - Sept24_Output[[1]]))
OPC_Start_Times$dive_index[11] = which.min(abs(OPC_Start_Times$start_time[11] - Sept24_Output[[1]]))

OPC_Start_Times$dive_index[12] = which.min(abs(OPC_Start_Times$start_time[12] - Sept25_Output[[1]]))
OPC_Start_Times$dive_index[13] = which.min(abs(OPC_Start_Times$start_time[13] - Sept25_Output[[1]]))
OPC_Start_Times$dive_index[14] = which.min(abs(OPC_Start_Times$start_time[14] - Sept25_Output[[1]]))
OPC_Start_Times$dive_index[15] = which.min(abs(OPC_Start_Times$start_time[15] - Sept25_Output[[1]]))
OPC_Start_Times$dive_index[16] = which.min(abs(OPC_Start_Times$start_time[16] - Sept25_Output[[1]]))
OPC_Start_Times$dive_index[17] = which.min(abs(OPC_Start_Times$start_time[17] - Sept25_Output[[1]]))

OPC_Start_Times$dive_index[18] = which.min(abs(OPC_Start_Times$start_time[18] - Sept26_Output[[1]]))
OPC_Start_Times$dive_index[19] = which.min(abs(OPC_Start_Times$start_time[19] - Sept26_Output[[1]]))
OPC_Start_Times$dive_index[20] = which.min(abs(OPC_Start_Times$start_time[20] - Sept26_Output[[1]]))

OPC_Start_Times$dive_number = NA

OPC_Start_Times$dive_number[1] = 7

OPC_Start_Times$dive_number[2] = 2
OPC_Start_Times$dive_number[3] = 4

OPC_Start_Times$dive_number[4] = 2
OPC_Start_Times$dive_number[5] = 3
OPC_Start_Times$dive_number[6] = 4
OPC_Start_Times$dive_number[7] = 6
OPC_Start_Times$dive_number[8] = 7

OPC_Start_Times$dive_number[9] = 4
OPC_Start_Times$dive_number[10] = 5
OPC_Start_Times$dive_number[11] = 6

OPC_Start_Times$dive_number[12] = 2
OPC_Start_Times$dive_number[13] = 3
OPC_Start_Times$dive_number[14] = 4
OPC_Start_Times$dive_number[15] = 5
OPC_Start_Times$dive_number[16] = 6
OPC_Start_Times$dive_number[17] = 7

OPC_Start_Times$dive_number[18] = 2
OPC_Start_Times$dive_number[19] = 4
OPC_Start_Times$dive_number[20] = 6

save(OPC_Start_Times, file = paste0(processed_dir, "OPC_Start_Times.rda"))


## Make depth profiles for dive data

load(paste0(processed_dir, "OPC_Start_Times.rda"))

for(j in 1:size(Sept2020, 2)) {
  for(k in 1:length(Sept2020[[j]]$DiveDiff)) {
    tmp = as.data.frame(colMeans(10 * log10(Sept2020[[j]]$DiveDiff[[k]][[3]]), na.rm = T))
    colnames(tmp)  ="Avg_Sv"
    tmp$Depth = 5:200
    
    plot1 = ggplot(tmp) +
      geom_path(aes(x = Avg_Sv, y = Depth)) +
      geom_path(aes(x = rollapply(Avg_Sv, FUN = mean, width = 10, partial = T, align = "center"),
                                  y = Depth), 
                color = "red",
                show.legend = FALSE) +
      coord_cartesian(expand = F, ylim = c(200,0)) +
      scale_y_reverse(breaks = c(0,20,40,60,80,100,120,140,160,180,200)) + 
      scale_x_continuous(limits = c(5,20), position = "top") +
      theme_bw() +
      labs(
        x = 'Difference in 200 kHz and\n455 kHz Sv (decibels)',
        y = NULL,
        fill = NULL,
        color = NULL
      ) +
      ggtitle(paste0("Sept ", dates[j], ", Dive ", as.character(k))) +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
    
    tmp2 = Sept2020[[j]]$DiveDiff[[k]]
    
    Sept2020[[j]]$DiveDiff[[k]] = c(tmp2,list(plot1))
    
    rm(tmp)
    rm(plot1)
      
  }
}

save(Sept2020, file = paste0(processed_dir, "Sept2020_R_Echosounder_Data.rda"))

## Generating OPC depth profiles

load(paste0(processed_dir, "Sept2020_R_Echosounder_Data.rda"))

good_casts = c(5, 7, 8, 13, 14, 15, 16, 17, 21, 22, 23, 27:33, 35, 36)

ca_prf = prf %>% 
  filter(OPC_cast_num %in% good_casts) %>% 
  group_by(OPC_cast_num, depth) %>%
  summarise(
    m = median(concentration, na.rm = T),
    C5m = median(C5_Estimate, na.rm = T),
    .groups = 'drop'
  )

ca_prf$OPC_cast_num = as.factor(ca_prf$OPC_cast_num)

OPC_Profiles = list()

for(i in seq_along(levels(ca_prf$OPC_cast_num))){
  
  cst = levels(ca_prf$OPC_cast_num)[i]
  
  tmp = subset(ca_prf, ca_prf$OPC_cast_num == cst)
  tmp = droplevels(tmp)

  tmp_plot = ggplot() +
    geom_path(data = tmp, aes(x = C5m, y = depth)) +
    scale_y_reverse(breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200)) +
    scale_x_continuous(limits = c(0,1000), position = "top") +
    coord_cartesian(expand = FALSE, ylim = c(200,0)) +
    labs(
      x = expression(paste(
        "Estimated C5 Abundance\n(individuals/", m ^ 3, ')'
      )),
      y = 'Depth (m)',
      fill = NULL,
      color = NULL
    ) +
    theme_bw() +
    ggtitle(paste0("OPC", sprintf("%03s", as.character(tmp$OPC_cast_num[i])))) +
    theme(plot.title = element_text(hjust = 0.5))
  
  OPC_Profiles = c(OPC_Profiles, list(assign(paste0("OPC", sprintf("%03s", as.character(tmp$OPC_cast_num[i])),"_Profile"), tmp_plot)))
  rm(list = paste0("OPC", sprintf("%03s", as.character(tmp$OPC_cast_num[i])),"_Profile"))
  rm(tmp)
  rm(tmp_plot)
}

## Testing marine snow hypothesis
dz = 1
min_size = 0.5
max_size = 1.5


prf2 = OPC_Data_Full %>%
  filter(depth>1) %>%
  filter(flag != 'depth') %>%
  group_by(OPC_cast_num, basin) %>%
  summarise(
    opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = F, reject_volume = F),
    .groups = 'drop'
  )

ca_prf2 = prf2 %>% 
  filter(OPC_cast_num %in% good_casts) %>% 
  group_by(OPC_cast_num, depth) %>%
  summarise(
    m = median(concentration, na.rm = T),
    .groups = 'drop'
  )
ca_prf2$OPC_cast_num = as.factor(ca_prf2$OPC_cast_num)


for (i in seq_along(levels(ca_prf2$OPC_cast_num))) {
  cst = levels(ca_prf$OPC_cast_num)[i]
  
  tmp = subset(ca_prf2, ca_prf2$OPC_cast_num == cst)
  tmp = droplevels(tmp)
  
  tmp_plot = ggplot() +
    geom_path(data = tmp, aes(x = m, y = depth)) +
    scale_y_reverse(breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200)) +
    scale_x_continuous(limits = c(0, 10000), position = "top") +
    coord_cartesian(expand = FALSE, ylim = c(200, 0)) +
    labs(
      x = expression(paste(
        "0.5-1.5 mm ESD Particle\nAbundance (#/", m ^ 3, ')'
      )),
      y = NULL,
      fill = NULL,
      color = NULL
    ) +
    theme_bw() +
    ggtitle(paste0("OPC", sprintf(
      "%03s", as.character(tmp$OPC_cast_num[i])
    ))) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  
  OPC_Profiles[[i]] = c(list(OPC_Profiles[[i]]), list(assign(
    paste0("OPC", sprintf(
      "%03s", as.character(tmp$OPC_cast_num[i])
    ), "_Profile"), tmp_plot
  )))
  rm(list = paste0("OPC", sprintf(
    "%03s", as.character(tmp$OPC_cast_num[i])
  ), "_Profile"))
  rm(tmp)
  rm(tmp_plot)
}


save(OPC_Profiles, file = paste0(processed_dir, "OPC_Profiles.rda"))

## Side by side profiles

load(paste0(processed_dir, "OPC_Start_Times.rda"))
load(paste0(processed_dir, "OPC_Profiles.rda"))
load(paste0(processed_dir, "Sept2020_R_Echosounder_Data.rda"))

OPC_Start_Times$start_time = c(1,
                               2,2,
                               3,3,3,3,3,
                               4,4,4,
                               5,5,5,5,5,5,
                               6,6,6)

for(i in 1:length(OPC_Profiles)) {
  plot2 = ggarrange(OPC_Profiles[[i]],
    # OPC_Profiles[[i]][[2]],
    Sept2020[[OPC_Start_Times$start_time[i]]]$DiveDiff[[OPC_Start_Times$dive_number[i]]][[7]],
    ncol = 2)
  
  file = paste0("OPC", sprintf("%03s", as.character(OPC_Start_Times$OPC_cast_num[i])),"_Comparison_Profile.png")
  ggsave(filename = file, path = paste0(figure_dir, '/OPC_Comparisons'), width = 8, units = "in")
}

## Getting average abundance for 21st of September only

Sept21_Casts = c(13, 14, 15, 16, 17)

da_prf = prf %>% 
  filter(OPC_cast_num %in% Sept21_Casts) %>% 
  group_by(depth) %>%
  summarise(
    m = mean(concentration, na.rm = T),
    C5m = mean(C5_Estimate, na.rm = T),
    .groups = 'drop'
  )

ggplot() +
  geom_path(data = da_prf, aes(x = C5m, y = depth)) +
  scale_y_reverse(breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200)) +
  scale_x_continuous(limits = c(0,1000), position = "top") +
  coord_cartesian(expand = FALSE, ylim = c(200,0)) +
  labs(
    x = expression(paste(
      "Estimated C5 Abundance\n(individuals/", m ^ 3, ')'
    )),
    y = 'Depth (m)',
    fill = NULL,
    color = NULL
  ) +
  theme_bw() +
  ggtitle("Mean Abundance for OPC Casts on September 21") +
  theme(plot.title = element_text(hjust = 0.5))

file = "21Sept_OPC_Casts.png"
ggsave(filename = file, path = figure_dir, width = 5, units = "in")

# Question mark: correlating Sv (forward problem) or biomass (inverse problem)?
## Kim's initial thought is to do the inverse problem
# Question mark: what depth intervals to use? Multinet was integrated over net depths, but OPC and echosounder
# are both finer scale

# Start with vertical profiles of 455-200 for each OPC cast to look at raw data (Sv for echosounder,
# abundance estimate for OPC)
