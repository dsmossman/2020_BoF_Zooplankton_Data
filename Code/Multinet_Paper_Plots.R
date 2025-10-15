rm(list=ls())
setwd("C:/Users/Delphine/Documents/2020_BoF_Zooplankton_Data")

## Libraries

library(tidyverse)
library(oce)
library(ocedata)
library(R.utils)
library(readxl)
library(ggpubr)
library(pracma)
library(R.matlab)
library(rstatix)
library(opcr)
library(zoo)
library(ggbreak)
library(grid)

## Directories

processed_dir = 'Processed_Data/'
data_dir = 'Raw_Data/'
figure_dir = 'Visuals/'
report_dir = getwd()

## Load in data structures created by Multinet_Abundance_Processing.R

load(paste0(processed_dir, 'Multi_Data.rda'))
load(paste0(processed_dir, 'Net_Metadata.rda'))
load(paste0(processed_dir, 'Net_Data.rda'))
load(paste0(processed_dir, 'Abundance_Data_Concentration.rda'))
load(paste0(processed_dir, 'Abundance_Data_Biomass.rda'))
load(paste0(processed_dir, "Cfin_Stages.rda"))
load(paste0(processed_dir, 'OPC_Data_Full.rda'))

## Edit the opc_speed function to remove the averaging part of instantaneous calculation
trace(opc_speed,edit=T)

## Edit the opc_abundance function to include speed calculation
## Uncomment below and copy, then recomment
# 
# function (df, dz = 2, min_size = 1, max_size = 4, good_only = TRUE,
#           reject_volume = TRUE)
# {
#   if (good_only) {
#     df = dplyr::filter(df, flag == 0)
#   }
#   opc_check(df)
#   df$zbin = bin(df$depth, d = dz)
#   vol = df %>% group_by(zbin, .drop = FALSE) %>% dplyr::summarize(v = sum(volume_filtered),
#                                                                   .groups = "drop")
#   cnt = df %>% unnest_longer(esd) %>% dplyr::filter(esd >=
#                                                       min_size & esd <= max_size) %>% group_by(zbin, .drop = FALSE) %>%
#     dplyr::summarize(c = n(), .groups = "drop")
#   spd = df %>% group_by(zbin, .drop = F) %>% dplyr::summarize(s = mean(speed),
#                                                               .groups = "drop")
#   out = full_join(cnt, vol, by = "zbin") %>% full_join(.,
#                                                        spd, by = "zbin") %>% transmute(depth = relabel(zbin),
#                                                                                        speed = s, count = c, volume = v, concentration = c/v)
#   out$concentration[is.infinite(out$concentration)] = NA
#   if (reject_volume) {
#     out = drop_volume(df = out, dz = dz)
#   }
#   return(out)
# }


# Paste function into this window
trace(opc_abundance,edit=T)

#####

# Read in Matlab integration files

# Top level of Integration list is tow (2-3 tows per day)

# Within each tow, [[1]] is Sv in linear space, [[2]] is SvDiff in log space,
# and [[3]] is nets; first three are 130kHz(-200kHz), next three are 200kHz(-455kHz), etc

# Within each net, [[1]] is Sv, [[2]] is SvDiff, and [[3]] is MaskedSv; first
# three are 130kHz(-200kHz), next three are 200kHz(-455kHz), etc

# Vector of dates
dates = c("19", "20", "21", "24", "25", "26")

# Load in each day
for (i in 1:length(dates)) {
  filename = paste0(
    "/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Processed_Data/",
    dates[i],
    "Sept_Masked_Data.mat"
  )
  assign(paste0("Sept", dates[i], "_Integration"),
         readMat(filename)$Integration)
  rm(filename)
}

# Human-readable names for sublists

DFs = sort(grep("Sept[0-9]{2}_Integration",names(.GlobalEnv),value=TRUE))

sublist = rep(c("FullSv", "SvDiff", "Net"), times = 5)
for (i in 1:length(sublist)) {
  sublist[i] = paste0(sublist[i], ceiling(i / 3))
}

subsublist = rep(c("NetSv", "NetSvDiff", "NetMaskedSv"), times = 4)
for (i in 1:length(subsublist)) {
  subsublist[i] = paste0(subsublist[i], ceiling(i / 3))
}

for (i in 1:length(DFs)) {
  temp = get(DFs[i])
  for (j in 1:length(temp)) {
    names(temp)[j] = paste0(rownames(temp), j)
    
    for (k in 1:length(temp[[j]])) {
      names(temp[[j]])[k] = sublist[k]
      if (k %% 3 == 0) {
        for (m in 1:length(temp[[j]][[k]])) {
          names(temp[[j]][[k]])[m] = subsublist[m]
        }
      }
    }
  }
  assign(DFs[i], temp)
  rm(temp)
}
#####

## Super mega ultra final truth data (final) (FINAL) (2) (FINAL)

# Start with the GMB station data

stations = c(8, 12, 15)
OPC_casts = list(c(13, 14),c(21,22),c(27,28))
# constants for OPC calculation
dz = 1
min_size = 1.5
max_size = 2

# calculation of OPC particle abundance to C5 concentration
OPC_to_C5 = function(concentration, speed) {
  B0 = 0.0384
  B1 = 0.5343
  B2 = 0.8001
  
  temp = (1/B1) * (log10(concentration) - B0 - B2 * speed)
  return(10^temp)
  
}

GMB_Deep_Net_OPC_Comparison = list()

for(i in 1:3) { # for each station
  # get the stage C5 cfin concentration in the nets
  temp1 = stages %>% filter(station == stations[i] & stage %in% c("CV")) %>%
    group_by(net, stage) %>%
    reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))

  # round net depth intervals to the nearest 5
  require(plyr)
  temp1$Intervals = Net_Metadata[Net_Metadata$Station_Number == stations[i],] %>% 
    group_by(Net_Num) %>%
    reframe(depth = round_any(mean(`Pressure_(dbar)`), 5, f = ceiling)) %>% slice(c(1:5))
  detach("package:plyr")

  # group by the new depth intervals and sum the concentration
  temp1 = temp1 %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))

  # get the OPC data for the station and calculate the C5 concentration
  temp2 = OPC_Data_Full %>% filter(OPC_cast_num %in% OPC_casts[[i]]) %>%
    filter(depth>1) %>% 
    ungroup() %>%
    reframe(opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size),
            C5_concentration = OPC_to_C5(concentration, speed))

  # setting up the final data frame in 1 m depth bins
  temp3 = data.frame(Depth_Bin = seq(0,199,1), Net_Concentration = NA, OPC_Concentration = NA)

  # assign the OPC concentrations
  for(j in 1:nrow(temp2)) {
    temp3$OPC_Concentration[temp3$Depth_Bin == temp2$depth[j]] = temp2$C5_concentration[j]
  }

  # assign the net concentrations
  for(k in 1:nrow(temp1)) {
    if(k == 1) {
      temp3$Net_Concentration[temp3$Depth_Bin <= temp1$`Intervals$depth`[k]] = temp1$cfin_concentration[k]
    } else {
      temp3$Net_Concentration[(temp3$Depth_Bin <= temp1$`Intervals$depth`[k] &
                                                     temp3$Depth_Bin >= temp1$`Intervals$depth`[k-1])] = temp1$cfin_concentration[k]
    }
  }

  # transform the dataframe into 5-m depth intervals and stick it in the list
  GMB_Deep_Net_OPC_Comparison[[i]] = temp3 %>% 
    transform(Depth_Bin=cut(Depth_Bin, seq(0,200,5),right=F)) %>% 
    group_by(Depth_Bin) %>% 
    summarise(Net_Concentration = mean(Net_Concentration), OPC_Concentration = mean(OPC_Concentration, na.rm=T))
  
}

# per-station plots
station8_plot = ggplot(data=GMB_Deep_Net_OPC_Comparison[[1]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,6000)) +
  scale_y_break(breaks = c(1999,2000),scales=0.25,ticklabels=seq(3000,6000,3000)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),         axis.ticks.x.top = element_blank(),         axis.text.x.top = element_blank()) +
  labs(y="Concentration (individuals/m^3)",x="Depth (m)")
ggsave(file=paste0(figure_dir,"Net_OPC_Depth_Profiles/","Station_8_Net_OPC_Comparison.png"),
       scale=2)
station12_plot = ggplot(data=GMB_Deep_Net_OPC_Comparison[[2]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,6000)) +
  scale_y_break(breaks = c(1999,2000),scales=0.25,ticklabels=seq(3000,6000,3000)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),         axis.ticks.x.top = element_blank(),         axis.text.x.top = element_blank()) +
  labs(y="Concentration (individuals/m^3)",x="Depth (m)")
ggsave(file=paste0(figure_dir,"Net_OPC_Depth_Profiles/","Station_12_Net_OPC_Comparison.png"),
       scale=2)
station15_plot = ggplot(data=GMB_Deep_Net_OPC_Comparison[[3]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,6000)) +
  scale_y_break(breaks = c(1999,2000),scales=0.25,ticklabels=seq(3000,6000,3000)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),         axis.ticks.x.top = element_blank(),         axis.text.x.top = element_blank()) +
  labs(y="Concentration (individuals/m^3)",x="Depth (m)")
ggsave(file=paste0(figure_dir,"Net_OPC_Depth_Profiles/","Station_15_Net_OPC_Comparison.png"),
       scale=2)


stations=c(9, 16)
OPC_casts = list(c(15,16),c(29,30))

GMB_Mid_Net_OPC_Comparison = list()

for(i in 1:2) {
  temp1 = stages %>% filter(station == stations[i] & stage %in% c("CV")) %>%
    group_by(net, stage) %>%
    reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))
  
  require(plyr)
  temp1$Intervals = Net_Metadata[Net_Metadata$Station_Number == stations[i],] %>% 
    group_by(Net_Num) %>%
    reframe(depth = round_any(mean(`Pressure_(dbar)`), 5, f = ceiling)) %>% slice(c(1:5))
  detach("package:plyr")
  
  temp1 = temp1 %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))
  
  temp2 = OPC_Data_Full %>% filter(OPC_cast_num %in% OPC_casts[[i]]) %>%
    filter(depth>1) %>% 
    ungroup() %>%
    reframe(opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size, reject_volume = T),
            C5_concentration = OPC_to_C5(concentration, speed))
  
  temp3 = data.frame(Depth_Bin = seq(0,199,1), Net_Concentration = NA, OPC_Concentration = NA)
  
  for(j in 1:nrow(temp2)) {
    temp3$OPC_Concentration[temp3$Depth_Bin == temp2$depth[j]] = temp2$C5_concentration[j]
  }
  
  for(k in 1:nrow(temp1)) {
    if(k == 1) {
      temp3$Net_Concentration[temp3$Depth_Bin <= temp1$`Intervals$depth`[k]] = temp1$cfin_concentration[k]
    } else {
      temp3$Net_Concentration[(temp3$Depth_Bin <= temp1$`Intervals$depth`[k] &
                                 temp3$Depth_Bin >= temp1$`Intervals$depth`[k-1])] = temp1$cfin_concentration[k]
    }
  }
  
  GMB_Mid_Net_OPC_Comparison[[i]] = temp3 %>% 
    transform(Depth_Bin=cut(Depth_Bin, seq(0,200,5),right=F)) %>% 
    group_by(Depth_Bin) %>% 
    summarise(Net_Concentration = mean(Net_Concentration), OPC_Concentration = mean(OPC_Concentration, na.rm=T))
  
}

station9_plot = ggplot(data=GMB_Mid_Net_OPC_Comparison[[1]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,6000)) +
  scale_y_break(breaks = c(1999,2000),scales=0.25,ticklabels=seq(3000,6000,3000)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),         axis.ticks.x.top = element_blank(),         axis.text.x.top = element_blank()) +
  labs(y="Concentration (individuals/m^3)",x="Depth (m)")
ggsave(file=paste0(figure_dir,"Net_OPC_Depth_Profiles/","Station_9_Net_OPC_Comparison.png"),
       scale=2)
station16_plot = ggplot(data=GMB_Mid_Net_OPC_Comparison[[2]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,6000)) +
  scale_y_break(breaks = c(1999,2000),scales=0.25,ticklabels=seq(3000,6000,3000)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),         axis.ticks.x.top = element_blank(),         axis.text.x.top = element_blank()) +
  labs(y="Concentration (individuals/m^3)",x="Depth (m)")
ggsave(file=paste0(figure_dir,"Net_OPC_Depth_Profiles/","Station_16_Net_OPC_Comparison.png"),
       scale=2)


stations=c(10, 13, 17)
OPC_casts=list(c(17),c(23),c(31,32))

GMB_Shallow_Net_OPC_Comparison = list()

for(i in 1:3) {
  temp1 = stages %>% filter(station == stations[i] & stage %in% c("CV")) %>%
    group_by(net, stage) %>%
    reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))
  
  require(plyr)
  temp1$Intervals = Net_Metadata[Net_Metadata$Station_Number == stations[i],] %>% 
    group_by(Net_Num) %>%
    reframe(depth = round_any(mean(`Pressure_(dbar)`), 5, f = ceiling)) %>% slice(c(1:5))
  detach("package:plyr")
  
  temp1 = temp1 %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))
  
  temp2 = OPC_Data_Full %>% filter(OPC_cast_num %in% OPC_casts[[i]]) %>%
    filter(depth>1) %>% 
    ungroup() %>%
    reframe(opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size, reject_volume = T),
            C5_concentration = OPC_to_C5(concentration, speed))
  
  temp3 = data.frame(Depth_Bin = seq(0,199,1), Net_Concentration = NA, OPC_Concentration = NA)
  
  for(j in 1:nrow(temp2)) {
    temp3$OPC_Concentration[temp3$Depth_Bin == temp2$depth[j]] = temp2$C5_concentration[j]
  }
  
  for(k in 1:nrow(temp1)) {
    if(k == 1) {
      temp3$Net_Concentration[temp3$Depth_Bin <= temp1$`Intervals$depth`[k]] = temp1$cfin_concentration[k]
    } else {
      temp3$Net_Concentration[(temp3$Depth_Bin <= temp1$`Intervals$depth`[k] &
                                 temp3$Depth_Bin >= temp1$`Intervals$depth`[k-1])] = temp1$cfin_concentration[k]
    }
  }
  
  GMB_Shallow_Net_OPC_Comparison[[i]] = temp3 %>% 
    transform(Depth_Bin=cut(Depth_Bin, seq(0,200,5),right=F)) %>% 
    group_by(Depth_Bin) %>% 
    summarise(Net_Concentration = mean(Net_Concentration), OPC_Concentration = mean(OPC_Concentration, na.rm=T))
  
}

station10_plot = ggplot(data=GMB_Shallow_Net_OPC_Comparison[[1]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,6000)) +
  scale_y_break(breaks = c(1999,2000),scales=0.25,ticklabels=seq(3000,6000,3000)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),         axis.ticks.x.top = element_blank(),         axis.text.x.top = element_blank()) +
  labs(y="Concentration (individuals/m^3)",x="Depth (m)")
ggsave(file=paste0(figure_dir,"Net_OPC_Depth_Profiles/","Station_10_Net_OPC_Comparison.png"),
       scale=2)
station13_plot = ggplot(data=GMB_Shallow_Net_OPC_Comparison[[2]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,6000)) +
  scale_y_break(breaks = c(1999,2000),scales=0.25,ticklabels=seq(3000,6000,3000)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),         axis.ticks.x.top = element_blank(),         axis.text.x.top = element_blank()) +
  labs(y="Concentration (individuals/m^3)",x="Depth (m)")
ggsave(file=paste0(figure_dir,"Net_OPC_Depth_Profiles/","Station_13_Net_OPC_Comparison.png"),
       scale=2)
station17_plot = ggplot(data=GMB_Shallow_Net_OPC_Comparison[[3]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,6000)) +
  scale_y_break(breaks = c(1999,2000),scales=0.25,ticklabels=seq(3000,6000,3000)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.text.x.top = element_blank()) +
  labs(y="Concentration (individuals/m^3)",x="Depth (m)")
ggsave(file=paste0(figure_dir,"Net_OPC_Depth_Profiles/","Station_17_Net_OPC_Comparison.png"),
       scale=2)


GMB_Plots = ggarrange(plotlist=list(print(station8_plot),print(station9_plot),print(station10_plot),
                        print(station12_plot),print(station16_plot),print(station13_plot),
                        print(station15_plot),NA,print(station17_plot)),ncol=3,nrow=3,
                      labels=c("Deep","Mid","Shallow"),vjust=1)
fname=paste0(figure_dir,"GMB_All_Stations_Comparison.png")
ggsave(file=fname,scale=3)

OPC_Data_Full %>% filter(OPC_cast_num %in% c(13,14,21,22,27,28,15,16,29,30,17,23,31,32)) %>%
  filter(depth>1) %>% 
  ungroup() %>%
  reframe(opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size),
          C5_concentration = OPC_to_C5(concentration, speed)) %>%
  transform(depth=cut(depth, seq(0,200,5),right=F)) %>% 
  group_by(depth) %>% 
  summarise(C5_concentration = mean(C5_concentration, na.rm=T)) %>%
  ungroup() %>%
  filter(row_number() >= 17) %>%
  select(2) %>%
  summarise(formatC(sd(as.matrix(.),na.rm=T),digits=5))

#####

## OB station data

stations = c(4, 6, 19)
OPC_casts = list(c(5),c(),c(35,36))

OB_Deep_Net_OPC_Comparison = list()

for(i in 1:3) {
  temp1 = stages %>% filter(station == stations[i] & stage %in% c("CV")) %>%
    group_by(net, stage) %>%
    reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))
  if(i != 3) { # below is to complete the dataframe (should probably think about reconfiguring to use the complete() function...)
  temp1 = rbind(temp1, data.frame(net=c(4),
                                  stage=c("CV"),
                                  cfin_biomass=0,
                                  cfin_concentration=0))
  temp1 = temp1[order(temp1$net),]
  } else {
    temp1 = rbind(temp1, data.frame(net=c(4),
                                    stage=c("CV"),
                                    cfin_biomass=0,
                                    cfin_concentration=0),
                  data.frame(net=c(5),
                             stage=c("CV"),
                             cfin_biomass=0,
                             cfin_concentration=0))
    temp1 = temp1[order(temp1$net),]
  }
  
  require(plyr)
  temp1$Intervals = Net_Metadata[Net_Metadata$Station_Number == stations[i],] %>% 
    group_by(Net_Num) %>%
    reframe(depth = round_any(mean(`Pressure_(dbar)`), 5, f = ceiling)) %>% slice(c(1:5))
  detach("package:plyr")
  
  temp1 = temp1 %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))
  
  temp2 = OPC_Data_Full %>% filter(OPC_cast_num %in% OPC_casts[[i]]) %>%
    filter(depth>1) %>% 
    ungroup()
  
  
  if(nrow(temp2) == 0) {
    temp2 = data.frame(C5_concentration = rep(0,times=200))
  } else {
      temp2 = temp2 %>%
        reframe(opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size, reject_volume = T),
                C5_concentration = OPC_to_C5(concentration, speed))
    }
  
  temp3 = data.frame(Depth_Bin = seq(0,199,1), Net_Concentration = NA, OPC_Concentration = NA)
  
  for(j in 1:nrow(temp2)) {
    temp3$OPC_Concentration[temp3$Depth_Bin == temp2$depth[j]] = temp2$C5_concentration[j]
  }
  
  for(k in 1:nrow(temp1)) {
    if(k == 1) {
      temp3$Net_Concentration[temp3$Depth_Bin <= temp1$`Intervals$depth`[k]] = temp1$cfin_concentration[k]
    } else {
      temp3$Net_Concentration[(temp3$Depth_Bin <= temp1$`Intervals$depth`[k] &
                                 temp3$Depth_Bin >= temp1$`Intervals$depth`[k-1])] = temp1$cfin_concentration[k]
    }
  }
  
  OB_Deep_Net_OPC_Comparison[[i]] = temp3 %>% 
    transform(Depth_Bin=cut(Depth_Bin, seq(0,200,5),right=F)) %>% 
    group_by(Depth_Bin) %>% 
    summarise(Net_Concentration = mean(Net_Concentration), OPC_Concentration = mean(OPC_Concentration, na.rm=T))
  
}

station4_plot = ggplot(data=OB_Deep_Net_OPC_Comparison[[1]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,6000)) +
  scale_y_break(breaks = c(1999,2000),scales=0.25,ticklabels=seq(3000,6000,3000)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.text.x.top = element_blank()) +
  labs(y="Concentration (individuals/m^3)",x="Depth (m)")
ggsave(file=paste0(figure_dir,"Net_OPC_Depth_Profiles/","Station_4_Net_OPC_Comparison.png"),
       scale=2)
station6_plot = ggplot(data=OB_Deep_Net_OPC_Comparison[[2]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,6000)) +
  scale_y_break(breaks = c(1999,2000),scales=0.25,ticklabels=seq(3000,6000,3000)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.text.x.top = element_blank()) +
  labs(y="Concentration (individuals/m^3)",x="Depth (m)")
ggsave(file=paste0(figure_dir,"Net_OPC_Depth_Profiles/","Station_6_Net_OPC_Comparison.png"),
       scale=2)
station19_plot = ggplot(data=OB_Deep_Net_OPC_Comparison[[3]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,6000)) +
  scale_y_break(breaks = c(1999,2000),scales=0.25,ticklabels=seq(3000,6000,3000)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.text.x.top = element_blank()) +
  labs(y="Concentration (individuals/m^3)",x="Depth (m)")
ggsave(file=paste0(figure_dir,"Net_OPC_Depth_Profiles/","Station_19_Net_OPC_Comparison.png"),
       scale=2)


stations=c(3, 7, 18)
OPC_casts = list(c(),c(),c(33))

OB_Mid_Net_OPC_Comparison = list()

for(i in 1:3) {
  temp1 = stages %>% filter(station == stations[i] & stage %in% c("CV")) %>%
    group_by(net, stage) %>%
    reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))
  
  if(i == 2) {
    temp1 = rbind(temp1, data.frame(net=c(3),
                                    stage=c("CV"),
                                    cfin_biomass=0,
                                    cfin_concentration=0),
                  data.frame(net=c(4),
                             stage=c("CV"),
                             cfin_biomass=0,
                             cfin_concentration=0))
    temp1 = temp1[order(temp1$net),]
  } else if(i == 3) {
    temp1 = rbind(temp1, data.frame(net=c(5),
                                    stage=c("CV"),
                                    cfin_biomass=0,
                                    cfin_concentration=0))
    temp1 = temp1[order(temp1$net),]
  }
  
  require(plyr)
  temp1$Intervals = Net_Metadata[Net_Metadata$Station_Number == stations[i],] %>% 
    group_by(Net_Num) %>%
    reframe(depth = round_any(mean(`Pressure_(dbar)`), 5, f = ceiling)) %>% slice(c(1:5))
  detach("package:plyr")
  
  temp1 = temp1 %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))
  
  temp2 = OPC_Data_Full %>% filter(OPC_cast_num %in% OPC_casts[[i]]) %>%
    filter(depth>1) %>%
    filter(flag != 'depth') %>%
    ungroup()
  
  
  if(nrow(temp2) == 0) {
    temp2 = data.frame(C5_concentration = rep(0,times=200))
  } else {
    temp2 = temp2 %>%
      reframe(opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size, reject_volume = T),
              C5_concentration = OPC_to_C5(concentration, speed))
  }
  
  temp3 = data.frame(Depth_Bin = seq(0,199,1), Net_Concentration = NA, OPC_Concentration = NA)
  
  for(j in 1:nrow(temp2)) {
    temp3$OPC_Concentration[temp3$Depth_Bin == temp2$depth[j]] = temp2$C5_concentration[j]
  }
  
  for(k in 1:nrow(temp1)) {
    if(k == 1) {
      temp3$Net_Concentration[temp3$Depth_Bin <= temp1$`Intervals$depth`[k]] = temp1$cfin_concentration[k]
    } else {
      temp3$Net_Concentration[(temp3$Depth_Bin <= temp1$`Intervals$depth`[k] &
                                 temp3$Depth_Bin >= temp1$`Intervals$depth`[k-1])] = temp1$cfin_concentration[k]
    }
  }
  
  OB_Mid_Net_OPC_Comparison[[i]] = temp3 %>% 
    transform(Depth_Bin=cut(Depth_Bin, seq(0,200,5),right=F)) %>% 
    group_by(Depth_Bin) %>% 
    summarise(Net_Concentration = mean(Net_Concentration), OPC_Concentration = mean(OPC_Concentration, na.rm=T))
  
}

station3_plot = ggplot(data=OB_Mid_Net_OPC_Comparison[[1]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,6000)) +
  scale_y_break(breaks = c(1999,2000),scales=0.25,ticklabels=seq(3000,6000,3000)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),         axis.ticks.x.top = element_blank(),         axis.text.x.top = element_blank()) +
  labs(y="Concentration (individuals/m^3)",x="Depth (m)")
ggsave(file=paste0(figure_dir,"Net_OPC_Depth_Profiles/","Station_3_Net_OPC_Comparison.png"),
       scale=2)
station7_plot = ggplot(data=OB_Mid_Net_OPC_Comparison[[2]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,6000)) +
  scale_y_break(breaks = c(1999,2000),scales=0.25,ticklabels=seq(3000,6000,3000)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),         axis.ticks.x.top = element_blank(),         axis.text.x.top = element_blank()) +
  labs(y="Concentration (individuals/m^3)",x="Depth (m)")
ggsave(file=paste0(figure_dir,"Net_OPC_Depth_Profiles/","Station_7_Net_OPC_Comparison.png"),
       scale=2)
station18_plot = ggplot(data=OB_Mid_Net_OPC_Comparison[[3]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,6000)) +
  scale_y_break(breaks = c(1999,2000),scales=0.25,ticklabels=seq(3000,6000,3000)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),         axis.ticks.x.top = element_blank(),         axis.text.x.top = element_blank()) +
  labs(y="Concentration (individuals/m^3)",x="Depth (m)")
ggsave(file=paste0(figure_dir,"Net_OPC_Depth_Profiles/","Station_18_Net_OPC_Comparison.png"),
       scale=2)


stations=c(5, 20)
OPC_casts=list(c(7,8),c())

OB_Shallow_Net_OPC_Comparison = list()

for(i in 1:2) {
  temp1 = stages %>% filter(station == stations[i] & stage %in% c("CV")) %>%
    group_by(net, stage) %>%
    reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))
  
  if(i == 1) {
    temp1 = rbind(temp1, data.frame(net=c(4),
                                    stage=c("CV"),
                                    cfin_biomass=0,
                                    cfin_concentration=0))
    temp1 = temp1[order(temp1$net),]
  } else if(i == 2) {
    temp1 = rbind(temp1, data.frame(net=c(4),
                                    stage=c("CV"),
                                    cfin_biomass=0,
                                    cfin_concentration=0),
                  data.frame(net=c(5),
                             stage=c("CV"),
                             cfin_biomass=0,
                             cfin_concentration=0))
    temp1 = temp1[order(temp1$net),]
  }
  
  require(plyr)
  temp1$Intervals = Net_Metadata[Net_Metadata$Station_Number == stations[i],] %>% 
    group_by(Net_Num) %>%
    reframe(depth = round_any(mean(`Pressure_(dbar)`), 5, f = ceiling)) %>% slice(c(1:5))
  detach("package:plyr")
  
  temp1 = temp1 %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))
  
  temp2 = OPC_Data_Full %>% filter(OPC_cast_num %in% OPC_casts[[i]]) %>%
    filter(depth>1) %>%
    filter(flag != 'depth') %>%
    ungroup()
  
  
  if(nrow(temp2) == 0) {
    temp2 = data.frame(C5_concentration = rep(0,times=200))
  } else {
    temp2 = temp2 %>%
      reframe(opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size, reject_volume = T),
              C5_concentration = OPC_to_C5(concentration, speed))
  }
  
  temp3 = data.frame(Depth_Bin = seq(0,199,1), Net_Concentration = NA, OPC_Concentration = NA)
  
  for(j in 1:nrow(temp2)) {
    temp3$OPC_Concentration[temp3$Depth_Bin == temp2$depth[j]] = temp2$C5_concentration[j]
  }
  
  for(k in 1:nrow(temp1)) {
    if(k == 1) {
      temp3$Net_Concentration[temp3$Depth_Bin <= temp1$`Intervals$depth`[k]] = temp1$cfin_concentration[k]
    } else {
      temp3$Net_Concentration[(temp3$Depth_Bin <= temp1$`Intervals$depth`[k] &
                                 temp3$Depth_Bin >= temp1$`Intervals$depth`[k-1])] = temp1$cfin_concentration[k]
    }
  }
  
  OB_Shallow_Net_OPC_Comparison[[i]] = temp3 %>% 
    transform(Depth_Bin=cut(Depth_Bin, seq(0,200,5),right=F)) %>% 
    group_by(Depth_Bin) %>% 
    summarise(Net_Concentration = mean(Net_Concentration), OPC_Concentration = mean(OPC_Concentration, na.rm=T))
  
}

station5_plot = ggplot(data=OB_Shallow_Net_OPC_Comparison[[1]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,6000)) +
  scale_y_break(breaks = c(1999,2000),scales=0.25,ticklabels=seq(3000,6000,3000)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),         axis.ticks.x.top = element_blank(),         axis.text.x.top = element_blank()) +
  labs(y="Concentration (individuals/m^3)",x="Depth (m)")
ggsave(file=paste0(figure_dir,"Net_OPC_Depth_Profiles/","Station_5_Net_OPC_Comparison.png"),
       scale=2)
station20_plot = ggplot(data=OB_Shallow_Net_OPC_Comparison[[2]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,6000)) +
  scale_y_break(breaks = c(1999,2000),scales=0.25,ticklabels=seq(3000,6000,3000)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),         axis.ticks.x.top = element_blank(),         axis.text.x.top = element_blank()) +
  labs(y="Concentration (individuals/m^3)",x="Depth (m)")
ggsave(file=paste0(figure_dir,"Net_OPC_Depth_Profiles/","Station_20_Net_OPC_Comparison.png"),
       scale=2)

OPC_Data_Full %>% filter(OPC_cast_num %in% c(5, 35, 36, 33, 7, 8)) %>%
  filter(depth>1) %>% 
  ungroup() %>%
  reframe(opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size),
          C5_concentration = OPC_to_C5(concentration, speed)) %>%
  transform(depth=cut(depth, seq(0,200,5),right=F)) %>% 
  group_by(depth) %>% 
  summarise(C5_concentration = mean(C5_concentration, na.rm=T)) %>%
  ungroup() %>%
  filter(row_number() >= 17) %>%
  select(2) %>%
  summarise(formatC(sd(as.matrix(.),na.rm=T),digits=5))


OB_Plots = ggarrange(plotlist=list(print(station4_plot),print(station3_plot),print(station5_plot),
                                   print(station6_plot),print(station7_plot),print(station20_plot),
                                   print(station19_plot),print(station18_plot),NA),ncol=3,nrow=3,
                     labels=c("Deep","Mid","Shallow"),vjust=1)
fname=paste0(figure_dir,"OB_All_Stations_Comparison.png")
ggsave(file=fname,scale=3)

#####

## Echosounder vertical bar graphs at each station, both masked 455 kHz Sv and 
# 200-455 kHz SvDiff

DFs = sort(grep("Sept[0-9]{2}_Integration",names(.GlobalEnv),value=TRUE))
echo_sv = list()

for(i in 1:length(DFs)) {
  # for each day
  integration = get(DFs[i])
  echo_sv[[i]] = list()
  for(j in 1:length(integration)) {
    # for each tow
    echo_sv[[i]][[j]] = list()
    
    echo_sv[[i]][[j]][[1]] = 10 * log10(integration[[j]][[5]]) # 200-455 kHz SvDiff
    
    mask = 13.7 <= echo_sv[[i]][[j]][[1]] & 
      echo_sv[[i]][[j]][[1]] <= 14.2 # mask
    echo_sv[[i]][[j]][[2]] = (10 * log10(integration[[j]][[7]])) * mask # masked 455 kHz
    echo_sv[[i]][[j]][[2]][echo_sv[[i]][[j]][[2]] == 0] = NaN # remove 0s
    echo_sv[[i]][[j]][[2]][is.na(echo_sv[[i]][[j]][[2]])] = NaN # remove NAs

  }
}

# creating dataframes grouped into 5 m depth bins
echo_grouped_sv = data.frame(matrix(0, ncol=17, nrow=40))
echo_grouped_sv[,1] = (paste0("[",seq(0,195,5),",",seq(5,200,5),")"))
names(echo_grouped_sv)[1] = "Depth_Bin"

echo_grouped_svdiff = data.frame(matrix(0, ncol=17, nrow=40))
echo_grouped_svdiff[,1] = (paste0("[",seq(0,195,5),",",seq(5,200,5),")"))
names(echo_grouped_svdiff)[1] = "Depth_Bin"

n = 2

for(k in 1:length(echo_sv)) {
  # for each day
  for(m in 1:length(echo_sv[[k]])) {
    # for each tow
    
    # get the 455 kHz Sv values
    test = as.data.frame(echo_sv[[k]][[m]][[2]])
    # transpose so rows = depth
    test = t(test)
    # Change NaNs to NAs
    test[which(is.nan(test))] = NA
    # Remove columns that are all NAs
    test = test[, colSums(is.na(test)) < nrow(test)]
    
    # Mean across columns
    test2 = cbind(seq(5,200,1),
                  rowMeans(test, na.rm=T))
    
    colnames(test2) = c("Depth", "Sv")
    
    # Result: depth and mean Sv/SvDiff from glider dives at each multinet tow, in 5 m
    # depth bins
    test3 = test2 %>% 
      transform(bin=cut(Depth, seq(5,200,5),right=F)) %>%
      group_by(bin) %>%
      reframe(Mean_Sv = mean(Sv))
    
    # Store these values in a dataframe
    for(p in 1:nrow(test3)) {
      echo_grouped_sv[which(echo_grouped_sv$Depth_Bin == test3$bin[p]),n] = test3$Mean_Sv[p]
    }
    
    # get the 200-455 kHz Sv values
    test = as.data.frame(echo_sv[[k]][[m]][[1]])
    # transpose so rows = depth
    test = t(test)
    # Change NaNs to NAs
    test[which(is.nan(test))] = NA
    # Remove columns that are all NAs
    test = test[, colSums(is.na(test)) < nrow(test)]
    
    # Mean across columns
    test2 = cbind(seq(5,200,1),
                  rowMeans(test, na.rm=T))
    
    colnames(test2) = c("Depth", "SvDiff")
    
    # Result: depth and mean Sv/SvDiff from glider dives at each multinet tow, in 5 m
    # depth bins
    test3 = test2 %>% 
      transform(bin=cut(Depth, seq(5,200,5),right=F)) %>%
      group_by(bin) %>%
      reframe(Mean_SvDiff = mean(SvDiff))
    
    # Store these values in a dataframe
    for(p in 1:nrow(test3)) {
      echo_grouped_svdiff[which(echo_grouped_svdiff$Depth_Bin == test3$bin[p]),n] = test3$Mean_SvDiff[p]
    }
    n = n + 1
  }
}

names(echo_grouped_sv)[2:ncol(echo_grouped_sv)] = c(3, 4,
                                                    5, 6, 7,
                                                    8, 9, 10, 
                                                    12, 13,
                                                    15, 16, 17,
                                                    18, 19, 20)
echo_grouped_sv$Depth_Bin = factor(echo_grouped_sv$Depth_Bin, levels = echo_grouped_sv$Depth_Bin)
names(echo_grouped_svdiff)[2:ncol(echo_grouped_svdiff)] = c(3, 4,
                                                            5, 6, 7,
                                                            8, 9, 10, 
                                                            12, 13,
                                                            15, 16, 17,
                                                            18, 19, 20)
echo_grouped_svdiff$Depth_Bin = factor(echo_grouped_svdiff$Depth_Bin, levels = echo_grouped_svdiff$Depth_Bin)

echo_grouped_sv[1,2:ncol(echo_grouped_sv)] = NaN
echo_grouped_svdiff[1,2:ncol(echo_grouped_sv)] = NaN

sv_plots=list()
svdiff_plots=list()

for(p in 1:16) {
  sv_plots[[p]] = local({
    p = p
    plot = ggplot(data = echo_grouped_sv, aes(y = Depth_Bin)) +
      geom_col(aes(x = echo_grouped_sv[,p+1]+100)) +
      scale_y_discrete(limits=rev, breaks = echo_grouped_sv$Depth_Bin[c(seq(1,40,3))]) +
      scale_x_continuous(labels=seq(-85,-60,5)) +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      coord_cartesian(xlim = c(-85+100,-60+100), expand = T) + 
      labs(x = "Masked 455 kHz Sv (dB)",y="Depth (m)")})
  fname=paste0(figure_dir, "Sv_Depth_Profiles/Station_",names(echo_grouped_sv)[p+1],"_Masked_455kHz_Sv.png")
  ggsave(sv_plots[[p]],file=fname,scale=2)
  
  svdiff_plots[[p]] = local({
    p=p
    plot = ggplot(data = echo_grouped_svdiff, aes(y = Depth_Bin)) +
      geom_col(aes(x = echo_grouped_svdiff[,p+1])) +
      scale_y_discrete(limits=rev, breaks = echo_grouped_svdiff$Depth_Bin[c(seq(1,40,3))]) +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      coord_cartesian(xlim = c(5,16), expand = T) +
      labs(x = "455-200 kHz Sv Difference (dB)",y="Depth (m)")})
  fname=paste0(figure_dir, "Sv_Depth_Profiles/Station_",names(echo_grouped_svdiff)[p+1],"_455-200kHz_SvDiff.png")
  ggsave(svdiff_plots[[p]],file=fname,scale=2)

  ggarrange(sv_plots[[p]],svdiff_plots[[p]],ncol=2)
  fname=paste0(figure_dir, "Sv_Depth_Profiles/Station_",names(echo_grouped_svdiff)[p],"_Sv_SvDiff_Comparison.png")
  ggsave(file=fname,scale=2)
}


## Plot echosounder and net/OPC data side by side

DFs=c(sort(grep("station[0-9]{1}_plot",names(.GlobalEnv),value=T)),
      sort(grep("station[0-9]{2}_plot",names(.GlobalEnv),value=T)))

for(i in 1:length(DFs)) {
  # not 100% perfectly aligned but PRETTY DAMN CLOSE
  full_plot = ggarrange(print(get(DFs[i])),sv_plots[[i]],svdiff_plots[[i]],ncol=3)
  
  station_num = colnames(echo_grouped_sv)[i+1]
  annotate_figure(full_plot,top=text_grob(paste0("Station ",station_num," Comparison"))) +
    theme(plot.background = element_rect(fill = 'white',color='white'))
  
  fname=paste0(figure_dir,"Net_OPC_Sv_Depth_Profiles/Station_",station_num,"_Full_Comparison.png")
  ggsave(file=fname,scale=2)
}

#####

## Plot echosounder data by "stations"

# Station numbers:
# GMB shallow: 10, 13, 17, seafloor depth 158
# OB shallow: 5, 20, seafloor depth 110

# GMB mid: 9, 16, seafloor depth 160.5
# OB mid: 3, 7, 18, seafloor depth 127.3

# GMB deep: 8, 12, 15, seafloor depth 169.3
# OB deep: 4, 6, 19, seafloor depth 135

GMB_Shallow = cbind(echo_grouped_sv[,c(1,9,11,14)] %>% 
                      reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:4), na.rm = TRUE)),
                    echo_grouped_svdiff[,c(9,11,14)] %>% 
                      reframe(Mean_SvDiff = rowMeans(select(.,1:3), na.rm = TRUE)))
GMB_Shallow_Sv_Plot = ggarrange(ggplot(data = GMB_Shallow, aes(y = Depth_Bin)) +
    geom_col(aes(x=Mean_Sv+100)) +
    scale_y_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
    scale_x_continuous(labels=seq(-85,-60,5)) +
    coord_cartesian(xlim = c(-85+100,-60+100), expand = T) + 
    theme_bw() +
    theme(panel.grid = element_blank()) +
    labs(x = "Masked 455 kHz (dB)", y="Depth (m)"),
  ggplot(data = GMB_Shallow, aes(y = Depth_Bin)) +
    geom_col(aes(x=Mean_SvDiff)) +
    scale_y_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    coord_cartesian(xlim = c(5,16), expand = T) +
    labs(x = "455-200 kHz Sv Difference (dB)", y="Depth (m)"))
ggsave(file = paste0(figure_dir, "GMB_Shallow_Sv_and_SvDiff.png"), scale=2)

GMB_Mid = cbind(echo_grouped_sv[,c(1,8,13)] %>% 
                      reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:3), na.rm = TRUE)),
                    echo_grouped_svdiff[,c(8,13)] %>% 
                      reframe(Mean_SvDiff = rowMeans(select(.,1:2), na.rm = TRUE)))
GMB_Mid_Sv_Plot = ggarrange(ggplot(data = GMB_Mid, aes(y = Depth_Bin)) +
            geom_col(aes(x=Mean_Sv+100)) +
            scale_y_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
            scale_x_continuous(labels=seq(-85,-60,5)) +
            coord_cartesian(xlim = c(-85+100,-60+100), expand = T) + 
            theme_bw() +
            theme(panel.grid = element_blank()) +
            labs(x = "Masked 455 kHz (dB)", y="Depth (m)"),
          ggplot(data = GMB_Mid, aes(y = Depth_Bin)) +
            geom_col(aes(x=Mean_SvDiff)) +
            scale_y_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
            theme_bw() +
            theme(panel.grid = element_blank()) +
            coord_cartesian(xlim = c(5,16), expand = T) +
            labs(x = "455-200 kHz Sv Difference (dB)", y="Depth (m)"))
ggsave(file = paste0(figure_dir, "GMB_Mid_Sv_and_SvDiff.png"), scale=2)

GMB_Deep = cbind(echo_grouped_sv[,c(1,7,10,12)] %>% 
                      reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:4), na.rm = TRUE)),
                    echo_grouped_svdiff[,c(7,10,12)] %>% 
                      reframe(Mean_SvDiff = rowMeans(select(.,1:3), na.rm = TRUE)))
GMB_Deep_Sv_Plot = ggarrange(ggplot(data = GMB_Deep, aes(y = Depth_Bin)) +
            geom_col(aes(x=Mean_Sv+100)) +
            scale_y_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
            scale_x_continuous(labels=seq(-85,-60,5)) +
            coord_cartesian(xlim = c(-85+100,-60+100), expand = T) + 
            theme_bw() +
            theme(panel.grid = element_blank()) +
            labs(x = "Masked 455 kHz (dB)", y="Depth (m)"),
          ggplot(data = GMB_Deep, aes(y = Depth_Bin)) +
            geom_col(aes(x=Mean_SvDiff)) +
            scale_y_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
            theme_bw() +
            theme(panel.grid = element_blank()) +
            coord_cartesian(xlim = c(5,16), expand = T) +
            labs(x = "455-200 kHz Sv Difference (dB)", y="Depth (m)"))
ggsave(file = paste0(figure_dir, "GMB_Deep_Sv_and_SvDiff.png"), scale=2)


OB_Shallow = cbind(echo_grouped_sv[,c(1,4,17)] %>% 
                      reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:3), na.rm = TRUE)),
                    echo_grouped_svdiff[,c(4,17)] %>% 
                      reframe(Mean_SvDiff = rowMeans(select(.,1:2), na.rm = TRUE)))
OB_Shallow_Sv_Plot = ggarrange(ggplot(data = OB_Shallow, aes(y = Depth_Bin)) +
            geom_col(aes(x=Mean_Sv+100)) +
            scale_y_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
            scale_x_continuous(labels=seq(-85,-60,5)) +
            coord_cartesian(xlim = c(-85+100,-60+100), expand = T) + 
            theme_bw() +
            theme(panel.grid = element_blank()) +
            labs(x = "Masked 455 kHz (dB)", y="Depth (m)"),
          ggplot(data = OB_Shallow, aes(y = Depth_Bin)) +
            geom_col(aes(x=Mean_SvDiff)) +
            scale_y_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
            theme_bw() +
            theme(panel.grid = element_blank()) +
            coord_cartesian(xlim = c(5,16), expand = T) +
            labs(x = "455-200 kHz Sv Difference (dB)", y="Depth (m)"))
ggsave(file = paste0(figure_dir, "OB_Shallow_Sv_and_SvDiff.png"), scale=2)

OB_Mid = cbind(echo_grouped_sv[,c(1,2,6,15)] %>% 
                  reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:4), na.rm = TRUE)),
                echo_grouped_svdiff[,c(2,6,15)] %>% 
                  reframe(Mean_SvDiff = rowMeans(select(.,1:3), na.rm = TRUE)))
OB_Mid_Sv_Plot = ggarrange(ggplot(data = OB_Mid, aes(y = Depth_Bin)) +
            geom_col(aes(x=Mean_Sv+100)) +
            scale_y_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
            scale_x_continuous(labels=seq(-85,-60,5)) +
            coord_cartesian(xlim = c(-85+100,-60+100), expand = T) + 
            theme_bw() +
            theme(panel.grid = element_blank()) +
            labs(x = "Masked 455 kHz (dB)", y="Depth (m)"),
          ggplot(data = OB_Mid, aes(y = Depth_Bin)) +
            geom_col(aes(x=Mean_SvDiff)) +
            scale_y_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
            theme_bw() +
            theme(panel.grid = element_blank()) +
            coord_cartesian(xlim = c(5,16), expand = T) +
            labs(x = "455-200 kHz Sv Difference (dB)", y="Depth (m)"))
ggsave(file = paste0(figure_dir, "OB_Mid_Sv_and_SvDiff.png"), scale=2)

OB_Deep = cbind(echo_grouped_sv[,c(1,3,5,16)] %>% 
                   reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:4), na.rm = TRUE)),
                 echo_grouped_svdiff[,c(3,5,16)] %>% 
                   reframe(Mean_SvDiff = rowMeans(select(.,1:3), na.rm = TRUE)))
OB_Deep_Sv_Plot = ggarrange(ggplot(data = OB_Deep, aes(y = Depth_Bin)) +
            geom_col(aes(x=Mean_Sv+100)) +
            scale_y_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
            scale_x_continuous(labels=seq(-85,-60,5)) +
            coord_cartesian(xlim = c(-85+100,-60+100), expand = T) + 
            theme_bw() +
            theme(panel.grid = element_blank()) +
            labs(x = "Masked 455 kHz (dB)", y="Depth (m)"),
          ggplot(data = OB_Deep, aes(y = Depth_Bin)) +
            geom_col(aes(x=Mean_SvDiff)) +
            scale_y_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
            theme_bw() +
            theme(panel.grid = element_blank()) +
            coord_cartesian(xlim = c(5,16), expand = T) +
            labs(x = "455-200 kHz Sv Difference (dB)", y="Depth (m)"))
ggsave(file = paste0(figure_dir, "OB_Deep_Sv_and_SvDiff.png"), scale=2)

#####

## Plot net/OPC data by "stations"

# Station numbers:
# GMB shallow: 10, 13, 17, seafloor depth 158
# OB shallow: 5, 20, seafloor depth 110

# GMB mid: 9, 16, seafloor depth 160.5
# OB mid: 3, 7, 18, seafloor depth 127.3

# GMB deep: 8, 12, 15, seafloor depth 169.3
# OB deep: 4, 6, 19, seafloor depth 135

Averaged_Net_Concentrations = function(stns) {
  
  temp1 = stages %>% filter(station %in% stns & stage %in% c("CV")) %>%
  group_by(net, stage) %>%
  reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))

require(plyr)
temp1$Intervals = Net_Metadata[Net_Metadata$Station_Number %in% stns,] %>% 
  group_by(Net_Num) %>%
  reframe(depth = round_any(mean(`Pressure_(dbar)`), 5, f = ceiling)) %>% slice(c(1:5))
detach("package:plyr")

temp1 = temp1 %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))

temp3 = data.frame(Depth_Bin = seq(0,195,5), Net_Concentration = NA)

for(k in 1:nrow(temp1)) {
  if(k == 1) {
    temp3$Net_Concentration[temp3$Depth_Bin <= temp1$`Intervals$depth`[k]] = temp1$cfin_concentration[k]
  } else {
    temp3$Net_Concentration[(temp3$Depth_Bin <= temp1$`Intervals$depth`[k] &
                               temp3$Depth_Bin >= temp1$`Intervals$depth`[k-1])] = temp1$cfin_concentration[k]
  }
}
return(temp3$Net_Concentration)
}

GMB_Shallow = cbind(GMB_Shallow_Net_OPC_Comparison[[1]][1],
                 Averaged_Net_Concentrations(c("10","13","17")),
                 rowMeans(cbind(GMB_Shallow_Net_OPC_Comparison[[1]][3],
                                GMB_Shallow_Net_OPC_Comparison[[2]][3],
                                GMB_Shallow_Net_OPC_Comparison[[3]][3]),na.rm=T))
names(GMB_Shallow)[2:3] = c("Net_Concentration","OPC_Concentration")

GMB_Shallow_Abundance_Plot = ggplot(data=GMB_Shallow, aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Shallow$Depth_Bin[c(seq(1,40,3))]) +
  # scale_y_continuous(limits = c(0, 5000)) +
  # scale_y_break(breaks = c(1000,1001), scales = 0.25, ticklabels=seq(2000, 4000, 2000), expand = F) +
  scale_y_continuous(limits = c(0,40000)) +
  scale_y_break(breaks = c(5999,6000),scales=0.25,ticklabels=seq(15000,35000,20000), expand = F) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.text.x.top = element_blank()) +
  labs(y=element_blank(),x="Depth (m)")
# ggsave(file = paste0(figure_dir, "GMB_Shallow_Abundance_Comparison.png"), scale=2)

GMB_Mid = cbind(GMB_Mid_Net_OPC_Comparison[[1]][1],
                 Averaged_Net_Concentrations(c("9","16")),
                 rowMeans(cbind(GMB_Mid_Net_OPC_Comparison[[1]][3],
                                GMB_Mid_Net_OPC_Comparison[[2]][3]),na.rm=T))
names(GMB_Mid)[2:3] = c("Net_Concentration","OPC_Concentration")

GMB_Mid_Abundance_Plot = ggplot(data=GMB_Mid, aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Mid$Depth_Bin[c(seq(1,40,3))]) +
  # scale_y_continuous(limits = c(0, 5000)) +
  # scale_y_break(breaks = c(1000,1001), scales = 0.25, ticklabels=seq(2000, 4000, 2000), expand = F) +
  scale_y_continuous(limits = c(0,40000)) +
  scale_y_break(breaks = c(5999,6000),scales=0.25,ticklabels=seq(15000,35000,20000), expand = F) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.text.x.top = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(y=element_blank(),x=element_blank())
# ggsave(file = paste0(figure_dir, "GMB_Mid_Abundance_Comparison.png"), scale=2)

GMB_Deep = cbind(GMB_Deep_Net_OPC_Comparison[[1]][1],
                 Averaged_Net_Concentrations(c("8","12","15")),
               rowMeans(cbind(GMB_Deep_Net_OPC_Comparison[[1]][3],
                              GMB_Deep_Net_OPC_Comparison[[2]][3],
                              GMB_Deep_Net_OPC_Comparison[[3]][3]),na.rm=T))
names(GMB_Deep)[2:3] = c("Net_Concentration","OPC_Concentration")

GMB_Deep_Abundance_Plot = ggplot(data=GMB_Deep, aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep$Depth_Bin[c(seq(1,40,3))]) +
  # scale_y_continuous(limits = c(0, 5000)) +
  # scale_y_break(breaks = c(1000,1001), scales = 0.25, ticklabels=seq(2000, 4000, 2000), expand = F) +
  scale_y_continuous(limits = c(0,40000)) +
  scale_y_break(breaks = c(5999,6000),scales=0.25,ticklabels=seq(15000,35000,20000), expand = F) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.text.x.top = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(y=element_blank(),x=element_blank())
# ggsave(file = paste0(figure_dir, "GMB_Deep_Abundance_Comparison.png"), scale=2)

fig = ggarrange(print(GMB_Shallow_Abundance_Plot), print(GMB_Mid_Abundance_Plot), print(GMB_Deep_Abundance_Plot),
                ncol=3,nrow=1,
                labels="AUTO") + theme(plot.background = element_rect(fill = 'white',color='white'))
annotate_figure(fig, bottom = textGrob("Concentration (individuals/m^3)",gp = gpar(cex = 1),vjust=-1.5))
ggsave(file = paste0(figure_dir, "GMB_Abundance_Comparisons.png"),scale=2)
#####

temp1 = stages %>% filter(station %in% c(5,20) & stage %in% c("CV")) %>%
  group_by(net, stage) %>%
  reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))

temp1 = rbind(temp1, data.frame(net=4,
                                stage=c("CV"),
                                cfin_biomass=0,
                                cfin_concentration=0))
temp1 = temp1[order(temp1$net),]

require(plyr)
temp1$Intervals = Net_Metadata[Net_Metadata$Station_Number %in% c(5,20),] %>% 
  group_by(Net_Num) %>%
  reframe(depth = round_any(mean(`Pressure_(dbar)`), 5, f = ceiling)) %>% slice(c(1:5))
detach("package:plyr")

temp1 = temp1 %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))

temp3 = data.frame(Depth_Bin = seq(0,195,5), Net_Concentration = NA)

for(k in 1:nrow(temp1)) {
  if(k == 1) {
    temp3$Net_Concentration[temp3$Depth_Bin <= temp1$`Intervals$depth`[k]] = temp1$cfin_concentration[k]
  } else {
    temp3$Net_Concentration[(temp3$Depth_Bin <= temp1$`Intervals$depth`[k] &
                               temp3$Depth_Bin >= temp1$`Intervals$depth`[k-1])] = temp1$cfin_concentration[k]
  }
}

OB_Shallow = cbind(OB_Shallow_Net_OPC_Comparison[[1]][1],
                   temp3$Net_Concentration,
                    rowMeans(cbind(OB_Shallow_Net_OPC_Comparison[[1]][3],
                                   OB_Shallow_Net_OPC_Comparison[[2]][3]),na.rm=T))
names(OB_Shallow)[2:3] = c("Net_Concentration","OPC_Concentration")

OB_Shallow_Abundance_Plot = ggplot(data=OB_Shallow, aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=OB_Shallow$Depth_Bin[c(seq(1,40,3))]) +
  # scale_y_continuous(limits = c(0, 5000)) +
  # scale_y_break(breaks = c(1000,1001), scales = 0.25, ticklabels=seq(2000, 4000, 2000), expand = F) +
  scale_y_continuous(limits = c(0,40000)) +
  scale_y_break(breaks = c(5999,6000),scales=0.25,ticklabels=seq(15000,35000,20000), expand = F) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.text.x.top = element_blank()) +
  labs(y=element_blank(),x="Depth (m)")
# ggsave(file = paste0(figure_dir, "OB_Shallow_Abundance_Comparison.png"), scale=2)

OB_Mid = cbind(OB_Mid_Net_OPC_Comparison[[1]][1],
                Averaged_Net_Concentrations(c(3,7,18)),
                rowMeans(cbind(OB_Mid_Net_OPC_Comparison[[1]][3],
                               OB_Mid_Net_OPC_Comparison[[2]][3],
                               OB_Mid_Net_OPC_Comparison[[3]][3]),na.rm=T))
names(OB_Mid)[2:3] = c("Net_Concentration","OPC_Concentration")

OB_Mid_Abundance_Plot = ggplot(data=OB_Mid, aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=OB_Mid$Depth_Bin[c(seq(1,40,3))]) +
  # scale_y_continuous(limits = c(0, 5000)) +
  # scale_y_break(breaks = c(1000,1001), scales = 0.25, ticklabels=seq(2000, 4000, 2000), expand = F) +
  scale_y_continuous(limits = c(0,40000)) +
  scale_y_break(breaks = c(5999,6000),scales=0.25,ticklabels=seq(15000,35000,20000), expand = F) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.text.x.top = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(y=element_blank(),x=element_blank())
# ggsave(file = paste0(figure_dir, "OB_Mid_Abundance_Comparison.png"), scale=2)


temp1 = stages %>% filter(station %in% c(4, 6, 19) & stage %in% c("CV")) %>%
  group_by(net, stage) %>%
  reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))
temp1 = rbind(temp1, data.frame(net=4,
                                stage=c("CV"),
                                cfin_biomass=0,
                                cfin_concentration=0))
temp1 = temp1[order(temp1$net),]

require(plyr)
temp1$Intervals = Net_Metadata[Net_Metadata$Station_Number %in% c(4, 6, 19),] %>% 
  group_by(Net_Num) %>%
  reframe(depth = round_any(mean(`Pressure_(dbar)`), 5, f = ceiling)) %>% slice(c(1:5))
detach("package:plyr")

temp1 = temp1 %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))

temp3 = data.frame(Depth_Bin = seq(0,195,5), Net_Concentration = NA)

for(k in 1:nrow(temp1)) {
  if(k == 1) {
    temp3$Net_Concentration[temp3$Depth_Bin <= temp1$`Intervals$depth`[k]] = temp1$cfin_concentration[k]
  } else {
    temp3$Net_Concentration[(temp3$Depth_Bin <= temp1$`Intervals$depth`[k] &
                               temp3$Depth_Bin >= temp1$`Intervals$depth`[k-1])] = temp1$cfin_concentration[k]
  }
}

OB_Deep = cbind(OB_Deep_Net_OPC_Comparison[[1]][1],
                 temp3$Net_Concentration,
                 rowMeans(cbind(OB_Deep_Net_OPC_Comparison[[1]][3],
                                OB_Deep_Net_OPC_Comparison[[2]][3],
                                OB_Deep_Net_OPC_Comparison[[3]][3]),na.rm=T))
names(OB_Deep)[2:3] = c("Net_Concentration","OPC_Concentration")

OB_Deep_Abundance_Plot = ggplot(data=OB_Deep, aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=OB_Deep$Depth_Bin[c(seq(1,40,3))]) +
  # scale_y_continuous(limits = c(0, 5000)) +
  # scale_y_break(breaks = c(1000,1001), scales = 0.25, ticklabels=seq(2000, 4000, 2000), expand = F) +
  scale_y_continuous(limits = c(0,40000)) +
  scale_y_break(breaks = c(5999,6000),scales=0.25,ticklabels=seq(15000,35000,20000), expand = F) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.text.x.top = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(y=element_blank(),x=element_blank())
# ggsave(file = paste0(figure_dir, "OB_Deep_Abundance_Comparison.png"), scale=2)

fig = ggarrange(print(OB_Shallow_Abundance_Plot), print(OB_Mid_Abundance_Plot), print(OB_Deep_Abundance_Plot),
          ncol=3,nrow=1,
          labels=c("D","E","F")) + theme(plot.background = element_rect(fill = 'white',color='white'))
annotate_figure(fig, bottom = textGrob("Concentration (individuals/m^3)",gp = gpar(cex = 1),vjust=-1.5))
ggsave(file = paste0(figure_dir, "OB_Abundance_Comparisons.png"),scale=2)

#####

## "Station" comparisons between net/OPC and echosounder

GMB_Shallow_Full = ggarrange(print(GMB_Shallow_Abundance_Plot),print(GMB_Shallow_Sv_Plot))
ggsave(GMB_Shallow_Full, file=paste0(figure_dir, "GMB_Shallow_Full_Comparison.png"),scale=2.5)

GMB_Mid_Full = ggarrange(print(GMB_Mid_Abundance_Plot),print(GMB_Mid_Sv_Plot))
ggsave(GMB_Mid_Full, file=paste0(figure_dir, "GMB_Mid_Full_Comparison.png"),scale=2.5)

GMB_Deep_Full = ggarrange(print(GMB_Deep_Abundance_Plot),print(GMB_Deep_Sv_Plot))
ggsave(GMB_Deep_Full, file=paste0(figure_dir, "GMB_Deep_Full_Comparison.png"),scale=2.5)

OB_Shallow_Full = ggarrange(print(OB_Shallow_Abundance_Plot),print(OB_Shallow_Sv_Plot))
ggsave(OB_Shallow_Full, file=paste0(figure_dir, "OB_Shallow_Full_Comparison.png"),scale=2.5)

OB_Mid_Full = ggarrange(print(OB_Mid_Abundance_Plot),print(OB_Mid_Sv_Plot))
ggsave(OB_Mid_Full, file=paste0(figure_dir, "OB_Mid_Full_Comparison.png"),scale=2.5)

OB_Deep_Full = ggarrange(print(OB_Deep_Abundance_Plot),print(OB_Deep_Sv_Plot))
ggsave(OB_Deep_Full, file=paste0(figure_dir, "OB_Deep_Full_Comparison.png"),scale=2.5)

#####

## Echosounder Sv adjustment and SONAR equation for C. fin concentration

# Want to test the effect of forcing the slope to be 1 vs using a linear model
# First, adjust Sv(echo) to Sv(cfin) using the correlation equation of your choice
# Then use the inverse problem to obtain the concentration
# Average them by depth (just as with the net or OPC) and plot

cfin_TS = -108.3

slope_1_intercept = -23.77

top_50_slope = 0.40
top_50_intercept = -62.75

all_data_slope = 0.49
all_data_intercept = -56.86

temp = echo_grouped_sv[,c(1,9,11,14)] %>% 
  reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:4), na.rm = TRUE))
temp$Concentration_1 = 10^(((temp$Mean_Sv + slope_1_intercept)- cfin_TS)/10) # slope fixed at 1
temp$Concentration_2 = 10^(((top_50_slope * temp$Mean_Sv + top_50_intercept)- cfin_TS)/10) # top 50%
temp$Concentration_3 = 10^(((all_data_slope * temp$Mean_Sv + all_data_intercept)- cfin_TS)/10) # all data
temp$Concentration_4 = 10^(((temp$Mean_Sv)- cfin_TS)/10) # no correction

Echo_Concentration = data.frame(Depth = rep(temp$Depth_Bin, times=4), 
                                Concentration = c(temp$Concentration_1,
                                                  temp$Concentration_2,
                                                  temp$Concentration_3,
                                                  temp$Concentration_4),
                                Equation = rep(c("1","2","3","No correction"),each=length(temp$Depth)))
Echo_Concentration$Concentration[is.nan(Echo_Concentration$Concentration)] = 0

GMB_Shallow_Abundance_Plot_Sv = GMB_Shallow_Abundance_Plot +
  geom_line(data=Echo_Concentration,aes(x=Depth,y=Concentration,group=Equation,color=Equation, linetype = Equation),linewidth=1) +
  scale_color_viridis_d(labels=c("Slope = 1","Top 50% of Echo Data","All Echo Data","No Correction")) +
  scale_linetype(labels=c("Slope = 1","Top 50% of Echo Data","All Echo Data","No Correction")) +
  theme(legend.position = "none") +
  theme(text = element_text(size=16),
        axis.text.x.bottom = element_text(hjust=1, angle=45))
fname=paste0(figure_dir,"GMB_Shallow_Abundance_Comparison_Sv_Corrected_Only.png")
# ggsave(file=fname,scale=2)

temp = echo_grouped_sv[,c(1,8,13)] %>% 
  reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:3), na.rm = TRUE))
temp$Concentration_1 = 10^(((temp$Mean_Sv + slope_1_intercept)- cfin_TS)/10) # slope fixed at 1
temp$Concentration_2 = 10^(((top_50_slope * temp$Mean_Sv + top_50_intercept)- cfin_TS)/10) # top 50%
temp$Concentration_3 = 10^(((all_data_slope * temp$Mean_Sv + all_data_intercept)- cfin_TS)/10) # all data
temp$Concentration_4 = 10^(((temp$Mean_Sv)- cfin_TS)/10) # no correction

Echo_Concentration = data.frame(Depth = rep(temp$Depth_Bin, times=4), 
                                Concentration = c(temp$Concentration_1,
                                                  temp$Concentration_2,
                                                  temp$Concentration_3,
                                                  temp$Concentration_4),
                                Equation = rep(c("1","2","3","No correction"),each=length(temp$Depth)))
Echo_Concentration$Concentration[is.nan(Echo_Concentration$Concentration)] = 0

GMB_Mid_Abundance_Plot_Sv = GMB_Mid_Abundance_Plot +
  geom_line(data=Echo_Concentration,aes(x=Depth,y=Concentration,group=Equation,color=Equation, linetype = Equation),linewidth=1) +
  scale_color_viridis_d(labels=c("Slope = 1","Top 50% of Echo Data","All Echo Data", "No Correction")) +
  scale_linetype(labels=c("Slope = 1","Top 50% of Echo Data","All Echo Data","No Correction")) +
  theme(legend.position = "none") +
  theme(text = element_text(size=16),
        axis.text.x.bottom = element_text(hjust=1, angle=45))
fname=paste0(figure_dir,"GMB_Mid_Abundance_Comparison_Sv_Corrected_Only.png")
# ggsave(file=fname,scale=2)

temp = echo_grouped_sv[,c(1,7,10,12)] %>% 
  reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:4), na.rm = TRUE))
temp$Concentration_1 = 10^(((temp$Mean_Sv + slope_1_intercept)- cfin_TS)/10) # slope fixed at 1
temp$Concentration_2 = 10^(((top_50_slope * temp$Mean_Sv + top_50_intercept)- cfin_TS)/10) # top 50%
temp$Concentration_3 = 10^(((all_data_slope * temp$Mean_Sv + all_data_intercept)- cfin_TS)/10) # all data
temp$Concentration_4 = 10^(((temp$Mean_Sv)- cfin_TS)/10) # no correction

Echo_Concentration = data.frame(Depth = rep(temp$Depth_Bin, times=4), 
                                Concentration = c(temp$Concentration_1,
                                                  temp$Concentration_2,
                                                  temp$Concentration_3,
                                                  temp$Concentration_4),
                                Equation = rep(c("1","2","3","No correction"),each=length(temp$Depth)))
Echo_Concentration$Concentration[is.nan(Echo_Concentration$Concentration)] = 0

GMB_Deep_Abundance_Plot_Sv = GMB_Deep_Abundance_Plot +
  geom_line(data=Echo_Concentration,aes(x=Depth,y=Concentration,group=Equation,color=Equation, linetype = Equation),linewidth=1) +
  scale_color_viridis_d(labels=c("Slope = 1","Top 50% of Echo Data","All Echo Data", "No Correction")) +
  scale_linetype(labels=c("Slope = 1","Top 50% of Echo Data","All Echo Data","No Correction")) +
  # theme(legend.position = "none") +
  theme(text = element_text(size=16),
        axis.text.x.bottom = element_text(hjust=1, angle=45))
fname=paste0(figure_dir,"GMB_Deep_Abundance_Comparison_Sv_Corrected_Only.png")
# ggsave(file=fname,scale=2)

cfin_correction_1 = function(sv) {
  10^(((sv + slope_1_intercept)- cfin_TS)/10)
}

cfin_correction_2 = function(sv) {
  10^(((top_50_slope * sv + top_50_intercept)- cfin_TS)/10)
}

cfin_correction_3 = function(sv) {
  10^(((all_data_slope * sv + all_data_intercept)- cfin_TS)/10)
}

temp = echo_grouped_sv[,c(1,4,17)] %>% 
  reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:3), na.rm = TRUE))
temp$Concentration_1 = 10^(((temp$Mean_Sv + slope_1_intercept)- cfin_TS)/10) # slope fixed at 1
temp$Concentration_2 = 10^(((top_50_slope * temp$Mean_Sv + top_50_intercept)- cfin_TS)/10) # top 50%
temp$Concentration_3 = 10^(((all_data_slope * temp$Mean_Sv + all_data_intercept)- cfin_TS)/10) # all data
temp$Concentration_4 = 10^(((temp$Mean_Sv)- cfin_TS)/10) # no correction

Echo_Concentration = data.frame(Depth = rep(temp$Depth_Bin, times=4), 
                                Concentration = c(temp$Concentration_1,
                                                  temp$Concentration_2,
                                                  temp$Concentration_3,
                                                  temp$Concentration_4),
                                Equation = rep(c("1","2","3","No correction"),each=length(temp$Depth)))
Echo_Concentration$Concentration[is.nan(Echo_Concentration$Concentration)] = 0

OB_Shallow_Abundance_Plot_Sv = OB_Shallow_Abundance_Plot +
  geom_line(data=Echo_Concentration,aes(x=Depth,y=Concentration,group=Equation,color=Equation, linetype = Equation),linewidth=1) +
  scale_color_viridis_d(labels=c("Slope = 1","Top 50% of Echo Data","All Echo Data", "No Correction")) +
  scale_linetype(labels=c("Slope = 1","Top 50% of Echo Data","All Echo Data","No Correction")) +
  theme(legend.position = "none") +
  theme(text = element_text(size=16),
        axis.text.x.bottom = element_text(hjust=1, angle=45))
fname=paste0(figure_dir,"OB_Shallow_Abundance_Comparison_Sv_Corrected_Only.png")
# ggsave(file=fname,scale=2)

temp = echo_grouped_sv[,c(1,2,6,15)] %>% 
  reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:4), na.rm = TRUE))
temp$Concentration_1 = 10^(((temp$Mean_Sv + slope_1_intercept)- cfin_TS)/10) # slope fixed at 1
temp$Concentration_2 = 10^(((top_50_slope * temp$Mean_Sv + top_50_intercept)- cfin_TS)/10) # top 50%
temp$Concentration_3 = 10^(((all_data_slope * temp$Mean_Sv + all_data_intercept)- cfin_TS)/10) # all data
temp$Concentration_4 = 10^(((temp$Mean_Sv)- cfin_TS)/10) # no correction

Echo_Concentration = data.frame(Depth = rep(temp$Depth_Bin, times=4), 
                                Concentration = c(temp$Concentration_1,
                                                  temp$Concentration_2,
                                                  temp$Concentration_3,
                                                  temp$Concentration_4),
                                Equation = rep(c("1","2","3","No correction"),each=length(temp$Depth)))
Echo_Concentration$Concentration[is.nan(Echo_Concentration$Concentration)] = 0

OB_Mid_Abundance_Plot_Sv = OB_Mid_Abundance_Plot +
  geom_line(data=Echo_Concentration,aes(x=Depth,y=Concentration,group=Equation,color=Equation, linetype = Equation),linewidth=1) +
  scale_color_viridis_d(labels=c("Slope = 1","Top 50% of Echo Data","All Echo Data", "No Correction")) +
  scale_linetype(labels=c("Slope = 1","Top 50% of Echo Data","All Echo Data","No Correction")) +
  theme(legend.position = "none") +
  theme(text = element_text(size=16),
        axis.text.x.bottom = element_text(hjust=1, angle=45))
fname=paste0(figure_dir,"OB_Mid_Abundance_Comparison_Sv_Corrected_Only.png")
# ggsave(file=fname,scale=2)

temp = echo_grouped_sv[,c(1,3,5,16)] %>% 
  reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:4), na.rm = TRUE))
temp$Concentration_1 = 10^(((temp$Mean_Sv + slope_1_intercept)- cfin_TS)/10) # slope fixed at 1
temp$Concentration_2 = 10^(((top_50_slope * temp$Mean_Sv + top_50_intercept)- cfin_TS)/10) # top 50%
temp$Concentration_3 = 10^(((all_data_slope * temp$Mean_Sv + all_data_intercept)- cfin_TS)/10) # all data
temp$Concentration_4 = 10^(((temp$Mean_Sv)- cfin_TS)/10) # no correction

Echo_Concentration = data.frame(Depth = rep(temp$Depth_Bin, times=4), 
                                Concentration = c(temp$Concentration_1,
                                                  temp$Concentration_2,
                                                  temp$Concentration_3,
                                                  temp$Concentration_4),
                                Equation = rep(c("1","2","3","No correction"),each=length(temp$Depth)))
Echo_Concentration$Concentration[is.nan(Echo_Concentration$Concentration)] = 0

OB_Deep_Abundance_Plot_Sv = OB_Deep_Abundance_Plot +
  geom_line(data=Echo_Concentration,aes(x=Depth,y=Concentration,group=Equation,color=Equation, linetype = Equation),linewidth=1) +
  scale_color_viridis_d(labels=c("Slope = 1","Top 50% of Echo Data","All Echo Data", "No Correction")) +
  scale_linetype(labels=c("Slope = 1","Top 50% of Echo Data","All Echo Data","No Correction")) +
  # theme(legend.position = "none") +
  theme(text = element_text(size=16),
        axis.text.x.bottom = element_text(hjust=1, angle=45))
fname=paste0(figure_dir,"OB_Deep_Abundance_Comparison_Sv_Corrected_Only.png")
# ggsave(file=fname,scale=2)

echo_grouped_sv %>% 
  select(c(4,17,2,6,15,3,5,16)) %>%
  mutate(across(1:8, cfin_correction_3)) %>%
  filter(row_number() >= 17) %>%
  reframe(sd(as.matrix(.),na.rm=T))

fig = ggarrange(print(GMB_Shallow_Abundance_Plot_Sv), print(GMB_Mid_Abundance_Plot_Sv), print(GMB_Deep_Abundance_Plot_Sv),
          labels="AUTO",ncol=3,widths=c(0.9,0.75,1))
annotate_figure(fig, bottom = textGrob("Concentration (individuals/m^3)",gp = gpar(cex = 1.2),vjust=-1.5))
ggsave(filename = paste0(figure_dir,"GMB_Abundance_Comparison_Sv.png"),width=15,height=8,units="in")

fig = ggarrange(print(OB_Shallow_Abundance_Plot_Sv), print(OB_Mid_Abundance_Plot_Sv), print(OB_Deep_Abundance_Plot_Sv),
          labels = c("D","E","F"),ncol=3,widths=c(0.9,0.75,1))
annotate_figure(fig, bottom = textGrob("Concentration (individuals/m^3)",gp = gpar(cex = 1.2),vjust=-1.5))
ggsave(filename = paste0(figure_dir,"OB_Abundance_Comparison_Sv.png"),width=15,height=8,units="in")

#####

## Bias figure

# Basically need to take the mean echosounder predicted concentration over each net depth
# and then divide by the actual concentration in each net
# Plot those ratios and the mean of the log-transformed ratio (and the 95% CI)

# First, averaging the echosounder over the net depths

require(plyr)
Intervals = Net_Metadata[Net_Metadata$Station_Number %in% c(10,13,17),] %>% 
  group_by(Net_Num) %>%
  reframe(depth = round_any(mean(`Pressure_(dbar)`), 5, f = ceiling)) %>% slice(c(1:5))
detach("package:plyr")

temp = echo_grouped_sv[,c(1,9,11,14)] %>% 
  reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:4), na.rm = TRUE))
temp$Concentration_1 = 10^(((temp$Mean_Sv + slope_1_intercept)- cfin_TS)/10) # slope fixed at 1
temp$Concentration_2 = 10^(((top_50_slope * temp$Mean_Sv + top_50_intercept)- cfin_TS)/10) # top 50%
temp$Concentration_3 = 10^(((all_data_slope * temp$Mean_Sv + all_data_intercept)- cfin_TS)/10) # all data
temp$Concentration_4 = 10^(((temp$Mean_Sv)- cfin_TS)/10) # no correction

Echo_Concentration = data.frame(Depth = rep(temp$Depth_Bin, times=4), 
                                Concentration = c(temp$Concentration_1,
                                                  temp$Concentration_2,
                                                  temp$Concentration_3,
                                                  temp$Concentration_4),
                                Equation = rep(c("1","2","3","No correction"),each=length(temp$Depth)))
Echo_Concentration$Concentration[is.nan(Echo_Concentration$Concentration)] = 0
Echo_Concentration$Depth = seq(0,195,5)

GMB_Shallow_Sv_Net_Intervals = data.frame()

for(k in 1:5) {
  if (k == 1) {
    temp = Echo_Concentration %>%
      group_by(Equation) %>%
      filter(Depth >= Intervals$depth[k]) %>%
      summarise(Sv_Mean = mean(Concentration))
    GMB_Shallow_Sv_Net_Intervals = cbind(net=Intervals$Net_Num[k]+1,temp,Net_Mean = rep(rev(unique(GMB_Shallow$Net_Concentration))[k+1],each=4))
  } else {
    temp = Echo_Concentration %>%
      group_by(Equation) %>%
      filter((Depth >= Intervals$depth[k]) & (Depth < Intervals$depth[k-1])) %>%
      summarise(Sv_Mean = mean(Concentration))
    GMB_Shallow_Sv_Net_Intervals = rbind(GMB_Shallow_Sv_Net_Intervals,cbind(net=Intervals$Net_Num[k]+1,temp,Net_Mean=rep(rev(unique(GMB_Shallow$Net_Concentration))[k+1],each=4)))
  }
}
GMB_Shallow_Sv_Net_Intervals$Ratio = GMB_Shallow_Sv_Net_Intervals$Sv_Mean/GMB_Shallow_Sv_Net_Intervals$Net_Mean


require(plyr)
Intervals = Net_Metadata[Net_Metadata$Station_Number %in% c(9, 16),] %>% 
  group_by(Net_Num) %>%
  reframe(depth = round_any(mean(`Pressure_(dbar)`), 5, f = ceiling)) %>% slice(c(1:5))
detach("package:plyr")

temp = echo_grouped_sv[,c(1,8,13)] %>% 
  reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:3), na.rm = TRUE))
temp$Concentration_1 = 10^(((temp$Mean_Sv + slope_1_intercept)- cfin_TS)/10) # slope fixed at 1
temp$Concentration_2 = 10^(((top_50_slope * temp$Mean_Sv + top_50_intercept)- cfin_TS)/10) # top 50%
temp$Concentration_3 = 10^(((all_data_slope * temp$Mean_Sv + all_data_intercept)- cfin_TS)/10) # all data
temp$Concentration_4 = 10^(((temp$Mean_Sv)- cfin_TS)/10) # no correction

Echo_Concentration = data.frame(Depth = rep(temp$Depth_Bin, times=4), 
                                Concentration = c(temp$Concentration_1,
                                                  temp$Concentration_2,
                                                  temp$Concentration_3,
                                                  temp$Concentration_4),
                                Equation = rep(c("1","2","3","No correction"),each=length(temp$Depth)))
Echo_Concentration$Concentration[is.nan(Echo_Concentration$Concentration)] = 0
Echo_Concentration$Depth = seq(0,195,5)

GMB_Mid_Sv_Net_Intervals = data.frame()

for(k in 1:5) {
  if (k == 1) {
    temp = Echo_Concentration %>%
      group_by(Equation) %>%
      filter(Depth >= Intervals$depth[k]) %>%
      summarise(Sv_Mean = mean(Concentration))
    GMB_Mid_Sv_Net_Intervals = cbind(net=Intervals$Net_Num[k]+1,temp,Net_Mean = rep(rev(unique(GMB_Mid$Net_Concentration))[k+1],each=4))
  } else {
    temp = Echo_Concentration %>%
      group_by(Equation) %>%
      filter((Depth >= Intervals$depth[k]) & (Depth < Intervals$depth[k-1])) %>%
      summarise(Sv_Mean = mean(Concentration))
    GMB_Mid_Sv_Net_Intervals = rbind(GMB_Mid_Sv_Net_Intervals,cbind(net=Intervals$Net_Num[k]+1,temp,Net_Mean=rep(rev(unique(GMB_Mid$Net_Concentration))[k+1],each=4)))
  }
}
GMB_Mid_Sv_Net_Intervals$Ratio = GMB_Mid_Sv_Net_Intervals$Sv_Mean/GMB_Mid_Sv_Net_Intervals$Net_Mean


require(plyr)
Intervals = Net_Metadata[Net_Metadata$Station_Number %in% c(8, 12, 15),] %>% 
  group_by(Net_Num) %>%
  reframe(depth = round_any(mean(`Pressure_(dbar)`), 5, f = ceiling)) %>% slice(c(1:5))
detach("package:plyr")

temp = echo_grouped_sv[,c(1,7,10,12)] %>% 
  reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:4), na.rm = TRUE))
temp$Concentration_1 = 10^(((temp$Mean_Sv + slope_1_intercept)- cfin_TS)/10) # slope fixed at 1
temp$Concentration_2 = 10^(((top_50_slope * temp$Mean_Sv + top_50_intercept)- cfin_TS)/10) # top 50%
temp$Concentration_3 = 10^(((all_data_slope * temp$Mean_Sv + all_data_intercept)- cfin_TS)/10) # all data
temp$Concentration_4 = 10^(((temp$Mean_Sv)- cfin_TS)/10) # no correction

Echo_Concentration = data.frame(Depth = rep(temp$Depth_Bin, times=4), 
                                Concentration = c(temp$Concentration_1,
                                                  temp$Concentration_2,
                                                  temp$Concentration_3,
                                                  temp$Concentration_4),
                                Equation = rep(c("1","2","3","No correction"),each=length(temp$Depth)))
Echo_Concentration$Concentration[is.nan(Echo_Concentration$Concentration)] = 0
Echo_Concentration$Depth = seq(0,195,5)

GMB_Deep_Sv_Net_Intervals = data.frame()

for(k in 1:5) {
  if (k == 1) {
    temp = Echo_Concentration %>%
      group_by(Equation) %>%
      filter(Depth >= Intervals$depth[k]) %>%
      summarise(Sv_Mean = mean(Concentration))
    GMB_Deep_Sv_Net_Intervals = cbind(net=Intervals$Net_Num[k]+1,temp,Net_Mean = rep(rev(unique(GMB_Deep$Net_Concentration))[k+1],each=4))
  } else {
    temp = Echo_Concentration %>%
      group_by(Equation) %>%
      filter((Depth >= Intervals$depth[k]) & (Depth < Intervals$depth[k-1])) %>%
      summarise(Sv_Mean = mean(Concentration))
    GMB_Deep_Sv_Net_Intervals = rbind(GMB_Deep_Sv_Net_Intervals,cbind(net=Intervals$Net_Num[k]+1,temp,Net_Mean=rep(rev(unique(GMB_Deep$Net_Concentration))[k+1],each=4)))
  }
}
GMB_Deep_Sv_Net_Intervals$Ratio = GMB_Deep_Sv_Net_Intervals$Sv_Mean/GMB_Deep_Sv_Net_Intervals$Net_Mean

GMB_Shallow_Sv_Net_Intervals = GMB_Shallow_Sv_Net_Intervals %>% filter(Ratio != Inf & Ratio != 0)
GMB_Mid_Sv_Net_Intervals = GMB_Mid_Sv_Net_Intervals %>% filter(Ratio != Inf & Ratio != 0)
GMB_Deep_Sv_Net_Intervals = GMB_Deep_Sv_Net_Intervals %>% filter(Ratio != Inf & Ratio != 0)

# rbind(GMB_Shallow_Sv_Net_Intervals,
#       GMB_Mid_Sv_Net_Intervals,
#       GMB_Deep_Sv_Net_Intervals) %>%
#   filter(Ratio != 0) %>%
#   reframe(mean(log10(Ratio)), sd(log10(Ratio)))

#####
require(plyr)
Intervals = Net_Metadata[Net_Metadata$Station_Number %in% c(5,20),] %>% 
  group_by(Net_Num) %>%
  reframe(depth = round_any(mean(`Pressure_(dbar)`), 5, f = ceiling)) %>% slice(c(1:5))
detach("package:plyr")

temp = echo_grouped_sv[,c(1,4,17)] %>% 
  reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:3), na.rm = TRUE))
temp$Concentration_1 = 10^(((temp$Mean_Sv + slope_1_intercept)- cfin_TS)/10) # slope fixed at 1
temp$Concentration_2 = 10^(((top_50_slope * temp$Mean_Sv + top_50_intercept)- cfin_TS)/10) # top 50%
temp$Concentration_3 = 10^(((all_data_slope * temp$Mean_Sv + all_data_intercept)- cfin_TS)/10) # all data
temp$Concentration_4 = 10^(((temp$Mean_Sv)- cfin_TS)/10) # no correction

Echo_Concentration = data.frame(Depth = rep(temp$Depth_Bin, times=4), 
                                Concentration = c(temp$Concentration_1,
                                                  temp$Concentration_2,
                                                  temp$Concentration_3,
                                                  temp$Concentration_4),
                                Equation = rep(c("1","2","3","No correction"),each=length(temp$Depth)))
Echo_Concentration$Concentration[is.nan(Echo_Concentration$Concentration)] = 0
Echo_Concentration$Depth = seq(0,195,5)

OB_Shallow_Sv_Net_Intervals = data.frame()

for(k in 1:5) {
  if (k == 1) {
    temp = Echo_Concentration %>%
      group_by(Equation) %>%
      filter(Depth >= Intervals$depth[k]) %>%
      summarise(Sv_Mean = mean(Concentration))
    OB_Shallow_Sv_Net_Intervals = cbind(net=Intervals$Net_Num[k]+1,temp,Net_Mean = rep(rev(unique(OB_Shallow$Net_Concentration))[k+1],each=4))
  } else {
    temp = Echo_Concentration %>%
      group_by(Equation) %>%
      filter((Depth >= Intervals$depth[k]) & (Depth < Intervals$depth[k-1])) %>%
      summarise(Sv_Mean = mean(Concentration))
    OB_Shallow_Sv_Net_Intervals = rbind(OB_Shallow_Sv_Net_Intervals,cbind(net=Intervals$Net_Num[k]+1,temp,Net_Mean=rep(rev(unique(OB_Shallow$Net_Concentration))[k+1],each=4)))
  }
}
OB_Shallow_Sv_Net_Intervals$Ratio = OB_Shallow_Sv_Net_Intervals$Sv_Mean/OB_Shallow_Sv_Net_Intervals$Net_Mean


require(plyr)
Intervals = Net_Metadata[Net_Metadata$Station_Number %in% c(3,7,18),] %>% 
  group_by(Net_Num) %>%
  reframe(depth = round_any(mean(`Pressure_(dbar)`), 5, f = ceiling)) %>% slice(c(1:5))
detach("package:plyr")

temp = echo_grouped_sv[,c(1,2,6,15)] %>% 
  reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:4), na.rm = TRUE))
temp$Concentration_1 = 10^(((temp$Mean_Sv + slope_1_intercept)- cfin_TS)/10) # slope fixed at 1
temp$Concentration_2 = 10^(((top_50_slope * temp$Mean_Sv + top_50_intercept)- cfin_TS)/10) # top 50%
temp$Concentration_3 = 10^(((all_data_slope * temp$Mean_Sv + all_data_intercept)- cfin_TS)/10) # all data
temp$Concentration_4 = 10^(((temp$Mean_Sv)- cfin_TS)/10) # no correction

Echo_Concentration = data.frame(Depth = rep(temp$Depth_Bin, times=4), 
                                Concentration = c(temp$Concentration_1,
                                                  temp$Concentration_2,
                                                  temp$Concentration_3,
                                                  temp$Concentration_4),
                                Equation = rep(c("1","2","3","No correction"),each=length(temp$Depth)))
Echo_Concentration$Concentration[is.nan(Echo_Concentration$Concentration)] = 0
Echo_Concentration$Depth = seq(0,195,5)

OB_Mid_Sv_Net_Intervals = data.frame()

for(k in 1:5) {
  if (k == 1) {
    temp = Echo_Concentration %>%
      group_by(Equation) %>%
      filter(Depth >= Intervals$depth[k]) %>%
      summarise(Sv_Mean = mean(Concentration))
    OB_Mid_Sv_Net_Intervals = cbind(net=Intervals$Net_Num[k]+1,temp,Net_Mean = rep(rev(unique(OB_Mid$Net_Concentration))[k+1],each=4))
  } else {
    temp = Echo_Concentration %>%
      group_by(Equation) %>%
      filter((Depth >= Intervals$depth[k]) & (Depth < Intervals$depth[k-1])) %>%
      summarise(Sv_Mean = mean(Concentration))
    OB_Mid_Sv_Net_Intervals = rbind(OB_Mid_Sv_Net_Intervals,cbind(net=Intervals$Net_Num[k]+1,temp,Net_Mean=rep(rev(unique(OB_Mid$Net_Concentration))[k+1],each=4)))
  }
}
OB_Mid_Sv_Net_Intervals$Ratio = OB_Mid_Sv_Net_Intervals$Sv_Mean/OB_Mid_Sv_Net_Intervals$Net_Mean


require(plyr)
Intervals = Net_Metadata[Net_Metadata$Station_Number %in% c(4, 6, 19),] %>% 
  group_by(Net_Num) %>%
  reframe(depth = round_any(mean(`Pressure_(dbar)`), 5, f = ceiling)) %>% slice(c(1:5))
detach("package:plyr")

temp = echo_grouped_sv[,c(1,3,5,16)] %>% 
  reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:4), na.rm = TRUE))
temp$Concentration_1 = 10^(((temp$Mean_Sv + slope_1_intercept)- cfin_TS)/10) # slope fixed at 1
temp$Concentration_2 = 10^(((top_50_slope * temp$Mean_Sv + top_50_intercept)- cfin_TS)/10) # top 50%
temp$Concentration_3 = 10^(((all_data_slope * temp$Mean_Sv + all_data_intercept)- cfin_TS)/10) # all data
temp$Concentration_4 = 10^(((temp$Mean_Sv)- cfin_TS)/10) # no correction

Echo_Concentration = data.frame(Depth = rep(temp$Depth_Bin, times=4), 
                                Concentration = c(temp$Concentration_1,
                                                  temp$Concentration_2,
                                                  temp$Concentration_3,
                                                  temp$Concentration_4),
                                Equation = rep(c("1","2","3","No correction"),each=length(temp$Depth)))
Echo_Concentration$Concentration[is.nan(Echo_Concentration$Concentration)] = 0
Echo_Concentration$Depth = seq(0,195,5)

OB_Deep_Sv_Net_Intervals = data.frame()

for(k in 1:5) {
  if (k == 1) {
    temp = Echo_Concentration %>%
      group_by(Equation) %>%
      filter(Depth >= Intervals$depth[k]) %>%
      summarise(Sv_Mean = mean(Concentration))
    OB_Deep_Sv_Net_Intervals = cbind(net=Intervals$Net_Num[k]+1,temp,Net_Mean = rep(rev(unique(OB_Deep$Net_Concentration))[k+1],each=4))
  } else {
    temp = Echo_Concentration %>%
      group_by(Equation) %>%
      filter((Depth >= Intervals$depth[k]) & (Depth < Intervals$depth[k-1])) %>%
      summarise(Sv_Mean = mean(Concentration))
    OB_Deep_Sv_Net_Intervals = rbind(OB_Deep_Sv_Net_Intervals,cbind(net=Intervals$Net_Num[k]+1,temp,Net_Mean=rep(rev(unique(OB_Deep$Net_Concentration))[k+1],each=4)))
  }
}
OB_Deep_Sv_Net_Intervals$Ratio = OB_Deep_Sv_Net_Intervals$Sv_Mean/OB_Deep_Sv_Net_Intervals$Net_Mean

OB_Shallow_Sv_Net_Intervals = OB_Shallow_Sv_Net_Intervals %>% filter(Ratio != Inf & Ratio !=0)
OB_Mid_Sv_Net_Intervals = OB_Mid_Sv_Net_Intervals %>% filter(Ratio != Inf & Ratio !=0)
OB_Deep_Sv_Net_Intervals = OB_Deep_Sv_Net_Intervals %>% filter(Ratio != Inf & Ratio !=0)

#####
## Putting the bias figure together

GMB_Sv_Net_Intervals = cbind(rbind(GMB_Shallow_Sv_Net_Intervals, GMB_Mid_Sv_Net_Intervals, GMB_Deep_Sv_Net_Intervals), Basin = "Grand Manan Basin")
OB_Sv_Net_Intervals = cbind(rbind(OB_Shallow_Sv_Net_Intervals, OB_Mid_Sv_Net_Intervals, OB_Deep_Sv_Net_Intervals), Basin = "Owen Basin")

Sv_Net_Intervals = rbind(GMB_Sv_Net_Intervals, OB_Sv_Net_Intervals)

# Individual ratios
plot1 = ggplot() +
  geom_point(data=Sv_Net_Intervals,aes(x=as.factor(Equation),
                                       y=Ratio, 
                                       shape=as.factor(net), color=as.factor(net)),
             position=position_jitter(width=0.1,height=0,seed=1),
             size = 3) +
  scale_shape_discrete(limits = c("1","2","3","4","5"),
                       drop = F,
                       labels = c("1","2","3","4","5")) +
  scale_y_log10(limits=c(0.03,10000),
                name="Predicted C5 Abundance /\nNet C5 Abundance") +
  theme_bw() +
  coord_cartesian(xlim=c(0.5,4.5)) +
  scale_x_discrete(position = "top",
                   limits = c("1","2","3","No correction"),
                   drop = T,
                   labels = c("Slope = 1","Top 50%\nof Echo Data","All Echo Data","No Correction")) +
  scale_color_viridis_d(begin=0.5,
                        limits = c("1","2","3","4","5"),
                        drop = F,
                        labels = c("1","2","3","4","5")) +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=10),
        panel.grid.major.x = element_blank(),
        text = element_text(size=16)) +
  labs(shape="Net",color="Net") +
  facet_wrap(~Basin, nrow = 2)

# Mean/SD of log-transformed ratios

Sv_Net_Intervals_1 = Sv_Net_Intervals %>%
  filter(Ratio != 0) %>%
  group_by(Basin) %>%
  reframe(Ratio_Mean = mean(log10(Ratio)), Ratio_SD = sd(log10(Ratio))) %>%
  mutate(Method = "Overall")

Sv_Net_Intervals_2 = Sv_Net_Intervals %>%
  filter(Ratio != 0) %>%
  filter(Equation != "No correction") %>%
  group_by(Basin) %>%
  reframe(Ratio_Mean = mean(log10(Ratio)), Ratio_SD = sd(log10(Ratio))) %>%
  mutate(Method = "Corrected Only")

Sv_Net_Intervals_Log10 = rbind(Sv_Net_Intervals_1, Sv_Net_Intervals_2)

plot2 = ggplot() +
  geom_point(data = Sv_Net_Intervals_Log10, aes(x = Method, y = Ratio_Mean, color = Method, group = Method),
             size=3) +
  scale_color_manual(values = c("blue", "red"), guide = "none") +
  geom_errorbar(data = Sv_Net_Intervals_Log10, 
                aes(x = Method, ymin = Ratio_Mean - Ratio_SD, ymax = Ratio_Mean + Ratio_SD),
                width = 0.1,
                linewidth = 0.75) +
  theme_bw() +
  scale_x_discrete(position = "top",
                   limits = c("Overall", "Corrected Only"),
                   drop = F,
                   labels = c("Overall Mean and SD", "Corrected Mean and SD")) +
  scale_y_continuous(limits = c(-1, 2),
    name="log10(Predicted C5 Abundance /\nNet C5 Abundance)") +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=10),
        panel.grid.major.x = element_blank(),
        text = element_text(size=16)) +
  facet_wrap(~Basin, nrow = 2)
  

ggarrange(plot1, plot2, ncol = 2, common.legend = F, legend = "bottom", labels="AUTO") +
  theme(plot.background = element_rect(fill = 'white',color='white'))
ggsave(paste0(figure_dir, "Both_Basins_Concentration_Ratio.png"),height=8,width=12,units="in")

#####

## Boxplot of missed echoes

Missed_Echoes = read.csv(paste0(data_dir,'Spreadsheets/Missed_Echoes.csv'),header=F)

Missed_Echoes_GMB = Missed_Echoes %>% 
  filter(V2 == "GMB") %>% 
  select(V3:V25) %>%
  gather()
Missed_Echoes_GMB$key = rep(c("10m","5m"),times=23)

Missed_Echoes_GMB %>% group_by(key) %>% reframe(median(value))


Missed_Echoes_OB = Missed_Echoes %>% 
  filter(V2 == "OB") %>% 
  select(V3:V25) %>%
  gather()
Missed_Echoes_OB$key = rep(c("10m","5m"),times=23)

Missed_Echoes_OB %>% group_by(key) %>% reframe(median(value))


Missed_Echoes_Combined = cbind(basin = rep(c("GMB","OB"),each=46), rbind(Missed_Echoes_GMB,Missed_Echoes_OB))

ggplot(data = Missed_Echoes_Combined, aes(x = basin, y = value)) +
  geom_boxplot(aes(fill = key), linewidth = 0.75) +
  scale_fill_viridis_d(begin = 0.5) +
  stat_summary(aes(group = key), fun = mean, geom = "point", show.legend = F, position = position_dodge(0.75), shape=4, size=4, stroke = 1) +
  labs(x = "Basin",
       y = "Percentage of Volume Backscatter Near Seafloor",
       fill = "Distance Threshold") +
  theme_bw()

ggsave(paste0(figure_dir,"Missed_Echoes_Combined.png"))

#####

## OPC data variances

sd(c(OB_Shallow$OPC_Concentration[17:nrow(OB_Shallow)],
  OB_Mid$OPC_Concentration[17:nrow(OB_Mid)],
  OB_Deep$OPC_Concentration[17:nrow(OB_Deep)]), na.rm = T)

sd(c(GMB_Shallow$OPC_Concentration[17:nrow(GMB_Shallow)],
     GMB_Mid$OPC_Concentration[17:nrow(GMB_Mid)],
     GMB_Deep$OPC_Concentration[17:nrow(GMB_Deep)]), na.rm = T)

## Echosounder data variances

Sv_Net_Intervals %>%
  group_by(Basin, Equation) %>%
  reframe(mean(Sv_Mean), sd(Sv_Mean), mean(Net_Mean), sd(Net_Mean))
