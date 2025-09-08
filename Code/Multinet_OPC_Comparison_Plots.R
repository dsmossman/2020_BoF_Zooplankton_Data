#####
## Initialization

setwd("C:/Users/dmossman/Box/2022 MSc Thesis Work")

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

## Directories

processed_dir = 'Processed_Data/'
data_dir = 'Raw_Data/'
figure_dir = 'Visuals/'
report_dir = getwd()

## Loading in file structures

load(paste0(processed_dir, 'Net_Metadata.rda'))
load(paste0(processed_dir, "Cfin_Stages.rda"))
load(paste0(processed_dir, 'OPC_Data_Full.rda'))

## OPC stuff

OPC_to_C5 = function(concentration, speed) {
  B0 = 0.0384
  B1 = 0.5343
  B2 = 0.8001
  
  temp = (1/B1) * (log10(concentration) - B0 - B2 * speed)
  return(10^temp)
  
}

dz = 5
min_size = 1.5
max_size = 2

#####

## GMB station data

stations = c(8, 12, 15)
OPC_casts = list(c(13, 14),c(21,22),c(27,28))

GMB_Deep_Net_OPC_Comparison = list()

for(i in 1:3) {
  temp1 = stages %>% filter(station == stations[i] & stage %in% c("CIV","CV","CVIF")) %>%
    group_by(net, stage) %>%
    reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))
  
  require(plyr)
  temp1$Intervals = Net_Metadata[Net_Metadata$Station_Number == stations[i],] %>% 
    group_by(Net_Num) %>%
    reframe(depth = rep(round_any(mean(`Pressure_(dbar)`), 5, f = ceiling), each = 3)) %>% slice(c(1:15))
  detach("package:plyr")
  
  temp1 = temp1 %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))
  
  temp2 = OPC_Data_Full %>% filter(OPC_cast_num %in% OPC_casts[[i]]) %>%
    filter(depth>1) %>%
    filter(flag != 'depth') %>%
    group_by(OPC_cast_num, basin) %>%
    reframe(opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = T,
                          reject_volume = T),
            speed = opc_speed(.),
            C5_concentration = OPC_to_C5(concentration, speed)) %>%
    group_by(depth) %>%
    reframe(C5_concentration= mean(C5_concentration))
  
  temp3 = data.frame(Depth_Bin = seq(0,195,5), Net_Concentration = NA, OPC_Concentration = NA)
  
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
  
  GMB_Deep_Net_OPC_Comparison[[i]] = temp3 %>% transform(Depth_Bin=cut(Depth_Bin, seq(0,200,5),right=F))
  
}

station8_plot = ggplot(data=GMB_Deep_Net_OPC_Comparison[[1]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,12000)) +
  scale_y_break(breaks = c(1000,2000),scales=0.25,ticklabels=seq(6000,12000,6000)) +
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
  scale_y_continuous(limits = c(0,12000)) +
  scale_y_break(breaks = c(1000,2000),scales=0.25,ticklabels=seq(6000,12000,6000)) +
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
  scale_y_continuous(limits = c(0,12000)) +
  scale_y_break(breaks = c(1000,2000),scales=0.25,ticklabels=seq(6000,12000,6000)) +
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
  temp1 = stages %>% filter(station == stations[i] & stage %in% c("CIV","CV","CVIF")) %>%
    group_by(net, stage) %>%
    reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))
  
  require(plyr)
  temp1$Intervals = Net_Metadata[Net_Metadata$Station_Number == stations[i],] %>% 
    group_by(Net_Num) %>%
    reframe(depth = rep(round_any(mean(`Pressure_(dbar)`), 5, f = ceiling), each = 3)) %>% slice(c(1:15))
  detach("package:plyr")
  
  temp1 = temp1 %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))
  
  temp2 = OPC_Data_Full %>% filter(OPC_cast_num %in% OPC_casts[[i]]) %>%
    filter(depth>1) %>%
    filter(flag != 'depth') %>%
    group_by(OPC_cast_num, basin) %>%
    reframe(opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = T,
                          reject_volume = T),
            speed = opc_speed(.),
            C5_concentration = OPC_to_C5(concentration, speed)) %>%
    group_by(depth) %>%
    reframe(C5_concentration= mean(C5_concentration))
  
  temp3 = data.frame(Depth_Bin = seq(0,195,5), Net_Concentration = NA, OPC_Concentration = NA)
  
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
  
  GMB_Mid_Net_OPC_Comparison[[i]] = temp3  %>% transform(Depth_Bin=cut(Depth_Bin, seq(0,200,5),right=F))
  
}

station9_plot = ggplot(data=GMB_Mid_Net_OPC_Comparison[[1]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,12000)) +
  scale_y_break(breaks = c(1000,2000),scales=0.25,ticklabels=seq(6000,12000,6000)) +
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
  scale_y_continuous(limits = c(0,12000)) +
  scale_y_break(breaks = c(1000,2000),scales=0.25,ticklabels=seq(6000,12000,6000)) +
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
  temp1 = stages %>% filter(station == stations[i] & stage %in% c("CIV","CV","CVIF")) %>%
    group_by(net, stage) %>%
    reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))
  
  require(plyr)
  temp1$Intervals = Net_Metadata[Net_Metadata$Station_Number == stations[i],] %>% 
    group_by(Net_Num) %>%
    reframe(depth = rep(round_any(mean(`Pressure_(dbar)`), 5, f = ceiling), each = 3)) %>% slice(c(1:15))
  detach("package:plyr")
  
  temp1 = temp1 %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))
  
  temp2 = OPC_Data_Full %>% filter(OPC_cast_num %in% OPC_casts[[i]]) %>%
    filter(depth>1) %>%
    filter(flag != 'depth') %>%
    group_by(OPC_cast_num, basin) %>%
    reframe(opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = T,
                          reject_volume = T),
            speed = opc_speed(.),
            C5_concentration = OPC_to_C5(concentration, speed)) %>%
    group_by(depth) %>%
    reframe(C5_concentration= mean(C5_concentration))
  
  temp3 = data.frame(Depth_Bin = seq(0,195,5), Net_Concentration = NA, OPC_Concentration = NA)
  
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
  
  GMB_Shallow_Net_OPC_Comparison[[i]] = temp3  %>% transform(Depth_Bin=cut(Depth_Bin, seq(0,200,5),right=F))
  
}

station10_plot = ggplot(data=GMB_Shallow_Net_OPC_Comparison[[1]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,12000)) +
  scale_y_break(breaks = c(1000,2000),scales=0.25,ticklabels=seq(6000,12000,6000)) +
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
  scale_y_continuous(limits = c(0,12000)) +
  scale_y_break(breaks = c(1000,2000),scales=0.25,ticklabels=seq(6000,12000,6000)) +
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
  scale_y_continuous(limits = c(0,12000)) +
  scale_y_break(breaks = c(1000,2000),scales=0.25,ticklabels=seq(6000,12000,6000)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.text.x.top = element_blank()) +
  labs(y="Concentration (individuals/m^3)",x="Depth (m)")
ggsave(file=paste0(figure_dir,"Net_OPC_Depth_Profiles/","Station_17_Net_OPC_Comparison.png"),
       scale=2)

#####
## OB station data

stations = c(4, 6, 19)
OPC_casts = list(c(5),c(),c(35,36))
dz = 5
min_size = 1.5
max_size = 2

OB_Deep_Net_OPC_Comparison = list()

for(i in 1:3) {
  temp1 = stages %>% filter(station == stations[i] & stage %in% c("CIV","CV","CVIF")) %>%
    group_by(net, stage) %>%
    reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))
  # Note from Delphine: the below code is because some of the nets in OB did not
  # have ANY C. fin in them, so I had to manually add those rows in with biomass/
  # concentration values of 0
  # Your data might be different but I left the bones here just in case
  
  # if(i != 3) {
  #   temp1 = rbind(temp1, data.frame(net=c(4,4,4),
  #                                   stage=c("CIV","CV","CVIF"),
  #                                   cfin_biomass=0,
  #                                   cfin_concentration=0))
  #   temp1 = temp1[order(temp1$net),]
  # } else {
  #   temp1 = rbind(temp1, data.frame(net=c(4,4,4),
  #                                   stage=c("CIV","CV","CVIF"),
  #                                   cfin_biomass=0,
  #                                   cfin_concentration=0),
  #                 data.frame(net=c(5,5,5),
  #                            stage=c("CIV","CV","CVIF"),
  #                            cfin_biomass=0,
  #                            cfin_concentration=0))
  #   temp1 = temp1[order(temp1$net),]
  # }
  
  require(plyr)
  temp1$Intervals = Net_Metadata[Net_Metadata$Station_Number == stations[i],] %>% 
    group_by(Net_Num) %>%
    reframe(depth = rep(round_any(mean(`Pressure_(dbar)`), 5, f = ceiling), each = 3)) %>% slice(c(1:15))
  detach("package:plyr")
  
  temp1 = temp1 %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))
  
  temp2 = OPC_Data_Full %>% filter(OPC_cast_num %in% OPC_casts[[i]]) %>%
    filter(depth>1) %>%
    filter(flag != 'depth') %>%
    group_by(OPC_cast_num, basin)
  
  
  if(nrow(temp2) == 0) {
    temp2 = data.frame(C5_concentration = rep(0,times=40))
  } else {
    temp2 = temp2 %>%
      reframe(opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = T,
                            reject_volume = T),
              speed = opc_speed(.),
              C5_concentration = OPC_to_C5(concentration, speed)) %>%
      group_by(depth) %>%
      reframe(C5_concentration= mean(C5_concentration))
  }
  
  temp3 = data.frame(Depth_Bin = seq(0,195,5), Net_Concentration = NA, OPC_Concentration = NA)
  
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
  
  OB_Deep_Net_OPC_Comparison[[i]] = temp3  %>% transform(Depth_Bin=cut(Depth_Bin, seq(0,200,5),right=F))
  
}

station4_plot = ggplot(data=OB_Deep_Net_OPC_Comparison[[1]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,12000)) +
  scale_y_break(breaks = c(1000,2000),scales=0.25,ticklabels=seq(6000,12000,6000)) +
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
  scale_y_continuous(limits = c(0,12000)) +
  scale_y_break(breaks = c(1000,2000),scales=0.25,ticklabels=seq(6000,12000,6000)) +
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
  scale_y_continuous(limits = c(0,12000)) +
  scale_y_break(breaks = c(1000,2000),scales=0.25,ticklabels=seq(6000,12000,6000)) +
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
  temp1 = stages %>% filter(station == stations[i] & stage %in% c("CIV","CV","CVIF")) %>%
    group_by(net, stage) %>%
    reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))
  
  # if(i == 2) {
  #   temp1 = rbind(temp1, data.frame(net=c(3,3,3),
  #                                   stage=c("CIV","CV","CVIF"),
  #                                   cfin_biomass=0,
  #                                   cfin_concentration=0),
  #                 data.frame(net=c(4,4,4),
  #                            stage=c("CIV","CV","CVIF"),
  #                            cfin_biomass=0,
  #                            cfin_concentration=0))
  #   temp1 = temp1[order(temp1$net),]
  # } else if(i == 3) {
  #   temp1 = rbind(temp1, data.frame(net=c(5,5,5),
  #                                   stage=c("CIV","CV","CVIF"),
  #                                   cfin_biomass=0,
  #                                   cfin_concentration=0))
  #   temp1 = temp1[order(temp1$net),]
  # }
  
  require(plyr)
  temp1$Intervals = Net_Metadata[Net_Metadata$Station_Number == stations[i],] %>% 
    group_by(Net_Num) %>%
    reframe(depth = rep(round_any(mean(`Pressure_(dbar)`), 5, f = ceiling), each = 3)) %>% slice(c(1:15))
  detach("package:plyr")
  
  temp1 = temp1 %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))
  
  temp2 = OPC_Data_Full %>% filter(OPC_cast_num %in% OPC_casts[[i]]) %>%
    filter(depth>1) %>%
    filter(flag != 'depth') %>%
    group_by(OPC_cast_num, basin)
  
  
  if(nrow(temp2) == 0) {
    temp2 = data.frame(C5_concentration = rep(0,times=40))
  } else {
    temp2 = temp2 %>%
      reframe(opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = T,
                            reject_volume = T),
              speed = opc_speed(.),
              C5_concentration = OPC_to_C5(concentration, speed)) %>%
      group_by(depth) %>%
      reframe(C5_concentration = mean(C5_concentration))
  }
  
  temp3 = data.frame(Depth_Bin = seq(0,195,5), Net_Concentration = NA, OPC_Concentration = NA)
  
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
  
  OB_Mid_Net_OPC_Comparison[[i]] = temp3  %>% transform(Depth_Bin=cut(Depth_Bin, seq(0,200,5),right=F))
  
}

station3_plot = ggplot(data=OB_Mid_Net_OPC_Comparison[[1]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,12000)) +
  scale_y_break(breaks = c(1000,2000),scales=0.25,ticklabels=seq(6000,12000,6000)) +
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
  scale_y_continuous(limits = c(0,12000)) +
  scale_y_break(breaks = c(1000,2000),scales=0.25,ticklabels=seq(6000,12000,6000)) +
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
  scale_y_continuous(limits = c(0,12000)) +
  scale_y_break(breaks = c(1000,2000),scales=0.25,ticklabels=seq(6000,12000,6000)) +
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
  temp1 = stages %>% filter(station == stations[i] & stage %in% c("CIV","CV","CVIF")) %>%
    group_by(net, stage) %>%
    reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))
  
  # if(i == 1) {
  #   temp1 = rbind(temp1, data.frame(net=c(4,4,4),
  #                                   stage=c("CIV","CV","CVIF"),
  #                                   cfin_biomass=0,
  #                                   cfin_concentration=0))
  #   temp1 = temp1[order(temp1$net),]
  # } else if(i == 2) {
  #   temp1 = rbind(temp1, data.frame(net=c(4,4,4),
  #                                   stage=c("CIV","CV","CVIF"),
  #                                   cfin_biomass=0,
  #                                   cfin_concentration=0),
  #                 data.frame(net=c(5,5,5),
  #                            stage=c("CIV","CV","CVIF"),
  #                            cfin_biomass=0,
  #                            cfin_concentration=0))
  #   temp1 = temp1[order(temp1$net),]
  # }
  
  require(plyr)
  temp1$Intervals = Net_Metadata[Net_Metadata$Station_Number == stations[i],] %>% 
    group_by(Net_Num) %>%
    reframe(depth = rep(round_any(mean(`Pressure_(dbar)`), 5, f = ceiling), each = 3)) %>% slice(c(1:15))
  detach("package:plyr")
  
  temp1 = temp1 %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))
  
  temp2 = OPC_Data_Full %>% filter(OPC_cast_num %in% OPC_casts[[i]]) %>%
    filter(depth>1) %>%
    filter(flag != 'depth') %>%
    group_by(OPC_cast_num, basin)
  
  
  if(nrow(temp2) == 0) {
    temp2 = data.frame(C5_concentration = rep(0,times=40))
  } else {
    temp2 = temp2 %>%
      reframe(opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = T,
                            reject_volume = T),
              speed = opc_speed(.),
              C5_concentration = OPC_to_C5(concentration, speed)) %>%
      group_by(depth) %>%
      reframe(C5_concentration = mean(C5_concentration))
  }
  
  temp3 = data.frame(Depth_Bin = seq(0,195,5), Net_Concentration = NA, OPC_Concentration = NA)
  
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
  
  OB_Shallow_Net_OPC_Comparison[[i]] = temp3  %>% transform(Depth_Bin=cut(Depth_Bin, seq(0,200,5),right=F))
  
}

station5_plot = ggplot(data=OB_Shallow_Net_OPC_Comparison[[1]], aes(x=factor(Depth_Bin))) +
  geom_col(aes(y=OPC_Concentration)) +
  geom_step(aes(y=Net_Concentration, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits=rev,breaks=GMB_Deep_Net_OPC_Comparison[[1]]$Depth_Bin[c(seq(1,40,3))]) +
  scale_y_continuous(limits = c(0,12000)) +
  scale_y_break(breaks = c(1000,2000),scales=0.25,ticklabels=seq(6000,12000,6000)) +
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
  scale_y_continuous(limits = c(0,12000)) +
  scale_y_break(breaks = c(1000,2000),scales=0.25,ticklabels=seq(6000,12000,6000)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),         axis.ticks.x.top = element_blank(),         axis.text.x.top = element_blank()) +
  labs(y="Concentration (individuals/m^3)",x="Depth (m)")
ggsave(file=paste0(figure_dir,"Net_OPC_Depth_Profiles/","Station_20_Net_OPC_Comparison.png"),
       scale=2)