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

sourceDirectory('Code/Hansen Zooplankton Code and Sample Data/src',
                modifiedOnly = F)

function_dir = 'Code/Hansen Zooplankton Code and Sample Data/src/'
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

#####

## Community composition pie charts

Pie_Chart_Data = Net_Data %>% 
  group_by(station,site,net) %>% 
  reframe(community_comp = first(community_comp)) %>%
  group_by(site,community_comp) %>%
  tally
Pie_Chart_Data$n = Pie_Chart_Data$n/40 * 100

ggplot(data=Pie_Chart_Data,aes(x="",y=n,fill=community_comp)) +
  geom_bar(stat="identity",width=1) +
  scale_fill_viridis_d(begin = 0.2, end = 1) +
  coord_polar("y",start=0) +
  theme_void() +
  geom_text(aes(label = paste0(n, "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = "Community Composition") +
  facet_wrap(~site) +
  theme(plot.background = element_rect(fill = 'white',color='white'))

fname=paste0(figure_dir,'Community_Comp_Pie_Chart.png')
ggsave(filename=fname,scale=2)

#####

## Comparison bar graphs

# Group by "station" (deep, mid, shallow) and basin (GMB, OB), and put into 5 m depth bins
# This will probably need to be done manually/by hand

# Station numbers:
# GMB shallow: 10, 13, 17, seafloor depth 158
# OB shallow: 5, 20, seafloor depth 110

# GMB mid: 9, 16, seafloor depth 160.5
# OB mid: 3, 7, 18, seafloor depth 127.3

# GMB deep: 8, 12, 15, seafloor depth 169.3
# OB deep: 4, 6, 19, seafloor depth 135

dz = 5
min_size = 1.5
max_size = 2

GMB_Shallow = OPC_Data_Full[OPC_Data_Full$OPC_cast_num %in% c(17,
                                                              23,
                                                              31, 32),] %>%
  filter(depth>1) %>%
  filter(flag != 'depth') %>%
  group_by(OPC_cast_num, basin) %>%
  reframe(
    opc_biomass(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = F,
                reject_volume = F)) %>%
  reframe(depth = depth, mass = mass) %>%
  group_by(depth) %>%
  reframe(mass = mean(mass) * 10^-4)
  
GMB_Mid = OPC_Data_Full[OPC_Data_Full$OPC_cast_num %in% c(15, 16,
                                                          29,30),] %>%
  filter(depth>1) %>%
  filter(flag != 'depth') %>%
  group_by(OPC_cast_num, basin) %>%
  reframe(
    opc_biomass(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = F,
                reject_volume = F)) %>%
  reframe(depth = depth, mass = mass) %>%
  group_by(depth) %>%
  reframe(mass = mean(mass) * 10^-4)

GMB_Deep = OPC_Data_Full[OPC_Data_Full$OPC_cast_num %in% c(13, 14,
                                                           21, 22,
                                                           27, 28),] %>%
  filter(depth>1) %>%
  filter(flag != 'depth') %>%
  group_by(OPC_cast_num, basin) %>%
  reframe(
    opc_biomass(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = F,
                reject_volume = F)) %>%
  reframe(depth = depth, mass = mass) %>%
  group_by(depth) %>%
  reframe(mass = mean(mass) * 10^-4)

OB_Shallow = OPC_Data_Full[OPC_Data_Full$OPC_cast_num %in% c(7, 8),] %>%
  filter(depth>1) %>%
  filter(flag != 'depth') %>%
  group_by(OPC_cast_num, basin) %>%
  reframe(
    opc_biomass(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = F,
                reject_volume = F)) %>%
  reframe(depth = depth, mass = mass) %>%
  group_by(depth) %>%
  reframe(mass = mean(mass) * 10^-4)

OB_Mid = OPC_Data_Full[OPC_Data_Full$OPC_cast_num %in% c(33),] %>%
  filter(depth>1) %>%
  filter(flag != 'depth') %>%
  group_by(OPC_cast_num, basin) %>%
  reframe(
    opc_biomass(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = F,
                reject_volume = F)) %>%
  reframe(depth = depth, mass = mass) %>%
  group_by(depth) %>%
  reframe(mass = mean(mass) * 10^-4)

OB_Deep = OPC_Data_Full[OPC_Data_Full$OPC_cast_num %in% c(5,
                                                          35, 36),] %>%
  filter(depth>1) %>%
  filter(flag != 'depth') %>%
  group_by(OPC_cast_num, basin) %>%
  reframe(
    opc_biomass(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = F,
                reject_volume = F)) %>%
  reframe(depth = depth, mass = mass) %>%
  group_by(depth) %>%
  reframe(mass = mean(mass) * 10^-4)

# Fill in the missing depth bins

DFs = sort(grep("OB_|GMB_",names(.GlobalEnv),value=TRUE))

OPC_biomass = data.frame(matrix(0, ncol=7, nrow=40))
OPC_biomass[,1] = (paste0(seq(0,195,5)))
names(OPC_biomass)[1] = "Depth_Bin"
n = 2

for(i in 1:length(DFs)) {
  test = get(DFs[i])
  for(j in 1:nrow(test)) {
    OPC_biomass[which(OPC_biomass$Depth_Bin == test$depth[j]),n] = test$mass[j]
  }
  names(OPC_biomass)[n] = DFs[i]
  n = n+1
}

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
    "/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/",
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

# Get the masked Sv values for each tow; these are in 1m depth bins already

for (i in 1:length(DFs)) {
  # for each day
  integration = get(DFs[i])
  tows = list()
  
  for (j in 1:length(integration)) {
    # for each tow
    tows[[j]] = list()
    
    # Get the full water column mask (not net-by-net) for 200-455 kHz
    
    tows[[j]][[1]] = 15.8 < 10 * log10(integration[[j]][[5]]) &
      16.3 > 10 * log10(integration[[j]][[5]])
    
    # Apply the mask back to the full water column 455 kHz and get rid of 0s and NAs
    
    tows[[j]][[2]] = integration[[j]][[7]] * tows[[j]][[1]]
    tows[[j]][[2]][tows[[j]][[2]] == 0] = NaN
    tows[[j]][[2]][is.na(tows[[j]][[2]])] = NaN
  }
  assign(paste0(paste0("Sept", dates[i], "_Masked")),tows)
  rm(tows,integration)
}

# Inverse problem for each tow's Sv values

DFs = sort(grep("Sept[0-9]{2}_Masked",names(.GlobalEnv),value=TRUE))
echo_masked_biomass = list()
cfin_TS = c(-129.7,	-122.3,	-108.3,	-99.9)

for(i in 1:length(DFs)) {
  # for each day
  masked = get(DFs[i])
  echo_masked_biomass[[i]] = list()
  for(j in 1:length(masked)) {
    # for each tow
    echo_masked_biomass[[i]][[j]] = list()
    temp = 10 * log10(masked[[j]][[2]])
    echo_masked_biomass[[i]][[j]][[1]] = 10^((temp - cfin_TS[3])/10) * 10^-6 * 268.3
    echo_masked_biomass[[i]][[j]][[2]] = 10^(0.58 + 0.04 * temp)
  }
}

# Group by "station" (deep, mid, shallow) and basin (GMB, OB), and put into 5 m depth bins
# This will probably need to be done manually/by hand

# Station numbers:
# GMB shallow: 10, 13, 17, seafloor depth 158
# OB shallow: 5, 20, seafloor depth 110

# GMB mid: 9, 16, seafloor depth 160.5
# OB mid: 3, 7, 18, seafloor depth 127.3

# GMB deep: 8, 12, 15, seafloor depth 169.3
# OB deep: 4, 6, 19, seafloor depth 135

echo_masked_biomass2 = data.frame(matrix(0, ncol=17, nrow=40))
echo_masked_biomass2[,1] = (paste0("[",seq(0,195,5),",",seq(5,200,5),")"))
names(echo_masked_biomass2)[1] = "Depth_Bin"
n = 2

for(k in 1:length(echo_masked_biomass)) {
  for(m in 1:length(echo_masked_biomass[[k]])) {
    test = as.data.frame(echo_masked_biomass[[k]][[m]][[2]])
    test = t(test)
    test[which(is.nan(test))] = NA
    test = test[, colSums(is.na(test)) < nrow(test)]
    
    # x = biomass value, y = depth
    
    test2 = data.frame()
    
    for (i in 1:nrow(test)) {
      for (j in 1:ncol(test)) {
        if (is.na(test[i, j] == TRUE)) {
          next
        } else
          test2 = rbind(test2, c(i + 4, test[i, j]))
        
      }
    }
    
    colnames(test2) = c("Depth", "Biomass")
    
    # Result: depth and mean biomass from glider dives at each multinet tow, in 5 m
    # depth bins
    test3 = test2 %>% 
      filter(!is_extreme(Biomass)) %>%
      transform(bin=cut(Depth, seq(0,200,5),right=F)) %>%
      group_by(bin) %>%
      reframe(Biomass = mean(Biomass))

    # Store these values in a dataframe
    for(p in 1:nrow(test3)) {
      echo_masked_biomass2[which(echo_masked_biomass2$Depth_Bin == test3$bin[p]),n] = test3$Biomass[p]
    }
    n = n + 1
  }
}

# Extremely messy reframing

names(echo_masked_biomass2)[2:ncol(echo_masked_biomass2)] = c(3, 4,
                                                              5, 6, 7,
                                                              8, 9, 10, 
                                                              12, 13,
                                                              15, 16, 17,
                                                              18, 19, 20)
echo_masked_biomass2$Depth_Bin = factor(echo_masked_biomass2$Depth_Bin, levels = echo_masked_biomass2$Depth_Bin)

GMB_Shallow = cbind(echo_masked_biomass2[,c(1,9,11,14)] %>% 
  mutate(Echo_Biomass = rowMeans(select(., 2:4), na.rm = TRUE)),
  OPC_biomass$GMB_Shallow)
names(GMB_Shallow)[ncol(GMB_Shallow)] = "OPC_Biomass"
GMB_Mid = cbind(echo_masked_biomass2[,c(1,8,13)] %>%
  mutate(Echo_Biomass = rowMeans(select(., 2:3), na.rm = TRUE)),
  OPC_biomass$GMB_Mid)
names(GMB_Mid)[ncol(GMB_Mid)] = "OPC_Biomass"
GMB_Deep = cbind(echo_masked_biomass2[,c(1,7,10,12)] %>%
  mutate(Echo_Biomass = rowMeans(select(., 2:4), na.rm = TRUE)),
  OPC_biomass$GMB_Deep)
names(GMB_Deep)[ncol(GMB_Deep)] = "OPC_Biomass"

OB_Shallow = cbind(echo_masked_biomass2[,c(1,4,17)] %>%
  mutate(Echo_Biomass = rowMeans(select(., 2:3), na.rm = TRUE)),
  OPC_biomass$OB_Shallow)
names(OB_Shallow)[ncol(OB_Shallow)] = "OPC_Biomass"
OB_Mid = cbind(echo_masked_biomass2[,c(1,2,6,15)] %>%
  mutate(Echo_Biomass = rowMeans(select(., 2:4), na.rm = TRUE)),
  OPC_biomass$OB_Mid)
names(OB_Mid)[ncol(OB_Mid)] = "OPC_Biomass"
OB_Deep = cbind(echo_masked_biomass2[,c(1,3,5,16)] %>%
  mutate(Echo_Biomass = rowMeans(select(., 2:4), na.rm = TRUE)),
  OPC_biomass$OB_Deep)
names(OB_Deep)[ncol(OB_Deep)] = "OPC_Biomass"

# Finally, the bar plots

GMB_Shallow_Plot = ggplot(data=GMB_Shallow, aes(x = Depth_Bin)) +
  geom_col(aes(y=Echo_Biomass,fill="Echosounder")) +
  geom_line(aes(y=rollmean(Echo_Biomass,5,na.pad=T,align="center"),group=NA)) +
  geom_segment(aes(x=Depth_Bin[which.max(rollmean(Echo_Biomass,5,na.pad=T,align="center"))],
                   xend=Depth_Bin[which.max(rollmean(Echo_Biomass,5,na.pad=T,align="center"))],
                   y=0,yend=0.025),
               linetype = "dashed") +
  geom_col(aes(y=-OPC_Biomass,fill="OPC")) +
  geom_line(aes(y=-rollmean(OPC_Biomass,5,na.pad=T,align="center"),group=NA)) +
  geom_segment(aes(x=Depth_Bin[which.max(rollmean(OPC_Biomass,5,na.pad=T,align="center"))],
                   xend=Depth_Bin[which.max(rollmean(OPC_Biomass,5,na.pad=T,align="center"))],
                   y=0,yend=-0.025),
               linetype = "dashed") +
  geom_vline(aes(xintercept=Depth_Bin[32]),show.legend = F,color="red") +
  annotate("text", x=GMB_Shallow$Depth_Bin[30],y=0.075,label="Mean seafloor depth",color="red") +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels=abs,limits=c(-0.025,0.025)) +
  scale_fill_viridis_d(begin=0,end=0.7) +
  theme_bw() +
  coord_flip(expand=F) +
  labs(y = "Mean Biomass (g/m^3)", x="Depth Bin", fill="Instrument")
fname = paste0(figure_dir,"GMB_Shallow_Echo_OPC_Comparsion_Plot.png")
ggsave(plot=GMB_Shallow_Plot,file=fname,scale=2)

GMB_Mid_Plot = ggplot(data=GMB_Mid, aes(x = Depth_Bin)) +
  geom_col(aes(y=Echo_Biomass,fill="Echosounder")) +
  geom_line(aes(y=rollmean(Echo_Biomass,5,na.pad=T,align="center"),group=NA)) +
  geom_segment(aes(x=Depth_Bin[which.max(rollmean(Echo_Biomass,5,na.pad=T,align="center"))],
                   xend=Depth_Bin[which.max(rollmean(Echo_Biomass,5,na.pad=T,align="center"))],
                   y=0,yend=0.025),
               linetype = "dashed") +
  geom_col(aes(y=-OPC_Biomass,fill="OPC")) +
  geom_line(aes(y=-rollmean(OPC_Biomass,5,na.pad=T,align="center"),group=NA)) +
  geom_segment(aes(x=Depth_Bin[which.max(rollmean(OPC_Biomass,5,na.pad=T,align="center"))],
                   xend=Depth_Bin[which.max(rollmean(OPC_Biomass,5,na.pad=T,align="center"))],
                   y=0,yend=-0.025),
               linetype = "dashed") +
  geom_vline(aes(xintercept=Depth_Bin[33]),show.legend = F,color="red") +
  annotate("text", x=GMB_Mid$Depth_Bin[31],y=0.075,label="Mean seafloor depth",color="red") +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels=abs,limits=c(-0.025,0.025)) +
  scale_fill_viridis_d(begin=0,end=0.7) +
  theme_bw() +
  coord_flip(expand=F) +
  labs(y = "Mean Biomass (g/m^3)", x="Depth Bin", fill="Instrument")
fname = paste0(figure_dir,"GMB_Mid_Echo_OPC_Comparsion_Plot.png")
ggsave(plot=GMB_Mid_Plot,file=fname,scale=2)

GMB_Deep_Plot = ggplot(data=GMB_Deep, aes(x = Depth_Bin)) +
  geom_col(aes(y=Echo_Biomass,fill="Echosounder")) +
  geom_line(aes(y=rollmean(Echo_Biomass,5,na.pad=T,align="center"),group=NA)) +
  geom_segment(aes(x=Depth_Bin[which.max(rollmean(Echo_Biomass,5,na.pad=T,align="center"))],
                   xend=Depth_Bin[which.max(rollmean(Echo_Biomass,5,na.pad=T,align="center"))],
                   y=0,yend=0.025),
               linetype = "dashed") +
  geom_col(aes(y=-OPC_Biomass,fill="OPC")) +
  geom_vline(aes(xintercept=Depth_Bin[34]),show.legend = F,color="red") +
  annotate("text", x=GMB_Deep$Depth_Bin[32],y=0.075,label="Mean seafloor depth",color="red") +
  geom_line(aes(y=-rollmean(OPC_Biomass,5,na.pad=T,align="center"),group=NA)) +
  geom_segment(aes(x=Depth_Bin[which.max(rollmean(OPC_Biomass,5,na.pad=T,align="center"))],
                   xend=Depth_Bin[which.max(rollmean(OPC_Biomass,5,na.pad=T,align="center"))],
                   y=0,yend=-0.025),
               linetype = "dashed") +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels=abs,limits=c(-0.025,0.025)) +
  scale_fill_viridis_d(begin=0,end=0.7) +
  theme_bw() +
  coord_flip(expand=F) +
  labs(y = "Mean Biomass (g/m^3)", x="Depth Bin", fill="Instrument")
fname = paste0(figure_dir,"GMB_Deep_Echo_OPC_Comparsion_Plot.png")
ggsave(plot=GMB_Deep_Plot,file=fname,scale=2)


OB_Shallow_Plot = ggplot(data=OB_Shallow, aes(x = Depth_Bin)) +
  geom_col(aes(y=Echo_Biomass,fill="Echosounder")) +
  geom_line(aes(y=rollmean(Echo_Biomass,5,na.pad=T,align="center"),group=NA)) +
  geom_segment(aes(x=Depth_Bin[which.max(rollmean(Echo_Biomass,5,na.pad=T,align="center"))],
                   xend=Depth_Bin[which.max(rollmean(Echo_Biomass,5,na.pad=T,align="center"))],
                   y=0,yend=0.025),
               linetype = "dashed") +
  geom_col(aes(y=-OPC_Biomass,fill="OPC")) +
  geom_line(aes(y=-rollmean(OPC_Biomass,5,na.pad=T,align="center"),group=NA)) +
  geom_segment(aes(x=Depth_Bin[which.max(rollmean(OPC_Biomass,5,na.pad=T,align="center"))],
                   xend=Depth_Bin[which.max(rollmean(OPC_Biomass,5,na.pad=T,align="center"))],
                   y=0,yend=-0.025),
               linetype = "dashed") +
  geom_vline(aes(xintercept=Depth_Bin[23]),show.legend = F,color="red") +
  annotate("text", x=OB_Shallow$Depth_Bin[21],y=0.075,label="Mean seafloor depth",color="red") +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels=abs,limits=c(-0.025,0.025)) +
  scale_fill_viridis_d(begin=0,end=0.7) +
  theme_bw() +
  coord_flip(expand=F) +
  labs(y = "Mean Biomass (g/m^3)", x="Depth Bin", fill="Instrument")
fname = paste0(figure_dir,"OB_Shallow_Echo_OPC_Comparsion_Plot.png")
ggsave(plot=OB_Shallow_Plot,file=fname,scale=2)

OB_Mid_Plot = ggplot(data=OB_Mid, aes(x = Depth_Bin)) +
  geom_col(aes(y=Echo_Biomass,fill="Echosounder")) +
  geom_line(aes(y=rollmean(Echo_Biomass,5,na.pad=T,align="center"),group=NA)) +
  geom_segment(aes(x=Depth_Bin[which.max(rollmean(Echo_Biomass,5,na.pad=T,align="center"))],
                   xend=Depth_Bin[which.max(rollmean(Echo_Biomass,5,na.pad=T,align="center"))],
                   y=0,yend=0.025),
               linetype = "dashed") +
  geom_col(aes(y=-OPC_Biomass,fill="OPC")) +
  geom_line(aes(y=-rollmean(OPC_Biomass,5,na.pad=T,align="center"),group=NA)) +
  geom_segment(aes(x=Depth_Bin[which.max(rollmean(OPC_Biomass,5,na.pad=T,align="center"))],
                   xend=Depth_Bin[which.max(rollmean(OPC_Biomass,5,na.pad=T,align="center"))],
                   y=0,yend=-0.025),
               linetype = "dashed") +
  geom_vline(aes(xintercept=Depth_Bin[26]),show.legend = F,color="red") +
  annotate("text", x=OB_Mid$Depth_Bin[24],y=0.075,label="Mean seafloor depth",color="red") +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels=abs,limits=c(-0.025,0.025)) +
  scale_fill_viridis_d(begin=0,end=0.7) +
  theme_bw() +
  coord_flip(expand=F) +
  labs(y = "Mean Biomass (g/m^3)", x="Depth Bin", fill="Instrument")
fname = paste0(figure_dir,"OB_Mid_Echo_OPC_Comparsion_Plot.png")
ggsave(plot=OB_Mid_Plot,file=fname,scale=2)

OB_Deep_Plot = ggplot(data=OB_Deep, aes(x = Depth_Bin)) +
  geom_col(aes(y=Echo_Biomass,fill="Echosounder")) +
  geom_line(aes(y=rollmean(Echo_Biomass,5,na.pad=T,align="center"),group=NA)) +
  geom_segment(aes(x=Depth_Bin[which.max(rollmean(Echo_Biomass,5,na.pad=T,align="center"))],
                   xend=Depth_Bin[which.max(rollmean(Echo_Biomass,5,na.pad=T,align="center"))],
                   y=0,yend=0.025),
               linetype = "dashed") +
  geom_col(aes(y=-OPC_Biomass,fill="OPC")) +
  geom_line(aes(y=-rollmean(OPC_Biomass,5,na.pad=T,align="center"),group=NA)) +
  geom_segment(aes(x=Depth_Bin[which.max(rollmean(OPC_Biomass,5,na.pad=T,align="center"))],
                   xend=Depth_Bin[which.max(rollmean(OPC_Biomass,5,na.pad=T,align="center"))],
                   y=0,yend=-0.025),
               linetype = "dashed") +
  geom_vline(aes(xintercept=Depth_Bin[28]),show.legend = F,color="red") +
  annotate("text", x=OB_Deep$Depth_Bin[26],y=0.075,label="Mean seafloor depth",color="red") +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels=abs,limits=c(-0.025,0.025)) +
  scale_fill_viridis_d(begin=0,end=0.7) +
  theme_bw() +
  coord_flip(expand=F) +
  labs(y = "Mean Biomass (g/m^3)", x="Depth Bin", fill="Instrument")
fname = paste0(figure_dir,"OB_Deep_Echo_OPC_Comparsion_Plot.png")
ggsave(plot=OB_Deep_Plot,file=fname,scale=2)
  
#####

## Multinet comparison bar plots

# Vertical bar graph
# Total biomass vs c.fin 4/5/6F biomass
# Note: need to use the wet biomass (from Natasha's measurements)
# instead of my calculated dry biomass??

# Station numbers:
# GMB shallow: 10, 13, 17
# OB shallow: 5, 20

# GMB mid: 9, 16
# OB mid: 3, 7, 18

# GMB deep: 8, 12, 15
# OB deep: 4, 6, 19

# small_copepods = c("Acartia hudsonica", "Acartia longiremis", "Acartia sp.", "Acartia tonsa",
#                    "Centropages hamatus", "Centropages typicus",
#                    "Metridia longa", "Metridia lucens", "Metridia sp.",
#                    "Microcalanus spp.",
#                    "Microsetella norvegica",
#                    "Oithona atlantica", "Oithona similis",
#                    "Paracalanus spp.",
#                    "Pseudocalanus spp.",
#                    "Temora longicornis")
# 
# Net_Data$taxa[Net_Data$taxa %in% small_copepods] = "Small copepod"

GMB_Shallow_Cfin = stages[stages$station %in% c(10, 13, 17),] %>% 
  filter(stage %in% c("CIV","CV","CVIF")) %>%
  group_by(net, stage) %>%
  reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))

# also compare small copepod and late-stage c. fin biomass
# Net_Data[(Net_Data$station %in% c(10, 13, 17) & Net_Data$taxa == "Small copepod"),] %>%
#   group_by(net) %>%
#   reframe(small_copepod_biomass = mean(biomass,na.rm=T))

GMB_Shallow_Net = Net_Data[Net_Data$station %in% c(10, 13, 17),] %>%
  group_by(net, station) %>%
  reframe(biomass = sum(biomass,na.rm=T), concentration = sum(concentration, na.rm=T)) %>% ungroup() %>%
  group_by(net) %>%
  reframe(total_biomass = mean(biomass,na.rm=T), total_concentration = mean(concentration,na.rm=T))

GMB_Shallow_Net_Plot = ggplot() +
  geom_col(data=GMB_Shallow_Cfin, aes(x=cfin_biomass, y=net, group=stage, fill=stage)) +
  geom_col(data=GMB_Shallow_Net, aes(x=-total_biomass, y=net, fill="Total Biomass")) +
  theme_bw() +
  scale_x_continuous(labels = abs,limits=c(-.13, .13)) +
  scale_fill_manual(values=(oceColorsViridis(4))) +
  labs(x="Mean Biomass (g/m^3)", y="Net",fill="Biomass Type")
fname = paste0(figure_dir,"GMB_Shallow_Net_Comparsion_Plot.png")
ggsave(plot=GMB_Shallow_Plot,file=fname,scale=2)


GMB_Mid_Cfin = stages[stages$station %in% c(9, 16),] %>% 
  group_by(net, stage) %>%
  filter(stage %in% c("CIV","CV","CVIF")) %>%
  reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))

GMB_Mid_Net = Net_Data[Net_Data$station %in% c(9, 16),] %>%
  group_by(net, station) %>%
  reframe(biomass = sum(biomass,na.rm=T), concentration = sum(concentration, na.rm=T)) %>% ungroup() %>%
  group_by(net) %>%
  reframe(total_biomass = mean(biomass,na.rm=T), total_concentration = mean(concentration,na.rm=T))

GMB_Mid_Net_Plot = ggplot() +
  geom_col(data=GMB_Mid_Cfin, aes(x=cfin_biomass, y=net, group=stage, fill=stage)) +
  geom_col(data=GMB_Mid_Net, aes(x=-total_biomass, y=net, fill="Total Biomass")) +
  theme_bw() +
  scale_x_continuous(labels = abs,limits=c(-.13, .13)) +
  scale_fill_manual(values=(oceColorsViridis(4))) +
  labs(x="Mean Biomass (g/m^3)", y="Net", fill="Biomass Type")
fname = paste0(figure_dir,"GMB_Mid_Net_Comparsion_Plot.png")
ggsave(plot=GMB_Mid_Plot,file=fname,scale=2)


GMB_Deep_Cfin = stages[stages$station %in% c(8, 12, 15),] %>% 
  group_by(net, stage) %>%
  filter(stage %in% c("CIV","CV","CVIF")) %>%
  reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))

GMB_Deep_Net = Net_Data[Net_Data$station %in% c(8, 12, 15),] %>%
  group_by(net, station) %>%
  reframe(biomass = sum(biomass,na.rm=T), concentration = sum(concentration, na.rm=T)) %>% ungroup() %>%
  group_by(net) %>%
  reframe(total_biomass = mean(biomass,na.rm=T), total_concentration = mean(concentration,na.rm=T))

GMB_Deep_Net_Plot = ggplot() +
  geom_col(data=GMB_Deep_Cfin, aes(x=cfin_biomass, y=net, group=stage, fill=stage)) +
  geom_col(data=GMB_Deep_Net, aes(x=-total_biomass, y=net, fill="Total Biomass")) +
  theme_bw() +
  scale_x_continuous(labels = abs, limits=c(-.13, .13)) +
  scale_fill_manual(values=(oceColorsViridis(4))) +
  labs(x="Mean Biomass (g/m^3)", y="Net", fill="Biomass Type")
fname = paste0(figure_dir,"GMB_Deep_Net_Comparsion_Plot.png")
ggsave(plot=GMB_Deep_Plot,file=fname,scale=2)


OB_Shallow_Cfin = stages[stages$station %in% c(5, 20),] %>% 
  group_by(net, stage) %>%
  filter(stage %in% c("CIV","CV","CVIF")) %>%
  reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))

OB_Shallow_Net = Net_Data[Net_Data$station %in% c(15, 20),] %>%
  group_by(net, station) %>%
  reframe(biomass = sum(biomass,na.rm=T), concentration = sum(concentration, na.rm=T)) %>% ungroup() %>%
  group_by(net) %>%
  reframe(total_biomass = mean(biomass,na.rm=T), total_concentration = mean(concentration,na.rm=T))

OB_Shallow_Net_Plot = ggplot() +
  geom_col(data=OB_Shallow_Cfin, aes(x=cfin_biomass, y=net, group=stage, fill=stage)) +
  geom_col(data=OB_Shallow_Net, aes(x=-total_biomass, y=net, fill="Total Biomass")) +
  theme_bw() +
  scale_x_continuous(labels = abs, limits=c(-.13, .13)) +
  scale_fill_manual(values=(oceColorsViridis(4))) +
  labs(x="Mean Biomass (g/m^3)", y="Net", fill="Biomass Type")
fname = paste0(figure_dir,"OB_Shallow_Net_Comparsion_Plot.png")
ggsave(plot=OB_Shallow_Plot,file=fname,scale=2)


OB_Mid_Cfin = stages[stages$station %in% c(3, 7, 18),] %>% 
  group_by(net, stage) %>%
  filter(stage %in% c("CIV","CV","CVIF")) %>%
  reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))

OB_Mid_Net = Net_Data[Net_Data$station %in% c(3, 7, 18),] %>%
  group_by(net, station) %>%
  reframe(biomass = sum(biomass,na.rm=T), concentration = sum(concentration, na.rm=T)) %>% ungroup() %>%
  group_by(net) %>%
  reframe(total_biomass = mean(biomass,na.rm=T), total_concentration = mean(concentration,na.rm=T))

OB_Mid_Net_Plot = ggplot() +
  geom_col(data=OB_Mid_Cfin, aes(x=cfin_biomass, y=net, group=stage, fill=stage)) +
  geom_col(data=OB_Mid_Net, aes(x=-total_biomass, y=net, fill="Total Biomass")) +
  theme_bw() +
  scale_x_continuous(labels = abs,limits=c(-.13, .13)) +
  scale_fill_manual(values=(oceColorsViridis(4))) +
  labs(x="Mean Biomass (g/m^3)", y="Net", fill="Biomass Type")
fname = paste0(figure_dir,"OB_Mid_Net_Comparsion_Plot.png")
ggsave(plot=OB_Mid_Plot,file=fname,scale=2)


OB_Deep_Cfin = stages[stages$station %in% c(4, 6, 19),] %>% 
  group_by(net, stage) %>%
  filter(stage %in% c("CIV","CV","CVIF")) %>%
  reframe(cfin_biomass = mean(biomass,na.rm=T), cfin_concentration = mean(concentration,na.rm=T))

OB_Deep_Net = Net_Data[Net_Data$station %in% c(4, 6, 19),] %>%
  group_by(net, station) %>%
  reframe(biomass = sum(biomass,na.rm=T), concentration = sum(concentration, na.rm=T)) %>% ungroup() %>%
  group_by(net) %>%
  reframe(total_biomass = mean(biomass,na.rm=T), total_concentration = mean(concentration,na.rm=T))

OB_Deep_Net_Plot = ggplot() +
  geom_col(data=OB_Deep_Cfin, aes(x=cfin_biomass, y=net, group=stage, fill=stage)) +
  geom_col(data=OB_Deep_Net, aes(x=-total_biomass, y=net, fill="Total Biomass")) +
  theme_bw() +
  scale_x_continuous(labels = abs,limits=c(-.13, .13)) +
  scale_fill_manual(values=(oceColorsViridis(4))) +
  labs(x="Mean Biomass (g/m^3)", y="Net", fill="Biomass Type")
fname = paste0(figure_dir,"OB_Deep_Net_Comparsion_Plot.png")
ggsave(plot=OB_Deep_Plot,file=fname,scale=2)

ggarrange(GMB_Shallow_Plot,GMB_Shallow_Net_Plot,
          ncol=2)
fname=paste0(figure_dir,"GMB_Shallow_Full_Comparison_Plot.png")
ggsave(file=fname,scale=2)

ggarrange(GMB_Mid_Plot,GMB_Mid_Net_Plot,
          ncol=2)
fname=paste0(figure_dir,"GMB_Mid_Full_Comparison_Plot.png")
ggsave(file=fname,scale=2)

ggarrange(GMB_Deep_Plot,GMB_Deep_Net_Plot,
          ncol=2)
fname=paste0(figure_dir,"GMB_Deep_Full_Comparison_Plot.png")
ggsave(file=fname,scale=2)

ggarrange(OB_Shallow_Plot,OB_Shallow_Net_Plot,
          ncol=2)
fname=paste0(figure_dir,"OB_Shallow_Full_Comparison_Plot.png")
ggsave(file=fname,scale=2)

ggarrange(OB_Mid_Plot,OB_Mid_Net_Plot,
          ncol=2)
fname=paste0(figure_dir,"OB_Mid_Full_Comparison_Plot.png")
ggsave(file=fname,scale=2)

ggarrange(OB_Deep_Plot,OB_Deep_Net_Plot,
          ncol=2)
fname=paste0(figure_dir,"OB_Deep_Full_Comparison_Plot.png")
ggsave(file=fname,scale=2)

#####

## MultiNet Wet Biomass plots

Net_Data_Wet = read_excel(paste0(data_dir,'Multinet/2022-01-12_BoF2020_Zooplankton_Data.xlsx'),sheet=2) %>%
  filter(site %in% c("OB","GMB"))

Net_Data_Wet$volume = NA
Net_Data_Wet$station = NA

for (j in 1:nrow(Net_Metadata)) {
  Net_Data_Wet$volume[which(as.character(Net_Data_Wet$sample_id) ==
                              as.character(Net_Metadata$Sample_ID[j]))] =
    Net_Metadata$`Volume_(m3)`[j]
  
  Net_Data_Wet$station[which(as.character(Net_Data_Wet$sample_id) ==
                               as.character(Net_Metadata$Sample_ID[j]))] =
    Net_Metadata$Station_Number[j]
}

Net_Data_Wet = Net_Data_Wet[!(Net_Data_Wet$split %in% grep("OR",Net_Data_Wet$split,value=TRUE)),]
Net_Data_Wet = Net_Data_Wet[!(Net_Data_Wet$split %in% grep("n/a",Net_Data_Wet$split,value=TRUE)),]
Net_Data_Wet = Net_Data_Wet[!(Net_Data_Wet$split %in% grep(" [+] ",Net_Data_Wet$split,value=TRUE)),]
Net_Data_Wet = Net_Data_Wet[!(Net_Data_Wet$tow == 10),]
Net_Data_Wet$`Weight (g)` = as.numeric(Net_Data_Wet$`Weight (g)`)

Net_Data_Wet$split = sapply(Net_Data_Wet$split, function(x) eval(parse(text = x)))

Net_Data_Wet$wet_biomass = Net_Data_Wet$`Weight (g)` / Net_Data_Wet$split / Net_Data_Wet$volume
Net_Data_Wet$net = as.factor(Net_Data_Wet$net)


OB_Shallow_Wet = Net_Data_Wet[Net_Data_Wet$station %in% c(15, 20),] %>%
  group_by(net, station) %>%
  reframe(wet_biomass = sum(wet_biomass,na.rm=T)) %>% ungroup() %>%
  group_by(net) %>%
  reframe(total_biomass = mean(wet_biomass,na.rm=T))

ggplot(data=OB_Shallow_Wet) +
  geom_col(aes(x=total_biomass,y=net)) +
  scale_x_continuous(limits=c(0,0.6)) +
  labs(y="Net",x="Wet Biomass (g/m^3)")
ggsave(filename=paste0(figure_dir,'OB_Shallow_Wet_Biomass.png'),scale=2)


#####

## OPC concentrations

GMB_Shallow_Abundance = OPC_Data_Full[OPC_Data_Full$OPC_cast_num %in% c(17,
                                                              23,
                                                              31, 32),] %>%
  filter(depth>1) %>%
  filter(flag != 'depth') %>%
  group_by(OPC_cast_num, basin) %>%
  reframe(
    opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = F,
                reject_volume = F)) %>%
  reframe(depth = depth, concentration = concentration) %>%
  group_by(depth) %>%
  reframe(concentration = mean(concentration))

GMB_Mid_Abundance = OPC_Data_Full[OPC_Data_Full$OPC_cast_num %in% c(15, 16,
                                                          29,30),] %>%
  filter(depth>1) %>%
  filter(flag != 'depth') %>%
  group_by(OPC_cast_num, basin) %>%
  reframe(
    opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = F,
                reject_volume = F)) %>%
  reframe(depth = depth, concentration = concentration) %>%
  group_by(depth) %>%
  reframe(concentration = mean(concentration))

GMB_Deep_Abundance = OPC_Data_Full[OPC_Data_Full$OPC_cast_num %in% c(13, 14,
                                                           21, 22,
                                                           27, 28),] %>%
  filter(depth>1) %>%
  filter(flag != 'depth') %>%
  group_by(OPC_cast_num, basin) %>%
  reframe(
    opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = F,
                reject_volume = F)) %>%
  reframe(depth = depth, concentration = concentration) %>%
  group_by(depth) %>%
  reframe(concentration = mean(concentration))

OB_Shallow_Abundance = OPC_Data_Full[OPC_Data_Full$OPC_cast_num %in% c(7, 8),] %>%
  filter(depth>1) %>%
  filter(flag != 'depth') %>%
  group_by(OPC_cast_num, basin) %>%
  reframe(
    opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = F,
                reject_volume = F)) %>%
  reframe(depth = depth, concentration = concentration) %>%
  group_by(depth) %>%
  reframe(concentration = mean(concentration))

OB_Mid_Abundance = OPC_Data_Full[OPC_Data_Full$OPC_cast_num %in% c(33),] %>%
  filter(depth>1) %>%
  filter(flag != 'depth') %>%
  group_by(OPC_cast_num, basin) %>%
  reframe(
    opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = F,
                reject_volume = F)) %>%
  reframe(depth = depth, concentration = concentration) %>%
  group_by(depth) %>%
  reframe(concentration = mean(concentration))

OB_Deep_Abundance = OPC_Data_Full[OPC_Data_Full$OPC_cast_num %in% c(5,
                                                          35, 36),] %>%
  filter(depth>1) %>%
  filter(flag != 'depth') %>%
  group_by(OPC_cast_num, basin) %>%
  reframe(
    opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = F,
                reject_volume = F)) %>%
  reframe(depth = depth, concentration = concentration) %>%
  group_by(depth) %>%
  reframe(concentration = mean(concentration))


# Fill in missing depth bins
DFs = sort(grep("_Abundance$|_Abundance$",names(.GlobalEnv),value=TRUE))

OPC_concentration = data.frame(matrix(0, ncol=7, nrow=40))
OPC_concentration[,1] = (paste0(seq(0,195,5)))
names(OPC_concentration)[1] = "Depth_Bin"
n = 2

for(i in 1:length(DFs)) {
  test = get(DFs[i])
  for(j in 1:nrow(test)) {
    OPC_concentration[which(OPC_concentration$Depth_Bin == test$depth[j]),n] = test$concentration[j]
  }
  names(OPC_concentration)[n] = DFs[i]
  n = n+1
}

#####

## Net concentration vs OPC concentration a la Baum 2003

# Station numbers:
# GMB shallow: 10, 13, 17, seafloor depth 158
# OB shallow: 5, 20, seafloor depth 110

# GMB mid: 9, 16, seafloor depth 160.5
# OB mid: 3, 7, 18, seafloor depth 127.3

# GMB deep: 8, 12, 15, seafloor depth 169.3
# OB deep: 4, 6, 19, seafloor depth 135

require(plyr)
GMB_Shallow_Cfin$Intervals = Net_Metadata[Net_Metadata$Station_Number %in% c(10, 13, 17),] %>% 
  group_by(Net_Num) %>%
  reframe(depth = rep(round_any(mean(`Pressure_(dbar)`), 5, f = ceiling), each = 3)) %>% slice(c(1:15))
detach("package:plyr", unload=TRUE)

GMB_Shallow_Cfin = GMB_Shallow_Cfin %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))

OPC_concentration$GMB_Shallow_Net_Abundance = 0

for(i in 1:nrow(GMB_Shallow_Cfin)) {
  if(i == 1) {
    OPC_concentration$GMB_Shallow_Net_Abundance[as.numeric(OPC_concentration$Depth_Bin) <= 
                                                   GMB_Shallow_Cfin$`Intervals$depth`[i]] = GMB_Shallow_Cfin$cfin_concentration[i]
  } else {
  OPC_concentration$GMB_Shallow_Net_Abundance[(as.numeric(OPC_concentration$Depth_Bin) <= 
                                                GMB_Shallow_Cfin$`Intervals$depth`[i] &
                                                 as.numeric(OPC_concentration$Depth_Bin) >=
                                                 GMB_Shallow_Cfin$`Intervals$depth`[i-1])] = GMB_Shallow_Cfin$cfin_concentration[i]
  }
}

GMB_Shallow_Abundance_Plot =ggplot(data=OPC_concentration, aes(x = factor(Depth_Bin, levels = Depth_Bin))) +
  geom_col(aes(y=GMB_Shallow_Abundance)) +
  geom_step(aes(y = GMB_Shallow_Net_Abundance, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits = rev) +
  theme_bw() +
  coord_flip() +
  labs(y="Abundance (individuals/m^3)", x = "Depth (m)")
ggsave(file = paste0(figure_dir, "GMB_Shallow_Abundance_Comparison.png"), scale=2)

require(plyr)
GMB_Mid_Cfin$Intervals = Net_Metadata[Net_Metadata$Station_Number %in% c(9, 16),] %>% 
  group_by(Net_Num) %>%
  reframe(depth = rep(round_any(mean(`Pressure_(dbar)`), 5, f = ceiling), each = 3)) %>% slice(c(1:15))
detach("package:plyr", unload=TRUE)

GMB_Mid_Cfin = GMB_Mid_Cfin %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))

OPC_concentration$GMB_Mid_Net_Abundance = 0

for(i in 1:nrow(GMB_Mid_Cfin)) {
  if(i == 1) {
    OPC_concentration$GMB_Mid_Net_Abundance[as.numeric(OPC_concentration$Depth_Bin) <= 
                                                  GMB_Mid_Cfin$`Intervals$depth`[i]] = GMB_Mid_Cfin$cfin_concentration[i]
  } else {
    OPC_concentration$GMB_Mid_Net_Abundance[(as.numeric(OPC_concentration$Depth_Bin) <= 
                                                   GMB_Mid_Cfin$`Intervals$depth`[i] &
                                                   as.numeric(OPC_concentration$Depth_Bin) >=
                                                   GMB_Mid_Cfin$`Intervals$depth`[i-1])] = GMB_Mid_Cfin$cfin_concentration[i]
  }
}

GMB_Mid_Abundance_Plot =ggplot(data=OPC_concentration, aes(x = factor(Depth_Bin, levels = Depth_Bin))) +
  geom_col(aes(y=GMB_Mid_Abundance)) +
  geom_step(aes(y = GMB_Mid_Net_Abundance, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits = rev) +
  theme_bw() +
  coord_flip() +
  labs(y="Abundance (individuals/m^3)", x = "Depth (m)")
ggsave(file = paste0(figure_dir, "GMB_Mid_Abundance_Comparison.png"), scale=2)

require(plyr)
GMB_Deep_Cfin$Intervals = Net_Metadata[Net_Metadata$Station_Number %in% c(8, 12, 15),] %>% 
  group_by(Net_Num) %>%
  reframe(depth = rep(round_any(mean(`Pressure_(dbar)`), 5, f = ceiling), each = 3)) %>% slice(c(1:15))
detach("package:plyr", unload=TRUE)

GMB_Deep_Cfin = GMB_Deep_Cfin %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))

OPC_concentration$GMB_Deep_Net_Abundance = 0

for(i in 1:nrow(GMB_Deep_Cfin)) {
  if(i == 1) {
    OPC_concentration$GMB_Deep_Net_Abundance[as.numeric(OPC_concentration$Depth_Bin) <= 
                                                  GMB_Deep_Cfin$`Intervals$depth`[i]] = GMB_Deep_Cfin$cfin_concentration[i]
  } else {
    OPC_concentration$GMB_Deep_Net_Abundance[(as.numeric(OPC_concentration$Depth_Bin) <= 
                                                   GMB_Deep_Cfin$`Intervals$depth`[i] &
                                                   as.numeric(OPC_concentration$Depth_Bin) >=
                                                   GMB_Deep_Cfin$`Intervals$depth`[i-1])] = GMB_Deep_Cfin$cfin_concentration[i]
  }
}

GMB_Deep_Abundance_Plot =ggplot(data=OPC_concentration, aes(x = factor(Depth_Bin, levels = Depth_Bin))) +
  geom_col(aes(y=GMB_Deep_Abundance)) +
  geom_step(aes(y = GMB_Deep_Net_Abundance, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits = rev) +
  theme_bw() +
  coord_flip() +
  labs(y="Abundance (individuals/m^3)", x = "Depth (m)")
ggsave(file = paste0(figure_dir, "GMB_Deep_Abundance_Comparison.png"), scale=2)


OB_Shallow_Cfin = rbind(OB_Shallow_Cfin, data.frame(net=c(4, 4, 4),
                                                    stage=c("CIV","CV","CVIF"),
                                                    cfin_biomass=c(0,0,0),
                                                    cfin_concentration=c(0,0,0)))
OB_Shallow_Cfin = OB_Shallow_Cfin[order(OB_Shallow_Cfin$net),]
require(plyr)
OB_Shallow_Cfin$Intervals = Net_Metadata[Net_Metadata$Station_Number %in% c(5, 20),] %>% 
  group_by(Net_Num) %>%
  reframe(depth = rep(round_any(mean(`Pressure_(dbar)`), 5, f = ceiling), each = 3)) %>% slice(c(1:15))
detach("package:plyr", unload=TRUE)

OB_Shallow_Cfin = OB_Shallow_Cfin %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))

OPC_concentration$OB_Shallow_Net_Abundance = 0

for(i in 1:nrow(OB_Shallow_Cfin)) {
  if(i == 1) {
    OPC_concentration$OB_Shallow_Net_Abundance[as.numeric(OPC_concentration$Depth_Bin) <= 
                                                  OB_Shallow_Cfin$`Intervals$depth`[i]] = OB_Shallow_Cfin$cfin_concentration[i]
  } else {
    OPC_concentration$OB_Shallow_Net_Abundance[(as.numeric(OPC_concentration$Depth_Bin) <= 
                                                   OB_Shallow_Cfin$`Intervals$depth`[i] &
                                                   as.numeric(OPC_concentration$Depth_Bin) >=
                                                   OB_Shallow_Cfin$`Intervals$depth`[i-1])] = OB_Shallow_Cfin$cfin_concentration[i]
  }
}

OB_Shallow_Abundance_Plot =ggplot(data=OPC_concentration, aes(x = factor(Depth_Bin, levels = Depth_Bin))) +
  geom_col(aes(y=OB_Shallow_Abundance)) +
  geom_step(aes(y = OB_Shallow_Net_Abundance, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits = rev) +
  theme_bw() +
  coord_flip() +
  labs(y="Abundance (individuals/m^3)", x = "Depth (m)")
ggsave(file = paste0(figure_dir, "OB_Shallow_Abundance_Comparison.png"), scale=2)

require(plyr)
OB_Mid_Cfin$Intervals = Net_Metadata[Net_Metadata$Station_Number %in% c(3, 7, 18),] %>% 
  group_by(Net_Num) %>%
  reframe(depth = rep(round_any(mean(`Pressure_(dbar)`), 5, f = ceiling), each = 3)) %>% slice(c(1:15))
detach("package:plyr", unload=TRUE)

OB_Mid_Cfin = OB_Mid_Cfin %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))

OPC_concentration$OB_Mid_Net_Abundance = 0

for(i in 1:nrow(OB_Mid_Cfin)) {
  if(i == 1) {
    OPC_concentration$OB_Mid_Net_Abundance[as.numeric(OPC_concentration$Depth_Bin) <= 
                                              OB_Mid_Cfin$`Intervals$depth`[i]] = OB_Mid_Cfin$cfin_concentration[i]
  } else {
    OPC_concentration$OB_Mid_Net_Abundance[(as.numeric(OPC_concentration$Depth_Bin) <= 
                                               OB_Mid_Cfin$`Intervals$depth`[i] &
                                               as.numeric(OPC_concentration$Depth_Bin) >=
                                               OB_Mid_Cfin$`Intervals$depth`[i-1])] = OB_Mid_Cfin$cfin_concentration[i]
  }
}

OB_Mid_Abundance_Plot =ggplot(data=OPC_concentration, aes(x = factor(Depth_Bin, levels = Depth_Bin))) +
  geom_col(aes(y=OB_Mid_Abundance)) +
  geom_step(aes(y = OB_Mid_Net_Abundance, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits = rev) +
  theme_bw() +
  coord_flip() +
  labs(y="Abundance (individuals/m^3)", x = "Depth (m)")
ggsave(file = paste0(figure_dir, "OB_Mid_Abundance_Comparison.png"), scale=2)

OB_Deep_Cfin = rbind(OB_Deep_Cfin, data.frame(net=c(4, 4, 4),
                                                    stage=c("CIV","CV","CVIF"),
                                                    cfin_biomass=c(0,0,0),
                                                    cfin_concentration=c(0,0,0)))
OB_Deep_Cfin = OB_Deep_Cfin[order(OB_Deep_Cfin$net),]
require(plyr)
OB_Deep_Cfin$Intervals = Net_Metadata[Net_Metadata$Station_Number %in% c(4, 6, 19),] %>% 
  group_by(Net_Num) %>%
  reframe(depth = rep(round_any(mean(`Pressure_(dbar)`), 5, f = ceiling), each = 3)) %>% slice(c(1:15))
detach("package:plyr", unload=TRUE)

OB_Deep_Cfin = OB_Deep_Cfin %>% group_by(Intervals$depth) %>% reframe (cfin_concentration = sum(cfin_concentration))

OPC_concentration$OB_Deep_Net_Abundance = 0

for(i in 1:nrow(OB_Deep_Cfin)) {
  if(i == 1) {
    OPC_concentration$OB_Deep_Net_Abundance[as.numeric(OPC_concentration$Depth_Bin) <= 
                                               OB_Deep_Cfin$`Intervals$depth`[i]] = OB_Deep_Cfin$cfin_concentration[i]
  } else {
    OPC_concentration$OB_Deep_Net_Abundance[(as.numeric(OPC_concentration$Depth_Bin) <= 
                                                OB_Deep_Cfin$`Intervals$depth`[i] &
                                                as.numeric(OPC_concentration$Depth_Bin) >=
                                                OB_Deep_Cfin$`Intervals$depth`[i-1])] = OB_Deep_Cfin$cfin_concentration[i]
  }
}

OB_Deep_Abundance_Plot =ggplot(data=OPC_concentration, aes(x = factor(Depth_Bin, levels = Depth_Bin))) +
  geom_col(aes(y=OB_Deep_Abundance)) +
  geom_step(aes(y = OB_Deep_Net_Abundance, group=NA),linewidth=1,direction="mid") +
  scale_x_discrete(limits = rev) +
  theme_bw() +
  coord_flip() +
  labs(y="Abundance (individuals/m^3)", x = "Depth (m)")
ggsave(file = paste0(figure_dir, "OB_Deep_Abundance_Comparison.png"), scale=2)

#####

## Super mega ultra final truth data (final) (FINAL) (2) (FINAL)

# Start with the GMB station data

stations = c(8, 12, 15)
OPC_casts = list(c(13, 14),c(21,22),c(27,28))
dz = 5
min_size = 1.5
max_size = 2

OPC_to_C5 = function(concentration, speed) {
  B0 = 0.0384
  B1 = 0.5343
  B2 = 0.8001
  
  temp = (1/B1) * (log10(concentration) - B0 - B2 * speed)
  return(10^temp)
  
}

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


GMB_Plots = ggarrange(plotlist=list(print(station8_plot),print(station9_plot),print(station10_plot),
                        print(station12_plot),print(station16_plot),print(station13_plot),
                        print(station15_plot),NA,print(station17_plot)),ncol=3,nrow=3,
                      labels=c("Deep","Mid","Shallow"),vjust=1)
fname=paste0(figure_dir,"GMB_All_Stations_Comparison.png")
ggsave(file=fname,scale=3)

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
  if(i != 3) {
  temp1 = rbind(temp1, data.frame(net=c(4,4,4),
                                  stage=c("CIV","CV","CVIF"),
                                  cfin_biomass=0,
                                  cfin_concentration=0))
  temp1 = temp1[order(temp1$net),]
  } else {
    temp1 = rbind(temp1, data.frame(net=c(4,4,4),
                                    stage=c("CIV","CV","CVIF"),
                                    cfin_biomass=0,
                                    cfin_concentration=0),
                  data.frame(net=c(5,5,5),
                             stage=c("CIV","CV","CVIF"),
                             cfin_biomass=0,
                             cfin_concentration=0))
    temp1 = temp1[order(temp1$net),]
  }
  
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
  
  if(i == 2) {
    temp1 = rbind(temp1, data.frame(net=c(3,3,3),
                                    stage=c("CIV","CV","CVIF"),
                                    cfin_biomass=0,
                                    cfin_concentration=0),
                  data.frame(net=c(4,4,4),
                             stage=c("CIV","CV","CVIF"),
                             cfin_biomass=0,
                             cfin_concentration=0))
    temp1 = temp1[order(temp1$net),]
  } else if(i == 3) {
    temp1 = rbind(temp1, data.frame(net=c(5,5,5),
                                    stage=c("CIV","CV","CVIF"),
                                    cfin_biomass=0,
                                    cfin_concentration=0))
    temp1 = temp1[order(temp1$net),]
  }
  
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
  
  if(i == 1) {
    temp1 = rbind(temp1, data.frame(net=c(4,4,4),
                                    stage=c("CIV","CV","CVIF"),
                                    cfin_biomass=0,
                                    cfin_concentration=0))
    temp1 = temp1[order(temp1$net),]
  } else if(i == 2) {
    temp1 = rbind(temp1, data.frame(net=c(4,4,4),
                                    stage=c("CIV","CV","CVIF"),
                                    cfin_biomass=0,
                                    cfin_concentration=0),
                  data.frame(net=c(5,5,5),
                             stage=c("CIV","CV","CVIF"),
                             cfin_biomass=0,
                             cfin_concentration=0))
    temp1 = temp1[order(temp1$net),]
  }
  
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
    
    mask = 15.8 <= echo_sv[[i]][[j]][[1]] & 
      echo_sv[[i]][[j]][[1]] <= 16.3 # mask
    echo_sv[[i]][[j]][[2]] = (10 * log10(integration[[j]][[7]])) * mask # masked 455 kHz
    echo_sv[[i]][[j]][[2]][echo_sv[[i]][[j]][[2]] == 0] = NaN
    echo_sv[[i]][[j]][[2]][is.na(echo_sv[[i]][[j]][[2]])] = NaN

  }
}

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

sv_plots=list()
svdiff_plots=list()

for(p in 1:16) {
  sv_plots[[p]] = local({
    p = p
    plot = ggplot(data = echo_grouped_sv, aes(y = Depth_Bin)) +
      geom_col(aes(x = echo_grouped_sv[,p+1])) +
      scale_y_discrete(limits=rev, breaks = echo_grouped_sv$Depth_Bin[c(seq(1,40,3))]) +
      scale_x_reverse() +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      coord_cartesian(xlim = c(-60,-85), expand = T) + 
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
                      reframe(Mean_SvDiff = rowMeans(select(.,), na.rm = TRUE)))
ggplot(data = GMB_Shallow, aes(y = Depth_Bin)) +
  geom_col(aes(x=Mean_Sv)) +
  scale_y_discrete(limits=rev) +
  scale_x_reverse() +
  theme_bw() +
  labs(x = "Masked 455 kHz (dB)", y="Depth (m)")

GMB_Mid = cbind(echo_grouped_sv[,c(1,8,13)] %>% 
                      reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:3), na.rm = TRUE)),
                    echo_grouped_svdiff[,c(8,13)] %>% 
                      reframe(Mean_SvDiff = rowMeans(select(.,), na.rm = TRUE)))
ggplot(data = GMB_Mid, aes(y = Depth_Bin)) +
  geom_col(aes(x=Mean_Sv)) +
  scale_y_discrete(limits=rev) +
  scale_x_reverse() +
  theme_bw() +
  labs(x = "Masked 455 kHz (dB)", y="Depth (m)")

GMB_Deep = cbind(echo_grouped_sv[,c(1,7,10,12)] %>% 
                      reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:4), na.rm = TRUE)),
                    echo_grouped_svdiff[,c(7,10,12)] %>% 
                      reframe(Mean_SvDiff = rowMeans(select(.,), na.rm = TRUE)))
ggplot(data = GMB_Deep, aes(y = Depth_Bin)) +
  geom_col(aes(x=Mean_Sv)) +
  scale_y_discrete(limits=rev) +
  scale_x_reverse() +
  theme_bw() +
  labs(x = "Masked 455 kHz (dB)", y="Depth (m)")


OB_Shallow = cbind(echo_grouped_sv[,c(1,4,17)] %>% 
                      reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:3), na.rm = TRUE)),
                    echo_grouped_svdiff[,c(4,17)] %>% 
                      reframe(Mean_SvDiff = rowMeans(select(.,), na.rm = TRUE)))
ggplot(data = OB_Shallow, aes(y = Depth_Bin)) +
  geom_col(aes(x=Mean_Sv)) +
  scale_y_discrete(limits=rev) +
  scale_x_reverse() +
  theme_bw() +
  labs(x = "Masked 455 kHz (dB)", y="Depth (m)")

OB_Mid = cbind(echo_grouped_sv[,c(1,2,6,15)] %>% 
                  reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:4), na.rm = TRUE)),
                echo_grouped_svdiff[,c(2,6,15)] %>% 
                  reframe(Mean_SvDiff = rowMeans(select(.,), na.rm = TRUE)))
ggplot(data = OB_Mid, aes(y = Depth_Bin)) +
  geom_col(aes(x=Mean_Sv)) +
  scale_y_discrete(limits=rev) +
  scale_x_reverse() +
  theme_bw() +
  labs(x = "Masked 455 kHz (dB)", y="Depth (m)")

OB_Deep = cbind(echo_grouped_sv[,c(1,3,5,16)] %>% 
                   reframe(Depth_Bin = Depth_Bin, Mean_Sv = rowMeans(select(., 2:4), na.rm = TRUE)),
                 echo_grouped_svdiff[,c(3,5,16)] %>% 
                   reframe(Mean_SvDiff = rowMeans(select(.,), na.rm = TRUE)))
ggplot(data = OB_Deep, aes(y = Depth_Bin)) +
  geom_col(aes(x=Mean_Sv)) +
  scale_y_discrete(limits=rev) +
  scale_x_reverse() +
  theme_bw() +
  labs(x = "Masked 455 kHz (dB)", y="Depth (m)")

