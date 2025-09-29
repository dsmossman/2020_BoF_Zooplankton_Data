# Last updated: 28 Feb 2022

#####
setwd("C:/Users/Delphine/Documents/2020_BoF_Zooplankton_Data")

# Libraries

library(tidyverse)
library(oce)
library(ocedata)
library(marmap)
library(R.utils)
library(readxl)

# Project Structure

# sourceDirectory points towards outside functions: ddm2dd is the main one
sourceDirectory('Code/Hansen Zooplankton Code and Sample Data/src',
                modifiedOnly = F)

function_dir = 'Code/Hansen Zooplankton Code and Sample Data/src/'
cache_dir = 'Processed_Data/'
data_dir = 'Raw_Data/'
figure_dir = 'Visuals/'
report_dir = getwd()

## Load what you already have/need if there; skip if this is your first time running this file

load(paste0(cache_dir, 'Multi_Data.rda')) # Multi_Data contains data about each multinet tow
load(paste0(cache_dir, 'Net_Metadata.rda')) # Net_Metadata contains data about each multinet tow net (nets 1-5)
load(paste0(cache_dir, 'Net_Data.rda')) # Net_Data contains data about biological contents of each multinet tow net (nets 1-5)

## Processing multinet data

# Load in multinet spreadsheet

Sept2020_Cruise_Leg_2_Multinet_Log = read_csv("Raw_Data/Spreadsheets/Sept2020_Cruise_Leg_2_Multinet_Log.csv")

# Set up usable multinet data frame
# Original spreadsheet includes a bunch of extra variables; trim them

Multi_Data = na.omit(data.frame(Sept2020_Cruise_Leg_2_Multinet_Log[c(2:7, 9, 11:22), c(1:2, 4:11, 19, 22)]))
names(Multi_Data) = c(
  "Station_Number",
  "Cast_ID",
  "Date",
  "Start_Time",
  "End_Time",
  "Start_Latitude",
  "Start_Longitude",
  "End_Latitude",
  "End_Longitude",
  "Unlock_Pressure",
  "Sample_IDs",
  "File_Name"
)

# Reformat lats/longs to decimal degrees using ddm2dd function
Multi_Data$Start_Latitude = ddm2dd(Multi_Data$Start_Latitude)
Multi_Data$Start_Longitude = -ddm2dd(Multi_Data$Start_Longitude)
Multi_Data$End_Latitude = ddm2dd(Multi_Data$End_Latitude)
Multi_Data$End_Longitude = -ddm2dd(Multi_Data$End_Longitude)

# Reformat cast IDs
Multi_Data$Cast_ID = paste0("multi", sprintf("%03s", as.character(Multi_Data$Cast_ID)))

save(Multi_Data, file = paste0(cache_dir, 'Multi_Data.rda'))

## Create data frame with individual multinet metadata

Net_Metadata = data.frame()

for (i in 1:nrow(Multi_Data)) {
  temp = read.delim(
    paste0(data_dir, "Multinet/Exported Data/", Multi_Data$File_Name[i]),
    skip = 34,
    header = F
  ) %>%
    group_by(V2) %>%
    summarise(across(everything(), last)) %>%
    ungroup()
  
  temp = temp[, 1:4]
  meta = data.frame(
    rep(Multi_Data$Station_Number[i], nrow(temp)),
    rep(Multi_Data$Cast_ID[i], nrow(temp)),
    rep(paste0(Multi_Data$Date[i], "-2020"), nrow(temp)),
    rbind(NA, t(
      str_split(Multi_Data$Sample_IDs[i], ", ", simplify = T)
    ))
  )
  temp = cbind(meta, temp)
  
  colnames(temp) = c(
    "Station_Number",
    "Cast_ID",
    "Date",
    "Sample_ID",
    "Net_Num",
    "Time",
    "Pressure_(dbar)",
    "Volume_(m3)"
  )
  
  temp$Datetime = as.POSIXct(paste0(temp$Date, ' ', temp$Time), format = "%d-%B-%Y %H:%M")
  
  Net_Metadata = rbind(Net_Metadata, temp)
  rm(temp)
}

# get the approximate depth intervals for each net/tow
Net_Metadata[which(Net_Metadata$`Pressure_(dbar)` < 0.5), 7] = 0

intervals = data.frame()

for (i in 1:length(Net_Metadata$`Pressure_(dbar)`)) {
  if (Net_Metadata$Net_Num[i] == 0) {
    intervals[i, 1] = NA
    next
  }
  else {
    intervals[i, 1] = Net_Metadata$`Pressure_(dbar)`[i - 1] - Net_Metadata$`Pressure_(dbar)`[i]
  }
}

Net_Metadata = cbind(Net_Metadata, intervals)
colnames(Net_Metadata)[10] = "Depth_Interval"

save(Net_Metadata, file = paste0(cache_dir, 'Net_Metadata.rda'))

## Biological Samples Count/Concentration data

# Load in excel spreadsheet

Net_Data = read_excel(paste0(
  data_dir,
  "Multinet/2021-12-23_BoF2020_Zooplankton_Data.xlsx"
))
Net_Data = Net_Data[which(Net_Data$site == "OB" | Net_Data$site == "GMB"),]
Net_Data$split = sapply(Net_Data$split, function(x) eval(parse(text = x)))

# eliminate the "copepoda" category and unused tows

Net_Data = subset(Net_Data, taxa != 'Copepoda')
Net_Data = subset(Net_Data, tow != 10)
Net_Data = subset(Net_Data, tow != 15)
Net_Data = subset(Net_Data, tow != 18)

# factorize relevant columns

Net_Data$tow = as.factor(Net_Data$tow)
Net_Data$net = as.factor(Net_Data$net)
Net_Data$sample_id = as.factor(Net_Data$sample_id)
Net_Data$site = as.factor(Net_Data$site)
Net_Data$taxa = as.factor(Net_Data$taxa)
Net_Data$split = as.character(Net_Data$split)

# combine juvenile/adult Oithona and Centropages to be the same species

Net_Data$taxa = gsub(pattern = 'Oithona sp.',
                     replacement = 'Oithona similis',
                     x = Net_Data$taxa)
Net_Data$taxa = gsub(pattern = 'Centropages sp.',
                     replacement = 'Centropages typicus',
                     x = Net_Data$taxa)

test = Net_Data %>% group_by(sample_id, taxa) %>% summarize_if(is.numeric, sum, na.rm = T)
test1 = test[which(test$taxa == "Oithona similis"), ]
test2 = test[which(test$taxa == "Centropages typicus"), ]

for (i in 1:nrow(test1)) {
  Net_Data[which(Net_Data$sample_id == test1$sample_id[i] &
                   Net_Data$taxa == "Oithona similis"),
           9:20] = test1[i, 3:14]
}

for (i in 1:nrow(test2)) {
  Net_Data[which(Net_Data$sample_id == test2$sample_id[i] &
                   Net_Data$taxa == "Centropages typicus"),
           9:20] = test2[i, 3:14]
}

Net_Data = Net_Data[!duplicated(Net_Data), ]
Net_Data[Net_Data == 0] = NA
Net_Data$split = as.numeric(Net_Data$split)

rm(test, test1, test2)

# Set up stations and volume columns

Net_Data = cbind(Net_Data[, 1:4], rep(NA, nrow(Net_Data)),
                 Net_Data[, 5:ncol(Net_Data)], rep(NA, nrow(Net_Data)))
colnames(Net_Data)[5] = 'station'
colnames(Net_Data)[ncol(Net_Data)] = 'volume'

for (j in 1:nrow(Net_Metadata)) {
  Net_Data$station[which(as.character(Net_Data$sample_id) ==
                           as.character(Net_Metadata$Sample_ID[j]))] =
    Net_Metadata$Station_Number[j]
  
  Net_Data$volume[which(as.character(Net_Data$sample_id) ==
                          as.character(Net_Metadata$Sample_ID[j]))] =
    Net_Metadata$`Volume_(m3)`[j]
}

Net_Data$station = as.factor(Net_Data$station)

Net_Data$concentration = Net_Data$Total / Net_Data$split / Net_Data$volume
Net_Data$raw_count = Net_Data$Total / Net_Data$split

# Weighted concentration

# for each row in Net_Data, find the net interval with the matching sample number and multiply
# it by the concentration to get the weighted concentration

weighted_concentration = c()

for (i in 1:nrow(Net_Data)) {
  interval = Net_Metadata$Depth_Interval[which(as.character(Net_Metadata$Sample_ID) ==
                                                 as.character(Net_Data$sample_id[i]))]
  weighted_concentration[i] = Net_Data$concentration[i] * interval
}
weighted_concentration = as.data.frame(weighted_concentration)
Net_Data = cbind(Net_Data, weighted_concentration)

# reorder by date
Net_Data = Net_Data[order(Net_Data$sample_date), ]
rownames(Net_Data) = 1:nrow(Net_Data)

# Biomass calculations

Net_Data$biomass = NA

copepods_biomass = data.frame(cbind(c("Acartia hudsonica", "Acartia longiremis", "Acartia sp.", "Acartia tonsa",
                              "Centropages hamatus", "Centropages typicus",
                              "Eurytemora herdmani",
                              "Metridia longa", "Metridia lucens", "Metridia sp.",
                              "Microcalanus spp.",
                              "Microsetella norvegica",
                              "Oithona atlantica", "Oithona similis",
                              "Paracalanus spp.",
                              "Paraeuchaeta norvegica", "Paraeuchaeta sp.",
                              "Pleuromamma robusta",
                              "Pseudocalanus spp.",
                              "Temora longicornis"),
                              10^-6 * c(4, 4, 4, 4,
                              40, 40,
                              6.745,
                              262, 70.95, 166.475,
                              6.98,
                              3.05,
                              2.87, 3.00,
                              5.55,
                              200, 200,
                              379,
                              11,
                              2.5)));
copepods_biomass[,2] = as.numeric(copepods_biomass[,2])

euphausiids = c("Meganyctiphanes norvegica", "Meganyctiphanes sp.",
                "Thysanoessa inermis", "Thysanoessa longicaudata", "Thysanoessa sp.");

siph = c("Siphonophora",
         "Siphonophora (colony)",
         "Siphonophora (bract)",
         "Siphonophora (nectophore)",
         "Physonectae (nectophore)")

for(i in 1:nrow(copepods_biomass)) {
  Net_Data$biomass[which(Net_Data$taxa == copepods_biomass[i,1])] = copepods_biomass[i,2] * Net_Data$concentration[which(Net_Data$taxa == copepods_biomass[i,1])]
}

Net_Data[,10:20] = replace(Net_Data[,10:20],is.na(Net_Data[,10:20]),0)


for(i in 1:nrow(Net_Data)) {
  if(Net_Data$taxa[i] == "Calanus finmarchicus") {
    Net_Data$biomass[i] = (10^-6 * (Net_Data$CI[i] * 4.1 +
      Net_Data$CII[i] * 7.6 +
      Net_Data$CIII[i] * 18.6 +
      Net_Data$CIV[i] * 51.9 +
      Net_Data$CV[i] * 156.1 +
      Net_Data$Female[i] * 268.3)) / Net_Data$volume[i] / Net_Data$split[i] 
  } else if(Net_Data$taxa[i] == "Calanus glacialis") {
    Net_Data$biomass[i] = 10^-6 * (Net_Data$CIV[i] * 227 +
      Net_Data$CV[i] * 514) / Net_Data$volume[i] / Net_Data$split[i]
  } else if(Net_Data$taxa[i] == "Calanus hyperboreus") {
    Net_Data$biomass[i] = 10^-6 * (Net_Data$CIV[i] * 304 +
      Net_Data$CV[i] * 1145) / Net_Data$volume[i] / Net_Data$split[i]
  } else if(Net_Data$taxa[i] == "Calanus sp.") {
    Net_Data$biomass[i] = 10^-6 * (Net_Data$Total[i] * 269.66) / Net_Data$volume[i] / Net_Data$split[i]
  } else if(Net_Data$taxa[i] %in% euphausiids) {
    Net_Data$biomass[i] = 10^-6 * (Net_Data$concentration[i] * 29750)
    Net_Data$biomass[i] = 10^-6 * (Net_Data$concentration[i]) * 0
  }
}

# determining community composition by biomass

Hist_Data = Net_Data %>% group_by(station, net) %>% summarise(concentration = log10(sum(concentration, na.rm = TRUE)))
colnames(Hist_Data)[3] = "total_concentration"

ggplot() +
  geom_histogram(data = Hist_Data,
                 aes(x = total_concentration, fill = net),
                 colour = "black",
                 bins = 40) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  labs(x = expression(paste("Log10 of Total Net Concentration (individuals/m"^"3"*")")),
       y = "Count",
       fill = "Net Number")

ggsave(filename = paste0(figure_dir, "Net_Concentration_Histogram.png"))

Hist_Data = Net_Data %>% group_by(station, net) %>% summarise(biomass = log10(sum(biomass, na.rm = TRUE)))
colnames(Hist_Data)[3] = "total_biomass"

ggplot() +
  geom_histogram(data = Hist_Data,
                 aes(x = total_biomass, fill = net),
                 colour = "black",
                 bins = 40) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  labs(x = expression(paste("Log10 of Total Net Biomass (grams/m"^"3"*")")),
       y = "Count",
       fill = "Net Number")

ggsave(filename = paste0(figure_dir, "Net_Biomass_Histogram.png"))


small_copepods = c("Acartia hudsonica", "Acartia longiremis", "Acartia sp.", "Acartia tonsa",
             "Centropages hamatus", "Centropages typicus",
             "Metridia longa", "Metridia lucens", "Metridia sp.",
             "Microcalanus spp.",
             "Microsetella norvegica",
             "Oithona atlantica", "Oithona similis",
             "Paracalanus spp.",
             "Pseudocalanus spp.",
             "Temora longicornis")

Net_Data$community_comp = NA
Cfin_Nets = data.frame("Basin" = integer(0), "Station" = integer(0), "Net" = integer(0), "Cfin_Biomass" = integer(0), "Cfin_Conc" = integer(0))

for(i in seq_along(levels(Net_Data$station))) {
  # define station
  stn = levels(Net_Data$station)[i]
  
  # subset the data
  tmp = subset(Net_Data, Net_Data$station == stn)
  tmp = droplevels(tmp)
  
  # for each net
  for(j in seq_along(levels(tmp$net))) {
    # define net
    netnum = levels(tmp$net)[j]
    
    # subset the data again
    tmp2 = subset(tmp, tmp$net == netnum)
    tmp2 = droplevels(tmp2)
    
    # get the c. fin, copepod, euphausiid, and siphonophore concentrations
    # cfin_conc = tmp2$concentration[which(tmp2$taxa == "Calanus finmarchicus")]
    # small_copepod_conc = sum(tmp2$concentration[which(tmp2$taxa %in% small_copepods)])
    # euph_conc = sum(tmp2$concentration[which(tmp2$taxa %in% euphausiids)])
    # siph_conc = sum(tmp2$concentration[which(tmp2$taxa %in% siph)])
    # 
    # concentrations = cbind(cfin_conc, small_copepod_conc, euph_conc, siph_conc)
    # dominant_conc = colnames(concentrations)[max.col(concentrations,ties.method="first")]
    # 
    # if(dominant_conc == "cfin_conc") {
    #   Cfin_Nets[nrow(Cfin_Nets) + 1,] = c(stn, netnum, max(cfin_conc))
    # }
    # 
    # if(max(concentrations) < 500) {
    #   tmp2$community_comp = "Low concentration"
    # } else if(dominant_conc == "small_copepod_conc") {
    #   tmp2$community_comp = "Small copepod dominant"
    # } else if(dominant_conc == "cfin_conc") {
    #   tmp2$community_comp = "C. finmarchicus dominant"
    # } else if(dominant_conc == "euph_conc") {
    #   tmp2$community_comp = "Euphausiid dominant"
    # } else {
    #   tmp2$community_comp = "Siphonophora dominant"
    # }
    
    # eventually: get the c. fin, copepod, euphausiid, and siphonophore biomasses
    cfin_mass = sum(tmp2$biomass[which(tmp2$taxa == "Calanus finmarchicus")], na.rm = T)
    small_copepod_mass = sum(tmp2$biomass[which(tmp2$taxa %in% small_copepods)], na.rm = T)
    euph_mass = sum(tmp2$biomass[which(tmp2$taxa %in% euphausiids)], na.rm = T)
    siph_mass = sum(tmp2$biomass[which(tmp2$taxa %in% siph)], na.rm = T)

    biomasses = cbind(cfin_mass, small_copepod_mass, euph_mass, siph_mass)
    dominant_mass = colnames(biomasses)[max.col(biomasses,ties.method="first")]
    
    cfin_conc = sum(tmp2$concentration[which(tmp2$taxa == "Calanus finmarchicus")], na.rm = T)

    if(dominant_mass == "cfin_mass") {
      Cfin_Nets[nrow(Cfin_Nets) + 1,] = c(as.character(tmp2$site[1]), stn, netnum, round(max(cfin_mass), 3), round(max(cfin_conc), 3))
    }

    if(max(biomasses) < 10^-1.8) {
      tmp2$community_comp = "Low biomass"
    } else if(dominant_mass == "small_copepod_mass") {
      tmp2$community_comp = "Small copepod dominant"
    } else if(dominant_mass == "cfin_mass") {
      tmp2$community_comp = "C. finmarchicus dominant"
    } else if(dominant_mass == "euph_mass") {
      tmp2$community_comp = "Euphausiid dominant"
    }
    
    print(i)
    print(j)
    print(biomasses)
    
    tmp$community_comp[which(tmp$net == netnum)] = tmp2$community_comp
    
  }
  
  Net_Data$community_comp[which(Net_Data$station == stn)] = tmp$community_comp
  
}

# save
save(Net_Data, file = paste0(cache_dir, 'Net_Data.rda'))

## Dominance calculations

# take the sum of all the concentrations in a particular net or station (depending on the figure)
# and divide each taxonomic category in that net or station by that sum. Then either graph it or
# put the information in a data table. The total will add up to 100 %. Then we can measure
# the size distributions on the species that make up 75 %, 80 % or 90 % of the samples

# By station

Abundance_Data_Concentration = Net_Data %>% group_by(station, sample_date, site, taxa) %>% summarise(concentration = sum(concentration, na.rm = TRUE))
total_concentrations = Abundance_Data_Concentration %>% summarise(concentration = sum(concentration, na.rm = TRUE))
Abundance_Data_Concentration$relative_abundance = NA
Abundance_Data_Concentration = Abundance_Data_Concentration[which(Abundance_Data_Concentration$site == "OB" | Abundance_Data_Concentration$site == "GMB"),]

for(i in 1:nrow(Abundance_Data_Concentration)) {
  Abundance_Data_Concentration$relative_abundance[i] = Abundance_Data_Concentration$concentration[i] / 
    total_concentrations$concentration[which(total_concentrations$station == Abundance_Data_Concentration$station[i])]
}

save(Abundance_Data_Concentration, file = paste0(cache_dir, "Abundance_Data_Concentration.rda"))

Abundance_Data_Biomass = Net_Data %>% group_by(station, sample_date, site, taxa) %>% summarise(biomass = sum(biomass, na.rm = TRUE))
total_biomass = Abundance_Data_Biomass %>% group_by(station) %>% summarise(biomass = sum(biomass, na.rm = TRUE))
Abundance_Data_Biomass$relative_abundance = rep(NA, nrow(Abundance_Data_Biomass))
Abundance_Data_Biomass = Abundance_Data_Biomass[which(Abundance_Data_Biomass$site == "OB" | Abundance_Data_Biomass$site == "GMB"),]

for(i in 1:nrow(Abundance_Data_Biomass)) {
  Abundance_Data_Biomass$relative_abundance[i] = Abundance_Data_Biomass$biomass[i] / 
    total_biomass$biomass[which(total_biomass$station == Abundance_Data_Biomass$station[i])]
}

save(Abundance_Data_Biomass, file = paste0(cache_dir, "Abundance_Data_Biomass.rda"))



## Histogram of concentrations by taxa

load(paste0(cache_dir, 'Net_Data_Compressed.rda'))

Hist_Data = Net_Data_Compressed %>% group_by(station, net, taxa) %>% summarise(concentration = log10(sum(concentration, na.rm = TRUE)))
colnames(Hist_Data)[4] = "total_concentration"

ggplot() +
  geom_histogram(data = Hist_Data,
                 aes(x = total_concentration, fill = net),
                 colour = "black",
                 bins = 40) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  labs(x = expression(paste("Log10 of Total Net Concentration (individuals/m"^"3"*")")),
       y = "Count",
       fill = "Net Number") +
  facet_wrap(~taxa)

ggsave(filename = paste0(figure_dir, "Net_Concentration_Histogram_By_Taxa.png"))


Hist_Data = Net_Data_Compressed %>% group_by(station, net, taxa) %>% summarise(biomass = log10(sum(biomass, na.rm = TRUE)))
colnames(Hist_Data)[4] = "total_biomass"
Hist_Data = Hist_Data[which(Hist_Data$taxa != "Siphonophora (pneumatophore)"),]

ggplot() +
  geom_histogram(data = Hist_Data,
                 aes(x = total_biomass, fill = net),
                 colour = "black",
                 bins = 40) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  labs(x = expression(paste("Log10 of Total Net Biomass (g/m"^"3"*")")),
       y = "Count",
       fill = "Net Number") +
  facet_wrap(~taxa)

ggsave(filename = paste0(figure_dir, "Net_Biomass_Histogram_By_Taxa.png"))

## C. fin counts by stage

# subset

cfin = subset(Net_Data, Net_Data$taxa == "Calanus finmarchicus")
cfin = droplevels(cfin)
cfin = cfin[,-c(10:12)]

# recalculate raw_counts and concentrations by stage

for(i in 10:15) {
  temp = cfin[,i] / cfin$split / cfin$volume
  cfin = cbind(cfin, temp)
  tname = paste0(colnames(cfin)[i], "_concentration")
  colnames(cfin)[ncol(cfin)] = tname
  
  temp = cfin[,i] / cfin$split
  cfin = cbind(cfin, temp)
  tname = paste0(colnames(cfin)[i], "_raw_count")
  colnames(cfin)[ncol(cfin)] = tname
}

# need a data frame with rows corresponding to c. fin stages and columns corresponding to
# net #, site, concentration, and raw count

# stage1 = cfin[which(cfin[,10] > 0),]
stage1 = cfin[,-c(11:20, 22:25, 29:ncol(cfin))]
stage1[,9] = "CI"
colnames(stage1)[9:10] = c("stage","num")
colnames(stage1)[13:14] = c("concentration","raw_count")
stage1$biomass = stage1$concentration * 10^-6 * 4.1

# stage2 = cfin[which(cfin[,11] > 0),]
stage2 = cfin[,-c(10, 12:20, 22:25, 27:28, 31:ncol(cfin))]
stage2[,9] = "CII"
colnames(stage2)[9:10] = c("stage","num")
colnames(stage2)[13:14] = c("concentration","raw_count")
stage2$biomass = stage2$concentration * 10^-6 * 7.6

# stage3 = cfin[which(cfin[,12] > 0),]
stage3 = cfin[,-c(10:11, 13:20, 22:25, 27:30, 33:ncol(cfin))]
stage3[,9] = "CIII"
colnames(stage3)[9:10] = c("stage","num")
colnames(stage3)[13:14] = c("concentration","raw_count")
stage3$biomass = stage3$concentration * 10^-6 * 18.6

# stage4 = cfin[which(cfin[,13] > 0),]
stage4 = cfin[,-c(10:12, 14:20, 22:25, 27:32, 35:ncol(cfin))]
stage4[,9] = "CIV"
colnames(stage4)[9:10] = c("stage","num")
colnames(stage4)[13:14] = c("concentration","raw_count")
stage4$biomass = stage4$concentration * 10^-6 * 51.9

# stage5 = cfin[which(cfin[,14] > 0),]
stage5 = cfin[,-c(10:13, 15:20, 22:25, 27:34, 37:ncol(cfin))]
stage5[,9] = "CV"
colnames(stage5)[9:10] = c("stage","num")
colnames(stage5)[13:14] = c("concentration","raw_count")
stage5$biomass = stage5$concentration * 10^-6 * 156.1

# stage6F = cfin[which(cfin[,15] > 0),]
stage6F = cfin[,-c(10:14, 16:20, 22:25, 27:36)]
stage6F[,9] = "CVIF"
colnames(stage6F)[9:10] = c("stage","num")
colnames(stage6F)[13:14] = c("concentration","raw_count")
stage6F$biomass = stage6F$concentration * 10^-6 * 268.3

stages = rbind(stage1, stage2, stage3, stage4, stage5, stage6F)
stages$stage = as.factor(stages$stage)
stages$sample_date = as.factor(stages$sample_date)

weighted_concentration = c()
for (i in 1:nrow(stages)) {
  interval = Net_Metadata$Depth_Interval[which(as.character(Net_Metadata$Sample_ID) ==
                                                 as.character(stages$sample_id[i]))]
  weighted_concentration[i] = stages$concentration[i] * interval
}
weighted_concentration = as.data.frame(weighted_concentration)
stages = cbind(stages, weighted_concentration)

# Sort by tow, net, and stage
stages = stages[order(stages$tow, stages$net), ]
rownames(stages) = 1:nrow(stages)

save(stages, file = paste0(cache_dir, "Cfin_Stages.rda"))
