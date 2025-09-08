# Last updated: 15 June 2022

#####
rm(list = ls())

setwd("C:/Users/dmossman/Box/2022 MSc Thesis Work/")

## Libraries

library(tidyverse)
# library(oce)
# library(ocedata)
library(R.utils)
library(readxl)
# library(ZooScatR)

## Project Structure
sourceDirectory('Code/Hansen Zooplankton Code and Sample Data/src',
                modifiedOnly = F)

function_dir = 'Code/Hansen Zooplankton Code and Sample Data/src/'
create_dir(function_dir)
cache_dir = 'Processed_Data/'
create_dir(cache_dir)
data_dir = 'Raw_Data/'
create_dir(data_dir)
figure_dir = 'Visuals/'
create_dir(figure_dir)
report_dir = getwd()
create_dir(report_dir)

## Load in data structures created by Multinet_Abundance_Processing.R

load(paste0(cache_dir, 'Multi_Data.rda'))
load(paste0(cache_dir, 'Net_Metadata.rda'))
load(paste0(cache_dir, 'Net_Data.rda'))
load(paste0(cache_dir, 'Abundance_Data_Concentration.rda'))
load(paste0(cache_dir, 'Abundance_Data_Biomass.rda'))

#####
# Target Strength

# DWBAapp() # launches the ZooScatR shiny app

# Target strength is the measure of a SINGLE zoop hit with a beam; that's why to get Sv
# we add the concentration

# most PITA part, primarily concerned with the fluid-like and gas-bearing
# Pretty sure everything but siphonophores counts as fluid-like
# For the fluid-like: ZooScatR R package, will have to come up with parameters myself
# for each unique taxa (maybe for higher level than genus?)
# For most of them, will just assume uniform size (literature values for that); copepods/C. fin
# and euphausiids, use the size frequency dist

# Joe's Values:

large_copepod_TS = c(-147.6,	-140.2,	-126.0,	-117.1);
small_copepod_TS = c(-157.4,	-149.9,	-135.7,	-126.6);
cfin_TS = c(-129.7,	-122.3,	-108.3,	-99.9);

# For siphonophores: Joe's values

siph_TS = c(-80, -70, -80, -80); # CURRENTLY PLACEHOLDER VALUES; REPLACE WITH JOE'S CALCS

# For euphausiids: ZooScatR

euph_TS = c(-69.93, -71.04, -73.46, -71.56);

# Then can get Sv from TS + 10log10(N) where N = net sample concentrations (individuals/m^3)
# Here is where I will do my siphonophore pneumatophore count testing

small_copepods = c("Acartia hudsonica", "Acartia longiremis", "Acartia sp.", "Acartia tonsa",
                   "Centropages hamatus", "Centropages typicus",
                   "Metridia longa", "Metridia lucens", "Metridia sp.",
                   "Microcalanus spp.",
                   "Microsetella norvegica",
                   "Oithona atlantica", "Oithona similis",
                   "Paracalanus spp.",
                   "Pseudocalanus spp.",
                   "Temora longicornis");

large_copepods = c("Calanus glacialis", "Calanus hyperboreus", "Calanus sp.",
                   "Paraeuchaeta norvegica", "Paraeuchaeta sp.",
                   "Pleuromamma robusta")

euphausiids = c("Meganyctiphanes norvegica", "Meganyctiphanes sp.",
                "Thysanoessa inermis", "Thysanoessa longicaudata", "Thysanoessa raschii", "Thysanoessa gregaria", "Thysanoessa sp.",
                "Euphausia krohnii", "Euphausia sp.",
                "Nematoscelis megalops", "Nematoscelis sp.");

for(i in 1:nrow(Net_Data)) {
  if (Net_Data$taxa[i] %in% large_copepods) {
    Net_Data$Sv1[i] = large_copepod_TS[1] + 10 * log10(Net_Data$concentration[i])
    Net_Data$Sv2[i] = large_copepod_TS[2] + 10 * log10(Net_Data$concentration[i])
    Net_Data$Sv3[i] = large_copepod_TS[3] + 10 * log10(Net_Data$concentration[i])
    Net_Data$Sv4[i] = large_copepod_TS[4] + 10 * log10(Net_Data$concentration[i])
  }
  else if (Net_Data$taxa[i] == "Siphonophora (pneumatophore)") {
    Net_Data$Sv1[i] = siph_TS[1] + 10 * log10(Net_Data$concentration[i])
    Net_Data$Sv2[i] = siph_TS[2] + 10 * log10(Net_Data$concentration[i])
    Net_Data$Sv3[i] = siph_TS[3] + 10 * log10(Net_Data$concentration[i])
    Net_Data$Sv4[i] = siph_TS[4] + 10 * log10(Net_Data$concentration[i])
  }
  else if (Net_Data$taxa[i] %in% small_copepods) {
    Net_Data$Sv1[i] = small_copepod_TS[1] + 10 * log10(Net_Data$concentration[i])
    Net_Data$Sv2[i] = small_copepod_TS[2] + 10 * log10(Net_Data$concentration[i])
    Net_Data$Sv3[i] = small_copepod_TS[3] + 10 * log10(Net_Data$concentration[i])
    Net_Data$Sv4[i] = small_copepod_TS[4] + 10 * log10(Net_Data$concentration[i])
  }
  else if (Net_Data$taxa[i] %in% euphausiids) {
    Net_Data$Sv1[i] = euph_TS[1] + 10 * log10(Net_Data$concentration[i])
    Net_Data$Sv2[i] = euph_TS[2] + 10 * log10(Net_Data$concentration[i])
    Net_Data$Sv3[i] = euph_TS[3] + 10 * log10(Net_Data$concentration[i])
    Net_Data$Sv4[i] = euph_TS[4] + 10 * log10(Net_Data$concentration[i])
  } 
  else if (Net_Data$taxa[i] == "Calanus finmarchicus") {
    Net_Data$Sv1[i] = cfin_TS[1] + 10 * log10(Net_Data$concentration[i])
    Net_Data$Sv2[i] = cfin_TS[2] + 10 * log10(Net_Data$concentration[i])
    Net_Data$Sv3[i] = cfin_TS[3] + 10 * log10(Net_Data$concentration[i])
    Net_Data$Sv4[i] = cfin_TS[4] + 10 * log10(Net_Data$concentration[i])
  }
  else {
    Net_Data$Sv1[i] = NA
    Net_Data$Sv2[i] = NA
    Net_Data$Sv3[i] = NA
    Net_Data$Sv4[i] = NA
  }
}

save(Net_Data, file = paste0(cache_dir, "Net_Data_With_Sv.rda"))

#####

# Then we just sum up the Svs from each net and tow and frequency
# Save in same format as the integrated echoes from the echosounder
# Text file for every tow, row for every frequency, column for every net

load(paste0(cache_dir, "Net_Data_With_Sv.rda"))

## ALL TAXA

for(i in seq_along(levels(Net_Data$station))) {

# preallocate space
  Integrated_Sv = data.frame(Net1 = rep(NA, 4), Net2 = rep(NA, 4), Net3 = rep(NA, 4),
                            Net4 = rep(NA, 4), Net5 = rep(NA, 4),
                            row.names = c("Freq1", "Freq2", "Freq3", "Freq4"))
  
  # define station
  stn = levels(Net_Data$station)[i]
  
  # subset station/tow
  tmp = subset(Net_Data, Net_Data$station == stn)
  tmp = droplevels(tmp)
  for(j in seq_along(levels(tmp$net))) {
    
    # define net
    netnum = levels(tmp$net)[j]
    
    # subsubset net for summation
    tmp2 = subset(tmp, tmp$net == netnum)
    tmp2 = droplevels(tmp2)
    
    # sum up the Sv from the net and put it in the Integrated Sv structure
    # don't forget to convert to linear space to sum!
    Integrated_Sv[1,j] = sum(10^(tmp2$Sv1/10), na.rm = TRUE)
    Integrated_Sv[2,j] = sum(10^(tmp2$Sv2/10), na.rm = TRUE)
    Integrated_Sv[3,j] = sum(10^(tmp2$Sv3/10), na.rm = TRUE)
    Integrated_Sv[4,j] = sum(10^(tmp2$Sv4/10), na.rm = TRUE)
  }
  
  # save files
  date = substr(tmp2$sample_date[1], 9, 10)
  townum = sprintf("%03d", as.numeric(as.character(tmp2$tow[1])))
  
  fout = paste0(cache_dir, 'Multinet/', 'Modeled_Sv_', date, "_Sep_2020_multi",townum,".txt")
  write.table(Integrated_Sv, fout, sep = "\t", row.names = TRUE, col.names = TRUE)
  rm(fout)
  
  
}

## COPEPODS ONLY

Copepod_Data = subset(Net_Data, Net_Data$taxa %in% small_copepods | Net_Data$taxa %in% large_copepods)

for(i in seq_along(levels(Copepod_Data$station))) {
  
  # preallocate space
  Integrated_Sv = data.frame(Net1 = rep(NA, 4), Net2 = rep(NA, 4), Net3 = rep(NA, 4),
                             Net4 = rep(NA, 4), Net5 = rep(NA, 4),
                             row.names = c("Freq1", "Freq2", "Freq3", "Freq4"))
  
  # define station
  stn = levels(Copepod_Data$station)[i]
  
  # subset station/tow
  tmp = subset(Copepod_Data, Copepod_Data$station == stn)
  tmp = droplevels(tmp)
  for(j in seq_along(levels(tmp$net))) {
    
    # define net
    netnum = levels(tmp$net)[j]
    
    # subsubset net for summation
    tmp2 = subset(tmp, tmp$net == netnum)
    tmp2 = droplevels(tmp2)
    
    # sum up the Sv from the net and put it in the Integrated Sv structure
    # don't forget to convert to linear space to sum!
    Integrated_Sv[1,j] = sum(10^(tmp2$Sv1/10), na.rm = TRUE)
    Integrated_Sv[2,j] = sum(10^(tmp2$Sv2/10), na.rm = TRUE)
    Integrated_Sv[3,j] = sum(10^(tmp2$Sv3/10), na.rm = TRUE)
    Integrated_Sv[4,j] = sum(10^(tmp2$Sv4/10), na.rm = TRUE)
  }
  
  # save file
  date = substr(tmp2$sample_date[1], 9, 10)
  townum = sprintf("%03d", as.numeric(as.character(tmp2$tow[1])))
  
  fout = paste0(cache_dir, 'Multinet/Copepods/', 'Modeled_Copepod_Sv_', date, "_Sep_2020_multi",townum,".txt")
  write.table(Integrated_Sv, fout, sep = "\t", row.names = TRUE, col.names = TRUE)
}

## LARGE COPEPODS ONLY

LCopepod_Data = subset(Net_Data, Net_Data$taxa %in% large_copepods)

for(i in seq_along(levels(LCopepod_Data$station))) {
  
  # preallocate space
  Integrated_Sv = data.frame(Net1 = rep(NA, 4), Net2 = rep(NA, 4), Net3 = rep(NA, 4),
                             Net4 = rep(NA, 4), Net5 = rep(NA, 4),
                             row.names = c("Freq1", "Freq2", "Freq3", "Freq4"))
  
  # define station
  stn = levels(LCopepod_Data$station)[i]
  
  # subset station/tow
  tmp = subset(LCopepod_Data, LCopepod_Data$station == stn)
  tmp = droplevels(tmp)
  for(j in seq_along(levels(tmp$net))) {
    
    # define net
    netnum = levels(tmp$net)[j]
    
    # subsubset net for summation
    tmp2 = subset(tmp, tmp$net == netnum)
    tmp2 = droplevels(tmp2)
    
    # sum up the Sv from the net and put it in the Integrated Sv structure
    # don't forget to convert to linear space to sum!
    Integrated_Sv[1,j] = sum(10^(tmp2$Sv1/10), na.rm = TRUE)
    Integrated_Sv[2,j] = sum(10^(tmp2$Sv2/10), na.rm = TRUE)
    Integrated_Sv[3,j] = sum(10^(tmp2$Sv3/10), na.rm = TRUE)
    Integrated_Sv[4,j] = sum(10^(tmp2$Sv4/10), na.rm = TRUE)
  }
  
  # save file
  date = substr(tmp2$sample_date[1], 9, 10)
  townum = sprintf("%03d", as.numeric(as.character(tmp2$tow[1])))
  
  fout = paste0(cache_dir, 'Multinet/LCopepod/', 'Modeled_LCopepod_Data_Sv_', date, "_Sep_2020_multi",townum,".txt")
  write.table(Integrated_Sv, fout, sep = "\t", row.names = TRUE, col.names = TRUE)
}

## CALANUS FINMARCHICUS ONLY

CFin_Data = subset(Net_Data, Net_Data$taxa == "Calanus finmarchicus")

for(i in seq_along(levels(CFin_Data$station))) {
  
  # preallocate space
  Integrated_Sv = data.frame(Net1 = rep(NA, 4), Net2 = rep(NA, 4), Net3 = rep(NA, 4),
                             Net4 = rep(NA, 4), Net5 = rep(NA, 4),
                             row.names = c("Freq1", "Freq2", "Freq3", "Freq4"))
  
  # define station
  stn = levels(CFin_Data$station)[i]
  
  # subset station/tow
  tmp = subset(CFin_Data, CFin_Data$station == stn)
  tmp = droplevels(tmp)
  for(j in seq_along(levels(tmp$net))) {
    
    # define net
    netnum = levels(tmp$net)[j]
    
    # subsubset net for summation
    tmp2 = subset(tmp, tmp$net == netnum)
    tmp2 = droplevels(tmp2)
    
    # sum up the Sv from the net and put it in the Integrated Sv structure
    # don't forget to convert to linear space to sum!
    Integrated_Sv[1,j] = sum(10^(tmp2$Sv1/10), na.rm = TRUE)
    Integrated_Sv[2,j] = sum(10^(tmp2$Sv2/10), na.rm = TRUE)
    Integrated_Sv[3,j] = sum(10^(tmp2$Sv3/10), na.rm = TRUE)
    Integrated_Sv[4,j] = sum(10^(tmp2$Sv4/10), na.rm = TRUE)
  }
  
  # save file
  date = substr(tmp2$sample_date[1], 9, 10)
  townum = sprintf("%03d", as.numeric(as.character(tmp2$tow[1])))
  
  fout = paste0(cache_dir, 'Multinet/CFin/', 'Modeled_CFin_Sv_', date, "_Sep_2020_multi",townum,".txt")
  write.table(Integrated_Sv, fout, sep = "\t", row.names = TRUE, col.names = TRUE)
}

### The Siphonophore Problem

siph = c("Siphonophora",
         "Siphonophora (colony)",
         "Siphonophora (bract)",
         "Siphonophora (nectophore)",
         "Physonectae (nectophore)")

load(paste0(cache_dir, "Net_Data_With_Sv.rda"))

# run below code to assume every net with siphonophore nectophores/bracts in it also had at
# least one pneumatophore, along with the ones we counted
for(i in 1:nrow(Net_Data)) {
  if (Net_Data$taxa[i] %in% siph) {
    Net_Data$Sv1[i] = siph_TS[1] + 10 * log10(1 / Net_Data$split[i] / Net_Data$volume[i])
    Net_Data$Sv2[i] = siph_TS[2] + 10 * log10(1 / Net_Data$split[i] / Net_Data$volume[i])
    Net_Data$Sv3[i] = siph_TS[3] + 10 * log10(1 / Net_Data$split[i] / Net_Data$volume[i])
    Net_Data$Sv4[i] = siph_TS[4] + 10 * log10(1 / Net_Data$split[i] / Net_Data$volume[i])
  }
}

for(i in seq_along(levels(Net_Data$station))) {
  
  # preallocate space
  Pneu_Integrated_Sv = data.frame(Net1 = rep(NA, 4), Net2 = rep(NA, 4), Net3 = rep(NA, 4),
                             Net4 = rep(NA, 4), Net5 = rep(NA, 4),
                             row.names = c("Freq1", "Freq2", "Freq3", "Freq4"))
  
  # define station
  stn = levels(Net_Data$station)[i]
  
  # subset station/tow
  tmp = subset(Net_Data, Net_Data$station == stn)
  tmp = droplevels(tmp)
  for(j in seq_along(levels(tmp$net))) {
    
    # define net
    netnum = levels(tmp$net)[j]
    
    # subsubset net for summation
    tmp2 = subset(tmp, tmp$net == netnum)
    tmp2 = droplevels(tmp2)
    
    # sum up the Sv from the net and put it in the Integrated Sv structure
    # don't forget to convert to linear space to sum!
    Pneu_Integrated_Sv[1,j] = sum(10^(tmp2$Sv1/10), na.rm = TRUE)
    Pneu_Integrated_Sv[2,j] = sum(10^(tmp2$Sv2/10), na.rm = TRUE)
    Pneu_Integrated_Sv[3,j] = sum(10^(tmp2$Sv3/10), na.rm = TRUE)
    Pneu_Integrated_Sv[4,j] = sum(10^(tmp2$Sv4/10), na.rm = TRUE)
  }
  
  # save files
  date = substr(tmp2$sample_date[1], 9, 10)
  townum = sprintf("%03d", as.numeric(as.character(tmp2$tow[1])))
  
  fout = paste0(cache_dir, 'Multinet/Pneumatophore/', 'Min_Pneu_Modeled_Sv_', date, "_Sep_2020_multi",townum,".txt")
  write.table(Pneu_Integrated_Sv, fout, sep = "\t", row.names = TRUE, col.names = TRUE)
  rm(fout)
  
}