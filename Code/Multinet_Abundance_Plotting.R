# Last updated: 18 May 2022

#####
setwd("C:/Users/dmossman/Box/2022 MSc Thesis Work")

## Libraries

library(tidyverse)
library(oce)
library(ocedata)
library(R.utils)
library(readxl)
library(ggpubr)
library(pracma)

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
load(paste0(cache_dir, "Cfin_Stages.rda"))

### PLOTS

##########
## Plotting Raw Counts

outdir = paste0(figure_dir,'Station_Counts/')
create_dir(outdir)

# Factors to consider: station, taxa (just calanus/c. fin, just the most abundant stuff),
# net number, basin, stage(s) (which will require recalculating the concentration/raw counts)

# Counts by station

nts = list()

# loop through and generate plot for each station
for(i in seq_along(levels(Net_Data$station))) {
  
  # define station
  stn = levels(Net_Data$station)[i]
  
  # define output file
  fout = paste0(outdir, stn, '_counts.png')
  
  message('Plotting raw counts for sample ', stn, ' and saving it as: ', fout)
  
  # open plot
  png(filename = fout, width = 7, height = 5, units = 'in', res = 200)
  
  # set margins for plotting
  par(mar = c(10,4,4,2))
  
  # subset for plotting
  tmp = subset(Net_Data, Net_Data$station == stn)
  tmp = droplevels(tmp)
  ylim_cts = range(tmp$raw_count, na.rm = TRUE)
  nts[[paste0(stn)]] = subset(Net_Data, Net_Data$station == stn)
  
  # make plot
  plot(tmp$raw_count~as.factor(tmp$taxa), 
       las = 2, xlab = '', ylab = 'counts per m^2', 
       ylim = ylim_cts, cex.axis = 0.5, main = paste0('Date: ', tmp$sample_date[1],
                                                      ', Site: ', tmp$site[1], 
                                                      ', Station: ', stn, 
                                                      ', Nets 1-5'))
  
  # save and close plot
  rm(tmp)
  dev.off()
  
}

# Counts by station and net

nts = list()

for(i in seq_along(levels(Net_Data$station))) {
  
  # define station
  stn = levels(Net_Data$station)[i]
  
  # define output file
  fout = paste0(outdir, stn, '_net_separated_counts.png')
  
  message('Plotting raw counts for sample ', stn, ' and saving it as: ', fout)
  
  # subset for plotting
  tmp = subset(Net_Data, Net_Data$station == stn)
  tmp = droplevels(tmp)
  ylim_cts = range(tmp$raw_count, na.rm = TRUE)
  nts[[paste0(stn)]] = subset(Net_Data, Net_Data$station == stn)
  
  # make plot
  
  ggplot(data = tmp, aes(x = taxa, y = raw_count)) +
    geom_bar(stat = "identity", aes(fill = net), color = "black", na.rm = T) +
    scale_fill_manual(values=rev(oceColorsViridis(6)))+
    theme_bw()+
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
    labs(title = paste0('Date: ', tmp$sample_date[1],
                        ', Site: ', tmp$site[1], 
                        ', Station: ', stn),
         x = "Sampled Taxa",
         y = "Raw Count (# individuals)",
         fill = "Net #") +
    theme(plot.title = element_text(hjust = 0.5))
  # needs title, better axes labels
  
  ggsave(filename = fout)
  
  rm(tmp)
  
}

# All calanus species by various metrics

# subset

taxa_list = c('Calanus finmarchicus', 
              'Calanus glacialis', 
              "Calanus hyperboreus",
              "Calanus sp.",
              "Calanus spp.")
bsn = subset(Net_Data, Net_Data$taxa %in% taxa_list)
bsn = droplevels(bsn)

# actual plots

ggplot(data = bsn, aes(x = taxa, y = log10(raw_count))) +
  geom_bar(stat = "identity", aes(fill = net), size = 0.5, na.rm = T) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1, size = 10)) +
  labs(title = paste0('Calanus sp.'),
       x = "Sampled Calanus Taxa",
       y = "Log10 of Raw Count (# individuals)",
       fill = "Net #") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = paste0(figure_dir, 'calanus.png'))

ggplot(data = bsn, aes(x = taxa, y = log10(raw_count))) +
  geom_bar(stat = "identity", aes(fill = net), size = 0.5, na.rm = T) +
  facet_wrap(~site) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1, size = 10)) +
  labs(title = paste0('Calanus sp. by basin'),
       x = "Sampled Calanus Taxa",
       y = "Log10 of Raw Count (# individuals)",
       fill = "Net #") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = paste0(figure_dir, 'calanus_basin.png'))

ggplot(data = bsn, aes(x = taxa, y = log10(raw_count))) +
  geom_bar(stat = "identity", aes(fill = net), size = 0.5, na.rm = T) +
  facet_wrap(~station) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1, size = 5)) +
  labs(title = paste0('Calanus sp. by station'),
       x = "Sampled Calanus Taxa",
       y = "Log10 of Raw Count (# individuals)",
       fill = "Net #") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = paste0(figure_dir, 'calanus_station.png'))

# C. fin by stages

load(paste0(cache_dir,'Cfin_Stages.rda'))

ggplot(data = stages, aes(x = stage, y = log10(raw_count))) +
  geom_bar(stat = "identity", aes(fill = net), color = NA) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  labs(title = "Calanus finmarchicus stage counts by net",
       x = "C. finmarchicus Stages",
       y = "Log10 of Raw Count (# individuals)",
       fill = "Net #") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(figure_dir, "calanus_finmarchicus_stages.png"))

ggplot(data = stages, aes(x = stage, y = log10(raw_count))) +
  geom_bar(stat = "identity", aes(fill = net), color = NA) +
  facet_wrap(~station) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  labs(title = "Calanus finmarchicus stage counts by net and station",
       x = "C. finmarchicus Stages",
       y = "Log10 of Raw Count (# individuals)",
       fill = "Net #") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(figure_dir, "calanus_finmarchicus_stages_station.png"))

ggplot(data = stages, aes(x = stage, y = log10(raw_count))) +
  geom_bar(stat = "identity", aes(fill = net), color = NA) +
  facet_wrap(~site) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  labs(title = "Calanus finmarchicus counts by net and basin",
       x = "C. finmarchicus Stages",
       y = "Log10 of Raw Count (# individuals)",
       fill = "Net #") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(figure_dir, "calanus_finmarchicus_stages_basin.png"))

##########
## Plotting Concentrations

outdir = paste0(figure_dir,'Station_Concentrations/')
create_dir(outdir)

nts = list()

# Concentration by station

# loop through and generate plot for each station
for(i in seq_along(levels(Net_Data$station))) {
  
  # define station
  stn = levels(Net_Data$station)[i]
  
  # define output file
  fout = paste0(outdir, stn, '_concentration.png')
  
  message('Plotting concentrations for sample ', stn, ' and saving it as: ', fout)
  
  # open plot
  png(filename = fout, width = 7, height = 5, units = 'in', res = 200)
  
  # set margins for plotting
  par(mar = c(10,4,4,2))
  
  # subset for plotting
  tmp = subset(Net_Data, Net_Data$station == stn)
  tmp = droplevels(tmp)
  ylim_cts = range(tmp$concentration, na.rm = TRUE)
  nts[[paste0(stn)]] = subset(Net_Data, Net_Data$station == stn)
  
  new_order = with(tmp, reorder(taxa, concentration, median, na.rm = T))
  
  # make plot
  plot(tmp$concentration ~ new_order, 
       las = 2, xlab = '', ylab = 'concentration per m^3', 
       ylim = ylim_cts, cex.axis = 0.5, main = paste0('Date: ', tmp$sample_date[1],
                                                      ', Site: ', tmp$site[1], 
                                                      ', Station: ', stn, 
                                                      ', Nets 1-5'))
  
  # save and close plot
  rm(tmp)
  dev.off()
  
}

# Concentrations by station and net

nts = list()

for(i in seq_along(levels(Net_Data$station))) {
  
  # define station
  stn = levels(Net_Data$station)[i]
  
  # define output file
  fout = paste0(outdir, stn, '_net_separated_concentrations.png')
  
  message('Plotting concentrations for sample ', stn, ' and saving it as: ', fout)
  
  # subset for plotting
  tmp = subset(Net_Data, Net_Data$station == stn)
  tmp = droplevels(tmp)
  ylim_cts = range(tmp$concentration, na.rm = TRUE)
  nts[[paste0(stn)]] = subset(Net_Data, Net_Data$station == stn)
  
  # make plot
  
  ggplot(data = tmp, aes(x = reorder(taxa, concentration, function(x) sum(x)), y = concentration)) +
    geom_bar(stat = "identity", aes(fill = net), color = "black", na.rm = T) +
    scale_fill_manual(values=rev(oceColorsViridis(6)))+
    theme_bw()+
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
    labs(title = paste0('Date: ', tmp$sample_date[1],
                   ', Site: ', tmp$site[1], 
                   ', Station: ', stn),
         x = "Sampled Taxa",
         y = expression(paste("Concentration (individuals/m"^"3"*")")),
         fill = "Net #") +
    theme(plot.title = element_text(hjust = 0.5))
  # needs title, better axes labels
  
  ggsave(filename = fout)
  
  rm(tmp)
  
}

# All calanus species concentrations by basin

ggplot(data = bsn, aes(x = reorder(taxa, concentration, function(x) sum(x)), y = log10(concentration))) +
  geom_bar(stat = "identity", aes(fill = net), size = 0.5, na.rm = T) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1, size = 10)) +
  labs(title = paste0('Calanus sp.'),
       x = "Sampled Calanus Taxa",
       y = expression(paste("Log10 of Concentration (individuals/m"^"3"*")")),
       fill = "Net #") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = paste0(figure_dir, 'calanus_concentrations.png'))

ggplot(data = bsn, aes(x = reorder(taxa, concentration, function(x) sum(x)), y = log10(concentration))) +
  geom_bar(stat = "identity", aes(fill = net), size = 0.5, na.rm = T) +
  facet_wrap(~site) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1, size = 10)) +
  labs(title = paste0('Calanus sp. by basin'),
       x = "Sampled Calanus Taxa",
       y = expression(paste("Log10 of Concentration (individuals/m"^"3"*")")),
       fill = "Net #") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = paste0(figure_dir, 'calanus_concentrations_basin.png'))

ggplot(data = bsn, aes(x = reorder(taxa, concentration, function(x) sum(x)), y = log10(concentration))) +
  geom_bar(stat = "identity", aes(fill = net), size = 0.5, na.rm = T) +
  facet_wrap(~station) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1, size = 5)) +
  labs(title = paste0('Calanus sp. by station'),
       x = "Sampled Calanus Taxa",
       y = expression(paste("Log10 of Concentration (individuals/m"^"3"*")")),
       fill = "Net #") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = paste0(figure_dir, 'calanus_concentrations_station.png'))


#C. fin concentrations by stage

ggplot(data = stages, aes(x = reorder(stage, concentration, function(x) sum(x)), y = log10(concentration))) +
  geom_bar(stat = "identity", aes(fill = net), color = NA) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  labs(title = "Calanus finmarchicus concentration by net",
       x = "C. finmarchicus stages",
       y = expression(paste("Log10 of Concentration (individuals/m"^"3"*")")),
       fill = "Net #") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(figure_dir, "calanus_finmarchicus_stages_concentrations.png"))

ggplot(data = stages, aes(x = reorder(stage, concentration, function(x) sum(x)), y = log10(concentration))) +
  geom_bar(stat = "identity", aes(fill = net), color = NA) +
  facet_wrap(~station) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  labs(title = "Calanus finmarchicus concentration by net and station",
       x = "C. finmarchicus stages",
       y = expression(paste("Log10 of Concentration (individuals/m"^"3"*")")),
       fill = "Net #") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(figure_dir, "calanus_finmarchicus_stages_station_concentrations.png"))

ggplot(data = stages, aes(x = reorder(stage, concentration, function(x) sum(x)), y = log10(concentration))) +
  geom_bar(stat = "identity", aes(fill = net), color = NA) +
  facet_wrap(~site) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  labs(title = "Calanus finmarchicus concentration by net and basin",
       x = "C. finmarchicus stages",
       y = expression(paste("Log10 of Concentration (individuals/m"^"3"*")")),
       fill = "Net #") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(figure_dir, "calanus_finmarchicus_stages_basin_concentrations.png"))


## Plotting Weighted Concentrations

# all taxa

nts = list()

for(i in seq_along(levels(Net_Data$station))) {
  
  # define station
  stn = levels(Net_Data$station)[i]
  
  # define output file
  fout = paste0(outdir, stn, '_net_separated_weighted_concentrations.png')
  
  message('Plotting weighted concentrations for sample ', stn, ' and saving it as: ', fout)
  
  # subset for plotting
  tmp = subset(Net_Data, Net_Data$station == stn)
  tmp = droplevels(tmp)
  ylim_cts = range(tmp$weighted_concentration, na.rm = TRUE)
  nts[[paste0(stn)]] = subset(Net_Data, Net_Data$station == stn)
  
  # make plot
  
  ggplot(data = tmp, aes(x = reorder(taxa, weighted_concentration, function(x) sum(x)), y = weighted_concentration)) +
    geom_bar(stat = "identity", aes(fill = net), color = "black", na.rm = T) +
    scale_fill_manual(values=rev(oceColorsViridis(6)))+
    theme_bw()+
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
    labs(title = paste0('Date: ', tmp$sample_date[1],
                        ', Site: ', tmp$site[1], 
                        ', Station: ', stn),
         x = "Sampled Taxa",
         y = expression(paste("Weighted Concentration (individuals/m"^"2"*")")),
         fill = "Net #") +
    theme(plot.title = element_text(hjust = 0.5))
  # needs title, better axes labels
  
  ggsave(filename = fout)
  
  rm(tmp)
  
}

## Siphonophora

siphonophora = c("Siphonophora",
         "Siphonophora (colony)",
         "Siphonophora (bract)",
         "Siphonophora (nectophore)",
         "Physonectae (nectophore)")

sip = subset(Net_Data, Net_Data$taxa %in% siphonophora) %>% group_by(tow, net, station) %>% summarise(weighted_concentration = sum(weighted_concentration, na.rm = T))

ggplot(data = sip, aes(x = as.factor(station), y = weighted_concentration)) +
  geom_bar(stat = "identity", aes(fill = net), color = "black", na.rm = T) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  labs(title = paste0('Siphonophora Concentration by Net and Station'),
       x = "Station",
       y = expression(paste("Weighted Concentration (individuals/m"^"2"*")")),
       fill = "Net #") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = paste0(figure_dir, 'siphonophora_weighted_concentrations.png'))

## Siphonophora and pneumatophores

pneu = subset(Net_Data, (Net_Data$taxa %in% siphonophora | Net_Data$taxa == "Siphonophora (pneumatophore)"))
pneu = droplevels(pneu)
pneu$taxa[which(pneu$taxa %in% siphonophora)] = "Siphonophora"
pneu = pneu %>% group_by(tow, net, station, taxa) %>% summarise(weighted_concentration = sum(weighted_concentration, na.rm = T))

ggplot(data = pneu, aes(x = as.factor(station), y = weighted_concentration)) +
  geom_bar(stat = "identity", aes(fill = net), color = "black", na.rm = T) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  facet_wrap(~ as.factor(taxa), scales = "free_y") +
  theme_bw() +
  labs(title = paste0('Siphonophora and Pneumatophore Concentration by Net and Station'),
       x = "Station",
       y = expression(paste("Weighted Concentration (individuals/m"^"2"*")")),
       fill = "Net #") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = paste0(figure_dir, 'pneumatophores_weighted_concentrations.png'))

## Euphausiids

euphausiids = c("Meganyctiphanes norvegica", "Meganyctiphanes sp.",
                "Thysanoessa inermis", "Thysanoessa longicaudata", "Thysanoessa raschii", "Thysanoessa gregaria", "Thysanoessa sp.",
                "Euphausia krohnii", "Euphausia sp.",
                "Nematoscelis megalops", "Nematoscelis sp.");

euph = subset(Net_Data, Net_Data$taxa %in% euphausiids) %>% group_by(tow, net, station) %>% summarise(weighted_concentration = sum(weighted_concentration, na.rm = T), count = sum(raw_count, na.rm = T))

ggplot(data = euph, aes(x = as.factor(station), y = weighted_concentration)) +
  geom_bar(stat = "identity", aes(fill = net), color = "black", na.rm = T) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  labs(title = paste0('Euphausiid Concentration by Net and Station'),
       x = "Station",
       y = expression(paste("Weighted Concentration (individuals/m"^"2"*")")),
       fill = "Net #") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = paste0(figure_dir, 'euphausiid_weighted_concentrations.png'))


#######

## Plotting relative abundance

outdir = paste0(figure_dir,'Abundances/')
create_dir(outdir)

load(paste0(cache_dir, 'Abundance_Data_Concentration.rda'))
load(paste0(cache_dir, 'Abundance_Data_Biomass.rda'))

test = Abundance_Data_Concentration %>% group_by(station, site) %>% summarise(total_concentration = sum(concentration, na.rm = T))
Abundance_Data_Concentration = Abundance_Data_Concentration %>% group_by(station, site, taxa) %>% summarise(concentration = sum(concentration, na.rm = T))
Abundance_Data_Concentration$relative_abundance = NA

for(i in 1:nrow(Abundance_Data_Concentration)) {
  Abundance_Data_Concentration$relative_abundance[i] = Abundance_Data_Concentration$concentration[i] / test$total_concentration[test$station == Abundance_Data_Concentration$station[i]]
}

Abundance_Data_Concentration = Abundance_Data_Concentration %>% group_by(site, taxa) %>% summarise(mean_abundance = mean(relative_abundance), ste_abundance = std_err(relative_abundance), std_abundance = sd(relative_abundance))
Abundance_Data_Concentration = Abundance_Data_Concentration[Abundance_Data_Concentration$mean_abundance >= 0.005,]
Abundance_Data_Concentration = Abundance_Data_Concentration %>% arrange(taxa, site)
Abundance_Data_Concentration = rbind(Abundance_Data_Concentration, Abundance_Data_Concentration[c(1:5,14:18,25,26,31:33),])
Abundance_Data_Concentration[34:48,3:5] = 0
Abundance_Data_Concentration[c(34:41,45,48),1] = "GMB"
Abundance_Data_Concentration[c(42:44,46,47),1] = "OB"
Abundance_Data_Concentration = Abundance_Data_Concentration %>% arrange(taxa, site)

ggplot(data = Abundance_Data_Concentration, aes(
  x = reorder(taxa, mean_abundance, function(x)
    - sum(x)),
  y = mean_abundance,
  fill = site,
  group = site
)) +
  geom_col(
    na.rm = F,
    color = "black",
    position = position_dodge(preserve = "single")
  ) +
  scale_fill_manual(values = rev(oceColorsViridis(6)), drop = FALSE) +
  scale_x_discrete(drop = FALSE) +
  geom_errorbar(
    stat = "identity",
    na.rm = F,
    aes(
      ymin = mean_abundance - ste_abundance,
      ymax = mean_abundance + ste_abundance,
      group = factor(site)
    ),
    position = position_dodge(0.9, preserve = "single"),
    size = 0.3,
    width = 0.5
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(
    angle = 60,
    hjust = 1,
    vjust = 1
  )) +
  labs(x = "Sampled Taxa",
       y = "Relative Abundance (% of total concentration)",
       fill = "Basin") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16))

ggsave(filename = paste0(outdir, "relative_total_abundances_concentration.png"))


test = Abundance_Data_Biomass %>% group_by(station, site) %>% summarise(total_biomass = sum(biomass, na.rm = T))
Abundance_Data_Biomass = Abundance_Data_Biomass %>% group_by(station, site, taxa) %>% summarise(biomass = sum(biomass, na.rm = T))
Abundance_Data_Biomass$relative_abundance = NA

for(i in 1:nrow(Abundance_Data_Biomass)) {
  Abundance_Data_Biomass$relative_abundance[i] = Abundance_Data_Biomass$biomass[i] / test$total_biomass[test$station == Abundance_Data_Biomass$station[i]]
}

Abundance_Data_Biomass = Abundance_Data_Biomass %>% group_by(site, taxa) %>% summarise(mean_abundance = mean(relative_abundance), ste_abundance = std_err(relative_abundance), std_abundance = sd(relative_abundance))
Abundance_Data_Biomass = Abundance_Data_Biomass[Abundance_Data_Biomass$mean_abundance >= 0.001,]
Abundance_Data_Biomass = Abundance_Data_Biomass %>% arrange(taxa, site)
Abundance_Data_Biomass = rbind(Abundance_Data_Biomass, Abundance_Data_Biomass[c(1:3,16,19,32,33,36,39,40),])
Abundance_Data_Biomass[41:50,3:5] = 0
Abundance_Data_Biomass[c(41:44,48,49),1] = "GMB"
Abundance_Data_Biomass[c(45:47,50),1] = "OB"

ggplot(data = Abundance_Data_Biomass, aes(
  x = reorder(taxa, mean_abundance, function(x)
    - sum(x)),
  y = mean_abundance
)) +
  geom_bar(
    stat = "identity",
    na.rm = T,
    color = "black",
    aes(fill = site),
    position = position_dodge(preserve = "single")
  ) +
  geom_errorbar(
    stat = "identity",
    na.rm = T,
    aes(
      ymin = mean_abundance - ste_abundance,
      ymax = mean_abundance + ste_abundance,
      group = factor(site)
    ),
    position = position_dodge(0.9, preserve = "single"),
    size = 0.3,
    width = 0.5
  ) +
  scale_fill_manual(values = rev(oceColorsViridis(6))) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(
    angle = 60,
    hjust = 1,
    vjust = 1
  )) +
  labs(x = "Sampled Taxa",
       y = "Relative Abundance (% of total biomass)",
       fill = "Basin") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16))

ggsave(filename = paste0(outdir, "relative_total_abundances_biomass.png"))


nts = list()

for(i in seq_along(levels(Abundance_Data_Concentration$station))) {
  
  # define station
  stn = levels(Abundance_Data_Concentration$station)[i]
  
  # define output file
  fout = paste0(outdir, stn, '_relative_abundances_concentration.png')
  
  message('Plotting relative abundances in concentrations for sample ', stn, ' and saving it as: ', fout)
  
  # subset for plotting
  tmp = subset(Abundance_Data_Concentration, Abundance_Data_Concentration$station == stn)
  tmp = droplevels(tmp)
  ylim_cts = range(tmp$relative_abundance, na.rm = TRUE)
  nts[[paste0(stn)]] = subset(Abundance_Data_Concentration, Abundance_Data_Concentration$station == stn)
  
  # make plot
  
  ggplot(data = tmp, aes(x = reorder(taxa, relative_abundance, function(x) -sum(x)), y = relative_abundance)) +
    geom_bar(stat = "identity", color = "black", na.rm = T) +
    theme_bw()+
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
    labs(title = paste0('Date: ', tmp$sample_date[1],
                        ', Site: ', tmp$site[1], 
                        ', Station: ', stn),
         x = "Sampled Taxa",
         y = expression(paste("Relative Abundance (% of station concentration)"))) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(filename = fout)
  
  rm(tmp)
  
}

nts = list()

for(i in seq_along(levels(Abundance_Data_Biomass$station))) {
  
  # define station
  stn = levels(Abundance_Data_Biomass$station)[i]
  
  # define output file
  fout = paste0(outdir, stn, '_relative_abundances_biomass.png')
  
  message('Plotting relative abundances in biomasses for sample ', stn, ' and saving it as: ', fout)
  
  # subset for plotting
  tmp = subset(Abundance_Data_Biomass, Abundance_Data_Biomass$station == stn)
  tmp = droplevels(tmp)
  ylim_cts = range(tmp$relative_abundance, na.rm = TRUE)
  nts[[paste0(stn)]] = subset(Abundance_Data_Biomass, Abundance_Data_Biomass$station == stn)
  
  # make plot
  
  ggplot(data = tmp, aes(x = reorder(taxa, relative_abundance, function(x) -sum(x)), y = relative_abundance)) +
    geom_bar(stat = "identity", color = "black", na.rm = T) +
    theme_bw()+
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
    labs(title = paste0('Date: ', tmp$sample_date[1],
                        ', Site: ', tmp$site[1], 
                        ', Station: ', stn),
         x = "Sampled Taxa",
         y = expression(paste("Relative Abundance (% of station biomass)"))) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(filename = fout)
  
  rm(tmp)
  
}


#####

## Matching up multinet tows and echosounder sectional plots

small_copepods = c("Acartia hudsonica", "Acartia longiremis", "Acartia sp.", "Acartia tonsa",
                   "Centropages hamatus", "Centropages typicus",
                   "Metridia longa", "Metridia lucens", "Metridia sp.",
                   "Microcalanus spp.",
                   "Microsetella norvegica",
                   "Oithona atlantica", "Oithona similis",
                   "Paracalanus spp.",
                   "Pseudocalanus spp.",
                   "Temora longicornis")

large_copepods = c("Calanus glacialis", "Calanus hyperboreus", "Calanus sp.",
                   "Paraeuchaeta norvegica", "Paraeuchaeta sp.",
                   "Pleuromamma robusta")

euphausiids = c("Meganyctiphanes norvegica", "Meganyctiphanes sp.",
                "Thysanoessa inermis", "Thysanoessa longicaudata", "Thysanoessa raschii", "Thysanoessa gregaria", "Thysanoessa sp.",
                "Euphausia krohnii", "Euphausia sp.",
                "Nematoscelis megalops", "Nematoscelis sp.")

siphonophora = c("Siphonophora",
                 "Siphonophora (bract)",
                 "Siphonophora (colony",
                 "Siphonophora (nectophore)",
                 "Physonectae (nectophore)")

Net_Data_Compressed = Net_Data[(which(Net_Data$taxa %in% small_copepods |
                                       Net_Data$taxa %in% large_copepods |
                                       Net_Data$taxa %in% euphausiids |
                                        Net_Data$taxa %in% siphonophora |
                                       Net_Data$taxa == "Siphonophora (pneumatophore)")),]
Net_Data_Compressed = Net_Data_Compressed[-c(10:20, 22, 23)]
Net_Data_Compressed$taxa[which(Net_Data_Compressed$taxa %in% euphausiids)] = "Euphausiid"
Net_Data_Compressed$taxa[which(Net_Data_Compressed$taxa %in% small_copepods)] = "Small copepod"
Net_Data_Compressed$taxa[which(Net_Data_Compressed$taxa %in% large_copepods)] = "Large copepod"
Net_Data_Compressed$taxa[which(Net_Data_Compressed$taxa %in% siphonophora)] = "Siphonophore"

stages_compressed = stages
colnames(stages_compressed)[9] = "taxa"
colnames(stages_compressed)[10] = "Total"
stages_compressed = stages_compressed %>% relocate(community_comp, .after = last_col())

stages_compressed$taxa = as.character(stages_compressed$taxa)
stages_compressed$taxa[which(stages_compressed$taxa == "CI" |
                               stages_compressed$taxa == "CII" |
                               stages_compressed$taxa == "CIII")] = "Calanus finmarchicus (early stage)"
stages_compressed$taxa[which(stages_compressed$taxa == "CIV" |
                               stages_compressed$taxa == "CV" |
                               stages_compressed$taxa == "CVIF")] = "Calanus finmarchicus (late stage)"

Net_Data_Compressed = rbind(Net_Data_Compressed, stages_compressed)
rownames(Net_Data_Compressed) = 1:nrow(Net_Data_Compressed)
Net_Data_Compressed$taxa = as.factor(Net_Data_Compressed$taxa)

save(Net_Data_Compressed, file = paste0(cache_dir, "Net_Data_Compressed.rda"))

##

load(paste0(cache_dir, 'Cfin_Stages.rda'))
load(paste0(cache_dir, 'Net_Data_Compressed.rda'))

outdir = paste0(figure_dir,'Depth_Profiles/')
create_dir(outdir)

nts = list()

for (i in seq_along(levels(Net_Data_Compressed$station))) {
  # define station
  stn = levels(Net_Data_Compressed$station)[i]
  
  # define output file
  fout = paste0(outdir, stn, '_multipanel_depth_profile.png')
  
  message('Plotting depth profile of zooplankton concentrations for sample ',
          stn,
          ' and saving it as: ',
          fout)
  
  # subset for plotting
  tmp = subset(Net_Data_Compressed, Net_Data_Compressed$station == stn)
  ylim_cts = range(tmp$concentration, na.rm = TRUE)
  nts[[paste0(stn)]] = subset(Net_Data_Compressed, Net_Data_Compressed$station == stn)
  
  # get net bottom depths
  intervals = Net_Metadata$`Pressure_(dbar)`[which(Net_Metadata$Station_Number == stn)][1:5]
  intervals = round(intervals, digits = 0)
  
  # make plot
  plot1 = ggplot(data = tmp, aes(x = concentration, y = net, fill = taxa)) +
    geom_col() +
    scale_fill_manual(drop = FALSE, values = rev(oceColorsViridis(7)),
                      labels = c("Calanus finmarchicus\n(early stage)",
                                 "Calanus finmarchicus\n(late stage)",
                                 "Euphausiid",
                                 "Large copepod",
                                 "Siphonophore\n(pneumatophore)",
                                 "Siphonophore",
                                 "Small copepod")) +
    theme_bw() +
    scale_x_continuous(position = "top") + scale_y_discrete(labels = c(
      "1" = paste0(intervals[2], "-", intervals[1]),
      "2" = paste0(intervals[3], "-", intervals[2]),
      "3" = paste0(intervals[4], "-", intervals[3]),
      "4" = paste0(intervals[5], "-", intervals[4]),
      "5" = paste0("0-", intervals[5]))
    ) + labs(
      x = expression(paste("Concentration (individuals/m" ^ "3" * ")")),
      y = "Net Depth Interval (m)",
      fill = "Taxa"
    ) +
    theme(text = element_text(size = 16))
  
  plot2 = ggplot(data = tmp, aes(x = biomass, y = net, fill = taxa)) +
    geom_col() +
    scale_fill_manual(drop = FALSE, values = rev(oceColorsViridis(7)),
                      labels = c("Calanus finmarchicus\n(early stage)",
                                 "Calanus finmarchicus\n(late stage)",
                                 "Euphausiid",
                                 "Large copepod",
                                 "Siphonophore\n(pneumatophore)",
                                 "Siphonophore",
                                 "Small copepod")) +
    theme_bw() +
    scale_x_continuous(position = "top") + scale_y_discrete(labels = c(
      "1" = paste0(intervals[2], "-", intervals[1]),
      "2" = paste0(intervals[3], "-", intervals[2]),
      "3" = paste0(intervals[4], "-", intervals[3]),
      "4" = paste0(intervals[5], "-", intervals[4]),
      "5" = paste0("0-", intervals[5]))
    ) + labs(
      x = expression(paste("Biomass (g/m" ^ "3" * ")")),
      y = NULL,
      fill = "Taxa"
    ) + theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) +
    theme(text = element_text(size = 16))
  
  plot3 = ggarrange(plot1, NULL, plot2, ncol = 3, labels = c("A", "", "B"), widths = c(1, 0.1, 1), align = "h", common.legend = T, legend = "right")
  annotate_figure(plot3, top = paste0(
    'Date: ',
    tmp$sample_date[1],
    ', Site: ',
    tmp$site[1],
    ', Tow: ',
    paste0("multi",sprintf("%03s", as.character(tmp$tow[1])))),
    fig.lab.size = 20) + theme_bw() +
    theme(panel.border = element_rect(color = NA))
  
  ggsave(filename = fout, width = 9, units = "in")
  
  # rm(tmp)
  
}


Net_Data_Basin = Net_Data_Compressed %>% group_by(taxa, net, site) %>% summarise(mean_conc = mean(concentration, na.rm = T),
                                                                            mean_mass = mean(biomass, na.rm = T),
                                                                            ste_conc = std_err(concentration),
                                                                            ste_mass = std_err(biomass))


plot4 = ggplot(data = Net_Data_Basin, aes(x = (mean_conc), y = net, fill = taxa)) +
  geom_col(position = "stack") +
  scale_fill_manual(drop = F, values = rev(oceColorsViridis(7))) +
  theme_bw() +
  scale_x_continuous(position = "top") +
  labs(
    x = expression(paste("Concentration (individuals/m" ^ "3" * ")")),
    y = "Net Number",
    fill = "Taxa"
  ) + theme(text = element_text(size = 16)) +
  facet_wrap(~site)

plot5 = ggplot(data = Net_Data_Basin, aes(x = (mean_mass), y = net, fill = taxa)) +
  geom_col(position = "stack") +
  scale_fill_manual(drop = F, values = rev(oceColorsViridis(7))) +
  theme_bw() +
  scale_x_continuous(position = "top") +
  labs(
    x = expression(paste("Biomass (g/m" ^ "3" * ")")),
    y = "Net Number",
    fill = "Taxa"
  ) + theme(text = element_text(size = 16)) +
  facet_wrap(~site)

plot6 = ggarrange(plot4, plot5, ncol = 1, align = "v", common.legend = T, legend = "right") +
  theme_bw() +
  theme(panel.border = element_rect(color = NA))
plot6
ggsave(filename = paste0(outdir, "basin_averaged_multipanel_depth_profile.png"), plot = plot6)

#####

Net_Data_Compressed %>%
  filter(taxa == "Euphausiid") %>%
  group_by(net,site) %>%
  summarise(mean_conc = mean(concentration), ste_conc = std_err(concentration), std_conc = sd(concentration))

#####

## Abundance of late-stage C. fin for September 21st only

stages_21 = stages %>% 
  filter(sample_date == "2020-09-21") %>%
  filter(stage == c("CIV", "CV", "CVIF")) %>%
  group_by(stage, net) %>%
  summarize(mean_count = mean(raw_count),
            mean_conc = mean(weighted_concentration))

ggplot(data = stages_21) +
  geom_bar(aes(x = mean_conc, y = net, fill =stage), stat = "identity") +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  labs(title = "Calanus finmarchicus concentrations on Sept 21",
       x = expression(paste("Mean Weighted Concentration (individuals/m"^"2"*")")),
       y = "Net #",
       fill = "Stage") +
  theme(plot.title = element_text(hjust = 0.5))

file = "21Sept_Multinet_Data.png"
ggsave(filename = file, path = figure_dir, width = 5, units = "in")

