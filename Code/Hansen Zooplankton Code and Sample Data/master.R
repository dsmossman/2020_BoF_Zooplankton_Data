# Master

# setup -------------------------------------------------------------------

setwd("~/Documents/BoF2020_Cruise/R_Code/Hansen Zooplankton Code and Sample Data")

# libraries
library(readxl)
library(oce)
library(ocedata)
library(marmap)
library(plyr)
library(R.utils)
library(ggplot2)

# functions
sourceDirectory('src/', modifiedOnly = F)

# set up project structure
function_dir = 'src/';  create_dir(function_dir)
cache_dir = 'cache/';         create_dir(cache_dir)
data_dir = 'data/';           create_dir(data_dir)
figure_dir = 'figures/';      create_dir(figure_dir)
report_dir = 'reports/';       create_dir(report_dir)

# get map data
get_map_data(cache_dir)

# process log file -----------------------------------------------------
# DM: Log file is the event log it looks like, indicates times and lat/longs for 
# CTD and net casts; I may need to load in the multinet and cage datasets to get
# enough information
# DM: But, this section basically "translates" the event log into something that can
# then be worked on in the rest of the R code; I will probably want to make it so my
# files are translated as closely as possible to what Hansen has established, for minimal
# fuss and reworking of the code

# ocean data log file
log_file = 'data/log.xlsx'
log_ofile = 'cache/log.rda'

if(!file.exists(log_ofile)){
  
  # read in data
  log = read_excel(log_file, sheet = 1)
  comments = read_excel(log_file, sheet = 2)
  
  # copy for output
  log_out = log
  
  # determine CTD station lat/lon
  log$lat = ddm2dd(log$lat)
  log$lon = -ddm2dd(log$lon)
  
  # add timestamp
  log$datetime = as.POSIXct(paste0(log$date, ' ', log$time), format = "%d-%b-%Y %H:%M")
  log$date = as.Date(log$date, format = '%d-%b-%Y')
  
  # fix column types
  # factors
  # log$sticker_id = as.factor(log$sticker_id)
  # log$station = as.factor(log$station)
  # log$type = as.factor(log$type)
  # log$whales = as.factor(log$whales)
  # numeric
  log$depth_bottom_snd = as.numeric(log$depth_bottom_snd)
  log$depth_bottom_ctd = as.numeric(log$depth_bottom_ctd)
  log$depth_net_ctd = as.numeric(log$depth_net_ctd)
  log$flow_start = as.numeric(log$flow_start)
  log$flow_stop = as.numeric(log$flow_stop)
  # logical
  # log$cryo_sample = as.logical(log$cryo_sample)
  # log$zoop_sample = as.logical(log$zoop_sample)
  
  # compute volume filtered from flow meter [see pg 5-6 in docs/flow_meter_manual.pdf for instructions]
  r = 26873                                   # rotor constant
  d = 0.75                                    # net diameter (m)
  diff = log$flow_stop-log$flow_start         # difference (counts)
  dist = (diff * r) / 999999                  # distance traveled (m)
  log$vol_flow = (pi * d^2 * dist) / 4        # volume filtered (m^3)
  
  # DM: multinet does this a different way, and might compute this automatically for me
  # I'll need to check the multinet files to see
  
  # estimate minimum volume filtered from net depth [based on volume of a cylinder of height = water depth]
  log$vol_depth = pi*((d/2)^2)*log$depth_net_ctd  # volume filtered (m^3)
  
  # compare
  plot(log$vol_depth, log$vol_flow)
  
  # save
  save(log, file = log_ofile)
  message('Log file saved as: ', log_ofile)
  
} else {
  message('Loading log file from: ', log_ofile)
  load(log_ofile)
}

# process ctd -------------------------------------------------------------
# DM: Section processes the CTD data; is this something I need if I am only computing
# abundance/concentration of plankton?
# Should probably work thru it anyway with the cage CTD data, if I can find it


# ctd data directory
ctd_dir = paste0(data_dir,'ctd/')

# ctd output file
ctd_ofile = paste0(cache_dir, 'ctd.rda')

if (!file.exists(ctd_ofile)) {
  
  # raw files
  ctd_files = list.files(ctd_dir, pattern = '*_final.cnv')
  
  # metadata
  ship = 'Shelagh'
  cruise = 'GSL2017'
  scientist = 'Hansen Johnson'
  institute = 'Dalhousie University / Canadian Whale Institute'
  
  CTD = list()
  for(i in seq_along(ctd_files)){
    
    # read data
    ctd = read.ctd.sbe(paste0(ctd_dir,ctd_files[i]), debug = F, monitor = F)
    
    # extract pressure
    pressure = ctd[['data']]$pressure
    
    # extract and convert time
    time = as.POSIXct(ctd[['data']]$time, origin = "2000-01-01 00:00", tz = "ADT") # I'm not sure why this is UTC, but this must be correct because it aligns with my notes and the system start time
    
    # time check
    time[1] == ctd[['metadata']]$startTime
    
    ctd[['time']] = time
    
    # extract station ID
    s = unlist(strsplit(ctd_files[i], split = '_'))
    id = s[2]
    
    ctd[['latitude']] = log$lat[which(log$id == paste0('CTD', id))]
    ctd[['longitude']] = log$lon[which(log$id == paste0('CTD', id))]
    
    # define cruise and ship
    ctd[['cruise']] = cruise
    ctd[['ship']] = ship
    ctd[['scientist']] = scientist
    ctd[['institute']] = institute
    
    # add to list
    CTD[[i]] = ctd
  }
  
  # save
  save(CTD, file = ctd_ofile)
  message('CTD data saved as: ', ctd_ofile)
}else{
  message('Loading CTD data from: ', ctd_ofile)
  load(ctd_ofile)
}

# process net data --------------------------------------------------------
# DM: Bit that I will need to work on the most I think, since the net excel files are not
# formatted the same way

# net data directory
net_dir = paste0(data_dir,'net/')

# net output file
net_ofile = paste0(cache_dir, 'net.rda')

#if (!file.exists(net_ofile)) {

# read in data
n1 = read_excel(paste0(net_dir,'leg_1.xlsx'), sheet = 1) # leg 1
n2 = read_excel(paste0(net_dir,'leg_2.xlsx'), sheet = 1) # leg 2

# make column names equivalent
n1$SEX = NA
n2$ANALYSIS = NA

# check equivalence
if(FALSE %in% (names(n1) %in% names(n2))){
  warning('DATA FRAMES NOT EQUIVALENT')
}

# join leg 1 and 2 datasets
net = rbind.data.frame(n1,n2)

# convert/assign column variable types
net$SAMPLEID = as.factor(net$SAMPLEID)
net$TAXA = as.factor(net$TAXA)

## DM: Below is QA/QC work, might not be necessary for my data

# remove and replace text in data value column
net$DATA_VALUE = gsub(pattern = 'N/A', replacement = NA, x = net$DATA_VALUE)
net$DATA_VALUE = gsub(pattern = 'too much sediment', replacement = NA, x = net$DATA_VALUE)
net$DATA_VALUE = as.numeric(net$DATA_VALUE)

# remove and replace text in stage column
net$STAGE = gsub(pattern = 'C', replacement = '', x = net$STAGE)
net$SEX[grep(pattern = ' female', x = net$STAGE)] = 'female'
net$SEX[grep(pattern = ' male', x = net$STAGE)] = 'male'
net$STAGE = gsub(pattern = 'female', replacement = '', x = net$STAGE)
net$STAGE = gsub(pattern = 'male', replacement = '', x = net$STAGE)
net$STAGE = trimws(net$STAGE)

# fix euphasiid names
net$TAXA = gsub(pattern = 'Euphausidae', replacement = 'Euphausiidae', x = net$TAXA)
net$TAXA = gsub(pattern = 'Euphausiid', replacement = 'Euphausiidae', x = net$TAXA)
net$TAXA = gsub(pattern = 'Euphausiidaeae', replacement = 'Euphausiidae', x = net$TAXA)

# fix evande names
net$TAXA = gsub(pattern = 'Evadne nordmanni', replacement = 'Evadne nordmanii', x = net$TAXA)


# rectify capitals
net$WHAT_WAS_IT = toupper(net$WHAT_WAS_IT)
net$BUG_SIZE = toupper(net$BUG_SIZE)

# add column for volume filtered
net$VOL = NA
net$WHALE = NA

# loop through and associate each sample with log info
# DM: so this is why I need the event/cage/multinet log information, especially
# the volume filtered
# Will not need the whale stuff tho

for(i in 1:nrow(log)){
  # station
  net$STN[which(as.character(net$SAMPLEID) == as.character(log$sticker_id[i]))] = log$station[i]
  # depth
  net$DEPTH[which(as.character(net$SAMPLEID) == as.character(log$sticker_id[i]))] = log$type[i]
  # volume filtered
  net$VOL[which(as.character(net$SAMPLEID) == as.character(log$sticker_id[i]))] = log$vol_depth[i]
  # whale presence
  net$WHALE[which(as.character(net$SAMPLEID) == as.character(log$sticker_id[i]))] = log$whales[i]
}

# calculate concentration 
net$CONC = net$DATA_VALUE / net$SPLIT_FRACTION / net$VOL
net$RAW_COUNT = net$DATA_VALUE / net$SPLIT_FRACTION

   # save
   save(net, file = net_ofile)
   message('Net data saved as: ', net_ofile)
#}else{
 # message('Loading net data from: ', net_ofile)
  #load(net_ofile)
#}

## Plotting
# DM: for these, I'll need to mess around to eliminate the whale category;
# I also want to figure out the categories that make sense for me to look at,
# e.g. by basin, by net #, by date, by station

# plot biomass by station -------------------------------------------------
# DM: not necessary for me since we did not weigh the samples
   
# make output directory
outdir = paste0(figure_dir,'biomass/'); create_dir(outdir)

# make a subset for plotting
bio = subset(net, net$WHAT_WAS_IT == 'WEIGHT' & net$BUG_SIZE == 'MESO+MACRO' & net$DEPTH == 'FULL')
bio = droplevels(bio)

# set ylim
ylim_bio = range(bio$CONC)

# plot data
ggplot(data = bio, aes(x = STN, y = RAW_COUNT))+
  geom_bar(stat="identity", aes(fill = WHALE), color = 'black', na.rm = T)+
  scale_fill_manual(values=c("white", "gray", "darkslategray"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggsave(filename = paste0(outdir, 'biomass.png'))

# plot counts by station ----------------------------------------------

# make output directory
outdir = paste0(figure_dir,'counts/'); create_dir(outdir)

# make a subset for plotting
cts = subset(net, net$WHAT_WAS_IT == 'COUNT' & net$DEPTH == 'FULL')
cts = droplevels(cts)
cts$STN = as.factor(cts$STN)

# set ylim
ylim_cts = range(cts$RAW_COUNT)

nts = list()

# loop through and generate plot for each station
for(i in seq_along(levels(cts$STN))){
  
  # define station
  stn = levels(cts$STN)[i]
  
  # define output file
  fout = paste0(outdir, stn, '_counts.png')
  
  message('Plotting raw counts for sample ', stn, ' and saving it as: ', fout)
  
  # open plot
  png(filename = fout, width = 7, height = 5, units = 'in', res = 200)
  
  # set margins for plotting
  par(mar = c(10,4,4,2))
  
  # subset for plotting
  tmp = subset(cts, cts$STN == stn)
  nts[[paste0(stn)]] = subset(cts, cts$STN == stn)
  
  # make plot
  plot(tmp$RAW_COUNT~as.factor(tmp$TAXA), 
       las = 2, xlab = '', ylab = 'counts per m^2', 
       ylim = ylim_cts, cex.axis = 0.5, main = paste0('Station: ', stn, ' (Sample ID: ', tmp$STN[1], ', Sticker ID: ', tmp$SAMPLEID[1], ')'))
  
  # close plot
  dev.off()
}

# plot target species -----------------------------------------------------

# make output directory
outdir = paste0(figure_dir,'target_species/'); create_dir(outdir)

### FINMARCHICUS ###

# make a subset for plotting
spp = subset(net, net$TAXA == 'Calanus finmarchicus' & net$DEPTH == 'FULL')
spp = droplevels(spp)

# plot data
ggplot(data = spp, aes(x = STN, y = RAW_COUNT))+
  geom_bar(stat="identity", aes(fill = STAGE), color = 'black', na.rm = T)+
  scale_fill_manual(values=rev(oceColorsViridis(6)))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggsave(filename = paste0(outdir, 'calanus_finmarchicus.png'))

### HYPERBOREUS ###

# make a subset for plotting
spp = subset(net, net$TAXA == 'Calanus hyperboreus' & net$DEPTH == 'FULL')
spp = droplevels(spp)

# plot data
ggplot(data = spp, aes(x = STN, y = RAW_COUNT))+
  geom_bar(stat="identity", aes(fill = STAGE), color = 'black', na.rm = T)+
  scale_fill_manual(values=rev(oceColorsViridis(4)))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggsave(filename = paste0(outdir, 'calanus_hyperboreus.png'))

### HYPERBOREUS ###

# make a subset for plotting
spp = subset(net, net$TAXA == 'Calanus hyperboreus' & net$DEPTH == 'FULL')
spp = droplevels(spp)

# plot data
ggplot(data = spp, aes(x = STN, y = RAW_COUNT))+
  geom_bar(stat="identity", aes(fill = STAGE), color = 'black', na.rm = T)+
  scale_fill_manual(values=rev(oceColorsViridis(4)))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggsave(filename = paste0(outdir, 'calanus_hyperboreus.png'))

### GLACIALIS ###

# make a subset for plotting
spp = subset(net, net$TAXA == 'Calanus glacialis' & net$DEPTH == 'FULL')
spp = droplevels(spp)

# plot data
ggplot(data = spp, aes(x = STN, y = RAW_COUNT))+
  geom_bar(stat="identity", aes(fill = STAGE), color = 'black', na.rm = T)+
  scale_fill_manual(values=rev(oceColorsViridis(2)))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggsave(filename = paste0(outdir, 'calanus_glacialis.png'))

### ALL CALANUS ###

# target taxa
taxa_list = c('Calanus finmarchicus', 
              'Calanus glacialis', 
              "Calanus hyperboreus")

# make a subset for plotting
spp = subset(net, net$DEPTH == 'FULL')
spp = spp[spp$TAXA %in% taxa_list,]

# remove other stages
spp$STAGE[!spp$STAGE %in% c('I', 'II', 'III', 'IV', 'V', 'VI')] = NA

# convert whale maybe/absent to absent
spp$WHALE[spp$WHALE=='maybe'] ='absent'

# remove unused levels
spp = droplevels(spp)

# plot data
ggplot(data = spp, aes(x = STN, y = RAW_COUNT))+
  # geom_bar(stat="identity", aes(fill = STAGE), color = 'black', na.rm = T)+
  geom_bar(stat="identity", aes(fill = STAGE, color = WHALE), size = 0.5, na.rm = T)+
  scale_color_manual(values=c(NA, 'red'))+
  scale_fill_manual(values=rev(oceColorsViridis(6)))+
  facet_wrap(~TAXA, nrow = 1)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size = 6))

ggsave(filename = paste0(outdir, 'calanus.png'), width = 8, height = 4, units = 'in')

### TARGET ###

# target taxa
taxa_list = c('Calanus finmarchicus',
              'Calanus glacialis',
              "Calanus hyperboreus",
              "Oithona similis",
              "Oithona atlantica",
              "Temora longicornis",
              "Pseudocalanus sp",
              "Evadne nordmanii",
              "Euphausiidae")

# target taxa

# make a subset for plotting
spp = subset(net, net$DEPTH == 'FULL')
spp = spp[spp$TAXA %in% taxa_list,]

# remove other stages
spp$STAGE[!spp$STAGE %in% c('I', 'II', 'III', 'IV', 'V', 'VI')] = NA

# convert whale maybe/absent to absent
spp$WHALE[spp$WHALE=='maybe'] ='absent'

# remove unused levels
spp = droplevels(spp)

# plot data
ggplot(data = spp, aes(x = STN, y = RAW_COUNT))+
  geom_bar(stat="identity", aes(fill = STAGE, color = WHALE), size = 0.5, na.rm = T)+
  scale_color_manual(values=c(NA, 'red'))+
  scale_fill_manual(values=rev(oceColorsViridis(6)), na.value = 'grey')+
  facet_wrap(~TAXA, nrow = 3)+
  theme_bw()+
  xlab('STATION')+
  ylab('RAW COUNTS')+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size = 6))

ggsave(filename = paste0(outdir, 'target_species.png'))

# plot station map --------------------------------------------------------
# DM: I think I've already done this

# read in map data
load('cache/map.rda')

# plotting dimensions
mlon = mean(log$lon)
mlat = mean(log$lat)
span = 275

png('figures/station_map.png', width = 7, height = 6, units = 'in', res = 250)

# plot coastline
plot(coastlineWorldFine, clon = mlon, clat = mlat, span = span)

# plot bathymetry
contour(bathyLon,bathyLat,bathyZ,levels = c(-50, -100, -150, -200, -250),lwd = c(1, 1, 2, 2, 3),lty = c(3, 1, 3, 1, 3),drawlabels = F,add = TRUE,col = 'darkgray')

# add depth legend
legend("bottomright",lwd = c(1, 1, 2, 2, 3),lty = c(3, 1, 3, 1, 3),col = 'darkgray',seg.len = 3,cex = 0.8,title = "Depth [m]",legend = c("50", "100", "150", "200", "250"),bg= "white")

#### add data ####
with(subset(log, log$type=='FULL' & log$whales == 'present'), points(lon, lat, pch=21, bg = 'red'))
with(subset(log, log$type=='FULL' & log$whales != 'present'), points(lon, lat, pch=21, bg = 'blue'))
with(subset(log, log$type=='FULL'), text(lon, lat, labels = 1:16, pos = c(2,4), cex = 0.7, col = 'darkslategrey'))

legend('bottomleft', pch = c(21,21), pt.bg = c('red', 'blue'), legend = c('Present', 'Absent'), title = 'Right whales', cex = 1)
# title('Sampling locations')

dev.off()

