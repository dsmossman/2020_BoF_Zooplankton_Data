rm(list=ls())

library(tidyverse)
library(oce)
library(ocedata)
library(R.utils)
library(gtools)
library(opcr)
library(ggpubr)


## Project Structure
setwd("C:/Users/Delphine/Documents/2020_BoF_Zooplankton_Data")

sourceDirectory('Code/Hansen Zooplankton Code and Sample Data/src',
                modifiedOnly = F)

function_dir = 'Code/Hansen Zooplankton Code and Sample Data/src/'
processed_dir = 'Processed_Data/'
data_dir = 'Raw_Data/'
figure_dir = 'Visuals/'
report_dir = getwd()
#####
setwd("C:/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Raw_Data/Cage/OPC/OPC_Data/d00")
## opc_process_cruise("BoF_2020", data_dir = getwd(), output_dir = "/Users/delphine/Documents/BoF2020_Cruise/Processed_Data/Cage/")

processed_file_path = "C:/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Processed_Data/Cage/"
processed_files = list.files(path = processed_file_path, pattern = "BoF_2020_.*rds")
processed_files = mixedsort(processed_files)

for(x in 1:length(processed_files)) {
  if (x <= 9) {
    assign(paste0("OPC00", x), readRDS(paste0(
      processed_file_path, processed_files[x]
    )))
  }
  else {
    assign(paste0("OPC0", x), readRDS(paste0(
      processed_file_path, processed_files[x]
    )))
  }
}

OPC_OB_Data = list(OPC003, OPC004, OPC005, OPC006, 
                    OPC007, OPC008, OPC009, OPC010, OPC011, OPC012,
                    OPC033, OPC034, OPC035, OPC036, OPC037, OPC038)
names(OPC_OB_Data) = c("OPC003", "OPC004", "OPC005", "OPC006", 
                       "OPC007", "OPC008", "OPC009", "OPC010", "OPC011", "OPC012",
                       "OPC033", "OPC034", "OPC035", "OPC036", "OPC037", "OPC038")

OPC_GMB_Data = list(OPC013, OPC014, OPC015, OPC016, OPC017, OPC018,
                     OPC019, OPC020, OPC021, OPC022, OPC023, OPC024, OPC025, OPC026,
                     OPC027, OPC028, OPC029, OPC030, OPC031, OPC032)
names(OPC_GMB_Data) = c("OPC013", "OPC014", "OPC015", "OPC016", "OPC017", "OPC018",
                        "OPC019", "OPC020", "OPC021", "OPC022", "OPC023", "OPC024", "OPC025", "OPC026",
                        "OPC027", "OPC028", "OPC029", "OPC030", "OPC031", "OPC032")

opc_plot_abundance(OPC028, dz = 5, min_size = 1.5, max_size = 2) + 
  theme(text = element_text(size = 16)) + 
  plot_annotation(title = "OPC028 Abundance Data",theme = theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 16)))
ggsave(paste0("OPC028", "_Plot_Abundance_Only.png"), path = "/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Processed_Data/Graphs",
       width = 6, height = 10, units="in")

opc_plot_image(OPC010) + theme(text = element_text(size = 16)) + plot_annotation(title = "Example OPC Data",
                                         theme = theme(plot.title = element_text(hjust = 0.5),
                                                       text = element_text(size = 16)))

opc_plot_diagnostics(OPC013) + plot_annotation(title = "Date: 21 Sept 2020,\nSite: GMB,\nOPC013",
                                         theme = theme(plot.title = element_text(hjust = 0.5)))
plot1 = opc_plot_flags(filter(OPC010, flag != 'depth')) + theme(text = element_text(size = 16))
plot2 = opc_plot_flags(filter(OPC020, flag != 'depth')) + theme(text = element_text(size = 16))
ggarrange(plot1, plot2) + plot_annotation(title = "Example OPC Data",
                                          theme = theme(plot.title = element_text(hjust = 0.5),
                                                        text = element_text(size = 16)))


plot = opc_plot_multipanel(filter(OPC003, flag != 'depth'), good_only = FALSE)
plot = plot + plot_annotation(title = names(OPC_OB_Data)[1], theme = theme(plot.title = element_text(hjust = 0.5)))
ggsave(paste0("OPC003", "_Plot.png"), device = "png", path = "/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Processed_Data/Graphs",
       width = 6, height = 10, units="in")


for(y in 2:length(OPC_OB_Data)){
  test = try(opc_plot_multipanel(OPC_OB_Data[[y]]), silent = T)
  if(class(test)[1] == "try-error") {
    plot = opc_plot_multipanel(OPC_OB_Data[[y]], good_only = F, amin_size = 1.5, dz=5)
    plot = plot + plot_annotation(title = names(OPC_OB_Data)[y], theme = theme(plot.title = element_text(hjust = 0.5)))
    ggsave(paste0(names(OPC_OB_Data)[y], "_Plot.png"), device = "png", path = "/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Processed_Data/Graphs",
           width = 6, height = 10, units="in")
  }
  else {
    plot = opc_plot_multipanel(OPC_OB_Data[[y]], amin_size = 1.5, dz=5)
    plot = plot + plot_annotation(title = names(OPC_OB_Data)[y], theme = theme(plot.title = element_text(hjust = 0.5)))
    ggsave(paste0(names(OPC_OB_Data)[y], "_Plot.png"), device = "png", path = "/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Processed_Data/Graphs",
           width = 6, height = 10, units="in")
  }
}


for(y in 1:length(OPC_GMB_Data)){
  test = try(opc_plot_multipanel(OPC_GMB_Data[[y]],  amin_size = 1.5), silent = T)
  if(class(test)[1] == "try-error") {
    plot = opc_plot_multipanel(OPC_GMB_Data[[y]], good_only = F, amin_size = 1.5, dz=5)
    plot = plot + plot_annotation(title = names(OPC_GMB_Data)[y], theme = theme(plot.title = element_text(hjust = 0.5)))
    ggsave(paste0(names(OPC_GMB_Data)[y], "_Plot.png"), device = "png", path = "/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Processed_Data/Graphs",
           width = 6, height = 10, units="in")
  }
  else {
    plot = opc_plot_multipanel(OPC_GMB_Data[[y]], amin_size = 1.5, dz=5)
    plot = plot + plot_annotation(title = names(OPC_GMB_Data)[y], theme = theme(plot.title = element_text(hjust = 0.5)))
    ggsave(paste0(names(OPC_GMB_Data)[y], "_Plot.png"), device = "png", path = "/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Processed_Data/Graphs",
           width = 6, height = 10, units="in")
  }
}


plot1 = opc_plot_image(OPC_OB_Data[[5]])
plot1 = plot1 + plot_annotation(title = names(OPC_OB_Data)[5], theme = theme(plot.title = element_text(hjust = 0.5)))
plot2 = opc_plot_image(OPC_OB_Data[[6]])
plot2 = plot2 + plot_annotation(title = names(OPC_OB_Data)[6], theme = theme(plot.title = element_text(hjust = 0.5)))

library(ggpubr)
plot3 = ggarrange(plot1, plot2, ncol = 2, common.legend = T, legend = "bottom")

setwd("C:/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/")
OPC_Data_Full = rbind(do.call("rbind", OPC_OB_Data), do.call("rbind", OPC_GMB_Data))
OPC_Data_Full = OPC_Data_Full[order(OPC_Data_Full$time),]
OPC_Data_Full$basin = NA


for(i in 1:nrow(OPC_Data_Full)) {
  cast_day = as.character(trunc.POSIXt(OPC_Data_Full$time[i], units = "days"))
  if (cast_day == "2020-09-19" |
      cast_day == "2020-09-20" |
      cast_day == "2020-09-26") {
    OPC_Data_Full$basin[i] = "OB"
  } else {
    OPC_Data_Full$basin[i] = "GMB"
  }
}

colnames(OPC_Data_Full)[11] = "OPC_cast_num"
colnames(OPC_Data_Full)[7] = "time"

OPC_Data_Full = OPC_Data_Full %>% group_by(OPC_cast_num) %>% mutate(speed=c(0,diff(depth)/diff(secs)))

save(OPC_Data_Full, file = paste0(processed_dir, 'OPC_Data_Full.rda'))
#####
load(paste0(processed_dir, 'OPC_Data_Full.rda'))

dz = 1
min_size = 1.5
max_size = 2

good_casts = c(2, 5, 7, 8, 13, 14, 15, 16, 17, 19, 21, 22, 23, 27, 28, 29, 30,
               31, 32, 33, 35, 36)

OPC_to_C5 = function(concentration, speed) {
  B0 = 0.0384
  B1 = 0.5343
  B2 = 0.8001
  
  temp = (1/B1) * (log10(concentration) - B0 - B2 * speed)
  return(10^temp)
  
}

# calculate abundance profiles
prf = OPC_Data_Full %>%
  ungroup() %>%
  filter(flag == 0) %>%
  filter(OPC_cast_num %in% good_casts) %>%
  group_by(OPC_cast_num, basin) %>%
  reframe(opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size))
# the "across()" is necessary because just using . uses the entire, ungrouped OPC_Data_Full
# dataframe, which we do not want
prf$speed = 0
prf$concentration[is.na(prf$concentration)] = 0

prf_spd = OPC_Data_Full %>%
  ungroup() %>%
  filter(flag == 0) %>%
  filter(OPC_cast_num %in% good_casts) %>%
  mutate(depth=cut(depth, seq(0,200,1),labels=seq(0,199,1))) %>%
  group_by(OPC_cast_num, depth, .drop=F) %>%
  reframe(speed = mean(speed,na.rm=T))

for(i in 1:nrow(prf_spd)) {
  prf$speed[prf$OPC_cast_num == prf_spd$OPC_cast_num[i] &
              prf$depth == prf_spd$depth[i]] = prf_spd$speed[i]
}

prf$C5_concentration = OPC_to_C5(prf$concentration,prf$speed)
prf$C5_concentration[is.nan(prf$C5_concentration)] = 0

save(prf, file = paste0(processed_dir, 'OPC_Data_Abundance_Estimate.rda'))

# biomass estimate from Hansen Johnson's work

prf_b = OPC_Data_Full %>%
  filter(depth>1) %>%
  filter(flag == 0) %>%
  group_by(OPC_cast_num, basin) %>%
  reframe(
    opc_biomass(across(), dz = dz, min_size = min_size, max_size = max_size)) %>%
  transform(depth=cut(depth, seq(0,200,5),right=F,labels=seq(0,195,5)))

save(prf_b, file = paste0(processed_dir, 'OPC_Data_Biomass_Estimate.rda'))

# average cast by station
st_prf = prf %>%
  ungroup() %>%
  transform(depth=cut(depth, seq(0,200,5),right=F,labels=seq(0,195,5))) %>%
  group_by(OPC_cast_num, basin, depth) %>%
  reframe(concentration = mean(concentration, na.rm=T),
          C5_concentration = mean(C5_concentration, na.rm=T))

st_prf_b = prf_b %>%
  group_by(OPC_cast_num, depth, basin) %>%
  summarise(
    biomass = mean(mass, na.rm = T)/1e3,.groups = 'drop')

# average station by basin
ba_prf = st_prf %>%
  ungroup() %>%
  group_by(depth, basin) %>%
  reframe(#speed = mean(speed, na.rm=T),
    m = mean(concentration, na.rm = T),
    upper = m + sd(concentration, na.rm = T),
    lower = m - sd(concentration, na.rm = T),
    C5_m = mean(C5_concentration, na.rm = T),
    C5_upper = C5_m + sd(C5_concentration, na.rm=T),
    C5_lower = C5_m - sd(C5_concentration, na.rm=T)#,
    # C5_m_2 = OPC_to_C5(m,speed),
    # C5_upper_2 = OPC_to_C5(upper,speed),
    # C5_lower_2 = OPC_to_C5(lower,speed)
  )
ba_prf$C5_lower_2[is.nan(ba_prf$C5_lower_2)] = 0

ba_prf_b = st_prf_b %>%
  group_by(depth, basin) %>%
  summarise(
    m = median(biomass, na.rm = T),
    upper = quantile(biomass, probs = 0.75, na.rm = T),
    lower = quantile(biomass, probs = 0.25, na.rm = T),
    .groups = 'drop'
  )

# Plots
plot1 = ggplot() +
  geom_ribbon(data = ba_prf, aes(xmin = C5_lower, xmax = C5_upper, y = depth, group = basin, fill = basin),
              alpha = 0.3) +
  geom_path(data = ba_prf, aes(x = C5_m, y = depth, color = basin, group = basin)) +
  scale_fill_manual(values = c('OB' = 'red', 'GMB' = 'darkslategrey')) +
  scale_color_manual(values = c('OB' = 'red', 'GMB' = 'darkslategrey')) +
  scale_y_discrete(limits = rev, breaks = seq(0,200,20),drop=F,labels=seq(0,200,20)) +
  geom_hline(yintercept="160",color="red",linetype=2) +
  geom_hline(yintercept="190",color="darkslategrey",linetype=2) +
  coord_cartesian(expand = F, xlim=c(0,5000)) +
  labs(x = expression(paste("Estimated C5 Concentration (individuals/", m^3,')')),
       y = 'Depth (m)', 
       fill = NULL, color = NULL)+
  theme_bw() + theme(text = element_text(size = 16))
ggsave(plot1, filename = paste0(figure_dir, "OPC_Basin_Averaged_Abundance.png"),
       height = 6, width = 5, units = "in")

plot2 = ggplot() +
  geom_ribbon(data = ba_prf_b, aes(xmin = lower, xmax = upper, y = depth, group = basin, fill = basin),
              alpha = 0.3) +
  geom_path(data = ba_prf_b, aes(x = m, y = depth, color = basin, group = basin)) +
  scale_fill_manual(values = c('OB' = 'red', 'GMB' = 'darkslategrey')) +
  scale_color_manual(values = c('OB' = 'red', 'GMB' = 'darkslategrey')) +
  scale_y_discrete(limits = rev, breaks = seq(0,200,20)) +
  coord_cartesian(expand = FALSE) +
  labs(x = "Estimated Particle Biomass (g)",
       y = 'Depth (m)', 
       fill = NULL, color = NULL)+
  theme_bw() + theme(text = element_text(size = 16))

# Ribbon plots of particle abundance by size class and depth bin

OPC_Data_Full$ZBin = cut(OPC_Data_Full$depth, breaks = c(0,40,80,120,160,200), include.lowest = TRUE)

hst = OPC_Data_Full %>%
  filter(flag == 0) %>%
  filter(OPC_cast_num %in% good_casts) %>%
  drop_na(ZBin) %>%
  group_by(OPC_cast_num, basin, ZBin) %>%
  reframe(
    opc_histogram(across(), ds = 0.2, min_size = 0.3, max_size = 3, good_only = T))

# average by station
st_hst = hst %>%
  group_by(OPC_cast_num, size, basin, ZBin) %>%
  summarise(
    concentration = mean(concentration, na.rm = T),
    .groups = 'drop')

# average by basin
ba_hst = st_hst %>%
  group_by(size, basin, ZBin) %>%
  summarise(
    m = mean(concentration, na.rm = T),
    upper = m + sd(concentration, na.rm = T),
    lower = m - sd(concentration, na.rm = T),
    .groups = 'drop'
  )

# labels

labs = tibble(
  ZBin = factor(levels(ba_hst$ZBin), ordered = T),
  label = c('0-40m', '41-80m', '81-120m', '121-160m', '161-200m')
)

# Histogram plot

plot3 = ggplot()+
  geom_ribbon(data = ba_hst, aes(ymin = lower, ymax = upper, x = size, group = basin, fill = basin),
              alpha = 0.3) +
  geom_path(data = ba_hst, aes(y = m, x = size, color = basin, group = basin)) +
  geom_text(data = labs, aes(x = 2.35, y = 75, label = label)) +
  scale_fill_manual(values = c('OB' = 'red', 'GMB' = 'darkslategrey')) +
  scale_color_manual(values = c('OB' = 'red', 'GMB' = 'darkslategrey')) +
  scale_y_continuous(position = 'right', breaks = seq(0,500,50)) +
  labs(x = 'Equivalent spherical diameter (mm)', 
       y = expression(paste("Abundance (particles ", m^-3,')')),
       fill = NULL, color = NULL) +
  coord_cartesian(ylim = c(0,500), expand = F)+
  facet_wrap(~ZBin, ncol = 1) +
  theme_bw() +
  theme(legend.position = 'none', 
        panel.grid = element_blank(),
        strip.background = element_blank(), 
        strip.text = element_blank(),
        text = element_text(size = 16))

# Full plot

ggarrange(plot1, plot2, plot3, nrow = 1, common.legend = T, legend = "bottom", labels = "AUTO") + theme(text = element_text(size = 16))

ggsave(filename = "OPC_Basin_Depth_Averaged_Abundance.png", path = figure_dir)


plot = OPC_Data_Full %>% filter(basin == "GMB") %>% filter(depth > 170) %>% 
  filter(flag != 'depth') %>%
  opc_plot_histogram(ds = 0.05, min_size = 0.5, max_size = 3)
plot + labs(title = "Grand Manan Basin OPC Histogram") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "GMB_OPC_Histogram.png", path = figure_dir, width = 12, height = 6, units = "in")


