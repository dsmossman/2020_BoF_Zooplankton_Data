library(tidyverse)
library(oce)
library(ocedata)
library(R.utils)
library(gtools)
library(opcr)
library(ggpubr)


## Project Structure
setwd("C:/Users/dmossman/Box/2022 MSc Thesis Work")

sourceDirectory('Code/Hansen Zooplankton Code and Sample Data/src',
                modifiedOnly = F)

function_dir = 'Code/Hansen Zooplankton Code and Sample Data/src/'
processed_dir = 'Processed_Data/'
data_dir = 'Raw_Data/'
figure_dir = 'Visuals/'
report_dir = getwd()

setwd("C:/Users/dmossman/Box/2022 MSc Thesis Work/Raw_Data/Cage/OPC/OPC_Data/d00")
## opc_process_cruise("BoF_2020", data_dir = getwd(), output_dir = "/Users/delphine/Documents/BoF2020_Cruise/Processed_Data/Cage/")

processed_file_path = "C:/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/Cage/"
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
ggsave(paste0("OPC028", "_Plot_Abundance_Only.png"), path = "/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/Graphs",
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
ggsave(paste0("OPC003", "_Plot.png"), device = "png", path = "/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/Graphs",
       width = 6, height = 10, units="in")


for(y in 2:length(OPC_OB_Data)){
  test = try(opc_plot_multipanel(OPC_OB_Data[[y]]), silent = T)
  if(class(test)[1] == "try-error") {
    plot = opc_plot_multipanel(OPC_OB_Data[[y]], good_only = F, amin_size = 1.5, dz=5)
    plot = plot + plot_annotation(title = names(OPC_OB_Data)[y], theme = theme(plot.title = element_text(hjust = 0.5)))
    ggsave(paste0(names(OPC_OB_Data)[y], "_Plot.png"), device = "png", path = "/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/Graphs",
           width = 6, height = 10, units="in")
  }
  else {
    plot = opc_plot_multipanel(OPC_OB_Data[[y]], amin_size = 1.5, dz=5)
    plot = plot + plot_annotation(title = names(OPC_OB_Data)[y], theme = theme(plot.title = element_text(hjust = 0.5)))
    ggsave(paste0(names(OPC_OB_Data)[y], "_Plot.png"), device = "png", path = "/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/Graphs",
           width = 6, height = 10, units="in")
  }
}


for(y in 1:length(OPC_GMB_Data)){
  test = try(opc_plot_multipanel(OPC_GMB_Data[[y]],  amin_size = 1.5), silent = T)
  if(class(test)[1] == "try-error") {
    plot = opc_plot_multipanel(OPC_GMB_Data[[y]], good_only = F, amin_size = 1.5, dz=5)
    plot = plot + plot_annotation(title = names(OPC_GMB_Data)[y], theme = theme(plot.title = element_text(hjust = 0.5)))
    ggsave(paste0(names(OPC_GMB_Data)[y], "_Plot.png"), device = "png", path = "/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/Graphs",
           width = 6, height = 10, units="in")
  }
  else {
    plot = opc_plot_multipanel(OPC_GMB_Data[[y]], amin_size = 1.5, dz=5)
    plot = plot + plot_annotation(title = names(OPC_GMB_Data)[y], theme = theme(plot.title = element_text(hjust = 0.5)))
    ggsave(paste0(names(OPC_GMB_Data)[y], "_Plot.png"), device = "png", path = "/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/Graphs",
           width = 6, height = 10, units="in")
  }
}


plot1 = opc_plot_image(OPC_OB_Data[[5]])
plot1 = plot1 + plot_annotation(title = names(OPC_OB_Data)[5], theme = theme(plot.title = element_text(hjust = 0.5)))
plot2 = opc_plot_image(OPC_OB_Data[[6]])
plot2 = plot2 + plot_annotation(title = names(OPC_OB_Data)[6], theme = theme(plot.title = element_text(hjust = 0.5)))

library(ggpubr)
plot3 = ggarrange(plot1, plot2, ncol = 2, common.legend = T, legend = "bottom")

setwd("C:/Users/dmossman/Box/2022 MSc Thesis Work/")
OPC_Data_Full = rbind(do.call("rbind", OPC_OB_Data), do.call("rbind", OPC_GMB_Data))
OPC_Speeds = OPC_Data_Full %>% group_by(cast) %>% summarise(opc_speed(across()))
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

save(OPC_Data_Full, file = paste0(processed_dir, 'OPC_Data_Full.rda'))


load(paste0(processed_dir, 'OPC_Data_Full.rda'))

dz = 1
min_size = 1.5
max_size = 2.5

# calculate abundance profiles
prf = OPC_Data_Full %>%
  filter(depth>1) %>%
  filter(flag != 'depth') %>%
  group_by(OPC_cast_num, basin) %>%
  reframe(
    opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = F, reject_volume = F),
  )

save(prf, file = paste0(processed_dir, 'OPC_Data_Abundance_Estimate.rda'))

# biomass estimate from Hansen Johnson's work

prf_b = OPC_Data_Full %>%
  filter(depth>1) %>%
  filter(flag != 'depth') %>%
  group_by(OPC_cast_num, basin) %>%
  reframe(
    opc_biomass(across(), dz = dz, min_size = min_size, max_size = max_size, good_only = F,
                reject_volume = F))

save(prf_b, file = paste0(processed_dir, 'OPC_Data_Biomass_Estimate.rda'))

# average cast by station
st_prf = prf %>%
  group_by(OPC_cast_num, depth, basin) %>%
  summarise(
    concentration = mean(concentration, na.rm = T),.groups = 'drop')

st_prf_b = prf_b %>%
  group_by(OPC_cast_num, depth, basin) %>%
  summarise(
    concentration = mean(concentration, na.rm = T)/1e3,.groups = 'drop')

# average station by basin
ba_prf = st_prf %>%
  group_by(depth, basin) %>%
  summarise(
    m = median(concentration, na.rm = T),
    upper = quantile(concentration, probs = 0.75, na.rm = T),
    lower = quantile(concentration, probs = 0.25, na.rm = T),
    .groups = 'drop'
  )

ba_prf_b = st_prf_b %>%
  group_by(depth, basin) %>%
  summarise(
    m = median(concentration, na.rm = T),
    upper = quantile(concentration, probs = 0.75, na.rm = T),
    lower = quantile(concentration, probs = 0.25, na.rm = T),
    .groups = 'drop'
  )

# Plots
plot1 = ggplot() +
  geom_ribbon(data = ba_prf, aes(xmin = lower, xmax = upper, y = depth, group = basin, fill = basin),
              alpha = 0.3) +
  geom_path(data = ba_prf, aes(x = m, y = depth, color = basin, group = basin)) +
  scale_fill_manual(values = c('OB' = 'red', 'GMB' = 'darkslategrey')) +
  scale_color_manual(values = c('OB' = 'red', 'GMB' = 'darkslategrey')) +
  scale_y_reverse(breaks = c(0,20,40,60,80,100,120,140,160,180,200)) +
  coord_cartesian(expand = FALSE) +
  labs(x = expression(paste("Particle Abundance (particles 0.8 to 3 mm ESD/", m^3,')')),
       y = 'Depth (m)', 
       fill = NULL, color = NULL)+
  theme_bw() + theme(text = element_text(size = 16))

plot2 = ggplot() +
  geom_ribbon(data = ba_prf_b, aes(xmin = lower, xmax = upper, y = depth, group = basin, fill = basin),
              alpha = 0.3) +
  geom_path(data = ba_prf_b, aes(x = m, y = depth, color = basin, group = basin)) +
  scale_fill_manual(values = c('OB' = 'red', 'GMB' = 'darkslategrey')) +
  scale_color_manual(values = c('OB' = 'red', 'GMB' = 'darkslategrey')) +
  scale_y_reverse(breaks = c(0,20,40,60,80,100,120,140,160,180,200)) +
  coord_cartesian(expand = FALSE) +
  labs(x = "Estimated Particle Biomass (mg)",
       y = 'Depth (m)', 
       fill = NULL, color = NULL)+
  theme_bw() + theme(text = element_text(size = 16))

# Ribbon plots of particle abundance by size class and depth bin

OPC_Data_Full$ZBin = cut(OPC_Data_Full$depth, breaks = c(0,40,80,120,160,200), include.lowest = TRUE)

hst = OPC_Data_Full %>%
  drop_na(ZBin) %>%
  group_by(OPC_cast_num, basin, ZBin) %>%
  reframe(
    opc_histogram(across(), ds = 0.2, min_size = min_size, max_size = max_size, good_only = F))

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
    m = median(concentration, na.rm = T),
    upper = quantile(concentration, probs = 0.75, na.rm = T),
    lower = quantile(concentration, probs = 0.25, na.rm = T),
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
  scale_y_continuous(position = 'right', breaks = seq(0,200,40)) +
  labs(x = 'Equivalent spherical diameter (mm)', 
       y = expression(paste("Abundance (particles ", m^-3,')')),
       fill = NULL, color = NULL) +
  coord_cartesian(ylim = c(0,200), expand = F)+
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


plot = OPC_Data_Full %>% filter(basin == "GMB") %>% filter(depth > 150) %>% 
  filter(flag != 'depth') %>%
  opc_plot_histogram(ds = 0.05, min_size = 1, max_size = 3)
plot + labs(title = "Grand Manan Basin OPC Histogram") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "GMB_OPC_Histogram.png", path = figure_dir, width = 12, height = 6, units = "in")


