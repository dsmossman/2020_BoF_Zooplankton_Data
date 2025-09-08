## f_opc_profile ##
# plot opc profile and size at depth and in temperature-salinity space

# input -------------------------------------------------------------------

# OPC configuration
dz = 5
min_size = 0.8
max_size = 3

# setup -------------------------------------------------------------------

library(tidyverse)
library(opcr)
library(oce)
library(metR)

# read --------------------------------------------------------------------

# read in station variables
sv = readRDS('data/processed/station_variables.rds') %>%
  filter(whales != 'unknown' & use_opc != 0) %>%
  select(id, whales)

# select station IDs
ids = sv$id

# read in opc data
op = readRDS('data/processed/opc.rds')

# add whales to opc data
op = left_join(op, sv, "id") %>%
  filter(id %in% ids)

# check sample size
op %>% select(id, whales) %>% distinct()

# read in ctd data
ct = readRDS('data/processed/ctd.rds') %>%
  mutate(
    cast_id = paste0(cruise, "_", sample_id),
    depth = swDepth(pressure)
  )

# add whales to ctd data and restrict to opc stations
ct = left_join(ct, sv, "id") %>%
  filter(id %in% unique(op$id) & whales != 'unknown')

# ctd profile -------------------------------------------------------------

# compute binned CTD profiles and average by station
ctd_avg = ct %>%
  mutate(
    zbin = bin(depth, dz)    
  ) %>%
  group_by(id, zbin, whales) %>%
  summarize(
    temperature = mean(temperature, na.rm = T),
    salinity = mean(salinity, na.rm = T),
    density = mean(density, na.rm = T),
    .groups = 'drop'
  ) %>%
  rename(depth = zbin) %>%
  mutate(depth = relabel(depth)) %>%
  group_by(depth, whales) %>%
  summarise(
    m_tmp = median(temperature, na.rm = T),
    upper_tmp = quantile(temperature, probs = 0.75, na.rm = T),
    lower_tmp = quantile(temperature, probs = 0.25, na.rm = T),
    m_sal = median(salinity, na.rm = T),
    upper_sal = quantile(salinity, probs = 0.75, na.rm = T),
    lower_sal = quantile(salinity, probs = 0.25, na.rm = T),
    .groups = 'drop'
  ) %>% filter(depth <= 100)

# opc profile -------------------------------------------------------------

# calculate abundance profiles
prf = op %>%
  filter(depth>1) %>%
  group_by(id, cast_id, whales) %>%
  summarise(
    opc_abundance(across(), dz = dz, min_size = min_size, max_size = max_size),
    .groups = 'drop'
  )

# average cast by station
st_prf = prf %>%
  group_by(id, depth, whales) %>%
  summarise(
    concentration = mean(concentration, na.rm = T),
    .groups = 'drop')

# average station by whale presence
wh_prf = st_prf %>%
  group_by(depth, whales) %>%
  summarise(
    m = median(concentration, na.rm = T),
    upper = quantile(concentration, probs = 0.75, na.rm = T),
    lower = quantile(concentration, probs = 0.25, na.rm = T),
    .groups = 'drop'
  ) %>% filter(depth <= 100)

# plot profile
p_prf = ggplot() +
  geom_hline(yintercept = c(0,20,40,60,80), color = 'grey', linetype = 2)+
  geom_ribbon(data = wh_prf, aes(xmin = lower, xmax = upper, y = depth, group = whales, fill = whales),
              alpha = 0.3) +
  geom_path(data = wh_prf, aes(x = m, y = depth, color = whales, group = whales)) +
  scale_fill_manual(values = c('present' = 'red', 'absent' = 'darkslategrey')) +
  scale_color_manual(values = c('present' = 'red', 'absent' = 'darkslategrey')) +
  scale_y_reverse(breaks = c(0,20,40,60,80,100)) +
  coord_cartesian(expand = FALSE) +
  labs(x = expression(paste("Abundance (particles ", m^-3,')')),
       y = 'Depth (m)', 
       fill = NULL, color = NULL)+
  theme_bw() +
  theme(legend.position = 'none',
        panel.grid = element_blank())

# opc biomass profile -----------------------------------------------------

# calculate abundance profiles
prf_b = op %>%
  filter(depth>1) %>%
  group_by(id, cast_id, whales) %>%
  summarise(
    opc_biomass(across(), dz = dz, min_size = min_size, max_size = max_size),
    .groups = 'drop'
  )

# average cast by station
st_prf_b = prf_b %>%
  group_by(id, depth, whales) %>%
  summarise(
    concentration = mean(concentration, na.rm = T)/1e3,
    .groups = 'drop')

# average station by whale presence
wh_prf_b = st_prf_b %>%
  group_by(depth, whales) %>%
  summarise(
    m = median(concentration, na.rm = T),
    upper = quantile(concentration, probs = 0.75, na.rm = T),
    lower = quantile(concentration, probs = 0.25, na.rm = T),
    .groups = 'drop'
  ) %>% filter(depth <= 100)

# plot profile
p_prf_b = ggplot() +
  geom_hline(yintercept = c(0,20,40,60,80), color = 'grey', linetype = 2)+
  geom_ribbon(data = wh_prf_b, aes(xmin = lower, xmax = upper, y = depth, group = whales, fill = whales),
              alpha = 0.3) +
  geom_path(data = wh_prf_b, aes(x = m, y = depth, color = whales, group = whales)) +
  scale_fill_manual(values = c('present' = 'red', 'absent' = 'darkslategrey')) +
  scale_color_manual(values = c('present' = 'red', 'absent' = 'darkslategrey')) +
  scale_y_reverse(breaks = c(0,20,40,60,80,100)) +
  coord_cartesian(expand = FALSE) +
  labs(x = expression(paste("Biomass (g ", m^-3,')')),
       y = 'Depth (m)', 
       fill = NULL, color = NULL)+
  theme_bw() +
  theme(
    legend.position = c(0.99,0.75), 
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(),
    legend.justification = c(1,1),
    panel.grid = element_blank())

# histogram ---------------------------------------------------------------

# define depth strata for size histograms
op$zbin = cut(op$depth, breaks = c(0,20,40,60,80,100), include.lowest = TRUE)

# compute histograms
hst = op %>%
  drop_na(zbin) %>%
  group_by(id, cast_id, whales, zbin) %>%
  summarise(
    opc_histogram(across(), ds = 0.2, min_size = min_size, max_size = max_size),
    .groups = 'drop'
  )

# compute average profile at each station
st_hst = hst %>%
  group_by(id, size, whales, zbin) %>%
  summarise(
    concentration = mean(concentration, na.rm = T),
    .groups = 'drop')

# average
wh_hst = st_hst %>%
  group_by(size, whales, zbin) %>%
  summarise(
    m = median(concentration, na.rm = T),
    upper = quantile(concentration, probs = 0.75, na.rm = T),
    lower = quantile(concentration, probs = 0.25, na.rm = T),
    .groups = 'drop'
  )

# define labels
labs = tibble(
  zbin = factor(levels(wh_hst$zbin), ordered = T),
  label = c('0-20m', '21-40m', '41-60m', '61-80m', '81-100m')
)

# plot
p_hst = ggplot()+
  geom_ribbon(data = wh_hst, aes(ymin = lower, ymax = upper, x = size, group = whales, fill = whales),
              alpha = 0.3) +
  geom_path(data = wh_hst, aes(y = m, x = size, color = whales, group = whales)) +
  geom_text(data = labs, aes(x = 2.35, y = 75, label = label)) +
  scale_fill_manual(values = c('present' = 'red', 'absent' = 'darkslategrey')) +
  scale_color_manual(values = c('present' = 'red', 'absent' = 'darkslategrey')) +
  scale_y_continuous(position = 'right', breaks = seq(0,80,20)) +
  labs(x = 'Equivalent spherical diameter (mm)', 
       y = expression(paste("Abundance (particles ", m^-3,')')),
       fill = NULL, color = NULL) +
  coord_cartesian(ylim = c(0,100), expand = F)+
  facet_wrap(~zbin, ncol = 1) +
  theme_bw() +
  theme(legend.position = 'none', 
        panel.grid = element_blank(),
        strip.background = element_blank(), 
        strip.text = element_blank())

# combine -----------------------------------------------------------------

# combine
p = wrap_plots(p_prf, p_prf_b, p_hst, nrow = 1) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(text = element_text(size = 16))

# save
ggsave(
  filename = paste0('figures/opc_profile.png'),
  plot = p,
  width = 10,
  height = 7,
  units = 'in',
  dpi = 300
)

# sample size -------------------------------------------------------------

n_whale1 = op %>% filter(whales == 'present') %>% distinct(id) %>% nrow()
n_whale0 = op %>% filter(whales == 'absent') %>% distinct(id) %>% nrow()

message('Stations with whales present: ', n_whale1)
message('Stations with whales absent: ', n_whale0)

# ts ----------------------------------------------------------------------

# format ts data
opc_ts = wh_prf_b %>% transmute(depth, whales, concentration = m)
ctd_ts = ctd_avg %>% rename(temperature = m_tmp, salinity = m_sal)
ts_df = full_join(opc_ts, ctd_ts)

# compute ts contours
ts_contours = expand_grid(salinity = seq(from = min(ts_df$lower_sal,na.rm = T),to = max(ts_df$upper_sal,na.rm = T),length.out = 100),
                          temperature = seq(from = min(ts_df$lower_tmp,na.rm = T),to = max(ts_df$upper_tmp,na.rm = T),length.out = 100))
ts_contours$density = oce::swSigmaTheta(salinity = ts_contours$salinity, temperature = ts_contours$temperature,pressure = 0)

# subset to deep water
ts_dfs = ts_df %>% filter(depth>40)

# find limits
min_sal = min(ts_dfs$lower_sal, na.rm = T)
max_sal = max(ts_dfs$upper_sal, na.rm = T)
min_tmp = min(ts_dfs$lower_tmp, na.rm = T)
max_tmp = max(ts_dfs$upper_tmp, na.rm = T)

# plot full ts
ts1 = ggplot()+
  geom_contour2(data=ts_contours,aes(x=salinity,y=temperature,z=density), color = 'grey', alpha = 0.7)+
  geom_text_contour(data=ts_contours,aes(x=salinity,y=temperature,z=density), 
                    rotate = F, skip = 0, color = 'grey', stroke = 0.2) +
  geom_linerange(data = ts_df, aes(x = salinity, ymin = lower_tmp, ymax = upper_tmp, group = whales, color = whales)) +
  geom_linerange(data = ts_df, aes(y = temperature, xmin = lower_sal, xmax = upper_sal, group = whales, color = whales)) +
  geom_point(data=ts_df, aes(x=salinity,y = temperature, fill=whales, size = concentration), shape = 21, alpha = 0.7) +
  annotate("rect", xmin = min_sal, xmax = max_sal, ymin = min_tmp, ymax = max_tmp, 
           fill = NA, color = 'black', alpha = 0.7, linetype = 2)+
  scale_fill_manual(values = c('present' = 'red', 'absent' = 'darkslategrey')) +
  scale_color_manual(values = c('present' = 'red', 'absent' = 'darkslategrey')) +
  labs(x = NULL, y = "Temperature (°C)", fill = 'Whales', color = 'Whales', subtitle = '0-100 m',
       size = expression(atop("Biomass", paste("(g ", m^-3,')')))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.box.just = 'top',
        legend.title.align = 0.5,
        legend.position = 'right')

# compute deep ts contours
ts_contours2 = expand_grid(salinity = seq(from = min_sal, to = max_sal, length.out = 100),
                           temperature = seq(from = min_tmp, to = max_tmp,length.out = 100))
ts_contours2$density = oce::swSigmaTheta(salinity = ts_contours2$salinity, 
                                         temperature = ts_contours2$temperature, 
                                         pressure = 0)

# plot deep ts
ts2 = ggplot()+
  geom_contour2(data=ts_contours2,aes(x=salinity,y=temperature,z=density), color = 'grey', alpha = 0.7)+
  geom_text_contour(data=ts_contours2,aes(x=salinity,y=temperature,z=density, label = round(..level.., 2)), 
                    rotate = F, stroke = 0.2, skip = 1, color = 'grey', 
                    label.placement = label_placement_fraction(1)) +
  geom_linerange(data = ts_dfs, aes(x = salinity, ymin = lower_tmp, ymax = upper_tmp, group = whales, color = whales)) +
  geom_linerange(data = ts_dfs, aes(y = temperature, xmin = lower_sal, xmax = upper_sal, group = whales, color = whales)) +
  geom_point(data=ts_dfs, aes(x=salinity,y = temperature, fill=whales, size = concentration), shape = 21, alpha = 0.7) +
  scale_fill_manual(values = c('present' = 'red', 'absent' = 'darkslategrey')) +
  scale_color_manual(values = c('present' = 'red', 'absent' = 'darkslategrey')) +
  labs(x = 'Salinity', y = "Temperature (°C)", fill = 'Whales', color = 'Whales', subtitle = '>40 m')+
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = 'none')

# combine
p_ts = wrap_plots(ts1, ts2, nrow = 2, heights = c(3,2)) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') +
  plot_layout(guides = 'collect')

# save
ggsave(
  filename = paste0('figures/opc_ts.png'),
  plot = p_ts,
  width = 8,
  height = 8,
  units = 'in',
  dpi = 300
)
