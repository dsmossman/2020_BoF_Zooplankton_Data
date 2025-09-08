## opc_check_bof ##
# quick review of fundy OPC data

devtools::install_github('hansenjohnson/opcr')
library(opcr)

# process data
df = opc_process_cruise(cruise = 'bof', data_dir = '~/Desktop/D00/')

# plot downcasts
df %>%
  filter(flag != 'depth') %>%
  ggplot(aes(x=scan, y=depth, group=cast))+
  geom_path(alpha=0.7)+
  scale_y_reverse()+
  theme_bw()

# plot average speeds
df %>%
  group_by(cast) %>%
  summarize(speed = opc_speed(across())) %>%
  ggplot(aes(x=speed))+
  geom_histogram(fill='grey', color='black', binwidth = 0.025)+
  theme_bw()

# plot attenuance
df %>%
  filter(flag != 'depth' & atten < 3000) %>%
  ggplot(aes(x=atten, y=depth, group=cast))+
  geom_path(alpha=0.7)+
  scale_y_reverse()+
  theme_bw()
