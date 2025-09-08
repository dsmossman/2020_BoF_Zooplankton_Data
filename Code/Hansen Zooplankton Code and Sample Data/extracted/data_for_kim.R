dat = subset(log, log$type=='FULL')

sv = data.frame(date = dat$date, time = dat$time, station = dat$station, lat = dat$lat, lon = dat$lon, whales = dat$whales)

write.csv(sv, file = 'gosl_plankton_stations.csv', row.names = F)
