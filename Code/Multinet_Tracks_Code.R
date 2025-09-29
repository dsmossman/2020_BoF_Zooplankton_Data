# Getting necessary libraries/file
multinet_tracks = read.csv("multinet_tracks.csv")
library(ggplot2)
library(dplyr)
library(marmap)
library(mapdata)

#Getting bathymetry for BoF area
bathy = fortify.bathy(getNOAA.bathy(-70,-60,42,46))
reg = map_data("world2Hires")
reg = subset(reg, region %in% c('Canada', 'USA'))
reg$long = (360 - reg$long)*-1

# Setting relevant lats/lons for study area
lons = c(-67, -66)
lats = c(44.5, 45)

# Building initial plot with coastlines and bathymetry contours
test_plot2 = ggplot() + theme_bw() + ylab("") + xlab("") + coord_map(xlim = lons, ylim = lats) +
  geom_polygon(
    data = reg,
    aes(x = long, y = lat, group = group),
    fill = "darkgrey",
    color = NA
  )
for (b in seq(-25, -150, -25)) {
  test_plot2 = test_plot2 + geom_contour(
    data = bathy,
    aes(x = x, y = y, z = z),
    breaks = c(b),
    size = c(0.3),
    colour = "grey"
  )
}

# Plotting multinet tracks by date
test_plot2 = test_plot2 + facet_wrap( ~ multi_dates) + geom_path(
  data = multi_tracks,
  aes(x = multi_lons, y = multi_lats),
# this is where I think I need an additional specification for making sure each path is separate, and not connected to the next one in the sequence
  colour = "black",
  alpha = 1,
  size = 0.3
)

# Plot output
print(test_plot2)