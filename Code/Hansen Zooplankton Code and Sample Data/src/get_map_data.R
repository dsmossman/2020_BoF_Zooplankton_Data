## get_map_data ##

# Read and save bathymetry data from NOAA server (etopo1), and define Roseway Basin ATBA, for future plotting

get_map_data = function(cache_dir='cache'){
  
  # map file
  map_file = paste0(cache_dir, '/map.rda')
  
  # Grab data from NOAA server ----------------------------------------------
  
  if (!file.exists(map_file)) { # only grab data if we don't have it
    
    # useful libraries
    library(oce)
    library(marmap)
    data(coastlineWorldFine, package="ocedata")
    
    # bathymetry data
    b <- getNOAA.bathy(-70,-55,40,55,1) 
    bathyLon <- as.numeric(rownames(b))
    bathyLat <- as.numeric(colnames(b))
    bathyZ <- as.numeric(b)
    dim(bathyZ) <- dim(b)
    bathyZ[bathyZ > 0] <- NA # to blank out the land
    
    # save all data
    save(bathyLon, bathyLat, bathyZ, b,
         coastlineWorldFine,
         file = map_file)
    
    message('Map data saved at: ', map_file)
    
  } else {
    message('Using map data from: \n', map_file)
  }
}