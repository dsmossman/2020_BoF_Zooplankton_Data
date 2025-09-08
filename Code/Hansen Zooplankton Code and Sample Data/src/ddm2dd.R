# convert degrees decimal minutes to decimal degrees
ddm2dd = function(ddm){

  ddm = strsplit(ddm, split = ' ')
  
  out = rep(NA, length(ddm))
  
  for(i in 1:length(ddm)){
    
    deg = as.numeric(ddm[[i]][1])
    dec = as.numeric(ddm[[i]][2])/60
    
    if(deg<1){
      dd = deg-dec
    } else {
      dd = deg+dec
    }
    
    out[i] = dd
  }
  return(out)
}

# # Testing
# ddm_lon = '67 07.321'
# ddm2dd(ddm_lon)
# ddm = c('40 36.449', '41 32.442', '47 31.329')
# ddm2dd(ddm)