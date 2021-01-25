retrieveDailySignal <- function(soil, 
                                bc, 
                                K, 
                                folder = "C:/Users/t24x137/Box/Floodplain_Shade_Box"){
  require(HGSReader)
  # require(sp)
  # require(raster)
  # require(rgeos)
  #require(tidyr)
  #require(stringr)
  # require(gstat)
  
  memory.limit(size = 56000)
  
  foldername <- soil
  folderlocation <- bc
  
  folderpath <- paste0(folder,
                       "/K_", 
                       K,
                       "m_day/", 
                       folderlocation, 
                       "/", 
                       soil, 
                       "/")
  
  f <- HGSFile(paste0(folderpath,
                      "temperatureTestero.pm.dat"))
  g <- HGSGetData(f, variables = c("X", "Y", "Z", "temp"))
  d <- g[1:50,,,,]
  return(d)
}
