processAndSave2 <- function(folderpath, modelname, doKriging = TRUE){
  
  require(HGSReader)
  source("C:/Users/t24x137/Desktop/Floodplain_Shade/r_codes/functions/calcDailyMean.R")
  require(sp)
  require(raster)
  require(rgeos)
  require(tidyr)
  require(stringr)
  require(gstat)
  
  memory.limit(size = 56000)
  
  # foldername <- soil
  # folderlocation <- bc
  # 
  # folderpath <- paste0(folder,
  #                      "/K_", 
  #                      K,
  #                      "m_day/", 
  #                      folderlocation, 
  #                      "/", 
  #                      soil, 
  #                      "/")
  
  f <- HGSFile(paste0(folderpath,
                      "temperatureTestero.pm.dat"))
  sat <- HGSGetData(f, variables = c("X", "Y", "Z", "Sat"), blockNumbers = 1)
  
  allnodecentered <- HGSGetData(f, variables = c("X", "Y", "Z", "Sat", "Depth2GWT", "Head"), blockNumbers = 1)
  
  g <- HGSGetData(f, variables = c("X", "Y", "Z", "temp"))
  
  gmeans <- calcDailyMean(g)
  modelstructure <- g[,,,1,c("X","Y","Z")]
  velocity <- HGSGetData(f, variables = c("Vx", "Vy", "Vz"), blockNumbers = c(1))
  
  ## Interpolate Raster ##
  if(doKriging == TRUE){
  allblank <- raster(ncols = 8000, 
                     nrows = (tail(modelstructure[1,2,,"Z"],1)-30) / 0.1, 
                     xmn = 0, 
                     xmx = 800, 
                     ymn = 30, 
                     ymx = tail(modelstructure[1,2,,"Z"], 1))
  timeslist <- list(24)
  
  alluvium_5mbedrock_polygon <- readWKT(paste0("POLYGON((0 30, 800 30, 800 ", tail(modelstructure[1,2,,"Z"], 1), ", 0 ", tail(modelstructure[1,2,,"Z"], 1),", 0 30))")) 
  
  for(i in 1:24){
    thisdf <- as.data.frame(gmeans[,,i])
    thisdf <- gather(thisdf, xidx, temp, V1:V496)
    thisdf$z <- modelstructure[1,2,,"Z"][seq(length(modelstructure[1,2,,"Z"]),1,by=-1)]
    thisdf$xidx <- as.numeric((str_extract(thisdf$xidx, "[0-9]+")))
    thisdf$x <- modelstructure[thisdf$xidx, 2, 1, "X"]
    thisdf <- thisdf[,-c(1)]
    sp::coordinates(thisdf) <- c("x", "z")
    thisdf <- crop(thisdf, alluvium_5mbedrock_polygon)
    nnthisdf <- gstat(formula = temp~1, locations = thisdf, nmax = 5, set=list(idp=0))
    interpthisdf <- interpolate(allblank, nnthisdf)
    
    timeslist[[i]] <- interpthisdf
  }
  saveRDS(timeslist, 
          file = paste0(folderpath, modelname, "_rasterlist.RData"))
  }
  
  saveRDS(velocity, 
          file = paste0(folderpath, modelname, "_velocity.RData"))
  saveRDS(gmeans, 
          file = paste0(folderpath, modelname, "_dailymeans.RData"))
  saveRDS(sat, 
          file = paste0(folderpath, modelname, "_saturation_T1.RData"))
  saveRDS(modelstructure, 
          file = paste0(folderpath, modelname, "_modelstructure.RData"))
  saveRDS(allnodecentered, 
          file = paste0(folderpath, modelname, "_all.RData"))
}

