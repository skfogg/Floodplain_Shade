##
## Process and Save Meacham results
##

# source("~/Floodplain_Shade/r_codes/functions/processAndSave3.R")

box_directory <- "C:/Users/t24x137/Box/Floodplain_Shade_Box/meacham_results/"

kvals <- rep(c("100", "400"), each = 8)
bcvals <- rep(rep(c("sunny", "shady"), each = 4), times = 2)
soilvals <- rep(c("0.5", "1.0", "2.0", "3.0"), times = 4)

eachfolder <- numeric(16)
for (i in 1:16){
  eachfolder[i] <- paste0(box_directory, "K_", kvals[i], "m_day/", bcvals[i], "/", "soil_", soilvals[i], "m")
}

eachmodel <- numeric(16)
for (i in 1:16){
  eachmodel[i] <- paste0("k", kvals[i], "_", soilvals[i], "_", bcvals[i])
}


# mapply(processAndSave3,
#        folderpath = eachfolder,
#        modelname = eachmodel)
library(HGSReader)
source("C:/Users/t24x137/Desktop/Floodplain_Shade/r_codes/functions/calcDailyMean.R")
# library(sp)
# library(raster)
# library(rgeos)
library(tidyr)
library(stringr)
library(gstat)


## inside process and save:
for(i in 1:16){
  
  folderpath <- eachfolder[i]
  modelname <- eachmodel[i]
  
  memory.limit(size = 56000)
  memory.size(max = TRUE)
  
  f <- HGSFile(paste0(folderpath, 
                      "/", 
                      modelname,
                      "o.pm.dat"))
  
  allnodecentered <- HGSGetData(f, variables = c("X", "Y", "Z", "Sat", "Depth2GWT", "Head"), blockNumbers = 1)
  saveRDS(allnodecentered, 
          file = paste0(folderpath, "/", modelname, "_all.RData"))
  rm(allnodecentered)
  
  sat <- HGSGetData(f, variables = c("X", "Y", "Z", "Sat"), blockNumbers = 1)
  saveRDS(sat, 
          file = paste0(folderpath, "/", modelname, "_saturation.RData"))
  rm(sat)
  
  g <- HGSGetData(f, variables = c("X", "Y", "Z", "temp"))
  first100m <- g[1:95,,,,]
  saveRDS(first100m,
          file(paste0(folderpath, "/", modelname, "_first100m.RData")))
  
  # g2 <- HGSGetData(f, variables = c("X","Z", "temp"))
  gmeans <- calcDailyMean(g)
  saveRDS(gmeans,
          file = paste0(folderpath, "/", modelname, "_dailymeans.RData"))
  rm(gmeans)
  
  modelstructure <- g[,,,1,c("X","Y","Z")]
  saveRDS(modelstructure, 
          file = paste0(folderpath, "/", modelname, "_modelstructure.RData"))
  rm(modelstructure)
  rm(g)
  
  velocity <- HGSGetData(f, variables = c("Vx", "Vy", "Vz"), blockNumbers = c(1))
  saveRDS(velocity, 
          file = paste0(folderpath, "/", modelname, "_velocity.RData"))
  rm(velocity)
  rm(f)
}


