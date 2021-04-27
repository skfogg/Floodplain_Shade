##
## Process and Save Isotropic results
##

source("~/Floodplain_Shade/r_codes/functions/processAndSave3.R")

box_directory <- "C:/Users/skati/Box/Floodplain_Shade_Box/"

kvals <- rep(c("100", "400"), each = 12)
bcvals <- rep(rep(c("riveronly", "sunny", "shady"), each = 4), times = 2)
soilvals <- rep(c("0.5", "1.0", "2.0", "3.0"), times = 12)

eachfolder <- numeric(24)
for (i in 1:24){
eachfolder[i] <- paste0(box_directory, "K_", kvals[i], "m_day/", bcvals[i], "/", "soil_", soilvals[i], "m")
}

eachmodel <- numeric(24)
for (i in 1:24){
eachmodel[i] <- paste0("k", kvals[i], "_", soilvals[i], "_", bcvals[i])
}


# mapply(processAndSave3,
#        folderpath = eachfolder,
#        modelname = eachmodel)
 library(HGSReader)
 source("C:/Users/skati/Documents/Floodplain_Shade/r_codes/functions/calcDailyMean.R")
 # library(sp)
 # library(raster)
 # library(rgeos)
 library(tidyr)
 library(stringr)
 library(gstat)


## inside process and save:
for(i in 4:24){
        
        i = 11
        
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
}



## Interpolate Raster ##
# if(doKriging == TRUE){
#   allblank <- raster(ncols = 8000, 
#                      nrows = (tail(modelstructure[1,2,,"Z"],1)-30) / 0.1, 
#                      xmn = 0, 
#                      xmx = 800, 
#                      ymn = 30, 
#                      ymx = tail(modelstructure[1,2,,"Z"], 1))
#   timeslist <- list(24)
#   
#   alluvium_5mbedrock_polygon <- readWKT(paste0("POLYGON((0 30, 800 30, 800 ", tail(modelstructure[1,2,,"Z"], 1), ", 0 ", tail(modelstructure[1,2,,"Z"], 1),", 0 30))")) 
#   
#   for(i in 1:24){
#     thisdf <- as.data.frame(gmeans[,,i])
#     thisdf <- gather(thisdf, xidx, temp, V1:V496)
#     thisdf$z <- modelstructure[1,2,,"Z"][seq(length(modelstructure[1,2,,"Z"]),1,by=-1)]
#     thisdf$xidx <- as.numeric((str_extract(thisdf$xidx, "[0-9]+")))
#     thisdf$x <- modelstructure[thisdf$xidx, 2, 1, "X"]
#     thisdf <- thisdf[,-c(1)]
#     sp::coordinates(thisdf) <- c("x", "z")
#     thisdf <- crop(thisdf, alluvium_5mbedrock_polygon)
#     nnthisdf <- gstat(formula = temp~1, locations = thisdf, nmax = 5, set=list(idp=0))
#     interpthisdf <- interpolate(allblank, nnthisdf)
#     
#     timeslist[[i]] <- interpthisdf
#   }
#   saveRDS(timeslist, 
#           file = paste0(folderpath, modelname, "_rasterlist.RData"))
# }
