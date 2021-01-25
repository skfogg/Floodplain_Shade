library(HGSReader)
library(raster)
#library(RColorBrewer)
#source('C:/Users/Katie Fogg/Desktop/HGSwork/incrementDim.R')
#library(animation)
library(sp)
library(gstat)
library(raster)
#library(dismo)
#library(deldir)
library(rgeos)
library(tidyr)
library(stringr)


soilmodel <- "soil_1.0m"
sunmodel <- "sunny"
K <- "K_300m_day"
folderlocation <- "sunny_iskulpaa_soil_input"

  # "fake_shady_iskulpaa"

##############
## Get Data ##
##############
folderpath <- paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\sensitivity\\", K,"\\", folderlocation, "\\", soilmodel, "\\")

assign(paste0(soilmodel, "_means_", sunmodel), readRDS(paste0(folderpath, soilmodel, "_dailymeans.RData")))
assign(paste0(soilmodel, "_structure_", sunmodel), readRDS(paste0(folderpath, soilmodel, "_modelstructure.RData")))


###################################
#### Create Raster List & Save ####
###################################

allblank <- raster(ncols = 8000, 
                   nrows = (tail(get(paste0(soilmodel, "_structure_", sunmodel))[1,2,,"Z"],1)-30) / 0.1, 
                   xmn = 0, 
                   xmx = 800, 
                   ymn = 30, 
                   ymx = tail(get(paste0(soilmodel, "_structure_", sunmodel))[1,2,,"Z"], 1))
timeslist <- list(24)

alluvium_5mbedrock_polygon <- readWKT(paste0("POLYGON((0 30, 800 30, 800 ", tail(get(paste0(soilmodel, "_structure_", sunmodel))[1,2,,"Z"], 1), ", 0 ", tail(get(paste0(soilmodel, "_structure_", sunmodel))[1,2,,"Z"], 1),", 0 30))")) 


for(i in 1:24){
  thisdf <- as.data.frame(get(paste0(soilmodel, "_means_", sunmodel))[,,i])
  thisdf <- gather(thisdf, xidx, temp, V1:V496)
  thisdf$z <- get(paste0(soilmodel, "_structure_", sunmodel))[1,2,,"Z"][seq(length(get(paste0(soilmodel, "_structure_", sunmodel))[1,2,,"Z"]),1,by=-1)]
  thisdf$xidx <- as.numeric((str_extract(thisdf$xidx, "[0-9]+")))
  thisdf$x <- get(paste0(soilmodel, "_structure_", sunmodel))[thisdf$xidx, 2, 1, "X"]
  thisdf <- thisdf[,-c(1)]
  sp::coordinates(thisdf) <- c("x", "z")
  thisdf <- crop(thisdf, alluvium_5mbedrock_polygon)
  nnthisdf <- gstat(formula = temp~1, locations = thisdf, nmax = 5, set=list(idp=0))
  interpthisdf <- interpolate(allblank, nnthisdf)
  
  timeslist[[i]] <- interpthisdf
}
saveRDS(timeslist, 
        paste0(folderpath, soilmodel, "_", sunmodel, "_rasterlist.RData"))




