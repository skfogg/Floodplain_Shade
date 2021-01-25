library("raster")
library("rgl")
library("rasterVis")
setwd("C:/Users/Katie Fogg/Desktop/HGSwork")

unscramble <- function(file){
  xvals <- scan(file, skip = 9, nlines = 504000)
  yvals <- scan(file, skip = 504010, nlines = 504000)
  zvals <- scan(file, skip = 1008011, nlines = 504000)
  head <- scan(file, skip = 1846774, nlines = 504000)
  saturation <- scan(file, skip = 2350775, nlines = 504000)
  depth2GWT <- scan(file, skip = 2854776, nlines = 504000)
  temp <- scan(file, skip = 4363063, nlines = 504000)
  
  layer <- c(rep("base", times = 2520000/3), rep("middle", times = 2520000/3), rep("top", times = 2520000/3))
  
  bigdf <- data.frame(x = xvals, 
                      y = yvals, 
                      z = zvals, 
                      temp = temp, 
                      head = head, 
                      saturation = saturation, 
                      depth2GWT = depth2GWT,
                      layer = layer)
  
  return(bigdf)
}

getVelocities <- function(file){
  
  xlinearvelocity <- scan(file, skip = 3358777, nlines = 334761)
  ylinearvelocity <- scan(file, skip = 3693539, nlines = 334761)
  zlinearvelocity <- scan(file, skip = 4028301, nlines = 334761)
  
  bigdf <- data.frame(x = xlinearvelocity, y = ylinearvelocity, z = zlinearvelocity)
  
  return(bigdf)
}

rasterpal <- colorRamp(c("dodgerblue", "red")) 

newaquifer <- unscramble("newest/version_1/spinUpo.pm.dat")
newaquifer$layer <- c(rep("base", times = 2520000/3), rep("middle", times = 2520000/3), rep("top", times = 2520000/3))
newaquifer$tround <- round(newaquifer$temp, 1)

colorForce <- (newaquifer$tround - min(newaquifer$tround))/(max(newaquifer$tround) - min(newaquifer$tround))
newaquifer$tempcolors <- rgb(rasterpal(colorForce), max = 255)
plot3d(newaquifer$x, newaquifer$y, newaquifer$z, col = newaquifer$tempcolors)

newaquifer$colorsat <- rgb(rasterpal(round(newaquifer$saturation, 2)), max = 255)
plot3d(newaquifer$x, newaquifer$y, newaquifer$z, col = newaquifer$colorsat)
unique(round(newaquifer$saturation, 2))

baseIDX <- 1:840000
satIDX <- 840001:1680000
unsatIDX <- 1680001:2520000


slicetoplot <- 5000
sliceIDX <- c(1:slicetoplot, 840001:(840001+slicetoplot), 1680001:(1680001+slicetoplot))
plot3d(newaquifer$x[sliceIDX], newaquifer$y[sliceIDX], newaquifer$z[sliceIDX], col = newaquifer$tempcolors[sliceIDX])
plot3d(newaquifer$x[sliceIDX], newaquifer$y[sliceIDX], newaquifer$z[sliceIDX], col = newaquifer$colorsat[sliceIDX])


summary(newaquifer$head[satIDX])
##############
newaquiferVel <- getVelocities("newest/spinUpo.pm.dat")


# plot(newaquiferVel$x)
# abline(v = length(newaquiferVel$x)/3, col = "red")
# plot(newaquiferVel$y)
# plot(newaquiferVel$z)


newaquiferraster <- rasterFromXYZ(newaquifer, res = c(2,2))
plot(newaquiferraster$head)
plot(newaquiferraster$saturation)
plot(newaquiferraster$depth2GWT)

image(newaquiferraster$temp, col = topo.colors(3))

plot3d(newaquifer$x, newaquifer$y, newaquifer$z, col = newaquifer$head)

length(newaquifer$z)

baseIDX <- 1:840000
satIDX <- 840001:1680000
unsatIDX <- 1680001:2520000


plot(newaquiferraster$saturation[baseIDX])


basedf <- newaquifer[baseIDX,]
baseraster <- rasterFromXYZ(basedf, res=c(2,2))
plot(baseraster)

satdf <- newaquifer[satIDX,]
satraster <- rasterFromXYZ(satdf, res=c(2,2))
plot(satraster)

unsatdf <- newaquifer[unsatIDX,]
unsatraster <- rasterFromXYZ(unsatdf, res=c(2,2))
plot(unsatraster)

newsatraster <- rasterFromXYZ(data.frame(x = satdf$x[1:1000], y = satdf$y[1:1000], z = round(satdf$temp,0)[1:1000]))
image(newsatraster, col = rainbow(3))
plot(newsatraster)

unique(round(satdf$temp, 0))


