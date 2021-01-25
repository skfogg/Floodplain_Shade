setwd("C:/Users/Katie Fogg/Desktop/HGSwork")
getwd()
source("C:/Users/Katie Fogg/Desktop/HGSwork/unscramble.R")

rasty <- unscramble("undergroundShade4o.pm.dat", headerlength = 8)
plot(rasty)

rasty6hr <- unscramble("undergroundShade4_2o.pm.dat", headerlength = 8)
plot(rasty6hr)

meter1 <- unscramble("undergroundShade5_2o.pm.dat", headerlength = 8)
plot(meter1)

readLines("undergroundShade6o.pm.dat", 10)
rastyV6 <-unscramble("undergroundShade6o.pm.dat", headerlength = 9, n = 1437728, tempLine = 2579883)


library("sp")
library("raster")
library("rgl")
xvals <- scan("undergroundShade6o.pm.dat", skip = 9, nlines = 1437728/5)
yvals <- scan("undergroundShade6o.pm.dat", skip = (1437728/5)+2+9, nlines = 1437728/5)
zvals <- scan("undergroundShade6o.pm.dat", skip = (1437728/5)+(1437728/5)+3+9, nlines = 1437728/5)
temp <- scan("undergroundShade6o.pm.dat", skip = 2579883, nlines = 1437728/5)

xvals <- scan("undergroundShade6_2o.pm.dat", skip = 9, nlines = 1437728/5)
yvals <- scan("undergroundShade6_2o.pm.dat", skip = (1437728/5)+2+9, nlines = 1437728/5)
zvals <- scan("undergroundShade6_2o.pm.dat", skip = (1437728/5)+(1437728/5)+3+9, nlines = 1437728/5)
temp <- scan("undergroundShade6_2o.pm.dat", skip = 2579883, nlines = 1437728/5)


bigdf2 <- data.frame(x = xvals, y = yvals, z = zvals, temp = temp) 
bigdf <- data.frame(x = xvals, y = yvals, z = zvals, temp = temp)
rastyV6 <- rasterFromXYZ(bigdf, res = c(2,2))

test <- raster(rastyV6)
plot3D(test)

plot(rastyV6)

t2 <- round(temp, 2)
t3 <- round(temp, 0)
df2 <- data.frame(x = xvals, y = yvals, z = zvals, temp = t2)
df3 <- data.frame(x = xvals, y = yvals, z = zvals, temp = t3, color = numeric(length(t3)))
df3_2 <- data.frame(x = xvals, y = yvals, z = zvals, temp = t3, color = numeric(length(t3)))

df3_2$color <- round(t3/2)
unique(floor(t3/2))

row.names(subset(df3_2, temp == 9))
subset(df3_2, temp == 15)$color <- "orange"
subset(df3_2, temp == 16)$color <- "redorange"
subset(df3_2, temp == 9)$color <- "darkred"


for(i in 1:length(df3$color)){
  if(df3$temp[i] == 3){
    df3$color[i] <- "cadetblue1"
  }
  if(df3$temp[i] == 9){
    df3$color[i] <- "khaki1"
  }
  if(df3$temp[i] == 12){
    df3$color[i] <- "gold"
  }
  if(df3$temp[i] == 13){
    df3$color[i] <- "darkorange1"
  }
  if(df3$temp[i] == 15){
    df3$color[i] <- "red2"
  }
}



library("rasterVis")
plot3D(rastyV6)

plot3d(bigdf$x, bigdf$y, bigdf$z, col = t2)

plot3d(df3$x[1:1000], df3$y[1:1000], df3$z[1:1000], col = t3[1:1000])

plot3d(df3$x, df3$y, df3$z, col = t3)
plot3d(df3_2$x, df3_2$y, df3_2$z, col = df3_2$color)


plot(1:10, col = 12)

# number of data points divided by number of layers
1437728/4

# Aquifer Base 586.75 - 638.6
min(df3_2$z[1:359432])
max(df3_2$z[1:359432]) 
plot3d(df3_2$x[1:359432], df3_2$y[1:359432], df3_2$z[1:359432], col = df3_2$color[1:359432])
baseIDX <- 1:359432

# Saturated Layer 589.24 - 641.11
min(df3_2$z[359433:(359432+359432)])
max(df3_2$z[359433:(359432+359432)]) 
plot3d(df3_2$x[359433:(359432+359432)], df3_2$y[359433:(359432+359432)], df3_2$z[359433:(359432+359432)], col = df3_2$color[359433:(359432+359432)])
satIDX <- 359433:(359432+359432)

# Unsaturated Layer 589.74 - 641.61
min(df3_2$z[(359432+359432+1):(359432+359432+359432)])
max(df3_2$z[(359432+359432+1):(359432+359432+359432)]) 
plot3d(df3_2$x[(359432+359432+1):(359432+359432+359432)], df3_2$y[(359432+359432+1):(359432+359432+359432)], df3_2$z[(359432+359432+1):(359432+359432+359432)], col = df3_2$color[(359432+359432+1):(359432+359432+359432)])
unsatIDX <- (359432+359432+1):(359432+359432+359432)

# Soil Top 589.84 - 641.71
min(df3_2$z[(359432+359432+359432+1):1437725])
max(df3_2$z[(359432+359432+359432+1):1437725]) 
plot3d(df3_2$x[(359432+359432+359432+1):1437725], df3_2$y[(359432+359432+359432+1):1437725], df3_2$z[(359432+359432+359432+1):1437725], col = df3_2$color[(359432+359432+359432+1):1437725])
topIDX <- (359432+359432+359432+1):1437725

############
df24hr <- unscramble2("undergroundShade6_3o.pm.dat")
unique(df24hr$tround)

plot3d(df24hr$x, df24hr$y, df24hr$z, col = df24hr$tround)

plot3d(df24hr$x[satIDX], df24hr$y[satIDX], df24hr$z[satIDX], col = df24hr$tround[satIDX]*2)

plot3d(df24hr$x[unsatIDX], df24hr$y[unsatIDX], df24hr$z[unsatIDX], col = df24hr$tround[unsatIDX])


plot3d(df24hr$x[unsatIDX], df24hr$y[unsatIDX], df24hr$tround[unsatIDX])
plot3d(df24hr$x[satIDX], df24hr$y[satIDX], df24hr$tround[satIDX])
plot3d(df24hr$x[baseIDX], df24hr$y[baseIDX], df24hr$tround[baseIDX])


#######
df6hr <-  unscramble2("undergroundShade6o.pm.dat")
df12hr <- unscramble2("undergroundShade6_2o.pm.dat")
df24hr <- unscramble2("undergroundShade6_3o.pm.dat")
df36hr <- unscramble2("undergroundShade6_4o.pm.dat")

median(df6hr$temp[satIDX])
median(df12hr$temp[satIDX])
median(df24hr$temp[satIDX])
median(df36hr$temp[satIDX])



plot3d(df36hr$x[satIDX], df36hr$y[satIDX], df36hr$tround[satIDX])
plot3d(df24hr$x[satIDX], df24hr$y[satIDX], df24hr$z[satIDX], col = df24hr$tround[satIDX]*2)

#######
spinup <- unscramble3("spinUpo.pm.dat")
spinupraster <- rasterFromXYZ(spinup, res = c(2,2))
plot(spinupraster$z)
image(spinupraster, col = topo.colors(30))

head(getValues(spinupraster), 100)

upstreamBounds <- subset(spinup, y == 5051056)
downstreamBounds <- subset(spinup, y == 5056280)


subset(downstreamBounds, x == min(downstreamBounds$x))
subset(downstreamBounds, x == max(downstreamBounds$x))


baseIDX <- 1:359432
satIDX <- 359433:(359432+359432)
unsatIDX <- (359432+359432+1):(359432+359432+359432)
topIDX <- (359432+359432+359432+1):1437725


length(unique(spinup$tround[satIDX]))

spinup$color <- numeric(1078290)

spinupSatTempDF <- data.frame(x = spinup$x[satIDX], y = spinup$y[satIDX], z = spinup$tround[satIDX])
spinupSatTempRast <- rasterFromXYZ(spinupSatTempDF, res = c(2,2))
image(spinupSatTempRast, col = topo.colors(30))
unique(spinup$tround[satIDX])

spinup$deci <- abs(spinup$tround - round(spinup$tround,0))
spinup$color <- numeric(1078290)

rastpal <- colorRamp(c("lightblue", "red"))
spinupcolors <- rastpal(spinup$deci)
plot3d(spinup$x[satIDX], spinup$y[satIDX], spinup$z[satIDX], col= rgb(red = spinupcolors[,1], green = spinupcolors[,2], blue = spinupcolors[,3], max = 255))
plot3d(spinup$x[unsatIDX], spinup$y[unsatIDX], spinup$z[unsatIDX], col= rgb(red = spinupcolors[,1], green = spinupcolors[,2], blue = spinupcolors[,3], max = 255))
plot3d(spinup$x, spinup$y, spinup$z, col= rgb(red = spinupcolors[,1], green = spinupcolors[,2], blue = spinupcolors[,3], max = 255))


#VARIABLES ="X","Y","Z","Zone","Head","Sat","Depth2GWT","Vx","Vy","Vz","Temp"

file = "spinUpo.pm.dat"
xvals <- scan(file, skip = 9, nlines = 215658)
yvals <- scan(file, skip = 215670, nlines =215658)
zvals <- scan(file, skip = 431331, nlines = 215658)
zone <- scan(file, skip = 646993, nlines =  142430)
head <- scan(file, skip = 789437, nlines = 142430)
saturation <- scan(file, skip = 1005085, nlines = 142430)
depth2GWT <-  scan(file, skip = 1220746, nlines = 142430)
xlinearvelocity <- scan(file, skip = 1436408, nlines = 142430)
ylinearvelocity <- scan(file, skip = 1578840, nlines = 142430)
zlinearvelocity <- scan(file, skip = 1721272, nlines = 142430)
temp <- scan(file, skip = 1863704, nlines = 215658)
elementnodelist <- scan(file, skip = 2079365, nlines = 215658) 


head(elementnodelist, 30)






t <- scan(file, skip = 1863704, nlines = 1078296/5)
head(t)
tail(t)
tround <- round(t, 2)
head(tround)
tail(tround)




  