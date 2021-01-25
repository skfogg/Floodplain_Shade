# SIMPLE AQUIFER MODEL 4
library("rgl")
library("SDMTools")
library("grDevices")
setwd("C:/Users/Katie Fogg/Desktop/HGSwork/simple")

unscramble <- function(file){
  
  # EXTRACT PERTINENT DATA FROM FILE
  xvals <- scan(file, skip = 6, nlines = 1200)
  yvals <- scan(file, skip = 1207, nlines = 1200)
  zvals <- scan(file, skip = 2408, nlines = 1200)
  head <- scan(file, skip = 4209, nlines = 1200)
  saturation <- scan(file, skip = 5410, nlines = 1200)
  depth2GWT <- scan(file, skip = 6611, nlines = 1200)
  temp <- scan(file, skip = 9612, nlines = 1200)
  
  # A COLUMN THAT LABELS MY LAYERS SO I CAN USE subset() TO GET DATA FROM ONE LAYER
  layer <- c(rep("base", times = 6000/4), rep("middle", times = 6000/4), rep("topsat", times = 6000/4), rep("top", times = 6000/4))
  
  bigdf <- data.frame(x = xvals, 
                      y = yvals, 
                      z = zvals, 
                      temp = temp, 
                      head = head, 
                      saturation = saturation, 
                      depth2GWT = depth2GWT,
                      layer = layer)
  
  # ADDING MORE COLUMN TO THE DATAFRAME
  # tround IS TEMPERATURE ROUNDED TO TENTHS OF DEGREES
    # I did this so i could use the unique() function to see 
    # the range of values of temperature in my model output
  bigdf$tround <- round(bigdf$temp, 1)
  
  # SETTING UP A COLOR RAMP FUNCTION TO USE FOR TEMPERATURES AND SATURATIONS
    # colorRamp returns a function (rasterpal) that grades color 
    # from color 1 ("dodgerblue") to color 2 ("red")
  rasterpal <- colorRamp(c("skyblue1", "red")) 
  
  # NORMALIZING ROUNDED TEMPERATURES TO BE A FRACTION BETWEEN 0 AND 1
    # My minimum temp is 0, max temp is 1; this is just the format that 
    # rasterpal() needs
  colorForce <- (bigdf$tround - min(bigdf$tround))/(max(bigdf$tround) - min(bigdf$tround))
  # ADDING COLUMN FOR TEMPERATURE COLORS TO USE IN MY PLOTS
  bigdf$tempcolors <- rgb(rasterpal(colorForce), max = 255)
  # ADDING COLUMN FOR SATURATION COLORS TO USE IN PLOTS
  bigdf$colorsat <- rgb(rasterpal(round(bigdf$saturation, 2)), max = 255)
  
  
  
  # min 1.1 round to 1, max 28.8 round to 29
  # scale temperature colors to range 1, 29
  minTempColorRange <- -6
  maxTempColorRange <- 46
  colorForce2 <- (bigdf$tround - minTempColorRange)/(maxTempColorRange - minTempColorRange)
  bigdf$newtempcolors <- rgb(rasterpal(colorForce2), max = 255)
  
  return(bigdf)
}

yr <- unscramble("C:/Users/Katie Fogg/Desktop/HGSwork/simple/4/1yr/simpleo.pm.dat")
mo9 <- unscramble("C:/Users/Katie Fogg/Desktop/HGSwork/simple/4/9mo/simpleo.pm.dat")
mo6 <- unscramble("C:/Users/Katie Fogg/Desktop/HGSwork/simple/4/6mo/simpleo.pm.dat")
mo3 <- unscramble("C:/Users/Katie Fogg/Desktop/HGSwork/simple/4/3mo/simpleo.pm.dat")

yr10 <- unscramble("C:/Users/Katie Fogg/Desktop/HGSwork/simple/5/10yr/simpleo.pm.dat")


yr10s <- unscramble("C:/Users/Katie Fogg/Desktop/HGSwork/simple/9/10yr/simpleo.pm.dat")


# Colors, full range using min and max soil temperatures
minTempColorRange <- -6 
maxTempColorRange <- 46
colorForce2 <- (yr10s$tround - minTempColorRange)/(maxTempColorRange - minTempColorRange)
yr10s$newtempcolors <- rgb(rasterpal(colorForce2), max = 255)



plot3d(yr10s$x, yr10s$y, yr10s$z, col = yr10s$newtempcolors,
       xlim = c(-30, 30))

plot3d(yr10s$x, yr10s$y, yr10s$z, col = yr10s$tempcolors,
       xlim = c(-30, 30))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'temperature colors')

legend_image <- as.raster(matrix(rgb(rasterpal(colorForce2), max = 255), ncol=1))
text(x=1.5, y = seq(-6, 46, l=5), labels = seq(-6, 46,l=5))
rasterImage(legend_image, 0, 0, 1,1)


colfunc <- colorRampPalette(c("red", "skyblue1"))
plot(1:29, 1:29, pch = 19, cex=2, col = colfunc(29))



# layout(matrix(1:2,ncol=2), width = c(2,1), height = c(1,1))
# plot3d(yr10$x, yr10$y, yr10$z, col = yr10$newtempcolors,
       # xlim = c(-30, 30))


whatcolor <- colfunc(29) #heat.colors(29)

legend_image <- as.raster(matrix(whatcolor, ncol=1))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'legend title')
text(x=1.5, y = seq(), labels = seq(0,1,l=5))
rasterImage(legend_image, 0, 0, 1,1)



# Adjusted the y-axis, used spheres instead of points
plot3d(mo3$x, mo3$y, mo3$z, col = mo3$newtempcolors,
       xlim = c(-30, 30), ylim = c(898,1002), zlim = c(8,13),
       xlab = "x", ylab = "y", zlab = "depth", type = "s", radius = 0.8)

plot3d(mo6$x, mo6$y, mo6$z, col = mo6$tempcolors,
       xlim = c(-30, 30), ylim = c(898,1002), zlim = c(8,13),
       xlab = "x", ylab = "y", zlab = "depth", type = "s", radius = 0.8)

plot3d(mo9$x, mo9$y, mo9$z, col = mo9$tempcolors,
       xlim = c(-30, 30), ylim = c(898,1002), zlim = c(8,13),
       xlab = "x", ylab = "y", zlab = "depth", type = "s", radius = 0.8)

plot3d(yr$x, yr$y, yr$z, col = yr$tempcolors,
       xlim = c(-30, 30),
       xlab = "x", ylab = "y", zlab = "depth")

plot3d(yr10$x, yr10$y, yr10$z, col = yr10$newtempcolors,
       xlim = c(-30, 30), xlab = "x", ylab = "y", zlab = "depth", cex = 2)

plot3d(yr10s$x, yr10s$y, yr10s$z, col = yr10s$newtempcolors,
       xlim = c(-30, 30), xlab = "x", ylab = "y", zlab = "depth", cex = 2)

plot3d(yr10s7$x, yr10s7$y, yr10s7$z, col = yr10s7$tempcolors,
       xlim = c(-30, 30), xlab = "x", ylab = "y", zlab = "depth", cex = 2)
unique(yr10s7$tround)
unique(yr10s$tround)

# ylim = c(898,1002), zlim = c(8,13),

rasterpal <- colorRamp(c("skyblue1", "red")) 
alltemps <- c(yr$tround, mo9$tround, mo6$tround, mo3$tround, yr10$tround, yr10s$tround)

# min 1.1 round to 1, max 28.8 round to 29
# scale temperature colors to range 1, 29
colorForce <- (alltemps - min(alltemps))/(max(alltemps) - min(alltemps))
newtempcolors <- rgb(rasterpal(colorForce), max = 255)


