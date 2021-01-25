install_github("FluvialLandscapeLab/HGSReader")
library(HGSReader)
library(raster)

gwcols <- colorRampPalette(c("aliceblue", "dodgerblue", "red"))

f <- HGSFile("C:/Users/Katie Fogg/Desktop/HGSwork/April2020/iskulpaa1/temperatureTestero.pm.dat")

g <- HGSGetData(f, variables = c("Sat", "X", "Z", "Y", "Head", "temp"), blockNumbers = 1)
y <- HGSGetData(f, variables = c("Vx", "Vy", "Vz"), blockNumbers = 1)

temp <- g[,2,,,"temp"]
sat <- g[,2,,,"Sat"]
head <- g[,2,,,"Head"]

g[1,2,,,"Z"]

plot(g[,2,27,1,"X"], g[,2,27,1,"temp"], type = "l", ylim= c(2, 15))
lines(g[,2,27,2,"X"], g[,2,27,2,"temp"], col = "red")
#Zoomed in
plot(g[,2,27,1,"X"], g[,2,27,1,"temp"], type = "o",
     ylim = c(2,3), xlim = c(0,10))
lines(g[,2,27,2,"X"], g[,2,27,2,"temp"], type = "o",
     col = "red")
length(g[,2,27,,"X"])
length(g[1,2,,,"Z"])

#### RASTER PLOTS ####
par(mfrow=c(1,1))

#proportional sublayering and graded x
tempr <- raster(t(unclass(temp)), xmn = 1, xmx = 496, ymn = 1, ymx = 48)
#proportional sublayering
tempr <- raster(t(unclass(temp)), xmn = 1, xmx = 501, ymn = 1, ymx = 48)
#uniform sublayering
tempr <- raster(t(unclass(temp)), xmn = 1, xmx = 501, ymn = 1, ymx = 56)

# this one
#png("Feb2020TempRaster1.png",  width = 800*6, height = 500*6, res = 72*6)
plot(flip(tempr, direction = 2), asp=NA, col = gwcols(30),
     ylab = "Node", xlab = "X")
#abline(h = 46.5, lty = 2)
abline(h = 25.5, lty = 1)
#dev.off()
abline(h = 44)
abline(h = 17)
max(temp)
min(temp)


# ADDING MORE COLUMN TO THE DATAFRAME
# tround IS TEMPERATURE ROUNDED TO TENTHS OF DEGREES
# I did this so i could use the unique() function to see 
# the range of values of temperature in my model output
tround <- round(g[,,,,"temp"], 1)

# SETTING UP A COLOR RAMP FUNCTION TO USE FOR TEMPERATURES AND SATURATIONS
# colorRamp returns a function (rasterpal) that grades color 
# from color 1 ("dodgerblue") to color 2 ("red")
rasterpal <- colorRamp(c("aliceblue", "dodgerblue", "red")) 

# NORMALIZING ROUNDED TEMPERATURES TO BE A FRACTION BETWEEN 0 AND 1
# My minimum temp is 0, max temp is 1; this is just the format that 
# rasterpal() needs
normalizeAll <- (tround - min(tround))/(max(tround) - min(tround))
# ADDING COLUMN FOR TEMPERATURE COLORS TO USE IN MY PLOTS
tempcolors <- rgb(rasterpal(normalizeAll), max = 255)

tcopy <- tround


library(scatterplot3d)
scatterplot3d(x = g[,2,30:48,,"X"],
              y = g[,2,30:48,,"Y"],
              z = g[,2,30:48,,"Z"],
              color = tempcolors,
              pch = 19,
              ylim = c(1, 3))


data1 <- as.data.frame(g[,2,,2,c("X","Z", "temp")])
data1$tround <- round(data1$temp, 1)
data1$tnormalized <- with(data1, (tround - min(tround))/(max(tround) - min(tround)))
data1$tcols <- rgb(rasterpal(data1$tnormalized), max = 255)

plot(Z~X, data=data1,
     col = data1$tcols,
     pch = ".", cex=3)

plot(x = g[,2,,2,"X"],
     y = g[,2,,2,"Z"],
     col = tempcolors,
     pch = 19)
par(bg = "black")
plot(x = g[,2,,2,"X"],
     y = g[,2,,2,"Z"],
     col = tempcolors,
     pch = ".", 
     ylim = c(34,39), xlim = c(0,200),
     cex = 3)


plot(g[1,2,,1,"temp"], g[501,2,,1,"Z"], type = "l")
plot(g[1,2,,1,"temp"], g[501,2,,1,"Z"], type = "o", ylim = c(27,28.1)) 

plot(g[,2,56,1,"temp"], type = "l")
plot(g[,2,20,1,"temp"], type = "l")

plot(g[1,2,,1,"X"], g[1,2,,1,"Z"])

satr <- raster(t(unclass(sat)), ymn = 1, ymx = 48, xmn = 1, xmx = 496)
plot(flip(satr, 2), asp = NA)
abline(h = 17, lty = 2)
max(sat)
min(sat)

headr <- raster(t(unclass(head)), ymn = 1, ymx = 56, xmn = 1, xmx = 501)
plot(flip(headr, 2), asp = NA)
abline(h = 51, lty = 2)

par(mfrow=c(1,6))
plot(g[1,2,,1,"Sat"], g[501,2,,1,"Z"], type = "o", ylim = c(27, 28.1), xlim =c(0, 1),
     ylab = "Z", xlab = "Saturation")
abline(h = 27.5, lty = 2)
plot(g[100,2,,1,"Sat"], g[501,2,,1,"Z"], type = "o", ylim = c(27, 28.1), xlim =c(0, 1),
     ylab = "Z", xlab = "Saturation")
abline(h = 27.5, lty = 2)
plot(g[200,2,,1,"Sat"], g[501,2,,1,"Z"], type = "o", ylim = c(27, 28.1), xlim =c(0, 1),
     ylab = "Z", xlab = "Saturation")
abline(h = 27.5, lty = 2)
plot(g[300,2,,1,"Sat"], g[501,2,,1,"Z"], type = "o", ylim = c(27, 28.1), xlim =c(0, 1),
     ylab = "Z", xlab = "Saturation")
abline(h = 27.5, lty = 2)
plot(g[400,2,,1,"Sat"], g[501,2,,1,"Z"], type = "o", ylim = c(27, 28.1), xlim =c(0, 1),
     ylab = "Z", xlab = "Saturation")
abline(h = 27.5, lty = 2)
plot(g[500,2,,1,"Sat"], g[501,2,,1,"Z"], type = "o", ylim = c(27, 28.1), xlim =c(0, 1),
     ylab = "Z", xlab = "Saturation")
abline(h = 27.5, lty = 2)
par(mfrow = c(1,1))

