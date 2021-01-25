

#results <- read.table("C:\\Users\\Katie Fogg\\Downloads\\undergroundShade3o.pm.dat", skip = 6, fill = TRUE, stringsAsFactors = FALSE)

readLines("C:\\Users\\Katie Fogg\\Downloads\\undergroundShade3o.pm.dat", 10)

#VARIABLES ="X","Y","Z","Zone","Head","Sat","Depth2GWT","Vx","Vy","Vz","Temp"       

#total lines minus header
3007179 - 7
#2 lines between each
3007172 - (10*2)
#divide by how many sections there are
3007152/11

#I know that temp data starts on line 2791519, ends 3007179

1078296/5


xvals <- scan("C:\\Users\\Katie Fogg\\Downloads\\undergroundShade3o.pm.dat", skip = 7, nlines = 215660)
length(xvals)

yvals <- scan("C:\\Users\\Katie Fogg\\Downloads\\undergroundShade3o.pm.dat", skip = 215668, nlines = 215660)

zvals <- scan("C:\\Users\\Katie Fogg\\Downloads\\undergroundShade3o.pm.dat", skip = 431329, nlines = 215660)

zone <- scan("C:\\Users\\Katie Fogg\\Downloads\\undergroundShade3o.pm.dat", skip = 646990, nlines = 142431)

head <- scan("C:\\Users\\Katie Fogg\\Downloads\\undergroundShade3o.pm.dat", skip = 789422, nlines = 215660)
 
saturation <- scan("C:\\Users\\Katie Fogg\\Downloads\\undergroundShade3o.pm.dat", skip = 1005084, nlines = 142431)

depth2gw <- scan("C:\\Users\\Katie Fogg\\Downloads\\undergroundShade3o.pm.dat", skip = 1220744, nlines = 142431)

#x linear velocity
1436405
#y linear velocity
1578837
#z linear velocity
1721269

temp <- scan("C:\\Users\\Katie Fogg\\Downloads\\undergroundShade3o.pm.dat", skip = 1863701, nlines = 215660)

bigdf <- data.frame(x = xvals, y = yvals, z = zvals, temp = temp)

library("sp")
library("raster")
xmin <- min(xvals)
xmax <- max(xvals)
ymin <- min(yvals)
ymax <- max(yvals)
zmin <- min(zvals)
zmax <- max(zvals)

raster(vals = temp, xmn = xmin, xmx = xmax, ymn = ymin, ymx = ymax)

rast1 <- rasterFromXYZ(bigdf, res = c(2,2))
plot(rast1)

cumulative <- read.table("C:/Users/Katie Fogg/Desktop/HGSwork/undergroundShade4o.mass_balance_cumulative.Temp.dat", skip = 3, col.names = c("Time", "In", "Out", "Stored", "In+Out-Stored"))
raw <- read.table("C:/Users/Katie Fogg/Desktop/HGSwork/undergroundShade4o.mass_balance_raw.Temp.dat", skip = 4, col.names = c("Time","Fixed head+","Fixed head-","Fixed conc+","Fixed conc-","NET1 Source/Sink rate","Porous media","PM decayed","PM sorbed","PM srb/dk","Init mass","Net since t0","NET2 Accum. rate","ERROR (NET1-NET2)","Error rel." ))
