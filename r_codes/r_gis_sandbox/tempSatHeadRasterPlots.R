install_github("FluvialLandscapeLab/HGSReader")
library(HGSReader)
library(raster)

gwcols <- colorRampPalette(c("aliceblue", "dodgerblue", "red"))


f <- HGSFile("C:/Users/Katie Fogg/Desktop/HGSwork/Feb2020/abdul/test3/abdul_thermal_alto.pm.dat")
#f <- HGSFile("C:/Users/Katie Fogg/Desktop/HGSwork/Feb2020/test24/temperatureTestero.pm.dat")

f$variables

g <- HGSGetData(f, variables = c("Sat", "X", "Z", "Y", "Head", "temp"), blockNumbers = 1)
y <- HGSGetData(f, variables = c("Vx", "Vy", "Vz"), blockNumbers = 1)

plot(x = y[300,1,,1,"Vx"], y = 1:55)
abline(h=51)
abline(h=25)
plot(g[300,2,,1,"Head"], y = 1:56, type = "o", col ="forestgreen", xlim = c(31.86, 31.888))
lines(g[301,2,,1,"Head"], y = 1:56, type = "o", col ="dodgerblue")
abline(h = 51.5, lty = 2)
abline(h = 25.5, lty = 1)
par(mfrow = c(1, 2))
plot(g[1,2,,1,"Head"], g[1,2,,1,"Z"], type = "o", ylim= c(37,38), main = "HEAD")
abline(h=37.5, lty= 2)
plot(g[1,2,,1,"Sat"], g[1,2,,1,"Z"], type = "o", ylim= c(37,38), main = "SAT")
abline(h=37.5, lty= 2)

plot(g[2,2,,1,"Head"], g[2,2,,1,"Z"], type = "o", ylim= c(37,38), main = "HEAD")
abline(h=37.48, lty= 2)
plot(g[2,2,,1,"Sat"], g[2,2,,1,"Z"], type = "o", ylim= c(37,38), main = "SAT")
abline(h=37.48, lty= 2)

plot(g[3,2,,1,"Head"], g[2,2,,1,"Z"], type = "o", ylim= c(37,38), main = "HEAD")
abline(h=37.48, lty= 2)
plot(g[3,2,,1,"Sat"], g[2,2,,1,"Z"], type = "o", ylim= c(37,38), main = "SAT")
abline(h=37.48, lty= 2)

plot(g[50,2,,1,"Head"], g[2,2,,1,"Z"], type = "o", ylim= c(37,38), main = "HEAD")
abline(h=37.48, lty= 2)
plot(g[50,2,,1,"Sat"], g[2,2,,1,"Z"], type = "o", ylim= c(37,38), main = "SAT")
abline(h=37.48, lty= 2)



temp <- g[,2,,1,"temp"]
sat <- g[,2,,1,"Sat"]
head <- g[,2,,1,"Head"]

#### RASTER PLOTS ####
tempr <- raster(t(unclass(temp)), xmn = 1, xmx = 501, ymn = 1, ymx = 56)
# this one
#png("Feb2020TempRaster1.png",  width = 800*6, height = 500*6, res = 72*6)
plot(flip(tempr, direction = 2), asp=NA, col = gwcols(30),
     ylab = "Node", xlab = "X")
abline(h = 51.5, lty = 2)
abline(h = 25.5, lty = 1)
#dev.off()

satr <- raster(t(unclass(sat)), ymn = 1, ymx = 56, xmn = 1, xmx = 501)
plot(flip(satr, 2), asp = NA)
abline(h = 51, lty = 2)

headr <- raster(t(unclass(head)), ymn = 1, ymx = 56, xmn = 1, xmx = 501)
plot(flip(headr, 2), asp = NA)
abline(h = 51, lty = 2)
 
plot(g[1,2,,1,"Head"], g[1,2,,1,"Z"], type = "o")
plot(g[2,2,,1,"Head"], g[2,2,,1,"Z"], type = "o", col ="red")
plot(g[3,2,,1,"Head"], g[3,2,,1,"Z"], type = "o", col ="orange")
plot(g[4,2,,1,"Head"], g[4,2,,1,"Z"], type = "o", col ="gold4")
plot(g[5,2,,1,"Head"], g[5,2,,1,"Z"], type = "o", col ="forestgreen")

plot(g[300,2,,1,"Head"], g[300,2,,1,"Z"], type = "o", col ="forestgreen")


#head in bedrock
plot(g[,2,20,1,"Head"])
#head in alluvium
plot(g[,2,40,1,"Head"])
#head in unsat alluvium
plot(g[,2,54,1,"Head"], type = "l")



#png("Feb2020TempRaster2.png",  width = 800*6, height = 500*6, res = 72*6)
plot(flip(tempr, direction = 2), col = gwcols(30),
     ylab = "Node", xlab = "X")
abline(h = 51.5, lty = 2)
abline(h = 25.5, lty = 1)
#dev.off()

#png("Feb2020ModelSat.png", width = 200*4, height = 500*4, res = 72*4)
#par(mar = c(5,5,2,2))
plot(g[200,2,,1,"Sat"], g[501,2,,1,"Z"], type = "o", ylim = c(27, 28), xlim =c(-0.1, 1.1),
     ylab = "Z", xlab = "Saturation")
abline(h = 27.5, lty = 2)
#dev.off()


plot(y =g[1,2,,1,"Z"], x=g[1,2,,1,"temp"], type = "l")
abline(h = 35)
abline(h = 37.5, lty = 2)

plot(y =g[1,2,,1,"Z"], x=g[1,2,,1,"temp"], type = "l", ylim = c(34,38))
abline(h = 35)
abline(h = 37.5, lty = 2)



plot(g[,2,26,1,"temp"], type = "l", ylim = c(2,16))
lines(g[,2,30,1,"temp"])
lines(g[,2,54,1,"temp"])
lines(g[,2,55,1,"temp"])
lines(g[,2,56,1,"temp"])


plot(g[,2,25,1,"temp"], type = "l", ylim = c(2,16))


streamtempinput <- read.csv("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\Feb2020\\streamTemp.txt", sep = " ", header = F)
png("Feb2020StreamTempInput.png", width = 800*4, height = 400*4, res = 72*4)
plot(streamtempinput[1:8760,1]/86400, streamtempinput[1:8760,3], type = "l",
     ylab = "Temperature (C)", xlab = "Day", main = "Stream Temperature Input")
dev.off()

plot(flip(tempr, direction = 2), col = gwcols(30))
abline(h = 51, lty = 2)
abline(h = 26, lty = 1)


g[100,2,,,"temp"]

satr <- raster(t(unclass(sat)), ymn = 1, ymx = 56, xmn = 1, xmx = 501)
plot(flip(satr, 2), asp = NA)
abline(h = 51, lty = 2)

plot(g[1,2,,1,"Sat"], g[1,2,,1,"Z"], type = "l")
lines(g[2,2,,1,"Sat"], g[1,2,,1,"Z"], col = "gold")
lines(g[3,2,,1,"Sat"], g[1,2,,1,"Z"], col = "orange")
lines(g[4,2,,1,"Sat"], g[1,2,,1,"Z"], col = "red")

plot(g[498,2,,1,"Sat"], g[1,2,,1,"Z"], type = "l", xlim = c(0,1))
lines(g[499,2,,1,"Sat"], g[1,2,,1,"Z"], col = "gold")
lines(g[500,2,,1,"Sat"], g[1,2,,1,"Z"], col = "orange")
lines(g[501,2,,1,"Sat"], g[1,2,,1,"Z"], col = "red")






bedrocktemp <- raster(t(unclass(g[,2,1:26,1,"temp"])), xmn = 1, xmx = 501, ymn = 1, ymx = 26)
plot(flip(bedrocktemp, direction = 2), col = gwcols(30))
plot(flip(bedrocktemp, direction = 2), asp = NA, col = gwcols(30))









satw <- g[1,1,,,"Sat"]
satw
longtemp1 <- g[,2,1,,"temp"]
longsat1 <- g[,2,1,,"Sat"]

plot(longsat1)

plot(longtemp1, ylim =c(2,20))
points(g[,2,2,,"temp"], col = "forestgreen")
points(g[,2,3,,"temp"], col = "blue")
points(g[,2,4,,"temp"], col = "purple")
points(g[,2,5,,"temp"], col = "forestgreen")

points(g[,2,6,,"temp"], col = "blue")
points(g[,2,7,,"temp"], col = "purple")
points(g[,2,8,,"temp"], col = "forestgreen")
points(g[,2,9,,"temp"], col = "blue")
points(g[,2,10,,"temp"], col = "gold")

points(g[,2,12,,"temp"], col = "forestgreen")
points(g[,2,14,,"temp"], col = "blue")
points(g[,2,16,,"temp"], col = "gold")

points(g[,2,18,,"temp"], col = "forestgreen")
points(g[,2,20,,"temp"], col = "blue")
points(g[,2,22,,"temp"], col = "gold")
points(g[,2,24,,"temp"], col = "forestgreen")
points(g[,2,25,,"temp"], col = "blue")

points(g[,2,26,,"temp"], col = "orange")
points(g[,2,27,,"temp"], col = "red")
points(g[,2,28,,"temp"], col = "brown")
points(g[,2,40,,"temp"], col = "gold")
points(g[,2,50,,"temp"], col = "red")
points(g[,2,56,,"temp"], col = "gold")



