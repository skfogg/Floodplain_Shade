##########################################
## MARCH NEW MODEL COMPARISON OF RESULTS #
##########################################

install_github("FluvialLandscapeLab/HGSReader")
library(HGSReader)
library(raster)

gwcols <- colorRampPalette(c("aliceblue", "dodgerblue", "red"))

f19 <- HGSFile("C:/Users/Katie Fogg/Desktop/HGSwork/March2020/newtest19/temperatureTestero.pm.dat")
f18 <- HGSFile("C:/Users/Katie Fogg/Desktop/HGSwork/March2020/newtest18/temperatureTestero.pm.dat")
f17 <- HGSFile("C:/Users/Katie Fogg/Desktop/HGSwork/March2020/newtest17/temperatureTestero.pm.dat")
f16 <- HGSFile("C:/Users/Katie Fogg/Desktop/HGSwork/March2020/newtest16/temperatureTestero.pm.dat")
f15 <- HGSFile("C:/Users/Katie Fogg/Desktop/HGSwork/March2020/newtest15/temperatureTestero.pm.dat")
f14 <- HGSFile("C:/Users/Katie Fogg/Desktop/HGSwork/March2020/newtest14/temperatureTestero.pm.dat")

g19 <- HGSGetData(f19, variables = c("Sat", "X", "Z", "Y", "Head", "temp"), blockNumbers = 1)
g18 <- HGSGetData(f18, variables = c("Sat", "X", "Z", "Y", "Head", "temp"), blockNumbers = 1)
g17 <- HGSGetData(f17, variables = c("Sat", "X", "Z", "Y", "Head", "temp"), blockNumbers = 1)
g16 <- HGSGetData(f16, variables = c("Sat", "X", "Z", "Y", "Head", "temp"), blockNumbers = 1)
g15 <- HGSGetData(f15, variables = c("Sat", "X", "Z", "Y", "Head", "temp"), blockNumbers = 1)
g14 <- HGSGetData(f14, variables = c("Sat", "X", "Z", "Y", "Head", "temp"), blockNumbers = 1)

temp15 <- g15[,2,,1,"temp"]
sat15 <- g15[,2,,1,"Sat"]
head15 <- g15[,2,,1,"Head"]

temp14 <- g14[,2,,1,"temp"]
sat14 <- g14[,2,,1,"Sat"]
head14 <- g14[,2,,1,"Head"]

plot(g14[,2,11,1,"temp"], type="l", ylim = c(0,15))
lines(g15[,2,11,1,"temp"], col = "blue")

lines(g14[,2,1,1,"temp"], type="l")
lines(g15[,2,1,1,"temp"], col = "blue")

par(mfrow = c(2,1))
plot(g14[,2,57,1,"temp"], type="l", ylim = c(0,15))
for(i in 26:57){
  lines(g14[,2,i,1,"temp"])
}

plot(g15[,2,48,1,"temp"], type="l", ylim = c(0,15))
for(i in 17:48){
  lines(g15[,2,i,1,"temp"])
}

par(mfrow = c(1,1))
plot(g14[1,2,,1,"temp"], g14[1,2,,1,"Z"], type = "l", xlim = c(0,11.9))
lines(g15[1,2,,1,"temp"], g15[1,2,,1,"Z"], col ="blue", type = "l")
lines(g16[1,2,,1,"temp"], g16[1,2,,1,"Z"], col ="red", type = "l")
lines(g17[1,2,,1,"temp"], g17[1,2,,1,"Z"], col ="orange", type = "l")
lines(g18[1,2,,1,"temp"], g18[1,2,,1,"Z"], col ="violet", type = "l")
lines(g19[1,2,,1,"temp"], g19[1,2,,1,"Z"], col ="red", type = "l")

g18[1,2,,1,"Z"]
g19[1,2,,1,"Z"]

plot(g18[,2,11,,"X"], g18[,2,11,,"temp"], type = "l", main = "30m")
lines(g19[,2,21,,"X"], g19[,2,21,,"temp"], col = "dodgerblue")

max(g18[,2,11,,"temp"] - g19[,2,21,,"temp"])

plot(g18[,2,1,,"X"], g18[,2,1,,"temp"], type = "l", main = "10m")
lines(g19[,2,1,,"X"], g19[,2,1,,"temp"], col = "dodgerblue")

max(g18[,2,1,,"temp"] - g19[,2,1,,"temp"])


plot(g18[,2,16,,"X"], g18[,2,16,,"temp"], type = "l", main = "35m")
lines(g19[,2,26,,"X"], g19[,2,26,,"temp"], col = "dodgerblue")

max(g18[,2,16,,"temp"] - g19[,2,26,,"temp"])

plot(g18[,2,17,,"X"], g18[,2,17,,"temp"], type = "l", main = "35.1m")
lines(g19[,2,27,,"X"], g19[,2,27,,"temp"], col = "dodgerblue")

plot(g18[,2,1:17,,"X"], g18[,2,1:17,,"temp"], pch = ".", main = "35.1m - 10m")
points(g19[,2,1:27,,"X"], g19[,2,1:27,,"temp"], col = "dodgerblue", pch = ".")

par(mfrow = c(1,6))
plot(g18[1,2,,,"temp"], g18[1,2,,,"Z"], type = "l")
lines(g19[1,2,,,"temp"], g19[1,2,,,"Z"], type = "l", col = "dodgerblue")

plot(g18[100,2,,,"temp"], g18[100,2,,,"Z"], type = "l")
lines(g19[100,2,,,"temp"], g19[100,2,,,"Z"], type = "l", col = "dodgerblue")

plot(g18[200,2,,,"temp"], g18[200,2,,,"Z"], type = "l")
lines(g19[200,2,,,"temp"], g19[200,2,,,"Z"], type = "l", col = "dodgerblue")

plot(g18[300,2,,,"temp"], g18[300,2,,,"Z"], type = "l")
lines(g19[300,2,,,"temp"], g19[300,2,,,"Z"], type = "l", col = "dodgerblue")

plot(g18[400,2,,,"temp"], g18[400,2,,,"Z"], type = "l")
lines(g19[400,2,,,"temp"], g19[400,2,,,"Z"], type = "l", col = "dodgerblue")

plot(g18[501,2,,,"temp"], g18[501,2,,,"Z"], type = "l")
lines(g19[501,2,,,"temp"], g19[501,2,,,"Z"], type = "l", col = "dodgerblue")


par(mfrow = c(1,6))
plot(g18[1,2,,,"temp"], g18[1,2,,,"Z"], type = "l")
lines(g19[1,2,,,"temp"], g19[1,2,,,"Z"], type = "l", col = "dodgerblue")

plot(g18[2,2,,,"temp"], g18[2,2,,,"Z"], type = "l")
lines(g19[2,2,,,"temp"], g19[2,2,,,"Z"], type = "l", col = "dodgerblue")

plot(g18[3,2,,,"temp"], g18[3,2,,,"Z"], type = "l")
lines(g19[3,2,,,"temp"], g19[3,2,,,"Z"], type = "l", col = "dodgerblue")

plot(g18[4,2,,,"temp"], g18[4,2,,,"Z"], type = "l")
lines(g19[4,2,,,"temp"], g19[4,2,,,"Z"], type = "l", col = "dodgerblue")

plot(g18[5,2,,,"temp"], g18[5,2,,,"Z"], type = "l")
lines(g19[5,2,,,"temp"], g19[5,2,,,"Z"], type = "l", col = "dodgerblue")

plot(g18[6,2,,,"temp"], g18[6,2,,,"Z"], type = "l")
lines(g19[6,2,,,"temp"], g19[6,2,,,"Z"], type = "l", col = "dodgerblue")



par(mfrow = c(1,1))
r=11
i=18-r
j=27-r
plot(g18[,2,i,,"X"], g18[,2,i,,"temp"], type = "l", 
     ylim = c(min(g18[,2,i,,"temp"], g19[,2,j,,"temp"]),
              max(g18[,2,i,,"temp"], g19[,2,j,,"temp"])),
     main = paste("proportional model =", round(g18[1,2,i,,"Z"],2), "uniform model =", g19[1,2,j,,"Z"]))
lines(g19[,2,j,,"X"], g19[,2,j,,"temp"], col = "dodgerblue")
text(x = 600, 
     y = min(g18[,2,i,,"temp"], g19[,2,j,,"temp"]), 
     pos = 3,
     labels = round(max(g18[,2,i,,"temp"] - g19[,2,j,,"temp"]), 2),
     col = "red")
text(x = 600, 
     y = min(g18[,2,i,,"temp"], g19[,2,j,,"temp"])+0.8, 
     pos = 3,
     labels = "Maximum Temp Diff:")


plot(g18[100,2,,,"temp"], g18[100,2,,,"Z"], type = "l")
lines(g19[100,2,,,"temp"], g19[100,2,,,"Z"], type = "l", col = "dodgerblue")

plot(g18[200,2,,,"temp"], g18[200,2,,,"Z"], type = "l")
lines(g19[200,2,,,"temp"], g19[200,2,,,"Z"], type = "l", col = "dodgerblue")

plot(g18[300,2,,,"temp"], g18[300,2,,,"Z"], type = "l")
lines(g19[300,2,,,"temp"], g19[300,2,,,"Z"], type = "l", col = "dodgerblue")

plot(g18[400,2,,,"temp"], g18[400,2,,,"Z"], type = "l")
lines(g19[400,2,,,"temp"], g19[400,2,,,"Z"], type = "l", col = "dodgerblue")

plot(g18[501,2,,,"temp"], g18[501,2,,,"Z"], type = "l")
lines(g19[501,2,,,"temp"], g19[501,2,,,"Z"], type = "l", col = "dodgerblue")

