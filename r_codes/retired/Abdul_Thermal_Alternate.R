#abdul_thermal_alt

install_github("FluvialLandscapeLab/HGSReader")
library(HGSReader)
library(raster)

gwcols <- colorRampPalette(c("aliceblue", "dodgerblue", "red"))


f <- HGSFile("C:/Users/Katie Fogg/Desktop/HGSwork/Feb2020/abdul/test5/abdul_thermal_alto.pm.dat")

f$variables

ab <- HGSGetData(f, variables = c("Sat", "X", "Z", "Y", "Head", "temperature"), blockNumbers = 1)


abtemp <- ab[,2,,1,"temperature"]
dim(abtemp)
temprab <- raster(t(unclass(abtemp)), xmn = 1, xmx = 501, ymn = 1, ymx = 56)
plot(flip(temprab, direction = 2), asp=NA, col = gwcols(30))

plot(ab[,2,30,1,"temperature"], type = "l")

gwtable <- HGSGetData(f, variables = c("Depth2GWT", "X", "Y", "Z"), blockNumbers = 1)
gwtable[100,2,,1,"Depth2GWT"]

sat <- ab[,2,,1,"Sat"]
satr <- raster(t(unclass(sat)), ymn = 1, ymx = 56, xmn = 1, xmx = 501)
plot(flip(satr, 2), asp = NA)
abline(h = 51, lty = 2)

head <- ab[,2,,1,"Head"]
headr <- raster(t(unclass(head)), ymn = 1, ymx = 56, xmn = 1, xmx = 501)
plot(flip(headr, 2), asp = NA)
abline(h = 51, lty = 2)

