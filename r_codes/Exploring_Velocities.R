##
## Velocities
##

boxdir <-  "C:\\Users\\t24x137\\Box\\Floodplain_Shade_Box"

structure_0.5 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_400m_day/riveronly/soil_0.5m/soil_0.5m_modelstructure.RData")
structure_1.0 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_400m_day/riveronly/soil_1.0m/soil_1.0m_modelstructure.RData")
structure_2.0 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_400m_day/riveronly/soil_2.0m/soil_2.0m_modelstructure.RData")
structure_3.0 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_400m_day/riveronly/soil_3.0m/soil_3.0m_modelstructure.RData")

structure_3.0_short <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_400m_day/short_runs/riveronly/soil_3.0m/soil_3.0m_modelstructure.RData")


structureList <- list(structure_0.5, structure_1.0, structure_2.0, structure_3.0)


## Q: Are hz velocities the same across model runs of the same K?
### Q Part 1: Does the surface BC affect the HZ velocity?
k400_sunny_v <- readRDS(paste0(boxdir, "\\K_400m_day\\sunny\\soil_0.5m\\soil_0.5m_velocity.RData"))
k400_shady_v <- readRDS(paste0(boxdir, "\\K_400m_day\\shady\\soil_0.5m\\soil_0.5m_velocity.RData"))
k400_riveronly_v <- readRDS(paste0(boxdir, "\\K_400m_day\\riveronly\\soil_0.5m\\soil_0.5m_velocity.RData"))

par(mfrow = c(1,1))
plot(k400_riveronly_v[,2,46,1,"Vx"] ~ structureList[[1]][1:945,2,46,"X"], col = "dodgerblue")
## Look at all depths within the HZ:
mapply(function(z,c) lines(k400_riveronly_v[,2,z,1,"Vx"] ~ structureList[[1]][1:945,2,z,"X"], col = c),
       17:46,
       hcl.colors(30))
points(k400_sunny_v[,2,46,1,"Vx"] ~ structureList[[1]][1:945,2,46,"X"], col = "gold")
mapply(function(z,c) lines(k400_sunny_v[,2,z,1,"Vx"] ~ structureList[[1]][1:945,2,z,"X"], col = c),
       17:46,
       "red")
points(k400_shady_v[,2,46,1,"Vx"] ~ structureList[[1]][1:945,2,46,"X"], col = "gray")
mapply(function(z,c) lines(k400_sunny_v[,2,z,1,"Vx"] ~ structureList[[1]][1:945,2,z,"X"], col = c),
       17:46,
       "purple")
### Result: As expected, the surface BC has no effect on the x-direction velocity

### Q Part 2: Does the thickness of the vadose zone affect the x-direction velocity?
k400_sunny0.5_v <- readRDS(paste0(boxdir, "\\K_400m_day\\sunny\\soil_0.5m\\soil_0.5m_velocity.RData"))
k400_sunny1.0_v <- readRDS(paste0(boxdir, "\\K_400m_day\\sunny\\soil_1.0m\\soil_1.0m_velocity.RData"))
k400_sunny2.0_v <- readRDS(paste0(boxdir, "\\K_400m_day\\sunny\\soil_2.0m\\soil_2.0m_velocity.RData"))
k400_sunny3.0_v <- readRDS(paste0(boxdir, "\\K_400m_day\\sunny\\soil_3.0m\\soil_3.0m_velocity.RData"))

plot(k400_sunny0.5_v[,2,46,1,"Vx"] ~ structureList[[1]][1:945,2,46,"X"], col = "brown", ylim =c(min(k400_sunny3.0_v[,2,46,1,"Vx"]),
                                                                                               max(k400_sunny3.0_v[,2,46,1,"Vx"])),
     type = "l")
mapply(function(z,c) lines(k400_sunny0.5_v[,2,z,1,"Vx"] ~ structureList[[1]][1:945,2,z,"X"], col = c),
       17:46,
       "brown")

lines(k400_sunny1.0_v[,2,46,1,"Vx"] ~ structureList[[1]][1:945,2,46,"X"], col = "orange")
mapply(function(z,c) lines(k400_sunny1.0_v[,2,z,1,"Vx"] ~ structureList[[1]][1:945,2,z,"X"], col = c),
       17:46,
       "orange")

lines(k400_sunny2.0_v[,2,46,1,"Vx"] ~ structureList[[1]][1:945,2,46,"X"], col = "forestgreen")
mapply(function(z,c) lines(k400_sunny2.0_v[,2,z,1,"Vx"] ~ structureList[[1]][1:945,2,z,"X"], col = c),
       17:46,
       "forestgreen")

plot(k400_sunny3.0_v[,2,46,1,"Vx"] ~ structureList[[1]][1:945,2,46,"X"], col = "black", type = "l")
mapply(function(z,c) lines(k400_sunny3.0_v[,2,z,1,"Vx"] ~ structureList[[1]][1:945,2,z,"X"], col = c),
       17:46,
       "black")

mean(k400_sunny0.5_v[,2,17:46,1,"Vx"])*86400
mean(k400_sunny1.0_v[,2,17:46,1,"Vx"])*86400
mean(k400_sunny2.0_v[,2,17:46,1,"Vx"])*86400
mean(k400_sunny3.0_v[,2,17:46,1,"Vx"])*86400


plot(k400_sunny3.0_v[,2,46,1,"Vx"] ~ structureList[[1]][1:945,2,46,"X"], col = hcl.colors(30)[30], type = "l")
mapply(function(z,c) lines(k400_sunny3.0_v[,2,z,1,"Vx"] ~ structureList[[1]][1:945,2,z,"X"], col = c),
       17:46,
       hcl.colors(30))

plot(k400_sunny3.0_v[,2,46,1,"Vx"] ~ structureList[[1]][1:945,2,46,"X"], col = hcl.colors(30)[30], type = "l")
mapply(function(z) lines(k400_sunny3.0_v[,2,z,1,"Vx"] ~ structureList[[4]][1:945,2,z,"X"], col = hcl.colors(length(z))),
       40:46)

mean(k400_sunny3.0_v[,2,17:40,1,"Vx"])*86400
mean(k400_sunny3.0_v[,2,17:43,1,"Vx"])*86400

k400_sunny3.0_sat <- readRDS(paste0(boxdir, "\\K_400m_day\\sunny\\soil_3.0m\\soil_3.0m_saturation_T1.RData"))
xval <- 295
plot(k400_sunny3.0_sat[xval,2,40:77,1,"Sat"], structureList[[4]][xval,2,40:77,"Z"], type = "o")
watertable <- -0.01*structureList[[4]][xval,2,1,"X"] + 48
abline(h=watertable, lty = 2)

## WATER TABLE NOT FLAT:
image(k400_sunny3.0_sat[,2,17:77,1,"Sat"])

k400_sunny0.5_sat <- readRDS(paste0(boxdir, "\\K_400m_day\\sunny\\soil_0.5m\\soil_0.5m_saturation_T1.RData"))
k400_sunny1.0_sat <- readRDS(paste0(boxdir, "\\K_400m_day\\sunny\\soil_1.0m\\soil_1.0m_saturation_T1.RData"))
k400_sunny2.0_sat <- readRDS(paste0(boxdir, "\\K_400m_day\\sunny\\soil_2.0m\\soil_2.0m_saturation_T1.RData"))
k400_3.0_short_sat <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_400m_day/short_runs/riveronly/soil_3.0m/soil_3.0m_saturation_T1.RData")
k100_sunny3.0_sat <- readRDS(paste0(boxdir, "\\K_100m_day\\sunny\\soil_3.0m\\soil_3.0m_saturation_T1.RData"))

image(k400_sunny0.5_sat[,2,17:51,1,"Sat"])
image(k400_sunny1.0_sat[,2,17:57,1,"Sat"])
image(k400_sunny2.0_sat[,2,17:67,1,"Sat"])
image(k400_sunny3.0_sat[,2,17:77,1,"Sat"])


filled.contour(structureList[[1]][,2,1,"X"],
               structureList[[1]][1,2,17:51,"Z"],
               k400_sunny0.5_sat[,2,17:51,1,"Sat"])
filled.contour(structureList[[2]][,2,1,"X"],
               structureList[[2]][1,2,17:57,"Z"],
               k400_sunny1.0_sat[,2,17:57,1,"Sat"])

filled.contour(structureList[[3]][,2,1,"X"],
               structureList[[3]][1,2,17:67,"Z"],
               k400_sunny2.0_sat[,2,17:67,1,"Sat"])
## K400 3.0
filled.contour(structureList[[4]][,2,1,"X"],
               structureList[[4]][1,2,17:77,"Z"],
               k400_sunny3.0_sat[,2,17:77,1,"Sat"])

# long model cut down:
filled.contour(structureList[[4]][1:545,2,1,"X"],
               structureList[[4]][1,2,17:77,"Z"],
               k400_sunny3.0_sat[1:545,2,17:77,1,"Sat"],
               main = "Long cut to 1000")

# Short model
filled.contour(structure_3.0_short[,2,1,"X"],
               structure_3.0_short[1,2,17:67,"Z"],
               k400_3.0_short_sat[,2,17:67,1,"Sat"],
               main = "K400 3.0 short model")

# K100 3.0
filled.contour(k100_sunny3.0_sat[,2,1,1,"X"],
               k100_sunny3.0_sat[1,2,17:77,1,"Z"],
               k100_sunny3.0_sat[,2,17:77,1,"Sat"],
               main = "K100 3.0")



f_k400_3.0 <- HGSFile(paste0(boxdir, "\\K_400m_day\\sunny\\soil_3.0m\\temperatureTestero.pm.dat"))
f_k400_3.0_short <- HGSFile(paste0(boxdir, "\\K_400m_day\\short_runs\\sunny\\soil_3.0m\\temperatureTestero.pm.dat"))



k400_3.0_head <- HGSGetData(f_k400_3.0, variables = c("X", "Y", "Z", "Head"), blockNumbers = 1)
k400_3.0_d2wt <- HGSGetData(f_k400_3.0, variables = c("X", "Y", "Z", "Depth2GWT"), blockNumbers = 1)

k400_3.0_d2wt_short <- HGSGetData(f_k400_3.0_short, variables = c("X", "Y", "Z", "Depth2GWT"), blockNumbers = 1)


f_k100_3.0 <- HGSFile(paste0(boxdir, "\\K_100m_day\\sunny\\soil_3.0m\\temperatureTestero.pm.dat"))
k100_3.0_d2wt <- HGSGetData(f_k100_3.0, variables = c("X", "Y", "Z", "Depth2GWT"), blockNumbers = 1)



dim(k400_3.0_head)
plot(k400_3.0_head[1,2,17:77,1,"Head"], k400_3.0_head[1,2,17:77,1,"Z"], pch = 19, col = "gray") #, ylim = c(28,51), xlim =c(0,48))
points(k400_3.0_head[2,2,17:77,1,"Head"], k400_3.0_head[1,2,17:77,1,"Z"], pch = 19, col = hcl.colors(10)[1])
mapply(function(x,c) points(k400_3.0_head[x,2,17:77,1,"Head"], k400_3.0_head[x,2,17:77,1,"Z"], pch = 19, col = c),
       2:22,
       rep(hcl.colors(10), times = 2))


xs <- 946
plot(k400_3.0_head[xs,2,17:77,1,"Head"], k400_3.0_head[xs,2,17:77,1,"Z"], pch = 19, col = "gray", xlim = c(28,48), ylim = c(25,50)) #, ylim = c(28,51), xlim =c(0,48))
mapply(function(x,c) points(k400_3.0_head[x,2,17:77,1,"Head"], k400_3.0_head[x,2,17:77,1,"Z"], pch = ".", col = c),
       1:946,
       rep(hcl.colors(10), times = 100))

xs <- 926
c(max = max(k400_3.0_head[xs,2,17:77,1,"Head"]), min = min(k400_3.0_head[xs,2,17:77,1,"Head"]), diff = max(k400_3.0_head[xs,2,17:77,1,"Head"])-min(k400_3.0_head[xs,2,17:77,1,"Head"]))


xs <- 900:946
plot(k400_3.0_d2wt[xs,2,47,1,"Depth2GWT"]~k400_3.0_d2wt[xs,2,47,1,"X"])

plot(k400_3.0_d2wt_short[,2,47,1,"Depth2GWT"]~k400_3.0_d2wt_short[,2,47,1,"X"], type = "l", col ="gray25")
lines(k100_3.0_d2wt[,2,47,1,"Depth2GWT"]~k100_3.0_d2wt[,2,47,1,"X"], col = "red")

plot(k400_3.0_d2wt[,2,47,1,"Depth2GWT"]~k400_3.0_d2wt[,2,47,1,"X"], col = "gray25", type = "l")
lines(k400_3.0_d2wt_short[,2,47,1,"Depth2GWT"]~k400_3.0_d2wt_short[,2,47,1,"X"])
lines(k100_3.0_d2wt[,2,47,1,"Depth2GWT"]~k100_3.0_d2wt[,2,47,1,"X"], col = "red")


