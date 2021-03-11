##
## K anisotrpic versus k isotropic
##
## conclusion: no marked difference...

boxdir <-  "C:/Users/t24x137/Box/Floodplain_Shade_Box"

aniso_v <- readRDS(paste0(boxdir, "/K_400m_day/short_runs/sunny/soil_1.0m/soil_1.0m_velocity.RData"))
iso_v <- readRDS(paste0(boxdir, "/K_400m_day_isotropic/sunny/soil_1.0m/soil_1.0m_velocity.RData"))

aniso_sat <- readRDS(paste0(boxdir, "/K_400m_day/short_runs/sunny/soil_1.0m/soil_1.0m_saturation_T1.RData"))
iso_sat <- readRDS(paste0(boxdir, "/K_400m_day_isotropic/sunny/soil_1.0m/soil_1.0m_saturation_T1.RData"))

aniso_temp <- readRDS(paste0(boxdir, "/K_400m_day/short_runs/sunny/soil_1.0m/soil_1.0m_dailymeans.RData"))
iso_temp <- readRDS(paste0(boxdir, "/K_400m_day_isotropic/sunny/soil_1.0m/soil_1.0m_dailymeans.RData"))


## Velocity ##
dim(aniso_v)
plot(aniso_v[,2,17,1,"Vx"], type = "l", ylim = c(0.00015, 0.0005), main = "HZ x-velocity when K is anisotropic")
mapply(function(z,c) lines(aniso_v[,2,z,1,"Vx"], col = c),
       17:46,
       "red")
plot(iso_v[,2,17,1,"Vx"], type = "l", ylim = c(0.00015, 0.0005), main = "HZ x-velocity when K is isotropic")
mapply(function(z,c) lines(iso_v[,2,z,1,"Vx"], col = c),
       17:46,
       "blue")

## Saturation ##
dim(aniso_sat)
filled.contour(aniso_sat[,2,1,1,"X"],
               aniso_sat[1,2,17:57,1,"Z"],
               aniso_sat[,2,17:57,1,"Sat"],
               main = "Saturation when K is anisotropic")
filled.contour(iso_sat[,2,1,1,"X"],
               iso_sat[1,2,17:57,1,"Z"],
               iso_sat[,2,17:57,1,"Sat"],
               main = "Saturation when K is isotropic")

x <- 200
plot(aniso_sat[x,2,17:57,1,"Sat"], aniso_sat[1,2,17:57,1,"Z"], type = "o", col = "red",
     ylab = "Z", xlab = "Saturation")
lines(iso_sat[x,2,17:57,1,"Sat"], iso_sat[1,2,17:57,1,"Z"], type = "o", col = "blue")


## Temperature ##
dim(aniso_temp[17:57,,1])
filled.contour(aniso_sat[,2,1,1,"X"],
               aniso_sat[1,2,17:57,1,"Z"],
               t(aniso_temp[17:57,,1]),
               main = "Temperature when K is anisotropic")

filled.contour(iso_sat[,2,1,1,"X"],
               iso_sat[1,2,17:57,1,"Z"],
               t(iso_temp[17:57,,1]),
               main = "Temperature when K is isotropic")

# Check sat #
plot(aniso_sat[,2,17,1,"Sat"]~aniso_sat[,2,17,1,"X"], type = "l", ylim = c(0,1))
mapply(function(z, c) lines(aniso_sat[,2,z,1,"Sat"]~aniso_sat[,2,z,1,"X"], col = c),
       17:57,
       hcl.colors(41, "Blues 3"))


## HZ temps only ##
plot(aniso_temp[17,,1]~aniso_sat[,2,17,1,"X"], type = "l", ylim = c(4,14))
mapply(function(z, c) lines(aniso_temp[z,,1]~aniso_sat[,2,z,1,"X"], col = c),
       17:47,
       hcl.colors(31, "Lajolla", rev = T))


plot(iso_temp[17,,1]~iso_sat[,2,17,1,"X"], type = "l", ylim = c(4,14))
mapply(function(z, c) lines(iso_temp[z,,1]~iso_sat[,2,z,1,"X"], col = c),
       17:47,
       hcl.colors(31, "viridis", rev = F))


plot(aniso_temp[17,,1]~aniso_sat[,2,17,1,"X"], type = "l", ylim = c(4,14))
mapply(function(z, c) lines(aniso_temp[z,,1]~aniso_sat[,2,z,1,"X"], col = c),
       17:47,
       "red")
mapply(function(z, c) lines(iso_temp[z,,1]~iso_sat[,2,z,1,"X"], col = c),
       17:47,
       "blue")

## Temps into unsat ##
plot(aniso_temp[17,,1]~aniso_sat[,2,17,1,"X"], type = "l", ylim = c(4,14))
mapply(function(z, c) lines(aniso_temp[z,,1]~aniso_sat[,2,z,1,"X"], col = c),
       17:57,
       hcl.colors(41, "Lajolla", rev = T))


plot(iso_temp[17,,1]~iso_sat[,2,17,1,"X"], type = "l", ylim = c(4,14))
mapply(function(z, c) lines(iso_temp[z,,1]~iso_sat[,2,z,1,"X"], col = c),
       17:57,
       hcl.colors(41, "viridis", rev = F))


plot(aniso_temp[17,,1]~aniso_sat[,2,17,1,"X"], type = "l", ylim = c(4,14))
mapply(function(z, c) lines(aniso_temp[z,,1]~aniso_sat[,2,z,1,"X"], col = c),
       17:57,
       "red")
mapply(function(z, c) lines(iso_temp[z,,1]~iso_sat[,2,z,1,"X"], col = c),
       17:57,
       "blue")


