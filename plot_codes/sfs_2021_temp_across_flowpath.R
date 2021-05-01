library(HGSReader)
library(lubridate)

####------ READ IN ALL DATA ------####
box_directory <- "C:/Users/t24x137/Box/Floodplain_Shade_Box/meacham_results/"
riveronly_directory <- "C:/Users/t24x137/Box/Floodplain_Shade_Box/iskulpaa_results/"

kvals <- rep(c("100", "400"), each = 8)
bcvals <- rep(rep(c("sunny", "shady"), each = 4), times = 2)
soilvals <- rep(c("0.5", "1.0", "2.0", "3.0"), times = 4)

eachfolder1 <- numeric(16)
for (i in 1:16){
  eachfolder1[i] <- paste0(box_directory, "K_", kvals[i], "m_day/", bcvals[i], "/", "soil_", soilvals[i], "m")
}

eachmodel1 <- numeric(16)
for (i in 1:16){
  eachmodel1[i] <- paste0("k", kvals[i], "_", soilvals[i], "_", bcvals[i])
}

kvals_ro <- rep(c("100", "400"), each = 4)
ro <- rep("riveronly", times = 8)
eachfolder_ro <- numeric(8)
for (i in 1:8){
  eachfolder_ro[i] <- paste0(riveronly_directory, "K_", kvals_ro[i], "m_day/", ro[i], "/", "soil_", soilvals[i], "m")
}
eachmodel_ro <- numeric(8)
for (i in 1:8){
  eachmodel_ro[i] <- paste0("k", kvals_ro[i], "_", soilvals[i], "_", ro[i])
}

eachmodel <- c(eachmodel1, eachmodel_ro)
eachfolder <- c(eachfolder1, eachfolder_ro)

for (i in 1:24){
  #assign(eachmodel[i], readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_dailymeans.RData")))
  #assign(paste0(eachmodel[i], "_s"), readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_modelstructure.RData")))
  assign(paste0(eachmodel[i], "_f100"), readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_first100m.RData")))
}

localbox <- "C:/Users/t24x137/Box/"
outtimes <- read.table(paste0(localbox, "HGSwork/outputTimes_inputTemps/OutputTimesYr7_fulldays.txt"))
days <- ymd_hms("2021-01-01 00:00:00") + (outtimes$V1[seq(1,by = 23, length.out = 24)]-(365*86400*7))

#-----------#----------#-----------#------------#-------------#--------------#----------------#

model_0.5_xvals <- k400_0.5_sunny_s[,2,1,"X"]
model_1.0_xvals <- k400_1.0_sunny_s[,2,1,"X"]
model_2.0_xvals <- k400_2.0_sunny_s[,2,1,"X"]
model_3.0_xvals <- k400_3.0_sunny_s[,2,1,"X"]

model_1.0_zvals <- k100_1.0_shady_s[1,2,,"Z"]
aquifer_z_idx <- 17:47
t_idx <- seq(2,24,5)

kNames2 <- rep(c("100", "400"), each = 4)
modelNames <- rep(c("0.5", "1.0", "2.0", "3.0"), times = 6)
modelNames2 <- modelNames[1:8]
listofSunny <- list(k100_0.5_sunny, k100_1.0_sunny, k100_2.0_sunny, k100_3.0_sunny,
                    k400_0.5_sunny, k400_1.0_sunny, k400_2.0_sunny, k400_3.0_sunny)
listofSunnydaily <- list(k100_0.5_sunny_f100, k100_1.0_sunny_f100, k100_2.0_sunny_f100, k100_3.0_sunny_f100,
                    k400_0.5_sunny_f100, k400_1.0_sunny_f100, k400_2.0_sunny_f100, k400_3.0_sunny_f100)

listofShady <- list(k100_0.5_shady, k100_1.0_shady, k100_2.0_shady, k100_3.0_shady,
                    k400_0.5_shady, k400_1.0_shady, k400_2.0_shady, k400_3.0_shady)
listofShadydaily <- list(k100_0.5_shady_f100, k100_1.0_shady_f100, k100_2.0_shady_f100, k100_3.0_shady_f100,
                         k400_0.5_shady_f100, k400_1.0_shady_f100, k400_2.0_shady_f100, k400_3.0_shady_f100)

listofRiveronly <- list(k100_0.5_riveronly, k100_1.0_riveronly, k100_2.0_riveronly, k100_3.0_riveronly,
                        k400_0.5_riveronly, k400_1.0_riveronly, k400_2.0_riveronly, k400_3.0_riveronly)
listofRiveronlydaily <- list(k100_0.5_riveronly_f100, k100_1.0_riveronly_f100, k100_2.0_riveronly_f100, k100_3.0_riveronly_f100,
                             k400_0.5_riveronly_f100, k400_1.0_riveronly_f100, k400_2.0_riveronly_f100, k400_3.0_riveronly_f100)

tempAcrossFP <- function(whichmodel, k, d2hz, bc, colorpal = hcl.colors(5, "RdYlGn"), ...){
  png(paste0(getwd(),"/plots/tempAcrossFP_k", k, "_", d2hz, "_", bc,".png"),
      height = 450,
      width = 800)
  par(cex.lab = 1.8,
      cex.main = 2,
      cex.axis = 1.5,
      mar = c(5,5,5,1),
      bg = "black",
      fg = "ghostwhite",
      col.axis = "ghostwhite",
      col.main = "ghostwhite",
      col.lab = "ghostwhite")
  plot(colMeans(whichmodel[aquifer_z_idx,,t_idx[1]]) ~ get(paste0("model_", d2hz, "_xvals")),
       type = "l",
       ylab = expression(paste("Temperature (", degree, "C)")),
       xlab = "Flow Path Length (m)",
       ylim = c(3,19))
  mapply(function(t, c) lines(colMeans(whichmodel[aquifer_z_idx,,t]) ~ get(paste0("model_", d2hz, "_xvals")), 
                              col = c,
                              ...),
         t_idx,
         colorpal)
  dev.off()
}

mapply(tempAcrossFP,
       whichmodel = listofRiveronly,
       k = kNames2,
       d2hz = modelNames2,
       bc = rep("riveronly", times = 8),
       MoreArgs = list(lwd = 3))

mapply(tempAcrossFP,
       whichmodel = listofSunny,
       k = kNames2,
       d2hz = modelNames2,
       bc = rep("sunny", times = 8),
       MoreArgs = list(lwd = 3))

mapply(tempAcrossFP,
       whichmodel = listofShady,
       k = kNames2,
       d2hz = modelNames2,
       bc = rep("shady", times = 8),
       MoreArgs = list(lwd = 3))

x_to_plot <- 2:47
days_id <- t_idx
hours <- seq(1,24,by=5)
aquifer_z_idx <- 17:47
outtimes <- read.table("C:/Users/t24x137/Box/HGSwork/outputTimes_inputTemps/OutputTimesYr7_fulldays.txt")
x_to_plot <- 2:47

starttimes <- days_id*24 - 23
daytimes <- mapply(function(s) seq(s, s+23, by = 5),
                   starttimes)
daytimesvector <- c(daytimes[,1], daytimes[,2], daytimes[,3], daytimes[,4], daytimes[,5])
lubridays <- ymd_hms("2021-01-01 00:00:00") + (outtimes$V1[seq(1,by = 23, length.out = 24)]-(365*86400*7))



tempAcrossFP2 <- function(whichmodel, whichdaily, k, d2hz, bc, xrange = 2:49, colorpal = "RdYlGn", ...){
  png(paste0(getwd(),"/plots/tempAcrossFP_k", k, "_", d2hz, "_", bc,".png"),
      height = 400,
      width = 800)
  par(cex.lab = 1.8,
      cex.main = 2,
      cex.axis = 1.5,
      mar = c(5,5,5,1),
      bg = "black",
      fg = "ghostwhite",
      col.axis = "ghostwhite",
      col.main = "ghostwhite",
      col.lab = "ghostwhite")
  layout(matrix(c(1,2,2,2), 1, 4))
  
  plot(whichdaily[xrange,2,30,1,"X"],
       rowMeans(whichdaily[xrange,2,aquifer_z_idx,1,"temp"]),
       type = "n",
       ylim = c(3,21),
       ylab = expression(paste("Temperature, ", degree, "C")),
       xlab = "Flow path length, m")
  mapply(function(t,c) lines(whichdaily[xrange,2,30,1,"X"],
                             rowMeans(whichdaily[xrange,2,aquifer_z_idx,t,"temp"]),
                             col = c,
                             lwd = 2),
         daytimesvector,
         rep(hcl.colors(5, colorpal), each = 5))
  
  plot(colMeans(whichmodel[aquifer_z_idx,,t_idx[1]]) ~ get(paste0("model_", d2hz, "_xvals")),
       type = "l",
       ylab = expression(paste("Temperature (", degree, "C)")),
       xlab = "Flow Path Length (m)",
       ylim = c(3,21))
  mapply(function(t, c) lines(colMeans(whichmodel[aquifer_z_idx,,t]) ~ get(paste0("model_", d2hz, "_xvals")), 
                              col = c,
                              ...),
         t_idx,
         hcl.colors(5, colorpal))
  dev.off()
}




mapply(tempAcrossFP2,
       whichmodel = listofRiveronly,
       whichdaily = listofRiveronlydaily, 
       k = kNames2,
       d2hz = modelNames2,
       bc = rep("riveronly", times = 8),
       MoreArgs = list(lwd = 3))

mapply(tempAcrossFP2,
       whichmodel = listofSunny,
       whichdaily = listofSunnydaily, 
       k = kNames2,
       d2hz = modelNames2,
       bc = rep("sunny", times = 8),
       MoreArgs = list(lwd = 3))

mapply(tempAcrossFP2,
       whichmodel = listofShady,
       whichdaily = listofShadydaily, 
       k = kNames2,
       d2hz = modelNames2,
       bc = rep("shady", times = 8),
       MoreArgs = list(lwd = 3))



