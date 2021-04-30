##
## Compare river only to sunny and shady HZ temperatures 
##


library(devtools)
#install_github("FluvialLandscapeLab/HGSReader")
library(HGSReader)
library(lubridate)

####------ READ IN ALL DATA ------####
box_directory <- "C:/Users/skati/Box/Floodplain_Shade_Box/meacham_results/"
riveronly_directory <- "C:/Users/skati/Box/Floodplain_Shade_Box/iskulpaa_results/"

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


ro <- rep("riveronly", times = 8)
eachfolder_ro <- numeric(8)
for (i in 1:8){
  eachfolder_ro[i] <- paste0(riveronly_directory, "K_", kvals[i], "m_day/", ro[i], "/", "soil_", soilvals[i], "m")
}
eachmodel_ro <- numeric(8)
for (i in 1:8){
  eachmodel_ro[i] <- paste0("k", kvals[i], "_", soilvals[i], "_", ro[i])
}

eachmodel <- c(eachmodel1, eachmodel_ro)
eachfolder <- c(eachfolder1, eachfolder_ro)

for (i in 1:24){
  assign(eachmodel[i], readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_dailymeans.RData")))
  assign(paste0(eachmodel[i], "_s"), readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_modelstructure.RData")))
  assign(paste0(eachmodel[i], "_f100"), readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_first100m.RData")))
}

localbox <- "C:/Users/skati/Box/"
outtimes <- read.table(paste0(localbox, "HGSwork/outputTimes_inputTemps/OutputTimesYr7_fulldays.txt"))
days <- ymd_hms("2021-01-01 00:00:00") + (outtimes$V1[seq(1,by = 23, length.out = 24)]-(365*86400*7))

#-------------------------------------------------------------

model_100_1.0_xvals <- k100_1.0_sunny_s[,2,1,"X"]
model_400_1.0_xvals <- k400_1.0_sunny_s[,2,1,"X"]
model_1.0_zvals <- shady_100_1.0_s[1,2,,"Z"]
aquifer_z_idx <- 17:47

## Temperature differences across all flow paths ##

## *** USE riveronly model as "shady model" *** ##

tempdiffsplot2 <- function(sunnyModel, riveronlyModel, k, depth2HZ, yl = c(0,2.65),...){
  if(k == "100"){
    structure2use <- model_100_1.0_xvals
    textAtX <- 450
  }else{
    structure2use <- model_400_1.0_xvals
    textAtX <- 900
  }
  
  plot(colMeans(sunnyModel[aquifer_z_idx,,1]) - colMeans(riveronlyModel[aquifer_z_idx,,1]) ~ structure2use, 
       type = "n",
       main = "Difference in HZ temps from riveronly model: Sunny",
       ylim = yl,
       xlim = c(0, 2000),
       xlab = "Flow Path Length (m)",
       ylab = "Temperature Difference")
  mapply(function(t, c) lines(colMeans(sunnyModel[aquifer_z_idx,,t]) - colMeans(riveronlyModel[aquifer_z_idx,,t]) ~ structure2use, 
                              col = c,
                              ...),
         seq(1,24,5),
         hcl.colors(5))
  mapply(function(t, c, xadj) text(structure2use[textAtX] + xadj, 
                                   (mean(sunnyModel[aquifer_z_idx,textAtX,t]) - mean(riveronlyModel[aquifer_z_idx,textAtX,t])), 
                                   months(days[t], abbreviate = T),
                                   col = c,
                                   pos = 3,
                                   cex = 2),
         seq(1,24,5),
         hcl.colors(5),
         c(0, -150, 0, -150, 0))
  
  text(-30,yl[2]-0.5, paste0("K = ", k,"m/day, depth2HZ = ", depth2HZ, "m"), pos = 4, cex = 2)
}


savetempdiffplots2 <- function(k = "100", d2hz = "0.5", sunnymodel, riveronlyModel, yl = c(0,2.65), ...){
  
  png(paste0(getwd(),"/plots/sunny-riveronly_k", k, "_", d2hz, ".png"),
      height = 600,
      width = 800)
  par(cex.lab = 1.8,
      cex.main = 2,
      cex.axis = 1.5,
      mar = c(5,5,5,1))
  tempdiffsplot2(sunnymodel, riveronlyModel, k, d2hz, yl = yl, ...)
  dev.off()
}

kNames2 <- rep(c("100", "400"), each = 4)
modelNames2 <- modelNames[1:8]
listofSunny <- list(sunny_100_0.5, sunny_100_1.0, sunny_100_2.0, sunny_100_3.0,
                    sunny_400_0.5, sunny_400_1.0, sunny_400_2.0, sunny_400_3.0)
listofShady <- list(shady_100_0.5, shady_100_1.0, shady_100_2.0, shady_100_3.0,
                    shady_400_0.5, shady_400_1.0, shady_400_2.0, shady_400_3.0)
listofRiveronly <- list(riveronly_100_0.5, riveronly_100_1.0, riveronly_100_2.0, riveronly_100_3.0,
                        riveronly_400_0.5, riveronly_400_1.0, riveronly_400_2.0, riveronly_400_3.0)
mapply(savetempdiffplots2,
       kNames2,
       modelNames2,
       listofSunny,
       listofRiveronly,
       MoreArgs = list(yl = c(-6,6), lwd = 3))

###################################################
###################################################
#### FOR SHADY Comparison now ####

tempdiffsplot3 <- function(shadyModel, riveronlyModel, k, depth2HZ, yl = c(0,2.65),...){
  if(k == "100"){
    structure2use <- model_100_1.0_xvals
    textAtX <- 450
  }else{
    structure2use <- model_400_1.0_xvals
    textAtX <- 900
  }
  
  plot(colMeans(shadyModel[aquifer_z_idx,,1]) - colMeans(riveronlyModel[aquifer_z_idx,,1]) ~ structure2use, 
       type = "n",
       main = "Difference in HZ temps from riveronly model: Shady",
       ylim = yl,
       xlim = c(0, 2000),
       xlab = "Flow Path Length (m)",
       ylab = "Temperature Difference")
  mapply(function(t, c) lines(colMeans(shadyModel[aquifer_z_idx,,t]) - colMeans(riveronlyModel[aquifer_z_idx,,t]) ~ structure2use, 
                              col = c,
                              ...),
         seq(1,24,5),
         hcl.colors(5))
  mapply(function(t, c, xadj) text(structure2use[textAtX] + xadj, 
                                   (mean(shadyModel[aquifer_z_idx,textAtX,t]) - mean(riveronlyModel[aquifer_z_idx,textAtX,t])), 
                                   months(days[t], abbreviate = T),
                                   col = c,
                                   pos = 3,
                                   cex = 2),
         seq(1,24,5),
         hcl.colors(5),
         c(0, -150, 0, -150, 0))
  
  text(-30,yl[2]-0.5, paste0("K = ", k,"m/day, depth2HZ = ", depth2HZ, "m"), pos = 4, cex = 2)
}


savetempdiffplots3 <- function(k = "100", d2hz = "0.5", shadyModel, riveronlyModel, yl = c(0,2.65), ...){
  
  png(paste0(getwd(),"/plots/shady-riveronly_k", k, "_", d2hz, ".png"),
      height = 600,
      width = 800)
  par(cex.lab = 1.8,
      cex.main = 2,
      cex.axis = 1.5,
      mar = c(5,5,5,1))
  tempdiffsplot3(shadyModel, riveronlyModel, k, d2hz, yl = yl, ...)
  dev.off()
}

kNames2 <- rep(c("100", "400"), each = 4)
modelNames2 <- modelNames[1:8]
listofSunny <- list(sunny_100_0.5, sunny_100_1.0, sunny_100_2.0, sunny_100_3.0,
                    sunny_400_0.5, sunny_400_1.0, sunny_400_2.0, sunny_400_3.0)
listofShady <- list(shady_100_0.5, shady_100_1.0, shady_100_2.0, shady_100_3.0,
                    shady_400_0.5, shady_400_1.0, shady_400_2.0, shady_400_3.0)
listofRiveronly <- list(riveronly_100_0.5, riveronly_100_1.0, riveronly_100_2.0, riveronly_100_3.0,
                        riveronly_400_0.5, riveronly_400_1.0, riveronly_400_2.0, riveronly_400_3.0)
mapply(savetempdiffplots3,
       kNames2,
       modelNames2,
       listofShady,
       listofRiveronly,
       MoreArgs = list(yl = c(-6,6), lwd = 3))



