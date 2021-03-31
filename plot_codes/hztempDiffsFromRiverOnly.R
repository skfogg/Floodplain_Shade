##
## Compare river only to sunny and shady HZ temperatures 
##


library(devtools)
#install_github("FluvialLandscapeLab/HGSReader")
library(HGSReader)
library(lubridate)

####------ READ IN ALL DATA ------####
localbox <- "C:/Users/skati/Box/"

kFolders <- rep(c("K_100m_day", "K_400m_day"), each = 12)
bcFolders <- rep(rep(c("sunny", "shady", "riveronly"), each = 4), times = 2)
modelFolders <- rep(c("soil_0.5m", "soil_1.0m", "soil_2.0m", "soil_3.0m"), times = 6)

kNames <- rep(c("100", "400"), each = 12)
modelNames <- rep(c("0.5", "1.0", "2.0", "3.0"), times = 6)

for(i in 1:24){
  assign(paste0(bcFolders[i], "_", kNames[i], "_", modelNames[i]),
         readRDS(paste0(localbox, "Floodplain_Shade_Box/", kFolders[i], "/", bcFolders[i], "/", modelFolders[i], "/", modelFolders[i], "_dailymeans.RData")))
}

for(i in 1:24){
  assign(paste0(bcFolders[i], "_", kNames[i], "_", modelNames[i], "_str"),
         readRDS(paste0(localbox, "Floodplain_Shade_Box/", kFolders[i], "/", bcFolders[i], "/", modelFolders[i], "/", modelFolders[i], "_modelstructure.RData")))
}

outtimes <- read.table(paste0(localbox, "HGSwork/outputTimes_inputTemps/OutputTimesYr7_fulldays.txt"))
days <- ymd_hms("2021-01-01 00:00:00") + (outtimes$V1[seq(1,by = 23, length.out = 24)]-(365*86400*7))

#-------------------------------------------------------------

model_100_1.0_xvals <- shady_100_1.0_str[,2,1,"X"]
model_400_1.0_xvals <- shady_400_1.0_str[,2,1,"X"]
model_1.0_zvals <- shady_100_1.0_str[1,2,,"Z"]
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



