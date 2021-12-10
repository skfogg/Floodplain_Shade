###
### NEW PLOTS FOR PAPER
### USING MEACHAM UPDATED INPUTS
### DECEMBER 8
###

library(HGSReader)
library(lubridate)

####------ READ IN ALL DATA ------####
box_directory <- "C:/Users/skati/Box/Floodplain_Shade_Box/meacham_updated_results/"

kvals <- rep(c("100", "400"), each = 12)
bcvals <- rep(rep(c("sunny", "shady", "riveronly"), each = 4), times = 2)
soilvals <- rep(c("0.5", "1.0", "2.0", "3.0"), times = 6)

eachfolder <- numeric(24)
for (i in 1:24){
  eachfolder[i] <- paste0(box_directory, "K_", kvals[i], "m_day/", bcvals[i], "/", "soil_", soilvals[i], "m")
}

eachmodel <- numeric(24)
for (i in 1:24){
  eachmodel[i] <- paste0("k", kvals[i], "_", soilvals[i], "_", bcvals[i])
}

for (i in 13:24){
  assign(eachmodel[i], readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_dailymeans.RData")))
  assign(paste0(eachmodel[i], "_s"), readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_modelstructure.RData")))
  assign(paste0(eachmodel[i], "_f100"), readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_first100m.RData")))
}

localbox <- "C:/Users/skati/Box/"
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
x_to_plot <- 2:47
days_id <- t_idx
hours <- seq(1,24,by=5)
aquifer_z_idx <- 17:47
outtimes <- read.table("C:/Users/skati/Box/HGSwork/outputTimes_inputTemps/OutputTimesYr7_fulldays.txt")
x_to_plot <- 2:47

starttimes <- days_id*24 - 23
daytimes <- mapply(function(s) seq(s, s+23, by = 5),
                   starttimes)
daytimesvector <- c(daytimes[,1], daytimes[,2], daytimes[,3], daytimes[,4], daytimes[,5])
lubridays <- ymd_hms("2021-01-01 00:00:00") + (outtimes$V1[seq(1,by = 23, length.out = 24)]-(365*86400*7))


tempAcrossFP2.2 <- function(whichmodel, whichdaily, k, d2hz, bc, xrange = 2:49, colorpal = "RdYlGn", a = 0.5, ...){
  
  reverseindex <- seq(length(whichmodel[,1,1]), 1, by = -1)
  
  png(paste0(getwd(),"/plots/tempAcrossFP_k", k, "_", d2hz, "_", bc,".png"),
      height = 400*5,
      width = 800*5,
      res = 72*5)
  par(cex.lab = 1.8,
      cex.main = 2,
      cex.axis = 1.5,
      mar = c(5,5,5,1),
      bg = "white",
      fg = "black",
      col.axis = "black",
      col.main = "black",
      col.lab = "black"
      )
  layout(matrix(c(1,2,2,2), 1, 4))
  
  plot(whichdaily[xrange,2,30,1,"X"],
       rowMeans(whichdaily[xrange,2,aquifer_z_idx,1,"temp"]),
       type = "n",
       ylim = c(2,23),
       ylab = expression(paste("Temperature, ", degree, "C")),
       xlab = "Flow path length, m")
  mapply(function(t,c) lines(whichdaily[xrange,2,30,1,"X"],
                             rowMeans(whichdaily[xrange,2,aquifer_z_idx,t,"temp"]),
                             col = c,
                             lwd = 2),
         daytimesvector,
         rep(hcl.colors(5, colorpal, alpha = a), each = 5))
  
  plot(colMeans(whichmodel[reverseindex[aquifer_z_idx],,t_idx[1]]) ~ get(paste0("model_", d2hz, "_xvals")),
       type = "n",
       ylab = expression(paste("Temperature (", degree, "C)")),
       xlab = "Flow Path Length (m)",
       ylim = c(2,23))
  mapply(function(t, c) lines(colMeans(whichmodel[reverseindex[aquifer_z_idx],,t]) ~ get(paste0("model_", d2hz, "_xvals")), 
                              col = c,
                              ...),
         t_idx,
         hcl.colors(5, colorpal, alpha = a))
  dev.off()
}




mapply(tempAcrossFP2.2,
       whichmodel = listofRiveronly,
       whichdaily = listofRiveronlydaily, 
       k = kNames2,
       d2hz = modelNames2,
       bc = rep("riveronly", times = 8),
       MoreArgs = list(lwd = 3, a = 1, colorpal = "Viridis"))

mapply(tempAcrossFP2.2,
       whichmodel = listofSunny,
       whichdaily = listofSunnydaily, 
       k = kNames2,
       d2hz = modelNames2,
       bc = rep("sunny", times = 8),
       MoreArgs = list(lwd = 3, a = 1, colorpal = "Viridis"))

mapply(tempAcrossFP2.2,
       whichmodel = listofShady,
       whichdaily = listofShadydaily, 
       k = kNames2,
       d2hz = modelNames2,
       bc = rep("shady", times = 8),
       MoreArgs = list(lwd = 3, a = 1, colorpal = "Viridis"))





