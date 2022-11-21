##
## Calculate amplitude damping (annual signal damping)
##
library(lubridate)

####------ READ IN ALL DATA ------####
box_directory <- "C:/Users/skati/OneDrive - Montana State University/BoxMigratedData/Floodplain_Shade_Box/meacham_updated_results/"

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

for (i in 1:24){
  assign(eachmodel[i], readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_dailymeans.RData")))
  assign(paste0(eachmodel[i], "_s"), readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_modelstructure.RData")))
}

listofSunny <- as.list(k100_0.5_sunny, k100_1.0_sunny, k100_2.0_sunny, k100_3.0_sunny,
                    k400_0.5_sunny, k400_1.0_sunny, k400_2.0_sunny, k400_3.0_sunny)
listofShady <- list(k100_0.5_shady, k100_1.0_shady, k100_2.0_shady, k100_3.0_shady,
                    k400_0.5_shady, k400_1.0_shady, k400_2.0_shady, k400_3.0_shady)
listofRiverOnly <- list(k100_0.5_riveronly, k100_1.0_riveronly, k100_2.0_riveronly, k100_3.0_riveronly,
                        k400_0.5_riveronly, k400_1.0_riveronly, k400_2.0_riveronly, k400_3.0_riveronly) 


### ---- Time Data --- ###
outtimes <- read.table("C:/Users/skati/OneDrive - Montana State University/BoxMigratedData/HGSwork/outputTimes_inputTemps/OutputTimesYr7_fulldays.txt")
lubridays <- ymd_hms("2021-01-01 00:00:00") + (outtimes$V1[seq(1,by = 23, length.out = 24)]-(365*86400*7))

# x-values (should be the same for all models)
x_vals <- k400_1.0_riveronly_s[,1,1,"X"]


calc_amps <- function(x){
  if(dim(x)[1] == 52){
    HZ_idx <- 6:35
  }
  if(dim(x)[1] == 57){
    HZ_idx <- 11:40
  }
  if(dim(x)[1] == 67){
    HZ_idx <- 21:50
  }
  if(dim(x)[1] == 77){
    HZ_idx <- 31:60
  }
  
  ## Calculate mean HZ temp at each time step to get 1 averaged annual signal
  hz_mean <- matrix(data = 0, nrow = 24, ncol = 413)
  for(i in 1:413){
    hz_mean[,i] <- colMeans(x[HZ_idx, i, ])
  }
  amps <- length(413)
  for(i in 1:413){
    amps[i] <-  (max(hz_mean[,i]) - min(hz_mean[,i]))/2
  }
  return(amps)
}

calc_amps_daily <- function(x){
  HZ_idx <- 18:47
  day_starts <- seq(1,by = 23, length.out = 24)
  day_ends <- seq(23, by = 23, length.out = 24)
  
  hz_mean <- matrix(data = 0, nrow = 576, ncol = 95)
  for(i in 1:95){
    hz_mean[,i] <- colMeans(x[i, 1, HZ_idx, , "temp"])
  }
  hz_amps <- matrix(data = 0, nrow = 24, ncol = 95)
  for(i in 1:95){
    for(j in 1:length(day_starts)){
    hz_amps[j,i] <-  (max(hz_mean[day_starts[j]:day_ends[j],i]) - min(hz_mean[day_starts[j]:day_ends[j],i]))/2
    }
  }
  return(hz_amps)
}

test <- calc_amps_daily(k100_3.0_riveronly_100m)
plot(test[1,1:41], type = "l", ylim = c(0,4))
mapply(function(t,c) lines(test[t,1:41], col = c, lwd = 2),
       t = 1:24,
       c = hcl.colors(24, "Earth"))

names(allmodels[[1]])

k100_0.5_riveronly_amps <- calc_amps(k100_0.5_riveronly)
k100_1.0_riveronly_amps <- calc_amps(k100_1.0_riveronly)
k100_2.0_riveronly_amps <- calc_amps(k100_2.0_riveronly)
k100_3.0_riveronly_amps <- calc_amps(k100_3.0_riveronly)

k400_0.5_riveronly_amps <- calc_amps(k400_0.5_riveronly)
k400_1.0_riveronly_amps <- calc_amps(k400_1.0_riveronly)
k400_2.0_riveronly_amps <- calc_amps(k400_2.0_riveronly)
k400_3.0_riveronly_amps <- calc_amps(k400_3.0_riveronly)

k100_0.5_shady_amps <- calc_amps(k100_0.5_shady)
k100_1.0_shady_amps <- calc_amps(k100_1.0_shady)
k100_2.0_shady_amps <- calc_amps(k100_2.0_shady)
k100_3.0_shady_amps <- calc_amps(k100_3.0_shady)

k400_0.5_shady_amps <- calc_amps(k400_0.5_shady)
k400_1.0_shady_amps <- calc_amps(k400_1.0_shady)
k400_2.0_shady_amps <- calc_amps(k400_2.0_shady)
k400_3.0_shady_amps <- calc_amps(k400_3.0_shady)

k100_0.5_sunny_amps <- calc_amps(k100_0.5_sunny)
k100_1.0_sunny_amps <- calc_amps(k100_1.0_sunny)
k100_2.0_sunny_amps <- calc_amps(k100_2.0_sunny)
k100_3.0_sunny_amps <- calc_amps(k100_3.0_sunny)

k400_0.5_sunny_amps <- calc_amps(k400_0.5_sunny)
k400_1.0_sunny_amps <- calc_amps(k400_1.0_sunny)
k400_2.0_sunny_amps <- calc_amps(k400_2.0_sunny)
k400_3.0_sunny_amps <- calc_amps(k400_3.0_sunny)


cols4 <- hcl.colors(5, "BrwnYl", rev = T)[2:5]
lw <- 3

#### ---- K100 ---- ####
png("plots/annual_amp_damp_k100.png", height = 900*5, width = 600*5, res = 72*5)
par(mfrow = c(3,1), mar = c(2,2,1,1), oma = c(3,3,0,0),
    cex.axis = 1.5)
## --riveronly --##
plot(k100_0.5_riveronly_amps ~ x_vals, ylim =c(0,10), type = "l", col = cols4[1], lwd = lw) 
lines(k100_1.0_riveronly_amps ~ x_vals, col = cols4[2], lwd = lw)
lines(k100_2.0_riveronly_amps ~ x_vals, col = cols4[3], lwd = lw)
lines(k100_3.0_riveronly_amps ~ x_vals, col = cols4[4], lwd = lw)
legend("topright", c("0.5", "1.0", "2.0", "3.0"), lwd = lw, col = cols4[1:4] , cex = 1.4, title = "Depth to Water Table")

## -- shady -- ##
plot(k100_0.5_shady_amps ~ x_vals, type = "l", col = cols4[1], ylim = c(0,10), lwd = lw)
lines(k100_1.0_shady_amps ~ x_vals, col = cols4[2], lwd = lw)
lines(k100_2.0_shady_amps ~ x_vals, col = cols4[3], lwd = lw)
lines(k100_3.0_shady_amps ~ x_vals, col = cols4[4], lwd = lw)

## -- sunny -- ##
plot(k100_0.5_sunny_amps ~ x_vals, type = "l", col = cols4[1], ylim = c(0,10), lwd = lw)
lines(k100_1.0_sunny_amps ~ x_vals, col = cols4[2], lwd = lw)
lines(k100_2.0_sunny_amps ~ x_vals, col = cols4[3], lwd = lw)
lines(k100_3.0_sunny_amps ~ x_vals, col = cols4[4], lwd = lw)

mtext(expression(paste("Annual Temperature Amplitude ( ", degree, "C)")), side = 2, 
      line = 1, outer = T, cex = 1.4)
mtext("Flow Path Length (m)", side = 1, line = 1, outer = T, cex = 1.4)
dev.off()

#### ---- K400 ---- ####
png("plots/annual_amp_damp_k400.png", height = 900*5, width = 600*5, res = 72*5)
par(mfrow = c(3,1), mar = c(2,2,1,1), oma = c(3,3,0,0),
    cex.axis = 1.5)
par(mfrow = c(3,1), mar = c(2,2,1,1))
## --riveronly --##
plot(k400_0.5_riveronly_amps ~ x_vals, ylim =c(0,10), type = "l", col = cols4[1], lwd = lw) 
lines(k400_1.0_riveronly_amps ~ x_vals, col = cols4[2], lwd = lw)
lines(k400_2.0_riveronly_amps ~ x_vals, col = cols4[3], lwd = lw)
lines(k400_3.0_riveronly_amps ~ x_vals, col = cols4[4], lwd = lw)
legend("topright", c("0.5", "1.0", "2.0", "3.0"), lwd = lw, col = cols4[1:4] , cex = 1.4, title = "Depth to Water Table")

## -- shady -- ##
plot(k400_0.5_shady_amps ~ x_vals, type = "l", col = cols4[1], ylim = c(0,10), lwd = lw)
lines(k400_1.0_shady_amps ~ x_vals, col = cols4[2], lwd = lw)
lines(k400_2.0_shady_amps ~ x_vals, col = cols4[3], lwd = lw)
lines(k400_3.0_shady_amps ~ x_vals, col = cols4[4], lwd = lw)

## -- sunny -- ##
plot(k400_0.5_sunny_amps ~ x_vals, type = "l", col = cols4[1], ylim = c(0,10), lwd = lw)
lines(k400_1.0_sunny_amps ~ x_vals, col = cols4[2], lwd = lw)
lines(k400_2.0_sunny_amps ~ x_vals, col = cols4[3], lwd = lw)
lines(k400_3.0_sunny_amps ~ x_vals, col = cols4[4], lwd = lw)

mtext(expression(paste("Annual Temperature Amplitude ( ", degree, "C)")), side = 2, 
      line = 1, outer = T, cex = 1.4)
mtext("Flow Path Length (m)", side = 1, line = 1, outer = T, cex = 1.4)
dev.off()


cols4 <- hcl.colors(5, "BrwnYl", rev = T)[2:5]
lw <- 3

#### ---- K100: CHANGE in AMP ---- ####
png("plots/annual_amp_damp_change_k100.png", height = 900*5, width = 600*5, res = 72*5)
par(mfrow = c(3,1), mar = c(2,2,1,1), oma = c(3,3,0,0),
    cex.axis = 1.5)
## --riveronly --##
plot(I(k100_0.5_riveronly_amps - k100_0.5_riveronly_amps[1]) ~ x_vals, ylim =c(-10,0), type = "l", col = cols4[1], lwd = lw) 
lines(I(k100_1.0_riveronly_amps - k100_1.0_riveronly_amps[1]) ~ x_vals, col = cols4[2], lwd = lw)
lines(I(k100_2.0_riveronly_amps - k100_2.0_riveronly_amps[1]) ~ x_vals, col = cols4[3], lwd = lw)
lines(I(k100_3.0_riveronly_amps - k100_3.0_riveronly_amps[1]) ~ x_vals, col = cols4[4], lwd = lw)
legend("topright", c("0.5", "1.0", "2.0", "3.0"), lwd = lw, col = cols4[1:4] , cex = 1.4, title = "Depth to Water Table")

## -- shady -- ##
plot(I(k100_0.5_shady_amps - k100_0.5_shady_amps[1]) ~ x_vals, type = "l", col = cols4[1], ylim =c(-10,0), lwd = lw)
lines(I(k100_1.0_shady_amps - k100_1.0_shady_amps[1]) ~ x_vals, col = cols4[2], lwd = lw)
lines(I(k100_2.0_shady_amps - k100_2.0_shady_amps[1]) ~ x_vals, col = cols4[3], lwd = lw)
lines(I(k100_3.0_shady_amps - k100_3.0_shady_amps[1]) ~ x_vals, col = cols4[4], lwd = lw)

## -- sunny -- ##
plot(I(k100_0.5_sunny_amps - k100_0.5_sunny_amps[1]) ~ x_vals, type = "l", col = cols4[1], ylim =c(-10,0), lwd = lw)
lines(I(k100_1.0_sunny_amps - k100_1.0_sunny_amps[1]) ~ x_vals, col = cols4[2], lwd = lw)
lines(I(k100_2.0_sunny_amps - k100_2.0_sunny_amps[1]) ~ x_vals, col = cols4[3], lwd = lw)
lines(I(k100_3.0_sunny_amps - k100_3.0_sunny_amps[1]) ~ x_vals, col = cols4[4], lwd = lw)

mtext(expression(paste("Annual Temperature Amplitude ( ", degree, "C)")), side = 2, 
      line = 1, outer = T, cex = 1.4)
mtext("Flow Path Length (m)", side = 1, line = 1, outer = T, cex = 1.4)
dev.off()

#### ---- K400: CHANGE in AMP ---- ####
png("plots/annual_amp_damp_change_k400.png", height = 900*5, width = 600*5, res = 72*5)
par(mfrow = c(3,1), mar = c(2,2,1,1), oma = c(3,3,0,0),
    cex.axis = 1.5)
par(mfrow = c(3,1), mar = c(2,2,1,1))
## --riveronly --##
plot(I(k400_0.5_riveronly_amps - k400_0.5_riveronly_amps[1]) ~ x_vals, ylim =c(-10,0), type = "l", col = cols4[1], lwd = lw) 
lines(I(k400_1.0_riveronly_amps - k400_1.0_riveronly_amps[1]) ~ x_vals, col = cols4[2], lwd = lw)
lines(I(k400_2.0_riveronly_amps - k400_2.0_riveronly_amps[1]) ~ x_vals, col = cols4[3], lwd = lw)
lines(I(k400_3.0_riveronly_amps - k400_3.0_riveronly_amps[1]) ~ x_vals, col = cols4[4], lwd = lw)
legend("topright", c("0.5", "1.0", "2.0", "3.0"), lwd = lw, col = cols4[1:4] , cex = 1.4, title = "Depth to Water Table")

## -- shady -- ##
plot(I(k400_0.5_shady_amps - k400_0.5_shady_amps[1]) ~ x_vals, type = "l", col = cols4[1], ylim =c(-10,0), lwd = lw)
lines(I(k400_1.0_shady_amps - k400_1.0_shady_amps[1]) ~ x_vals, col = cols4[2], lwd = lw)
lines(I(k400_2.0_shady_amps - k400_2.0_shady_amps[1]) ~ x_vals, col = cols4[3], lwd = lw)
lines(I(k400_3.0_shady_amps - k400_3.0_shady_amps[1]) ~ x_vals, col = cols4[4], lwd = lw)

## -- sunny -- ##
plot(I(k400_0.5_sunny_amps - k400_0.5_sunny_amps[1]) ~ x_vals, type = "l", col = cols4[1], ylim =c(-10,0), lwd = lw)
lines(I(k400_1.0_sunny_amps - k400_1.0_sunny_amps[1]) ~ x_vals, col = cols4[2], lwd = lw)
lines(I(k400_2.0_sunny_amps - k400_2.0_sunny_amps[1]) ~ x_vals, col = cols4[3], lwd = lw)
lines(I(k400_3.0_sunny_amps - k400_3.0_sunny_amps[1]) ~ x_vals, col = cols4[4], lwd = lw)

mtext(expression(paste("Annual Temperature Amplitude ( ", degree, "C)")), side = 2, 
      line = 1, outer = T, cex = 1.4)
mtext("Flow Path Length (m)", side = 1, line = 1, outer = T, cex = 1.4)
dev.off()


#### ---- K100: AMP RATIO ---- ####
png("plots/annual_amp_ratio_k100.png", height = 900*5, width = 600*5, res = 72*5)
par(mfrow = c(3,1), mar = c(2,2,1,1), oma = c(3,3,0,0),
    cex.axis = 1.5)
## --riveronly --##
plot(I(k100_0.5_riveronly_amps/k100_0.5_riveronly_amps[1]) ~ x_vals, ylim =c(0,1.3), type = "l", col = cols4[1], lwd = lw) 
lines(I(k100_1.0_riveronly_amps/k100_1.0_riveronly_amps[1]) ~ x_vals, col = cols4[2], lwd = lw)
lines(I(k100_2.0_riveronly_amps/k100_2.0_riveronly_amps[1]) ~ x_vals, col = cols4[3], lwd = lw)
lines(I(k100_3.0_riveronly_amps/k100_3.0_riveronly_amps[1]) ~ x_vals, col = cols4[4], lwd = lw)
legend("topright", c("0.5", "1.0", "2.0", "3.0"), lwd = lw, col = cols4[1:4] , cex = 1.4, title = "Depth to Water Table")

## -- shady -- ##
plot(I(k100_0.5_shady_amps/k100_0.5_shady_amps[1]) ~ x_vals, type = "l", col = cols4[1], ylim =c(0,1.3), lwd = lw)
lines(I(k100_1.0_shady_amps/k100_1.0_shady_amps[1]) ~ x_vals, col = cols4[2], lwd = lw)
lines(I(k100_2.0_shady_amps/k100_2.0_shady_amps[1]) ~ x_vals, col = cols4[3], lwd = lw)
lines(I(k100_3.0_shady_amps/k100_3.0_shady_amps[1]) ~ x_vals, col = cols4[4], lwd = lw)

## -- sunny -- ##
plot(I(k100_0.5_sunny_amps/k100_0.5_sunny_amps[1]) ~ x_vals, type = "l", col = cols4[1], ylim =c(0,1.3), lwd = lw)
lines(I(k100_1.0_sunny_amps/k100_1.0_sunny_amps[1]) ~ x_vals, col = cols4[2], lwd = lw)
lines(I(k100_2.0_sunny_amps/k100_2.0_sunny_amps[1]) ~ x_vals, col = cols4[3], lwd = lw)
lines(I(k100_3.0_sunny_amps/k100_3.0_sunny_amps[1]) ~ x_vals, col = cols4[4], lwd = lw)

mtext("Amplitude Ratio", side = 2, 
      line = 1, outer = T, cex = 1.4)
mtext("Flow Path Length (m)", side = 1, line = 1, outer = T, cex = 1.4)
dev.off()

#### ---- K400: AMP RATIO ---- ####
png("plots/annual_amp_ratio_k400.png", height = 900*5, width = 600*5, res = 72*5)
par(mfrow = c(3,1), mar = c(2,2,1,1), oma = c(3,3,0,0),
    cex.axis = 1.5)
par(mfrow = c(3,1), mar = c(2,2,1,1))
## --riveronly --##
plot(I(k400_0.5_riveronly_amps/k400_0.5_riveronly_amps[1]) ~ x_vals, ylim =c(0,1.3), type = "l", col = cols4[1], lwd = lw) 
lines(I(k400_1.0_riveronly_amps/k400_1.0_riveronly_amps[1]) ~ x_vals, col = cols4[2], lwd = lw)
lines(I(k400_2.0_riveronly_amps/k400_2.0_riveronly_amps[1]) ~ x_vals, col = cols4[3], lwd = lw)
lines(I(k400_3.0_riveronly_amps/k400_3.0_riveronly_amps[1]) ~ x_vals, col = cols4[4], lwd = lw)
legend("topright", c("0.5", "1.0", "2.0", "3.0"), lwd = lw, col = cols4[1:4] , cex = 1.4, title = "Depth to Water Table")

## -- shady -- ##
plot(I(k400_0.5_shady_amps/k400_0.5_shady_amps[1]) ~ x_vals, type = "l", col = cols4[1], ylim =c(0,1.3), lwd = lw)
lines(I(k400_1.0_shady_amps/k400_1.0_shady_amps[1]) ~ x_vals, col = cols4[2], lwd = lw)
lines(I(k400_2.0_shady_amps/k400_2.0_shady_amps[1]) ~ x_vals, col = cols4[3], lwd = lw)
lines(I(k400_3.0_shady_amps/k400_3.0_shady_amps[1]) ~ x_vals, col = cols4[4], lwd = lw)

## -- sunny -- ##
plot(I(k400_0.5_sunny_amps/k400_0.5_sunny_amps[1]) ~ x_vals, type = "l", col = cols4[1], ylim =c(0,1.3), lwd = lw)
lines(I(k400_1.0_sunny_amps/k400_1.0_sunny_amps[1]) ~ x_vals, col = cols4[2], lwd = lw)
lines(I(k400_2.0_sunny_amps/k400_2.0_sunny_amps[1]) ~ x_vals, col = cols4[3], lwd = lw)
lines(I(k400_3.0_sunny_amps/k400_3.0_sunny_amps[1]) ~ x_vals, col = cols4[4], lwd = lw)

mtext("Amplitude Ratio", side = 2, 
      line = 1, outer = T, cex = 1.4)
mtext("Flow Path Length (m)", side = 1, line = 1, outer = T, cex = 1.4)
dev.off()





col2 <- hcl.colors(4, "YlOrRd")
#### ---- K100: AMP RATIO By thickness ---- ####
png("plots/annual_amp_ratio_k100_2.png", height = 900*5, width = 600*5, res = 72*5)
par(mfrow = c(4,1), mar = c(2,2,1,1), oma = c(3,3,0,0),
    cex.axis = 1.5)
## -- 0.5 --##
plot(I(k100_0.5_riveronly_amps/k100_0.5_riveronly_amps[1]) ~ x_vals, ylim =c(0,1.3), type = "l", 
     col = "dodgerblue", lwd = lw) 
lines(I(k100_0.5_shady_amps/k100_0.5_shady_amps[1]) ~ x_vals, col = col2[3], lwd = lw)
lines(I(k100_0.5_sunny_amps/k100_0.5_sunny_amps[1]) ~ x_vals, col = col2[1], lwd = lw)

## -- 1.0 -- ##
plot(I(k100_1.0_riveronly_amps/k100_1.0_riveronly_amps[1]) ~ x_vals, ylim =c(0,1.3), type = "l", 
     col = "dodgerblue", lwd = lw) 
lines(I(k100_1.0_shady_amps/k100_1.0_shady_amps[1]) ~ x_vals, col = col2[3], lwd = lw)
lines(I(k100_1.0_sunny_amps/k100_1.0_sunny_amps[1]) ~ x_vals, col = col2[1], lwd = lw)

## -- 2.0 -- ##
plot(I(k100_2.0_riveronly_amps/k100_2.0_riveronly_amps[1]) ~ x_vals, ylim =c(0,1.3), type = "l", 
     col = "dodgerblue", lwd = lw) 
lines(I(k100_2.0_shady_amps/k100_2.0_shady_amps[1]) ~ x_vals, col = col2[3], lwd = lw)
lines(I(k100_2.0_sunny_amps/k100_2.0_sunny_amps[1]) ~ x_vals, col = col2[1], lwd = lw)
legend("topright", c("river-only", "shady", "sunny"), lwd = lw, 
       col = c("dodgerblue", col2[3], col2[1]) , cex = 1.4, title = "Scenario")

## -- 3.0 -- ##
plot(I(k100_3.0_riveronly_amps/k100_3.0_riveronly_amps[1]) ~ x_vals, ylim =c(0,1.3), type = "l", 
     col = "dodgerblue", lwd = lw) 
lines(I(k100_3.0_shady_amps/k100_3.0_shady_amps[1]) ~ x_vals, col = col2[3], lwd = lw)
lines(I(k100_3.0_sunny_amps/k100_3.0_sunny_amps[1]) ~ x_vals, col = col2[1], lwd = lw)

mtext("Amplitude Ratio", side = 2, 
      line = 1, outer = T, cex = 1.4)
mtext("Flow Path Length (m)", side = 1, line = 1, outer = T, cex = 1.4)
dev.off()

#### ---- K400: AMP RATIO By thickness ---- ####
png("plots/annual_amp_ratio_k400_2.png", height = 900*5, width = 600*5, res = 72*5)
par(mfrow = c(4,1), mar = c(2,2,1,1), oma = c(3,3,0,0),
    cex.axis = 1.5)
## -- 0.5 --##
plot(I(k400_0.5_riveronly_amps/k400_0.5_riveronly_amps[1]) ~ x_vals, ylim =c(0,1.3), type = "l", 
     col = "dodgerblue", lwd = lw) 
lines(I(k400_0.5_shady_amps/k400_0.5_shady_amps[1]) ~ x_vals, col = col2[3], lwd = lw)
lines(I(k400_0.5_sunny_amps/k400_0.5_sunny_amps[1]) ~ x_vals, col = col2[1], lwd = lw)

## -- 1.0 -- ##
plot(I(k400_1.0_riveronly_amps/k400_1.0_riveronly_amps[1]) ~ x_vals, ylim =c(0,1.3), type = "l", 
     col = "dodgerblue", lwd = lw) 
lines(I(k400_1.0_shady_amps/k400_1.0_shady_amps[1]) ~ x_vals, col = col2[3], lwd = lw)
lines(I(k400_1.0_sunny_amps/k400_1.0_sunny_amps[1]) ~ x_vals, col = col2[1], lwd = lw)

## -- 2.0 -- ##
plot(I(k400_2.0_riveronly_amps/k400_2.0_riveronly_amps[1]) ~ x_vals, ylim =c(0,1.3), type = "l", 
     col = "dodgerblue", lwd = lw) 
lines(I(k400_2.0_shady_amps/k400_2.0_shady_amps[1]) ~ x_vals, col = col2[3], lwd = lw)
lines(I(k400_2.0_sunny_amps/k400_2.0_sunny_amps[1]) ~ x_vals, col = col2[1], lwd = lw)
legend("topright", c("river-only", "shady", "sunny"), lwd = lw, 
       col = c("dodgerblue", col2[3], col2[1]) , cex = 1.4, title = "Scenario")


## -- 3.0 -- ##
plot(I(k400_3.0_riveronly_amps/k400_3.0_riveronly_amps[1]) ~ x_vals, ylim =c(0,1.3), type = "l", 
     col = "dodgerblue", lwd = lw) 
lines(I(k400_3.0_shady_amps/k400_3.0_shady_amps[1]) ~ x_vals, col = col2[3], lwd = lw)
lines(I(k400_3.0_sunny_amps/k400_3.0_sunny_amps[1]) ~ x_vals, col = col2[1], lwd = lw)

mtext("Amplitude Ratio", side = 2, 
      line = 1, outer = T, cex = 1.4)
mtext("Flow Path Length (m)", side = 1, line = 1, outer = T, cex = 1.4)
dev.off()

