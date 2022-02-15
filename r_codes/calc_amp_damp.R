##
## Calculate amplitude damping (annual signal damping)
##
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
outtimes <- read.table("C:/Users/skati/Box/HGSwork/outputTimes_inputTemps/OutputTimesYr7_fulldays.txt")
lubridays <- ymd_hms("2021-01-01 00:00:00") + (outtimes$V1[seq(1,by = 23, length.out = 24)]-(365*86400*7))

# x-values (should be the same for all models)
x_vals <- k400_1.0_riveronly_s[,1,1,"X"]


calc_amps <- function(x){
  hz_mean <- matrix(data = 0, nrow = 24, ncol = 413)
  for(i in 1:413){
    hz_mean[,i] <- colMeans(x[1:40, i, ])
  }
  amps <- length(413)
  for(i in 1:413){
    amps[i] <-  (max(hz_mean[,i]) - min(hz_mean[,i]))/2
  }
  return(amps)
}

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


cols4 <- hcl.colors(4)

#### ---- K100 ---- ####
par(mfrow = c(3,1), mar = c(2,2,1,1))
## --riveronly --##
plot(k100_0.5_riveronly_amps ~ x_vals, ylim =c(0,10), type = "l", col = cols4[1], lwd = 2) 
lines(k100_1.0_riveronly_amps ~ x_vals, col = cols4[2], lwd = 2)
lines(k100_2.0_riveronly_amps ~ x_vals, col = cols4[3], lwd = 2)
lines(k100_3.0_riveronly_amps ~ x_vals, col = cols4[4], lwd = 2)
legend("topright", c("0.5", "1.0", "2.0", "3.0"), lwd = 2, col = cols4[1:4])

## -- shady -- ##
plot(k100_0.5_shady_amps ~ x_vals, type = "l", col = cols4[1], ylim = c(0,10), lwd = 2)
lines(k100_1.0_shady_amps ~ x_vals, col = cols4[2], lwd = 2)
lines(k100_2.0_shady_amps ~ x_vals, col = cols4[3], lwd = 2)
lines(k100_3.0_shady_amps ~ x_vals, col = cols4[4], lwd = 2)

## -- sunny -- ##
plot(k100_0.5_sunny_amps ~ x_vals, type = "l", col = cols4[1], ylim = c(0,10), lwd = 2)
lines(k100_1.0_sunny_amps ~ x_vals, col = cols4[2], lwd = 2)
lines(k100_2.0_sunny_amps ~ x_vals, col = cols4[3], lwd = 2)
lines(k100_3.0_sunny_amps ~ x_vals, col = cols4[4], lwd = 2)

#### ---- K400 ---- ####
par(mfrow = c(3,1), mar = c(2,2,1,1))
## --riveronly --##
plot(k400_0.5_riveronly_amps ~ x_vals, ylim =c(0,10), type = "l", col = cols4[1], lwd = 2) 
lines(k400_1.0_riveronly_amps ~ x_vals, col = cols4[2], lwd = 2)
lines(k400_2.0_riveronly_amps ~ x_vals, col = cols4[3], lwd = 2)
lines(k400_3.0_riveronly_amps ~ x_vals, col = cols4[4], lwd = 2)
legend("topright", c("0.5", "1.0", "2.0", "3.0"), lwd = 2, col = cols4[1:4])

## -- shady -- ##
plot(k400_0.5_shady_amps ~ x_vals, type = "l", col = cols4[1], ylim = c(0,10), lwd = 2)
lines(k400_1.0_shady_amps ~ x_vals, col = cols4[2], lwd = 2)
lines(k400_2.0_shady_amps ~ x_vals, col = cols4[3], lwd = 2)
lines(k400_3.0_shady_amps ~ x_vals, col = cols4[4], lwd = 2)

## -- sunny -- ##
plot(k400_0.5_sunny_amps ~ x_vals, type = "l", col = cols4[1], ylim = c(0,10), lwd = 2)
lines(k400_1.0_sunny_amps ~ x_vals, col = cols4[2], lwd = 2)
lines(k400_2.0_sunny_amps ~ x_vals, col = cols4[3], lwd = 2)
lines(k400_3.0_sunny_amps ~ x_vals, col = cols4[4], lwd = 2)

