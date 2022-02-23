##
## Vertical amplitude damping
##

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
z_vals_0.5 <- k400_0.5_riveronly_s[1,1,,"Z"]

jd <- c(1,16,31,46,61,76,91,106,121,136,151,166,181,196,211,226,
        241,256,271,286,301,316,331,346)

x = k100_0.5_shady
calc_vert_amps <- function(x){
  # hz_mean <- matrix(data = 0, nrow = 24, ncol = 413)
  # for(i in 1:413){
  #   hz_mean[,i] <- colMeans(x[1:40, i, ])
  # }
  
  plot(x[,1,1], z_vals_0.5, type = "l")
  amps <- length(413)
  for(i in 1:413){
    amps[i] <-  (max(hz_mean[,i]) - min(hz_mean[,i]))/2
  }
  return(amps)
}
