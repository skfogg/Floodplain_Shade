##
## Plots to Compare shady Floodplains with differing hydraulic conductivity, K, values
##
library(lubridate)

sensitivityDataFolder <- "C:/Users/t24x137/Box/HGSwork/sensitivity"
shady_400_1.0 <- readRDS(paste0(sensitivityDataFolder, 
                                       "/K_400m_day/shady/soil_1.0m/soil_1.0m_dailymeans.RData"))
shady_200_1.0 <- readRDS("C:/Users/t24x137/Box/Chapter_1_HGS/K_200m_day/shady/soil_1.0m/soil_1.0m_dailymeans.RData")
shady_300_1.0 <- readRDS("C:/Users/t24x137/Box/Chapter_1_HGS/K_300m_day/shady/soil_1.0m/soil_1.0m_dailymeans.RData")
shady_100_1.0 <- readRDS("C:/Users/t24x137/Box/Chapter_1_HGS/K_100m_day/shady/soil_1.0m/soil_1.0m_dailymeans.RData")

model_structure <- readRDS(paste0(sensitivityDataFolder, 
                                  "/K_400m_day/shady/soil_1.0m/soil_1.0m_modelstructure.RData"))
outtimes <- read.table("C:/Users/t24x137/Box/HGSwork/outputTimes_inputTemps/OutputTimesYr7_fulldays.txt")
days <- ymd_hms("2021-01-01 00:00:00") + (outtimes$V1[seq(1,by = 23, length.out = 24)]-(365*86400*7))


time <- 20
plotcol <- hcl.colors(4, "Fall")
par(mfrow = c(1,1),
    mar = c(5,4,4,1)+0.1,
    oma= c(0,0,0,0))
png(paste0("Compare_K_Shady_Floodplain", months(days[time]), "_", day(days[time]), ".png"),
    height = 400*5,
    width = 700*5,
    res = 72*5)
plot(model_structure[,2,32,"X"], shady_100_1.0[32,,time],
     type = "l",
     col = plotcol[1],
     lwd = 2,
     ylim = c(3,18),
       #c(min(shady_100_1.0[32,,time], shady_400_1.0[32,,time]),
            #  max(shady_100_1.0[32,,time], shady_400_1.0[32,,time])),
     main = paste0(months(days[time]), " ", day(days[time])),
     ylab = "Temperature",
     xlab = "Flow Path Length")
lines(model_structure[,2,32,"X"], shady_200_1.0[32,,time],
      col = plotcol[2],
      lwd = 2)
lines(model_structure[,2,32,"X"], shady_300_1.0[32,,time],
      col = plotcol[3],
      lwd = 2)
lines(model_structure[,2,32,"X"], shady_400_1.0[32,,time],
      col = plotcol[4],
      lwd = 2)
legend("topright", c("K = 100 m/day",
                        "K = 200 m/day",
                        "K = 300 m/day",
                        "K = 400 m/day"),
       col = plotcol,
       lwd = 2)
dev.off()


