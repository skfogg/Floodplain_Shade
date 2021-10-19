library(devtools)
install_github("FluvialLandscapeLab/hydrogeom")
install_github("FluvialLandscapeLab/HGSReader")

library(hydrogeom)
library(stringr)

source("r_codes/functions/loadMeanData.R")
source("r_codes/functions/loadStructureData.R")
source("r_codes/functions/ribbonplot.R")

soil_1.0m_means_shady_400 <- loadMeanData("soil_1.0m", "shady", "400")
soil_1.0m_structure_shady_400 <- loadStructureData("soil_1.0m", "shady", "400")
velocity400 <-readRDS("C:/Users/Katie Fogg/Desktop/HGSwork/sensitivity/K_400m_day/fake_shady_iskulpaa/soil_1.0m/soil_1.0m_velocity.RData")
hzsample400 <- data.frame(temperature = soil_1.0m_means_shady_400[21,1:445,1],
                          meter = soil_1.0m_structure_shady_400[1:445,2,21,"X"])



soil_1.0m_means_shady_100 <- loadMeanData("soil_1.0m", "shady", "100")
soil_1.0m_structure_shady_100 <- loadStructureData("soil_1.0m", "shady", "100")
velocity100 <-readRDS("C:/Users/Katie Fogg/Desktop/HGSwork/sensitivity/K_100m_day/sunny_iskulpaa_soil_input/soil_1.0m/soil_1.0m_velocity.RData")


mean(velocity100[,1,32,,"Vx"])
mean(velocity400[,1,32,,"Vx"])
# Mean x velocity in 400m/day HZ faster than 100m/day HZ (as expected)

# meters/velocity = residence time
hzsample400$meanRT <- hzsample400$meter / mean(velocity400[,1,32,,"Vx"])



hypoUmatilla <- hyporheicBins(nbins = 18,
                              factor = 2,
                              minRT = 60,
                              maxRT = 17154594,
                              porosity = 0.25,
                              hyporheicSize = 35.95,
                              b = -1.39)
hypo400 <- hyporheicBins(nbins = 18,
                         factor = 2,
                         minRT = hzsample400$meanRT[2],
                         maxRT = hzsample400$meanRT[445],
                         porosity = 0.25,
                         hyporheicSize = 2400,
                         b = -1.39)

hypo400_binned <- TSZStats(round(hzsample400$meanRT[2:445],0),
                           tau_0 = round(hzsample400$meanRT[2],0),
                           tau_n = round(hzsample400$meanRT[445],0),
                           q = 400,
                           MoreArgs = list(alpha = 1.39),
                           shape = "powerLaw")

par(mfrow = c(3,1), mar = c(2,2,2,1), oma = c(2,2,0,0))
plot(hypo400_binned$to,
     hypo400_binned$entering,
     type = "l",
     main = "'Entering'")
plot(hypo400_binned$to,
     hypo400_binned$continuing,
     type = "l",
     main = "'Continuing'")
plot(hypo400_binned$meanResTime,
     hypo400_binned$returning,
     type = "l",
     main = "'Returning'")
mtext("'From' Residence Time", side = 1, outer = T, line = 1)
mtext("Flow (m/s)", side = 2, outer = T, line = 1)



hydrogeomTimes <- hypo400$to
(meters_at_RT_times <- mean(velocity[,1,32,,"Vx"])*hydrogeomTimes)
hypowaterupwelling <- hypo400$returning


## 1.0m: 11:41 (HZ Range)
## x @ 445 == flow path 800 meters


# ## temperature values
# soil_1.0m_means_shady_100[21,1:445,1]
# 
# ## meters
# soil_1.0m_structure_shady_100[1:445,2,21,"X"]

hzsample <- data.frame(temperature = soil_1.0m_means_shady_400[21,1:445,1],
                       meter = soil_1.0m_structure_shady_400[1:445,2,21,"X"])

temperatureAtMeter <- data.frame(meters = meters_at_RT_times, temp = NA)
for(i in 1:length(meters_at_RT_times)){
  diff <- min(abs(hzsample$meter - meters_at_RT_times[i]))
  hzsample$diffs <- abs(hzsample$meter - meters_at_RT_times[i])
  gettemp <- subset(hzsample, diffs == diff)
  temperatureAtMeter$temp[i] <- tail(gettemp$temperature, 1)  
}

plot(temp~meters, data = temperatureAtMeter, type = "l")

ribbonplot(data = soil_1.0m_means_shady_400,
           structuredata = soil_1.0m_structure_shady_400,
           zRange = 11:41,
           xRange = 1:445,
           t = 1, 
           colors = hcl.colors(length(11:41), palette = "DarkMint", rev = F),
           ylim = c(2,22),
           add = F)
ribbonplot(data = soil_1.0m_means_shady_400,
           structuredata = soil_1.0m_structure_shady_400,
           zRange = 21,
           xRange = 1:445,
           t = 1, 
           colors = "blue",
           ylim = c(2,22),
           add = T)
axis(1, at = meters_at_RT_times, labels = round(hydrogeomTimes, 1),
     line = 2)
points(temp~meters, data = temperatureAtMeter, pch = 20, col = "yellow")

barplot(hypowaterupwelling, )



plot(hypowaterupwelling ~ temperatureAtMeter$meters,
     log = "y")

temperatureAtMeter$temp * hypowaterupwelling
weighted.mean(temperatureAtMeter$temp, hypowaterupwelling)


stanSpatTemp <- subset(hzsample, round(meter,1) %in% seq(0.9, 798.9, by =2))
plot(temperature~meter, stanSpatTemp)
mean(stanSpatTemp$temperature)


colordf <- data.frame(v = seq(1,22,by=0.5), c = c(hcl.colors(21, "Blues"), "white",hcl.colors(21, "Reds", rev=T)))

seq(0.9, 798.9, by =2)
