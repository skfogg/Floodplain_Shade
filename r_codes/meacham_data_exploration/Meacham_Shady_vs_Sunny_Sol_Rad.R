### SolRad ###

library("zoo")
library("xts")
library("lubridate")

foldername <- "C:/Users/Katie Fogg/Desktop/meacham/"

shady1 <- read.csv(paste0(foldername, "SShadyWSPyranometerUp_10732077_final-1.csv"),
                   col.names = c("idx", "datetime", "rad"))
shady2 <- read.csv(paste0(foldername, "SShadyWSPyranometerUp_10732077_final-2.csv"),
                   col.names = c("idx", "datetime", "rad"))
sunny1 <- read.csv(paste0(foldername, "SSunnyWSPyranometerUp_10556690_final-1.csv"),
                   col.names = c("idx", "datetime", "rad"))
sunny2 <- read.csv(paste0(foldername, "SSunnyWSPyranometerUp_10556690_final-2.csv"),
                   col.names = c("idx", "datetime", "rad"))
sunny3 <- read.csv(paste0(foldername, "SSunnyWSPyranometerUp_10556690_final-3.csv"),
                   col.names = c("idx", "datetime", "rad"))

shadydf <- rbind(shady1, shady2)
sunnydf <- rbind(sunny1, sunny2, sunny3)


shady <- xts(zoo(shadydf$rad, order.by = mdy_hms(shadydf$datetime)))
sunny <- xts(zoo(sunnydf$rad[1:708], order.by = mdy_hms(sunnydf$datetime[1:708])))

plot.zoo(shady)
lines(as.zoo(sunny), col = "orange")

plot.zoo(shady["2015-07-31"])
lines(as.zoo(sunny["2015-07-31"]), col = "orange")

## Watts per day ##
plot.zoo(cumsum(shady["2015-07-30"]), ylim = c(0,30000))
lines(cumsum(as.zoo(sunny["2015-07-30"])), col = "orange")

shady_wattsum <- apply.daily(shady["2015-07-30/2015-08-05"], sum) 
sunny_wattsum <- apply.daily(sunny["2015-07-30/2015-08-05"], sum)

plot.zoo(shady_wattsum, ylim =c(000, 30000), type = "p")
points(as.zoo(sunny_wattsum), col = "orange")

(mean(sunny_wattsum) - mean(shady_wattsum)) / mean(sunny_wattsum)

## Watts accumulated over the afternoon ##
plot.zoo(cumsum(shady["2015-07-30 12:00:00/2015-07-31 00:00:00"]), ylim= c(900, 15100))
lines(as.zoo(cumsum(sunny["2015-07-30 12:00:00/2015-07-31 00:00:00"])),
      col = "orange")

date <- ymd("2015-08-05")
(sum(sunny[paste0(date, " 12:00:00/", date + 1," 00:00:00")]) - 
   sum(shady[paste0(date, " 12:00:00/", date+1," 00:00:00")]))/sum(sunny[paste0(date, " 12:00:00/", date+1," 00:00:00")])


### Weather Station Air temperature ##
foldername <- "C:/Users/Katie Fogg/Desktop/meacham/"

shady1a <- read.csv(paste0(foldername, "SShadyWSTempRH_10730177_final-1.csv"),
                   col.names = c("idx", "datetime", "temp", "rh"))
shady2a <- read.csv(paste0(foldername, "SShadyWSTempRH_10730177_final-2.csv"),
                   col.names = c("idx", "datetime", "temp", "rh"))
sunny1a <- read.csv(paste0(foldername, "SSunnyWSTempRH_10553854_final-1.csv"),
                   col.names = c("idx", "datetime", "temp", "rh"))
sunny2a <- read.csv(paste0(foldername, "SSunnyWSTempRH_10553854_final-2.csv"),
                   col.names = c("idx", "datetime", "temp", "rh"))

shadyadf <- rbind(shady1a, shady2a)
sunnyadf <- rbind(sunny1a, sunny2a)


shadyt <- xts(zoo(shadyadf$temp, order.by = mdy_hms(shadyadf$datetime)))
sunnyt <- xts(zoo(sunnyadf$temp, order.by = mdy_hms(sunnyadf$datetime)))

plot.zoo(shadyt)
lines(as.zoo(sunnyt), col= "orange")

length(sunny)
length(shady)
