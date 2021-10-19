### SUNNY SOIL COMPOUND SINE WAVE CALC ###
library(zoo)
library(xts)
library(lubridate)

## WHEN WORKING ON TOWER:
box_directory <- "C:/Users/t24x137/Box/UmatillaShare/"

source(paste0(box_directory, "includeNATimes.R"))
sunnysoil <- read.csv(paste0(box_directory, "sun/sunnysoil.csv"))

sunnysoilx <- xts(zoo(sunnysoil$Temp, order.by = mdy_hms(sunnysoil$DateTime)))
sunnysoilx <- includeNATimes(sunnysoilx)
sunnysoil <- includeNATimes(sunnysoilx, dropTS = T)
colnames(sunnysoil) <- c("DateTime", "Temp")

sunnysoil$DateTime <- as.character(sunnysoil$DateTime)
sunnysoil$PosixTime <- ymd_hms(sunnysoil$DateTime)
sunnysoil$NumTime <- as.numeric(sunnysoil$PosixTime)
sunnysoil$NumTimeOfYr <- sunnysoil$NumTime - as.numeric(mdy_hms("01/01/20 00:00:00"))

sunnysoildailymeanx <- apply.daily(sunnysoilx, mean)
plot(sunnysoildailymeanx)
sunnysoil$DailyMean <- rep(coredata(sunnysoildailymeanx), each = 24) 
plot(sunnysoil$DailyMean, type = "l")

sunnysoildaily <- data.frame(DateTime = as.character(index(sunnysoildailymeanx)),
                             PosixTime = index(sunnysoildailymeanx),
                             NumTime = as.numeric(index(sunnysoildailymeanx)),
                             NumTimeOfYr = as.numeric(index(sunnysoildailymeanx)) - as.numeric(mdy_hms("01/01/20 00:00:00")),
                             DailyMean = coredata(sunnysoildailymeanx))
names(sunnysoildaily)[5] <- "DailyMean"

# mean + amplitude*cos(2*pi*frequency*t + period)


### --------------------------- DAILY MEAN MODEL --------------------------- ###
### ------------------------------------------------------------------------ ###
# start val for m:
mean(sunnysoildaily$DailyMean, na.rm = T)
# start val for a:
(max(sunnysoildaily$DailyMean, na.rm = T)-min(sunnysoildaily$DailyMean, na.rm = T))/2
# fucking guess the radian shift for p

dailymeanmodel <- nls(DailyMean ~ m + a*sin(2*pi*(1/31536000)*NumTimeOfYr + p),
                      sunnysoildaily,
                      start = list(m = 9.64,
                                   a = 13.95,
                                   p = 0.1))
summary(dailymeanmodel)
plot(dailymeanmodel)
plot(DailyMean ~ I(NumTimeOfYr/86400), sunnysoildaily, type = "l")
lines(sunnysoildaily[complete.cases(sunnysoildaily$DailyMean),4]/86400, predict(dailymeanmodel), col = "blue")


t <- seq(0, 31536000*2, by = 86400)
plot(coef(dailymeanmodel)[1] + coef(dailymeanmodel)[2]*sin(2*pi*(1/31536000)*t + coef(dailymeanmodel)[3]),
     type = "l", 
     lwd = 2,
     ylab = "Daily Mean Temperture")
lines(DailyMean ~ I(NumTimeOfYr/86400), sunnysoildaily, col = "blue")

### -------------------- DAILY AMPLITUDE MODEL -------------------------- ###
### --------------------------------------------------------------------- ###
amps <- apply.daily(sunnysoilx, function(x) (max(x)-min(x))/2)
plot.zoo(amps)

sunnysoildaily$DailyAmp <- coredata(amps)
plot(sunnysoildaily$DailyAmp, type = "l")

# start val for m:
mean(sunnysoildaily$DailyAmp, na.rm = T)
# start val for a:
(max(sunnysoildaily$DailyAmp, na.rm = T)-min(sunnysoildaily$DailyAmp, na.rm = T))/2
# fucking guess the radian shift for p

dailyampmodel <- nls(DailyAmp ~ m + a*sin(2*pi*(1/31536000)*NumTimeOfYr + p),
                     sunnysoildaily,
                     start = list(m = 0.83,
                                  a = 1.57,
                                  p = 0.1))
summary(dailyampmodel)
plot(coef(dailyampmodel)[1] + coef(dailyampmodel)[2]*sin(2*pi*(1/31536000)*t + coef(dailyampmodel)[3]),
     type = "l", 
     lwd = 2,
     ylab = "Daily Amplitude",
     ylim = c(0,3))
lines(DailyAmp ~ I(NumTimeOfYr/86400), sunnysoildaily, col = "violet")

## --------------------- DAILY FREQUENCY MODEL ------------------- ##
## --------------------------------------------------------------- ##

dailyfreqmodel <- nls(I(Temp-DailyMean) ~ a*sin(2*pi*(1/86400)*NumTimeOfYr + p),
                      sunnysoil,
                      start = list(a = 2,
                                   p = 0.1))
summary(dailyfreqmodel)
t2 <- seq(0, 31536000*2, by = 3600)
plot(coef(dailyfreqmodel)[1]*sin(2*pi*(1/86400)*t2 + coef(dailyfreqmodel)[2]),
     type = "l")
lines(sunnysoil$Temp - sunnysoil$DailyMean, col = "yellow")

plot(sunnysoil$Temp - sunnysoil$DailyMean, type = "l")
lines(predict(dailyfreqmodel), col = "yellow")


## --------------------- COMPOUND MODEL -------------------------- ##
## --------------------------------------------------------------- ##

means <- coef(dailymeanmodel)[1] + coef(dailymeanmodel)[2]*sin(2*pi*(1/31536000)*t2 + coef(dailymeanmodel)[3])
amps <- coef(dailyampmodel)[1] + coef(dailyampmodel)[2]*sin(2*pi*(1/31536000)*t2 + coef(dailyampmodel)[3])

meachamsun <- means + amps*sin(2*pi*(1/86400)*t2 + coef(dailyfreqmodel)[2])
plot(meachamsun ~ t2, type = "l")
lines(sunnysoil$Temp ~ sunnysoil$NumTimeOfYr,
      col = "orange")






