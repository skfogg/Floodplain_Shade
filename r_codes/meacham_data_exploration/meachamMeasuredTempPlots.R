
## PLOTS FOR PRESENTATION
meacham <- raster("C:/Users/Katie Fogg/Desktop/HGSwork/gisProps/2013_floodplain_elevation.asc")
png("C:/Users/Katie Fogg/Desktop/HGSwork/for_presentation/MeachamFloodplain.png",
    width = 1000, height = 1000)
par(bty = "n", mar = c(1,1,1,5), cex = 2, cex.lab = 2)
plot(meacham,
     xaxt = "n",
     yaxt = "n")
dev.off()
###################
library(lubridate)
library(zoo)
library(xts)
control <- read.csv("C:/Users/Katie Fogg/Desktop/HGSwork/for_presentation/TopControlMeacham2014.csv")
topR <- read.csv("C:/Users/Katie Fogg/Desktop/HGSwork/for_presentation/TopRestorationMeacham.csv")
bottomR <- read.csv("C:/Users/Katie Fogg/Desktop/HGSwork/for_presentation/BottomRestorationMeacham.csv")

head(control)
c <- xts(zoo(control$Temperature, order.by = mdy_hm(control$ReadingDateTime)))
plot(c["2014"])
plot(control$Temperature, type = "l")

head(topR)
mtemp <- xts(zoo(topR$Temperature, order.by = mdy_hm(topR$Reading.Date.mm.dd.yyyy)))
plot(mtemp["2005"])
plot(mtemp["2006"])
plot(mtemp["2007"])


head(bottomR)
tail(bottomR)
br <- bottomR[!is.na(bottomR$WaterTemperature),]
tail(br)

mtemp2 <- xts(zoo(br$WaterTemperature, order.by = mdy_hm(br$ReadingDateTime)))
plot(mtemp2)
plot(mtemp2["2007"])


dates <- "07-16/09-18"
yrs <- c(2007:2011, 2013:2015)
reachMeansT <- data.frame(yr = yrs, 
                         site = rep("t", length = 8), 
                         meanT = numeric(8))

for (i in 1:8){
reachMeansT$yr[i] <- yrs[i]
reachMeansT$meanT[i] <- mean(mtemp[paste0(yrs[i], "-07-16/", yrs[i],"-09-18")])
}

reachMeansB <- data.frame(yr = yrs, 
                          site = rep("b", length = 8), 
                          meanT = numeric(8))

for (i in 1:8){
  reachMeansB$yr[i] <- yrs[i]
  reachMeansB$meanT[i] <- mean(mtemp2[paste0(yrs[i], "-07-16/", yrs[i],"-09-18")])
}

reachMeans <- rbind(reachMeansT, reachMeansB)

png("C:/Users/Katie Fogg/Desktop/HGSwork/for_presentation/meachamTemp.png",
    width = 800,
    height = 500)
par(bg = "black",
    col.axis = "gray95",
    col.lab = "gray95",
    col = "gray95",
    col.main = "gray95",
    fg = "gray95",
    cex = 1.5,
    cex.lab = 1.5,
    cex.main = 1.5,
    cex.axis = 1.5,
    mar = c(5,5,4,1)
    )
plot(meanT ~ yr, 
     data = reachMeansT, 
     type = "o",
     ylim = c(min(reachMeans$meanT), max(reachMeans$meanT)),
     ylab = "Mean Summer Temperature (C)",
     xlab = "Year",
     main = "Meacham Creek Mean Temperature 07/16 - 09/19",
     pch = 16,
     lwd = 2,
     lty = 2,
     col = "orange",
     cex = 1.5)
lines(meanT ~ yr, 
      data = reachMeansB, 
      type = "o", 
      pch = 16,
      lwd = 2,
      lty = 2,
      cex = 1.5,
      col = "limegreen")
dev.off()


png("C:/Users/Katie Fogg/Desktop/HGSwork/for_presentation/meachamTempLegend.png",
    width = 800,
    height = 500)
par(bg = "black",
    col.axis = "gray95",
    col.lab = "gray95",
    col = "gray95",
    col.main = "gray95",
    fg = "gray95",
    cex = 1.5,
    cex.lab = 1.5,
    cex.main = 1.5,
    cex.axis = 1.5,
    mar = c(5,5,4,1),
    bty = "n")
plot(1:10, 1:10, col = "black",
     xaxt = "n",
     yaxt = "n",
     xlab = "",
     ylab = "")
legend(2,8, c("Upstream", "Downstream"), 
       pch = c(16,16),
       lwd = 2,
       lty = 2,
       cex = 1.5,
       col = c("orange", "limegreen"),
       bty = "n")
dev.off()

