library(raster)
library(rasterVis)
library(RColorBrewer)
#library(animation)
library(sp)
#library(gstat)
#library(dismo)
#library(deldir)
#library(rgdal)
library(rgeos)
#library(tidyr)
library(stringr)
#source("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\r_codes\\functions\\loadMeanData.R")
#source("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\r_codes\\functions\\loadStructureData.R")
source("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\r_codes\\functions\\rasterHeatMap.R")

monthnames_abrv <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
daysinmonth <- c(31, 28, 31, 
                 30, 31, 30,
                 31, 31, 30, 
                 31, 30, 31)
dayofyr <- paste(monthnames_abrv[1], seq(1:daysinmonth[1]))
for(i in 2:12){
  dayofyr <- c(dayofyr, paste(monthnames_abrv[i], seq(1:daysinmonth[i])))
}
jframe <- data.frame(outputnumber = 1:24, jday = normalJday+1, dayofyear = dayofyr[normalJday+1])



soil_1.0m_sunny_raster <- readRDS("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\sensitivity\\K_100m_day\\sunny_iskulpaa_soil_input\\soil_1.0m\\soil_1.0m_sunny_rasterlist.RData")
soil_1.0m_shady_raster <- readRDS("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\sensitivity\\K_100m_day\\fake_shady_iskulpaa\\soil_1.0m\\soil_1.0m_shady_rasterlist.RData")

tempColTest <- colorRampPalette(c(hcl.colors(5, "Plasma"), "chartreuse"))

bedrock <- Line(rbind(c(0,35),c(800,35)))
unsat <- Line(rbind(c(0,38),c(800,38)))
themekf <- rasterTheme(region = tempColTest(22))

## Does not work unless copy-pasty
## 1 ##
i = 1
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 2 ##
i = 2
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 3 ##
i = 3
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 4 ##
i = 4
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 5 ##
i = 5
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 6 ##
i = 6
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 7 ##
i = 7
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 8 ##
i = 8
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 9 ##
i = 9
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 10 ##
i = 10
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 11 ##
i = 11
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 12 ##
i = 12
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 13 ##
i = 13
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 14 ##
i = 14
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 15 ##
i = 15
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 16 ##
i = 16
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 17 ##
i = 17
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 18 ##
i = 18
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 19 ##
i = 19
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 20 ##
i = 20
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 21 ##
i = 21
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 22 ##
i = 22
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 23 ##
i = 23
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()

## 24 ##
i = 24
png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
    res = 72*8,
    width = 800*8,
    height = 600*8)
stacktest <- stack(soil_1.0m_sunny_raster[[i]],
                   soil_1.0m_shady_raster[[i]])
names(stacktest) <- c("Sunny", "Shady")
p1 <- levelplot(stacktest,
                xlim = c(0,800),
                colorkey = list(col = tempColTest,
                                at = seq(1,22,by=1),
                                tick.number = 22),
                xlab = "Flow Path Length (m)",
                ylab = "Depth (m)",
                scales = list(y = list(at = c(38, 36, 34, 32), labels = c(1, 3, 5, 7)))) + 
  layer(sp.lines(unsat, lty = 2)) +
  layer(sp.lines(bedrock, lty = 1)) +
  layer(sp.text(loc = c(700,32), txt = jframe$dayofyear[i]))
plot(p1)
update(p1, aspect = "fill", 
       layout = c(1,2), 
       at = seq(1,22,by=1), 
       par.settings = themekf)
dev.off()
