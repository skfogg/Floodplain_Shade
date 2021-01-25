library(raster)
library(rasterVis)
library(RColorBrewer)
library(animation)
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

soil_1.0m_sunny_raster <- readRDS("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\sensitivity\\K_100m_day\\sunny_iskulpaa_soil_input\\soil_1.0m\\soil_1.0m_sunny_rasterlist.RData")
soil_1.0m_shady_raster <- readRDS("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\sensitivity\\K_100m_day\\fake_shady_iskulpaa\\soil_1.0m\\soil_1.0m_shady_rasterlist.RData")

tempColTest <- colorRampPalette(c(hcl.colors(5, "Plasma"), "chartreuse"))

colordf <- data.frame(v = seq(1,22,by=0.5), 
                      c = tempColTest(length(seq(1,22,by=0.5))))

#######################
##### Output Times ####
#######################
yr7Jdays <- seq(365*86400*7, by = 15*86400, length.out = 24)

yr7Jdays_fulldays <- unlist(lapply(yr7Jdays, 
                                   function(x)
                                     seq(x, by = 3600, length.out = 24)))
normalJday <- (yr7Jdays- (365*86400*7))/86400

everyother <- seq(2,24,by=2)
everyothermonth <- everyother[seq(1,12,by=2)]

monthnames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
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
seasons <- subset(jframe, jday %in% c(1,91,181,271))


## DO IT FUCKING THIS WAY BECAUSE COLORTABLE FUCKS 
## UP A BUNCH
## OF DEFAULT PLOTTING SHIT!
plot(soil_1.0m_shady_raster[[1]], 
     col= tempColTest(length(seq(1,22,by=0.5))), 
     breaks= seq(1,22,by=0.5),
     asp = NA,
     legend = F)


#########################
### NEW SHADE V SUNNY ###
#########################
for(i in 1:1){
  png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
      res = 72*8,
      width = 800*8,
      height = 600*8)
 layout(matrix(c(1,1,1,1,1,1,1,1,1,1,2,2,
                 3,3,3,3,3,3,3,3,3,3,2,2), 
               2, 12, byrow = T))
  
  # par(mar = c(3,2,5,2))
  
  plot(soil_1.0m_sunny_raster[[i]], 
       col = tempColTest(length(seq(1,22,by=0.5))), 
       breaks = seq(1,22,by=0.5),
       asp = NA,
       legend = F,
       main = "SUNNY",
       bty = "n",
       xlim = c(0, 800))
  segments(0,35,800,35, lty = 1)
  segments(0,38,800,38, lty = 2)
  text(readWKT("POINT (750 31)"), labels = jframe$dayofyear[i], cex = 1.3)
  points(readWKT("POINT (700 38.15)"), pch = 6, cex = 1)
  
  # plot(soil_1.0m_sunny_raster[[i]], 
  #      col= tempColTest(length(seq(1,22,by=0.5))), 
  #      breaks= seq(1,22,by=0.5),
  #      legend.only = T,
  #      smallplot = c(850, 855, 32, 38),
  #      add = F)

  par(new = T)
  plot(rep(1,times = length(colordf$v)),
       colordf$v,
       xaxt = "n",
       bty = "n",
       pch= 15,
       col = colordf$c,
       ylim = c(0.5,22.5),
       cex = 2.6,
       ylab = "",
       xlab = "",
       yaxt = "n")
  axis(4, at = c(5,10,15,20),
       pos = 1.1)
  
  par(new = T)
  plot(soil_1.0m_shady_raster[[1]], 
       col= tempColTest(length(seq(1,22,by=0.5))), 
       breaks= seq(1,22,by=0.5),
       asp = NA,
       legend = F,
       main = "SHADY",
       bty = "n")
  segments(0,35,800,35, lty = 1)
  segments(0,38,800,38, lty = 2)
  text(readWKT("POINT (750 31)"), labels = jframe$dayofyear[i], cex = 1.3)
  points(readWKT("POINT (700 38.15)"), pch = 6, cex = 1)
  
  dev.off()
}






#################################
#### Compare Sunny and Shady ####
#################################
saveHTML({
  par(mfrow = c(2,1),
      mar = c(2,2,5,1))
  for(i in 1:24){
    plot(soil_1.0m_sunny_raster[[i]], 
         col= tempColTest(length(seq(1,22,by=0.5))), 
         breaks= seq(1,22,by=0.5),
         asp = NA,
         main = "Sunny")
    segments(0,35,800,35, lty = 1)
    segments(0,38,800,38, lty = 2)
    points(readWKT("POINT (700 38.15)"), pch = 6, cex = 1)
    
    
    plot(soil_1.0m_shady_raster[[i]], 
         col= tempColTest(length(seq(1,22,by=0.5))), 
         breaks= seq(1,22,by=0.5),
         asp = NA,
         main = "Shady")
    segments(0,35,800,35, lty = 1)
    segments(0,38,800,38, lty = 2)
    points(readWKT("POINT (700 38.15)"), pch = 6, cex = 1)
    
  }
},
img.name = "shady_1.0m_heat_map",
htmlfile = "soil1.0_sunny_rasterAnimation.html",
interval = 0.5,
ani.width = 800,
ani.height = 800,
ani.res = 72*5
)

##########################################
#### Compare K Values at Aoil = 1.0 m ####
##########################################
sensitivityFolder <- "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\sensitivity\\"
k100 <- readRDS(paste0(sensitivityFolder, 
                       "K_100m_day\\fake_shady_iskulpaa\\soil_1.0m\\soil_1.0m_shady_rasterlist.RData"))
k200 <- readRDS(paste0(sensitivityFolder, 
                       "K_200m_day\\fake_shady_iskulpaa\\soil_1.0m\\soil_1.0m_shady_rasterlist.RData"))
k300 <- readRDS(paste0(sensitivityFolder, 
                       "K_300m_day\\fake_shady_iskulpaa\\soil_1.0m\\soil_1.0m_shady_rasterlist.RData"))
k400 <- readRDS(paste0(sensitivityFolder, 
                       "K_400m_day\\fake_shady_iskulpaa\\soil_1.0m\\soil_1.0m_shady_rasterlist.RData"))


saveHTML({
  par(mfrow = c(4,1),
      mar = c(4,4,5,4),
      cex.main = 2,
      cex.lab = 2,
      cex.axis = 2)
  for(i in 1:24){
    plot(k100[[i]], 
         col= tempColTest(length(seq(1,22,by=0.5))), 
         breaks= seq(1,22,by=0.5),
         asp = NA,
         main = "K = 100 m/day")
    segments(0,35,800,35, lty = 1)
    segments(0,38,800,38, lty = 2)
    points(readWKT("POINT (700 38.15)"), pch = 6, cex = 1)
    
    
    plot(k200[[i]], 
         col= tempColTest(length(seq(1,22,by=0.5))), 
         breaks= seq(1,22,by=0.5),
         asp = NA,
         main = "K = 200 m/day")
    segments(0,35,800,35, lty = 1)
    segments(0,38,800,38, lty = 2)
    points(readWKT("POINT (700 38.15)"), pch = 6, cex = 1)
    
    plot(k300[[i]], 
         col= tempColTest(length(seq(1,22,by=0.5))), 
         breaks= seq(1,22,by=0.5),
         asp = NA,
         main = "K = 300 m/day")
    segments(0,35,800,35, lty = 1)
    segments(0,38,800,38, lty = 2)
    points(readWKT("POINT (700 38.15)"), pch = 6, cex = 1)
    
    plot(k400[[i]], 
         col= tempColTest(length(seq(1,22,by=0.5))), 
         breaks= seq(1,22,by=0.5),
         asp = NA,
         main = "K = 400 m/day")
    segments(0,35,800,35, lty = 1)
    segments(0,38,800,38, lty = 2)
    points(readWKT("POINT (700 38.15)"), pch = 6, cex = 1)
    
  }
},
img.name = "shady_kVariable_heat_map",
htmlfile = "shady_kVariable_1.0_rasterAnimation.html",
interval = 0.5,
ani.width = 800,
ani.height = 1200,
ani.res = 72*5
)


as.data.frame(k400[[1]], xy = T)

for (i in 1:24){
names(k400[[i]]) <- paste0("t", i)
}

k400df <- lapply(k400, as.data.frame, xy = T)

library(plotly)

plot_geo(data = k400df[[1]])

fig <- plot_ly(x = k400df[[1]]$x,
               y = k400df[[1]]$y,
               z = k400df[[1]]$t1, type = "heatmap")
fig





