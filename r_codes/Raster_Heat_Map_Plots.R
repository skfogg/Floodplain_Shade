#library(HGSReader)
library(raster)
#library(rasterVis)
#library(scatterplot3d)
library(RColorBrewer)
#library(animation)
library(sp)
#library(gstat)
#library(raster)
#library(dismo)
#library(deldir)
#library(rgdal)
library(rgeos)
#library(tidyr)
library(stringr)
#source('C:/Users/Katie Fogg/Desktop/HGSwork/r_codes/incrementDim.R')
source("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\r_codes\\functions\\loadMeanData.R")
source("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\r_codes\\functions\\loadStructureData.R")
source("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\r_codes\\functions\\ribbonplot.R")


allruns <- "soil_1.0m"
#####################
#### Import Data ####
#####################
soil_1.0m_means_shady_100 <- loadMeanData("soil_1.0m", "shady", "100")
soil_1.0m_structure_shady_100 <- loadStructureData("soil_1.0m", "shady", "100")


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


#####################
##### Color Shiz ####
#####################
soil_1.0m_sunny_raster <- readRDS("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\sensitivity\\K_100m_day\\sunny_iskulpaa_soil_input\\soil_1.0m\\soil_1.0m_sunny_rasterlist.RData")
soil_1.0m_shady_raster <- readRDS("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\sensitivity\\K_100m_day\\fake_shady_iskulpaa\\soil_1.0m\\soil_1.0m_shady_rasterlist.RData")

tempColTest <- colorRampPalette(c(hcl.colors(5, "Plasma"), "chartreuse"))
tempColTest(5)

colordf <- data.frame(v = seq(1,22,by=0.5), 
                      c = tempColTest(length(seq(1,22,by=0.5))))
                      #c = rainbow(length(seq(1,22,by=0.5)), rev = T))
                      #c = hcl.colors(length(seq(1,22,by=0.5)), "Plasma"))
  #data.frame(v = seq(1,22,by=0.5), c = c(hcl.colors(21, "Blues"), "white",hcl.colors(21, "Reds", rev=T)))


for(i in 1:24){
  colortable(soil_1.0m_sunny_raster[[i]]) <- colordf
  colortable(soil_1.0m_shady_raster[[i]]) <- colordf
}

for(i in 1:24){
  png(paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_", i,".png"),
      res = 72*8,
      width = 800*8,
      height = 400*8)
  layout(matrix(c(1,2,2,2,2,2,2,2,2,2,3,3,
                  4,5,5,5,5,5,5,5,5,5,3,3), 2, 12, byrow = T))
  
  par(mar = c(5,2,2,2))
  plot(soil_1.0m_sunny_raster[[i]], asp = NA, axes = T, 
       bty = "n",
       yaxt = "n",
       xlab = "Flow Path Length (m)",
       main = "Sunny")
  segments(0,35,800,35, lty = 1)
  segments(0,38,800,38, lty = 2)
  text(readWKT("POINT (750 31)"), labels = jframe$dayofyear[i], cex = 1.3)
  points(readWKT("POINT (700 38.15)"), pch = 6, cex = 1)
  
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
  
  plot(soil_1.0m_shady_raster[[i]], asp = NA, axes = T, 
       bty = "n",
       yaxt = "n",
       xlab = "Flow Path Length (m)",
       main = "Shady")
  segments(0,35,800,35, lty = 1)
  segments(0,38,800,38, lty = 2)
  text(readWKT("POINT (750 31)"), labels = jframe$dayofyear[i], cex = 1.3)
  points(readWKT("POINT (700 38.15)"), pch = 6, cex = 1)
  
  dev.off()
}

##############
###################
##############
library("ggplot2")
test_spdf <- as(soil_1.0m_shady_raster[[1]], "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

ggplot() +  
  geom_raster(data=test_df, aes(x=x, y=y, fill=value), alpha=1) +
  scale_fill_viridis() 
  #coord_equal() +
  #theme_map() +
  #theme(legend.position="bottom") +
  #theme(legend.key.width=unit(2, "cm"))


tempPalette <- colorRamp(c("#88002D", "#FDF5EB")) 
tempPalette2 <- colorRampPalette(c("aliceblue", "dodgerblue", "red"))

spround <- round(testraster, 1)
spNorm <- (spround - 1)/(22 - 1)
#spCols <- rgb(tempPalette(), max = 255)
spCols <- color_values(spNorm)
colortable(testraster) <- spCols
plot(testraster, asp=NA)
abline(h=35, lty = 1)
abline(h=38, lty = 2)


 newarray <- soil_1.0m_means_sunny

max(soil_1.0m_means_sunny, soil_1.0m_means_shady)
min(soil_1.0m_means_sunny, soil_1.0m_means_shady)
tail(hcl.colors(20, "OrRd"),1)

plot(raster(round(soil_1.0m_means_sunny[,,1],0)), 
     col = color_values(round(soil_1.0m_means_sunny[,,1],0)),
     asp = NA)

#plot(1:10, col = colour_values(c(10,1,9,2,8,3,7,4,6,5)), pch = 19, ylim = c(0,11), xlim = c(0,11))

dim(soil_1.0m_means_shady)
sp <- incrementDim(soil_1.0m_means_sunny, dimIDX = 3, newVals = spCols, incrementName = "color")


#### Temperature Raster Animation ####
readRDS(file = paste0(directory, "nnmsklist_", model2plot, ".RData"))

image(nnmsklist[[20]], 
      ylim = c(20,40),
      #col = tempPalette2(50),
      col = hcl.colors(50, "YlOrRd", rev = T),
      zlim = c(min(modellist[[model2plot]][,2,,,"temp"]),
               max(modellist[[model2plot]][,2,,,"temp"])))
abline(a = 38,
       b = -(1/100),
       col = "black",
       lty = 3)
abline(a = 35,
       b = -(1/100),
       col = "black",
       lty = 3)

library(animation)

rasterHeatPlot <- function(x){
  layout(matrix(1:2,ncol=2), width = c(2,1), height = c(1,1))
  image(nnmsklist[[x]], 
        ylim = c(20,40),
        col = tempPalette2(50),
        #col = hcl.colors(50, "YlOrRd", rev = TRUE),
        zlim = c(min(modellist[[model2plot]][,2,,,"temp"]),
                 max(modellist[[model2plot]][,2,,,"temp"])),
        main = rep(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), each = 2)[x])
  abline(a = 38,
         b = -(1/100),
         col = "black",
         lty = 3)
  abline(a = 35,
         b = -(1/100),
         col = "black",
         lty = 3)
  legend_image <- as.raster(matrix(hcl.colors(12, "YlOrRd", rev = F), ncol=1))
  plot(c(0,2), c(min(modellist[[model2plot]][,2,,,"temp"]),
                 max(modellist[[model2plot]][,2,,,"temp"])),
       type = 'n', axes = F,xlab = '', ylab = '', main = 'temperature')
  text(x = 1.5, 
       y = seq(min(modellist[[model2plot]][,2,,,"temp"]),
               max(modellist[[model2plot]][,2,,,"temp"]), 
               l = 5), 
       labels = round(seq(min(modellist[[model2plot]][,2,,,"temp"]),
                          max(modellist[[model2plot]][,2,,,"temp"]), 
                          l = 5),1))
  rasterImage(legend_image, 0, min(modellist[[model2plot]][,2,,,"temp"]), 1, max(modellist[[model2plot]][,2,,,"temp"]))
}


saveHTML({
  lapply(t, rasterHeatPlot)
},
img.name = model2plot,
htmlfile = "soil2.0_rasterAnimation.html",
interval = 0.5,
ani.width = 800,
ani.height = 400,
ani.res = 72*5
)

# saveGIF({
#   lapply(t, rasterHeatPlot)
# },
# movie.name = model2plot,
# img.name = "soil2.0m_gifanimation.gif",
# interval = 0.5,
# ani.width = 800,
# ani.height = 400
# )




soil_1.0m_structure_shady[,2,,"Z"]
