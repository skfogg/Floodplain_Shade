rasterHeatMap <- function(raster, colors = c(hcl.colors(5, "Plasma"), "chartreuse"), mintemp = 1, maxtemp = 22, breakInterval = 0.5, ...){
    require(RColorBrewer)
  
    tempCol <- colorRampPalette(colors)
  
    plot(raster, 
         col = tempCol(length(seq(mintemp, maxtemp, by=breakInterval))), 
         breaks = seq(mintemp, maxtemp, by=breakInterval),
         asp = NA,
         xlab = "Flow Path Length (m)",
         ...)
    segments(0,35,800,35, lty = 1)
    segments(0,38,800,38, lty = 2)
    points(readWKT("POINT (700 38.15)"), pch = 6, cex = 1)
    
}