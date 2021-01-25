ribbonplot <- function(data, structuredata, zRange, xRange, t, colors, add = FALSE,...){
  
  if(add == TRUE){
    mapply(function(z,c) lines(structuredata[xRange,2,z,"X"], 
                               data[z,xRange,t],
                               col = c,
                               lwd = 1),
           zRange,
           colors)
  }else{
  
  plot(structuredata[xRange,2,zRange,"X"], 
       data[zRange,xRange,t],
       main = "",
       ylab = "",
       xlab = "",
       type = "n",
       ...)
  
  mapply(function(z,c) lines(structuredata[xRange,2,z,"X"], 
                             data[z,xRange,t],
                             col = c,
                             lwd = 1),
         zRange,
         colors)
  }
}