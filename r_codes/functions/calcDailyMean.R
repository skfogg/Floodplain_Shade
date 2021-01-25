
## Processing functions

calcDailyMean <- function(cube){
  xs <- cube[,2,1,1,"X"]
  ys <- cube[1,2,1,1,"Y"]
  zs <- cube[1,2,,1,"Z"]
  tlength <- length(cube[1,1,1,,"temp"])
  
  sID <- seq(1, tlength-23, by = 24)
  eID <- seq(24, tlength, by = 24)
  
  poop <- array(NA, dim = c(Z = length(zs), X = length(xs), Time = 24))
  
  xmeans <- numeric(length(xs))
  for (t in 1:24){
    s <- sID[t]
    e <- eID[t]
    
    for(z in 1:length(zs)){
      for(x in 1:length(xs)){
        xmeans[x] <- mean(cube[x,2,seq(length(zs), 1, by = -1)[z],s:e,"temp"])
      }
      poop[z,,t] <- xmeans
    }
  }
return(poop)
}






