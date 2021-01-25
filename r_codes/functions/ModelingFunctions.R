###
### Model making functions
###

includeNATimes <- function(x, dropTS = FALSE){
  xdf <- data.frame(datetime = index(x), value = coredata(x))
  emptydf <- data.frame(datetime = seq(index(x)[1], 
                                       last(index(x)), 
                                       by = as.numeric(index(x)[2])-as.numeric(index(x)[1])),
                        value = NA)
  # Left Join
  newdf <- merge(emptydf, xdf, all.x = TRUE)
  # Don't include just NA column
  xreturn <- newdf[,-2]
  
  if (dropTS == TRUE){
    return(xreturn) 
  } else{
    xtsreturn <- xts(zoo(xreturn[,2]), order.by = xreturn[,1])
    return(xtsreturn)
  }
}

rmIncompleteDays <- function(x){
  rs <- split(x, f = "days")
  
  rsc <- lapply(rs, anyNA)
  rsl <- lapply(rs, length)
  
  timestep <- as.numeric(index(x)[2])-as.numeric(index(x)[1])
  numTimeStepDays <- 86400/timestep
  
  rmvec <- c(0)
  for (i in 1:length(rs)){
    if(rsc[[i]] == TRUE | rsl[[i]] < numTimeStepDays){
      rmvec <- c(rmvec, i)
    }
  }
  rs <- rs[-rmvec]
  
  times <- as.POSIXct(unlist(lapply(rs, index)), origin = '1970-01-01', tz = "UTC")
  xwhole <- xts(zoo(unlist(rs), order.by = times))
  
  return(xwhole)
}

rangecalc <- function(x){
  return(max(x)-min(x))
}

toTempTimeDf <- function(x){
  temps <- coredata(x)
  t <- as.numeric(index(x))
  temptimedf <- data.frame(temps = temps, t = t)
  return(temptimedf)
}


