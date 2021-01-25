loadMeanData <- function(soil, sun, K){
  
  #folderlocation <- sun
  if(str_detect(sun, "shady")){
    folderlocation <- "fake_shady_iskulpaa"  
  }else{
    folderlocation <- "sunny_iskulpaa_soil_input"
  }
  folderpath <- paste0("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\sensitivity\\K_", 
                       K,
                       "m_day\\", 
                       folderlocation, 
                       "\\", 
                       soil, 
                       "\\")

  return(readRDS(paste0(folderpath, soil, "_dailymeans.RData")))
}

