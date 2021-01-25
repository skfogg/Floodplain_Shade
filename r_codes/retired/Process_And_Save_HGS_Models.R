##########################################################
### Calculate and Save Daily Mean Array for Model Runs ###
##########################################################

library(devtools)
#install_github("FluvialLandscapeLab/HGSReader")
library(HGSReader)
source("C:/Users/Katie Fogg/Desktop/HGSwork/r_codes/functions/calcDailyMean.R")

memory.limit(size = 56000)

rivercontrols <- c( 
  "soil_0.3m_riveronly",
  "soil_0.5m_riveronly",
  "soil_1.0m_riveronly",
  "soil_2.0m_riveronly")

soilcontrols <- c(
  "soil_0.3m_soilonly",
  "soil_0.5m_soilonly",
  "soil_1.0m_soilonly",
  "soil_2.0m_soilonly")

models <- c(
  "soil_0.3m",
  "soil_0.5m",
  "soil_1.0m",
  "soil_2.0m")

allruns <- c(rivercontrols, soilcontrols, models)

###################
### Data Import ### 
###################
## folder path for 100m/day sunny:
folderpath <- "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\sensitivity\\sunny_iskulpaa_soil_input\\"

## folder path for 100m/day shady:
allruns <- "soil_0.3m"
folderpath <- "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\sensitivity\\fake_shady_iskulpaa\\"

## folder path for 400m/day shady:
allruns <- "soil_1.0m"
folderpath <- "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\sensitivity\\K_400m_day\\fake_shady_iskulpaa\\"

## folder path for 300m/day shady:
allruns <- "soil_1.0m"
folderpath <- "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\sensitivity\\K_300m_day\\sunny_iskulpaa_soil_input\\"

## folder path for 200m/day sunny:
allruns <- "soil_1.0m"
folderpath <- "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\sensitivity\\K_200m_day\\sunny_iskulpaa_soil_input\\"


###########################
## Loop through all runs ##
###########################
for(i in 1:length(allruns)){
  foldername <- allruns[i]
  
  f <- HGSFile(paste0(folderpath,
                      foldername, 
                      "\\temperatureTestero.pm.dat"))
  sat <- HGSGetData(f, variables = c("X", "Y", "Z", "Sat"), blockNumbers = 1)
  g <- HGSGetData(f, variables = c("X", "Y", "Z", "temp"))
  
  gmeans <- calcDailyMean(g)
  
  modelstructure <- g[,,,1,c("X","Y","Z")]
  velocity <- HGSGetData(f, variables = c("Vx", "Vy", "Vz"), blockNumbers = c(1))
  
  saveRDS(velocity, file = paste0(folderpath, foldername, "\\", foldername, "_velocity.RData"))
  saveRDS(gmeans, file = paste0(folderpath, foldername, "\\", foldername, "_dailymeans.RData"))
  saveRDS(sat, file = paste0(folderpath, foldername, "\\", foldername, "_saturation_T1.RData"))
  saveRDS(modelstructure, file = paste0(folderpath, foldername, "\\", foldername, "_modelstructure.RData"))
  
}

