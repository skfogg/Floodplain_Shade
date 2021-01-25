library(neonUtilities)

listZipfiles("C:/Users/Katie Fogg/Downloads/NEON_temp-soil.zip")


#soilTemps <- loadByProduct(dpID = "DP1.00041.001",
#                           site = "YELL",
#                           startdate = "2019-01",
#                           enddate = "2020-01",
#                           avg = "30")

setwd("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\April2020\\Sensitivity_1\\NEON_soil")
soilTemps <- readRDS("soilTemps.RDS")
