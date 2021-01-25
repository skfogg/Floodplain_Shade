library("raster")
base <- raster("C:/Users/Katie Fogg/Desktop/HGSwork/aquiferBottomConstantSlope.asc")
mid <- base + 2.5
top0.1 <- mid + 0.1 # a tenth of a meter (10 cm)
top1 <- mid + 1 # 1 meter thick


top0.5 <- mid+0.5
top0.25 <- mid+0.25
top0.55 <-mid+0.55
top0.6 <- mid+0.6

writeRaster(top0.1, "C:/Users/Katie Fogg/Desktop/HGSwork/unsatLayer0_1.asc")
writeRaster(top1, "C:/Users/Katie Fogg/Desktop/HGSwork/unsatLayer1_0.asc")

writeRaster(top0.5, "C:/Users/Katie Fogg/Desktop/HGSwork/topOfUnsat0_5.asc")
writeRaster(top0.25, "C:/Users/Katie Fogg/Desktop/HGSwork/unsatLayer0_25.asc", overwrite = T)

writeRaster(top0.55, "C:/Users/Katie Fogg/Desktop/HGSwork/topOfUnsat0_55.asc")
writeRaster(top0.6, "C:/Users/Katie Fogg/Desktop/HGSwork/topOfUnsat0_6.asc")


top1
mid
top0.6
top0.5
