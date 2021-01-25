library("raster")
byronbase <- raster("C:/Users/Katie Fogg/Desktop/HGSwork/aquiferBottomConstantSlope.asc")

plot(byronbase)
extent(byronbase)

byrondf <- data.frame(x = seq()) 

subset(byronbase, x == 39400)

aquiferslopeYdirection <- (max(getValues(byronbase))-min(getValues(byronbase)))/(5056500 - 5050900)
aquiferslopeYdirection

aquiferslopeYdirection <- (max(getValues(byronbase))-min(getValues(byronbase)))/(5056500 - 5050900)




#############################
#slope of 0.01 in y direction
#newaquifer <- raster(nrows = 2800, ncol = 300, xmn = 393749, xmx = 394349,
#                     ymn = 5050900, ymx = 5056500, res = c(2,2))

newaquifer <- raster(nrows = 502, ncol = 5,
                     xmn = 0, xmx = 10,
                     ymn = 0, ymx = 1004,
                     res = c(2,2))


rasterFill <- rep(seq(10, 0, length.out = 502), each = 5) 
#check slope
(max(rasterFill)-min(rasterFill))/(1000)

newaquifer <- setValues(newaquifer, rasterFill)
plot(newaquifer)

writeRaster(newaquifer, "C:/Users/Katie Fogg/Desktop/HGSwork/newaquifer.asc", overwrite = TRUE)

#middle of sat zone
layer1.25 <- newaquifer + 1.25
writeRaster(layer1.25, "C:/Users/Katie Fogg/Desktop/HGSwork/newlayer1_25.asc", overwrite = TRUE)

#top of sat zone
layer2.5 <- newaquifer + 2.5
writeRaster(layer2.5, "C:/Users/Katie Fogg/Desktop/HGSwork/newlayer2_5.asc", overwrite = TRUE)

#top of unsat zone
layer3.0 <- newaquifer + 3.0
writeRaster(layer3.0, "C:/Users/Katie Fogg/Desktop/HGSwork/newlayer3_0.asc", overwrite = TRUE)

#layer 3.5 above base
layer3.5 <- newaquifer + 3.5
writeRaster(layer3.5, "C:/Users/Katie Fogg/Desktop/HGSwork/newlayer3_5.asc", overwrite = TRUE)



#calculate boundary condition head values
slope <- 0.1
deltay <- 2

slope*deltay + 640
abs(slope*deltay - 584)


min(getValues(layer3.0)) + 0.2

