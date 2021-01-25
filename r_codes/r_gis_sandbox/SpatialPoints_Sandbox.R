
library(sp)

set.seed(1331)
pts = cbind(1:5, 1:5)
dimnames(pts)[[1]] = letters[1:5]
df = data.frame(a = 1:5)
row.names(df) = letters[5:1]


#base layer
x <- c(seq(0,4, by = 0.1),
       4.5, 5, 5.5, 6, 7,
       seq(8,1000, by = 2))
z <- -0.01*x+10
plot(z~x, ylim = c(9,10), xlim = c(0,10))


coord <- cbind(x,z)
head(coord)
tail(coord)

b <- SpatialPointsDataFrame(coord, data = data.frame(base = 1:543))

plot(b)


#bedrock top
coord2 <- cbind(x, z+25)

#alluvium top
coord3 <- cbind(x, z+28)


#all together
all <- rbind(coord, coord2, coord3)

a <- SpatialPointsDataFrame(all, data = data.frame(model = 1:nrow(all)))
plot(a) 

