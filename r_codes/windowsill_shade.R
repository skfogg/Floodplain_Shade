

## windowsill planter

sun <- c(25.4,
         31.5,
         20.0,
         28.1,
         27.7,
         21.9,
         19.1,
         16.4,
         17.0,
         27.4,
         24.4,
         29.4,
         16.8,
         27.5,
         34.9,
         31.3,
         29.7,
         21.9,
         23.8,
         32.1)
shade <- c(22.6,
           22.1,
           18.4,
           15.8,
           19.7,
           23.9,
           18.7,
           17.2,
           15.7,
           18.2,
           15.8,
           15.9,
           16.8,
           15.6,
           25.3,
           17.6,
           31.2,
           15.8,
           19.2,
           16.0)

df <- data.frame(x=c(sun,shade), soil = rep(c("sun", "shade"), each = 20))

model <- lm(x~soil, data = df)
summary(model)

plot(x~soil, data = df)


