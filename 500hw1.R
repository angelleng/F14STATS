# STATS 500 HW1 
read.csv("autodat.csv") -> autodat
autodat$mpg[autodat$mpg == 99] <- NA
autodat$horsepower[autodat$horsepower == 999] <- NA
autodat$acceleration[autodat$acceleration == 99] <- NA
autodat$origin <- factor(autodat$origin)
levels(autodat$origin) <- c("American", "European", "Japanese")
summary(autodat)

hist(autodat$acceleration) 
plot(density(autodat$acceleration, na.rm = T), main = "kernel density of acceleration")

mean(autodat$acceleration, na.rm = T)
sd(autodat$acceleration, na.rm = T)

plot(density(autodat$horsepower, na.rm = T), main = "kernel density of horsepower")
plot(density(log(autodat$horsepower), na.rm = T), main = "kernel density of log(hoursepower")

boxplot(autodat$mpg ~ autodat$origin, main = "boxplot of mpg")

tapply(autodat$mpg, autodat$origin, function(x) mean(x, na.rm = T))

pairs(autodat[, c("mpg", "displacement", "horsepower", "weight", "acceleration")])



apply(autodat[autodat$origin == "American", c("mpg", "displacement", "horsepower", "weight", "acceleration")], 2, function(x) mean(x, na.rm = T))
