average.steps.interval <- tapply(cleanData$steps, cleanData$interval, mean)
x <- split(cleanData[,1], cleanData$interval)
y <- sapply(x, mean)
plot(as.numeric(names(average.steps.interval)), average.steps.interval, 
     type = "l", xlab = "Time of a day (min)",
     ylab = "Number of steps in the 5-minuts intervals", 
     main = "Time series of average number of steps")

plot(average.steps.interval, type = "l", xlab = "Index of the 5-minuts interval",
     ylab = "Number of steps", main = "Time series of average number of steps")