set.seed(123)

t <- 1:100
y <- cumsum(rnorm(100))
 
plot(t, y,
     type = "l",
     lwd = 2,
     col = "darkgreen",
     main = "Random Walk",
     xlab = "Time",
     ylab = "Value")

grid()
