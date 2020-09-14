x <- c(20, 22, 25, 28, 30)
y <- c(70, 90, 125, 190, 280)

slope <- lm(y ~ x)$coefficient[2]
intercept <- lm(y ~ x)$coefficient[1]

#slope <- 10.71
#intercept <- -99.65

x_a <- 15
x_b <- 35

plot(x, y, xlab = "N Chambers", ylab = "Diameter (mm)", pch = 20, cex = 3, main = paste("Slope = ", round(slope, 2), "; Intercept = ", round(intercept, 2), sep = ""))

lines(x = c(x_a, x_b), y = c((x_a * slope) + intercept, (x_b * slope) + intercept), lwd = 3)

