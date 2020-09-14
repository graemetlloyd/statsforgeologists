# This script simply summarises all the lower bed ammonites collectively

x <- read.csv("/Users/eargtl/Documents/Teaching/statsforgeologists/Ammonite_quadrats/Lower_Bed/AllQuadrats.csv")

plot(x = x[, "N_chambers"  ], y = x[, "Diameter_mm"], pch = 20, col = rgb(0, 0, 0, 0.5), cex = 3, xlab = "N Chambers", ylab = "Diameter (mm)")

mean(x[, "Diameter_mm"])
sd(x[, "Diameter_mm"])

t.test(x = x[, "Diameter_mm"], mu = 200)
