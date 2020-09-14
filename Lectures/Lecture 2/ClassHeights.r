ClassHeights <- c("5'11\"", "5'9\"", "6'1\"", "5'7\"", "5'1\"", "6'5\"", "6'2\"", "5'8.5\"", "5'8\"", "5'11\"", "5'4\"", "5'10\"", "5'2\"", "5'4\"", "5'7\"", "5'9\"", "5'3\"", "5'3\"", "5'6\"", "5'9\"", "5'8\"", "6'0\"")


ClassHeights <- matrix(as.numeric(unlist(strsplit(ClassHeights, split = "\"|'"))), ncol = 2, byrow = TRUE)

ClassHeights <- (ClassHeights[, 1] * 12) + ClassHeights[, 2]

pdf("Class Heights.pdf", width = 10, height = 7)


hist(ClassHeights, breaks = min(ClassHeights):max(ClassHeights), border = 0, col = "Grey")

hist(ClassHeights, breaks = min(ClassHeights):max(ClassHeights), border = 0, col = "Grey")

FittedNormal <- dnorm(x = seq(from = min(ClassHeights), to = max(ClassHeights), length.out = 1000), mean = mean(ClassHeights), sd = sd(ClassHeights))

points(x = seq(from = min(ClassHeights), to = max(ClassHeights), length.out = 1000), y = FittedNormal * length(ClassHeights), type = "l", col = "Red", lwd = 3)




RandomlyGeneratedHeights <- list()

for(i in 1:25) {
  if(i != 18) {
    RandHeights <- rnorm(length(ClassHeights), mean = mean(ClassHeights), sd = sd(ClassHeights))
    RandHeights <- RandHeights[intersect(which(RandHeights >= min(ClassHeights)), which(RandHeights <= max(ClassHeights)))]
    RandomlyGeneratedHeights[[i]] <- RandHeights
  } else {
    RandomlyGeneratedHeights[[i]] <- ClassHeights
  }
}


par(mfrow = c(5, 5), mar = c(0,0,0,0))

for(i in 1:25) {
  
  hist(RandomlyGeneratedHeights[[i]], breaks = min(ClassHeights):max(ClassHeights), ylim = c(0, max(unlist(lapply(lapply(lapply(lapply(RandomlyGeneratedHeights, hist, plot = FALSE, breaks = min(ClassHeights):max(ClassHeights)), '[', "counts"), unlist), max)))), axes = FALSE, xlab = NULL, ylab = NULL, main = NULL, border = 0, col = "Grey")
  points(x = seq(from = min(ClassHeights), to = max(ClassHeights), length.out = 1000), y = FittedNormal * length(ClassHeights), type = "l", col = "Red", lwd = 2)
}




#hist(x = ClassHeights, col = rgb(1, 0, 0, 0.5), add = TRUE, breaks = min(ClassHeights):max(ClassHeights))

BullsHeights <- c("6'3\"", "6'4\"", "6'4\"", "6'4\"", "6'9\"", "6'6\"", "6'5\"", "7'0\"", "7'0\"", "6'10\"", "6'4\"", "6'3\"", "6'6\"", "6'7\"", "6'11\"", "6'8\"")
SkyHeights <- c("6'3\"", "6'4\"", "6'1\"", "6'5\"", "5'10\"", "6'2\"", "6'5\"", "6'2\"", "6'4\"", "5'9\"", "5'10\"", "5'8\"")
KentuckyDerbyHeights <- c("5'4\"", "5'5\"", "5'2\"", "5'3\"", "5'4\"", "5'8\"", "5'6\"", "5'1\"", "5'2\"", "5'6\"", "5'3\"", "5'7\"", "5'5\"", "5'4\"", "5'8\"", "5'6\"", "5'6\"", "5'3\"")

BullsHeights <- (as.numeric(unlist(lapply(strsplit(BullsHeights, "\"|'"), '[', 1))) * 12) + as.numeric(unlist(lapply(strsplit(BullsHeights, "\"|'"), '[', 2)))
SkyHeights <- (as.numeric(unlist(lapply(strsplit(SkyHeights, "\"|'"), '[', 1))) * 12) + as.numeric(unlist(lapply(strsplit(SkyHeights, "\"|'"), '[', 2)))
KentuckyDerbyHeights <- (as.numeric(unlist(lapply(strsplit(KentuckyDerbyHeights, "\"|'"), '[', 1))) * 12) + as.numeric(unlist(lapply(strsplit(KentuckyDerbyHeights, "\"|'"), '[', 2)))

ANOVAData <- list()

ANOVAData[["GeologistHeights"]] <- ClassHeights
ANOVAData[["BasketballPlayerHeights"]] <- c(BullsHeights, SkyHeights)
ANOVAData[["JockeyHeights"]] <- KentuckyDerbyHeights

par(mfrow = c(3, 1))
for(i in 1:3) hist(ANOVAData[[i]], xlim = c(min(unlist(ANOVAData)), max(unlist(ANOVAData))), breaks = min(unlist(ANOVAData)):max(unlist(ANOVAData)), main = names(ANOVAData)[i], col = rainbow(3)[i], border = 0, xlab = "Height (inches)")



dev.off()



pdf("VarianceExample.pdf", width = 10, height = 7)

plot(x = ClassHeights, y = c(1:length(ClassHeights)), pch = 20, col = "Red", cex = 2, xlab = "Height (Inches)", axes = FALSE, ylab = "")
axis(side = 1)

plot(x = ClassHeights, y = c(1:length(ClassHeights)), pch = 20, col = "Red", cex = 2, xlab = "Height (Inches)", axes = FALSE, ylab = "")
axis(side = 1)
lines(x = rep(mean(ClassHeights), 2), y = c(0, length (ClassHeights) + 1), lwd = 3, lty = 2, col = "Red")

plot(x = ClassHeights, y = c(1:length(ClassHeights)), pch = 20, col = "Red", cex = 2, xlab = "Height (Inches)", axes = FALSE, ylab = "")
axis(side = 1)
lines(x = rep(mean(ClassHeights), 2), y = c(0, length (ClassHeights) + 1), lwd = 3, lty = 2, col = "Red")
for(i in 1:length(ClassHeights)) lines(x = c(ClassHeights[i], mean(ClassHeights)), y = c(i, i), col = "Red", lwd = 2)

plot(x = (ClassHeights - mean(ClassHeights)) ^ 2, y = c(1:length(ClassHeights)), pch = 20, col = "Red", cex = 2, xlab = "Height (Inches) - Squared distance from mean", axes = FALSE, ylab = "")
axis(side = 1)
lines(x = rep(0, 2), y = c(0, length(ClassHeights) + 1), lwd = 3, lty = 2, col = "Red")
for(i in 1:length(ClassHeights)) lines(x = c(0, (ClassHeights[i] - mean(ClassHeights)) ^ 2), y = c(i, i), col = "Red", lwd = 2)

dev.off()

minheight <- min(c(ANOVAData[["GeologistHeights"]], ANOVAData[["BasketballPlayerHeights"]]))
maxheight <- max(c(ANOVAData[["GeologistHeights"]], ANOVAData[["BasketballPlayerHeights"]]))
minheight <- floor(minheight / 2) * 2
maxheight <- ceiling(maxheight / 2) * 2
histbreaks <- seq(from = minheight, to = maxheight, length.out = ((maxheight - minheight) / 2) + 1)

GeolHeights <- ANOVAData[["GeologistHeights"]]
BasketHeights <- ANOVAData[["BasketballPlayerHeights"]]
t.test(x = GeolHeights, y = BasketHeights)


hist(ANOVAData[["GeologistHeights"]], breaks = histbreaks, border = 0, col = rgb(0, 0, 1, 0.5), axes = FALSE, main = "", xlab = "Height (inches)", ylab = "")
hist(ANOVAData[["BasketballPlayerHeights"]], breaks = histbreaks, add = TRUE, col = rgb(1, 0, 0, 0.5), border = 0)
axis(side = 1)

#t.test(y1,y2)

