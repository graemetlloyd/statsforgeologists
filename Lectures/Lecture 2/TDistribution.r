ClassHeights <- c("5'2\"", "5'11\"", "6'2\"", "5'7\"", "5'4\"", "5'5\"", "5'2\"", "5'9\"", "5'9\"", "5'10\"", "5'9\"", "5'5\"", "5'11\"", "5'5\"", "5'8\"", "5'10\"", "5'8\"", "5'7\"", "5'6\"", "5'9\"", "6'3\"", "5'5\"", "6'0\"", "5'3\"", "6'5\"", "6'0\"", "5'8\"", "6'0\"", "5'6\"", "5'5\"", "5'8\"", "5'6\"", "6'1\"", "5'10\"", "5'6\"", "5'7\"", "6'2\"", "5'6\"", "5'11\"", "5'7\"", "5'1\"", "6'1\"", "6'4\"")

ClassHeights <- matrix(as.numeric(unlist(strsplit(ClassHeights, split = "\"|'"))), ncol = 2, byrow = TRUE)

ClassHeights <- (ClassHeights[, 1] * 12) + ClassHeights[, 2]


# Class size for height data:
ClassSize <- length(ClassHeights)

plot(
x = seq(from = -5 * sd(ClassHeights), to = 5 * sd(ClassHeights), length.out = 1000) + mean(ClassHeights),

y = dt(seq(from = -5, to = 5, length.out = 1000), df = ClassSize - 1)

, type = "l")

Alpha <- 0.01



XValues <- seq(from = -5, to = 5, length.out = 1000)

TwoTail <- qt(p = c(Alpha / 2, 1 - (Alpha / 2)), df = ClassSize - 1)

TValues <- dt(XValues, df = SampleSize - 1)

plot(x = XValues, y = TValues, type = "n", axes = FALSE, xlab = "", ylab = "")
polygon(x = c(XValues[which(XValues < TwoTail[1])], XValues[which(XValues < TwoTail[1])][length(XValues[which(XValues < TwoTail[1])])], XValues[which(XValues < TwoTail[1])][1]), y = c(TValues[which(XValues < TwoTail[1])], 0, 0), border = NA, col = "grey")
polygon(x = c(XValues[intersect(which(XValues > TwoTail[1]), which(XValues < TwoTail[2]))[1]], XValues[intersect(which(XValues > TwoTail[1]), which(XValues < TwoTail[2]))], XValues[intersect(which(XValues > TwoTail[1]), which(XValues < TwoTail[2]))[length(intersect(which(XValues > TwoTail[1]), which(XValues < TwoTail[2])))]]), y = c(0, TValues[intersect(which(XValues > TwoTail[1]), which(XValues < TwoTail[2]))], 0), border = NA, col = "red")
polygon(x = c(XValues[which(XValues > TwoTail[2])], XValues[which(XValues > TwoTail[2])][length(XValues[which(XValues > TwoTail[2])])], XValues[which(XValues > TwoTail[2])][1]), y = c(TValues[which(XValues > TwoTail[2])], 0, 0), border = NA, col = "grey")





Alpha <- 0.1

pdf("T-distributions.pdf", width = 10, height = 7)

par(mfrow = c(3, 3), mar = c(0.1, 0.1, 0.1, 0.1))

for(SampleSize in c(3, 5, 10)) {
  
  for(Alpha in c(0.5, 0.1, 0.05)) {
    
    
  }

}

dev.off()
