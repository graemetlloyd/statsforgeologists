# COINS

# Enter class data into R:
ZeroHeads <- 2
OneHeads <- 7
TwoHeads <- 15
ThreeHeads <- 6
FourHeads <- 1

# Combine into vector:
ClassFlips <- c(
rep(0, ZeroHeads),
rep(1, OneHeads),
rep(2, TwoHeads),
rep(3, ThreeHeads),
rep(4, FourHeads)
)

# Plot raw data:
hist(ClassFlips + 0.5, breaks = 0:5,
  main = paste("Histogram of N heads from four coin flips\n(for ",
  length(ClassFlips), " students)", sep = ""), xlab = "N heads", border = 0,
  col = rgb(0.5, 0.5, 0.5, 0.5), xlim = c(0, 5), axes = FALSE, plot = TRUE)
axis(side = 2)
axis(side = 1, at = c(0:4) + 0.5, labels = as.character(0:4))

# Plot with expected:
hist(ClassFlips + 0.5, breaks = 0:5,
  main = paste("Histogram of N heads from four coin flips\n(for ",
  length(ClassFlips), " students)", sep = ""), xlab = "N heads", border = 0,
  col = rgb(0.5, 0.5, 0.5, 0.5), xlim = c(0, 5), axes = FALSE, plot = TRUE)
barplot(dbinom(x = 0:4, size = 4, prob = 0.5) * length(ClassFlips),
  add = TRUE, space = 0, col = rgb(1, 0, 0, 0.5), border = 0)
axis(side = 1, at = c(0:4) + 0.5, labels = as.character(0:4))

# DICE

# Enter class data into R:
Ones <- 4
Twos <- 5
Threes <- 6
Fours <- 4
Fives <- 6
Sixes <- 5

# Combine into vector:
ClassRolls <- c(
rep(1, Ones),
rep(2, Twos),
rep(3, Threes),
rep(4, Fours),
rep(5, Fives),
rep(6, Sixes)
)

# Plot raw data:
hist(ClassRolls + 0.5, breaks = 1:7,
  main = paste("Histogram of one dice roll\n(for ",
  length(ClassRolls), " students)", sep = ""), xlab = "N heads", border = 0,
  col = rgb(0.5, 0.5, 0.5, 0.5), xlim = c(1, 7), axes = FALSE, plot = TRUE)
axis(side = 2)
axis(side = 1, at = c(1:6) + 0.5, labels = as.character(1:6))

# Plot raw data:
hist(ClassRolls + 0.5, breaks = 1:7,
  main = paste("Histogram of one dice roll\n(for ",
  length(ClassRolls), " students)", sep = ""), xlab = "N heads", border = 0,
  col = rgb(0.5, 0.5, 0.5, 0.5), xlim = c(1, 7), axes = FALSE, plot = TRUE)
barplot(c(0, rep(1 / 6, 6) * length(ClassRolls)),
  add = TRUE, space = 0, col = rgb(1, 0, 0, 0.5), border = 0)
axis(side = 2)
axis(side = 1, at = c(1:6) + 0.5, labels = as.character(1:6))
