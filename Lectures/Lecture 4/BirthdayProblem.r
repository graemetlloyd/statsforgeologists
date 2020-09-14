BirthdayProblem <- function(k) {
  p <- numeric(k)  # create numeric vector to store probabilities
  for (i in 1:k) {
    q <- 1 - (0:(i - 1)) / 365  # 1 - prob(no matches)
    p[i] <- 1 - prod(q)
  }
  prob <- p[k]
  prob
}

NStudents <- 34

BirthdayProblem(k = NStudents)

NPeeps <- c()

for(i in 1:100) NPeeps[i] <- BirthdayProblem(k = i)

plot(x = 1:100, y = NPeeps, type = "l", lwd = 3, xlab = "N People", ylab = "Probability of at least one shared birthday")
lines(x = c(23, 23), y = c(0, BirthdayProblem(k = 23)), col = "red", lwd = 3)
lines(x = c(1, 23), y = c(BirthdayProblem(k = 23), BirthdayProblem(k = 23)), col = "red", lwd = 3)

plot(x = 1:100, y = NPeeps, type = "l", lwd = 3, xlab = "N People", ylab = "Probability of at least one shared birthday")
lines(x = c(NStudents, NStudents), y = c(0, BirthdayProblem(k = NStudents)), col = "red", lwd = 3)
lines(x = c(1, NStudents), y = c(BirthdayProblem(k = NStudents), BirthdayProblem(k = NStudents)), col = "red", lwd = 3)
