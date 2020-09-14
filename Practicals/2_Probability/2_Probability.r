################################################################################
#                                                                              #
#                          PRACTICAL II - PROBABILITY                          #
#                                                                              #
################################################################################

################################################################################
#                                                                              #
#                               TODAY'S AIMS                                   #
#                                                                              #
# - LEARN HOW TO USE R TO CALCULATE PROBABLILITY EXPECTATIONS AND INFERENCES   #
# - LEARN HOW TO GENERATE PROBABILITY DISTRIBUTIONS IN R                       #
# - LEARN HOW TO INTERPRET AND EXPLOIT PROBABILITY DISTRIBUTIONS IN R          #
#                                                                              #
################################################################################

# Before we begin today's practical it is probably worth reusing something we
# learned last week: how to set the working directory. In case you forgot, you
# can do this by (on a Windows machine) using Session > Set Working Directory >
# Choose Directory.... This is where any outputs (e.g., a PDF of plots) will
# appear. You can check this worked using getwd:
getwd()

# In lecture we used real coins to generate a probability inference, but like
# most repetitive tasks the same thing can be done much faster with a computer.
# We can simulate coin flipping by writing our own R function like this:
CoinFlipper <- function(NFlips = 1, pHeads = 0.5) {
  return(c("Tails", "Heads")[rbinom(n = NFlips, 1, prob = pHeads) + 1])
}

# Lets test it using the values from class (4 flips, assuming a fair coin,
# i.e., where the probability of heads is 0.5):
CoinFlipper(NFlips = 4, pHeads = 0.5)

# You should get a sequence of four values each of either "Heads" or "Tails".
# Note that this is randomly generated. So if we run it multiple times we
# should get different answers (just as we would flipping a real coin):
CoinFlipper(NFlips = 4, pHeads = 0.5)
CoinFlipper(NFlips = 4, pHeads = 0.5)
CoinFlipper(NFlips = 4, pHeads = 0.5)
CoinFlipper(NFlips = 4, pHeads = 0.5)
CoinFlipper(NFlips = 4, pHeads = 0.5)

# The power of computers is that we can do this a very large number of times.
# Let's start with just ten REPLICATES (you can think of these as individual
# trials, i.e., ten students flipping a coin four times). This time though
# we will just count the number of Heads:
apply(matrix(CoinFlipper(NFlips = 4 * 10, pHeads = 0.5),
  nrow = 10) == "Heads", 1, sum)

# You should get a series of numbers of any value between 0 and 4. Again, this
# is randomly generated so if we repeat the process the answer should change.
# Let's do that, but this time store the data in a variable as well:
FourFlipsTenTimes <- apply(matrix(CoinFlipper(NFlips = 4 * 10, pHeads = 0.5),
  nrow = 10) == "Heads", 1, sum)
FourFlipsTenTimes

# Remembering Lesson #1 from lecture it is more helpful to visualise the data
# to help us understand what is going on. A histogram is what we want here:
hist(FourFlipsTenTimes, breaks = seq(from = -0.5, to = 4.5, length.out = 6),
  main = "Histogram of N heads from four coin flips\n(for ten replicates)",
  xlab = "N heads")

# Note we are using an extra option here ("breaks") to make sure our data are
# plotted in bins that correspond to our full range of possible values (0 to
# 4). We can compare this with an expectation using the PROBABILITY DENSITY
# FUNCTION for the BINOMIAL:
dbinom(x = 0:4, size = 4, prob = 0.5)

# This is exactly the same as the one we worked out on the whiteboard during
# lecture and gives us the probability of each outcome (from 0 heads to 4
# heads, the full *possible* range), but to fully turn it into an expectation
# we need to multiply it by the sample size (10), i.e., the number of
# replicates:
dbinom(x = 0:4, size = 4, prob = 0.5) * 10

# We can plot this as an overlay on our data to directly compare the two:
hist(FourFlipsTenTimes + 0.5, breaks = 0:5,
  main = "Histogram of N heads from four coin flips\n(for ten replicates)",
  xlab = "N heads", border = 0, col = rgb(0.5, 0.5, 0.5, 0.5),
  xlim = c(0, 5), axes = FALSE, plot = TRUE)
barplot(dbinom(x = 0:4, size = 4, prob = 0.5) * 10, add = TRUE, space = 0,
  col = rgb(1, 0, 0, 0.5), border = 0)
axis(side = 1, at = c(0:4) + 0.5, labels = as.character(0:4))

# Our data (the inference) is in grey and the expectation (from probability) is
# in red. You should see that they do not match up particularly well. But what
# happens if we increase the number of replicates? I.e., imagine a classroom of
# 100 students all flipping a coin 4 times:
N_reps <- 100
FourFlipsNTimes <- apply(matrix(CoinFlipper(NFlips = 4 * N_reps, pHeads = 0.5),
  nrow = N_reps) == "Heads", 1, sum)
hist(FourFlipsNTimes + 0.5, breaks = 0:5,
  main = paste("Histogram of N heads from four coin flips\n(for ", N_reps,
  " replicates)", sep = ""),
  xlab = "N heads", border = 0, col = rgb(0.5, 0.5, 0.5, 0.5),
  xlim = c(0, 5), axes = FALSE, plot = TRUE)
barplot(dbinom(x = 0:4, size = 4, prob = 0.5) * N_reps, add = TRUE, space = 0,
  col = rgb(1, 0, 0, 0.5), border = 0)
axis(side = 1, at = c(0:4) + 0.5, labels = as.character(0:4))

# What happened? Do they match better? What if we go bigger and do 10000?:
N_reps <- 10000
FourFlipsNTimes <- apply(matrix(CoinFlipper(NFlips = 4 * N_reps, pHeads = 0.5),
  nrow = N_reps) == "Heads", 1, sum)
hist(FourFlipsNTimes + 0.5, breaks = 0:5,
  main = paste("Histogram of N heads from four coin flips\n(for ", N_reps,
  " replicates)", sep = ""), xlab = "N heads", border = 0,
  col = rgb(0.5, 0.5, 0.5, 0.5), xlim = c(0, 5), axes = FALSE, plot = TRUE)
barplot(dbinom(x = 0:4, size = 4, prob = 0.5) * N_reps, add = TRUE, space = 0,
  col = rgb(1, 0, 0, 0.5), border = 0)
axis(side = 1, at = c(0:4) + 0.5, labels = as.character(0:4))

# You should now see they match so closely it is hard to distinguish the grey
# and the red. We are in an unusual situation here as we know that our
# probability inference (that we make from the virtual coin flips) *should*
# perfectly match our probability expectation (calculated using the binomial).
# This is because we know the probability of our virtual coin coming up heads
# is exactly 0.5 as that is what we set with our function. In reality we often
# only have an inference and not an expectation, but what we can see here is
# that if our SAMPLE (the total number of values we were able to measure) is
# large enough (10,000 in this case) we will tend to be very close to the true
# answer. Let's put this notion to the test by looking at some data generated
# by another set of virtual coin flips where the probability of heads is
# different (i.e., not 0.5) and see if we can guess what it is:
BiasedCoinFlips <- c(0, 0, 1, 0, 1, 1, 2, 0, 2, 2, 2, 1, 0, 2, 0, 0, 2, 0, 0,
  1, 0, 0, 1, 1, 2, 2, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 2, 0, 1,
  1, 1, 1, 1, 0, 2, 1, 1, 1, 1, 1, 0, 1, 1, 2, 1, 2, 0, 3, 0, 1, 1, 0, 3, 3,
  0, 1, 1, 1, 1, 0, 2, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 2,
  1, 1, 0, 1, 0, 0)

# These correspond to the same basic framework (number of heads from four flips)
# and 100 total replicates (i.e., a sample size of 100). Let's start by plotting
# the data:
hist(BiasedCoinFlips + 0.5, breaks = 0:5, main =
  "Histogram of N heads from four flips of a biased coin\n(for 100 replicates)",
  xlab = "N heads", border = 0, col = rgb(0.5, 0.5, 0.5, 0.5),
  xlim = c(0, 5), axes = FALSE, plot = TRUE)
axis(side = 1, at = c(0:4) + 0.5, labels = as.character(0:4))
axis(side = 2)

# We should see this looks more "skewed" than our previous fair coin (p(Heads =
# 0.5) example. Specifically, there are lower numbers of heads. So how can we
# infer p(Heads) for this biased coin? One way might be to generate
# probability distributions for hypothetical values of p(Heads) and overlay
# them. Specifically it looks like p(Heads) < 0.5 so we can try every value from
# 0.0 to 0.5 (incrementing by 0.1). As we need to generate six plots we will
# output this as a PDF using:
pdf("BiasedCoinFlips.pdf")
for(pHeads in seq(from = 0, to = 0.5, length.out = 6)) {
  hist(BiasedCoinFlips + 0.5, breaks = 0:5,
    main = "Histogram of N heads from four flips of a biased coin",
    xlab = "N heads", border = 0,
    col = rgb(0.5, 0.5, 0.5, 0.5), xlim = c(0, 5), axes = FALSE, plot = TRUE)
  barplot(dbinom(x = 0:4, size = 4, prob = pHeads) * 100, add = TRUE,
    space = 0, col = rgb(1, 0, 0, 0.5), border = 0,
    main = paste("\n\n(for 100 replicates; overlay p = ",
    pHeads, ")", sep = ""))
  axis(side = 1, at = c(0:4) + 0.5, labels = as.character(0:4))
}
dev.off()

# Note that we are using another major R (and programming in general) feature
# here called a FOR LOOP (the "for" part in the above block of code). This
# allows us to "loop" through a series of values, performing an otherwise
# identical task. Here the value we loop through is the probability of heads
# (from 0.0 to 0.5). Now let's look at the file:
browseURL("BiasedCoinFlips.pdf")

# (If the above doesn't work navigate to the working directory yourself and
# double click on the file.) If we now page through the PDF we should see the
# only thing that changes is the probability expectation (red) for each value
# of p(Heads), listed in the plot title. Can you make a clear determination
# of what the true value of p(Heads) likely is? (I.e., where do the red and
# grey best match up?) If not, can you at least rule out some of the options?
#
# Now let's imagine we were able to obtain some more data on the coin, this
# time the outcome of 1000 sets of 4 flips:
BiasedCoinFlips <- c(0, 0, 1, 0, 2, 1, 0, 0, 1, 1, 0, 1, 0, 2, 1, 0, 2, 1, 2,
  1, 1, 0, 0, 0, 0, 0, 1, 0, 2, 1, 0, 1, 1, 1, 1, 1, 3, 1, 0, 1, 0, 0, 1, 1,
  0, 0, 1, 0, 2, 1, 1, 1, 2, 0, 1, 1, 1, 0, 0, 1, 1, 0, 2, 0, 1, 1, 1, 1, 1,
  2, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 2, 0, 1, 0, 0, 0, 0, 1, 0, 2, 0, 0, 2, 0,
  1, 2, 1, 0, 1, 2, 2, 3, 0, 2, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1,
  0, 0, 0, 1, 0, 0, 0, 2, 0, 2, 0, 2, 1, 3, 1, 0, 0, 2, 0, 2, 2, 1, 0, 1, 1,
  1, 1, 0, 2, 2, 1, 1, 0, 2, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 2, 0, 1,
  2, 0, 2, 0, 2, 1, 0, 1, 1, 1, 1, 2, 3, 0, 1, 1, 0, 2, 1, 0, 0, 1, 2, 0, 0,
  1, 2, 1, 2, 1, 1, 0, 1, 3, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 2,
  1, 1, 3, 0, 0, 3, 1, 0, 1, 0, 0, 1, 0, 2, 2, 1, 1, 0, 1, 1, 2, 2, 0, 2, 2,
  1, 0, 1, 1, 0, 1, 0, 0, 0, 2, 0, 0, 2, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1,
  1, 1, 1, 0, 1, 2, 1, 0, 0, 2, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 2, 0, 0, 0, 0,
  0, 0, 0, 1, 1, 2, 0, 1, 1, 1, 0, 0, 2, 0, 0, 0, 1, 0, 2, 0, 0, 0, 2, 0, 0,
  2, 1, 0, 1, 0, 2, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 0, 0, 0, 0, 1, 0, 0, 1, 0,
  1, 0, 1, 2, 0, 2, 1, 0, 1, 0, 0, 1, 1, 3, 1, 0, 2, 0, 1, 0, 0, 2, 0, 0, 0,
  0, 2, 2, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 2, 3, 1, 0, 2, 1, 0, 0, 1, 1, 0, 0,
  2, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 2, 1, 1, 0, 2, 0,
  0, 1, 0, 1, 1, 2, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 2, 1, 2, 2, 0, 0, 1, 1, 1,
  0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 2, 2, 0, 1, 0, 1, 1, 1, 1, 1, 0,
  2, 2, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 2, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1,
  1, 0, 1, 1, 0, 1, 1, 0, 1, 2, 0, 2, 1, 2, 2, 1, 2, 2, 1, 1, 1, 1, 0, 0, 0,
  1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 2, 1, 2, 0, 2, 2, 0, 1, 1, 0, 1, 1, 0, 1,
  1, 1, 0, 0, 1, 0, 2, 0, 1, 1, 2, 0, 1, 0, 0, 0, 2, 0, 1, 2, 1, 0, 1, 3, 0,
  0, 1, 1, 0, 1, 0, 1, 0, 1, 2, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 2, 1, 3,
  1, 0, 1, 0, 0, 0, 2, 1, 1, 1, 1, 2, 0, 0, 1, 4, 0, 3, 1, 1, 0, 1, 0, 1, 1,
  0, 3, 1, 1, 1, 1, 1, 2, 0, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 0, 2, 0, 1, 0, 0,
  2, 2, 1, 1, 1, 0, 0, 1, 2, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1,
  0, 0, 1, 2, 1, 0, 0, 2, 0, 0, 0, 1, 2, 1, 1, 0, 0, 0, 2, 0, 1, 0, 0, 1, 1,
  1, 1, 2, 1, 1, 1, 0, 2, 1, 1, 2, 0, 1, 1, 0, 1, 2, 1, 0, 3, 0, 0, 2, 2, 1,
  1, 0, 0, 0, 1, 1, 0, 0, 3, 1, 0, 0, 2, 0, 0, 1, 0, 0, 0, 2, 0, 1, 0, 2, 1,
  2, 0, 1, 2, 0, 0, 0, 2, 2, 0, 0, 2, 0, 0, 2, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0,
  1, 0, 0, 2, 1, 0, 2, 1, 0, 0, 1, 0, 1, 3, 2, 1, 1, 0, 0, 1, 0, 1, 2, 1, 1,
  1, 2, 0, 1, 1, 1, 0, 1, 2, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 2, 1, 2, 0, 0,
  0, 1, 1, 2, 2, 2, 2, 0, 3, 2, 1, 0, 1, 1, 1, 2, 1, 1, 0, 1, 0, 2, 1, 1, 2,
  2, 3, 1, 0, 0, 1, 0, 1, 2, 0, 1, 1, 0, 2, 0, 1, 1, 0, 0, 1, 1, 2, 2, 0, 1,
  1, 0, 0, 1, 0, 0, 1, 1, 0, 2, 0, 2, 0, 0, 2, 1, 2, 0, 0, 2, 1, 1, 0, 0, 0,
  1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 2, 2, 1, 1, 2, 1, 1, 1, 2, 2, 1, 1, 0, 0,
  2, 0, 0, 0, 0, 1, 1, 2, 1, 0, 1, 2, 1, 1, 0, 0, 2, 1, 1, 1, 2, 3, 2, 2, 2,
  0, 1, 1, 0, 1, 1, 1, 1, 2, 2, 1, 2, 0, 2, 0, 0, 2, 1, 0, 0, 2, 2, 0, 1, 0,
  0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 2, 1, 0, 0, 1, 0,
  2, 0, 0, 0, 1, 2)

# Let's repeat our plot:
pdf("BiasedCoinFlips.pdf")
for(pHeads in seq(from = 0, to = 0.5, length.out = 6)) {
  hist(BiasedCoinFlips + 0.5, breaks = 0:5,
    main = "Histogram of N heads from four flips of a biased coin",
    xlab = "N heads", border = 0,
    col = rgb(0.5, 0.5, 0.5, 0.5), xlim = c(0, 5), axes = FALSE, plot = TRUE)
  barplot(dbinom(x = 0:4, size = 4, prob = pHeads) * 1000, add = TRUE,
    space = 0, col = rgb(1, 0, 0, 0.5), border = 0,
    main = paste("\n\n(for 1000 replicates; overlay p = ",
    pHeads, ")", sep = ""))
  axis(side = 1, at = c(0:4) + 0.5, labels = as.character(0:4))
}
dev.off()

# And take a look again:
browseURL("BiasedCoinFlips.pdf")

# What would your estimate for p(Heads) be now? Did it get easier to pick a
# single value?
#
# Now let's return to some real data, specifically the dinosaur example from
# lecture. First we will get the data into memory:
DinoSpeciesByGeologicStage <- c(3, 1, 13, 41, 22, 33, 45, 16, 12, 4, 22, 29,
  46, 46, 79, 73, 28, 31, 35, 128, 210, 100, 98, 73, 46, 73, 298, 218)

# We will also need the age data so we can plot the data correctly. Here are
# the midpoints for each geologic stage (in millions of years ago and in the
# same order as the above):
GeologicStageMidpointDates <- c(241, 232.5, 222.25, 210.05, 201.6, 198.05,
  193.05, 186.3, 179.3, 173.6, 169.65, 166.2, 162.95, 158.45, 153.25, 148.15,
  142.85, 138.3, 133.2, 127.5, 118.5, 105.8, 96.55, 91.4, 87.55, 84.65, 77.05,
  68.05)

# We can plot this data with:
plot(x = GeologicStageMidpointDates, y = DinoSpeciesByGeologicStage,
  xlim = c(max(GeologicStageMidpointDates), min(GeologicStageMidpointDates)),
  type = "l", xlab = "Time (Ma)", ylab = "N dinosaur species")

# If you wish you can also try installing and using the geoscale package to
# exactly recreate the plot used in the lecture slide with:
install.packages("geoscale", dependencies = TRUE)
library(geoscale)
geoscalePlot(ages = GeologicStageMidpointDates,
  data = DinoSpeciesByGeologicStage, age.lim = c(238, 72), type = "n",
  cex.age = 1, cex.ts = 1, cex.pt = 1, boxes = "Age",
  units = c("Epoch",  "Period"), label = "N Dinosaur Species",
  tick.scale = 10)
points(x = GeologicStageMidpointDates, y = DinoSpeciesByGeologicStage,
  pch = 20, col = "Black", type = "l", lwd = 3)
points(x = GeologicStageMidpointDates, y = DinoSpeciesByGeologicStage,
  pch = 20, col = "Black", cex = 3)

# As in lecture we are going to simplify this data into a series of increases
# and decreases using the "diff" function, which gives the successive
# differences between a series of values:
diff(DinoSpeciesByGeologicStage)

# Negative values indicate drops in species number, positive ones rises in
# species number. You should also see one zero value (species number staying
# the same) which we deliberately ignored in lecture so we could use the
# Binomial. We can thus get our number of increases with:
sum(diff(DinoSpeciesByGeologicStage) > 0)

# And decreases with:
sum(diff(DinoSpeciesByGeologicStage) < 0)

# This will give us the 14 Increases and 12 Decreases we used in lecture. We
# can generate our binomial using the dbinom (= probability Density for the BINOMial)
# function with:
dbinom(x = 0:26, size = 26, prob = 0.5)

# This is better understood (Lesson #1) visually:
barplot(dbinom(x = 0:26, size = 26, prob = 0.5), space = 0, border = 0,
  xlim = c(0, 26), col = c(rep("Grey", 11), rep("Grey", 5), rep("Grey", 11)),
  names.arg = c(0:26), xlab = "N increases", ylab = "P(X = k)")

# We can generate the numbers for our CUMULATIVE DISTRIBUTION FUNCTION using
# the "cumsum" (i.e., CUMulative SUM) function:
cumsum(dbinom(x = 0:26, size = 26, prob = 0.5))

# Again, plotting will help here:
barplot(cumsum(dbinom(x = 0:26, size = 26, prob = 0.5)), space = 0, border = 0,
  xlim = c(0, 26), col = c(rep("Grey", 11), rep("Grey", 5), rep("Grey", 11)),
  names.arg = c(0:26), xlab = "N increases", ylab = "Cumulative P(X = k)")

# In class we read off values from this plot to find QUARTILES (percentiles of
# the data typically given in 25% increments). But we can do this directly in R
# using the "qbinom" (Quartile BINOMial) function. Here we will give it the two
# values needed to find the middle 50% (i.e., 0.25 and 0.75):
qbinom(p = c(0.25, 0.75), size = 26, prob = 0.5)

# This should return the 11 and 15 increases we found in lecture. I.e., half of
# the probability is concentrated (most dense) across only 5 of the 27 possible
# values (0-26 Increases).
#
# Now instead of a "fair coin" hypothesis of equal probability of increases and
# decreases we will consider a hypothesis from an older publication that found
# a ratio of increases to decreases of 20:4. We can treat this as a probability
# inference, specifically p(Increase) = 20 / 24 (i.e., 0.83333...). Let's
# generate a binomial distribution for this across our 27 values:
barplot(dbinom(x = 0:26, size = 26, prob = 20 / 24), space = 0, border = 0,
  xlim = c(0, 26), col = c(rep("Grey", 11), rep("Grey", 5), rep("Grey", 11)),
  names.arg = c(0:26), xlab = "N increases", ylab = "P(X = k)")

# We can again split this distribution into sections that correspond to
# different interpretations of the data. Specifically, we want to consider
# whether our observed value (14 increases) is consistent with this older
# hypothesis. We can consider the middle 50% of our data again. NB: this 50%
# value is effectively arbitrarily chosen and we might want to pick a larger
# value to better match the spread of density of values around our hypothesised
# p(Increases) value. Here we will use a very commonly applied value - 95%.
# Thus to get this middle 95% we need the number of increases corresponding
# to 2.5% and 97.5%, i.e., probabilities of 0.025 and 0.975. We can use qbinom
# to get the answers:
qbinom(p = c(0.025, 0.975), size = 26, prob = 20 / 24)

# We now get values of 18 and 25. We can visualise this here:
barplot(dbinom(x = 0:26, size = 26, prob = 20 / 24), space = 0, border = 0,
  xlim = c(0, 26), col = c(rep("Grey", 18), rep("Red", 8), rep("Grey", 1)),
  names.arg = c(0:26), xlab = "N increases", ylab = "P(X = k)")

# The red is where 95% of the probabilities sit and we should see clearly that
# our observed value (14 Increases) sits to the left of this. I.e., our data
# seem to fairly clearly disagree with this older hypothesis and specifically
# they show fewer Increases than the hypothesis laid out in the older
# publication.
#
# We can even go a step further and calculate the exact probability that our
# value (14) corresponds to using the inverse of qbinom (which translates a
# probability into a specific outcome) and use "pbinom" (which can translate a
# specific outcome, here 14 Increases) into a probability. We can do this by
# filling in the same values (size = 26, p(Increases) = 20 / 24) and our
# observed value (14):
pbinom(q = 14, size = 26, prob = 20 / 24)

# You should see a very low number (0.0004363638). I.e., even if we had used
# 99.9% instead of 95% we would still decide our older hypothesis is a poor
# explanation for our data. We also already have a better hypothesis
# (p(Increase) = 0.5) that we used in lecture. We can calculate the probability
# that this corresponds to by changing the value for "prob" to 0.5:
pbinom(q = 14, size = 26, prob = 0.5)

# You should get a much larger value (0.7214015). Note that this is not the
# probability of our hypothesised p(Increases) value being true, just where
# in the associated distribution of values it falls. I.e., if our N Increases
# exactly corresponded to the middle value of the distribution then the p would
# be 0.5. We can still conclude, however, that the true value of p(Increases)
# is likely to be much closer to 0.5 than 0.8333 as 0.7214 is closer to 0.5 than
# 0.0004 is.
#
# Now let's consider a different data set: the years of eruption of Mt Etna
# between 1000 AD and 2000 AD:
EtnaEruptionYears <- c(1169, 1169, 1329, 1329, 1536, 1669, 1669, 1693, 1832,
  1843, 1868, 1928, 1929, 1979, 1981, 1984, 1985, 1987, 1991)

# We can find out how many eruptions this is in total by getting the length of
# our vector:
length(EtnaEruptionYears)

# I.e., 19. This corresponds to 1.9 eruptions every 100 years. We can use this
# value to ask questions like: what is the probability of exactly one eruption
# in any 100-year span? To answer this question we are going to use another
# probability distribution called the POISSON. This is intended for use in
# calculating the probability of a finite number of events occurring in discrete
# units of time (as in this example) or space. Like the Binomial and the
# Discrete Uniform it is a discrete probability distribution (i.e., only specific
# integer values are possible), but unlike those two it is only bounded at the
# left. In other words, the minimum possible value is zero, but there is no
# maximum (i.e., like the Normal it theoretically extends to infinity at the
# maximum). We will see what this means for calculating probabilities below.
# We can calculate the probabilitites for 0 to 20 (20 being an arbitrary upper
# value) eruptions using the "dpois" (Density POISson) function which only
# requires us to provide the range of values (k, here N eruptions) over which
# we want to calculate probabilities and the mean number of eruptions
# (represented by the Greek letter lambda in the Poisson's probability density
# function):
dpois(x = 0:20, lambda = 1.9)

# Again, this is better understood visually (Lesson #1):
barplot(dpois(x = 0:19, lambda = 1.9), space = 0, border = 0,
  ylab = "P(X = k)", xlab = "k (N eruptions)")
axis(side = 1, )

# You should see the probablity densities are clumped at the left hand side of
# the plot, i.e., the probablities of 0-5 eruptions in a 100-year span are much
# higher than those of >5 eruptions. Note that although we can't plot them there
# are technically p-values for every value going to infinity, but we can see from
# our plot that anything over 10 eruptions is too small to see visually. We can
# now answer our earlier question by simply plugging in a value of 1 for x in our
# dpois function:
dpois(x = 1, lambda = 1.9)

# I.e., the probability of there being exactly one eruption in a 100-year span
# is 0.2841804. In practice though we might care about whether there is *at
# least* one eruption in a 100-year span. This is where that unbounded right
# creates a problem as to do this we would need to add the probabilities of 1
# eruption, to 2 eruptions, to 3 eruptions etc. off to infinity. However, we
# can use one of our probability rules to make this problem much simpler.
# Specifically the probability of all outcomes must sum to one, thus we can
# calulate the *opposite* of what we want and subtract that from one. I.e., we
# can know the probability of at least one eruption in a 100-year span by
# instead calculating the probability of zero eruptions in a 100-year span -
# the only other possible outcome - and subtract that from 1. Thus, the
# answer is simply:
1 - dpois(x = 0, lambda = 1.9)

# I.e., 0.8504314. That finishes up today's practical, but as a final exercise
# you could go and check to see whether or not Etna has erupted in the current
# 100-year span (i.e., since 2000). What does this mean for our prediction?

################################################################################
#                                                                              #
#                                     MCQ                                      #
#                                                                              #
# You are now ready to attempt this week's MCQ which is a series of ten        #
# multiple choice questions (MCQs) on last week's lecture and this practical.  #
# As last time you will find it on Minerva: SOEE1475 > Statistics resources >  #
# MCQs.                                                                        #
#                                                                              #
################################################################################

