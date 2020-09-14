################################################################################
#                                                                              #
#                      PRACTICAL VII - TEMPORAL STATISTICS                     #
#                                                                              #
################################################################################

################################################################################
#                                                                              #
#                               TODAY'S AIMS                                   #
#                                                                              #
# - LEARN HOW TO TEST FOR DIFFERENT PATTERNS OF EVENTS THROUGH TIME IN R       #
# - LEARN HOW TO PERFORM A KOLOMOGOROV-SMIRNOV TEST IN R                       #
# - LEARN HOW TO TEST FOR NON-RANDOMNESS OF A MARKOV CHAIN IN R                #
#                                                                              #
################################################################################

# Before we start begin by setting your working directory.

# For the first part of this practical we will consider the four main ways in
# which a series of events can be distributed over time (regular/uniform,
# random, clustered, and trend), and how these can be tested for. We will not
# address more complex patterns associated with things like waveforms as they
# are beyond the scope of a first year statistical course. However, you should
# know that these do exist in the geologic record and are studied.
#
# As an example data set for today we will use a database of impact structures
# on Earth:
browseURL("http://www.passc.net/EarthImpactDatabase/New%20website_05-2018/Index.html")

# I have already modified the impact age data from the last 1000 Ma for entry
# into R:
ImpactAges <- sort(c(0.000007, 0.000067, 0.00014, 0.0009, 0.001, 0.0011, 0.0039,
  0.004, 0.0042, 0.0054, 0.0066, 0.007, 0.01, 0.0214, 0.049, 0.05, 0.052,
  0.0635, 0.099, 0.1, 0.105, 0.220, 0.250, 0.27, 0.3, 0.9, 0.999, 1, 1.07, 1.4,
  2.999, 3.0, 3.5, 3.7, 4.998, 4.999, 5, 15, 15.1, 20.5, 34.999, 35, 35.3, 35.7,
  36.4, 37.2, 39, 39.999, 40, 42.3, 45, 45.999, 46, 48.999, 49.0, 50, 50.50,
  46.5, 58, 60, 64.98, 64.999, 65, 65.17, 69.998, 69.999, 70, 70.3, 74.1, 75,
  76.20, 80, 81.0, 83, 89.0, 91, 95, 97, 99, 100, 110, 115, 119.998, 119.999,
  120, 121.0, 123, 128, 142.0, 142.5, 145.0, 149.999, 150, 160, 165, 167, 169,
  170, 180, 189.999, 190, 199.998, 199.999, 200, 201, 214, 220, 230, 249.998,
  249.999, 250, 254.7, 279.999, 280, 289.999, 290, 299.996, 299.997, 299.998,
  299.999, 300, 319.999, 320, 342, 344.999, 345, 351, 359.999, 360, 364, 376.8,
  378, 380, 395, 399.999, 400, 429.999, 430, 445, 449.999, 450, 453, 454.999,
  455, 456.5, 457.999, 458, 465, 469.999, 470, 499.996, 499.997, 499.998,
  499.999, 500, 505, 508, 515, 520, 535, 545, 549.999, 550, 560, 570, 573, 590,
  599.998, 599.999, 600, 646, 700, 999.998, 999.999, 1000))

# And a simple timeline plot can visualise it:
plot(x = ImpactAges, y = rep(0, length(ImpactAges)), type = "n",
  axes = FALSE, xlab = "", ylab = "", xlim = c(1000, 0),
  ylim = c(-500, 500))
lines(x = c(0, 1000), y = c(0, 0), lwd = 2)
lines(x = c(0, 0), y = c(-35, 35), lwd = 2)
lines(x = c(1000, 1000), y = c(-35, 35), lwd = 2)
text(x = c(0, 1000), y = c(-60, -60), labels = c(0, 1000))
text(x = 500, y = -100, labels = "Age (Ma)")
points(x = ImpactAges, y = rep(0, length(ImpactAges)), pch = 20,
  col = "red", cex = 2)

# What kind of pattern do you think this represents?
#
# As in lecture if you have a clear idea you can use just the test you think
# would be most appropriate. However, here we will explore the four main options
# in turn so that you know how to do all of them.
#
# First regular or uniform. We can probably visually reject the idea that the
# data are perfectly regular, although it is worth noting that astronomically
# driven cycles can be very regular (think of the annual regularity of the
# seasons for example). We can begin by plotting the impacts as a cumulative
# number of events:
plot(x = c(1000, sort(ImpactAges, decreasing = TRUE), 0),
  y = c(0:length(ImpactAges), length(ImpactAges)), type = "l",
  xlab = "Time (Ma)", ylab = "Cumulative N events", lwd = 2, xlim = c(1000, 0))

# Next we can plot the expectation under a uniform distribution:
plot(x = c(1000, 0), y = c(0, length(ImpactAges)), type = "l",
  xlab = "Time (Ma)", ylab = "Cumulative N events", lwd = 2, col = "red",
  xlim = c(1000, 0))

# And now the two together to compare them:
plot(x = c(1000, sort(ImpactAges, decreasing = TRUE), 0),
  y = c(0:length(ImpactAges), length(ImpactAges)), type = "l",
  xlab = "Time (Ma)", ylab = "Cumulative N events", lwd = 2, xlim = c(1000, 0))
points(x = c(1000, 0), y = c(0, length(ImpactAges)), type = "l", lwd = 2,
  col = "red")

# Do the two lines look like a good match? We can test this quantitatively using
# the Kolomogorov-Smirnov test (ks.test). Here we will assess the fit between
# these two lines (the D statistic) and the probability that this relationship
# is correct:
ks.test(x = ImpactAges, y = "punif", min = 0, max = 1000)$statistic
ks.test(x = ImpactAges, y = "punif", min = 0, max = 1000)$p.value

# What value of D did you get? Was this higher or lower than you expected?
# (Bear in mind 0 would be perfect agreement and 1 would be maximum
# disagreement.) What about the probability? Does the KS test think there is
# a reasonable chance the data match a uniform distribution? Is this what you
# expected?
#
# Next we can consider a random explanation. We will use the Poisson
# distribution to generate an expectation for this and a Chi-squared test to
# see if the expectation is a good match to the data. As these are both
# discrete approaches we will first need to "bin" the data as we previously did
# with the spatial data. Specifically we will go for a simple ten-bin system
# (every 100 million years from 1000 to 0 Ma). Before we peform a test, though,
# we can visuliase our data as a histogram to give a sense of the numbers of
# impacts in each time bin:
hist(ImpactAges, breaks = seq(from = 1000, to = 0, length.out = 11),
  xlim = c(1000, 0), xlab = "Time (Ma)")

# It might be hard to gauge whether this looks random, but remember that in
# asking this question we completely ignore the order of the values, and only
# consider the overall distribution.
#
# To proceed we can actually get the data we want (the counts from each time
# bin) using the histogram function(hist), but turning off the plotting part:
BinnedImpacts <- hist(ImpactAges,
  breaks = seq(from = 1000, to = 0, length.out = 11), plot = FALSE)$counts
BinnedImpacts

# Here plot = FALSE tells it we don't want a graph but just the data. The seq
# function lets it now we want eleven "breaks" (the boundaries of the bins)
# running between 1000 and 0, i.e.:
seq(from = 1000, to = 0, length.out = 11)

# Finally, the $counts gives us just the counts of (in this case) number of
# impacts in each bin.
#
# This gives us our observed values, but for a Chi-squared test we need some
# expected values too. This needs a single parameter - the mean. In this case
# the total number of impacts divided by the number of time bins. For the
# first part we simply need:
length(ImpactAges)

# And the second is ten, so our lambda parameter is:
length(ImpactAges) / 10

# We can thus generate a plot of the Poisson (random expectation) with:
barplot(height = dpois(x = 0:40, lambda = length(ImpactAges) / 10) * 10,
  border = 0, space = 0, names.arg = 0:40,
  ylab = "Expected bins with N impacts", xlab = "N impacts per bin")

# By contrast the observed distribution looks like this:
barplot(height = apply(apply(matrix(BinnedImpacts), 1, '==', 0:40), 1, sum),
  border = 0, space = 0, names.arg = 0:40, ylab = "Bins with N impacts")

# Immediately we should see a couple of things here: 1) Our observed and
# expected distributions look very diffreent, and 2) There is a lcear lack
# data in our observed values (only ten values spread over 41 bins (0-40).
# The former suggests (as you may already have guessed) that our data aren't
# random, but the latter reminds of another issue with testing for random
# this way - we want a bigger sample size. However, we should remember that
# samll sample sizes more typcially lead to Type II errors, so if we do
# reject the null we might not be too worried about this.
#
# We can generate new variables to store the expected and observed impacts:
ExpectedImpacts <- dpois(x = 0:40, lambda = length(ImpactAges) / 10) * 10
ObservedImpacts <- apply(apply(matrix(BinnedImpacts), 1, '==', 0:40), 1, sum)
ExpectedImpacts
ObservedImpacts

# Next we can begin to calculate our Chi-squared values by subtracting the
# former from the latter:
ObservedImpacts - ExpectedImpacts

# Take the squares:
(ObservedImpacts - ExpectedImpacts) ^ 2

# Divide by the expected values:
(ObservedImpacts - ExpectedImpacts) ^ 2 / ExpectedImpacts

# Take the absolute values:
abs((ObservedImpacts - ExpectedImpacts) ^ 2 / ExpectedImpacts)

# And finally the sum:
sum(abs((ObservedImpacts - ExpectedImpacts) ^ 2 / ExpectedImpacts))

# This is our Chi-squared value. How does this compare to previous Chi-squared
# values? Why might this be? (NB: think about the number of bins we are using.)
# To interpret this we still need to infer our degrees of freedom. Remember
# that this is not the number of time bins, but the range of values we used for
# our Poisson distribution (i.e., number of bins with that many impacts in).
# Here we used the range 0 to 40 (41 values in total). So our degrees of
# freedom are 40:
ImpactDF <- 40

# We will use a more stringent Alpha than previously (0.001):
ImpactAlpha <- 0.001

# And produce our Chi-squared distribution plot (with critical value) as we
# have done previously:
ChiSqRange <- seq(from = 0, to = 100, length.out = 1000)
plot(x = ChiSqRange, y = dchisq(x = ChiSqRange, df = ImpactDF), type = "n",
  ylab = "f(x)", xlab = "Chi-squared value",
  main = paste("Null: impacts are randomly distributed\ndf = ", ImpactDF + 1,
  " - 1; Alpha = ", ImpactAlpha, "; right-tailed test", sep = ""))
polygon(x = c(0, ChiSqRange, 30, 0), y = c(0, dchisq(x = ChiSqRange,
  df = ImpactDF), 0, 0), border = NA, col = "grey")
  CriticalValue <- qchisq(p = 1 - ImpactAlpha, df = ImpactDF)
polygon(x = c(CriticalValue, ChiSqRange[ChiSqRange >= CriticalValue], 30,
  CriticalValue), y = c(0, dchisq(x = ChiSqRange[ChiSqRange >= CriticalValue],
  df = ImpactDF), 0, 0), border = NA, col = "red")
lines(x = c(CriticalValue, CriticalValue), y = c(0,
  max(dchisq(x = ChiSqRange, df = ImpactDF)) * 0.95), col = "blue", lwd = 3)
text(x = CriticalValue, y = max(dchisq(x = ChiSqRange, df = ImpactDF)),
  labels = round(CriticalValue, 2), col = "blue")

# Where does our Chi-squared value fall with respect to the critical value?
# Does this mean we accept or reject the null? How close were we to doing the
# opposite? What does this mean for the possibility that impacts on Earth have
# occurred randomly through the last billion years?
#
# Next we will consider whether impacts may be clustered in time. To do this
# we will modify the Nearest Neighbour Index (NNI) we used last week for 1D
# data. First we can "trick" R into thinking we have xy coordinates for our
# data, even though we will treat our y values as all equal (i.e., placing
# everything on a single line and hence keeping the data effectively one-
# dimensional):
cbind(ImpactAges, rep(0, length(ImpactAges)))

# I.e., the second column here represents our "y" values. Then we can get a
# distance matrix like before with:
dist(cbind(ImpactAges, rep(0, length(ImpactAges))))

# And modify this to get nearest neighbour distances with:
apply(as.matrix(dist(cbind(ImpactAges, rep(0, length(ImpactAges))))), 2,
  sort)[2, ]

# And take the mean and store it with:
MeanNND <- mean(apply(as.matrix(dist(cbind(ImpactAges, rep(0,
  length(ImpactAges))))), 2, sort)[2, ])
MeanNND

# Next we want our expected value (assuming a random distribution). This is
# going to be our "Area" (the length of our sampling window, 1000 million
# years) divided by the number of events (175 total impacts) halved:
ExpectedNND <- 0.5 * (1000 / length(ImpactAges))
ExpectedNND

# Then we can get our NNI, R simply by dividing the former by the latter:
MeanNND / ExpectedNND

# What interpretation can we attach to this value? Does this match with our
# earlier visual interpretations of the data? What about with our previous
# test for uniform (approaching R = 2) or random (R around 1)?
#
# Finally, we will consider whether our data represent a trend. To aid this
# a visualisation of the gaps between impacts over time can help a lot. We can
# begin to get at this with the "diff" function which gives the difference
# between successive values:
rev(diff(ImpactAges))

# Note that I have reversed these (with rev) so they are in temporal order.
# This is due to a peculiar issue with the way geologists use ages as
# increasing values backwards in time instead of negative values. It might
# be hard to make much of a determination from this data without plotting it
# so let's do that with:
plot(x = 1:(length(ImpactAges) - 1), y = rev(diff(ImpactAges)), pch = 20,
  cex = 2, col = "red", xlab = "Impacts in rank order",
  ylab = "Interval between impacts (Myr)")

# This is not easy to interpret as the data do not fall out on a clear straight
# line, but also most points seem to hug the x-axis. This is a useful time to
# learn that we can plot one or both of our axes on a log scale using the log
# option. Here we want to plot the y-axis on a log scale (log = "y"), but you
# can also plot the x-axis this way (log = "x"), or even both (log = "xy"):
plot(x = 1:(length(ImpactAges) - 1), y = rev(diff(ImpactAges)), pch = 20,
  cex = 2, col = "red", xlab = "Impacts in rank order",
  ylab = "Interval between impacts (Myr)", log = "y")

# This helps, but also adds some complexity in interpretation. You should see
# most of the points follow something of a curve. Note, that as we are about
# to apply a Spearman's rank correlation (and not a regression) we don't need
# to worry about this feature of the data. (Unless of course that curve
# changed direction across the plot!) However, there is a second, weird feature
# of a horizonal line at 1e-03. This is due to the repeated appearance of a gap
# of 1000 years between events. This is likely some odd artefact of the data.
#
# The important point is that overall gaps between events do seem to reduce
# over time. If this is the case does this represent an increase or decrease
# in impact frequency? We can test this with a correlation:
cor.test(x = 1:(length(ImpactAges) - 1), y = rank(diff(ImpactAges)),
  method = "pearson")$estimate

# You should get a positive value, but remember that our ages get smaller as
# they get younger so effectively this represents a negative correlation and
# hence suggests increasing frequency of impacts. However, as we learned in
# lecture Pearson's correlation coefficent (r) is actually not appropriate
# here as our x-values are definitively not normally distributed. This is
# because they just represent the gaps between events as a rank order variable:
1:(length(ImpactAges) - 1)

# Thus we should actually use the Spearman rank correlation (r') instead:
cor.test(x = 1:(length(ImpactAges) - 1), y = rank(diff(ImpactAges)),
  method = "spearman")$estimate

# The value returned is the same as the Pearson from above, showing that the
# correlation is effectively the same type of test, but using ranks as input.
# Furthermore, this is a positive value (really a negative correlation), but
# suggests the correlation is not neat (close to one).
#
# You should also get a warning about ties, meaning the p-value (r' = 0) cannot
# be exactly calculated. We can still estimate it though with the exact =
# FALSE option as we have done previously:
cor.test(x = 1:(length(ImpactAges) - 1), y = rank(diff(ImpactAges)),
  method = "spearman", exact = FALSE)$p.value

# This is the probability that the true correlation coefficient is zero. Thus
# if we use our Alpha from before (0.001) we can ask whether the data do...:
cor.test(x = 1:(length(ImpactAges) - 1), y = rank(diff(ImpactAges)),
  method = "kendall", exact = FALSE)$p.value > 0.001

# ...or do not...:
cor.test(x = 1:(length(ImpactAges) - 1), y = rank(diff(ImpactAges)),
  method = "kendall", exact = FALSE)$p.value < 0.001

# ...conform to the null (true correlation equals zero). What does this mean for
# the idea that our data follow a trend? If we do accept a trend which way
# are the data trending? (I.e., increasing or decreasing frequency of impacts.)
# Finally, given our suspicions about the Etna data from lecture are we happy
# that the data we have faithfully records the frequency of impacts on Earth
# over the last billion years? If not, what factors might effect this? Are
# these temporally biased? What else can we use to estimate the frequency at
# which the Earth has been struck by meteorites?
#
# NB: Recently a paper came out suggesting impacts have been increasing over
# the last 300 m.y., but using lunar craters as evidence. If you are interested
# you can find it here:
browseURL("http://science.sciencemag.org/content/363/6424/253")

# Now we will move on to considering Markov chains.
#
# After a week long field trip you synthesise your notes and measurements into
# a single sedimentary log consisting of Coal (C), Limestone (L), and Shale
# (S). You suspect there is a pattern to the sequence of these three major
# rock types and set out to test it using Markov Chains. You start by
# considering your three main rock types as the three discrete states of your
# sedimentary system. After checking for way-up criteria, slumps, and faulting
# you are happy that your sedimentary log conforms to the theory of
# superposition and hence the units are in temporal order. You thus write out
# the time sequence (from oldest to youngest) as follows:
#
# S-L-C-S-S-L-C-S-L-L-C-S-S-L-L-C-S-L-L-C-S-L-C
#
# You begin by constructing the transition frequency matrix (modify the
# following before copy-and-pasting into R):
SedimentaryTranFreqMatrix <- matrix(c(
0, 0, 0, # N transitions from C to: C, L, and S, respectively
0, 0, 0, # N transitions from L to: C, L, and S, respectively
0, 0, 0  # N transitions from S to: C, L, and S, respectively
), byrow = TRUE, ncol = 3, dimnames = list(c("C", "L", "S"), c("C", "L", "S")))
SedimentaryTranFreqMatrix

# It should immediately be obvious by the various zero values that there is
# some structure to the transitions (i.e., despite over 20 transistions in total
# many pathways are never followed). However to know for sure we will continue by
# calculating the transition probability matrix. To do this we will first need the
# row totals:
apply(SedimentaryTranFreqMatrix, 1, sum)

# Now we can divide through by these:
SedimentaryTranProbMatrix <- SedimentaryTranFreqMatrix /
  apply(SedimentaryTranFreqMatrix, 1, sum)
SedimentaryTranProbMatrix

# It is recommended at this stage that you draw out the Markov chain here,
# omitting arrows corresponding to a transition probability of zero. What
# can you see when you do this?
#
# Next you can test whether or not this pattern is distinguishable from random.
# To do this you need to generate an expected transition frequency matrix, but
# first you can start with the expected transition probability matrix. The key
# here is that under a simple random process we consider each transition equally
# likely and hence as there are three possible transitions from each state (to
# C, L, or S) the expected random probability is going to be one-third for
# every one:
ExpectedTranProbMatrix <- matrix(c(
1 / 3, 1 / 3, 1 / 3,
1 / 3, 1 / 3, 1 / 3,
1 / 3, 1 / 3, 1 / 3
), byrow = TRUE, ncol = 3, dimnames = list(c("C", "L", "S"), c("C", "L", "S")))
ExpectedTranProbMatrix

# Note that to convert this to a transition frequency matrix you might naively
# assume you simply multiply each value by the total number of transitions:
ExpectedTranProbMatrix * 22

# This is wrong for two reasons. Firstly, this actually contains 66 predicted
# transitions as we are effectively duplicating every row (three times twenty-
# two). Secondly, the frequency of each rock type in the sequence is not equal
# and so the number of possible transitions is not equal for every row.
# Specifically, there are five transitions *from* C, 9 from L, and 8 from S.
# These are the values you need to use to get your expected transition frequency
# matrix:
ExpectedTranFreqMatrix <- ExpectedTranProbMatrix * c(5, 9, 8)
ExpectedTranFreqMatrix

# You can now confirm the row totals match for both your real and expected data:
apply(SedimentaryTranFreqMatrix, 1, sum)
apply(ExpectedTranFreqMatrix, 1, sum)

# You thus now have expected and observed transition frequency matrices,
# allowing you to perform a Chi-squared test where the null model is random
# transitions and hence the alternative is (some) non-random bias to the
# transitions. As always you can begin by doing a quick visual inspection of
# the two matrices to give you some prior guess as to what the answer should be:
SedimentaryTranFreqMatrix
ExpectedTranFreqMatrix

# You should see some individual observed and expected values are quite close,
# but generally they seem quite different. You can now begin to calculate your
# Chi-squared value. First, you can subtract the expected from each observed
# value:
SedimentaryTranFreqMatrix - ExpectedTranFreqMatrix

# Then get the square of each of these:
(SedimentaryTranFreqMatrix - ExpectedTranFreqMatrix) ^ 2

# Divide by the expected value:
(SedimentaryTranFreqMatrix - ExpectedTranFreqMatrix) ^ 2 /
  ExpectedTranFreqMatrix

# Take the absolute value of these:
abs((SedimentaryTranFreqMatrix - ExpectedTranFreqMatrix) ^ 2 /
  ExpectedTranFreqMatrix)

# And finally, the sum:
sum(abs((SedimentaryTranFreqMatrix - ExpectedTranFreqMatrix) ^ 2 /
  ExpectedTranFreqMatrix))

# Take a note of this value. It should be larger than zero, but as always we
# need to know the degrees of freedom to place it in context. Previously we
# have encountered the general rule of degrees of freedom (n - 1) so you might
# think the correct value here is 8 (9 - 1). However, things are more
# complicated here than previously. Specifically, there are greater
# constraints on a transition matrix than a set of grid cells or time bins. I.e.,
# there are degrees of freedom for each row - if you know there are five
# transitions from C and you already have values for L and S then there is no
# freedom for the final value to vary. But there are also constraints for each
# column. If we know there are three transitions to S and we have already recorded
# two of these there is no freedom for the last one to vary either. Thus the rule
# here is more restrictive: for an n x n matrix there are (n - 1) ^ 2 degrees of
# freedom. Calculate this value then add it into the line below and copy and
# paste it into R:
DF <-

# You should also specify a value for Alpha of your choosing:
Alpha <-

# You are now ready to interpret your Chi-squared value:
ChiSqRange <- seq(from = 0, to = 30, length.out = 1000)
plot(x = ChiSqRange, y = dchisq(x = ChiSqRange, df = DF), type = "n",
  ylab = "f(x)", xlab = "Chi-squared value",
  main = paste("Null: transitions are randomly structured\ndf = ", DF + 1,
  " - 1; Alpha = ", Alpha, "; right-tailed test", sep = ""))
polygon(x = c(0, ChiSqRange, 30, 0), y = c(0, dchisq(x = ChiSqRange, df = DF),
  0, 0), border = NA, col = "grey")
CriticalValue <- qchisq(p = 1 - Alpha, df = DF)
  polygon(x = c(CriticalValue, ChiSqRange[ChiSqRange >= CriticalValue], 30,
  CriticalValue), y = c(0, dchisq(x = ChiSqRange[ChiSqRange >= CriticalValue],
  df = DF), 0, 0), border = NA, col = "red")
lines(x = c(CriticalValue, CriticalValue), y = c(0,
  max(dchisq(x = ChiSqRange, df = DF)) * 0.95), col = "blue", lwd = 3)
text(x = CriticalValue, y = max(dchisq(x = ChiSqRange, df = DF)),
  labels = round(CriticalValue, 2), col = "blue")

# Which side of the critical value did your Chi-squared value fall? Did you
# accept or reject the null? What does this mean for your sedimentary sequence?
# Is this what you expected intuitively from looking at the sequence? Try
# changing alpha above and repeating until you change your acceptance/
# rejection of the null. At what value does this occur? What does this mean for
# your confidence in the result?

################################################################################
#                                                                              #
#                                     MCQ                                      #
#                                                                              #
# You are now ready to attempt this week's MCQ which is a series of ten        #
# multiple choice questions (MCQs) on last week's lecture and this practical.  #
#                                                                              #
################################################################################
