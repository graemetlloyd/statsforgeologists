################################################################################
#                                                                              #
#              PRACTICAL V - NULL HYPOTHESIS STATISTICAL TESTING               #
#                                                                              #
################################################################################

################################################################################
#                                                                              #
#                               TODAY's AIMS                                   #
#                                                                              #
# - LEARN TO APPLY NULL HYPOTHESIS STATISTICAL TESTING TO UNIVARIATE DATA      #
# - LEARN TO APPLY NULL HYPOTHESIS STATISTICAL TESTING TO BIVARIATE DATA       #
# - LEARN HOW TO CONSIDER TYPE I AND TYPE II ERROR RATES                       #
#                                                                              #
################################################################################

# Before we start begin by setting your working directory.

# Today we will focus on re-examining our previous example data sets through
# the lens of Null Hypothesis Statistical Testing (NHST). But first we will
# go over the birthday problem from lecture. Intuitively we should know that
# amongst a group of 366 people there must be at least one shared birthday
# (ignoring the 29th February as this makes the maths somewhat difficult).
# In lecture we found that even amongst a group size of 23 people at least
# one shared birthday is more likely than not (p > 0.5). This may surprise
# you, but if we work through the probability calculation we can start to see
# why this is the case,
#
# In lecture we discovered that the problem is at first blush very
# complicated. We need to add together the probability of two people sharing
# a birthday to three people sharing a birthday and so on up to the group
# size (i.e., for 23 people this would be 22 separate calculations). But
# remembering our probability rules from Lecture 1 there is a much simpler
# way to do this: calculate the *opposite* of what we want and subtract that
# probability from one. I.e., if we know the probability of everyone having
# a unique birthday this must be one minus the probability of at least one
# shared birthday (the value we actually want).
#
# We can work through this in code form starting from the simplest scenario,
# a group of one person. The probability they have a unique borthday is,
# simply:
1

# Next a group of two people. Here we can consider the first person as being
# "allowed" to have a birthday on any day:
365 / 365

# This gets us our one from above. Then the second person can have a birthday
# on all but one day of the year (i.e., that of the first person):
364 / 365

# To combine our data we need to know if we are applying the OR or the AND
# rule. The key statement should be that the first person can have their
# birthday on any day AND the second person on all but one day. It is the AND
# rule, so we need to multiply:
(365 / 365) * (364 / 365)

# Given we will be subtracting these probabilities from one so far this seems
# to meet most people's intuitive expectations (i.e., a very low probability
# of a shared birthday). By adding a third person we are removing another day
# that they could have their birthday on and again we will multiply:
(365 / 365) * (364 / 365) * (363 / 365)

# We can now start to see a general pattern emerging of multiplying fractions
# of 365 with the numerator declining by one as successive people are added to
# the group. We can thus gather this information into a function that will do
# this job of iterating through successive people for us:
BirthdayProblem <- function(NIndividuals) {
  return(1 - prod((366 - 1:NIndividuals) / 365))
}

# Note that we have also added a "1 -" term to get us to the probability we
# want (i.e., at least one shared birthday). Let's test it with the three
# values we already worked out, 1, 2, and 3:
BirthdayProblem(NIndividuals = 1)
BirthdayProblem(NIndividuals = 2)
BirthdayProblem(NIndividuals = 3)

# This should confirm that the function works. Now we can enter our class
# size from lecture (15) into it to see how surprised we should be to find a
# shared birthday:
BirthdayProblem(NIndividuals = 15)

# You should see that it is higher than most people would guess. I.e., we should
# not have been that surprised to find a shared birthday. If everyone had showed
# up (27 registered students) the probability grows substantially:
BirthdayProblem(NIndividuals = 27)

# Probability exercises like this remind us not to always rely on our intuitions
# as to how likely an event is and are thus extensible to more scientific problems
# where knowing the likelihood of a particular outcome can have more far reaching
# implications.
#
# We are now going to revisit some data from previous practicals to re-examine
# them under NHST, but first we need to get them into memory. First our dinosaur
# species number data:
DinoSpeciesByGeologicStage <- c(3, 1, 13, 41, 22, 33, 45, 16, 12, 4, 22, 29,
  46, 46, 79, 73, 28, 31, 35, 128, 210, 100, 98, 73, 46, 73, 298, 218)
GeologicStageMidpointDates <- c(241, 232.5, 222.25, 210.05, 201.6, 198.05,
  193.05, 186.3, 179.3, 173.6, 169.65, 166.2, 162.95, 158.45, 153.25, 148.15,
  142.85, 138.3, 133.2, 127.5, 118.5, 105.8, 96.55, 91.4, 87.55, 84.65, 77.05,
  68.05)

# Then our profession heights data:
ClassHeights <- c(71, 69, 73, 67, 61, 77, 74, 68.5, 68, 71, 64, 70, 62, 64, 67,
  69, 63, 63, 66, 69, 68, 72)
BasketballHeights <- c(75, 76, 76, 76, 81, 78, 77, 84, 84, 82, 76, 75, 78, 79,
  83, 80, 75, 76, 73, 77, 70, 74, 77, 74, 76, 69, 70, 68)
JockeyHeights <- c(64, 65, 62, 63, 64, 68, 66, 61, 62, 66, 63, 67, 65, 64, 68,
  66, 66, 63)

# Now your own ammonite quartet data. (You will have to modify both lines
# to replace the ammonite numbers with the diameters and N chambers you got
# from practicals 3 and 4):
AmmoniteDiameters <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
  17, 18, 19, 20)
AmmoniteChambers <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
  17, 18, 19, 20)

# Finally the other quadrat you previously compared your own ammonite diameters
# to:
AmmoniteDiameters2 <- c(196, 202, 140, 174, 155, 164, 238, 176, 217, 217, 122,
  137, 104, 208, 192, 141, 126, 192, 173, 116)

# We will begin with the dinosaur species example. Remember that here we
# simplified this data into stage-to-stage values of either an increase or
# decrease in species number, allowing us to investigate the data using the
# binomial distribution. The obvious null hypothesis here is the simplest, most
# boring explanation - that increases and decreases are equally probable. I.e.,
# a model of a fair coin where each side has equal probability. We can get the
# probabilities of each specific number of outcomes using the probability
# density function for the binomial:
x <- dbinom(x = c(0:26), size = 26, prob = 0.5)

# Here we are setting x as the full range of outcomes (0 to 26 increases), the
# size is 26 (the number of stage-to-stage transitions in our sample), and
# finally p which is the probability of an increase (0.5). We can do a quick
# visualisation of this data using a barplot:
barplot(height = x, space = 0, border = 0, xlim = c(0, 26), col = "Grey",
  names.arg = c(0:26), xlab = "N increases", ylab = "P(X = k)")

# Note that as expected the most probable outcome is 13 (exactly 50% of 26),
# but that generally the density is spread over several outcomes and only
# becomes vanishingly small at the extremes. Now to place our observed value
# in context we must first choose an ALPHA value (significance-level) and
# whether to perform a one- or two-tailed test. Previously we built our
# interpretations of this space around the middle 50% of the distribution. This
# represents the confidence-level (1 - alpha) and a two-tailed test. Thus an
# alpha of 0.5 is where we will start:
Alpha <- 0.5

# Previously we used the cumulative distribution function to find what actual
# values the edge of our middle % (our confidence-level) corresponded to. In
# the language of NHST these are CRITICAL VALUES that denote the crossing from
# a null hypothesis outcome to an alternative hypothesis outcome. As we are
# doing a two-tailed test there will be two of these, but for a one-tailed test
# there would only be one. We can get critical values using the qbinom function:
CriticalValues <- qbinom(p = c(Alpha / 2, 1 - (Alpha / 2)), size = 26,
  prob = 0.5)
CriticalValues

# This gives us the 11 and 15 we saw in lecture. We can thus divide our
# distribution up into values corresponding to the null (here coloured grey)
# and the alternative hypotheses (here coloured red) with:
barplot(height = x, space = 0, border = 0, xlim = c(0, 26),
  col = c(rep("Red", CriticalValues[1]), rep("Grey", diff(CriticalValues) + 1),
  rep("Red", CriticalValues[1] - 1)), names.arg = c(0:26),
  xlab = "N increases", ylab = "P(X = k)")

# We can already see that this means our alternative hypothesis really includes
# two possible outcomes (i.e., if in the left-hand red portion we would reject
# the null in favour of an alternative of decreases being more probable than
# increases, if in the right-hand red portion we would reject the null in
# favour of an alternative of increases being more probable than decreases).
# We can now look at our actual observed number of increases:
NIncreases <- sum(diff(DinoSpeciesByGeologicStage) > 0)
NIncreases

# This is better understood visually by plotting it as an arrow in the space:
barplot(height = x, space = 0, border = 0, xlim = c(0, 26),
  col = c(rep("Red", CriticalValues[1]), rep("Grey", diff(CriticalValues) + 1),
  rep("Red", CriticalValues[1] - 1)), names.arg = c(0:26),
  xlab = "N increases", ylab = "P(X = k)")
lines(x = c(NIncreases, NIncreases + 0.5, NIncreases + 0.5, NIncreases + 0.5,
  NIncreases + 1), y = c(max(x) / 8, 0, max(x) / 2, 0, max(x) / 8), lwd = 2)

# Do we reject or accept the null hypothesis based on this?
#
# Many scientific papers prefer a lower value of alpha (e.g., 0.05). Let's see
# what would happen if we had chosen that value instead:
Alpha <- 0.05

# We will need to recalculate our critical values as alpha has changed:
CriticalValues <- qbinom(p = c(Alpha / 2, 1 - (Alpha / 2)), size = 26,
  prob = 0.5)
CriticalValues

# You should see they now span a larger range (i.e., by decreasing our
# significance value we must necessarily increase our confidence-level,
# specifically this is now 1 - 0.05 = 0.95, or 95%). We can replot the data:
barplot(height = x, space = 0, border = 0, xlim = c(0, 26),
  col = c(rep("Red", CriticalValues[1]), rep("Grey", diff(CriticalValues) + 1),
  rep("Red", CriticalValues[1] - 1)), names.arg = c(0:26),
  xlab = "N increases", ylab = "P(X = k)")

# What happened to the red areas? Does this make it harder or easier to reject
# the null?
#
# Now we can replot the data with our observed value included:
barplot(height = x, space = 0, border = 0, xlim = c(0, 26),
  col = c(rep("Red", CriticalValues[1]), rep("Grey", diff(CriticalValues) + 1),
  rep("Red", CriticalValues[1] - 1)), names.arg = c(0:26),
  xlab = "N increases", ylab = "P(X = k)")
lines(x = c(NIncreases, NIncreases + 0.5, NIncreases + 0.5, NIncreases + 0.5,
  NIncreases + 1), y = c(max(x) / 8, 0, max(x) / 2, 0, max(x) / 8), lwd = 2)

# Do we reject or accept the null hypothesis based on this? Is this a different
# outcome from when alpha was 0.5?
#
# Now let's try a larger value for alpha:
Alpha <- 0.95

# Recalculate our critical values again:
CriticalValues <- qbinom(p = c(Alpha / 2, 1 - (Alpha / 2)), size = 26,
  prob = 0.5)
CriticalValues

# And replot:
barplot(height = x, space = 0, border = 0, xlim = c(0, 26),
  col = c(rep("Red", CriticalValues[1]), rep("Grey", diff(CriticalValues) + 1),
  rep("Red", CriticalValues[1] - 1)), names.arg = c(0:26),
  xlab = "N increases", ylab = "P(X = k)")
lines(x = c(NIncreases, NIncreases + 0.5, NIncreases + 0.5, NIncreases + 0.5,
  NIncreases + 1), y = c(max(x) / 8, 0, max(x) / 2, 0, max(x) / 8), lwd = 2)

# Do we reject or accept the null now? If we do reject the null what does the
# alternative hypothesis indicate? I.e., is it more increases than decreases,
# or more decreases than increases?
#
# Note that so far all we have done is try different values of alpha. The data
# and the model haven't changed, only our interpretation of the outcome.
# I.e., our interpretation is crucially dependent on the alpha value we use. It
# should be clear that we should not choose this value *after* we have measured
# our data as we could then simply pick a value that would give us the outcome
# we wanted. Thus a general rule is that alpha should be selected before any
# statistics are performed.
#
# Let's move on to look at your ammonite data. Remember that we can plot the
# actual values as a histogram:
hist(x = AmmoniteDiameters, breaks = c(50, 75, 100, 125, 150, 175, 200, 225,
  250, 275, 300), border = NA, col = "grey", xlab = "Diameter (mm)", main = "")

# But it is easier to treat it mathematically as a normal distribution with a
# mean of:
mean(AmmoniteDiameters)

# And a standard deviation of:
sd(AmmoniteDiameters)

# We can plot that with:
plot(x = seq(from = 50, to = 300, length.out = 1000),
  y = dnorm(x = seq(from = 50, to = 300, length.out = 1000),
  mean = mean(AmmoniteDiameters), sd = sd(AmmoniteDiameters)),
  type = "l", xlab = "Diameter (mm)", ylab = "f(x)")

# Now let's revisit one use of the one-sample t-test - to get a confidence-
# interval around our estimate of the population mean:
t.test(x = AmmoniteDiameters, conf.level = 0.95)$conf.int[1:2]

# You should be able to tell that this corresponds to a much narrower
# distribution than the sample dispersion. That is because it is acting like
# our sample means distribution from lecture, i.e., the t-distribution is
# filling in as a sample means distribution for your single sample. As such
# it's dispersion is the uncertainty around your estimate of the population
# mean and not the dispersion of values in the population. (Because of this
# feature we can still use the t-test when our data are not normally
# distributed.) We can visualise this too, but first we must import a custom
# function:
source("https://raw.githubusercontent.com/graemetlloyd/statsforgeologists/master/Functions/StatsFunctions.r")

# Now we can plot the data:
TTestVisualisation(x = AmmoniteDiameters, Alpha = 0.05, Type = "TwoTail",
  PlotRange = c(50, 300), xlab = "Diameter (mm)")

# There is a lot to note here. First this is plotted on the same scale
# (50-300mm) as the previous two plots so you can see how narrow the
# t-distribution is compared to the sample distribution. Secondly, the blue
# lines represent the two ends of the confidence-interval you just got from
# the t.test function above (check the labelled numbers match). Thirdly, the
# exact shape of the distribution is dependent on the sample size (N), recorded
# in the plot title. But note that there are other similarities to the binomial
# distribution from above as well. In fact these are more than just
# similarities. The ends of the confidence-interval also represent critical
# values. As there are two we are essentially performing a two-tailed test.
# Here the confidence-level is 95% (0.95), and thus the alpha is 1 - this,
# i.e., 0.05:
Alpha <- 0.05

# The only difference is this time there is no observed value we are testing
# against our null. However, if you remember there was an observed value (the
# suggested mean ammonite diameter from the commercial collector of 200 mm).
# We can thus perform essentially the same NHST as we did before, but using
# 200 as our observed value. This time the null hyppthesis is that the suggested
# value is a plausible population mean and the alternative is that suggested
# value is an implausible population mean, being either too large or too small
# (a two-tailed test). We can visualise the outcome by plotting the commercial
# collector's estimate with an arrow:
TTestVisualisation(x = AmmoniteDiameters, Alpha = Alpha, Type = "TwoTail",
  PlotRange = c(50, 300), xlab = "Diameter (mm)")
lines(x = c(198, 200, 200, 200, 202), y = c(0.02, 0, 0.08, 0, 0.02), lwd = 2)

# Do you accept or reject the null? Note that you can do the same thing using
# the probability that 200 is the true mean:
t.test(x = AmmoniteDiameters, mu = 200)$p.value

# If this is less than Alpha, you reject the null, and if more than Alpha
# accept the null:
t.test(x = AmmoniteDiameters, mu = 200)$p.value < Alpha

# What did you find? If you do reject the null is the suggested population
# mean implausibly large, or implausibly small?
#
# You can also make these simpler statements for two-sample t-tests. Previously
# you examined the probability that your quadrat and a second quadrat came from
# populations wuth the same mean value. Here the assumption is that they come
# from the exact same population so we should probably expect to accept the null
# (same population mean) rather than reject it (different population means).
# Let's check:
t.test(x = AmmoniteDiameters, y = AmmoniteDiameters2)$p.value < Alpha

# What did you find? What if we make Alpha much larger (0.99)?:
Alpha <- 0.99
t.test(x = AmmoniteDiameters, y = AmmoniteDiameters2)$p.value < Alpha

# What did you find now? Given we can be pretty certain that the two quadrats
# come from the same population what kind of error would this be? I.e., Type
# I or Type II?
#
# Now let's look at our profession heights data. Remember that we found some
# compelling differences previously. Let's start by using a lower Alpha (0.01):
Alpha <- 0.01

# First we can look at the raw p-value for each possible pairwise comparison.
# Remember that this is the probability that the two samples are drawn from
# populations with the same mean value (i.e., the probability of the null):
t.test(x = ClassHeights, y = BasketballHeights)$p.value
t.test(x = ClassHeights, y = JockeyHeights)$p.value
t.test(x = BasketballHeights, y = JockeyHeights)$p.value

# You should see they are all very small. Now let's check to see if any fall
# below Alpha:
t.test(x = ClassHeights, y = BasketballHeights)$p.value < Alpha
t.test(x = ClassHeights, y = JockeyHeights)$p.value < Alpha
t.test(x = BasketballHeights, y = JockeyHeights)$p.value < Alpha

# Were we able to reject the null here? If so what alternative hypothes(es)
# do we favour instead? It might help to check the means of each sample for
# this;
mean(ClassHeights)
mean(BasketballHeights)
mean(JockeyHeights)

# Does this conform with our previous interpretations of this data? What if we
# make Alpha much smaller (0.00001)?
Alpha <- 0.00001
t.test(x = ClassHeights, y = BasketballHeights)$p.value < Alpha
t.test(x = ClassHeights, y = JockeyHeights)$p.value < Alpha
t.test(x = BasketballHeights, y = JockeyHeights)$p.value < Alpha

# Do any of our conclusions change? If so, was it the most or least likely
# pair? (Think about the absolute differences between our three sample means.)
#
# Now let's start thinking about bivariate data. We previously encountered
# confidence intervals about our regression parameters (slope and intercept)
# when looking at ammonite diameters against ammonite chambers:
confint(lm(AmmoniteDiameters ~ AmmoniteChambers), level = 1 - Alpha)

# The second row of this table are the critical values for the slope
# parameter, which in NHST of regressions is usually the more important. I.e.,
# the typical null here is that the true slope is zero corresponding to no
# response of y to x. Our alternative is thus a slope that is not zero and
# again there are two possible outcomes (positive slope or negative slope).
# Let's look again, but at a higher Alpha (0.025):
Alpha <- 0.025
confint(lm(AmmoniteDiameters ~ AmmoniteChambers), level = 1 - Alpha)

# Do the critical values of the slope parameter bracket zero? I.e., is one
# negative and the other positive? If not then we should reject the null.
# If we do reject the null is the true slope considered positive or
# negative? Is this consistent with what we found previously?
#
# We can do something similar with correlation, but this time the null is that
# the true value of the correlation coefficient (r) is zero. Again we can test
# this by getting the critical values for r and asking if they bracket zero:
cor.test(x = AmmoniteChambers, y = AmmoniteDiameters,
  conf.level = 1 - Alpha)$conf.int[1:2]

# Do they? If not, and we reject the null in favour of the alternative, is the
# true value of r considered positive or negative? Does this change our
# interpetation of how ammonite chambers and diameters are related? Is it
# consistent with our slope parameter conclusions from above?
#
# Next we will look at some random data to consider Type I and Type II errors.
# Specifically we will generate 100 separate data sets of 20 values each by
# drawing random values from the standard normal (mean = 0, sd = 1):
RandomData <- matrix(data = rnorm(20 * 100), ncol = 20)

# You can plot the first two data sets against each other with:
plot(x = RandomData[1, ], y = RandomData[2, ], pch = 20, cex = 3)

# Do the two look correlated? Note that we know that they are not as they are
# all independently drawn from a random distribution. We can test this with
# our correlation test function:
cor.test(x = RandomData[1, ], y = RandomData[2, ],
  conf.level = 1 - Alpha)$conf.int[1:2]

# The outputs here are the critical values. Do they bracket zero? We can write
# a little function to check:
BracketZero <- function(x) return(sum(x < 0) == 1)

# And test it on the same data:
BracketZero(cor.test(x = RandomData[1, ], y = RandomData[2, ],
  conf.level = 1 - Alpha)$conf.int[1:2])

# This should return TRUE if they do and FALSE if they do not bracket zero.
# Now let's do this for every possible pairing:
for(i in 1:99) for(j in (i + 1):100) cat(BracketZero(cor.test(
  x = RandomData[i, ], y = RandomData[j, ],
  conf.level = 1 - Alpha)$conf.int[1:2]), " ")

# Are they all TRUEs? Or do you see some FALSEs? Remember that we know none of
# our data is correlated - it is all random noise. Let's go back and count up
# the TRUE and FALSE answers:
TRUEs <- FALSEs <- 0
for(i in 1:99) {
  for(j in (i + 1):100) {
    ifelse(BracketZero(cor.test(x = RandomData[i, ], y = RandomData[j, ],
      conf.level = 1 - Alpha)$conf.int[1:2]), TRUEs <- TRUEs + 1,
      FALSEs <- FALSEs + 1)
  }
}
TRUEs
FALSEs

# How many TRUEs and how many FALSEs were there? Note that we can also look at
# the frequency of FALSEs as a proportion of the total number of comparsions
# (4950) with:
FALSEs / 4950

# Does this bear any resemblance to our Alpha value?:
Alpha

# Let's try again with a different Alpha (0.1):
Alpha <- 0.1
TRUEs <- FALSEs <- 0
for(i in 1:99) {
  for(j in (i + 1):100) {
    ifelse(BracketZero(cor.test(x = RandomData[i, ], y = RandomData[j, ],
    conf.level = 1 - Alpha)$conf.int[1:2]), TRUEs <- TRUEs + 1,
    FALSEs <- FALSEs + 1)
  }
}
FALSEs / 4950

# Did it change? What about the resemblance to Alpha? Note that what we are
# looking at here is the Type I error rate. I.e., the proportion of times we
# reject the null when we shouldn't (and we know we shouldn't here as the data
# are all random and not correlated). Which value of Alpha kept this rate
# lower? What does that tell us about our choice of a value for Alpha? Does
# this have anything to do with Nicholas Cage?:
browseURL("http://www.tylervigen.com/spurious-correlations")

# Finally, we have mostly looked here at what are in effect two-tail tests, and
# that is because this is the most logical way to treat the null in many
# typical scenarios. However, there are occasions where we might wish to
# employ one-tailed tests and there is even an option in the t.test function to
# do this (alternative):
t.test(x = AmmoniteDiameters, conf.level = 1 - Alpha, alternative = "two.sided")
t.test(x = AmmoniteDiameters, conf.level = 1 - Alpha, alternative = "greater")
t.test(x = AmmoniteDiameters, conf.level = 1 - Alpha, alternative = "less")

# The alternative option here corresponds to the alternative hypothesis.
# Unfortunately the one-tailed alternatives seem to be mislabelled, at least
# for our purposes. Specifically, the "greater" really applies the confidence
# interval (null interval) to the right of the distribution (i.e., off to
# infinity at right). Thus the alternative here is actually a value lower than
# the critical value (i.e., "less"). The opposite is true for "less" where the
# confidence interval (null) stretches from negative infinity to the critical
# value with the true alternative actually being greater. We can see this
# visually by plotting:
TTestVisualisation(x = AmmoniteDiameters, Alpha = Alpha, Type = "LeftTail",
  PlotRange = c(110, 210), xlab = "Diameter (mm)")

# This corresponds to:
t.test(x = AmmoniteDiameters, conf.level = 1 - Alpha, alternative = "greater")

# I.e., compare the critical value to the confidence interval:
t.test(x = AmmoniteDiameters, conf.level = 1 - Alpha,
  alternative = "greater")$conf.int[1:2]

# We can do the same for a right-tailed test:
TTestVisualisation(x = AmmoniteDiameters, Alpha = Alpha, Type = "RightTail",
  PlotRange = c(110, 210), xlab = "Diameter (mm)")

# Which corresponds to:
t.test(x = AmmoniteDiameters, conf.level = 1 - Alpha, alternative = "less")

# I.e.:
t.test(x = AmmoniteDiameters, conf.level = 1 - Alpha,
  alternative = "less")$conf.int[1:2]

# Note that with one-tailed tests we are shifting the meaning of the null and
# reducing the alternative hypothesis to a single outcome (smaller value for
# a left-tailed test, larger value for a right-tailed test). Thus we can reuse
# our commercial collector's estimated mean diameter from earlier again, but
# in a one-tailed context. First we can consider the alternative of a smaller
# value for the mean:
TTestVisualisation(x = AmmoniteDiameters, Alpha = Alpha, Type = "LeftTail",
  PlotRange = c(110, 210), xlab = "Diameter (mm)")
lines(x = c(198, 200, 200, 200, 202), y = c(0.02, 0, 0.08, 0, 0.02), lwd = 2)

# The arrow here denotes the collector's estimate. Would we accept the
# alternative here? Note that this is the same as asking whether the p-value
# is less than the Alpha (reject the null) or not (accept the null). This time
# "less" gives us the correct value:
t.test(x = AmmoniteDiameters, conf.level = 1 - Alpha,
  alternative = "less")$p.value < Alpha

# We can do the same thing fro a right-tailed test (alternative of a greater
# value for the mean):
TTestVisualisation(x = AmmoniteDiameters, Alpha = Alpha, Type = "RightTail",
  PlotRange = c(110, 210), xlab = "Diameter (mm)")
lines(x = c(198, 200, 200, 200, 202), y = c(0.02, 0, 0.08, 0, 0.02), lwd = 2)

# Do we accept or reject the null now? Again, we can do this as a straight true
# or false with (again, now "greater" is the correct option):
t.test(x = AmmoniteDiameters, conf.level = 1 - Alpha,
  alternative = "greater")$p.value < Alpha

# Is this what you expected to find? How does this differ when we applied a
# two-tailed test to the same data and estimated population mean? What does
# this mean generally for choosing one- over two-tailed tests?
#
# Note that here it isn't really appropriate to use a one-tailed test, but that
# rejecting the null for a two-tailed test requires us to know which of two
# (or more) possible outcomes we favour instead. I.e., it is not enough to
# just reject the null for a two-tailed test, additional information is
# required to say what is going on with the data.

################################################################################
#                                                                              #
#                                     MCQ                                      #
#                                                                              #
# You are now ready to attempt this week's MCQ which is a series of ten        #
# multiple choice questions (MCQs) on last week's lecture and this practical.  #
# This is on Minerva: SOEE1475 > Statistics Resources > MCQ.                   #
#                                                                              #
################################################################################
