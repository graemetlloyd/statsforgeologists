################################################################################
#                                                                              #
#                    PRACTICAL VIII - DIRECTIONAL STATISTICS                   #
#                                                                              #
################################################################################

################################################################################
#                                                                              #
#                               TODAY'S AIMS                                   #
#                                                                              #
# - LEARN HOW TO FORMAT AND VISUALISE CIRCULAR DATA IN R                       #
# - LEARN HOW TO TEST FOR UNIFORM AND RANDOM DISTRIBUTIONS OF BEARINGS IN R    #
# - LEARN HOW TO COMPARE TWO SAMPLES OF BEARINGS IN R                          #
#                                                                              #
################################################################################

# Before we start begin by setting your working directory.

# Today we are going to use some more specific statistical tests than in
# previous practicals requiring us to use a special library of functions.
# There are mutliple such packages, but we are going to use the "circular"
# library. You can install it with:
install.packages("circular", dependencies = TRUE)

# And load the functions into memory with:
library(circular)

# Next you (would normally!) need to measure the bearings of all of your
# ammonites following the protocol from lecture. As you did so you would
# modify the line below, placing each bearing in sequence, separated by
# a comma, then running the line in R as normal:
AmmoniteBearings <- c(, , , , , , , , , , , , , , , , , , , )

# However...
#
# As we are working remotely you can instead get these measurements by
# modifying the line below by replacing 'X4' with your own quadrat's
# code then running the line:
AmmoniteBearings <- read.csv("https://raw.githubusercontent.com/graemetlloyd/statsforgeologists/master/Ammonite_quadrats/Lower_Bed/Quadrat_X4.csv")[, "Aperture_bearing_degrees"]

# Before we learn how to treat this data correctly (i.e., as circular) lets
# convince ourselves that treating it as regular univariate data is problematic.
# First we can plot the data as a histogram using 20-degree bins, from 0 to 360:
hist(x = AmmoniteBearings, breaks = seq(from = 0, to = 360, by = 20),
  col = "grey", border = 0, xlab = "Bearing (degrees)")

# We can calculate the mean:
mean(AmmoniteBearings)

# Convert this value to a compass value (e.g., SSE, WNW, NE, S). Does this
# capture the "middle" of your bearings? Note that we could attempt something
# of a cheat here to get the "right" answer. I.e., add 180 degrees to every
# value, calculate the mean, then subtract 180 again:
(mean((AmmoniteBearings + 180) %% 360) - 180) %% 360

# (Note the "%% 360" part is to make sure each value is modulo 360.) Is this a
# more sensible answer? This is probably a safe estimate for the current data
# set as the variance in your values is very low, but this would not work as a
# general solution (e.g., for the Skye sandstone palaeocurrent data we
# encountered in class).
#
# Before we apply special circular statistics to the data we need to format it
# correctly. We can do this with the function circular:
circular(x = AmmoniteBearings)

# You should see that there are now a weird series of messages before your
# bearings, including "Type", "Units", "Template", "Modulo", "Zero", and
# "Rotation". Note that these are all default values and many of them are
# incorrect for our specific data. We can see that by asking for a circular
# mean:
mean(circular(x = AmmoniteBearings))

# We know this should be pretty much identical to your (cheat) univariate
# estimate from above, but will not be. This is because the package has
# defaulted to treating the data as radians (which they are not), that they are
# modulo "asis" (which they are not), that they are to be plotted with
# zero at right on the circle (we want this at the top, like a compass), and
# that they are to be plotted counterclockwise (which we also do not want). We
# can fix these one at a time with the various options of the circular
# function. First, the units:
mean(circular(x = AmmoniteBearings, units = "degrees"))

# The mean is still wrong, and may even be negative. That's because the
# modulo value is wrong. Remember from lecture that if our data were
# orientations (e.g., strikes, ripple crests) then our data would be modulo 180
# (or pi, in radians). Here they are directional, and hence are modulo 360 -
# 2 pi in radians. We will fix that next:
mean(circular(x = AmmoniteBearings, units = "degrees", modulo = "2pi"))

# This should now be the correct mean value. Next we need to fix the plotting
# issues. To check where we stand with our current modifications we can plot
# a circle diagram:
plot.circular(circular(x = AmmoniteBearings, units = "degrees", modulo = "2pi"))

# You should see that the orientation is not quite what we want (zero at the
# top, i.e., representing North). We can fix that by setting a "zero" value.
# This also needs to be in radians, and the value we want is pi / 2:
plot.circular(circular(x = AmmoniteBearings, units = "degrees", modulo = "2pi",
  zero = pi / 2))

# This is better, but we aren't quite there. Look at the horizontal labels
# (East and West, 90 and 270). What is wrong? To fix this we need to set the
# rotation value as clockwise:
plot.circular(circular(x = AmmoniteBearings, units = "degrees", modulo = "2pi",
  zero = pi / 2, rotation = "clock"))

# We finally have our data in the format we (as geologists) want it. You might
# wonder why the defaults are so very different to what we want - that is
# because circular mathematics has many more applications than we as
# geologists use (i.e., we want a compass, but most circular mathematicians are
# interested in different applications. Now that we have the right settings we
# can create a new variable in the correct format to use for the later
# functions:
AmmoniteBearingsCircular <- circular(x = AmmoniteBearings, units = "degrees",
  modulo = "2pi", zero = pi / 2, rotation = "clock")
AmmoniteBearingsCircular

# We can now get circular versions of basic statistics, e.g., the mean:
mean(AmmoniteBearingsCircular)

# Or standard deviation:
sd(AmmoniteBearingsCircular)

# Note we do not need special names for these functions - if the data are
# already formatted as circular the package knows to treat them as such. We
# can show this with the is.circular function:
is.circular(AmmoniteBearings)
is.circular(AmmoniteBearingsCircular)

# In lecture we encountered the resultant as a means of summarising basic
# statistics about a series of directional measurements. Note that the mean
# from above is obtained from the resultant (theta-bar). We can also get the
# resultant length with:
rho.circular(AmmoniteBearingsCircular) * length(AmmoniteBearingsCircular)

# Remember that this can never be longer than the number of values:
length(AmmoniteBearingsCircular)

# The rho.circular function gives the mean resultant length:
rho.circular(AmmoniteBearingsCircular)

# And one minus this is the circular variance:
1 - rho.circular(AmmoniteBearingsCircular)

# What do these data collectively tell you about the palaeocurrent from your
# ammonite quadrat? Are they what you expected? You can also visualise your
# data as a rose diagram:
rose.diag(x = AmmoniteBearingsCircular, bins = 36, col = "black")

# Here the number for "bins" just divides your circle into that many equal-
# sized bins. I.e., 36 corresponds to 10-degree bins.
#
# Next you can treat the data as though it fits a von Mises distribution - the
# circular equivalent of the Normal distribution. Specifically, we can estimate
# mu (the mean) and kappa (the concentration parameter):
mle.vonmises(x = AmmoniteBearingsCircular)$mu
mle.vonmises(x = AmmoniteBearingsCircular)$kappa

# What value did you get for kappa? What does this tell you about the data?
# Does that make sense based on the values you have and your visualisations?
#
# Next you can perform a Rayleigh test on your data. Remember here that this
# is a test for a uniform distribution which here would correspond to an even
# distribution of ammonite bearings:
rayleigh.test(x = AmmoniteBearingsCircular)$p.value

# How probable is it that the data are uniformly distributed? Is this what you
# expected? If using an alpha of 0.001 would you reject the null (uniform
# distribution) in favour of the laternative (non-uniform)?
#
# Next we can use the trusty Chi-squared test to see if the data conform to
# either a uniform or random distribution. Remember that this corresponds to two
# different tests as it means two different expectations of the distribution
# of the bearings. As Chi-squared tests are categorical they also require the
# data to be binned (as we have done previously with spatial and temporal
# data). Here we will use 30-degree bins. How many does that mean in total?
# Write your answer below and copy and paste into R:
NBins <-

# Next you need to calculate the expected number of bearings in each bin.
# Write your answer in below and copy and paste into R:
ExpectedBearings <-

# Now you will need to count the number of bearings in each bin for your
# actual data. NB: Treat your bins as beginning at 0, i.e., the first bin is
# 0 to 30 degrees. Modify the following (place values inside parentheses,
# separated by commas) and copy and paste into R:
NBearingsPerBin <-

# Now you can calculate your Chi-squared value. First get the observed minus
# expected:
NBearingsPerBin - ExpectedBearings

# Then the square of these:
(NBearingsPerBin - ExpectedBearings) ^ 2

# Divided through by the expected value:
((NBearingsPerBin - ExpectedBearings) ^ 2) / ExpectedBearings

# Absolute of this:
abs(((NBearingsPerBin - ExpectedBearings) ^ 2) / ExpectedBearings)

# And finally the sum:
sum(abs(((NBearingsPerBin - ExpectedBearings) ^ 2) / ExpectedBearings))

# This is your Chi-squared value, but as always we need to calculate the
# degrees of freedom to interpret this. This will be simple, just the NBins
# minus one. Modify the below and copy and paste it into R:
UniformDF <-

# We also want an Alpha value to determine the critical value for the data.
# Here we will use 0.01:
Alpha <- 0.01

# We now have enough information ot produce a plot of the Chi-squared
# distribution:
ChiSqRange <- seq(from = 0, to = 100, length.out = 1000)
plot(x = ChiSqRange, y = dchisq(x = ChiSqRange, df = UniformDF), type = "n",
  ylab = "f(x)", xlab = "Chi-squared value",
  main = paste("Null: Bearings are uniformly distributed\ndf = ", UniformDF + 1,
  " - 1; Alpha = ", Alpha, "; right-tailed test", sep = ""))
polygon(x = c(0, ChiSqRange, 30, 0), y = c(0, dchisq(x = ChiSqRange,
  df = UniformDF), 0, 0), border = NA, col = "grey")
CriticalValue <- qchisq(p = 1 - Alpha, df = UniformDF)
polygon(x = c(CriticalValue, ChiSqRange[ChiSqRange >= CriticalValue], 30,
  CriticalValue), y = c(0, dchisq(x = ChiSqRange[ChiSqRange >= CriticalValue],
  df = UniformDF), 0, 0), border = NA, col = "red")
lines(x = c(CriticalValue, CriticalValue), y = c(0,
  max(dchisq(x = ChiSqRange, df = UniformDF)) * 0.95), col = "blue", lwd = 3)
text(x = CriticalValue, y = max(dchisq(x = ChiSqRange, df = UniformDF)),
  labels = round(CriticalValue, 2), col = "blue")

# Which side of the critical value does your Chi-squared value fall? Does this
# mean you accept or reject the null? If you reject it what possibilities for
# the true distribution does this leave?
#
# Next you can test for a random distribution. Once again, we can use the
# Poisson to do this, by generating an expected distribution of number of bins
# with that many bearings. Remember that this distribution needs a single
# value, lambda, representing the mean. Here this will be the mean number of
# bearings per bin. You already calculated this as the expected value for the
# uniform distribution:
Lambda <- ExpectedBearings

# You can visualise this expectation with:
barplot(height = dpois(x = 0:20, lambda = Lambda), space = 0, border = 0,
  names.arg = 0:20, xlab = "Bearings per bin", ylab = "P(X = x)")

# This is the probabilties for zero through twenty bearings in each bin
# (twenty being the maximum possible). We can turn this into expected values
# by multiplying each one by the total number of bearings:
ExpectedBearingsPerBin <- dpois(x = 0:20, lambda = Lambda) *
  length(AmmoniteBearingsCircular)
ExpectedBearingsPerBin

# You can modify your own counts to fit the same binning with:
ObservedBearingsPerBin <- apply(apply(matrix(NBearingsPerBin), 1, '==',
  0:20), 1, sum)
ObservedBearingsPerBin

# Now you could just calculate a Chi-squared using the data in its current form
# as we have previously, but note that when the number of bins is higher than
# the number of data points there is a risk of Type I error (rejecting the null
# when the null is correct). So instead we will clump some of the bins
# together. Specifically, we will make four new bins, three bins of five values
# and one of four values:
ExpectedBearingsPerBin <- c(sum(ExpectedBearingsPerBin[1:5]),
  sum(ExpectedBearingsPerBin[6:10]), sum(ExpectedBearingsPerBin[11:15]),
  sum(ExpectedBearingsPerBin[16:21]))
ObservedBearingsPerBin <- c(sum(ObservedBearingsPerBin[1:5]),
  sum(ObservedBearingsPerBin[6:10]), sum(ObservedBearingsPerBin[11:15]),
  sum(ObservedBearingsPerBin[16:21]))
ExpectedBearingsPerBin
ObservedBearingsPerBin

# Now you can calculate your Chi-squared value. First observed minus expected:
ObservedBearingsPerBin - ExpectedBearingsPerBin

# Then the squares:
(ObservedBearingsPerBin - ExpectedBearingsPerBin) ^ 2

# Divided by expected:
((ObservedBearingsPerBin - ExpectedBearingsPerBin) ^ 2) / ExpectedBearingsPerBin

# Absolute values:
abs(((ObservedBearingsPerBin - ExpectedBearingsPerBin) ^ 2) /
  ExpectedBearingsPerBin)

# And finally, the sum:
sum(abs(((ObservedBearingsPerBin - ExpectedBearingsPerBin) ^ 2) /
  ExpectedBearingsPerBin))

# This is your Chi-squared value. But you now need the degrees of freedom. As
# we have clumped the data into four bins this is going to be relatively small:
# just four minus one:
RandomDF <- 3

# We will stick with our Alpha from before, and so we can generate the
# Chi-squared distribution with:
ChiSqRange <- seq(from = 0, to = 30, length.out = 1000)
plot(x = ChiSqRange, y = dchisq(x = ChiSqRange, df = RandomDF), type = "n",
  ylab = "f(x)", xlab = "Chi-squared value",
  main = paste("Null: Bearings are randomly distributed\ndf = ", RandomDF + 1,
  " - 1; Alpha = ", Alpha, "; right-tailed test", sep = ""))
polygon(x = c(0, ChiSqRange, 30, 0), y = c(0, dchisq(x = ChiSqRange,
  df = RandomDF), 0, 0), border = NA, col = "grey")
CriticalValue <- qchisq(p = 1 - Alpha, df = RandomDF)
polygon(x = c(CriticalValue, ChiSqRange[ChiSqRange >= CriticalValue], 30,
  CriticalValue), y = c(0, dchisq(x = ChiSqRange[ChiSqRange >= CriticalValue],
  df = RandomDF), 0, 0), border = NA, col = "red")
lines(x = c(CriticalValue, CriticalValue), y = c(0,
  max(dchisq(x = ChiSqRange, df = RandomDF)) * 0.95), col = "blue", lwd = 3)
text(x = CriticalValue, y = max(dchisq(x = ChiSqRange, df = RandomDF)),
  labels = round(CriticalValue, 2), col = "blue")

# Where does your Chi-squared value fall? Do you accept or reject the null? Is
# this what you expected?
#
# Next we will consider a new set of bearings acquired from a later bed
# (higher in the sequence, but not from what would have been your assessment
# bed!). Here we are interested in whether the palaeocurrent has changed over
# time:
AmmoniteBearings2 <- c(93, 90, 88, 95, 87, 104, 76, 92, 91, 88, 67, 62, 69,
  76, 54, 89, 84, 77, 78, 83)

# As before, we will need to reformat the data as circular to use the special
# circular package functions. We will use the same settings as before to do
# this:
AmmoniteBearingsCircular2 <- circular(x = AmmoniteBearings2,
  units = "degrees", modulo = "2pi", zero = pi / 2, rotation = "clock")
AmmoniteBearingsCircular2

# First we can plot the data with:
plot.circular(x = AmmoniteBearingsCircular2)

# Or as a rose diagram with:
rose.diag(x = AmmoniteBearingsCircular2, bins = 36, col = "black")

# What is your initial expectation? Has the palaeocurrent changed direction?
#
# We can test this with the Watson Two-sample Test. This is essentially a
# circular version of the Two-sample t-test and thus assesses the probability
# of two samples being drawn from populations with the same mean. It requires
# us to specify the alpha in advance (here we will use 0.01):
watson.two.test(x = AmmoniteBearingsCircular, y = AmmoniteBearingsCircular2,
  alpha = 0.01)

# Do you accept or reject the hypothesis of equal population means? Is this
# what you expected from your visual inspection of the data? What was the
# mean from your own quadrat, as a compass direction? What is the compass
# direction of the bearings from the later bed? How much of a shift does this
# represent?

################################################################################
#                                                                              #
#                                     MCQ                                      #
#                                                                              #
# You are now ready to attempt this week's MCQ which is a series of ten        #
# multiple choice questions (MCQs) on last week's lecture and this practical.  #
# This is on Minerva: SOEE1475 > Statistics Resources > MCQ.                   #
#                                                                              #
################################################################################
