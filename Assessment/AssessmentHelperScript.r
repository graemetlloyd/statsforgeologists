################################################################################
#                                                                              #
#                          ASSESSMENT HELPER SCRIPT                            #
#                                                                              #
################################################################################

# This is a script to help you analyse your assessment quadrat. Note that really
# it just repeats and synthesises material from across the practicals and it is
# assumed you have already completed all of those. I.e., there is nothing in
# this script that you have not already encountered in practical.
#
# Just because something is in this script does not mean you have to use it,
# rather it is meant to cover the bases of the basic and special statistics
# that apply to the various types of data you can get from your ammonite
# quadrat. Similarly, everything here only appears once but that doesn't mean
# you should always only apply a test once. You might want to, for example,
# show results of the same test on your lower and upper bed quadrats, or try
# different binning schemes for your Chi-square tests to see if that changes
# your conclusions. In such cases it is recommended that you simply duplicate
# the lines of code.
#
# Note that there are also some troubleshooting tips at the end of the script
# if you get into difficulties.
#
# It is recommended that you always start by setting your working directory so
# any plots you produce appear in the folder of your choosing and not
# whatever the default is. I.e., you can find out what that is with:
getwd()

# Now make sure you have any appropriate packages installed with:
install.packages("circular", dependencies = TRUE)
source("https://raw.githubusercontent.com/graemetlloyd/statsforgeologists/master/Functions/StatsFunctions.r")

# And load these functions into memory with:
library(circular)

# Next replace the line below with the code from the back of your quadrat.
# (NB: the script is useless until you do this as there will be no data!):
[REPLACE THIS LINE WITH THE BLOCK OF CODE FROM THE BACK OF YOUR QUADRAT]

# You may also wish to use your lower bed quadrat data too for comparison.
# You can get that by modfiying the end of this line to point to your own
# quadrat as you did previously. (NB: X4 is not a quadrat so you will get an
# error if you do not do this!):
LowerBedQuadratData <- read.csv("https://raw.githubusercontent.com/graemetlloyd/statsforgeologists/master/Ammonite_quadrats/Lower_Bed/Quadrat_X4.csv")

# NB: This represent the "correct" answers, not your own measurements you have
# previously made with rulers/protractors and hence are to the same precision as
# your assessment data.
#
# Now we can reorganise your data into new variables to make things a bit clearer:
UpperBedAmmoniteNumbers <- AssessmentQuadratData[, "Ammonite_number"]
UpperBedAmmoniteDiameters <- AssessmentQuadratData[, "Diameter_mm"]
UpperBedAmmoniteNChambers <- AssessmentQuadratData[, "N_chambers"]
UpperBedAmmoniteXCoordinates <- AssessmentQuadratData[, "X_coordinate"]
UpperBedAmmoniteYCoordinates <- AssessmentQuadratData[, "Y_coordinate"]
UpperBedAmmoniteApertureBearings <- AssessmentQuadratData[, "Aperture_bearing_degrees"]
LowerBedAmmoniteNumbers <- LowerBedQuadratData[, "Ammonite_number"]
LowerBedAmmoniteDiameters <- LowerBedQuadratData[, "Diameter_mm"]
LowerBedAmmoniteNChambers <- LowerBedQuadratData[, "N_chambers"]
LowerBedAmmoniteXCoordinates <- LowerBedQuadratData[, "X_coordinate"]
LowerBedAmmoniteYCoordinates <- LowerBedQuadratData[, "Y_coordinate"]
LowerBedAmmoniteApertureBearings <- LowerBedQuadratData[, "Aperture_bearing_degrees"]

# Note to use the bearings we will first need to reformat these as circular
# data as we did previously:
UpperBedAmmoniteApertureBearings <- circular(x = UpperBedAmmoniteApertureBearings,
  units = "degrees", modulo = "2pi", zero = pi / 2, rotation = "clock")
LowerBedAmmoniteApertureBearings <- circular(x = LowerBedAmmoniteApertureBearings,
  units = "degrees", modulo = "2pi", zero = pi / 2, rotation = "clock")

# In each block below I use the ammonite numbers for each example (i.e., the
# values 1 to 20). Note that this is NOT useful data! It is up to you to
# modify these lines yourself to get values or make plots from the useful
# variables (diameters, chamber counts, XY coordinates, etc.).
#
# It is also up to you to modify plot titles, x- or y-axis labels etc.
# Additionally, for Chi-squared tests you will have to decide how data are
# binned (e.g,, grid cells, divisions of 360 degrees) and also how many values
# to use from a Poisson distribution, (Remember that these run from zero to
# infinity but practically speaking we must chose a cutoff somewhere.)

################################################################################
#                                                                              #
#                             UNIVARIATE STATISTICS                            #
#                                                                              #
################################################################################

# Location metrics - mean:
mean(UpperBedAmmoniteNumbers)

# Location metrics - median:
median(UpperBedAmmoniteNumbers)

# Location metrics - mode (can return multiple values):
x <- UpperBedAmmoniteNumbers
rle(sort(x))$values[which(rle(sort(x))$lengths == max(rle(sort(x))$lengths))]

# Dispersion metrics - variance:
var(UpperBedAmmoniteNumbers)

# Dispersion metrics - standard deviation:
sd(UpperBedAmmoniteNumbers)

# One sample t-test - confidence intervals:
Alpha <-
t.test(x = UpperBedAmmoniteNumbers, conf.level = 1 - Alpha)$conf.int[1:2]

# One sample t-test - probability hypothesised population mean is true:
HypothesisedMean <-
t.test(x = UpperBedAmmoniteNumbers, mu = HypothesisedMean)$p.value

# Two sample t-test - probability two samples share same population mean:
t.test(x = UpperBedAmmoniteNumbers, y = UpperBedAmmoniteNumbers)$p.value

# Univariate plot - boxplot (vanilla version):
boxplot(UpperBedAmmoniteNumbers)

# Univariate plot - boxplot (y-label):
boxplot(UpperBedAmmoniteNumbers, ylab = "My y-label")

# Univariate plot - boxplot (y-label and title):
boxplot(UpperBedAmmoniteNumbers, ylab = "My y-label", main = "My plot title")

# Univariate plot - boxplot (y-label, title, and logged y-axis):
boxplot(UpperBedAmmoniteNumbers, ylab = "My y-label", main = "My plot title",
  log = "y")

# Univariate plot - histogram (vanilla version):
hist(UpperBedAmmoniteNumbers)

# Univariate plot - histogram (custom bins, i.e., "breaks"):
hist(UpperBedAmmoniteNumbers, breaks = seq(from = 0, to = 20, length.out = 21))

# Univariate plot - histogram (custom bins, and title):
hist(UpperBedAmmoniteNumbers, breaks = seq(from = 0, to = 20, length.out = 21),
  main = "My plot title")

# Univariate plot - histogram (custom bins, title, and coloured bars):
hist(UpperBedAmmoniteNumbers, breaks = seq(from = 0, to = 20, length.out = 21),
  main = "My plot title", col = "red")

# Univariate plot - histogram (custom bins, title, coloured bars, and no
# borders):
hist(UpperBedAmmoniteNumbers, breaks = seq(from = 0, to = 20, length.out = 21),
  main = "My plot title", col = "red", border = 0)

# Export your plots - modify filename and copy and paste your plot code
# between the pdf() and dev.off() functions - see (e.g.) practical one for
# examples:
pdf("plotfilename.pdf")
[REPLACE THIS LINE WITH YOUR PLOT CODE - WILL JUST GIVE YOU AN ERROR IF NOT!]
dev.off()

################################################################################
#                                                                              #
#                              BIVARIATE STATISTICS                            #
#                                                                              #
################################################################################

# Correlation coefficient - Pearson r value:
cor.test(x = UpperBedAmmoniteNumbers, y = UpperBedAmmoniteNumbers,
  method = "pearson")$estimate

# Correlation coefficient - probability true r is zero:
cor.test(x = UpperBedAmmoniteNumbers, y = UpperBedAmmoniteNumbers,
  method = "pearson")$p.value

# Correlation coefficient - Spearman rho value:
cor.test(x = UpperBedAmmoniteNumbers, y = UpperBedAmmoniteNumbers,
  method = "spearman")$estimate

# Correlation coefficient - probability true r is zero:
cor.test(x = UpperBedAmmoniteNumbers, y = UpperBedAmmoniteNumbers,
  method = "spearman")$p.value

# Correlation coefficient - probability true r is zero (estimated value
# if ties in data):
cor.test(x = UpperBedAmmoniteNumbers, y = UpperBedAmmoniteNumbers,
  method = "spearman", exact = FALSE)$p.value

# Regression - slope for simple linear model:
x <- UpperBedAmmoniteNumbers
y <- UpperBedAmmoniteNumbers
lm(y ~ x)$coefficients[2]

# Regression - intercept for simple linear model:
x <- UpperBedAmmoniteNumbers
y <- UpperBedAmmoniteNumbers
lm(y ~ x)$coefficients[1]

# Regression - coefficients for second order polynomial model (i.e.,
# y = ax^2 + bx + c). Values are in order c, b, a:
x <- UpperBedAmmoniteNumbers
y <- UpperBedAmmoniteNumbers
lm(y ~ poly(x, 2))$coefficients

# Regression - coefficient for our exponential model (i.e.,
# y = e^(0.12 * x) * a:
x <- UpperBedAmmoniteNumbers
y <- UpperBedAmmoniteNumbers
lm(y ~ exp(0.12 * x) - 1)$coefficients

# Coefficient of determination - r-squared for simple linear model:
x <- UpperBedAmmoniteNumbers
y <- UpperBedAmmoniteNumbers
summary(lm(y ~ x))$r.squared

# Coefficient of determination - r-squared for second order polynomial model:
x <- UpperBedAmmoniteNumbers
y <- UpperBedAmmoniteNumbers
summary(lm(y ~ poly(x, 2)))$r.squared

# Coefficient of determination - r-squared for our exponential model:
x <- UpperBedAmmoniteNumbers
y <- UpperBedAmmoniteNumbers
summary(lm(y ~ exp(0.12 * x) - 1))$r.squared

# Bivariate plot - scatter plot (vanilla version):
plot(x = UpperBedAmmoniteNumbers, y = UpperBedAmmoniteNumbers)

# Bivariate plot - scatter plot (vanilla version):
plot(x = UpperBedAmmoniteNumbers, y = UpperBedAmmoniteNumbers)

# Bivariate plot - scatter plot (with points filled black):
plot(x = UpperBedAmmoniteNumbers, y = UpperBedAmmoniteNumbers,
  pch = 20, col = "black")

# Bivariate plot - scatter plot (with points filled black and plot title):
plot(x = UpperBedAmmoniteNumbers, y = UpperBedAmmoniteNumbers, pch = 20,
  col = "black", main = "My plot title")

# Bivariate plot - scatter plot (with points filled black, plot title and
# axis labels):
plot(x = UpperBedAmmoniteNumbers, y = UpperBedAmmoniteNumbers,
  pch = 20, col = "black", main = "My plot title", xlab = "My x-axis label",
  ylab = "My y-axis label")

# Bivariate plot - scatter plot (with points filled black, plot title, axis
# labels and logged y-axis):
plot(x = UpperBedAmmoniteNumbers, y = UpperBedAmmoniteNumbers,
  pch = 20, col = "black", main = "My plot title", xlab = "My x-axis label",
  ylab = "My y-axis label", log = "y")

# Bivariate plot - scatter plot (with points filled black, plot title, axis
# labels and logged x-axis):
plot(x = UpperBedAmmoniteNumbers, y = UpperBedAmmoniteNumbers,
  pch = 20, col = "black", main = "My plot title", xlab = "My x-axis label",
  ylab = "My y-axis label", log = "x")

# Bivariate plot - scatter plot (with points filled black, plot title, axis
# labels and both axes logged):
plot(x = UpperBedAmmoniteNumbers, y = UpperBedAmmoniteNumbers,
  pch = 20, col = "black", main = "My plot title", xlab = "My x-axis label",
  ylab = "My y-axis label", log = "xy")

# Export your plots - modify filename and copy and paste your plot code
# between the pdf() and dev.off() functions - see (e.g.) practical one for
# examples:
pdf("plotfilename.pdf")
[REPLACE THIS LINE WITH YOUR PLOT CODE - WILL JUST GIVE YOU AN ERROR IF NOT!]
dev.off()

################################################################################
#                                                                              #
#                              SPATIAL STATISTICS                              #
#                                                                              #
################################################################################

# Chi-squared test for uniform and random spatial distributions
#
# First you must pick the cells you will use to count your ammonites. Note that
# these do not need to be squares (As we have done previously), but they should
# all be the same shape and size (e.g., you could have ten 20 cm by 50 cm
# rectangles if you wished).
#
# Next you need to count the number of ammonites in each cell then enter each
# cell count into the following as numbers separated by commas:
CellCounts <- c()

# Next you need to work out the expected number of ammonites per grid cell by
# taking the total number of ammonites and dividing it by the total number
# of grid cells:
ExpectedNAmmonitesPerGridCell <-

# Now you can get your Chi-ssquared value for the uniform null hypothesis
# with:
sum(abs(((CellCounts - ExpectedNAmmonitesPerGridCell) ^ 2) /
  ExpectedNAmmonitesPerGridCell))

# Then set your uniform degrees of freedom (N grid cells minus 1):
UniformDF <-

# And pick an alpha value:
Alpha <-

# Then find your critical value. Remember if your Chi-squared is higher than
# this you reject the null, lower you accept it:
qchisq(p = 1 - Alpha, df = UniformDF)

# Now for the test for random you will need to decide how many values to use
# from your Poisson distribution. (Remember that technically this goes to
# infinity, but practically we can't do that.) Try picking different values
# and use the barplot to figure out a good one (encompasses where most of the
# data are):
PoissonLimit <-
barplot(height = dpois(x = 0:PoissonLimit, lambda = ExpectedNAmmonitesPerGridCell),
  space = 0, border = 0, names.arg = 0:PoissonLimit,
  xlab = "N ammonites in a grid cell", ylab = "p(X = x)")

# Now you can get the actual counts of number of grid cells with N ammonites
# using:
AmmoniteCounts <- apply(apply(matrix(CellCounts), 1, '==', 0:PoissonLimit), 1, sum)
names(AmmoniteCounts) <- 0:PoissonLimit
AmmoniteCounts

# And the Chi-squared value for your test where the null hypothesis is random
# distribution with:
sum(abs(((AmmoniteCounts - (dpois(x = 0:PoissonLimit,
  lambda = ExpectedNAmmonitesPerGridCell) * length(CellCounts))) ^ 2) / (dpois(x = 0:PoissonLimit,
  lambda = ExpectedNAmmonitesPerGridCell) * length(CellCounts))))

# This will set your degrees of freedom (NB: there is no minus one as zero is already
# being excluded:
RandomDF <- PoissonLimit

# And finally your critical value with (uses same Alpha as before):
qchisq(p = 1 - Alpha, df = RandomDF)

# Spatial statistics - nearest neighbour statistic (R):
XValues <- UpperBedAmmoniteNumbers
YValues <- UpperBedAmmoniteNumbers
SpaceDimensions <- 1000 * 1000 # Assumes quadrat dimensions in mm
NearestNeighbourDistances <- apply(as.matrix(dist(x = cbind(XValues,
  YValues), diag = TRUE, upper = TRUE)), 1, sort)[2, ]
mean(NearestNeighbourDistances) / (0.5 * sqrt((SpaceDimensions) / length(XValues)))

################################################################################
#                                                                              #
#                              DIRECTIONAL STATISTICS                          #
#                                                                              #
################################################################################

# Convert ammonite numbers to circular (again, this is nonsense data!):
UpperBedAmmoniteNumbersAsCircular <- circular(x = UpperBedAmmoniteNumbers,
  units = "degrees", modulo = "2pi", zero = pi / 2, rotation = "clock")

# Directional statistics - circular mean:
mean.circular(UpperBedAmmoniteNumbersAsCircular)

# Directional statistics - circular median:
median.circular(UpperBedAmmoniteNumbersAsCircular)

# Directional statistics - circular variance:
var.circular(UpperBedAmmoniteNumbersAsCircular)

# Directional statistics - circular standard deviation:
sd.circular(UpperBedAmmoniteNumbersAsCircular)

# Directional statistics - mean resultant length:
rho.circular(UpperBedAmmoniteNumbersAsCircular)

# Directional statistics - Von Mises circular mean:
mle.vonmises(x = UpperBedAmmoniteNumbersAsCircular)$mu

# Directional statistics - Von Mises concentration parameter:
mle.vonmises(x = UpperBedAmmoniteNumbersAsCircular)$kappa

# Directional statistics - Rayleigh test probability that directions are
# uniformly distributed:
rayleigh.test(x = UpperBedAmmoniteNumbersAsCircular)$p.value

# Directional statistics - Watson two-sample test:
Alpha <-
watson.two.test(x = UpperBedAmmoniteNumbersAsCircular,
  y = UpperBedAmmoniteNumbersAsCircular, alpha = Alpha)

# Directional plots - circular plot:
plot.circular(circular(x = UpperBedAmmoniteNumbersAsCircular,
  units = "degrees", modulo = "2pi", zero = pi / 2, rotation = "clock"))

# Directional plots - circular plot (with title):
plot.circular(circular(x = UpperBedAmmoniteNumbersAsCircular,
  units = "degrees", modulo = "2pi", zero = pi / 2, rotation = "clock"),
  main = "My plot title")

# Directional plots - rose diagram (vanilla version):
rose.diag(x = UpperBedAmmoniteNumbersAsCircular)

# Directional plots - rose diagram (with number of equal sized bins):
rose.diag(x = UpperBedAmmoniteNumbersAsCircular, bins = 36)

# Directional plots - rose diagram (with number of equal sized bins and
# specified colour of bars):
rose.diag(x = UpperBedAmmoniteNumbersAsCircular, bins = 36, col = "black")

# Directional plots - rose diagram (with number of equal sized bins,
# specified colour of bars, and plot title):
rose.diag(x = UpperBedAmmoniteNumbersAsCircular, bins = 36, col = "black",
  main = "My plot title")

# Export your plots - modify filename and copy and paste your plot code
# between the pdf() and dev.off() functions - see (e.g.) practical one for
# examples:
pdf("plotfilename.pdf")
[REPLACE THIS LINE WITH YOUR PLOT CODE - WILL JUST GIVE YOU AN ERROR IF NOT!]
dev.off()

# Chi-squared test for uniform and random directional distributions
#
# First you must pick the bins you will use to count your bearings. Generally
# it will be easier if you pick a value that divides nicely into 360.
#
# Next you need to count the number of bearings in each bin and enter each
# count into the following as numbers separated by commas:
NBearingsPerBin <- c()

# Next you need to work out the expected number of ammonites per bin by
# taking the total number of ammonites and dividing it by the total number
# of bins:
ExpectedBearings <-

# Now you can get your Chi-squared value for the uniform null hypothesis
# with:
sum(abs(((NBearingsPerBin - ExpectedBearings) ^ 2) / ExpectedBearings))

# Then set your uniform degrees of freedom (N bins minus 1):
UniformDF <-

# And pick an alpha value:
Alpha <-

# Then find your critical value. Remember if your Chi-squared is higher than
# this you reject the null, lower you accept it:
qchisq(p = 1 - Alpha, df = UniformDF)

# Now for the test for random you will need to decide how many values to use
# from your Poisson distribution. (Remember that technically this goes to
# infinity, but practically we can't do that.) Try picking different values
# and use the barplot to figure out a good one (encompasses where most of the
# data are):
PoissonLimit <-
barplot(height = dpois(x = 0:PoissonLimit, lambda = ExpectedBearings),
  space = 0, border = 0, names.arg = 0:PoissonLimit,
  xlab = "N ammonites in a bin", ylab = "p(X = x)")

# Now you can get the exected and actual counts of number of bins with N
# ammonites using:
ExpectedBearingsPerBin <- dpois(x = 0:PoissonLimit, lambda = Lambda) *
  length(ExpectedBearings)
ObservedBearingsPerBin <- apply(apply(matrix(NBearingsPerBin), 1, '==',
  0:PoissonLimit), 1, sum)

# And the Chi-squared value for your test where the null hypothesis is random
# distribution with:
sum(abs(((ObservedBearingsPerBin - ExpectedBearingsPerBin) ^ 2) /
  ExpectedBearingsPerBin))

# This will set your degrees of freedom (NB: there is no minus one as zero is
# already being excluded:
RandomDF <- PoissonLimit

# And finally your critical value with (uses same Alpha as before):
qchisq(p = 1 - Alpha, df = RandomDF)

################################################################################
#                                                                              #
#                                 TROUBLESHOOTING                              #
#                                                                              #
################################################################################

# The most likely issues you will face are typos or incomplete copy-pastes. R,
# like pretty much all programming languages, needs to be told precisely what
# to do. You should be pretty familiar with this by now, but make sure
# everything is typed correctly, including case (i.e., This is not this).
#
# Similarly many symbols must be paired, e.g., opening quotes " or brackets
# ([{ must have corresponding closing quotes " or brackets )]}. If you
# miss one of these you will likely see that the prompt (> symbol) will turn
# into a plus symbol (+). If this happens the way out is to hit the escape key
# (which will cancel the current operation), then try again.
#
# Another issue you may have is with plots as sometimes on PCs plot windows do
# not get "killed" properly. If this occurs try typing dev.off() (a contraction
# of (plotting) device off) until you get an error then try plotting again.
#
# One final issue I have seen a few times, especially if executing a large
# block of code, is that you should make sure you hit enter on your last line
# of code. (Sometimes R will switch to a new active window, normally the
# plotting device, and you may think this means all the lines have been
# executed, but do make sure this is the case by checking that the console
# has gone to the next line, i.e., that the cursor is sitting at a new
# prompt (>).)
