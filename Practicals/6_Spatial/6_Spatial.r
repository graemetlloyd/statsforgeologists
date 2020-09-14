################################################################################
#                                                                              #
#                       PRACTICAL VI - SPATIAL STATISTICS                      #
#                                                                              #
################################################################################

################################################################################
#                                                                              #
#                               TODAY'S AIMS                                   #
#                                                                              #
# - LEARN TO APPLY THE CHI-SQUARED TEST AND GRID CELL COUNTS TO TEST FOR       #
#   UNIFORM AND RANDOM POINT DISTRIBUTIONS                                     #
# - LEARN TO APPLY THE NEAREST NEIGHBOUR INDEX TO CHOOSE AMONGST POINT         #
#   DISTRIBUTIONS                                                              #
# - LEARN HOW TO PERFORM A SIMPLE 3D REGRESSION                                #
#                                                                              #
################################################################################

# Before we start begin by setting your working directory.

# Today you will continue using your quadrat to see if you can confirm what you
# found in lecture (that your ammonite quadrat best matches a uniform point
# distribution). However, this time you will use a different grid pattern (see
# slide at front) and apply some nearest neighbour distance analysis. For the
# latter you are going to need an xy coordinate system for your quadrat.
# Specifically, we need to set an ORIGIN, the ppint where x = 0 and y = 0, and
# an ORIENTATION, which decides the directions in which x and y increase, i.e.,
# what we consider the x and y axes to be. Here we will treat North as up
# (marked in the top left of your quadrat) and the South-West (bottom left)
# corner of your quadrat as the origin. We will also use millimetres as our
# scale. Thus the bottom left corner will have the xy coordinates 0,0, the top
# left 0,1000, the top right 1000,1000, and the bottom right 1000,0. You can
# thus use the custom rulers you previously used to measure diameters to assign
# xy coordinates to the centres of each ammonite. Here I recommend you use the
# thin grey lines as guides as these show the nearest 100 millimetre grid cell.
# As you go replace the ammonite numbers below with the x and y coordinates for
# each one:
XValues <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
  20)
YValues <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
  20)

# You can check your coordinates make sense by producing a plot:
plot(x = XValues, y = YValues, xlim = c(0, 1000), ylim = c(0, 1000),
  type = "n", axes = FALSE, xlab = "", ylab = "")
for(i in 0:10 * 100) {
  lines(x = c(0, 1000), y = c(i, i), col = "grey")
  lines(x = c(i, i), y = c(0, 1000), col = "grey")
}
text(x = XValues, y = YValues, labels = 1:20)

# This should look like your quadrat, but with only the centre points of each
# ammonite marked as the ammonite number. If anything looks out of place then
# go check your coordinates and fix any problems you find. Alternatively, you
# can cheat and import this data by swapping out "X4" with your own quadrat
# number on the following lines:
XValues <- read.csv("https://raw.githubusercontent.com/graemetlloyd/statsforgeologists/master/Ammonite_quadrats/Lower_Bed/Quadrat_X4.csv")[, "X_coordinate"]
YValues <- read.csv("https://raw.githubusercontent.com/graemetlloyd/statsforgeologists/master/Ammonite_quadrats/Lower_Bed/Quadrat_X4.csv")[, "Y_coordinate"]

# Repeat the plot from above as a final check the coordinates work:
plot(x = XValues, y = YValues, xlim = c(0, 1000), ylim = c(0, 1000),
type = "n", axes = FALSE, xlab = "", ylab = "")
for(i in 0:10 * 100) {
  lines(x = c(0, 1000), y = c(i, i), col = "grey")
  lines(x = c(i, i), y = c(0, 1000), col = "grey")
}
text(x = XValues, y = YValues, labels = 1:20)

# We will return to this data later. For now we will focus on the grid
# count approach we encountered in lecture, but this time you will divide
# your quadrat into 25 squares (each 2 x 2 of the smaller grey cells, i.e.,
# 20cm x 20cm; see slide at front). Repeat what we did in lecture and count
# the number of ammonite (centres) in each grid cell and modify the below
# accordingly before running the line.
#
# You may find that some ammonite centres fall (or at least appear to fall)
# exactly on the border of your grid cell. In these cases we have a couple
# of options available to us:
#
# 1. Count the ammonite in both cells, but as a fraction (i.e., one half,
#    0.5) in each cell.
# 2. Run two separate analyses where the ammonite is counted in either one
#    cell or the other.
#
# Here I suggest you adopt the former as it is practically easier for you to
# only do one analysis, plus if this happened multiple times you would end up
# having to do 4, 8, 16 etc. analyses. The important point is no matter what
# you do that the sum should be the same as your totla number of ammonites:
GridCounts <- c(
0, 0, 0, 0, 0, # first row of grid cells
0, 0, 0, 0, 0, # second row of grid cells
0, 0, 0, 0, 0, # third row of grid cells
0, 0, 0, 0, 0, # fourth row of grid cells
0, 0, 0, 0, 0) # fifth row of grid cells

# To double check you got them all you can check that the count adds up to 20
# with:
sum(GridCounts) == 20

# If you didn't get TRUE for this then you missed something - go back and
# check you got every ammonite. NB: It is always worth doing little checks
# like this as human error is inevitable, but if it creeps through then bad
# mistakes can be made as values may reflect not the data, but a missed value,
# typo etc.
#
# We are now going to repeat the first Chi-squared test we did in lecture
# (where we are testing for a uniform distribution - i.e., an even, or nearly
# even distribution of points). To do this we need to set our EXPECTED value for
# the number of ammonites in each grid cell. As in lecture this will be the total
# number of ammonites divided by the number of grid cells. Once you know this
# number enter it after the "<-" in the below and run it in R. (NB: remember that
# it doesn't have to be a whole number!):
ExpectedNAmmonitesPerGridCell <-

# We now need to get the differences between each actual count and the expected
# value (O_j - E_j from the Chi-squared equation). We can do this with:
GridCounts - ExpectedNAmmonitesPerGridCell

# Next we want the square of this difference. We can get this with:
(GridCounts - ExpectedNAmmonitesPerGridCell) ^ 2

# These values then need to be divided by the expected value. We can do this
# with:
((GridCounts - ExpectedNAmmonitesPerGridCell) ^ 2) /
  ExpectedNAmmonitesPerGridCell

# Note that at this stage we should also make sure the values are absolute
# (positive). We can do this with the abs() function in R, but you should see
# in this example all our data will already be positive. This is only really
# relevant in scenarios where the expected value would be negative. For
# completeness we will apply this function anyway as it doesn't change
# anything:
abs(((GridCounts - ExpectedNAmmonitesPerGridCell) ^ 2) /
  ExpectedNAmmonitesPerGridCell)

# Now to get our Chi-squared value we simply need to sum the above:
sum(abs(((GridCounts - ExpectedNAmmonitesPerGridCell) ^ 2) /
  ExpectedNAmmonitesPerGridCell))

# This is your Chi-squared value. Note it down for now as we need some context
# still to interpret it. (You should see, however, that it is different from the
# one you got in lecture.) Note that this is purely because we have changed the
# grid we have used. The raw data (position and number of ammonites) has not
# altered. Thus choosing an appropriate grid is important, and more generally
# the way data are broken down into different categories to perform a Chi-
# squared test matters. However, we shouldn't yet be worried that this has
# changed our answer (i.e., which hypothesis we go with) as changing our grid
# also changes our degrees of freedom. What are the degrees of freedom now?
# Fill in the value after the "<-" here:
DF <-

# (Make sure you run this line after editing it!) As we saw in lecture
# the degrees of freedom change the shape of the Chi-squared distribution,
# which changes the probability of your specific Chi-squared value as well as
# the critical value that decides whether we accept or reject the null (which
# in the current example is that the data are uniformly distributed). We can
# plot the distribution (over the range 0-50) with:
ChiSqRange <- seq(from = 0, to = 50, length.out = 1000)
plot(x = ChiSqRange, y = dchisq(x = ChiSqRange, df = DF), type = "n",
  ylab = "f(x)", xlab = "Chi-squared value", main = "")
polygon(x = c(0, ChiSqRange, 50, 0), y = c(0, dchisq(x = ChiSqRange, df = DF),
  0, 0), border = NA, col = "grey")

# You should see that the probablity density is now quite far "adrift" from
# zero, even though zero would represent the perfectly uniform (regular)
# outcome. This should make some sense though, and it would actually be
# mathematically impossible for your quadrat to match the expected number of
# ammonites here:
ExpectedNAmmonitesPerGridCell

# I.e., as this isn't an integer we could never count that many ammonites
# and hence the observed-expected difference could never be zero, regardless
# of how the ammonites are distributed. Next we will apply a null hypothesis
# statistical test. Remember from lecture that this is a situation where a
# two-tailed test doesn't make sense. Instead as our null hypothesis is a
# uniform distribution we are really concerned only if our data has a Chi-
# squared value that is too high (which would imply random or clustered
# distributions). Thus we are performing a right-tailed test. As in lecture
# we will use an Alpha of 0.1:
Alpha <- 0.1

# We can then replot our data with the critical value included so you can
# decide if your quadrat represents a uniform distribution (null; chi-square
# value falls below the critical value) or not (alternative; Chi-squared value
# falls above the critical value):
ChiSqRange <- seq(from = 0, to = 50, length.out = 1000)
plot(x = ChiSqRange, y = dchisq(x = ChiSqRange, df = DF), type = "n",
  ylab = "f(x)", xlab = "Chi-squared value",
  main = paste("Null: data are uniformly distributed\ndf = ", DF + 1,
  " - 1; Alpha = ", Alpha, "; right-tailed test", sep = ""))
polygon(x = c(0, ChiSqRange, 50, 0), y = c(0, dchisq(x = ChiSqRange, df = DF),
  0, 0), border = NA, col = "grey")
CriticalValue <- qchisq(p = 1 - Alpha, df = DF)
polygon(x = c(CriticalValue, ChiSqRange[ChiSqRange >= CriticalValue], 50,
  CriticalValue), y = c(0, dchisq(x = ChiSqRange[ChiSqRange >= CriticalValue],
  df = DF), 0, 0), border = NA, col = "red")
lines(x = c(CriticalValue, CriticalValue), y = c(0,
  max(dchisq(x = ChiSqRange, df = DF)) * 0.95), col = "blue", lwd = 3)
text(x = CriticalValue, y = max(dchisq(x = ChiSqRange, df = DF)),
  labels = round(CriticalValue, 2), col = "blue")

# Where does your Chi-squared value fall? Which hypothesis does this correspond
# to? Does this differ from the result you got in lecture using the larger
# (and hence fewer) grid cells?
#
# Next we will consider whether your own quadrat best fits a random
# distribution. Remember from lecture we found that using the 4 grid cells per
# quadrat approach for all quadrats combined we rejected a random
# distribution. Thus we might expect the same result here. However, two things
# will be different this time: 1) we are only looking at a single quadrat (a
# distribution that holds across the whole sample may be different for smaller
# subsets of that sample), and 2) we are now using smaller grid cells (some
# spatial patterns are scale-dependent and what holds at one scale may be
# different at another). Switching to a null of randomly distributed ammonites
# also requires "binning" the data differently as now our expectation will
# reflect the number of grid cells with N ammonites rather than a global
# expectation of N ammonites in every grid cell. The key parameter for the
# Poisson distribution (which we will use as our model of randomly distributed
# ammonites) is the mean number of ammonites per grid cell. We already
# calculated this earlier:
ExpectedNAmmonitesPerGridCell

# Thus we can get an appropriate Poisson distribution with:
barplot(height = dpois(x = 0:10, lambda = ExpectedNAmmonitesPerGridCell),
  space = 0, border = 0, names.arg = 0:10, xlab = "N ammonites in a grid cell",
  ylab = "p(X = x)")

# Note that here we are only looking at the probability of grid cells having
# zero to ten ammonites in them. Technically there are non-zero probabilities
# for every number of ammonites off to infinity, but these will be so close to
# zero in many cases that we don't need to worry too much about them. We can
# also contrast this expectation with the graphical equivalent of our previous
# test for the uniform distribution:
barplot(height = c(0, 1, rep(0, 9)), space = 0, border = 0,
  names.arg = 0:10, xlab = "N ammonites in a grid cell", ylab = "p(X = x)")

# (Note that this isn't quite correct, as the expected value is actually a
# fraction, but this should give a visual idea of the difference between
# uniform and random distribution expectations.) Next you need to convert your
# data to this same format (i.e., number of grid cells with each ammonite
# count, from zero to ten). We can do this with some clever use of the
# "apply" function, which lets us apply some other function to every row or
# column of a matrix. Here we will convert our data to a single column matrix
# and ask how many values are equal to zero, one, two, etc. up to ten and then
# get the sums of each count:
AmmoniteCounts <- apply(apply(matrix(GridCounts), 1, '==', 0:10), 1, sum)
names(AmmoniteCounts) <- 0:10
AmmoniteCounts

# We can plot this too:
barplot(height = AmmoniteCounts, space = 0, border = 0, names.arg = 0:10,
  xlab = "N ammonites in a grid cell", ylab = "Frequency")

# Next we need to change our Poisson distribution from a probability density
# plot to an actual expectation by multiplying each probability by the total
# number of ammonites (20):
barplot(height = dpois(x = 0:10, lambda = ExpectedNAmmonitesPerGridCell) * 20,
  space = 0, border = 0, names.arg = 0:10, xlab = "N ammonites in a grid cell",
  ylab = "Frequency")

# Note the y-axis has changed to frequency, and more generally that values of
# greater than one are allowed now. We can also see quite clearly that a
# random distribution predicts the most common number of ammonites in a grid
# cell should be zero, whereas under our uniform expectation one would be the
# mode (most common) number. This should give you a heads up of where your
# data fall. Now you can begin to calculate your Chi-squared again, this time
# with a new expectation (random distribution). Note this ability to pick
# different expectations is the major strength of the Chi-squared test. We can
# start by taking our observed values (counts of grid cells with N ammonites)
# and subtract our Poisson distribution expectation with:
AmmoniteCounts - (dpois(x = 0:10, lambda = ExpectedNAmmonitesPerGridCell) * 20)

# And now get the square of these with:
(AmmoniteCounts - (dpois(x = 0:10,
  lambda = ExpectedNAmmonitesPerGridCell) * 20)) ^ 2

# Divide this by our expected values with:
((AmmoniteCounts - (dpois(x = 0:10,
  lambda = ExpectedNAmmonitesPerGridCell) * 20)) ^ 2) / (dpois(x = 0:10,
  lambda = ExpectedNAmmonitesPerGridCell) * 20)

# Make these absolute with:
abs(((AmmoniteCounts - (dpois(x = 0:10,
  lambda = ExpectedNAmmonitesPerGridCell) * 20)) ^ 2) / (dpois(x = 0:10,
  lambda = ExpectedNAmmonitesPerGridCell) * 20))

# And finally get your actual Chi-squared value by taking the sum with:
sum(abs(((AmmoniteCounts - (dpois(x = 0:10,
  lambda = ExpectedNAmmonitesPerGridCell) * 20)) ^ 2) / (dpois(x = 0:10,
  lambda = ExpectedNAmmonitesPerGridCell) * 20)))

# Again we cannot interpret a Chi-squared value in isolation (unless it is
# exactly zero - i.e., the data exactly match the expectation). We will need
# to work out the degrees of freedom again. This time it will be the number of
# bins minus one. Here we used eleven (zero to ten) bins, so fill out the
# appropriate degrees of freedom below (add the number after the "<-") and
# run the line:
RandomDF <-

# We can generate the new plot (with critical value for interpretation) with:
ChiSqRange <- seq(from = 0, to = 50, length.out = 1000)
plot(x = ChiSqRange, y = dchisq(x = ChiSqRange, df = RandomDF), type = "n",
  ylab = "f(x)", xlab = "Chi-squared value",
  main = paste("Null: data are randomly distributed\ndf = ", RandomDF + 1,
  " - 1; Alpha = ", Alpha, "; right-tailed test", sep = ""))
polygon(x = c(0, ChiSqRange, 50, 0), y = c(0, dchisq(x = ChiSqRange,
  df = RandomDF), 0, 0), border = NA, col = "grey")
CriticalValue <- qchisq(p = 1 - Alpha, df = RandomDF)
polygon(x = c(CriticalValue, ChiSqRange[ChiSqRange >= CriticalValue], 50,
  CriticalValue), y = c(0, dchisq(x = ChiSqRange[ChiSqRange >= CriticalValue],
  df = RandomDF), 0, 0), border = NA, col = "red")
lines(x = c(CriticalValue, CriticalValue), y = c(0,
  max(dchisq(x = ChiSqRange, df = RandomDF)) * 0.95), col = "blue", lwd = 3)
text(x = CriticalValue, y = max(dchisq(x = ChiSqRange, df = RandomDF)),
  labels = round(CriticalValue, 2), col = "blue")

# Where does your Chi-squared fall with respect to the critical value? Which
# hypothesis does this correspond to? Does this agree with your interpretation
# from the uniform hypothesis test we conducted before? Check with your
# neighbour(s) to see what they found.
#
# Another way to test for a particular type of point distribution is the
# Nearest Neighbour Statistic, sometimes called the NEAREST NEIGHBOUR INDEX
# (NNI). To calculate this we can use our coordinate data from earlier:
plot(x = XValues, y = YValues, xlim = c(0, 1000), ylim = c(0, 1000),
type = "n", axes = FALSE, xlab = "", ylab = "")
for(i in 0:10 * 100) {
  lines(x = c(0, 1000), y = c(i, i), col = "grey")
  lines(x = c(i, i), y = c(0, 1000), col = "grey")
}
text(x = XValues, y = YValues, labels = 1:20)

# In order to calculate this metric we first have to calculate the nearest
# neighbour distance for each ammonite, from one to twenty. Note that you
# could do this manually with a ruler, but this would get tedious quickly as
# there are (N^2 - N) / 2 distances to measure here (i.e., 190 in this case).
# Of course, as a human you would quickly realise that many of these do not
# need to be measured as you can visually gauge that most points can not possibly
# represent the nearest neighbour to some other point. Nevertheless a computer
# can do this task for you extremely fast, precisely, and without error. To get
# the distances between every one of our ammonites we can simply use:
dist(x = cbind(XValues, YValues), diag = TRUE, upper = TRUE)

# This is what is known as a DISTANCE MATRIX and as such has a specific set of
# properties: 1) It is square (has same number of rows as columns), 2) Each
# row and column have exact matching pairs (i.e., each ammonite appears as
# both a row and a column - this is why the matrix must be square), 3) The
# DIAGONAL of the matrix (top left through to bottom right value) is always
# all zero values, 4) This is because these values represent the distance
# between each object (ammonite) and itself (i.e., they must occupy the same
# space as they are the same thing), 5) Each OFF-DIAGONAL value represents
# the distance between two objects that are *not* the same object, 6) These
# distances are SYMMETRIC about the diagonal (i.e., the distance for row
# 2, column 3 must be the same as for row 3, column 2 - try checking this
# yourself), and 7) This means that the diagonal and the "upper right"
# triangle represent REDUNDANT information and hence we can represent the
# full range of information with just the "lower left" triangle:
dist(x = cbind(XValues, YValues))

# These distances are in millimetres. Try picking a pair of ammonites and
# visually confirming the distance is at least approximately correct:
plot(x = XValues, y = YValues, xlim = c(0, 1000), ylim = c(0, 1000),
type = "n", axes = FALSE, xlab = "", ylab = "")
for(i in 0:10 * 100) {
  lines(x = c(0, 1000), y = c(i, i), col = "grey")
  lines(x = c(i, i), y = c(0, 1000), col = "grey")
}
text(x = XValues, y = YValues, labels = 1:20)

# (Remember that each grid cell in the plot is 100 x 100 millimetres.) For the
# nearest neighbour distance we do not need all of these distances, just the
# shortest one. We can get these by using our apply function again. First we
# will want to sort each row (or column, it doesn't matter here as the matrix
# is symmetric) independently:
apply(as.matrix(dist(x = cbind(XValues, YValues), diag = TRUE, upper = TRUE)),
  1, sort)

# You should now see each column is sorted from shortest to longest distance
# from that ammonite (column name) to every other ammonite. However, note that
# we don't actually want the shortest distance here (the top row) as this is
# our diagonal from before - the distance from each ammonite to itself.
# Instead we want the second row:
apply(as.matrix(dist(x = cbind(XValues, YValues), diag = TRUE, upper = TRUE)),
  1, sort)[2, ]

# These are our nearest neighbour distances and we will store them in an
# appropriately named variable with:
AmmoniteNearestNeighbourDistances <- apply(as.matrix(dist(x = cbind(XValues,
  YValues), diag = TRUE, upper = TRUE)), 1, sort)[2, ]
AmmoniteNearestNeighbourDistances

# You are already most of the way to calculating the NNI (R). The mean nearest
# neighbour distance (d-bar term) for your quadrat is thus:
mean(AmmoniteNearestNeighbourDistances)

# Next we need to get an expected mean nearest neighbour distance under the
# assumption of a random distribution. We saw the equation for this in lecture
# - half of the square root of the total area of the space divided by the
# number of objects. Here our space is 1000 by 1000 millimetres (remember these
# units must match those of your distances!), so the area, A, is:
1000 * 1000

# And you have 20 ammonites. Thus your expected mean nearest neighbour distance
# is:
0.5 * sqrt((1000 * 1000) / 20)

# You can now calculate R, with:
mean(AmmoniteNearestNeighbourDistances) / (0.5 * sqrt((1000 * 1000) / 20))

# What kind of point distribution does this value of R correspond to? (Check
# the slides from lecture.) Does this fit with your determination(s) from the
# Chi-squared tests earlier? Assuming you are now happy with which point
# distribution your ammonites represent what might this suggest about their
# preservation as fossils? I.e., why might they fit this particular pattern?
# (Hint: we discussed part of the solution to this near the beginning of
# the lecture, but a more complete understanding will come when we look at
# directional statistics.)
#
# Now let's briefly consider 3D regression. Imagine you spent some time in the
# field tracing a particular contact between two rock units across a landscape.
# You suspect this contact represents a plane that dips towards the South-East
# but you are unable to take dips and strikes due to inaccessibility or the
# absence of a good surface on which to place your compass-clinometer. However,
# you do have a GPS with you that can give your coordinates (as Eastings and
# Northings) and altitude (in feet). You record nine outcrops of your contact
# this way:
Eastings <- c(5, 22, 31, 44, 48, 59, 75, 79, 95)
Northings <- c(13, 94, 29, 56, 58, 12, 75, 42, 31)
Altitude <- c(311, 399, 380, 364, 342, 308, 375, 306, 285)

# You then perform a 3D regression on this data as you would a 2D regression
# using the lm (linear model) function, but this time with two explanatory
# variables (Eastings and Northings) and one response variable (Altitude):
lm(Altitude ~ Eastings + Northings)

# You thus get an intercept and two "slope" parameters, but these are now
# harder to interpret. As with most statistical problems the easiest way to
# interpret the data is to plot it, but to do that we will need a special
# 3D package:
install.packages("scatterplot3d", dependencies = TRUE)
library(scatterplot3d)

# You should now be able to visualise the data with:
s3d <- scatterplot3d(x = Eastings, y = Northings, z = Altitude, type = "h",
  pch = 20, color = "red", xlab = "Easting", ylab = "Northing",
  zlab = "Altitiude (ft)")
Plane <- lm(Altitude ~ Eastings + Northings)
s3d$plane3d(Plane)

# NB: If this doesn't work because you are unable to install the package the
# graph can also be accessed with:
browseURL("https://github.com/graemetlloyd/statsforgeologists/raw/master/ExamplePlots/3DRegression.pdf")

# You should see the data represent a 3D scatter plot with axes corresponding
# to our three-dimensions: Eastings, Northings, and Altitude. To aid the
# visualisation each point is on a "stalk" so you can more clearly see where
# it emerges from on the horizontal plane. The regression plane is shown with
# dashed lines. Which way does the contact dip? Does this agree with your
# estimate made in the field? Could we, at least theoretically, extract a
# dip and strike from this plane? What kind of steps would we have to go
# through to do this?

################################################################################
#                                                                              #
#                                     MCQ                                      #
#                                                                              #
# You are now ready to attempt this week's MCQ which is a series of ten        #
# multiple choice questions (MCQs) on last week's lecture and this practical.  #
#                                                                              #
################################################################################
