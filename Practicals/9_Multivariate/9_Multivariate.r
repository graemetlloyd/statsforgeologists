################################################################################
#                                                                              #
#                    PRACTICAL IX - MULTIVARIATE STATISTICS                    #
#                                                                              #
################################################################################

################################################################################
#                                                                              #
#                               TODAY'S AIMS                                   #
#                                                                              #
# - LEARN HOW TO PERFORM A SIMPLE BIVARIATE ORDINATION IN R AND INTERPRET THE  #
#   SPACE (DISTRIBUTION OF VARIANCE ACROSS AXES AND MEANING OF MAJOR AXES OF   #
#   VARIANCE)                                                                  #
# - LEARN HOW TO PERFORM MORE COMPLEX MULTIVARIATE ORDINATIONS IN R AND        #
#   INTERPRET THE RESULTING SPACE                                              #
# - LEARN HOW TO APPLY MULTIVARIATE METHODOLOGIES TO SPHERICAL DATA AND PLOT   #
#   STEREONETS AND TERNARY DIAGRAMS IN R                                       #
#                                                                              #
################################################################################

# Before we start begin by setting your working directory.

# Today we are going to use some libraries that will let us produce ternary
# plots and stereonets in R. You can install these with:
install.packages("Ternary", dependencies = TRUE)
install.packages("pracma", dependencies = TRUE)
install.packages("RFOC", dependencies = TRUE)

# And load them with:
library(Ternary)
library(pracma)
library(RFOC)

# But before we get into multivariate analysis in R we will do a final visit
# of the Lower Bed ammonite quadrat data. To do this modify the line below to
# your own ammonite quadrat:
QuadratNumber <- "X12"

# From this the following will build a web address (URL) that will point to
# the correct measurements for your quadrat:
CSVPath <- paste("http://www.graemetlloyd.com/teaching/SOEE1475/20/Lower_Bed/Quadrat_", QuadratNumber, ".csv", sep = "")
CSVPath

# You can read this data into R with:
AmmoniteQuadratValues <- read.csv(CSVPath)
AmmoniteQuadratValues

# These are the ammonites (numbered 1-20), their X and Y coordinates (in
# millimetres from an origin at the bottom left of the quadrat), their
# diameters (in millimetres), the number of chambers, the bearing of their
# apertures (in degrees), and the chirality of their spirals as viewed from
# above. If you compare these to your own measurements taken in previous
# practicals you should hopefully find they are very similar. However, you
# will likely have also found you were unable to replicate the precision of
# these values. Don't worry about this as you were working with a smaller
# scale version and should still have got the right overall answers we
# discussed in lecture.
#
# Before we leave the lower bed behind for good we can use the xy coordinates
# to illustrate one form of multivariate analysis known as "Classic Multi-
# Dimensional Scaling" (CMDS), sometimes also called "Principal Coordinates".
# This works by accepting a distance matrix - a matrix where the rows and
# columns represent the same set of objects and the values the distances
# between them - and turning this into a set of coordinates for those objects.
# This can be used to do multivariate analysis as those distances can come from
# as many variables as desired. However, here we are just going to use two -
# our x and y axes. Note that this is not something we *should* do with this
# data, this is simply a way to illustrate the process. We can generate the
# distance matrix with:
AmmoniteDistanceMatrix <- dist(AmmoniteQuadratValues[,
  c("X_coordinate", "Y_coordinate")])
AmmoniteDistanceMatrix

# Next we can ordinate the data with:
AmmoniteOrdination <- cmdscale(d = AmmoniteDistanceMatrix, k = 19)

# Here we provide the function with the distance matrix (d) and set the number
# of axes we will allow the variance to be spread over (k). Here we are
# allowing the maximum possible (which is N - 1, for N total objects), i.e.,
# 20 ammonites - 1 = 19. You should see, however, that many of these axes are
# immediately discarded as their effective variance is zero (i.e., the warning
# message). You might be wondering why there are more than two axes at all as
# we know the raw data only requires two axes to plot it. Well you can
# visually compare the variance across each ordination axis with:
barplot(apply(AmmoniteOrdination, 2, var), ylab = "Variance", space = 0,
  border = 0, names.arg = 1:ncol(AmmoniteOrdination), xlab = "Ordination axes")

# What does this show you? Remember that each successive axis must always have
# LOWER variance than the preceding one. You should see that from the third axis
# onwards there is effectively no variance, i.e., the ordination has correctly
# identified that only two axes are really needed to explain the data. You can
# plot these with:
plot(x = AmmoniteOrdination[, 1], y = AmmoniteOrdination[, 2], xlab = "PC1",
  ylab = "PC2", type = "n", asp = 1)
text(x = AmmoniteOrdination[, 1], y = AmmoniteOrdination[, 2], labels = 1:20)

# What can you see when you look at this plot? Hint: try comparing it to the
# raw data and thinking about rotation and flips by using the code below:
par(mfrow = c(1, 2))
plot(x = AmmoniteOrdination[, 1], y = AmmoniteOrdination[, 2], xlab = "PC1",
  ylab = "PC2", type = "n", asp = 1, main = "Ordination")
text(x = AmmoniteOrdination[, 1], y = AmmoniteOrdination[, 2], labels = 1:20)
plot(x = AmmoniteQuadratValues[, "X_coordinate"],
  y = AmmoniteQuadratValues[, "Y_coordinate"], xlab = "X", ylab = "Y",
  type = "n", asp = 1, main = "Raw Data")
text(x = AmmoniteQuadratValues[, "X_coordinate"],
  y = AmmoniteQuadratValues[, "Y_coordinate"], labels = 1:20)

# You should see that there is not a substantial reduction in dimensionality,
# but the data do not really allow this as the spread of data is roughly
# equal in all directions - i.e., consistent with the spatial pattern of a
# uniform distribution we found previously. The other thing you should notice
# is there is likely some rotation of the data and possibly even a reflection.
# Remember a rotation is a typical part of the ordination process. If you look
# at the axis values you should also see some negative values and generally
# that the new origin (0, 0) sits directly in the centre of the data. Again,
# what we would expect from an ordination. Now let's look at a similar type of
# data set, the distance in miles (by road) between several major UK cities:
UKCityDistances <- matrix(c(0, 239, 53, 252, 174, 80, 181, 82, 65, 194, 97, 25,
  344, 210, 292, 186, 264, 88, 231, 217, 129, 195, 307, 239, 0, 195, 17, 65,
  160, 190, 319, 208, 77, 217, 229, 575, 419, 107, 418, 143, 154, 118, 74, 128,
  198, 538, 53, 95, 0, 208, 130, 38, 146, 128, 38, 160, 72, 34, 385, 241,
  240, 227, 229, 37, 178, 164, 76, 156, 347, 252, 17, 208, 0, 78, 173, 184,
  332, 222, 71, 231, 242, 589, 432, 124, 431, 131, 175, 138, 94, 141, 218,
  552, 174, 65, 130, 78, 0, 95, 141, 254, 144, 56, 153, 164, 511, 354, 141,
  353, 129, 90, 104, 70, 63, 157, 473, 80, 160, 38, 173, 95, 0, 123, 159, 71,
  123, 99, 70, 416, 279, 213, 259, 196, 16, 151, 137, 49, 156, 379, 181, 190,
  146, 184, 141, 123, 0, 260, 185, 112, 217, 174, 517, 383, 282, 360, 156,
  139, 235, 209, 161, 271, 480, 82, 319, 128, 332, 254, 159, 260, 0, 131,
  274, 155, 94, 265, 145, 364, 107, 343, 161, 301, 288, 204, 259, 228, 65,
  208, 38, 222, 144, 71, 185, 131, 0, 184, 34, 41, 371, 214, 238, 213, 257,
  59, 173, 162, 81, 131, 333, 194, 77, 160, 71, 56, 123, 112, 274, 184, 0,
  198, 191, 529, 389, 170, 372, 74, 123, 150, 114, 111, 212, 492, 97, 217,
  72, 231, 153, 99, 217, 155, 34, 198, 0, 73, 372, 216, 237, 214, 270, 81,
  165, 161, 90, 105, 335, 25, 229, 34, 242, 164, 70, 174, 94, 41, 191, 73, 0,
  351, 211, 271, 193, 257, 70, 208, 196, 110, 171, 314, 344, 575, 385, 589,
  511, 416, 517, 265, 371, 529, 372, 351, 0, 167, 602, 158, 600, 419, 530,
  527, 448, 477, 107, 210, 419, 241, 432, 354, 279, 383, 145, 214, 389, 216,
  211, 167, 0, 446, 45, 466, 270, 374, 371, 292, 321, 141, 292, 107, 240,
  124, 141, 213, 282, 364, 238, 170, 237, 271, 602, 446, 0, 444, 244, 203,
  120, 75, 164, 199, 565, 186, 418, 227, 431, 353, 259, 360, 107, 213, 372,
  214, 193, 158, 45, 444, 0, 442, 263, 373, 369, 290, 320, 121, 264, 143,
  229, 131, 129, 196, 156, 343, 257, 74, 270, 257, 600, 466, 244, 442, 0,
  196, 228, 189, 185, 285, 563, 88, 154, 37, 175, 90, 16, 139, 161, 59, 123,
  81, 70, 419, 270, 203, 263, 196, 0, 142, 127, 40, 140, 384, 231, 118, 178,
  138, 104, 151, 235, 301, 173, 150, 165, 208, 530, 374, 120, 373, 228, 142,
  0, 44, 102, 110, 493, 217, 74, 164, 94, 70, 137, 209, 288, 162, 114, 161,
  196, 527, 371, 75, 369, 189, 127, 44, 0, 88, 124, 490, 129, 128, 76, 141,
  63, 49, 161, 204, 81, 111, 90, 110, 448, 292, 164, 290, 185, 40, 102, 88,
  0, 116, 411, 195, 198, 156, 218, 157, 156, 271, 259, 131, 212, 105, 171,
  477, 321, 199, 320, 285, 140, 110, 124, 116, 0, 440, 307, 538, 347, 552,
  473, 379, 480, 228, 333, 492, 335, 314, 107, 141, 565, 121, 563, 384, 493,
  490, 411, 440, 0),  ncol = 23, dimnames = list(rev(c("Aberdeen",
  "Aberystwyth", "Birmingham", "Bristol", "Cardiff", "Derby", "Dover",
  "Edinburgh", "Exeter", "Glasgow", "Inverness", "Leeds", "Liverpool",
  "London", "Manchester", "Newcastle", "Norwich", "Nottingham", "Oxford",
  "Portsmouth", "Sheffield", "Southampton", "York")), rev(c("Aberdeen",
  "Aberystwyth", "Birmingham", "Bristol", "Cardiff", "Derby", "Dover",
  "Edinburgh", "Exeter", "Glasgow", "Inverness", "Leeds", "Liverpool",
  "London", "Manchester", "Newcastle", "Norwich", "Nottingham", "Oxford",
  "Portsmouth", "Sheffield", "Southampton", "York"))))
UKCityDistances

# We can ordinate this with CMDS again:
UKCityDistanceOrdination <- cmdscale(UKCityDistances,
  k = ncol(UKCityDistances) - 1)

# You should see, again, that we do not need all the axes of variance we ask
# for. Let's start by looking at variance across each axis again:
barplot(apply(UKCityDistanceOrdination, 2, var), ylab = "Variance",
  space = 0, border = 0, names.arg = 1:ncol(UKCityDistanceOrdination),
  xlab = "Ordination axes")

# We might assume, again, that we only really need two axes here as it is
# again a spatial distance matrix. However, two aspects make this more complex.
# First distances between objects on the Earth's surface require a third
# dimension to correctly account for the curvature of the Earth. This might not
# be a huge problem at the scale of the UK, but will still exist. Second, and
# more significantly, these are road distances and roads are not laid out in
# perfect straight lines or Great Circles. Thus we should expect the pattern
# we see, of more than two axes of notable variation. Nevertheless, we can see
# that the vast majority of variance is contained on the first axis and more
# generally that we can probably ignore the third axis onwards. We can thus
# plot the data to explore what the major axis might represent with:
plot(x = UKCityDistanceOrdination[, 1], y = UKCityDistanceOrdination[, 2],
  type = "n", asp = 1, xlab = "PC1", ylab = "PC2")
text(x = UKCityDistanceOrdination[, 1], y = UKCityDistanceOrdination[, 2],
  labels = rownames(UKCityDistanceOrdination), srt = -45)

# Note that the data are plotted with a 1:1 aspect ratio (asp = 1) so you can
# clearly tell how the variance compares between the two axes. Using your
# knowledge of UK geography what can you say about the major axis of variance
# (i.e., the x-axis or PC1)? What about the second axis (y-axis or PC2)? What
# else strikes you about the plot?
#
# So far we have looked at two data sets where we already have a pre-conceived
# notion of where our objects sit relative to each other. Now let's look at a
# case where we do not to see if we can begin to understand how a novel space
# might be constructed. Specifically, we are going to generate a "flagspace"
# using the flags of France, Germany, India, Ireland, Italy, the Netherlands,
# Japan, and the United Kingdom as examples. Below I have come up with 13
# differences between these flags that can be coded numerically. These represent
# 13 different axes of variation - exactly the kind of data set we might want
# to reduce the dimensions of to better understand:
CentralSymbol <-        c(0, 0, 1, 0, 0, 0, 1, 0)
CrossSymbol <-          c(0, 0, 0, 0, 0, 0, 0, 1)
HasBlack <-             c(0, 0, 1, 0, 0, 0, 0, 0)
HasBlue <-              c(1, 1, 0, 0, 0, 1, 0, 1)
HasGreen <-             c(0, 0, 1, 1, 1, 0, 0, 0)
HasOrange <-            c(0, 0, 1, 1, 0, 0, 0, 0)
HasRed <-               c(1, 1, 0, 0, 1, 1, 1, 1)
HasWhite <-             c(1, 0, 1, 1, 1, 1, 1, 1)
HasYellow <-            c(0, 1, 0, 0, 0, 0, 0, 0)
SaltireSymbol <-        c(0, 0, 0, 0, 0, 0, 0, 1)
ThreeHorizontalBands <- c(0, 1, 1, 0, 0, 1, 0, 0)
ThreeVerticalBands <-   c(1, 0, 0, 1, 1, 0, 0, 0)
TotalNColours <-        c(3, 3, 4, 3, 3, 3, 2, 3)

# These can be combined into a single large data matrix with:
FlagData <- t(matrix(c(CentralSymbol, CrossSymbol, HasBlack, HasBlue,
  HasGreen, HasOrange, HasRed, HasWhite, HasYellow, SaltireSymbol,
  ThreeHorizontalBands, ThreeVerticalBands, TotalNColours), ncol = 8,
  byrow = TRUE, dimnames = list(c("CentralSymbol", "CrossSymbol", "HasBlack",
  "HasBlue", "HasGreen", "HasOrange", "HasRed", "HasWhite", "HasYellow",
  "SaltireSymbol", "ThreeHorizontalBands", "ThreeVerticalBands",
  "TotalNColours"), c("France", "Germany", "India", "Ireland", "Italy",
  "Netherlands", "Japan", "United Kingdom"))))
FlagData

# First we can convert all of this data down into a single distance matrix
# with:
FlagDistances <- dist(x = FlagData)
FlagDistances

# Note that underneath this is just the Pythagorean Theorem we discussed in
# lecture. Now we can ordinate the data with:
FlagPrincipalCoordinates <- cmdscale(FlagDistances, k = 7)
FlagPrincipalCoordinates

# And again, look at the distribution of variance with:
barplot(apply(FlagPrincipalCoordinates, 2, var), ylab = "Variance", space = 0,
  border = 0, names.arg = 1:ncol(FlagPrincipalCoordinates),
  xlab = "Ordination axes")

# This time we can see the attempt to reduce the dimensionality of the data
# was less successful as we can see notable variance across a much larger
# number of axes. We can, however, force CMDS to place the variance on just
# two axes with:
FlagPrincipalCoordinates <- cmdscale(FlagDistances, k = 2)
FlagPrincipalCoordinates

# Note this isn't generally recommended as this will tend to distort our
# perception of the distances between the objects we are interested in, but we
# can still use this data to explore what the major axes of variation might
# represent. We can thus plot the data with:
plot(x = FlagPrincipalCoordinates[, 1], y = FlagPrincipalCoordinates[, 2],
  type = "n", asp = 1, xlab = "PC1", ylab = "PC2")
text(x = FlagPrincipalCoordinates[, 1], y = FlagPrincipalCoordinates[, 2],
  labels = rownames(FlagData), srt = -45)

# Unfortuantely it is not easy to understand this data by just looking at the
# names of the countries. Instead it would help to visually see each flag in
# the space. This requires writing a new (and long) function. This is in the
# StatsFunctions.r file for this course and can be viewed here:
browseURL("http://www.graemetlloyd.com/teaching/SOEE1475/20/StatsFunctions.r")

# However, all yyou need to do is get it into memory with:
source("http://www.graemetlloyd.com/teaching/SOEE1475/20/StatsFunctions.r")

# Next we will need to set the plotting size (height and width) for each flag:
FlagWidth <- diff(range(FlagPrincipalCoordinates[, 1])) * (1 / 15)
FlagHeight <- diff(range(FlagPrincipalCoordinates[, 1])) * (1 / 15) * (2 / 3)

# Now we are ready to plot the data:
plot(x = FlagPrincipalCoordinates[, 1], y = FlagPrincipalCoordinates[, 2],
  type = "n", asp = 1, xlab = "PC1", ylab = "PC2")
FlagPlotter(Country = "France", XLimits = c(FlagPrincipalCoordinates["France",
  1] - ((1 / 2) * FlagWidth), FlagPrincipalCoordinates["France", 1] + ((1 / 2)
  * FlagWidth)), YLimits = c(FlagPrincipalCoordinates["France", 2] - ((1 / 2)
  * FlagHeight), FlagPrincipalCoordinates["France", 2] + ((1 / 2) *
  FlagHeight)))
FlagPlotter(Country = "India", XLimits = c(FlagPrincipalCoordinates["India",
  1] - ((1 / 2) * FlagWidth), FlagPrincipalCoordinates["India", 1] + ((1 / 2)
  * FlagWidth)), YLimits = c(FlagPrincipalCoordinates["India", 2] - ((1 / 2)
  * FlagHeight), FlagPrincipalCoordinates["India", 2] + ((1 / 2) *
  FlagHeight)))
FlagPlotter(Country = "Ireland", XLimits = c(FlagPrincipalCoordinates["Ireland",
  1] - ((1 / 2) * FlagWidth), FlagPrincipalCoordinates["Ireland", 1] + ((1 / 2)
  * FlagWidth)), YLimits = c(FlagPrincipalCoordinates["Ireland", 2] - ((1 / 2)
  * FlagHeight), FlagPrincipalCoordinates["Ireland", 2] + ((1 / 2) *
  FlagHeight)))
FlagPlotter(Country = "Italy", XLimits = c(FlagPrincipalCoordinates["Italy",
  1] - ((1 / 2) * FlagWidth), FlagPrincipalCoordinates["Italy", 1] + ((1 / 2)
  * FlagWidth)), YLimits = c(FlagPrincipalCoordinates["Italy", 2] - ((1 / 2)
  * FlagHeight), FlagPrincipalCoordinates["Italy", 2] + ((1 / 2) *
  FlagHeight)))
FlagPlotter(Country = "Germany", XLimits = c(FlagPrincipalCoordinates["Germany",
  1] - ((1 / 2) * FlagWidth), FlagPrincipalCoordinates["Germany", 1] + ((1 / 2)
  * FlagWidth)), YLimits = c(FlagPrincipalCoordinates["Germany", 2] - ((1 / 2)
  * FlagHeight), FlagPrincipalCoordinates["Germany", 2] + ((1 / 2) *
  FlagHeight)))
FlagPlotter(Country = "Japan", XLimits = c(FlagPrincipalCoordinates["Japan",
  1] - ((1 / 2) * FlagWidth), FlagPrincipalCoordinates["Japan", 1] + ((1 / 2)
  * FlagWidth)), YLimits = c(FlagPrincipalCoordinates["Japan", 2] - ((1 / 2)
  * FlagHeight), FlagPrincipalCoordinates["Japan", 2] + ((1 / 2) *
  FlagHeight)))
FlagPlotter(Country = "Netherlands", XLimits =
  c(FlagPrincipalCoordinates["Netherlands", 1] - ((1 / 2) * FlagWidth),
  FlagPrincipalCoordinates["Netherlands", 1] + ((1 / 2) * FlagWidth)),
  YLimits = c(FlagPrincipalCoordinates["Netherlands", 2] - ((1 / 2) *
  FlagHeight), FlagPrincipalCoordinates["Netherlands", 2] + ((1 / 2) *
  FlagHeight)))
FlagPlotter(Country = "United Kingdom", XLimits =
  c(FlagPrincipalCoordinates["United Kingdom", 1] - ((1 / 2) * FlagWidth),
  FlagPrincipalCoordinates["United Kingdom", 1] + ((1 / 2) * FlagWidth)),
  YLimits = c(FlagPrincipalCoordinates["United Kingdom", 2] - ((1 / 2) *
  FlagHeight), FlagPrincipalCoordinates["United Kingdom", 2] + ((1 / 2) *
  FlagHeight)))

# You should see that there is not a major difference in the variance between
# PC1 and PC2. However, there are some clear aspects to the two axes that
# capture the difference in the appearance of the flags. Can you see what
# these are?
#
# These types of ordination "spaces" have a wide range of applications. For
# example, you could generate a volcano-space or a litho-space as a means of
# understanding both: 1) what the major axes of variance capture, and 2) which
# volcanoes, or lithologies, are most similar to each other. If you want to
# explore the non-geological examples we discussed in lecture then you can
# read about "whiskeyspace" here:
browseURL("http://whiskyanalysis.com/index.php/background/early-attempts-at-flavour-classification/")

# Or "musicspace" here (worth trying with headphones to see if you can hear
# what the major axes represent):
browseURL("http://everynoise.com/engenremap.html")

# In lecture we also discussed how some of the properties of ordination spaces
# can be co-opted to understand spherical data, i.e., the kinds of things we
# as geologists might plot on stereonets. Specifically, we will look here at
# another set of fold axis measurements (azimuths and plunges):
FoldAxisData <- matrix(c(7, 24, 348, 36, 321, 18, 302, 8, 278, 14, 302, 16,
  308, 2, 120, 4, 293, 13, 291, 14, 288, 10, 287, 10, 298, 16, 285, 10, 301,
  14, 293, 14, 35, 18, 77, 28, 288, 17, 318, 18, 322, 24, 312, 24, 312, 16,
  355, 18, 308, 24, 317, 32, 317, 17, 312, 17, 317, 14, 314, 14, 315, 18, 322,
  25), ncol = 2, byrow = TRUE, dimnames = list(c(), c("Azimuth", "Plunge")))
FoldAxisData

# Firstly we can plot this data using the "RFOC" package with:
net()
qpoint(az = FoldAxisData[, "Azimuth"], iang = 90 - FoldAxisData[, "Plunge"],
  pch = 20, cex = 2, col = "red")

# As always, Lesson #1 applies so before we conduct an analysis do you think
# the data visually correspond to: random (well spread across the whole
# stereonet), a girdle (falling along a great circle), or a point (clustered
# around a single value)?
#
# Now, to get the eigenvalues we first need to perform an ordination which we
# can do by first converting our azimuths and plunges to Cartesian (xyz)
# coordinates with:
XYZCoordinates <- sph2cart(tpr = cbind(FoldAxisData[, "Azimuth"] *
  (pi / 180), FoldAxisData[, "Plunge"] * (pi / 180), rep(1, nrow(FoldAxisData))))
XYZCoordinates

# We can check that this still makes sense by plotting the data again using
# the point size to attempt to convey the third axis (z, depth):
plot(x = XYZCoordinates[, "x"], y = XYZCoordinates[, "y"], pch = 20,
  col = "black", cex = (XYZCoordinates[, "z"] - min(XYZCoordinates[, "z"]))
  / max(XYZCoordinates[, "z"] - min(XYZCoordinates[, "z"])) + 1, asp = 1,
  xlab = "x", ylab = "y")

# Note that this may be rotated from the stereonet, but as here we only care
# about preservation of distances this is fine.
#
# Next we want to ge the first three eigenvalues (lambda 1-3) for interpreting
# the data:
FirstThreeEigenvalues <- cmdscale(dist(XYZCoordinates),
  k = nrow(FoldAxisData) - 1, eig = TRUE)$eig[1:3]
FirstThreeEigenvalues

# Finally, we want to "weight" these so they sum to one:
FirstThreeEigenvalues <- FirstThreeEigenvalues / sum(FirstThreeEigenvalues)
FirstThreeEigenvalues

# Now we can define them as each lambda number ready for analysis:
Lambda1 <- FirstThreeEigenvalues[1]
Lambda2 <- FirstThreeEigenvalues[2]
Lambda3 <- FirstThreeEigenvalues[3]
Lambda1
Lambda2
Lambda3

# Note that these eigenvalues correspond to each ordination axis in order and
# thus must always be ranked from highest to lowest. They are also rescaled
# here so that they sum to 1. Based on the possible relationships between
# lambda 1-3 from lecture do you think the data best conform to random,
# clustered, great circle, or scattered on a great circle, but with a
# preferred direction? Does this match up with your visual inspection? Next
# we will generate the ternary diagram from lecture that is
# intended to help us make this decision. First we need to perform the
# necessary point, girdle, and random calculations from lecture:
Point <- (Lambda1 * Lambda2) / nrow(FoldAxisData)
Girdle <- (3 * Lambda2 * Lambda3) / nrow(FoldAxisData)
Random <- (3 * Lambda3) / nrow(FoldAxisData)

# Then we can plot the data using the functions from the Ternary package with:
TernaryPlot(atip = "Random", btip = "Point", ctip = "Girdle",
  alab = expression(lambda[1] == lambda[2]),
  blab = expression(lambda[2] == lambda[3]),
  clab = expression(lambda[3] == '0'), point = "down", axis.labels = FALSE)
AddToTernary(PlottingFunction = points,
  coordinates = list(c(Random, Point, Girdle)), col = "red", pch = 20,
  cex = 2)

# Where does the point fall? Is this what you expected?
#
# Next we can try the other plot from lecture intended to help both establish
# whether the data conform to a girdle or cluster (shape parameter, K) and
# the strength parameter (C), which separately estimates the degree of
# randomness to the data (a low C being more random, a high C more non-random).
# Again, we must begin by calculating the log ratios used as the x- and y-axes:
X <- log(Lambda2 / Lambda3)
Y <- log(Lambda1 / Lambda2)

# Next we can plot the data (with additional C and K axes plotted into the same
# bivariate space:
par(mar = c(5, 5, 6, 6))
plot(x = X, y = Y, type = "n", xlim = c(0, 5), ylim = c(0, 5), bty = "l",
  xlab = expression(ln(lambda[2] / lambda[3])),
  ylab = expression(ln(lambda[1] / lambda[2])))
for(i in 1:4 * 2) lines(x = c(0, i), y = c(i, 0), col = "blue", lty = 2,
  lwd = 2)
for(i in c(0.2, 0.5, 1, 2, 5)) lines(x = c(0, 5), y = c(0, i * 5),
  col = "blue", lty = 2, lwd = 2)
for(i in 1:4 * 2) text(x = i / 2, y = i / 2, labels = paste("C =", i),
  col = "blue", cex = 1.5)
for(i in c(0.2, 0.5, 1, 2, 5)) text(x = ifelse(i * 5 > 5, 5, i * 5),
  y = ifelse((1 / i) * 5 > 5, 5, (1 / i) * 5), labels = paste("K =", i),
  col = "blue", cex = 1.5, adj = c(1, 0))
mtext(text = "Clusters", side = 3, col = "blue", cex = 1.5)
mtext(text = "Girdles", side = 4, col = "blue", cex = 1.5)
points(x = X, y = Y, pch = 20, col = "red", cex = 2)
par(mar = c(5, 4, 4, 2) + 0.1)

# What does this suggest about the data? Does this agree with the previous
# analyses and your own estimate based on the visualisation and eigenvalues?
# Overall, what conclusion would you come to about the data? Are the fold axes
# all approximately parallel (point distribution), arranged in a plane (girdle
# distribution), or random (no pattern)?

################################################################################
#                                                                              #
#                                     MCQ                                      #
#                                                                              #
# You are now ready to attempt this week's MCQ which is a series of ten        #
# multiple choice questions (MCQs) on last week's lecture and this practical.  #
# This is on Minerva: SOEE1475 > Statistics Resources > MCQ.                   #
#                                                                              #
################################################################################
