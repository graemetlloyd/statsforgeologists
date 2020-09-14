################################################################################
#                                                                              #
#                       PRACTICAL IV - BIVARIATE STATISTICS                    #
#                                                                              #
################################################################################

################################################################################
#                                                                              #
#                               TODAY's AIMS                                   #
#                                                                              #
# - LEARN TO DIAGNOSE APPROPRIATE AND INAPPROPRIATE APPROACHES TO BIVARIATE    #
#   DATA ANALYSIS USING ANSCOMBE"S QUARTET                                     #
# - APPLY BIVARIATE ANALYSIS TO YOUR OWN AMMONITE QUADRAT                      #
# - LEARN HOW TO USE REGRESSION AND CORRELATION TO MAKE DETERMINATIONS ABOUT   #
#   THE RELATIONSHIP BETWEEN TWO VARIABLES                                     #
#                                                                              #
################################################################################

# Before we start begin by setting your working directory as we did in
# Practicals 1-3. This is (still) a useful habit to get into!

# We will start today's practical by looking at the data from Anscombe's quartet
# that we encountered in lecture. Anscombe's paper was essentially making the
# same point as our two Lesson's: 1) Always plot your data, and 2) Don't rely
# on summary statistics (mean, sd, r etc.) to do the job of thinking about the
# data for you. First we will enter the data into R with separate x and y
# variables for each quartet (I-IV):
x1 <- c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
y1 <- c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 5.68)
x2 <- c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
y2 <- c(9.14, 8.14, 8.74, 8.77, 9.26, 8.1, 6.13, 3.1, 9.13, 7.26, 4.74)
x3 <- c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
y3 <- c(7.46, 6.77, 12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.42, 5.73)
x4 <- c(8, 8, 8, 8, 8, 8, 8, 19, 8, 8, 8)
y4 <- c(6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 12.5, 5.56, 7.91, 6.89)

# We can combine these into a matrix with:
AnscombeQuartet <- cbind(x1, y1, x2, y2, x3, y3, x4, y4)

# Here we are using the cbind function which works like c, except it combines
# vectors of numbers together as columns in a matrix. You can also do this as
# rows with rbind. We should find our data now form a single table:
AnscombeQuartet

# NB: We were only able to combine the data in this way as each quartet is made
# up of exactly 11 values. If they were different lengths a list would be more
# appropriate (see the geologist, basketball player, and jockey heights example
# in the last practical). For now just visually confirm that each pair of x and
# y is unique, i.e., the data are different. (You should see x1 and x2 are
# identical, but as y1 and y2 are not the collective bivariate relationship
# should be different.) Before we plot our data we can perform some summary
# statistics on the data, first we will look at the mean of each set of
# x-values:
apply(AnscombeQuartet, 2, mean)[c("x1", "x2", "x3", "x4")]

# We should see they are all exactly 9. What about the y-values:
apply(AnscombeQuartet, 2, mean)[c("y1", "y2", "y3", "y4")]

# This time they are not exactly equal, but to two decimal places they are all
# 7.50. Certainly these are not large differences. Now we can do the same thing
# with the standard deviation of x-values:
apply(AnscombeQuartet, 2, sd)[c("x1", "x2", "x3", "x4")]

# You should see again they are identical (3.316625), even at six decimal
# places. What about our y-values:
apply(AnscombeQuartet, 2, sd)[c("y1", "y2", "y3", "y4")]

# Again they are not perfectly the same, but at two decimal places they are all
# 2.03. Now we can consider the bivariate relationships by first performing
# least squares linear regression (as we did in lecture with our ammonite
# sample). First our slopes::
lm(AnscombeQuartet[, "y1"] ~ AnscombeQuartet[, "x1"])$coefficients[2]
lm(AnscombeQuartet[, "y2"] ~ AnscombeQuartet[, "x2"])$coefficients[2]
lm(AnscombeQuartet[, "y3"] ~ AnscombeQuartet[, "x3"])$coefficients[2]
lm(AnscombeQuartet[, "y4"] ~ AnscombeQuartet[, "x4"])$coefficients[2]

# Note that they are not exactly identical, but at one decimal place they are
# all 0.5 (i.e., a positive relationship between x and y). And now our
# intercepts:
lm(AnscombeQuartet[, "y1"] ~ AnscombeQuartet[, "x1"])$coefficients[1]
lm(AnscombeQuartet[, "y2"] ~ AnscombeQuartet[, "x2"])$coefficients[1]
lm(AnscombeQuartet[, "y3"] ~ AnscombeQuartet[, "x3"])$coefficients[1]
lm(AnscombeQuartet[, "y4"] ~ AnscombeQuartet[, "x4"])$coefficients[1]

# Again they are not identical, but they are very close and at two decimal
# places they are indistinguishable. Next up we can perform a correlation, and
# get estimates for our Pearson's correlation coefficient, r:
cor.test(x = AnscombeQuartet[, "x1"], y = AnscombeQuartet[, "y1"])$estimate
cor.test(x = AnscombeQuartet[, "x2"], y = AnscombeQuartet[, "y2"])$estimate
cor.test(x = AnscombeQuartet[, "x3"], y = AnscombeQuartet[, "y3"])$estimate
cor.test(x = AnscombeQuartet[, "x4"], y = AnscombeQuartet[, "y4"])$estimate

# Again, we can see non-identical, but very similar values. This time
# indistinguishable at two decimal places. Note they also show a strong
# positive relationship between x and y. We can also get the probability that
# the true value for r is zero:
cor.test(x = AnscombeQuartet[, "x1"], y = AnscombeQuartet[, "y1"])$p.value
cor.test(x = AnscombeQuartet[, "x2"], y = AnscombeQuartet[, "y2"])$p.value
cor.test(x = AnscombeQuartet[, "x3"], y = AnscombeQuartet[, "y3"])$p.value
cor.test(x = AnscombeQuartet[, "x4"], y = AnscombeQuartet[, "y4"])$p.value

# These all show a low probability that the true r is zero and hence without
# further checks we might assume that x and y are always strongly positively
# correlated, here with p values being identical down to four decimal places
# (0.0022). But the whole point of Anscombe's paper is that we should not rely
# on these summary values alone, but initially plot the data as a check on our
# assumptions. We can plot all four quartets with:
par(mfrow = c(2, 2))
plot(x = x1, y = y1, xlim = c(3, 19), xlab = "x", ylab = "y", pch = 20, cex = 2,
  main = "Quartet I", ylim = c(0, 13))
lines(x = c(0, 25), y = c((0 * lm(AnscombeQuartet[, "y1"] ~
  AnscombeQuartet[, "x1"])$coefficients[2]) + lm(AnscombeQuartet[, "y1"] ~
  AnscombeQuartet[, "x1"])$coefficients[1], (25 * lm(AnscombeQuartet[, "y1"] ~
  AnscombeQuartet[, "x1"])$coefficients[2]) + lm(AnscombeQuartet[, "y1"] ~
  AnscombeQuartet[, "x1"])$coefficients[1]))
plot(x = x2, y = y2, xlim = c(3, 19), xlab = "x", ylab = "y", pch = 20, cex = 2,
  main = "Quartet II", ylim = c(0, 13))
lines(x = c(0, 25), y = c((0 * lm(AnscombeQuartet[, "y2"] ~
  AnscombeQuartet[, "x2"])$coefficients[2]) + lm(AnscombeQuartet[, "y2"] ~
  AnscombeQuartet[, "x2"])$coefficients[1], (25 * lm(AnscombeQuartet[, "y2"] ~
  AnscombeQuartet[, "x2"])$coefficients[2]) + lm(AnscombeQuartet[, "y2"] ~
  AnscombeQuartet[, "x2"])$coefficients[1]))
plot(x = x3, y = y3, xlim = c(3, 19), xlab = "x", ylab = "y", pch = 20, cex = 2,
  main = "Quartet III", ylim = c(0, 13))
lines(x = c(0, 25), y = c((0 * lm(AnscombeQuartet[, "y3"] ~
  AnscombeQuartet[, "x3"])$coefficients[2]) + lm(AnscombeQuartet[, "y3"] ~
  AnscombeQuartet[, "x3"])$coefficients[1], (25 * lm(AnscombeQuartet[, "y3"] ~
  AnscombeQuartet[, "x3"])$coefficients[2]) + lm(AnscombeQuartet[, "y3"] ~
  AnscombeQuartet[, "x3"])$coefficients[1]))
plot(x = x4, y = y4, xlim = c(3, 19), xlab = "x", ylab = "y", pch = 20, cex = 2,
  main = "Quartet IV", ylim = c(0, 13))
lines(x = c(0, 25), y = c((0 * lm(AnscombeQuartet[, "y4"] ~
  AnscombeQuartet[, "x4"])$coefficients[2]) + lm(AnscombeQuartet[, "y4"] ~
  AnscombeQuartet[, "x4"])$coefficients[1], (25 * lm(AnscombeQuartet[, "y4"] ~
  AnscombeQuartet[, "x4"])$coefficients[2]) + lm(AnscombeQuartet[, "y4"] ~
  AnscombeQuartet[, "x4"])$coefficients[1]))

# You should now be able to see that each data set is telling us a very
# different story, and that none of our summary statistics were able to reveal
# this - only plotting the data and using our eyes and brains does. We can go
# through the data sets one-by-one to try and make determinations of how they
# should each be dealt with. Let's start with the first one:
plot(x = x1, y = y1, xlim = c(3, 19), xlab = "x", ylab = "y", pch = 20,
  cex = 2, main = "Quartet I")

# This is what a lot of real data will look like and actually meets most of the
# assumptions we are making when we perform linear regression and correlation.
# You should see a clear relationship of a rise in y as x increases, but this
# is not a pure relationship (i.e., the points do not all fall on a perfectly
# straight line). This can indicate some degree of error in the measurements
# or that there are other contributing factors that explain changes in y than
# just x, or both. In this case we don't have to make any real changes and can
# treat our regression and correlation values as meaningful. I.e., there is a
# strong positive linear relationship between x and y. (We might worry that our
# data, x and y, are not normally distributed, but as we saw with univariate
# data this can be hard to determine when our sample size is small (here it is
# just 11). Tests that assume data are normally distributed are termed
# PARAMETRIC, and many will have some NON-PARAMETRIC alternative that "relaxes"
# this assumption. If we are really concerned about this here, we could use a
# non-parametric alternative to Pearson's r, termed Spearman's rho. It can be
# accessed through the same cor.test function (by adding a method option) and
# instead treats the data as ranked (i.e., like Moh's hardness scale the test
# only cares about the relative order of the data). Rho falls on the same kind
# of -1 to 1 scale as r and so values can be interpreted the same way:
cor.test(x = AnscombeQuartet[, "x1"], y = AnscombeQuartet[, "y1"],
  method = "spearman")$estimate
cor.test(x = AnscombeQuartet[, "x1"], y = AnscombeQuartet[, "y1"],
  method = "spearman")$p.value

# We still see a high (>0.8) value for our correlation coefficient (this time
# rho instead of r), again indicating a strong positive relationship. We also
# still see a low probability that the true value for rho is zero. This
# combined with our visual inspection that the relationship between x and y is
# linear should mean we are happy with our initial conclusion that x has a
# strong positive linear effect on y. Now let's look at quartet II:
plot(x = x2, y = y2, xlim = c(3, 19), xlab = "x", ylab = "y", pch = 20, cex = 2,
  main = "Quartet II")

# This time we should see a clear non-linear relationship between x and y. This
# means we should immediately abandon the idea we can perform either a linear
# regression or a correlation on this data as the values we get will be
# misleading. Furthermore, there is no simple replacement for correlation when
# the relationship is non-linear. We can, however, look at non-linear forms of
# regression. Specifically, we could fit a second-order polynomial to the data.
# In other words, we will make estimates for two "slopes" (a and b)
# and an intercept with the equation y = ax^2 + bx + c. We can do this by
# adding an extra option to the lm (linear model) function called "poly":
lm(AnscombeQuartet[, "y2"] ~ poly(AnscombeQuartet[, "x2"], 2))$coefficients

# These results reflect estimated values for c, b, and a, respectively. We can
# also use another function called predict to predict our observed values for y
# based on our sampled value of x with:
predict(lm(AnscombeQuartet[, "y2"] ~ poly(AnscombeQuartet[, "x2"], 2)))

# Compare these with the actual values:
AnscombeQuartet[, "y2"]

# You should see they are pretty close; we can make a different bivariate plot to
# show this:
plot(x = predict(lm(AnscombeQuartet[, "y2"] ~ poly(AnscombeQuartet[, "x2"], 2))),
  y = AnscombeQuartet[, "y2"], pch = 20, xlab = "Predicted y",
  ylab = "Observed y", cex = 2)
lines(x = c(0, 25), y = c(0, 25), lty = 2, lwd = 2, col = "Grey")

# You should see they all fall nicely on the grey dashed line which represents
# the 1:1 relationship - i.e., the line of perfect agreement. In other words
# our new curvilinear regression is doing a much better job of summarising the
# relationship between x and y than our linear regression did:
plot(x = predict(lm(AnscombeQuartet[, "y2"] ~ AnscombeQuartet[, "x2"])),
  y = AnscombeQuartet[, "y2"], pch = 20, xlab = "Predicted y",
  ylab = "Observed y", cex = 2, xlim =c(1, 11), ylim = c(0, 11))
lines(x = c(0, 25), y = c(0, 25), lty = 2, lwd = 2, col = "Grey")

# We can further visually confirm this by getting the coefficients (i.e., the
# values of a, b, and c) for our polynomial with:
PolyCoefficients <- lm(AnscombeQuartet[, "y2"] ~ poly(AnscombeQuartet[, "x2"],
  2, raw = TRUE))$coefficients

# Then set up a range of x values (here from 0 to 25), using a large number
# (1000) so that plotting them will look like a smooth curve:
xs <- seq(from = 0, to = 25, length.out = 1000)

# Now we can predict y-values for every value of x from the above (i.e., we are
# going to both interpolate and extrapolate):
y_pred <- ((xs^2) * PolyCoefficients[3]) + (xs * PolyCoefficients[2]) +
  PolyCoefficients[1]

# And finally we can plot the data and then the regression:
plot(x = x2, y = y2, xlim = c(3, 19), xlab = "x", ylab = "y", pch = 20, cex = 2,
  main = "Quartet II")
points(xs, y_pred, type = "l")

# You should see both a curvilinear line and a good fit between that line and
# the data. This is a more realistic model for the relationship between x and
# y, and shows us that there is not a clear linear response of y increasing
# (or decreasing) as we increase x. Instead it depends where we are along x as
# to how y will respond. This is thus more complex to interpret than our linear
# data, but for now you only need to know: 1) Not to fit a linear regression to
# data that are clearly non-linear and 2) More complex regressions are
# possible in R, for example using polynomials as we did here. Now let's
# move on to Quartet III:
plot(x = x3, y = y3, xlim = c(3, 19), xlab = "x", ylab = "y", pch = 20, cex = 2,
  main = "Quartet III")

# This is a clear outlier problem, with one apparently "rogue" value dragging
# the linear regression away from what would otherwise be a line that
# intersected every data point. As we have already encountered in Lecture if
# this were our own data we would want to double check the rogue value: was it
# measured badly? Was there a typo? Can we repeat the measurement to see if we
# get the same value again? For now, we will assume this is a bad value and
# remove it from the data. As it is the third value we can do this by subsetting
# our variable with a negative:
x3[-3]
y3[-3]

# This gives us the data without a specific value, i.e., the inverse of how we
# normally subset the data by asking for the values we do want to be returned.
# (Remember that this kind of trick can really help us in calculating
# probabilities too!) We can plot this to confirm the outlier is not included:
plot(x = x3[-3], y = y3[-3], xlim = c(3, 19), xlab = "x", ylab = "y", pch = 20,
  cex = 2, main = "Quartet III")

# You should now just see a "gap" where the outlier previously fitted in the
# sequence of values. We can now redo our linear regression without this value
# too:
lm(y3[-3] ~ x3[-3])$coefficients

# These are our intercept and slope. Remember the values we got with the
# outlier included:
lm(y3 ~ x3)$coefficients

# I.e., both our slope and intercept have changed considerably by removing the
# outlier. We can visually confirm this by plotting the two regression lines:
plot(x = x3[-3], y = y3[-3], xlim = c(3, 19), xlab = "x", ylab = "y", pch = 20,
  cex = 2, main = "Quartet III")
lines(x = c(0, 22), y = c(lm(y3 ~ x3)$coefficients[1],
  (22 * lm(y3 ~ x3)$coefficients[2]) + lm(y3 ~ x3)$coefficients[1]), lwd = 2)
lines(x = c(0, 22), y = c(lm(y3[-3] ~ x3[-3])$coefficients[1],
  (22 * lm(y3[-3] ~ x3[-3])$coefficients[2]) +
  lm(y3[-3] ~ x3[-3])$coefficients[1]), col = "Red", lwd = 2)

# Here our previous regression line is in black and our new one is in red. You
# should see that by excluding the outlier we are able to produce a line that
# is a much better fit to the remaining data and hence likely does a much
# better job of describing the relationship between x and y. Imagine we were
# fitting this line to make a prediction of what y would be when x was 100.
# Which line would do the better job? We can also revisit our correlation, as
# we are happy that x and y share a linear relationship. Previously we found a
# estimate for r of:
cor.test(x = x3, y = y3)$estimate

# And a probability that the true value of r was zero of:
cor.test(x = x3, y = y3)$p.value

# What happens when we remove our outlier?:
cor.test(x = x3[-3], y = y3[-3])$estimate
cor.test(x = x3[-3], y = y3[-3])$p.value

# You should see we now have almost a perfect linear response of y to x and an
# extremely low probability that r is actually zero. In other words, by
# including the outlier we were underestimating r considerably. Finally, we
# will turn to Quartet IV:
plot(x = x4, y = y4, xlim = c(3, 19), xlab = "x", ylab = "y", pch = 20, cex = 2,
  main = "Quartet IV")

# This appears to be another outlier problem as everything hinges on that one
# point at top right. Let's remind ourselves of the linear regression:
lm(y4 ~ x4)$coefficients

# And without the outlier (the 8th value):
lm(y4[-8] ~ x4[-8])$coefficients

# You should see that although the regression returns an intercept it has
# actually failed to work as it cannot produce a slope. The reason for this is
# that all the remaining x-values are now identical:
x4[-8]

# In other words the regression would be a vertical line (slope of infinity or
# minus infinity) and in this case there wouldn't really be an intercept as the
# line would be parallel to the y-axis. I.e., visually, it would look something
# like this:
plot(x = x4[-8], y = y4[-8], xlim = c(3, 19), xlab = "x", ylab = "y", pch = 20,
  cex = 2, main = "Quartet IV")
lines(x = c(8, 8), y = c(0, 100), lwd = 2)

# As discussed in Lecture this would indicate a failure of study design. I.e.,
# x was not varied enough to properly gauge the response of y. Alternatively,
# x and y might have been mislabelled. If that were the case the plot should
# really look like this:
plot(x = y4[-8], y = x4[-8], ylim = c(3, 19), xlab = "x", ylab = "y", pch = 20,
  cex = 2, main = "Quartet IV")
lines(x = c(0, 100), y = c(8, 8), lwd = 2)

# This time we would interpret the linear regression as having a slope of
# zero, i.e., x has no effect on y. We can also consider what happens to our
# correlation when the outlier is removed. Remember before that we got:
cor.test(x = x4, y = y4)$estimate
cor.test(x = x4, y = y4)$p.value

# Without our outlier we get:
cor.test(x = x4[-8], y = y4[-8])$estimate
cor.test(x = x4[-8], y = y4[-8])$p.value

# In other words the correlation fails as the data make no sense (relationship
# is either perfectly negatively OR positively correlated as slope is infinity
# or negative infinity).
#
# Collectively these quartets reiterate the two core lessons of this course: 1)
# Always plot your data, and 2) Summary statistics are not a substitute for
# thinking about your data. More specifically we have learned:
#   - Do not apply linear regression when the response of y to x does not
#     follow a straight line
#   - Do not apply correlation if this is the case as the answer is not
#     meaningful
#   - However, it is possible to fit more complex regressions in R (e.g., using
#     poly)
#   - If your conclusions hinge on a single data point they are not safe and
#     more generally if removing a single data point dramatically changes the
#     slope, intercept, r, or p-value (of r = 0) your conclusions are not safe
#
# Now you can apply these lessons to your own ammonite quadrat data. First you
# will want to get your diameters you measured last week (i.e., modify the
# line below to replace each ammonite's number with it's diameter in mm). If
# you saved your script from last week then you can just copy and paste that
# line over the top of this one:
AmmoniteDiameters <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
  17, 18, 19, 20)

# Next you will count the number of chambers for each ammonite, again, these
# must be done in order for the regression and correlation to work. You will
# likely find that the printed version of your quadrat is too small for you to
# count the central chambers so it is advised you start by downloading the PDF
# version of your quadrat (i.e., modify the last part of this line to point to
# your unique quadrat as you did in the previous practical):
browseURL("https://github.com/graemetlloyd/statsforgeologists/raw/master/Ammonite_quadrats/Lower_Bed/X4.pdf")

# As with the diameters you should fill this data out on your sheet before
# entering it here (it is always good to have redundancy in this way). You
# should find that you can zoom in and out allowing you to better count the
# smaller central chambers. Once you are done, replace each ammonite number
# with the chamber count in the line below:
AmmoniteChambers <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
  17, 18, 19, 20)

# Note that for linear regression we are assuming each of our two variables are
# normally distributed. We have already thought about this for our diameter data.
# We should ask ourselves the same question about our new chamber count data.
# However, we already know that it cannot be properly normally distributed as the
# normal distribution is a continuous distribution but our chambers are count,
# and hence discrete, data. In an ideal world we would not apply normal
# distribution assumptions to such data, but in the real world this problem occurs
# all the time so for now we will just continue as though our data are normally
# distributed. (NB: this assumption is related to the mean being a meaningful
# "middle" of the data, which is used when we infer our slope value.) We can
# begin by restating, from lecture, that we are considering the number of
# chambers to be the explanatory variable (x) and the diameter the response
# variable (y). We can visualise this by plotting N Chambers on our x-, and
# Diameter on our y-axis:
plot(x = AmmoniteChambers, y = AmmoniteDiameters, pch = 20, cex = 3,
  xlab = "N Chambers", ylab = "Diameter (mm)",
  main = "Bivariate plot of ammonite chambers against ammonite diameter")

# Does the data look like it shows a linear response of y to x? If so, what
# kind of slope (relationship) would it show out of positive (y increases as
# x increases), negative (y decreases as x increases), or none (y is unchanged
# as x increases)? Is this what we expected from Lecture?
#
# Let's make the next step and fit a regression line using the lm function:
lm(AmmoniteDiameters ~ AmmoniteChambers)$coefficients

# For this function we give it the response variable followed by a tilda (~)
# and then the explanatory variable. I.e., here AmmoniteDiameters ~
# AmmoniteChambers. We can also plot the data with the line included:
plot(x = AmmoniteChambers, y = AmmoniteDiameters, pch = 20, cex = 3,
  xlab = "N Chambers", ylab = "Diameter (mm)",
  main = "Bivariate plot of ammonite chambers against ammonite diameter")
abline(lm(AmmoniteDiameters ~ AmmoniteChambers), lwd = 2)

# For the (biased) sample of five ammonites in Lecture we came up with a slope
# of c. 20 and an intercept of c. -345. How close are your values to these?
# Check with a neighbour what values they got. Note that like other values we
# get from a sample our slope and intercept are just estimates of the "true"
# value we would get if we were able to sample all the data (i.e., all
# ammonites). We can thus express our uncertainty around our own sample
# using the confint ("CONFidence INTerval") function:
confint(lm(AmmoniteDiameters ~ AmmoniteChambers), level = 0.99)

# Here we get the range of values for our intercept and slope at a specified
# confidence level (here 99%). Just as with other confidence intervals (e.g.,
# as with the one-sample t-test we encountered last week) because we assume a
# normal distribution if we want 100% confidence we will get the negative
# infinity to positive infinity range:
confint(lm(AmmoniteDiameters ~ AmmoniteChambers), level = 1)

# Thus we must balance our desire for a high-level of confidence with our
# desire to give a narrow range of values. E.g., at 50% confidence you should
# see a narrower range than at 99%:
confint(lm(AmmoniteDiameters ~ AmmoniteChambers), level = 0.5)

# Once again, we can also visualise this data, but first we need to define a
# new function:
ConfidencePlot <- function(x, y, level, from, to, xlab, ylab) {
  Model <- lm(y ~ x)
  NewX <- seq(from = from, to = to, length.out = 1000)
  PredictedValues <- predict(Model, newdata = data.frame(x = NewX),
  interval = "confidence", level = level)
  plot(y ~ x, type = "n", xlab = xlab, ylab = ylab)
  polygon(c(rev(NewX), NewX), c(rev(PredictedValues[, 3]),
  PredictedValues[, 2]), col = "Grey", border = NA)
  abline(Model, lwd = 2)
  points(x = x, y = y, pch = 20, cex = 3)
}

# First try plotting at 99% confidence:
ConfidencePlot(x = AmmoniteChambers, y = AmmoniteDiameters, level = 0.99,
  from = 15, to = 35, xlab = "N Chambers", ylab = "Diameter (mm)")

# You should see the interaction of slope and intercept uncertainties lead to
# curved edges to the confidence interval (shown as a grey polygon around the
# regression line), which is narrowest at the middle (near the mean). If we
# go for a 50% confidence interval you should see the polygon shrinks:
ConfidencePlot(x = AmmoniteChambers, y = AmmoniteDiameters, level = 0.50,
  from = 15, to = 35, xlab = "N Chambers", ylab = "Diameter (mm)")

# We will discuss confidence intervals in more detail later in the course. For
# now we will move on to another useful tool in regression called RESIDUALS.
# These are the results we get by subtracting our regression line from the
# data. You can also think of this as rotating the regression line until it is
# horizontal, pulling the data points with it. The purpose of this approach is
# to confirm the appropriateness of the regression in the first place. I.e.,
# any remaining structure in the data ought to be indistinguishable from
# random noise. Before you apply this to your own data let's look at the
# residuals of linear regression applied to Anscombe's quartet:
par(mfrow = c(2, 2))
plot(x = x1, y = lm(y1 ~ x1)$residuals, cex = 3, pch = 20, xlab = "x",
  ylab = "Residuals after regressing y on x", main = "Quartet I residuals")
lines(x = c(0, 20), y = c(0, 0), lwd = 2)
plot(x = x2, y = lm(y2 ~ x2)$residuals, cex = 3, pch = 20, xlab = "x",
  ylab = "Residuals after regressing y on x", main = "Quartet II residuals")
lines(x = c(0, 20), y = c(0, 0), lwd = 2)
plot(x = x3, y = lm(y3 ~ x3)$residuals, cex = 3, pch = 20, xlab = "x",
  ylab = "Residuals after regressing y on x", main = "Quartet III residuals")
lines(x = c(0, 20), y = c(0, 0), lwd = 2)
plot(x = x4, y = lm(y4 ~ x4)$residuals, cex = 3, pch = 20, xlab = "x",
  ylab = "Residuals after regressing y on x", main = "Quartet IV residuals")
lines(x = c(0, 20), y = c(0, 0), lwd = 2)

# The horizontal line here thus represents our linear regression. You should
# see that only Quartet I really fits the idea of random (noisy) structure
# left in the residuals. Quartet II shows a curvilinear relationship, Quartet
# III has a linear relationship that has not been captured due to the outlier
# and Quartet IV still shows the same non-variance of x-values from before. We
# can use this as a backdrop for interpreting the residuals of the ammonite
# data:
plot(x = AmmoniteChambers, y = lm(AmmoniteDiameters ~
  AmmoniteChambers)$residuals, cex = 3, pch = 20)
lines(x = c(15, 35), y = c(0, 0))

# Can you see any clear linear or curvilinear patterns? Or does this look more
# like Quartet I's residuals (i.e., random noise)?
#
# You might be happy that if you knew nothing else about your data that a
# linear model makes sense. However, we discussed in Lecture multiple reasons
# that we might not consider a linear model to be a good explanation.
# Specifically:
#
# 1. Looking at chamber sizes from the ammonites it should be clear that they
#    differ in size and so adding each chamber should not lead to a *fixed*
#    increase in diameter.
# 2. The intercept is a large negative value yet at zero chambers our ammonite
#    should logically be zero too.
# 3. There is *some* suggestion that the data visually exhibit a curvilinear
#    relationship.
#
# Previously we fitted a polynomial model to our data for Quartet II so we can
# begin by trying that here and plotting the results:
plot(x = AmmoniteChambers, y = AmmoniteDiameters, cex = 3, pch = 20, xlab =
  "N Chambers", ylab = "Diameter (mm)", main =
  "Bivariate plot of ammonite chambers against ammonite diameter")
lines(x = seq(from = 0, to = 40, length.out = 1000), y =
  predict(lm(AmmoniteDiameters ~ poly(AmmoniteChambers, 2, raw = TRUE)), newdata
  = data.frame(AmmoniteChambers = seq(from = 0, to = 40, length.out = 1000))))

# You will likely see a curve with a slope that gets steeper as number of
# chambers increases. However, it might not look very different from a straight
# line. Let's repeat the plot again, but with our x- and y-axes expanded
# outwards (using the xlim and ylim options) to better see what the model is
# suggesting about the relationship between x and y:
plot(x = AmmoniteChambers, y = AmmoniteDiameters, cex = 3, pch = 20, xlim =
  c(0, 30), ylim = c(0, 300), xlab = "N Chambers", ylab = "Diameter (mm)",
  main = "Bivariate plot of ammonite chambers against ammonite diameter")
lines(x = seq(from = 0, to = 40, length.out = 1000), y =
  predict(lm(AmmoniteDiameters ~ poly(AmmoniteChambers, 2, raw = TRUE)), newdata
  = data.frame(AmmoniteChambers = seq(from = 0, to = 40, length.out = 1000))))

# We should see that this model doesn't really make any sense as if we
# extrapolate backwards to younger (fewer chambered) ammonites then at a certain
# point the model flips and predicts larger juveniles. So this is also a bad
# model. Just for completeness we can also look at our linear model this way:
plot(x = AmmoniteChambers, y = AmmoniteDiameters, cex = 3, pch = 20, xlim =
  c(0, 30), ylim = c(0, 300), xlab = "N Chambers", ylab = "Diameter (mm)",
  main = "Bivariate plot of ammonite chambers against ammonite diameter")
lines(x = seq(from = 0, to = 40, length.out = 1000), y =
  predict(lm(AmmoniteDiameters ~ AmmoniteChambers), newdata
  = data.frame(AmmoniteChambers = seq(from = 0, to = 40, length.out = 1000))))

# Here we can see as we extrapolate backwards that very quickly we are predicting
# negative ammonite diameters in younger individuals. What we really want here is
# an exponential curve that describes the phenomenon we observe - i.e., a
# MONOTONIC rise in diameter as chamber number increases, but one where the amount
# that diameter increases also increases with chamber number. At the same time we
# want to make sure our intercept is (or is close to) zero as this makes logical
# sense in describing the age-size relationship for any organism.
#
# This latter goal is known as REGRESSION THROUGH THE ORIGIN as it forces the line
# through the 0,0 point of our plot. We can do this with lm by adding a "0 +" to
# the formula and plot the results with:
plot(x = AmmoniteChambers, y = AmmoniteDiameters, cex = 3, pch = 20, xlim =
  c(0, 30), ylim = c(0, 300), xlab = "N Chambers", ylab = "Diameter (mm)",
  main = "Bivariate plot of ammonite chambers against ammonite diameter")
lines(x = seq(from = 0, to = 40, length.out = 1000), y =
  predict(lm(AmmoniteDiameters ~ 0 + AmmoniteChambers), newdata
  = data.frame(AmmoniteChambers = seq(from = 0, to = 40, length.out = 1000))))

# We can see that this does pass through 0,0, but now the model is not a good fit
# to the data. We can try the same thing with the polynomial:
plot(x = AmmoniteChambers, y = AmmoniteDiameters, cex = 3, pch = 20, xlim =
  c(0, 30), ylim = c(0, 300), xlab = "N Chambers", ylab = "Diameter (mm)",
  main = "Bivariate plot of ammonite chambers against ammonite diameter")
lines(x = seq(from = 0, to = 40, length.out = 1000), y =
  predict(lm(AmmoniteDiameters ~ 0 + poly(AmmoniteChambers, 2, raw = TRUE)),
  newdata = data.frame(AmmoniteChambers = seq(from = 0, to = 40,
  length.out = 1000))))

# Again, this doesn't quite get us where we want as we still predict some
# negative diameter younger individuals. Really here we need a different model,
# one with exponential (exp()) built in:
plot(x = AmmoniteChambers, y = AmmoniteDiameters, cex = 3, pch = 20, xlim =
  c(0, 30), ylim = c(0, 300), xlab = "N Chambers", ylab = "Diameter (mm)",
  main = "Bivariate plot of ammonite chambers against ammonite diameter")
lines(x = seq(from = 0, to = 40, length.out = 1000), y =
  predict(lm(AmmoniteDiameters ~ exp(0.12 * AmmoniteChambers) - 1), newdata =
  data.frame(AmmoniteChambers = seq(from = 0, to = 40, length.out = 1000))))

# You should see this model is still not perfect but gets us closer than
# anything else. We can also use this model to get predicted ammonite diameters
# for one to 50 chambers with:
predict(lm(AmmoniteDiameters ~ exp(0.12 * AmmoniteChambers) - 1), newdata =
  data.frame(AmmoniteChambers = 1:50))

# Compare some of these with your own measured values. How well does the model
# do?
#
# Even though we know the true relationship is likely curved in this way (and
# not a straight line) we do also suspect it is monotonic, i.e., as we
# increase x, y will consistently increase too (and not decrease). Thus
# although we should not use a Pearson Correlation coefficient here we can
# (as above) try the Spearman rank correlation instead:
cor.test(x = AmmoniteChambers, y = AmmoniteDiameters, method = "spearman")

# What you will most likely see here is that R gives us a warning that ties
# are hampering the calculation. This is because Spearman's rank assumes the
# data can be ranked perfectly without ties (e.g., no joint second largest
# value. To search for ties we can use the duplicated() function. First on
# the diameters:
duplicated(AmmoniteDiameters)

# Then on the chambers:
duplicated(AmmoniteChambers)

# This is a logical (returning TRUE if a duplicate is found and FALSE if
# not). What you will most likely see is very few if any duplicates in your
# diameters but mutliple in your chambers. Again, this is because our chambers
# are count (discrete) data and hence ties are more likely to happen.
#
# Unfortunately this means we cannot exactly compute a p-value for our data.
# However, we can use the exact = FALSE option to get an approximate one.
# (Remember that this is the probability that the true correlation is zero
# and by extension that the true relationship between the two variables is
# no relationship - x does not affect y.) We can do this with:
cor.test(x = AmmoniteChambers, y = AmmoniteDiameters, method = "spearman",
  exact = FALSE)$estimate

# You will likely get a strong positive value, which is what we should expect.
# You can get the estimated p-value with:
cor.test(x = AmmoniteChambers, y = AmmoniteDiameters, method = "spearman",
  exact = FALSE)$p.value

# This should be pretty small and smaller than what we got in Lecture because
# your sample size is larger and hence we are more confident that zero is an
# unlikely true value for the correlation coefficient.
#
# To finish we will look at a third statistic for bivariate data called the
# COEFFICIENT OF DETERMINATION, and often abbreviated to R^2 (r-squared).
# This is derived directly from a regression and represents the proportion of
# y that can be explained by x (under the specific type of regression used).
# We might naively assume that r-squared is the same as our Pearson's
# correlation coefficient (r), squared. However, this is only true when the
# regression line being fit is linear. In other words, although correlation
# is only meaningful when the response of y to x is linear the coefficient of
# determination is meaningful regardless of the shape of the regression. We
# can illustrate this by revisiting Quartet II:
plot(x = x2, y = y2, xlim = c(3, 19), xlab = "x", ylab = "y", pch = 20, cex = 2,
  main = "Quartet II")
points(xs, y_pred, type = "l")

# Here we fitted a polynomial regression to the data and we can see that it
# is an excellent fit. We might realistically expect, then, x to explain a
# very large proportion of y, i.e., to have an r-squared of close to one. We
# can get the exact value by using the summary function for our model and the
# $r.squared part of the output:
summary(lm(y2 ~ poly(x2, 2, raw = TRUE)))$r.squared

# You should see a value that is indeed very close to one. We can compare this
# to our (inappropriate) linear regression for the same data:
summary(lm(y2 ~ x2))$r.squared

# You should get a notably lower value. Thus r-squared can be a useful tool
# in comparing different explanatory models for the same data. But as always,
# plotting should be applied first to gauge the appropriateness of these. You
# can now apply this to your own ammonite data, first for the linear model:
summary(lm(AmmoniteDiameters ~ AmmoniteChambers))$r.squared

# Then for the exponential model:
summary(lm(AmmoniteDiameters ~ exp(0.12 * AmmoniteChambers) - 1))$r.squared

# What value did you find? Which model does this suggest does the better job?
#
# Finally, you can compare the linear value to the square of the correlation
# coefficient (r) you got earlier:
cor.test(x = AmmoniteChambers, y = AmmoniteDiameters)$estimate ^ 2

# You should see this is the same as your R-squared because in this case the
# regression is linear. Note that squaring a negative correlation will return
# a positive value (i.e., r-squared runs on a 0 to 1 scale) as it does not
# make sense for x to explain a negative proportion of the data. In other
# words, collectively regression, correlation, and the coefficient of
# determination are tools that tell you different things about the relationship
# between x and y.

################################################################################
#                                                                              #
#                                     MCQ                                      #
#                                                                              #
# You are now ready to attempt this week's MCQ which is a series of ten        #
# multiple choice questions (MCQs) on last week's lecture and this practical.  #
#                                                                              #
################################################################################
