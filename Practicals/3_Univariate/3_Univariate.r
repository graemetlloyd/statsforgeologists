################################################################################
#                                                                              #
#                     PRACTICAL III - UNIVARIATE STATISTICS                    #
#                                                                              #
################################################################################

################################################################################
#                                                                              #
#                               TODAY's AIMS                                   #
#                                                                              #
# - LEARN TO GET YOUR OWN UNIVARIATE DATA INTO R, PLOT AND SUMMARISE IT        #
# - LEARN TO PERFORM ONE- AND TWO-SAMPLE T-TESTS                               #
# - LEARN TO PERFORM AN ANOVA TEST                                             #
#                                                                              #
################################################################################

# Before we start begin by setting your working directory as we did in
# Practicals 1 and 2. This is a useful habit to get into.

# Today you will start using your ammonite quadrat from lecture. We will find
# that this can help us answer multiple different questions, but today we
# will focus on the size of our ammonites by measuring their diameters as we
# did in lecture. Before we begin you can get a digital copy of your quadrat as
# a PDF by modifying the following line. I.e., replace "X4" with whatever your
# quadrat's code is (it will not work otherwise!):
browseURL("http://www.graemetlloyd.com/teaching/SOEE1475/20/Lower_Bed/X4.pdf")

# You can save this to your working directory ready to refer to in later
# practicals (i.e., use Save/Download rather than Open).
#
# Next you will need to complete the task you began in lecture of measuring
# the diameters (in mm) of each ammonite in your quadrat. Use the table on the
# back of your quadrat to write these numbers down. (You will use these again
# in future practicals.) When you are done you should modify the following line
# replacing the number of the ammonite with the diameter you measured. E.g., if
# your first ammonite has a diamater of 205 then replace 1 with 205. When you
# are done run the line in R:
AmmoniteDiameters <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
  17, 18, 19, 20)

# Let's begin by plotting the data using the same bins as the histogram we
# generated in lecture:
hist(AmmoniteDiameters, breaks = c(50, 75, 100, 125, 150, 175, 200, 225, 250,
  275, 300), border = 0, col = "Grey", xlab = "Ammonite Diameter (mm)",
  main = "Histogram of Ammonite Diameters")

# NB: If you get an error here it is most likely because you didn't read the
# previous line and your ammonite diameters are still just the ammonite
# numbers!
#
# How does this compare with the larger sample we generated in lecture? I.e.,
# compare the data from just your quadrat with the plot from the Lecture
# Capture (available on Minerva). You might see that your own data looks less
# like the "ideal" normal distribution, i.e., a clear central peak with symmetric
# tapering to less common values at the extremes. This is due to the nature of
# small sample sizes, but hopefully you should be convinced that the data do
# approximate a normal distribution based on the larger sample we were able to
# use in lecture by combining data from multiple quadrats together. We should
# thus probably not be too worried about using summary values for location and
# dispersion that assume data are normally distributed. For example, the mean:
AmmoniteMean <- mean(AmmoniteDiameters)
AmmoniteMean

# Now the median (middle) value:
AmmoniteMedian <- median(AmmoniteDiameters)
AmmoniteMedian

# And finally the mode(s):
Mode <- function(x) return(rle(sort(x))$values[which(rle(sort(x))$lengths
  == max(rle(sort(x))$lengths))])
AmmoniteMode <- Mode(AmmoniteDiameters)
AmmoniteMode

# You may find there are multiple values for the mode. More generally you will
# likely find that your mean, median and mode are different, even though they
# are all intended to capture the "centre" of the distribution of values.
# However, they should still be fairly close. If the data perfectly fit the idea
# of a normal distribution these values would all be identical, and represent
# the peak at the centre of the distribution. I.e., just as the special feature
# of a uniform distribution is that each value has equal probability the special
# feature of a normal distribution is that the mean, median, and mode are the
# same. However, the scourge of small sample sizes effects us here.
#
# Let's replot the histogram with the mean and median overlaid on top:
hist(AmmoniteDiameters, breaks = c(50, 75, 100, 125, 150, 175, 200, 225, 250,
  275, 300), border = 0, col = "Grey", xlab = "Ammonite Diameter (mm)",
  main = "Histogram of Ammonite Diameters")
lines(x = c(AmmoniteMean, AmmoniteMean), y = c(0, 1000), col = "Red", lty = 2)
text(x = AmmoniteMean, y = 0, col = "Red", labels = "Mean")
lines(x = c(AmmoniteMedian, AmmoniteMedian), y = c(0, 1000), col = "Blue", lty = 2)
text(x = AmmoniteMedian, y = 1, col = "Blue", labels = "Median")

# You should see that these two values are at least close to each other. But
# what can we say about the population mean? Here the population would be all
# the ammonites that existed at the time (i.e., the Jurassic). Although most
# individual ammonites will not have made it into the rock record, and many
# others are not exposed at the surface, we should still be able to make some
# statements about this population. Note that we are assuming our sample here
# is unbiased, e.g., that there was no difference between the chances of a
# large individual or a small individual being preserved. Furthermore, to avoid
# a collection bias we are counting all the ammonites in each quadrat. (Without
# this strategy we may consciously, or subconsciously, have biased our sample
# towards, for example, larger individuals - undermining any conclusions we may
# have reached.) To illustrate this point we encountered a commercial fossil
# collector on the beach who estimated the average ammonite to be 200 mm in
# diamater. We are suspicious of this value, so we can use a one-sample t-test
# to directly estimate the probability of this hypothesis:
t.test(x = AmmoniteDiameters, mu = 200)$p.value

# What value did you get? Was it low (close to zero) or high (close to one)?
# What does this mean for the collector's hypothesis? Does this make us
# suspicuous that they are biased towards collecting larger or smaller
# ammonites? (I.e., compare this with the mean of your sample.)
#
# You can also make your own estimate of the population mean, or at least
# specify a range you believe it lies in. To do this you must first choose a
# confidence interval. We will discuss what these mean in more detail later in
# the course, but all you need to know now is that this represents the middle
# N% of the t-distribution. Like in lecture we will here choose a relatively
# high value (99%; conf.level = 0.99):
t.test(x = AmmoniteDiameters, conf.level = 0.99)$conf.int[1:2]

# These represent the quartile values of 0.5% and 99.5% (i.e., refer back to
# what we did with the dinsoaur data in the last practical). How big of a range
# do they span? What if we go for a lower value, e.g., 50% (conf.level = 0.5)?:
t.test(x = AmmoniteDiameters, conf.level = 0.5)$conf.int[1:2]

# You should see the range is narrower. This a general phenomenon in
# statistics, i.e., the more confident we wish to be the less precise we can
# be, and vice versa. Thus the choice of confidence level is a trade off between
# wanting to be more confident or more precise.
#
# But what if we want to be 100% (conf.level = 1) confident?:
t.test(x = AmmoniteDiameters, conf.level = 1)$conf.int[1:2]

# You should see that at this level the answer is useless, i.e., the range of
# possible values is negative infinity to positive infinity. This ia (again) a
# reflection that the underlying assumption is that our data are normally
# distributed and hence the full area under the curve spans infinite values,
# (Note that many other probability functions extend to infinity at one end and
# we will encounter these, and how to deal with them, later in the course.) For
# now you simply need to know that 100% confidence is not a privilege we have
# access to with statistics (or more generally as scientists).
#
# Now let's move to thinking about the other kind of t-test, the two-sample
# t-test. As there weren't enough students to cover every quadrat I measured
# one set of ammonites myself and got the following set of measurements:
AmmoniteDiameters2 <- c(196, 202, 140, 174, 155, 164, 238, 176, 217, 217, 122,
  137, 104, 208, 192, 141, 126, 192, 173, 116)

# Although we have a clear expectation that these two samples are derived from
# the same population (as they are really part of the same larger sample of the
# bigger 10-by-10 metre quadrat) you can apply the two-sample t-test which will
# give the probability that the two samples (yours and mine) are derived from
# populations with the same mean:
t.test(x = AmmoniteDiameters, y = AmmoniteDiameters2)$p.value

# You will most likely get a moderate value (probably no zero after the decimal
# point), i.e., a reasonably high probability they are from populations with the
# same mean. It will likely not be 1 though (even though we know it should be).
# Remember, though, that this kind of answer cannot ever tell us that our two
# samples are drawn from the same population, only the probability the
# populations they are drawn from have the same mean.
#
# You can also see just how close the two sample means are with:
mean(AmmoniteDiameters)
mean(AmmoniteDiameters2)

# You will again (likely) find that these two values are slightly different,
# which is what we expect from two (random) samples of the same population.
# This is why we can't be certain (i.e., p = 1) that the two samples come from
# populations with the same mean. In theory, we could continue and compare any
# two quadrats (any two samples) to ask the question of whether they are
# drawn from populations with the same mean, but this would get laborious (100
# total quadrats would equal 4950 so-called "pairwise comparisons"). Besides
# which there is already a technique that effectively does the job of an
# N-sample t-test (where N is 3 or larger). This is called ANOVA, a contraction
# of "ANalysis Of VAriance".
#
# This is a bit more complex to calculate so we wil go through it step-by-step
# below. Here we will apply it in the simplest case (only three samples) by
# reusing our heights for professions example from lecture. Specifically, we
# will import the heights of the students in SOEE1475 as our geologists sample:
ClassHeights <- c(71, 69, 73, 67, 61, 77, 74, 68.5, 68, 71, 64, 70, 62, 64, 67,
  69, 63, 63, 66, 69, 68, 72)

# And our combined Chicago Bulls and Chicago Sky basketball players sample:
BasketballHeights <- c(75, 76, 76, 76, 81, 78, 77, 84, 84, 82, 76, 75, 78, 79,
  83, 80, 75, 76, 73, 77, 70, 74, 77, 74, 76, 69, 70, 68)

# And as a third sample we will use another profession we expect to represent a
# population with a different mean height, namely jockeys:
JockeyHeights <- c(64, 65, 62, 63, 64, 68, 66, 61, 62, 66, 63, 67, 65, 64, 68,
  66, 66, 63)

# This comes from the heights of jockeys in the Kentucky Derby (the only heights
# I could find online) and although it is sex-biased (i.e., mostly male riders)
# the normal assumed bias of this leading to a larger mean height doesn't apply.
# I.e., we don't need to be concerned in the same way we might have been about
# the basketball players sample where we deliberately avoided a potential mis-
# interpretation of a sex-biased difference as a profession-biased difference in
# population means. In other words this is an application of Lesson #2. We will
# now combine our samples into a single variable (in the computing sense) with:
ANOVAData <- list(Geologists = ClassHeights,
  Basketball_players = BasketballHeights, Jockeys = JockeyHeights)

# This is a different type of variable called a LIST. These are helpful when we
# wish to store either data of different types (e.g., numbers and names) or, as in
# this case, samples of different sizes. I.e., a table wouldn't work as the
# number of "rows" for each column would be different, but also that our rows
# would not have any corresponding meaning (e.g., you are not logically "matched
# up with" a particular jockey or basketball player. We can see exactly how
# large each sample is with:
lapply(ANOVAData, length)

# There are a couple of things to note here: 1. None of our samples are
# especially large (only 20 or so for each profession), and 2. there is a strange
# dollar symbol in front of each sample name. As we found out in lecture our
# class sample was apparently too small to really tell if heights nicely
# fitted a normal distribution so it might seem like a bad idea to have similarly
# small samples for our two sporting professions. However, what we should
# find here is that the EFFECT SIZE (i.e., the strength of the difference
# between profession heights) is so large we will comfortably reject (show a
# low probability) that each sample is drawn from populations with the same
# mean.
#
# When the effect size is expected to be much smaller, e.g., the male:female
# birth ratio (which is very slightly male-skewed), a much larger sample is
# needed to show that (in that example) 50:50 is not correct. I.e., if we suspect
# that 50.5:49.5 is the correct ratio we would need more data than if it were a
# stronger difference such as 75:25.
#
# The dollar symbols, on the other hand, just indicate the special way lists need
# to be subset as variables (i.e., they do not use the square brackets in the same
# way as vectors or tables do). We have already seen (above) that statistical test
# functions in R (e.g., the t.test) output their result(s) as lists (e.g., we used
# the $p.value above to get just the probability value of the output).
#
# Before we start to see how ANOVA works let's visualise our data (Lesson #1).
# We can do this with a boxplot:
boxplot(ANOVAData, ylab = "Height (inches)")

# Note that the boxplot function already understands each part of the list
# should be it's own boxplot, but with a common y-axis. This is another
# reason to store data as a list. You should see here the expected skews of
# each sample (basketball players with highest median, jockeys with lowest).
# But note that each sample at least overlaps with the others (even the tallest
# jockey isn't shorter than the shortest basketball player). If there were no
# overlap the answer would (normally) be obvious and a test would probably not
# be required. We can also visualise this data as a stack of histograms:
par(mfrow = c(3, 1))
for(i in 1:3) hist(ANOVAData[[i]], xlim = c(min(unlist(ANOVAData)),
  max(unlist(ANOVAData))),
  breaks = min(unlist(ANOVAData)):max(unlist(ANOVAData)),
  main = names(ANOVAData)[i], col = rainbow(3)[i], border = 0,
  xlab = "Height (inches)")

# ANOVA operates by comparing the ratio of the distances of each data point
# (termed REPLICATES) from the TREATMENT (i.e., sample) mean to the GLOBAL MEAN
# (i.e., the mean of all REPLICATES). Here our treatments are our three
# professions and our replicates the heights of each individual. We can begin
# by looking at our treatment means:
lapply(ANOVAData, mean)

# Again, this just shows us what we already knew. I.e., basketball players have
# the highest mean height, and jockeys the lowest. We can next look at the
# global mean (effectively treating the data like a single large sample):
mean(unlist(ANOVAData))

# We see this is larger than our geologist or jockey means and is really being
# disproportionately influenced by the large height of our basketball players.
# I.e., the tallest basketball players are much taller than the shortest
# jockeys are shorter than the mean geologists. (Our basketball treatment is
# also larger (more individuals) than our jockey treatment.)
#
# With ANOVA we are in effect asking whether our samples might actually be
# drawn from a single population with a mean close to our global mean, and
# thus differences between treatment means are just the outcome of random
# samples. I.e., in the same way the average height of a member of SOEE1475
# is likely to be slightly lower or higher than the mean for all Leeds
# undergraduates. Of course here we suspect that this difference is not slight
# and due to a real difference in mean heights amongst professions. The next
# step in ANOVA is to calculate the sum of the squared distance for each
# treatment:
unlist(lapply(ANOVAData, var)) * (unlist(lapply(ANOVAData, length)) - 1)

# This is the distance from the mean squared (as we did with the VARIANCE
# metric in lecture) as a way to make the values positive before summing them.
# We now sum these values together:
sum(unlist(lapply(ANOVAData, var)) * (unlist(lapply(ANOVAData, length)) - 1))

# Now they need to be normalised by the total number of individuals (as
# otherwise just a larger number of individuals would lead to a larger sum of
# squares distance where really we want to capture something about the average
# distance of each individual from the mean. Specifically we use something
# called the DEGREES OF FREEDOM. We will encounter this properly later in the
# course, but all we need to know now is that this is the number of individuals
# in each treatment minus one:
lapply(lapply(ANOVAData, length), function(x, subt_amnt) x - subt_amnt,
  subt_amnt = 1)

# We can sum these as well:
sum(unlist(lapply(lapply(ANOVAData, length),
  function(x, subt_amnt) x - subt_amnt, subt_amnt = 1)))

# And divide our sum of squares by this total:
sum(unlist(lapply(ANOVAData, var)) * (unlist(lapply(ANOVAData, length)) - 1)) /
  sum(unlist(lapply(lapply(ANOVAData, length),
  function(x, subt_amnt) x - subt_amnt, subt_amnt = 1)))

# This is our WITHIN GROUP VARIANCE, the first part of our ANOVA ratio. The
# next part is our AMONG GROUP value, where we start by getting the squared
# distance between our treatment means and our global mean:
(mean(unlist(ANOVAData)) - unlist(lapply(ANOVAData, mean))) ^ 2

# This time we multiply each difference by the TREATMENT SIZE, i.e., the number
# of individuals in each treatment:
((mean(unlist(ANOVAData)) - unlist(lapply(ANOVAData, mean))) ^ 2) *
  unlist(lapply(ANOVAData, length))

# Again we will begin by summing these:
sum(((mean(unlist(ANOVAData)) - unlist(lapply(ANOVAData, mean))) ^ 2) *
  unlist(lapply(ANOVAData, length)))

# And we will normalise by degrees of variance, this time it is the number of
# treatments minus one, i.e., 2:
sum(((mean(unlist(ANOVAData)) - unlist(lapply(ANOVAData, mean))) ^ 2) *
  unlist(lapply(ANOVAData, length))) / (length(ANOVAData) - 1)

# Collectively we now have within group and among group variance values:
WithinGroup <- sum(unlist(lapply(ANOVAData, var)) * (unlist(lapply(ANOVAData,
  length)) - 1)) / sum(unlist(lapply(lapply(ANOVAData, length),
  function(x, subt_amnt) x - subt_amnt, subt_amnt = 1)))
AmongGroup <- sum(((mean(unlist(ANOVAData)) - unlist(lapply(ANOVAData,
  mean))) ^ 2) * unlist(lapply(ANOVAData, length))) / (length(ANOVAData) - 1)

# We can use these to get something called the F-STATISTIC, which is simply
# among group divided by within group variance:
FValue <- AmongGroup / WithinGroup
FValue

# We can see this value is much higher than one - there is much greater
# variance among groups than within them. Or to put it another way our
# professions are more different from each other than individuals are from
# their treatment means. However, as with any small sample we might expect
# some difference by chance alone so like our t-tests earlier we really want to
# turn this value into a probability, specifically the probability that our
# three treatments are all drawn from populations with the same mean. We can do
# this using the F-DISTRIBUTION (another probability distribution - these usually
# lie behind every statistical test and is why we began the course by introducing
# them as a general concept). This time though we must also give both of our
# two degrees of freedom values to get a probability (i.e., the first being the
# number of treatments minus one and the second being the number of replicates
# minus the number of treatments):
df(x = FValue, df1 = (length(ANOVAData) - 1),
  df2 = sum(unlist(lapply(lapply(ANOVAData, length),
  function(x, subt_amnt) x - subt_amnt, subt_amnt = 1))))

# You should see a tiny value. I.e., it seems likely (as we already thought)
# that geologists, jockeys and basketball players are not drawn from
# populations with the same mean height. The only major disadvantage with this
# conclusion is that it doesn't tell us exactly why this is the case. I.e., is
# it that basketball players are taller? That jockeys are shorter? Or both? We
# know from our t-test in lecture that certainly geologists and basketball
# players are very likely to be drawn from populations with a different mean
# height:
t.test(x = ANOVAData$Geologists, y = ANOVAData$Basketball_players)$p.value

# We can, as the number of treatments is small, just repeat this for our two
# other comparisons:
t.test(x = ANOVAData$Geologists, y = ANOVAData$Jockeys)$p.value
t.test(x = ANOVAData$Basketball_players, y = ANOVAData$Jockeys)$p.value

# Collectively then it is pretty clear that all three professions are likely
# drawn from populations with different mean heights (all probabilties are very
# small), and specifically by looking at their means we can say that population
# means for: 1. geologists are taller than jockeys, 2. basketball players are
# taller than geologists, and 3. basketball players are taller than jockeys. We
# can additionally see that jockeys and basketball players sharing a population
# mean is the least probable outcome, and geologists and jockeys sharing a
# population mean is the most probable outcome - exactly as we would expect.
#
# Although that concludes today's practical it is strongly advised that you
# save your ammonite measurements somewhere, ideally by saving this script
# (as you have edited it for your own quadrat). You will reuse these values
# next week.

################################################################################
#                                                                              #
#                                     MCQ                                      #
#                                                                              #
# You are now ready to attempt this week's MCQ which is a series of ten        #
# multiple choice questions (MCQs) on last week's lecture and this practical.  #
# This is on Minerva: SOEE1475 > Statistics resources > MCQ.                   #
#                                                                              #
################################################################################
