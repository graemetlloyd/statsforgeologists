################################################################################
#                                                                              #
#     PRACTICAL I - INTRODUCTION TO RSTUDIO AND BASIC DATA VISUALISATION       #
#                                                                              #
################################################################################

################################################################################
#                                                                              #
#                           HOW TO USE THIS SCRIPT:                            #
#                                                                              #
# All lines below are either comments (beginning with the hash symbol) or R    #
# code. Both can be copied and pasted directly into the R console, but the     #
# former will not be executed.                                                 #
#                                                                              #
# To use this script you must:                                                 #
#                                                                              #
# 1. Load it into RStudio (the implementation of R we will use here). Note     #
#    that double-clicking on the file may not work, so...                      #
#    Word Processor as these will attempt to format the data inappropriately   #
#    (e.g., messing up quote symbols etc.).                                    #
# 2. Open RStudio. From a University Machine go to the Start menu then scroll  #
#    down until you see the RStudio folder and click on it. RStudio should be  #
#    the only thing inside). NB: If you prefer you can use bring your own      #
#    machine to practical and use that instead (R and RStudio are free and     #
#    on any operating system. R is available at: https://cran.r-project.org/   #
#    and RStudio is availabe at: https://rstudio.com/.                         #
# 3. Load this script into RStudio (File > Open File), and...                  #
# 4. Work through each line below, reading the explanations beginning with '#' #
#    and either copying and pasting (DO NOT retype!) from the script to the    #
#    console window or click the "Run" button. (If you do the former you may   #
#    have to hit enter to execute the line.) Note that many operations in      #
#    R will provide no immediate feedback on their effect, but persevere as    #
#    the next part will usually explain things.                                #
#                                                                              #
################################################################################

################################################################################
#                                                                              #
#                               TODAY's AIMS                                   #
#                                                                              #
# - LEARN HOW TO USE RSTUDIO AND THE R CONSOLE                                 #
# - LEARN HOW TO IMPORT DATA INTO R/RSTUDIO                                    #
# - LEARN TO PRODUCE BASIC DATA VISUALISATIONS IN RSTUDIO                      #
#                                                                              #
################################################################################

# Note that RStudio's interface consists of multiple "panels". Typically these
# will include the CONSOLE (lower left; sometimes also called the terminal) the
# file or script (typically upper left) and two other panels which may show
# either a help file or a graph. Note these can be resized or customized to
# suit your personal working style.

# R is a statistical programming language and can be used in a number of ways.
# First of all it can do basic calculations (copy and paste from the script to
# the console or hit "Run"):
1 + 2

# You should see the answer 3 appear in the console (if not try hitting enter).
# Now try typing something else into the console yourself (NB: * is used for
# multiply and / for divide):

# Most of the time we will not want to print our answer to the screen, but
# store it in something called a VARIABLE (basically a "bucket" in which data
# resides). (Note that this is the computing sense of the word and during this
# course we will also encounter a separate statistical meaning - any
# characteristic, number, or quantity that can be measured or counted):
x <- 1 + 2

# Here our variable is named "x" and the arrow "<-" indicates that the data on
# the right (1 + 2) is being stored in it. Note that this time nothing will
# happen after hitting enter because no other response is required. (x has
# "caught" the output before it reaches the screen.) We can see the contents
# of a variable at any time by typing its' name:
x

# This time you should see the answer again (3). We can always modify and
# update our variable by performing a calculation on it and overwriting it
# with the answer. For example, we can multiply (using the "*" symbol) by
# 2 by typing:
x <- x * 2

# Again, no response will be given, so we can type it again to check it has
# gotten the correct answer (6):
x

# Note that R will NEVER warn you if you try and overwrite a variable so good
# practice is to use a new name each time. Furthermore, it is best to use
# more descriptive names (avoid "x", "y" etc.) and also avoid names that are
# already used by basic R functions (mean, table etc.). We can also use
# variables to store more complex forms of data, such as vectors:
TyrannosaurBodyMasses <- c(2013, 627, 276, 889, 2710, 605, 3215, 17, 1027,
  2487, 72, 494, 699, 44, 2539, 3018, 2345, 947, 51, 7694, 172)

# A VECTOR can be thought of as a single row or column of data and can be
# entered directly into R, as we have done here, using the combine
# FUNCTION "c". Functions are the "doing" parts of R and are usually
# indicated by their name (here "c") followed by parentheses ("()") which
# contain data or other options separated by commas. In this case we only
# have data - a series of body mass estimates for tyrannosaurid dinosaurs in
# kilograms - and we are storing ("<-") this data in an appropriately named
# variable ("TyrannosaurBodyMasses"). Again, we can view the contents by
# typing its' name:
TyrannosaurBodyMasses

# It's worth noting here that data in R can be of fundamentally different
# types. Normally either numeric (numbers) or characters (text, also termed
# STRINGS). We can make sure our data is numeric by using another function,
# is.numeric():
is.numeric(TyrannosaurBodyMasses)

# Note that we are giving only one thing to this function, the name of the
# variable we want to be checked. This is a logical function - there are
# only two possible answers, TRUE or FALSE. You should see the answer "TRUE".
# However, our data set is incomplete as we only have a series of body masses
# and no indication which particular type of dinosaur each corresponds to. To
# add this information we could convert our data into a table with two columns
# (species name and body mass). However, this can be problematic as it would
# involve mixing two different data types (character and numeric, respectvely).
# Besides which, R already has a built in structure for adding names to data:
names(TyrannosaurBodyMasses)

# The "names" function returns the names assigned to a vector. Here you should
# get the answer "NULL" which is R for "this has not been set or is missing".
# We can set the correct names by typing the following:
names(TyrannosaurBodyMasses) <- c("Albertosaurus_sarcophagus",
  "Alectrosaurus_olseni", "Alioramus_spp", "Appalachiosaurus_montgomeriensis",
  "Daspletosaurus_torosus", "Daspletosaurus_torosus", "Daspletosaurus_torosus",
  "Dilong_paradoxus", "Dryptosaurus_aquilunguis", "Gorgosaurus_libratus",
  "Guanlong_wucaii", "Juratyrant_langhami", "Qianzhousaurus_sinensis",
  "Raptorex_kriegsteini", "Tarbosaurus_bataar", "Tarbosaurus_bataar",
  "Tarbosaurus_bataar", "Teratophoneus_curriei", "Timimus_hermani",
  "Tyrannosaurus_rex", "Xiongguanlong_baimoensis")

# Note that we still use names() and not just the variable name as we want to
# store this text as the names for each item in the vector and not overwrite
# the body masses themselves. We can check these names are now set by using the
# names function again:
names(TyrannosaurBodyMasses)

# Note that these names are in "snake case", which is a coding term for using
# underscores ("_") to separate words in a text string (series of characters).
# The common alternative to this is to use "camel case" (or Pascal case), where
# each word begins with a capital, like our variable name
# ("TyrannosaurBodyMasses"). The reason why this is done is names with spaces are
# not generally allowed in computing, e.g., try:
Tyrannosaur Body Masses <- c(2013, 627, 276, 889, 2710, 605, 3215, 17, 1027,
  2487, 72, 494, 699, 44, 2539, 3018, 2345, 947, 51, 7694, 172)

# You should get an error message as R does not understand what you want it to
# do. More generally you will find that R (or any programming language) requires
# very specifically formatted instructions in order to process them. (This is
# why you are working exclusively from this script! You will edit scripts in
# future practicals though, as well as for your assessment.)
#
# The next thing we should learn now that we have a more complex variable type
# is how to access only part of it. This is done by using square brackets ("[]")
# after the variable name. So to get the fifth body mass from our data we can
# type:
TyrannosaurBodyMasses[5]

# Or the second to tenth:
TyrannosaurBodyMasses[2:10]

# Or the third, fifth, and ninth:
TyrannosaurBodyMasses[c(3, 5, 9)]

# Note that we have here learned to use the colon to get a sequence of values
# (2:10) and the combine function ("c()") again to give a series of specific
# values. However, good practice is to use our names and subset the data not
# with numbers but with text (which should always appear in quotes, ""). This is
# because data can be reordered or modified, meaning the second value (for example)
# can change, but the names should continue to be attached to the correct value. We
# can get the body mass for Tyrannosaurus rex with:
TyrannosaurBodyMasses["Tyrannosaurus_rex"]

# You should see a value of 7694. Bearing in mind this is in kilograms we can
# see that Tyrannosaurus rex was a large animal.
#
# Although this is one simple way to get data into R usually we will have
# collected data in some other format, such as a spreadsheet. If you were
# worried you would have to type it in manually, fear not! We can import data
# from MS Excel. First of all, let's look at just such a data set: the NOAA
# database of volcanoes:
browseURL("https://raw.githubusercontent.com/graemetlloyd/statsforgeologists/master/Practicals/1_IntroToR/NOAAVolcanoDatabase.xlsx")

# The above line should load the Excel file in your default browser (manually
# copy and paste the URL if it fails for some reason). Note that the browser
# may simply ask if you want to (or automatically) download the file. Say yes
# and download it to an appropriate folder of your choice. NB: if it auto
# downloads it will most likely be found in the downloads folder of your
# machine - if so move it to your personal folder. Once this is done open the
# file in Excel to view its' contents. You should see a table with ten columns
# and 1571 rows (excluding the headers). Each row corresponds to a volcano on
# Earth and each column some basic information about it. Note that elevation is
# in metres, negative latitudes and longitudes are South and West respectively,
# and there are a series of codes for Last known aruption:
#   D1 = Last known eruption 1964 or later
#   D2 = Last known eruption 1900-1963
#   D3 = Last known eruption 1800-1899
#   D4 = Last known eruption 1700-1799
#   D5 = Last known eruption 1500-1699
#   D6 = Last known eruption A.D. 1-1499
#   D7 = Last known eruption B.C. (Holocene)
#   U = Undated, but probable Holocene eruption
#   ? = Uncertain Holocene eruption
#   Q = Quaternary eruption(s) with the only known Holocene activity being
#     hyderothermal.
# There is also an odd looking "Number" column. This is an essential part of all
# good databases and represents a unique value for each record that cannot be
# duplicated elsewhere and is known as a PRIMARY KEY. NB: Your student ID is
# just such a number and ansures you will never be confused with someone else
# who happens to have the same name as you. (I.e, this is why a number system is
# used.)
#
# When you collect your own data later in your degree (e.g., during field work)
# it is highly recommended that you use such a system. E.g., if you measure
# something across a series of samples then later decide to measure a second
# feature you will need to know which original measurement corresponds to which
# sample or you will not be able to match them up, forcing you to take all your
# original measurements over again. Numbering each sample or record from the
# start saves you this headache.
#
# Unfortunately there is no "base" function in R that will import data from
# Excel so we will have to engage with another important aspect of R which is
# PACKAGES. These are specialised sets of functions usually revolving around a
# particular topic (e.g., there are many packages that are geology specific) or
# process. The one we require here is called "gdata" and this will have to be
# installed before we can use it:
install.packages("gdata", dependencies = TRUE)

# (If you are prompted to use a personal directory just click yes.) This might
# take a few seconds to work, but you should see some text and a status bar.
# Now this package should be permanentally installed on your machine the next
# task is to load it (and its' functions) into memory:
library(gdata)

# Here we use the function "library()" that simply loads a package (aka library)
# into memory. We can now use the function "read.xls()" to read in our volcano
# data. We will store ("<-") this in a new variable ("VolcanoData"), but we
# also need to supply read.xls with the filepath to our data. Here we will do
# this using the "file.choose()" function which will load a file explorer much
# as you would do with GUI software. Navigate to where you downloaded the data
# and select the Excel file:
VolcanoData <- read.xls(file.choose())

# If you get an error (unfortunately pretty likely as University machines often
# do not have the required Perl installation) try the following workaround:
#   1. Load the data in Excel.
#   2. Save the data as a "CSV" (comma-delimited) file instead.
#   3. Run this line (and choose the new CSV version not the old xlsx one):
VolcanoData <- read.csv(file.choose())

# We can also read in data directly from a web link:
VolcanoData <- read.csv("https://raw.githubusercontent.com/graemetlloyd/statsforgeologists/master/Practicals/1_IntroToR/NOAAVolcanoDatabase.csv")

# This has imported the data as a TABLE (i.e., with rows and columns). You
# should find it has automatically recognised the headers:
colnames(VolcanoData)

# This should return the headers from the Excel file. Note that "colnames()"
# functions like "names()" did for vectors, but with the added "col" part to
# indicate columns. There are row names too:
rownames(VolcanoData)

# Like vectors we can also use square brackets to isolate only part of the
# data. For example, just the latitudes:
VolcanoData[, "Latitude"]

# Note that this time the square brackets must contain a comma. This separates
# row values (before the comma) from column values (after the comma). We don't
# provide a row value here so it defaults to giving us all rows. We can reuse
# our tricks with vectors to get, say, the first 100 latitudes:
VolcanoData[1:100, "Latitude"]

# We may also want to access just part of the data. For example, there are many
# different kinds of volcano. We can see just how many by taking the unique
# values in our "Type" column:
unique(VolcanoData[, "Type"])

# Here we use the "unique()" function to remove any duplicates. Now let's find
# which values in the "Type" column are stratovolcanoes:
which(VolcanoData[, "Type"] == "Stratovolcano")

# This is a more complex function ("which()") that returns any values that are
# logically TRUE from the statement VolcanoData[, "Type"] == "Stratovolcano".
# I.e., any value for "Type" that equals ("==") "Stratovolcano". Note that a
# double equals is used here because "=" on its own is reserved for storing
# data in a variable:
x = 3
x

# "<-" is preferable though as it avoids confusion with mathematical statements
# and clearly shows (as it forms an arrow) what is being stored where. Now that
# we have a means of showing which volcanoes are stratovolcanoes we can look at
# just those rows using:
VolcanoData[which(VolcanoData[, "Type"] == "Stratovolcano"), ]

# This is still a pretty large list, but it includes many iconic volcanoes
# (e.g., Fuji, St Helens). We could also get just a specific volcano using its'
# name. So for Fuji:
VolcanoData[which(VolcanoData[, "Volcano.Name"] == "Fuji"), ]

# Note that, again, we have to use the *exact* name. None of these will work:
VolcanoData[which(VolcanoData[, "Volcano.Name"] == "fuji"), ]
VolcanoData[which(VolcanoData[, "Volcano.Name"] == "Mt Fuji"), ]
VolcanoData[which(VolcanoData[, "Volcano.Name"] == "Mount Fuji"), ]

# You should now have a basic grasp of how R operates, but we have not yet
# encountered its' true strength: plotting data. We now have two data sets in
# memory to play with so let's do some simple visualisations. First of all, we
# will look at our Tyrannosaur data. Note that this is UNIVARIATE data - i.e.,
# it concerns only a single type of measurement. This is best plotted using
# either a histogram or a boxplot. For a histogram we can just type:
hist(TyrannosaurBodyMasses)

# And for a boxplot:
boxplot(TyrannosaurBodyMasses)

# (NB: In RStudio you should see graphs appear in a separate panel, probably
# at lower right.)
#
# The most important thing to note here is how simple it is to get straight to
# a graph (visualisation) of the data. Go ahead and look at the instructions
# for producing a boxplot in Excel:
browseURL("https://support.office.com/en-gb/article/create-a-box-plot-10204530-8cdf-40fe-a711-2eb9785e510f")

# Or a histogram:
browseURL("https://support.microsoft.com/en-gb/help/214269/how-to-use-the-histogram-tool-in-excel")

# These are just two of many reasons why R is the right tool for the job. But
# let's get back to looking at these plots:
hist(TyrannosaurBodyMasses)

# The histogram automatically "bins" our data (here into 1000 Kg widths) and
# then counts the number of values in each bin ("Frequency" on the y-axis).
# You should immediately see that most tyrannosaurs are actually (relatively)
# small (0-1000 Kg). Now for our boxplot:
boxplot(TyrannosaurBodyMasses)

# This is a little more complex to process visually. The "box" has a thicker
# line in the centre that represents the median (i.e., middle) value of the
# data. Again, this is quite small (relatively) at just under 1000 Kg. The top
# of the box represents the 75% value, and the bottom the 25% value of the
# data (i.e., the median can also be thought of as the 50% value). Thus the
# middle 50% of the data sits inside the box. Again, we can see that the data
# are clearly mostly found towards the lower end of the full range of values.
# The "whiskers" (this type of plot is often also termed box-and-whiskers) are
# a little more complex to interpret as their meaning can vary. Sometimes they
# cover the full range of values, but here the upper whisker does not and the
# highest point is instead plotted as an OUTLIER - an anomalous value that is
# vastly different to the rest of the data. We can tell by using the y-axis that
# this value is Tyrannosaurus rex - further emphasising that T. rex is not just
# the biggest tyrannosaur, but is an anomalously large one.
#
# Next we can consider BIVARIATE data (two different types of measurement) using
# our volcano data. Specifically, we will look at latitude and elevation. Here
# there is a single obvious plot type - the scatter plot. In R these can be
# produced with simply "plot()":
plot(x = VolcanoData[, "Latitude"], y = VolcanoData[, "Elevation"])

# This data is pretty complex and it can be hard to tease out clear patterns.
# We do see most volcanoes have a positive elevation (i.e., most values are
# above zero on the y-axis). We can go and check our data to see what those
# negative (< 0) elevations represent:
VolcanoData[which(VolcanoData[, "Elevation"] < 0), ]

# You should see the "Type" column is dominated by "Submarine volcano", so this
# makes sense. There are many other kinds of plot we can make in R (and many
# options to explore for prettier visualisations). For some examples check
# out:
browseURL("https://www.r-graph-gallery.com/")

# We will encounter many more throughout the course, including ways to add a
# geologic timescale to a plot as well as how to create stereonets in R.
#
# However, it is worth pointing out that one particular type of plot should be
# avoided: the dreaded pie chart. I will not teach you how to do these in R
# and if you want to know why they are bad you can read one of the many
# articles online:
browseURL("https://www.google.co.uk/search?q=why+pie+charts+are+terrible")

# To finish up we will look at how we can extract our graphs from R. But first
# we need to set the "working directory", the folder to which any files will be
# written by default. This can be done using the menu: Session > Set Working
# Directory > Choose DIrectory... Select the same folder you placed your
# downloaded volcano data in previously. You can check this has worked using
# the "getwd()" (get working directory) function:
getwd()

# This is another function we don't need to give any information to (i.e.,
# there is nothing inside the parentheses). It should return the filepath to
# the folder you selected before.
#
# Now let's export our first plot, our histogram from above:
jpeg("TyrannosaurBodyMassesHistogram.jpg")
hist(TyrannosaurBodyMasses)
dev.off()

# Note we are now using a new function ("jpeg()") that "opens" a file with the
# supplied name (here "TyrannosaurBodyMassesHistogram.jpg"). However, we still
# need to execute our plot ("hist(TyrannosaurBodyMasses)") and finally we use
# another new function ("dev.off()") that turns "off" the plotting device.
# This effectively "closes" the file being written to. Now you should have a new
# jpeg in your working directory. You should be able to take a look at it with:
browseURL("TyrannosaurBodyMassesHistogram.jpg")

# You might note that it is not particularly high resolution. This is largely
# due to this being a RASTER IMAGE (i.e., being constructed of pixels) and not
# a VECTOR IMAGE (a mathematical description of each object being plotted that
# can be zoomed into infinitely without loss of resolution). Clare will teach
# you more about this later in the semester with Inkscaoe. To export in vector
# format we can use a PDF instead:
pdf("TyrannosaurBodyMassesHistogram.pdf")
hist(TyrannosaurBodyMasses)
dev.off()

# Take a look at the PDF file:
browseURL("TyrannosaurBodyMassesHistogram.pdf")

# Try zooming in as far as you can until the image pixelates. You should find
# this never happens. Another advantage of PDFs as output is we can include
# multiple plots in them. Let's try adding all three plots from above into a
# single PDF:
pdf("Practical_1_plots.pdf")
hist(TyrannosaurBodyMasses)
boxplot(TyrannosaurBodyMasses)
plot(VolcanoData[, "Latitude"], VolcanoData[, "Elevation"])
dev.off()

# Take a look at the PDF file:
browseURL("Practical_1_plots.pdf")

# If you scroll/page through you should see all of our plots are available as
# vector images. To finish off today the following code will replot the same
# data, but this time employing some of the different plot options to make
# things a bit clearer and prettier. For example, adding x- and y-axis labels
# ("xlab" or "ylab" options, respectively), adding limits to the x- or y-axes
# ("xlim" or "ylim" options, respectively), adding a plot title ("main" option),
# changing the plotting character ("pch" option), changing the plot colour ("col"
# option) as well as two additional types of plot (barplot of ordered tyrannosaur
# body masses with species labels and a map of volcano locations):
pdf("Practical_1_plots.pdf")
hist(TyrannosaurBodyMasses, main = "Histogram of tyrannosaur body masses",
  xlab = "Body Mass (Kg)", col = "black")
boxplot(TyrannosaurBodyMasses, main = "Boxplot of tyrannosaur body masses",
  ylab = "Body Mass (Kg)")
TyrannosaurBodyMasses <- TyrannosaurBodyMasses[order(TyrannosaurBodyMasses)]
barplot(TyrannosaurBodyMasses, horiz = TRUE,
main = "Bar plot of tyrannosaur body masses by species", xlab = "Body Mass (Kg)",
  names.arg = FALSE, space = 0, border = 0, xlim = c(0, 11000))
text(x = TyrannosaurBodyMasses, y = c(1:length(TyrannosaurBodyMasses)) - 0.6,
  labels = gsub("_", " ", names(TyrannosaurBodyMasses)), pos = 4,
  vfont = c("sans serif", "bold italic"), col = "black")
plot(x = VolcanoData[, "Latitude"], y = VolcanoData[, "Elevation"],
  main = "Scatter plot of volcano latitude vs elevation",
  xlab = "Latitude (degrees; negative = S, positive = N)",
  ylab = "Elevation (m)", pch = 20, col = "blue")
install.packages("maps", dependencies = TRUE)
library(maps)
map(border = NA, fill = TRUE, col = "grey", main = "Map of world volcanoes")
points(x = VolcanoData[, "Longitude"], y = VolcanoData[, "Latitude"], pch = 20,
  col = "red")
dev.off()

# Don't worry too much at this stage what all of the above does. Just take a
# final look at the PDF file:
browseURL("Practical_1_plots.pdf")

# This should give you some idea of the potential of R for visualising data and
# we will encounter many of these functions again throughout the remainder of
# the course.

################################################################################
#                                                                              #
#                                     MCQ                                      #
#                                                                              #
# You are now ready to attempt this week's MCQ which is a series of ten        #
# multiple choice questions (MCQs) on this practical. There will be a new set  #
# of MCQs each week drawn from the previous lecture and practical and you will #
# find them on Minerva: SOEE1475 > Statistics resources > MCQs. These are      #
# formative assessments designed to continously assess your comprehension of   #
# the preceding lecture and practical (although there is no lecture for this   #
# first one). As such they can be taken as many times as you like, but it is   #
# generally recommended that you take them as soon as possible after each      #
# practical and ensure you understand why you got any questions wrong before   #
# proceeding in the course as concepts are built upon successively in          #
# subsequent lectures and practicals. Additionally many of the insights        #
# garnered from MCQs will help develop the skills you require to complete the  #
# summative assessment. Finally, I will also provied model answers for each    #
# MCQ on Minerva, but note that these just reapeat the information present in  #
# the MCQs themselves.                                                         #
#                                                                              #
################################################################################
