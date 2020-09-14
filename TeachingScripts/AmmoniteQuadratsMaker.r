# This makes (new) ammonite quadrat data for both the lower and upper beds

# a = first spiral constant
# b = second spiral constant
# N_points = number of points to define the spiral
# r_max = The maximum r value (polar coordinate) used to draw the spiral
# N_chambers = the number of chambers to draw
# chamber_exponent = the exponent value to use in spacing the chamber walls
# chamber_wall_proportion = the proportion (distance to center) to use when drawing the chamber wall (found by guessing although an approximation seems possible!)
# termination_bearing = bearing of aperture from origin in degrees (0/360 being "North", i.e., the top)
# chirality whther spiral is left- ("Sinistral") or right-handed ("Dextral")

AmmonitePlotter <- function(a = 0.1, b = 0.15, N_points = 10000, r_max = 1000, N_chambers = 30, chamber_exponent = 1.14, chamber_wall_proportion = 0.39, termination_bearing = 180, chirality = "Sinistral") {
  
    # Create r values (polar coordinates) from input:
    r <- seq(from = 0, to = r_max, length.out = N_points)
    
    # Logarithmic spiral equation to get theta (polar coordinates) for all values of r:
    x <- y <- theta <- (1 / b) * log(r / a)
    
    # Convert polar coordinates to cartesian values:
    x <- r[2:length(r)] * cos(theta[2:length(theta)])
    y <- r[2:length(r)] * sin(theta[2:length(theta)])
    x <- c(0, x)
    y <- c(0, y)
    
    # If chirality is dextral multiply all x values by -1 (makes it a dextral spiral):
    if(chirality == "Dextral") x <- x * -1
    
    # Get chamber wall start points:
    chamberwall <- round((N_points * ((chamber_exponent ^ (1:N_chambers)) / (chamber_exponent ^ N_chambers))))
    
    # Get coordinates for drawing chamber walls:
    chamberwalls <- cbind(x[chamberwall], chamber_wall_proportion * x[chamberwall], y[chamberwall], chamber_wall_proportion * y[chamberwall])
    
    # Add column names to chamber walls matrix:
    colnames(chamberwalls) <- c("x1", "x2", "y1", "y2")
    
    # Get bearing of aperture:
    bearing <- ((-1 * (atan2(y[N_points], x[N_points]) * (180 / pi) - 180)) - 90) %% 360
    
    # Get rotation radians required to shift aperture to chosen bearing:
    rotateradians <- (bearing - termination_bearing) / (180 / pi)
    
    # Rotate x and y to match new bearing but do not update x and y yet (or first step will mess up latter!)::
    new_x <- (x * cos(rotateradians)) - (y * sin(rotateradians))
    new_y <- (x * sin(rotateradians)) + (y * cos(rotateradians))
    
    # Update x and y with newly rotated coordinates:
    x <- new_x
    y <- new_y
    
    # Rotate x and y of chamber walls but do not override yet (would create error as need original coordinates for y rotation after x rotation:
    new_wall_x <- (chamberwalls[, c("x1", "x2")] * cos(rotateradians)) - (chamberwalls[, c("y1", "y2")] * sin(rotateradians))
    new_wall_y <- (chamberwalls[, c("x1", "x2")] * sin(rotateradians)) + (chamberwalls[, c("y1", "y2")] * cos(rotateradians))

    # Update chambr walls with new (rotated) x and y:
    chamberwalls[, c("x1", "x2")] <- new_wall_x
    chamberwalls[, c("y1", "y2")] <- new_wall_y
    
    # Compile output:
    output <- list(cbind(x, y), chamberwall, chamberwalls)
    
    # Add anems to output:
    names(output) <- c("spiral", "wall_begins", "wall_coordinates")
    
    # Return output:
    return(output)
    
}

# Short function to convert maximum diameter into a radius:
AmmoniteMaxDiameterToMaxRadius <- function(x) x * 0.6130215

# Function to get dividing walls between chambers for binning later:
ChamberPositions  <- function(chamber_exponent = 1.14, N_chambers = 30, N_points = 10000, r_max = AmmoniteMaxDiameterToMaxRadius(250)) {
  
  ChamberwallRadii <- (round((N_points * ((chamber_exponent ^ (1:N_chambers)) / (chamber_exponent ^ N_chambers)))) / N_points) * r_max
  
  LogChamberwallRadii <- log(ChamberwallRadii)
  
  DividingWalls <- exp((LogChamberwallRadii[2:length(LogChamberwallRadii)] + LogChamberwallRadii[1:(length(LogChamberwallRadii) - 1)]) / 2)
  
  return(DividingWalls)
  
}

# Set dividing walls between chamber counts (i.e., for deciding chamber count given a diameter value):
DividingWalls <- ChamberPositions()

# For the Xth quadrat (A-J) row:
for(X in LETTERS[1:10]) {
    
    # For the Yth quadrat (1-10) column:
    for(Y in 1:10) {
        
        # Start writing to file:
        pdf(paste("/Users/eargtl/Documents/Teaching/statsforgeologists/Ammonite_quadrats/Lower_Bed/", X, Y, ".pdf", sep = ""), , height = 8.27, width = 11.69)
        
        # Set margins of plot to zero:
        par(mai = c(0, 0, 0, 0))
        
        # Set number of ammonites:
        N_ammonites <- 20
        
        # Set chirality of ammonites:
        Chirality <- sample(x = c("Sinistral", "Dextral"), size = N_ammonites, replace = TRUE)
        
        # Set mean ammonite size (in mm):
        mean_ammonite_size <- 100
        
        # Draw random aperture bearings of ammonites (to be used later for current direction):
        aperture_bearings <- rnorm(N_ammonites, mean = 0, sd = 13) %% 360
        
        # Make sure bearings span 360/0 so that students can learn problem with non-circular stats:
        while((any(aperture_bearings < 60) + any(aperture_bearings > 300)) < 2) aperture_bearings <- rnorm(N_ammonites, mean = 0, sd = 30) %% 360
        
        # Draw ammonite sizes (sorted so there will be a relationship with N chambers for regression):
        ammonite_sizes <- sort(rnorm(N_ammonites, mean = mean_ammonite_size, sd = 20))
        
        # Set N chambers based on ammonite sizes:
        N_chambers <- unlist(lapply(as.list(ammonite_sizes), function(x) sum(x > DividingWalls) + 1))
        
        # Make sure there are at least five different chamber numbers in the sample:
        while(length(unique(N_chambers)) < 4) {
          
          # Draw ammonite sizes (sorted so there will be a relationship with N chambers for regression):
          ammonite_sizes <- sort(rnorm(N_ammonites, mean = mean_ammonite_size, sd = 20))
          
          # Set N chambers based on ammonite sizes:
          N_chambers <- unlist(lapply(as.list(ammonite_sizes), function(x) sum(x > DividingWalls) + 1))
          
        }
        
        # Create empty plot of quadrat:
        plot(x = 0, y = 0, xlim = c(-50, 1050), ylim = c(-50, 1050), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n", asp = 1)
        
        # Plot smaller ten cm sub-quadrat lines:
        for(i in seq(from = 100, to = 900, length.out = 9)) {
          
            # Plot verticals:
            lines(x = c(i, i), y = c(0, 1000), col = "grey")
            
            # Plot horizontals:
            lines(x = c(0, 1000), y = c(i, i), col = "grey")
            
        }
        
        # Draw out borders of quadrat:
        lines(x = c(0, 0, 1000, 1000, 0), y = c(0, 1000, 1000, 0, 0), lty = 2)
        
        # Draw starting origin at random:
        origins <- cbind(runif(1, 25, 950), runif(1, 25, 950))
        
        # For each subsequent ammonite:
        for(i in 2:N_ammonites) {
            
            # Draw new origin:
            origins <- rbind(origins, cbind(runif(1, 25, 950), runif(1, 25, 950)))
            
            # Redraw origin if too close to other ammonites until safe separation found (will effect distances later):
            while(any(dist(origins) < (1.75 * mean_ammonite_size))) origins[i, ] <- cbind(runif(1, 25, 950), runif(1, 25, 950))
            
        }
        
        # For each ammonite:
        for(i in 1:N_ammonites) {
            
            # Get origin:
            origin <- origins[i, ]
            
            # Get x/y coordinates for ammonite:
            ammonite_i <- AmmonitePlotter(N_chambers = N_chambers[i], termination_bearing = aperture_bearings[i], r_max = ammonite_sizes[i], chirality = Chirality[i])
            
            # Plot ammonite spiral in quadrat:
            points(ammonite_i$spiral[, "x"] + origin[1], ammonite_i$spiral[, "y"] + origin[2], xlim = c(0, 4000), ylim = c(0, 4000), type = "l", lwd = 0.7)
            
            # Plot ammonite chamber walls in quadrat:
            for(j in 1:nrow(ammonite_i$wall_coordinates)) points(x = ammonite_i$wall_coordinates[j, c("x1", "x2")] + origin[1], y = ammonite_i$wall_coordinates[j, c("y1", "y2")] + origin[2], type = "l", lwd = 0.7)
            
        }
        
        # Add quadrat name to top left:
        text(x = -20, y = 1050, labels = c(paste("Quadrat ", X, Y, "\n(Lower bed)", sep = "")), pos = 4)
        
        # Add compass point (N) at top left:
        lines(x = c(-50, -50), y = c(850, 1000))
        lines(x = c(-60, -40), y = c(925, 925))
        polygon(x = c(-60, -50, -40, -60), y = c(960, 1000, 960, 960), col = "black")
        text(x = -50, y = 1040, labels = c("N"), pos = 1)
        
        # Add scale bar at bottom right:
        polygon(x = c(800, 800, 1000, 1000, 800), y = c(-60, -70, -70, -60, -60), col = "black")
        text(x = 1020, y = -40, labels = c("20 cm"), pos = 2)
        
        # Add ammonite numbers (to match to data):
        text(x = origins[order(origins[, 2], decreasing = TRUE), 1], y = origins[order(origins[, 2], decreasing = TRUE), 2], labels = as.character(1:20), cex = 3, col = rgb(1, 0, 0, 0.5))
        
        # Empty plot (second page for two-sided printing):
        plot(x = 1, y = 1, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(0, 100), ylim = c(0, 100))
        
        # Add horizontal lines for table:
        lapply(as.list(seq(from = 0, to = 100, length.out = 22)), function(x) lines(x = c(0, 100), y = c(x, x)))
        
        # Add vertical lines for table:
        lapply(as.list(seq(from = 0, to = 100, length.out = 5)), function(x) lines(x = c(x, x), y = c(0, 100)))
        
        # Add values for first column:
        for(i in 1:21) text(x = 12.5, y = seq(from = 0, to = 100, length.out = 22)[i] + 4, labels = rev(c("#", as.character(1:20)))[i], pos = 1, cex = 1.3)
        
        # Close plot/stop plotting to PDF:
        dev.off()
        
        # Create sorted data (numbered as in plot from top to bottom):
        ammonite_sizes <- ammonite_sizes[order(origins[, 2], decreasing = TRUE)]
        N_chambers <- N_chambers[order(origins[, 2], decreasing = TRUE)]
        aperture_bearings <- aperture_bearings[order(origins[, 2], decreasing = TRUE)]
        origins <- origins[order(origins[, 2], decreasing = TRUE), ]
        
        # Convert ammonite size to measured value:
        for(i in 1:20) ammonite_sizes[i] <- diff(range(AmmonitePlotter(N_chambers = N_chambers[i], termination_bearing = 0, r_max = ammonite_sizes[i])$spiral[, "y"]))
        
        # Round all values to a realistic level of precision:
        ammonite_sizes <- round(ammonite_sizes)
        aperture_bearings <- round(aperture_bearings)
        origins <- matrix(as.vector(round(origins)), ncol = 2, dimnames = list(1:20, c("x", "y")))
        
        # Place data for quadrat into single matrix:
        quadratdata <- cbind(1:20, origins, ammonite_sizes, N_chambers, aperture_bearings, Chirality)
        
        # Add column names to data:
        colnames(quadratdata) <- c("Ammonite_number", "X_coordinate", "Y_coordinate", "Diameter_mm", "N_chambers", "Aperture_bearing_degrees", "Chirality")
        
        # Write out as CSV:
        write.csv(quadratdata, file = paste("/Users/eargtl/Documents/Teaching/statsforgeologists/Ammonite_quadrats/Lower_Bed/Quadrat_", X, Y, ".csv", sep = ""), row.names = FALSE)
        
    }
    
}

setwd("/Users/eargtl/Documents/Teaching/statsforgeologists/Ammonite_quadrats/Lower_Bed")

CSVFiles <- list.files()[intersect(grep(".csv", list.files()), grep("Quadrat_", list.files()))]

AllQuadrats <- matrix(nrow = 0, ncol = 8, dimnames = list(c(), c("Quadrat_number", "Ammonite_number", "X_coordinate", "Y_coordinate", "Diameter_mm", "N_chambers", "Aperture_bearing_degrees", "Chirality")))

for(i in CSVFiles) {
  
  currentCSV <- read.csv(i)
  
  currentCSV <- cbind(rep(strsplit(i, "_|\\.")[[1]][2], 20), currentCSV)
  
  AllQuadrats <- rbind(AllQuadrats, currentCSV)
  
}

colnames(AllQuadrats)[1] <- "Quadrat"

write.csv(AllQuadrats, file = "/Users/eargtl/Documents/Teaching/statsforgeologists/Ammonite_quadrats/Lower_Bed/AllQuadrats.csv", row.names = FALSE)

# For the Xth quadrat (A-J) row:
for(X in LETTERS[1:10]) {
  
  # For the Yth quadrat (1-10) column:
  for(Y in 1:10) {
    
    # Start writing to file:
    pdf(paste("/Users/eargtl/Documents/Teaching/statsforgeologists/Ammonite_quadrats/Upper_Bed/", X, Y, ".pdf", sep = ""), height = 8.27, width = 11.69)
    
    # Set margins of plot to zero:
    par(mai = c(0, 0, 0, 0))
    
    # Set number of ammonites:
    N_ammonites <- 20
    
    # Set mean ammonite size (in mm):
    mean_ammonite_size <- 80

    # Draw random aperture bearings of ammonites (to be used later for current direction):
    aperture_bearings <- runif(N_ammonites, min = 0, max = 359)
    
    # Draw ammonite sizes (sorted so there will be a relationship with N chambers for regression):
    ammonite_sizes <- sort(rnorm(N_ammonites, mean = mean_ammonite_size, sd = 30))
    
    # Set N chambers based on ammonite sizes:
    N_chambers <- unlist(lapply(as.list(ammonite_sizes), function(x) sum(x > DividingWalls) + 1))
    
    # Make sure there are at least five different chamber numbers in the sample and that no ammonite is smaller than 10 mm:
    while(length(unique(N_chambers)) < 4 || min(ammonite_sizes) < 10) {
      
      # Draw ammonite sizes (sorted so there will be a relationship with N chambers for regression):
      ammonite_sizes <- sort(rnorm(N_ammonites, mean = mean_ammonite_size, sd = 20))
      
      # Set N chambers based on ammonite sizes:
      N_chambers <- unlist(lapply(as.list(ammonite_sizes), function(x) sum(x > DividingWalls) + 1))
      
    }
    
    # Set chirality of ammonites:
    Chirality <- sample(x = c("Sinistral", "Dextral"), size = N_ammonites, replace = TRUE)

    # Create empty plot of quadrat:
    plot(x = 0, y = 0, xlim = c(-50, 1050), ylim = c(-50, 1050), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n", asp = 1)
    
    # Plot smaller ten cm sub-quadrat lines:
    for(i in seq(from = 100, to = 900, length.out = 9)) {
      
      # Plot verticals:
      lines(x = c(i, i), y = c(0, 1000), col = "grey")
      
      # Plot horizontals:
      lines(x = c(0, 1000), y = c(i, i), col = "grey")
      
    }
    
    # Draw out borders of quadrat:
    lines(x = c(0, 0, 1000, 1000, 0), y = c(0, 1000, 1000, 0, 0), lty = 2)
    
    # Draw starting origin at random:
    origins <- cbind(runif(1, 25, 950), runif(1, 25, 950))
    
    # For each subsequent ammonite:
    for(i in 2:N_ammonites) {
      
      # Draw new origin:
      origins <- rbind(origins, cbind(runif(1, 25, 950), runif(1, 25, 950)))
      
    }
    
    # For each ammonite:
    for(i in 1:N_ammonites) {
      
      # Get origin:
      origin <- origins[i, ]
      
      # Get x/y coordinates for ammonite:
      ammonite_i <- AmmonitePlotter(N_chambers = N_chambers[i], termination_bearing = aperture_bearings[i], r_max = ammonite_sizes[i], chirality = Chirality[i])
      
      # Plot ammonite spiral in quadrat:
      points(ammonite_i$spiral[, "x"] + origin[1], ammonite_i$spiral[, "y"] + origin[2], xlim = c(0, 4000), ylim = c(0, 4000), type = "l", lwd = 0.7)
      
      # Plot ammonite chamber walls in quadrat:
      for(j in 1:nrow(ammonite_i$wall_coordinates)) points(x = ammonite_i$wall_coordinates[j, c("x1", "x2")] + origin[1], y = ammonite_i$wall_coordinates[j, c("y1", "y2")] + origin[2], type = "l", lwd = 0.7)
      
    }
    
    # Add quadrat name to top left:
    text(x = -20, y = 1050, labels = c(paste("Quadrat ", X, Y, "\n(Upper bed)", sep = "")), pos = 4)
    
    # Add compass point (N) at top left:
    lines(x = c(-50, -50), y = c(850, 1000))
    lines(x = c(-60, -40), y = c(925, 925))
    polygon(x = c(-60, -50, -40, -60), y = c(960, 1000, 960, 960), col = "black")
    text(x = -50, y = 1040, labels = c("N"), pos = 1)
    
    # Add scale bar at bottom right:
    polygon(x = c(800, 800, 1000, 1000, 800), y = c(-60, -70, -70, -60, -60), col = "black")
    text(x = 1020, y = -40, labels = c("20 cm"), pos = 2)
    
    # Add ammonite numbers (to match to data):
    text(x = origins[order(origins[, 2], decreasing = TRUE), 1], y = origins[order(origins[, 2], decreasing = TRUE), 2], labels = as.character(1:20), cex = 3, col = rgb(1, 0, 0, 0.5))
    
    # Empty plot (second page for two-sided printing):
    plot(x = 0, y = 0, xlim = c(-50, 1050), ylim = c(-50, 1050), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
    
    # Add customised code to plot:
    text(x = 0, y = 500, srt = 0, labels =
    paste(
    "# It is recommended that you begin by downloading the digital version of this quadrat, available here:\n",
    paste("browseURL(\"https://raw.githubusercontent.com/graemetlloyd/statsforgeologists/master/Ammonite_quadrats/Upper_Bed/", X, Y, ".pdf\")\n", sep = ""),
    "\n",
    "# This will make it easier for you to simply copy and paste the lines below instead of typing them out\n",
    "# yourself.\n",
    "#\n",
    "# For this assessment quadrat the measurements have already been done for you. DO NOT measure the data\n",
    "# yourself as you will be assessed on the measurements you are given. For this quadrat the data can be\n",
    "# imported into R with:\n",
    paste("AssessmentQuadratData = read.csv(\"https://raw.githubusercontent.com/graemetlloyd/statsforgeologists/master/Ammonite_quadrats/Upper_Bed/Quadrat_", X, Y, ".csv\")\n", sep = ""),
    "\n",
    "# The column names for the data indicate what is available:\n",
    "colnames(AssessmentQuadratData)\n",
    "\n",
    "# Thus, you can get the ammmonite number with:\n",
    "AssessmentQuadratData[, \"Ammonite_number\"]\n",
    "\n",
    "# The x coordinates with:\n",
    "AssessmentQuadratData[, \"X_coordinate\"]\n",
    "\n",
    "# The y coordinates with:\n",
    "AssessmentQuadratData[, \"Y_coordinate\"]\n",
    "\n",
    "# The diameters with:\n",
    "AssessmentQuadratData[, \"Diameter_mm\"]\n",
    "\n",
    "# The chamber count with:\n",
    "AssessmentQuadratData[, \"N_chambers\"]\n",
    "\n",
    "# And the aperture bearing with:\n",
    "AssessmentQuadratData[, \"Aperture_bearing_degrees\"]\n",
    "\n",
    "# NB: The chirality it is not required here and can be ignored.\n",
    sep = ""), family = "mono", pos = 4, cex = 0.7)
    
    # Close plot/stop plotting to PDF:
    dev.off()
    
    # Create sorted data (numbered as in plot from top to bottom):
    ammonite_sizes <- ammonite_sizes[order(origins[, 2], decreasing = TRUE)]
    N_chambers <- N_chambers[order(origins[, 2], decreasing = TRUE)]
    aperture_bearings <- aperture_bearings[order(origins[, 2], decreasing = TRUE)]
    origins <- origins[order(origins[, 2], decreasing = TRUE), ]
    
    # Convert ammonite size to measured value:
    for(i in 1:20) ammonite_sizes[i] <- diff(range(AmmonitePlotter(N_chambers = N_chambers[i], termination_bearing = 0, r_max = ammonite_sizes[i])$spiral[, "y"]))
    
    # Round all values to a realistic level of precision:
    ammonite_sizes <- round(ammonite_sizes)
    aperture_bearings <- round(aperture_bearings)
    origins <- matrix(as.vector(round(origins)), ncol = 2, dimnames = list(1:20, c("x", "y")))
    
    # Place data for quadrat into single matrix:
    quadratdata <- cbind(1:20, origins, ammonite_sizes, N_chambers, aperture_bearings, Chirality)
    
    # Add column names to data:
    colnames(quadratdata) <- c("Ammonite_number", "X_coordinate", "Y_coordinate", "Diameter_mm", "N_chambers", "Aperture_bearing_degrees", "Chirality")
    
    # Write out as CSV:
    write.csv(quadratdata, file = paste("/Users/eargtl/Documents/Teaching/statsforgeologists/Ammonite_quadrats/Upper_Bed/Quadrat_", X, Y, ".csv", sep = ""), row.names = FALSE)
    
  }
  
}

setwd("/Users/eargtl/Documents/Teaching/statsforgeologists/Ammonite_quadrats/Upper_Bed")

CSVFiles <- list.files()[intersect(grep(".csv", list.files()), grep("Quadrat_", list.files()))]

AllQuadrats <- matrix(nrow = 0, ncol = 8, dimnames = list(c(), c("Quadrat_number", "Ammonite_number", "X_coordinate", "Y_coordinate", "Diameter_mm", "N_chambers", "Aperture_bearing_degrees", "Chirality")))

for(i in CSVFiles) {
  
  currentCSV <- read.csv(i)
  
  currentCSV <- cbind(rep(strsplit(i, "_|\\.")[[1]][2], 20), currentCSV)
  
  AllQuadrats <- rbind(AllQuadrats, currentCSV)
  
}

colnames(AllQuadrats)[1] <- "Quadrat"

write.csv(AllQuadrats, file = "/Users/eargtl/Documents/Teaching/statsforgeologists/Ammonite_quadrats/Upper_Bed/AllQuadrats.csv", row.names = FALSE)

