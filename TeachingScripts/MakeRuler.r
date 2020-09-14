# This script generates custom ruers 9scaled to the printed size of the ammonite quadrats) that can be used for measuring ammonite diameters
# The resulting sheet should be printed (and duplicated if necessary), then copied onto acetate and each ruler individually cut out

# Begin plotting of rulers:
pdf("/Users/eargtl/Documents/Teaching/statsforgeologists/Ammonite_quadrats/Rulers.pdf", height = 8, width = 8)

# Will need 3 of them (3 * 33, i.e., 99):
for(k in 1:3) {
  
  # Set margins of plot to zero:
  par(mai = c(0, 0, 0, 0))
  
  # Create empty plot of quadrat:
  plot(x = 0, y = 0, xlim = c(-50, 1050), ylim = c(-50, 1050), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  
  # Ruler plotting function:
  Rulers <- function(originx, originy) {
    
    # Draw main line of ruler:
    lines(x = c(originx, originx + 300), y = c(originy, originy))
    
    # Plot large ticks (every 100 mm):
    for(i in seq(from = originx, to = originx + 300, length.out = 4)) lines(x = c(i, i), y = c(originy - 15, originy))
    
    # Plot medium ticks (every 10 mm)
    for(i in seq(from = originx, to = originx + 300, length.out = 31)) lines(x = c(i, i), y = c(originy - 12, originy))
    
    # Plot small ticks (every 5mm):
    for(i in seq(from = originx + 5, to = originx + 295, length.out = 30)) lines(x = c(i, i), y = c(originy - 9, originy))
    
    # Plot 0 to 300 numbers every 100 mm:
    text(x = originx, y = originy - 15, labels = c("0"), pos = 1)
    text(x = originx + 100, y = originy - 15, labels = c("100"), pos = 1)
    text(x = originx + 200, y = originy - 15, labels = c("200"), pos = 1)
    text(x = originx + 300, y = originy - 15, labels = c("300"), pos = 1)
    
  }
  
  # For each row:
  for(i in c(-50, 333, 750)) {
    
    # For each column:
    for(j in seq(from = 1000, to = 0, length.out = 11)) Rulers(i, j)
    
  }
  
  # Empty plot (second page for two-sided printing):
  plot(x = 1, y = 1, type = "n", axes = FALSE, xlab = "", ylab = "")
  
}

# Finish plotting to PDF:
dev.off()
