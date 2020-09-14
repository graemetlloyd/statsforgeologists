# This script generates custom protractors that can be used for measuring aperture bearings for ammonites
# The resulting sheet should be printed (and duplicated if necessary), then copied onto acetate and each protratcor individually cut out

pdf("/Users/eargtl/Documents/Teaching/statsforgeologists/Ammonite_quadrats/Protractor.pdf", height = 7, width = 7)

par(mfrow = c(3, 3), mar = c(0.1, 0.1, 0.1, 0.1))

for(k in 1:9) {
  
  plot(x = 0, y = 0, xlim = c(-50, 1050), ylim = c(-50, 1050), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  
  origin <- 550
  
  points(x = c(500 - abs(cos((0:90) * (pi / 180)) * 500), rev(500 + abs(cos((0:90) * (pi / 180)) * 500))), y = c(origin + abs(sin((0:90) * (pi / 180)) * 500), rev(origin + abs(sin((0:90) * (pi / 180)) * 500))), type = "l")
  
  lines(x = c(500, 500), y = c(origin, origin + 500))
  lines(x = c(0, 1000), y = c(origin, origin))
  
  for(i in (1:8 * 10)) {
    
    adjacent1 <- abs(cos(i * (pi / 180)) * 500)
    adjacent2 <- abs(cos(i * (pi / 180)) * 475)
    adjacent3 <- abs(cos(i * (pi / 180)) * 440)
    adjacent4 <- abs(cos(i * (pi / 180)) * 400)
    
    opposite1 <- abs(sin(i * (pi / 180)) * 500)
    opposite2 <- abs(sin(i * (pi / 180)) * 475)
    opposite3 <- abs(sin(i * (pi / 180)) * 440)
    opposite4 <- abs(sin(i * (pi / 180)) * 400)
    
    lines(x = c(500, 500 - adjacent4), y = c(origin, origin + opposite4))
    
    lines(x = c(500, 500 + adjacent4), y = c(origin, origin + opposite4))
    
    lines(x = c(500 - adjacent2, 500 - adjacent1), y = c(origin + opposite2, origin + opposite1))
    
    lines(x = c(500 + adjacent2, 500 + adjacent1), y = c(origin + opposite2, origin + opposite1))
    
    text(x = 500 - adjacent3, y = origin + opposite3, labels = i + 270)
    text(x = 500 + adjacent3, y = origin + opposite3, labels = abs(90 - i))
    
  }
  
  for(i in seq(from = 5, to = 85, by = 10)) {
    
    adjacent1 <- abs(cos(i * (pi / 180)) * 500)
    adjacent2 <- abs(cos(i * (pi / 180)) * 490)
    adjacent3 <- abs(cos(i * (pi / 180)) * 400)
    
    opposite1 <- abs(sin(i * (pi / 180)) * 500)
    opposite2 <- abs(sin(i * (pi / 180)) * 490)
    opposite3 <- abs(sin(i * (pi / 180)) * 400)
    
    lines(x = c(500, 500 - adjacent3), y = c(origin, origin + opposite3), lwd = 0.7)
    
    lines(x = c(500, 500 + adjacent3), y = c(origin, origin + opposite3), lwd = 0.7)
    
    lines(x = c(500 - adjacent2, 500 - adjacent1), y = c(origin + opposite2, origin + opposite1))
    
    lines(x = c(500 + adjacent2, 500 + adjacent1), y = c(origin + opposite2, origin + opposite1))
    
  }
  
  origin <- -50
  
  points(x = c(500 - abs(cos((0:90) * (pi / 180)) * 500), rev(500 + abs(cos((0:90) * (pi / 180)) * 500))), y = c(origin + abs(sin((0:90) * (pi / 180)) * 500), rev(origin + abs(sin((0:90) * (pi / 180)) * 500))), type = "l")
  
  lines(x = c(500, 500), y = c(origin, origin + 500))
  lines(x = c(0, 1000), y = c(origin, origin))
  
  for(i in (1:8 * 10)) {
    
    adjacent1 <- abs(cos(i * (pi / 180)) * 500)
    adjacent2 <- abs(cos(i * (pi / 180)) * 475)
    adjacent3 <- abs(cos(i * (pi / 180)) * 440)
    adjacent4 <- abs(cos(i * (pi / 180)) * 400)
    
    opposite1 <- abs(sin(i * (pi / 180)) * 500)
    opposite2 <- abs(sin(i * (pi / 180)) * 475)
    opposite3 <- abs(sin(i * (pi / 180)) * 440)
    opposite4 <- abs(sin(i * (pi / 180)) * 400)
    
    lines(x = c(500, 500 - adjacent4), y = c(origin, origin + opposite4))
    
    lines(x = c(500, 500 + adjacent4), y = c(origin, origin + opposite4))
    
    lines(x = c(500 - adjacent2, 500 - adjacent1), y = c(origin + opposite2, origin + opposite1))
    
    lines(x = c(500 + adjacent2, 500 + adjacent1), y = c(origin + opposite2, origin + opposite1))
    
    text(x = 500 - adjacent3, y = origin + opposite3, labels = i + 270)
    text(x = 500 + adjacent3, y = origin + opposite3, labels = abs(90 - i))
    
  }
  
  for(i in seq(from = 5, to = 85, by = 10)) {
    
    adjacent1 <- abs(cos(i * (pi / 180)) * 500)
    adjacent2 <- abs(cos(i * (pi / 180)) * 490)
    adjacent3 <- abs(cos(i * (pi / 180)) * 400)
    
    opposite1 <- abs(sin(i * (pi / 180)) * 500)
    opposite2 <- abs(sin(i * (pi / 180)) * 490)
    opposite3 <- abs(sin(i * (pi / 180)) * 400)
    
    lines(x = c(500, 500 - adjacent3), y = c(origin, origin + opposite3), lwd = 0.7)
    
    lines(x = c(500, 500 + adjacent3), y = c(origin, origin + opposite3), lwd = 0.7)
    
    lines(x = c(500 - adjacent2, 500 - adjacent1), y = c(origin + opposite2, origin + opposite1))
    
    lines(x = c(500 + adjacent2, 500 + adjacent1), y = c(origin + opposite2, origin + opposite1))
    
  }

}

dev.off()
