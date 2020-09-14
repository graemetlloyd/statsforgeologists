TTestVisualisation <- function(x, Alpha, Type, H0Colour = "Grey", H1Colour = "Red", PlotRange, xlab) {
  
  # Get sample size from size of x:
  SampleSize <- length(x)
  
  # Place plot range on t-score scale:
  PlotRange <- (PlotRange - mean(x)) / (sd(x) / sqrt(SampleSize))
  
  # Set up starting x values (i.e., starting t-scores, mean = 0):
  RawXValues <- seq(from = PlotRange[1], to = PlotRange[2], length.out = 1000)
  
  # Turn into actual x balues using input data to shift back to observed postion:
  XValues <- (RawXValues * (sd(x) / sqrt(SampleSize))) + mean(x)
  
  # Get the critical values for a two-tail test:
  if(Type == "TwoTail") TwoTail <- (qt(p = c(Alpha / 2, 1 - (Alpha / 2)), df = SampleSize - 1) * (sd(x) / sqrt(SampleSize))) + mean(x)
  
  # If doing a one-tailed test:
  if(Type == "LeftTail" || Type == "RightTail") LeftTail <- RightTail <- (qt(p = c(Alpha, 1 - Alpha), df = SampleSize - 1) * (sd(x) / sqrt(SampleSize))) + mean(x)
  
  # Get T-score values for plotting:
  TValues <- dt(RawXValues, df = SampleSize - 1)
  
  # Set two-tail title text:
  if(Type == "TwoTail") TitleText <- paste("N = ", SampleSize, "; alpha = ", Alpha, "; two-tailed", sep = "")
  
  # Set left-tail title text:
  if(Type == "LeftTail") TitleText <- paste("N = ", SampleSize, "; alpha = ", Alpha, "; left-tailed", sep = "")
  
  # Set right-tail title text:
  if(Type == "RightTail") TitleText <- paste("N = ", SampleSize, "; alpha = ", Alpha, "; right-tailed", sep = "")
  
  # Make blank plotting space with correct limits:
  plot(x = XValues, y = TValues, type = "n", axes = TRUE, xlab = xlab, ylab = "", main = TitleText)
  
  # If two-tailed:
  if(Type == "TwoTail") {
    
    # Plot left-tail polygon:
    polygon(x = c(XValues[which(XValues < TwoTail[1])], XValues[which(XValues < TwoTail[1])][length(XValues[which(XValues < TwoTail[1])])], XValues[which(XValues < TwoTail[1])][1]), y = c(TValues[which(XValues < TwoTail[1])], 0, 0), border = NA, col = H1Colour)
    
    # Plot middle polygon:
    polygon(x = c(XValues[intersect(which(XValues >= TwoTail[1]), which(XValues < TwoTail[2]))[1]], XValues[intersect(which(XValues >= TwoTail[1]), which(XValues < TwoTail[2]))], XValues[intersect(which(XValues >= TwoTail[1]), which(XValues < TwoTail[2]))[length(intersect(which(XValues >= TwoTail[1]), which(XValues < TwoTail[2])))]]), y = c(0, TValues[intersect(which(XValues > TwoTail[1]), which(XValues < TwoTail[2]))], 0), border = NA, col = H0Colour)
    
    # Plot right-tail polygon:
    polygon(x = c(XValues[which(XValues >= TwoTail[2])], XValues[which(XValues >= TwoTail[2])][length(XValues[which(XValues >= TwoTail[2])])], XValues[which(XValues >= TwoTail[2])][1]), y = c(TValues[which(XValues >= TwoTail[2])], 0, 0), border = NA, col = H1Colour)
    
    # Plot lefthand critical-value line:
    lines(x = c(TwoTail[1], TwoTail[1]), y = c(0, max(TValues)), col = "blue")
    
    # Plot righthand critical-value line:
    lines(x = c(TwoTail[2], TwoTail[2]), y = c(0, max(TValues)), col = "blue")
    
    # Plot lefthand critical-value:
    text(x = TwoTail[1], y = max(TValues), labels = round(TwoTail[1], 2), col = "blue")
    
    # Plot righthand critical-value:
    text(x = TwoTail[2], y = max(TValues), labels = round(TwoTail[2], 2), col = "blue")
    
  }
  
  # If left-tailed:
  if(Type == "LeftTail") {
    
    # Plor left-tail:
    polygon(x = c(XValues[which(XValues < LeftTail[1])], XValues[which(XValues < LeftTail[1])][length(XValues[which(XValues < LeftTail[1])])], XValues[which(XValues < LeftTail)][1]), y = c(TValues[which(XValues < LeftTail[1])], 0, 0), border = NA, col = H1Colour)
    
    # Plot rest of distribution:
    polygon(x = c(XValues[which(XValues >= LeftTail[1])], XValues[which(XValues >= LeftTail[1])][length(XValues[which(XValues >= LeftTail[1])])], XValues[which(XValues >= LeftTail[1])][1]), y = c(TValues[which(XValues >= LeftTail[1])], 0, 0), border = NA, col = H0Colour)
    
    # Plot critical value line:
    lines(x = c(LeftTail[1], LeftTail[1]), y = c(0, max(TValues)), col = "blue")
    
    # Plot critical value:
    text(x = LeftTail[1], y = max(TValues), labels = round(LeftTail[1], 2), col = "blue")

  }
  
  # If right-tailed:
  if(Type == "RightTail") {
    
    # Plot mian distribution:
    polygon(x = c(XValues[which(XValues < RightTail[2])], XValues[which(XValues < RightTail[2])][length(XValues[which(XValues < RightTail[2])])], XValues[which(XValues < RightTail)][2]), y = c(TValues[which(XValues < RightTail[2])], 0, 0), border = NA, col = H0Colour)
    
    # Plot right-tail:
    polygon(x = c(XValues[which(XValues >= RightTail[2])], XValues[which(XValues >= RightTail[2])][length(XValues[which(XValues >= RightTail[2])])], XValues[which(XValues >= RightTail[2])][1]), y = c(TValues[which(XValues >= RightTail[2])], 0, 0), border = NA, col = H1Colour)
    
    # Plot critical value line:
    lines(x = c(RightTail[2], RightTail[2]), y = c(0, max(TValues)), col = "blue")
    
    # Plot critical value:
    text(x = RightTail[2], y = max(TValues), labels = round(RightTail[2], 2), col = "blue")

  }
  
}

FlagPlotter <- function(Country = "Italy", XLimits = c(0, 1.5), YLimits = c(0, 1)) {
  
  # Set defaults:
  Background <- LeftBand <- CentreBand <- RightBand <- TopBand <- MiddleBand <- BottomBand <- CentreCircle <- CentreWheel <- ThickCross <- ThinCross <- ThickSaltire <- ThinSaltire <- FALSE
  
  # If plotting France:
  if(Country == "France") {
    
    # Turn on vertical bands:
    LeftBand <- CentreBand <- RightBand <- TRUE
    
    # Set band colours:
    LeftBandColour <- "blue"
    CentreBandColour <- "white"
    RightBandColour <- "red"
    
  }
  
  # If plotting Germany:
  if(Country == "Germany") {
    
    # Turn on horizontal bands:
    TopBand <- MiddleBand <- BottomBand <- TRUE
    
    # Set band colours:
    TopBandColour <- "black"
    MiddleBandColour <- "red"
    BottomBandColour <- "yellow"
    
  }
  
  # If plotting India:
  if(Country == "India") {
    
    # Turn on horizontal bands:
    TopBand <- MiddleBand <- BottomBand <- CentreWheel <- TRUE
    
    # Set band colours:
    TopBandColour <- "orange"
    MiddleBandColour <- "white"
    BottomBandColour <- "green"
    
  }
  
  # If plotting Ireland:
  if(Country == "Ireland") {
    
    # Turn on vertical bands:
    LeftBand <- CentreBand <- RightBand <- TRUE
    
    # Set band colours:
    LeftBandColour <- "green"
    CentreBandColour <- "white"
    RightBandColour <- "orange"
    
  }
  
  # If plotting Italy:
  if(Country == "Italy") {
    
    # Turn on vertical bands:
    LeftBand <- CentreBand <- RightBand <- TRUE
    
    # Set band colours:
    LeftBandColour <- "green"
    CentreBandColour <- "white"
    RightBandColour <- "red"
    
  }
  
  # If plotting Japan:
  if(Country == "Japan") {
    
    # Turn on background and central cricle:
    Background <- CentreCircle <- TRUE
    
    # Set background and circle colours:
    BackgroundColour <- "white"
    CentreCircleColour <- "red"
    
  }
  
  # If plotting Netherlands:
  if(Country == "Netherlands") {
    
    # Turn on horizontal bands:
    TopBand <- MiddleBand <- BottomBand <- TRUE
    
    # Set band colours:
    TopBandColour <- "red"
    MiddleBandColour <- "white"
    BottomBandColour <- "blue"
    
  }
  
  # If plotting United Kingdoms:
  if(Country == "United Kingdom") {
    
    # Turn on background, cross, and saltire:
    Background <- ThickCross <- ThinCross <- ThickSaltire <- ThinSaltire <- TRUE
    
    # Set background, cross, and saltire colours:
    BackgroundColour <- "blue"
    ThickCrossColour <- "white"
    ThinCrossColour <- "red"
    ThickSaltireColour <- "white"
    ThinSaltireColour <- "red"
    
  }
  
  # If using background plot with chosen colour:
  if(Background) polygon(x = c(XLimits[1], XLimits[2], XLimits[2], XLimits[1]), y = c(YLimits[1], YLimits[1], YLimits[2], YLimits[2]), col = BackgroundColour, border = 0)
  
  # If using left band plot with chosen colour:
  if(LeftBand) polygon(x = c(XLimits[1], XLimits[1] + ((1 / 3) * abs(diff(XLimits))), XLimits[1] + ((1 / 3) * abs(diff(XLimits))), XLimits[1]), y = c(YLimits[1], YLimits[1], YLimits[2], YLimits[2]), col = LeftBandColour, border = 0)
  
  # If using centre band plot with chosen colour:
  if(CentreBand) polygon(x = c(XLimits[1] + ((1 / 3) * abs(diff(XLimits))), XLimits[1] + ((2 / 3) * abs(diff(XLimits))), XLimits[1] + ((2 / 3) * abs(diff(XLimits))), XLimits[1] + ((1 / 3) * abs(diff(XLimits)))), y = c(YLimits[1], YLimits[1], YLimits[2], YLimits[2]), col = CentreBandColour, border = 0)
  
  # If using right band plot with chosen colour:
  if(RightBand) polygon(x = c(XLimits[1] + ((2 / 3) * abs(diff(XLimits))), XLimits[1] + ((3 / 3) * abs(diff(XLimits))), XLimits[1] + ((3 / 3) * abs(diff(XLimits))), XLimits[1] + ((2 / 3) * abs(diff(XLimits)))), y = c(YLimits[1], YLimits[1], YLimits[2], YLimits[2]), col = RightBandColour, border = 0)
  
  # If using top band plot with chosen colour:
  if(TopBand) polygon(x = c(XLimits[1], XLimits[2], XLimits[2], XLimits[1]), y = c(YLimits[2], YLimits[2], YLimits[2] - ((1 / 3) * abs(diff(YLimits))), YLimits[2] - ((1 / 3) * abs(diff(YLimits)))), col = TopBandColour, border = 0)
  
  # If using middle band plot with chosen colour:
  if(MiddleBand) polygon(x = c(XLimits[1], XLimits[2], XLimits[2], XLimits[1]), y = c(YLimits[2] - ((1 / 3) * abs(diff(YLimits))), YLimits[2] - ((1 / 3) * abs(diff(YLimits))), YLimits[2] - ((2 / 3) * abs(diff(YLimits))), YLimits[2] - ((2 / 3) * abs(diff(YLimits)))), col = MiddleBandColour, border = 0)
  
  # If using bottom band plot with chosen colour:
  if(BottomBand) polygon(x = c(XLimits[1], XLimits[2], XLimits[2], XLimits[1]), y = c(YLimits[2] - ((2 / 3) * abs(diff(YLimits))), YLimits[2] - ((2 / 3) * abs(diff(YLimits))), YLimits[2] - ((3 / 3) * abs(diff(YLimits))), YLimits[2] - ((3 / 3) * abs(diff(YLimits)))), col = BottomBandColour, border = 0)
  
  # If using centre wheel plot with set colour:
  if(CentreWheel) {
    
    # Set wheel radius:
    radius <- abs(diff(YLimits)) * (1 / 9)
    
    # Set angles:
    angles <- seq(0, 2 * pi - (2 * pi / 1000), by = 2 * pi / 1000)
    
    # Plot wheel:
    polygon(x = cos(angles) * radius + mean(XLimits), y = sin(angles) * radius + mean(YLimits), border = "blue")
    
  }
  
  # If using centre circle plot with chosen colour:
  if(CentreCircle) {
    
    # Set circle radius:
    radius <- abs(diff(YLimits)) * (5 / 18)
    
    # Set angles:
    angles <- seq(0, 2 * pi - (2 * pi / 1000), by = 2 * pi / 1000)
    
    # Plot circle:
    polygon(x = cos(angles) * radius + mean(XLimits), y = sin(angles) * radius + mean(YLimits), col = CentreCircleColour, border = 0)
    
  }
  
  # If using thick saltire plot with chosen colour:
  if(ThickSaltire) {
    
    # Plot
    polygon(x = c(XLimits[1], XLimits[1] + ((55 / 60) * abs(diff(XLimits))), XLimits[2], XLimits[2], XLimits[1] + ((5 / 60) * abs(diff(XLimits))), XLimits[1]), y = c(YLimits[1] + ((55 / 60) * abs(diff(YLimits))), YLimits[1], YLimits[1], YLimits[1] + ((5 / 60) * abs(diff(YLimits))), YLimits[2], YLimits[2]), col = ThickSaltireColour, border = 0)
    polygon(x = c(XLimits[1], XLimits[1] + ((5 / 60) * abs(diff(XLimits))), XLimits[2], XLimits[2], XLimits[1] + ((55 / 60) * abs(diff(XLimits))), XLimits[1]), y = c(YLimits[1], YLimits[1], YLimits[1] + ((55 / 60) * abs(diff(YLimits))), YLimits[2], YLimits[2], YLimits[1] + ((5 / 60) * abs(diff(YLimits)))), col = ThickSaltireColour, border = 0)
    
  }
  
  # If using thin saltire plot with chosen colour:
  if(ThinSaltire) {
    
    polygon(x = c(XLimits[1], XLimits[1] + ((57 / 60) * abs(diff(XLimits))), XLimits[2], XLimits[2], XLimits[1] + ((3 / 60) * abs(diff(XLimits))), XLimits[1]), y = c(YLimits[1] + ((57 / 60) * abs(diff(YLimits))), YLimits[1], YLimits[1], YLimits[1] + ((3 / 60) * abs(diff(YLimits))), YLimits[2], YLimits[2]), col = ThinSaltireColour, border = 0)
    polygon(x = c(XLimits[1], XLimits[1] + ((3 / 60) * abs(diff(XLimits))), XLimits[2], XLimits[2], XLimits[1] + ((57 / 60) * abs(diff(XLimits))), XLimits[1]), y = c(YLimits[1], YLimits[1], YLimits[1] + ((57 / 60) * abs(diff(YLimits))), YLimits[2], YLimits[2], YLimits[1] + ((3 / 60) * abs(diff(YLimits)))), col = ThinSaltireColour, border = 0)
    
  }
  
  # If using thick cross plot with chosen colour:
  if(ThickCross) {
    
    polygon(x = c(XLimits[1] + ((25 / 60) * abs(diff(XLimits))), XLimits[1] + ((35 / 60) * abs(diff(XLimits))), XLimits[1] + ((35 / 60) * abs(diff(XLimits))), XLimits[1] + ((25 / 60) * abs(diff(XLimits)))), y = YLimits[c(1, 1, 2, 2)], col = ThickCrossColour, border = 0)
    polygon(x = XLimits[c(1, 2, 2, 1)], y = c(YLimits[1] + ((1 / 3) * abs(diff(YLimits))), YLimits[1] + ((1 / 3) * abs(diff(YLimits))), YLimits[1] + ((2 / 3) * abs(diff(YLimits))), YLimits[1] + ((2 / 3) * abs(diff(YLimits)))), col = ThickCrossColour, border = 0)
    
  }
  
  # If using thin cross plot with chosen colour:
  if(ThinCross) {
    
    polygon(x = c(XLimits[1] + ((27 / 60) * abs(diff(XLimits))), XLimits[1] + ((33 / 60) * abs(diff(XLimits))), XLimits[1] + ((33 / 60) * abs(diff(XLimits))), XLimits[1] + ((27 / 60) * abs(diff(XLimits)))), y = YLimits[c(1, 1, 2, 2)], col = ThinCrossColour, border = 0)
    polygon(x = XLimits[c(1, 2, 2, 1)], y = c(YLimits[1] + ((12 / 30) * abs(diff(YLimits))), YLimits[1] + ((12 / 30) * abs(diff(YLimits))), YLimits[1] + ((18 / 30) * abs(diff(YLimits))), YLimits[1] + ((18 / 30) * abs(diff(YLimits)))), col = ThinCrossColour, border = 0)
    
  }
  
  # Add black border to flag:
  polygon(x = XLimits[c(1, 2, 2, 1)], y = YLimits[c(1, 1, 2, 2)], col = rgb(1, 1, 1, 0), border = "black")
  
}
