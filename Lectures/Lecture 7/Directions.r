
AmmoniteBearings <- c(
0, # 310 to 320
0, # 320 to 330
0, # 330 to 340
0, # 340 to 350
0, # 350 to 360
0, # 0 to 10
0, # 10 to 20
0, # 20 to 30
0, # 30 to 40
0  # 40 to 50
)

barplot(AmmoniteBearings, names.arg = c("310-320", "320-330", "330-340", "340-350", "350-360", "0-10", "10-20", "20-30", "30-40", "40-50"), border = 0, space = 0, col = "grey", ylab = "Frequency")
