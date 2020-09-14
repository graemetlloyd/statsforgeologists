EtnaEruptionYears <- c(1169, 1169, 1329, 1329, 1536, 1669, 1669,
  1693, 1832, 1843, 1868, 1928, 1929, 1979, 1981, 1984, 1985,
  1987, 1991)

cor.test(x = 1:18, y = rank(diff(EtnaEruptionYears)), type = "spearman")$estimate
