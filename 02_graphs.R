# Plot-Layout: 1 Zeile, 2 Spalten
par(mfrow = c(1, 2))

# sicherstellen, dass die Variable numerisch ist
data$Q6.2_1 <- as.numeric(data$Q6.2_1)

# Histogramm mit Dichte anstatt Häufigkeit
hist(
  data$Q6.2_1,
  breaks = 7,                     # passend zur Skala 1–7
  freq = FALSE,                   # wichtig! -> damit die Dichtekurve passt
  col = "lightblue",
  border = "white",
  main = "Histogramm mit Dichtekurve: Reaktion der Familie",
  xlab = "Reaktion (1 = sehr negativ, 7 = sehr positiv)"
)

# Dichte berechnen (einmal berechnen für beide Geoms)
d <- density(na.omit(data$Q6.2_1))

# Dichtekurve einzeichnen
lines(d, col = "red", lwd = 2)

# Punkte auf der Dichtekurve einzeichnen
points(d$x, d$y, col = "red", pch = 16, cex = 0.6)

boxplot(
  data$Q6.2_1,
  main = "Boxplot: Reaktion der Familie auf eine mögliche Gründung",
  ylab = "Reaktion (1 = sehr negativ, 7 = sehr positiv)",
  col = "lightblue",
  border = "darkblue",
  notch = TRUE   # optional: zeigt Median-Konfidenzintervall
)

# Layout zurücksetzen
par(mfrow = c(1, 1))