# ============================================================
# Vorbereitung
# ============================================================

data$Q5.1   <- as.numeric(data$Q5.1)
data$Q6.2_1 <- as.numeric(data$Q6.2_1)

# ============================================================
# Funktion: Histogramm + Boxplot (voll kontrolliertes Layout)
# ============================================================

plot_single_variable <- function(
    x,
    main_title,
    xlab_hist_manual,
    xlab_long,
    ylab_manual
) {
  
  par(
    mfrow = c(1, 2),
    mar = c(4, 5, 4, 2),
    oma = c(4, 0, 6, 0)
  )
  
  # ----------------------------
  # Histogramm
  # ----------------------------
  hist(
    x,
    breaks = 7,
    freq = FALSE,
    col = "lightblue",
    border = "white",
    main = "Histogramm",
    xlab = "",
    cex.axis = 0.8
  )
  
  mtext(
    xlab_hist_manual,
    side = 1,
    line = 2,
    adj = 0
  )
  
  # ----------------------------
  # Boxplot
  # ----------------------------
  boxplot(
    x,
    main = "Boxplot",
    ylab = "",
    col = "lightblue",
    border = "darkblue",
    notch = FALSE,
    cex.axis = 0.8,
    las = 1
  )
  
  mtext(
    ylab_manual,
    side = 2,
    line = 4,
    cex = 0.9
  )
  
  # ----------------------------
  # Haupttitel
  # ----------------------------
  mtext(
    main_title,
    outer = TRUE,
    side = 3,
    line = 3,
    cex = 1.05,
    font = 2
  )
  
  # ----------------------------
  # Skalenhinweis
  # ----------------------------
  mtext(
    xlab_long,
    outer = TRUE,
    side = 1,
    line = 1,
    cex = 0.85
  )
  
  par(mfrow = c(1, 1))
}

# ============================================================
# Funktion: Säulendiagramm (nominal / ordinal)
# ============================================================

plot_bar_variable <- function(
    x,
    main_title,
    xlab,
    ylab,
    labels = NULL
) {
  
  freq <- table(x)
  
  par(mar = c(5, 5, 4, 2))
  
  barplot(
    freq,
    col = "lightblue",
    border = "white",
    main = main_title,
    xlab = xlab,
    ylab = ylab,
    names.arg = labels,
    cex.axis = 0.8,
    cex.names = 0.8
  )
}

# ============================================================
# Q5.1 – NOMINAL
# ============================================================

# Histogramm + Boxplot (bewusst methodisch problematisch, aber reflektiert)
plot_single_variable(
  x = na.omit(data$Q5.1),
  main_title =
    "Q5.1: Selbständigkeit der Eltern\n(nominale Variable – metrische Darstellung problematisch)",
  xlab_hist_manual = "Antwortkategorie",
  xlab_long =
    "Kodierung: 0 = No, 1 = Yes father, 2 = Yes mother, 3 = Yes both",
  ylab_manual =
    "Antwortkategorie\n(nominal, keine Rangordnung)"
)

# Säulendiagramm (korrekt für nominale Variable)
plot_bar_variable(
  x = na.omit(data$Q5.1),
  main_title = "Q5.1: Selbständigkeit der Eltern (Säulendiagramm)",
  xlab = "Antwortkategorie",
  ylab = "Häufigkeit",
  labels = c("No", "Yes father", "Yes mother", "Yes both")
)

# ============================================================
# Q6.2_1 – ORDINAL
# ============================================================

# Histogramm + Boxplot
plot_single_variable(
  x = na.omit(data$Q6.2_1),
  main_title =
    "Q6.2_1: Reaktion der Familie\n(ordinal skalierte Bewertung)",
  xlab_hist_manual = "Reaktion",
  xlab_long =
    "Skala: 1 = sehr negativ, 7 = sehr positiv",
  ylab_manual =
    "Reaktion\n(1 = sehr negativ, 7 = sehr positiv)"
)

# Säulendiagramm (ordinal → Verteilung)
plot_bar_variable(
  x = na.omit(data$Q6.2_1),
  main_title = "Q6.2_1: Reaktion der Familie (Säulendiagramm)",
  xlab = "Reaktion",
  ylab = "Häufigkeit",
  labels = 1:7
)
