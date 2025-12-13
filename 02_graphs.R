# =========================
# Pakete
# =========================
library(openxlsx)

# =========================
# Daten laden
# =========================
file <- "GUESSS.xlsx"
stopifnot(file.exists(file))

guess <- read.xlsx(file, sheet = 1)

# =========================
# Plot-Layout (Ränder)
# =========================
par(mar = c(6, 7, 4, 2) + 0.1)

# =========================
# Farben
# =========================

my_color <- "#6A5ACD"

# =========================
# Variablen definieren
# =========================
ordinal_vars <- c(
  "Q4.1.1_2",
  "Q6.2_1",
  "Q11.6.2_1",
  "Q11.6.2_2",
  "Q11.6.2_3"
)

nominal_var <- "Q5.1"

# =========================
# Typen umwandeln
# =========================
guess[ordinal_vars] <- lapply(guess[ordinal_vars], as.numeric)
guess[[nominal_var]] <- as.factor(guess[[nominal_var]])

# =========================
# X-Achsen-Beschriftung (ordinal)
# =========================
get_xlab_ordinal <- function(var_name) {
  if (var_name == "Q6.2_1") {
    "Ausprägung\n(1 = very negatively, 7 = very positively)"
  } else {
    "Ausprägung\n(1 = strongly disagree, 7 = strongly agree)"
  }
}

# =========================
# Ordinale Variablen
# Boxplots & Histogramme
# =========================
for (var in ordinal_vars) {
  
  x <- guess[[var]]
  xlab_text <- get_xlab_ordinal(var)
  
  ## Boxplot
  boxplot(
    x,
    main = paste("Boxplot von", var),
    ylab = xlab_text,
    ylim = c(1, 7.5),
    col = my_color
  )
  
  ## Histogramm vorbereiten
  h <- hist(
    x,
    breaks = seq(0.5, 7.5, by = 1),
    plot = FALSE
  )
  
  ## Histogramm (ohne automatische y-Achse)
  hist(
    x,
    breaks = seq(0.5, 7.5, by = 1),
    main = paste("Histogramm von", var),
    xlab = xlab_text,
    ylab = "Dichte",
    xlim = c(0.5, 7.5),
    ylim = c(0, max(h$density) * 1.25),
    col = my_color,
    border = "gray60",
    freq = FALSE,
    yaxt = "n"
  )
  
  ## Einheitliche y-Achse (2 Dezimalstellen)
  axis(
    side = 2,
    at = pretty(c(0, max(h$density) * 1.25)),
    labels = formatC(
      pretty(c(0, max(h$density) * 1.25)),
      format = "f",
      digits = 2
    )
  )
  
  ## Dichtewerte über Balken
  text(
    x = h$mids,
    y = h$density,
    label = ifelse(h$density > 0, round(h$density, 2), ""),
    pos = 3,
    cex = 0.8
  )
}

# =========================
# Nominale Variable Q5.1
# Säulen- & Balkendiagramm
# =========================

freq_Q5.1 <- table(guess[[nominal_var]])
max_freq  <- max(freq_Q5.1) * 1.3

labels_Q5 <- c(
  "No",
  "Yes, father",
  "Yes, mother",
  "Yes, both"
)

## Säulendiagramm (vertikal)
bp_vert <- barplot(
  freq_Q5.1,
  main = "Säulendiagramm von Q5.1",
  xlab = "Kategorie",
  ylab = "Häufigkeit",
  ylim = c(0, max_freq),
  col = my_color,
  names.arg = labels_Q5
)

text(
  x = bp_vert,
  y = freq_Q5.1,
  label = freq_Q5.1,
  pos = 3,
  cex = 0.9
)

# -------------------------
# Balkendiagramm Q5.1
# -------------------------

# Grössere linke Margin nur für diesen Plot
par(mar = c(5, 9, 4, 2) + 0.1)

freq_Q5.1 <- table(guess[[nominal_var]])
max_freq  <- max(freq_Q5.1) * 1.3

labels_Q5 <- c(
  "No",
  "Yes, father",
  "Yes, mother",
  "Yes, both"
)

bp_horiz <- barplot(
  freq_Q5.1,
  horiz = TRUE,
  main = "Balkendiagramm von Q5.1",
  xlab = "Häufigkeit",
  ylab = "",                 # ylab leer lassen!
  xlim = c(0, max_freq),
  col = my_color,
  names.arg = labels_Q5,
  las = 1,                   # horizontale Labels
  cex.names = 0.9            # gut lesbar, aber nicht dominant
)

# ylabel manuell und sauber links platzieren
mtext(
  "Kategorie",
  side = 2,
  line = 6,
  cex = 1
)

# Werte neben Balken
text(
  x = freq_Q5.1,
  y = bp_horiz,
  label = freq_Q5.1,
  pos = 4,
  cex = 0.9
)
