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
# Relevante Variablen auswählen
# =========================
data <- guess[, c(
  "Q4.1.1_2",     # abhängige Variable (ordinal)
  "Q6.2_1",       # unabhängige Variable (ordinal)
  "Q5.1",         # unabhängige Variable (nominal)
  "Q11.6.2_1",    # unabhängige Variable (ordinal)
  "Q11.6.2_2",    # unabhängige Variable (ordinal)
  "Q11.6.2_3"     # unabhängige Variable (ordinal)
)]

# =========================
# Numerisch umwandeln
# =========================
data[] <- lapply(data, as.numeric)

# =========================
# Modus-Funktion (alle gleich häufigsten Werte)
# =========================
mode_value <- function(x) {
  x <- x[!is.na(x)]
  tab <- table(x)
  as.numeric(names(tab)[tab == max(tab)])
}

# =========================
# Kennwerte berechnen
# =========================

## Q4.1.1_2
mean_Q4.1.1_2   <- mean(data$Q4.1.1_2, na.rm = TRUE)
median_Q4.1.1_2 <- median(data$Q4.1.1_2, na.rm = TRUE)
mode_Q4.1.1_2   <- mode_value(data$Q4.1.1_2)
var_Q4.1.1_2    <- var(data$Q4.1.1_2, na.rm = TRUE)
sd_Q4.1.1_2     <- sd(data$Q4.1.1_2, na.rm = TRUE)

## Q6.2_1
mean_Q6.2_1   <- mean(data$Q6.2_1, na.rm = TRUE)
median_Q6.2_1 <- median(data$Q6.2_1, na.rm = TRUE)
mode_Q6.2_1   <- mode_value(data$Q6.2_1)
var_Q6.2_1    <- var(data$Q6.2_1, na.rm = TRUE)
sd_Q6.2_1     <- sd(data$Q6.2_1, na.rm = TRUE)

## Q5.1 (nominal → nur Modus)
mode_Q5.1 <- mode_value(data$Q5.1)

## Q11.6.2_1
mean_Q11.6.2_1   <- mean(data$Q11.6.2_1, na.rm = TRUE)
median_Q11.6.2_1 <- median(data$Q11.6.2_1, na.rm = TRUE)
mode_Q11.6.2_1   <- mode_value(data$Q11.6.2_1)
var_Q11.6.2_1    <- var(data$Q11.6.2_1, na.rm = TRUE)
sd_Q11.6.2_1     <- sd(data$Q11.6.2_1, na.rm = TRUE)

## Q11.6.2_2
mean_Q11.6.2_2   <- mean(data$Q11.6.2_2, na.rm = TRUE)
median_Q11.6.2_2 <- median(data$Q11.6.2_2, na.rm = TRUE)
mode_Q11.6.2_2   <- mode_value(data$Q11.6.2_2)
var_Q11.6.2_2    <- var(data$Q11.6.2_2, na.rm = TRUE)
sd_Q11.6.2_2     <- sd(data$Q11.6.2_2, na.rm = TRUE)

## Q11.6.2_3
mean_Q11.6.2_3   <- mean(data$Q11.6.2_3, na.rm = TRUE)
median_Q11.6.2_3 <- median(data$Q11.6.2_3, na.rm = TRUE)
mode_Q11.6.2_3   <- mode_value(data$Q11.6.2_3)
var_Q11.6.2_3    <- var(data$Q11.6.2_3, na.rm = TRUE)
sd_Q11.6.2_3     <- sd(data$Q11.6.2_3, na.rm = TRUE)

# =========================
# Ausgabe
# =========================

mean_Q4.1.1_2; median_Q4.1.1_2; mode_Q4.1.1_2; var_Q4.1.1_2; sd_Q4.1.1_2
mean_Q6.2_1;   median_Q6.2_1;   mode_Q6.2_1;   var_Q6.2_1;   sd_Q6.2_1
mode_Q5.1
mean_Q11.6.2_1; median_Q11.6.2_1; mode_Q11.6.2_1; var_Q11.6.2_1; sd_Q11.6.2_1
mean_Q11.6.2_2; median_Q11.6.2_2; mode_Q11.6.2_2; var_Q11.6.2_2; sd_Q11.6.2_2
mean_Q11.6.2_3; median_Q11.6.2_3; mode_Q11.6.2_3; var_Q11.6.2_3; sd_Q11.6.2_3