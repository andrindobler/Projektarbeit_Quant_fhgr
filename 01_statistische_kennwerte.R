

# =========================
# Daten laden
# =========================

library(openxlsx)

file <- "GUESSS.xlsx"
stopifnot(file.exists(file))

guess <- read.xlsx(file, sheet = 1)


# Relevante Variablen auswählen
data <- guess[, c("Q6.2_1", "Q5.1")]

# Numerisch umwandeln
data$Q6.2_1 <- as.numeric(data$Q6.2_1)
data$Q5.1   <- as.numeric(data$Q5.1)

# Modus-Funktion
mode_value <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

### ---- Kennwerte für Q6.2_1 ---- ###
mean_Q6.2_1   <- mean(data$Q6.2_1, na.rm = TRUE)
median_Q6.2_1 <- median(data$Q6.2_1, na.rm = TRUE)
mode_Q6.2_1   <- mode_value(data$Q6.2_1)
var_Q6.2_1    <- var(data$Q6.2_1, na.rm = TRUE)
sd_Q6.2_1     <- sd(data$Q6.2_1, na.rm = TRUE)

### ---- Kennwerte für Q5.1 ---- ###
mean_Q5.1   <- mean(data$Q5.1, na.rm = TRUE)
median_Q5.1 <- median(data$Q5.1, na.rm = TRUE)
mode_Q5.1   <- mode_value(data$Q5.1)
var_Q5.1    <- var(data$Q5.1, na.rm = TRUE)
sd_Q5.1     <- sd(data$Q5.1, na.rm = TRUE)

# Ausgabe
mean_Q6.2_1; median_Q6.2_1; mode_Q6.2_1; var_Q6.2_1; sd_Q6.2_1
mean_Q5.1;   median_Q5.1;   mode_Q5.1;   var_Q5.1;   sd_Q5.1
