# =====================================================
# Plot-Funktionen für R Markdown
# =====================================================

# -------------------------
# X-Achsen-Beschriftung
# -------------------------
get_xlab_ordinal <- function(var_name) {
  if (var_name == "Q6.2_1") {
    "Ausprägung\n(1 = very negatively, 7 = very positively)"
  } else {
    "Ausprägung\n(1 = strongly disagree, 7 = strongly agree)"
  }
}

# -------------------------
# Boxplot (ordinal, einzeln)
# -------------------------
plot_box_ordinal <- function(data, var, color = "#6A5ACD") {
  
  x <- data[[var]]
  xlab_text <- get_xlab_ordinal(var)
  
  par(mar = c(6, 7, 4, 2) + 0.1)
  
  boxplot(
    x,
    main = paste("Boxplot von", var),
    ylab = xlab_text,
    ylim = c(1, 7.5),
    col = color
  )
}

# -------------------------
# Histogramm (ordinal, einzeln)
# -------------------------
plot_hist_ordinal <- function(data, var, color = "#6A5ACD") {
  
  x <- data[[var]]
  xlab_text <- get_xlab_ordinal(var)
  
  par(mar = c(6, 7, 4, 2) + 0.1)
  
  h <- hist(
    x,
    breaks = seq(0.5, 7.5, by = 1),
    plot = FALSE
  )
  
  hist(
    x,
    breaks = seq(0.5, 7.5, by = 1),
    main = paste("Histogramm von", var),
    xlab = xlab_text,
    ylab = "Dichte",
    xlim = c(0.5, 7.5),
    ylim = c(0, max(h$density) * 1.25),
    col = color,
    border = "gray60",
    freq = FALSE,
    yaxt = "n"
  )
  
  axis(
    side = 2,
    at = pretty(c(0, max(h$density) * 1.25)),
    labels = formatC(
      pretty(c(0, max(h$density) * 1.25)),
      format = "f",
      digits = 2
    )
  )
  
  text(
    x = h$mids,
    y = h$density,
    label = ifelse(h$density > 0, round(h$density, 2), ""),
    pos = 3,
    cex = 0.8
  )
}

# -------------------------
# Nominale Variable Q5.1
# -------------------------
plot_Q5_1 <- function(data, horiz = FALSE, color = "#6A5ACD") {
  
  freq <- table(data$Q5.1)
  max_freq <- max(freq) * 1.3
  
  labels <- c(
    "No",
    "Yes, father",
    "Yes, mother",
    "Yes, both"
  )
  
  if (!horiz) {
    
    par(mar = c(6, 7, 4, 2) + 0.1)
    
    bp <- barplot(
      freq,
      main = "Säulendiagramm von Q5.1",
      ylab = "Häufigkeit",
      ylim = c(0, max_freq),
      col = color,
      names.arg = labels
    )
    
    text(bp, freq, freq, pos = 3)
    
  } else {
    
    par(mar = c(5, 9, 4, 2) + 0.1)
    
    bp <- barplot(
      freq,
      horiz = TRUE,
      main = "Balkendiagramm von Q5.1",
      xlab = "Häufigkeit",
      col = color,
      names.arg = labels,
      las = 1,
      cex.names = 0.9
    )
    
    mtext("Kategorie", side = 2, line = 6)
    text(freq, bp, freq, pos = 4)
  }
}