
# ============================================================
# 04_Gruppenvergleiche.R
# Gruppenvergleich: Q5.1 (No vs any Yes) -> Q4.1.1_2
# ============================================================

# Daten/Variablen laden
source("01_statistische_kennwerte.R")

# -------------------------
# Gruppenvariable erstellen
# -------------------------
df_groups <- data[, c("Q5.1", "Q4.1.1_2")]
df_groups <- df_groups[complete.cases(df_groups), ]


# Gruppenvariable aus Q5.1
# 0 = No, 1/2/3 = Yes (any)
df_groups$group_family_entrepreneur <- ifelse(df_groups$Q5.1 == 0, "No", "Yes (any)")
df_groups$group_family_entrepreneur <- factor(df_groups$group_family_entrepreneur,
                                       levels = c("No", "Yes (any)"))

# Quick check
table(df_groups$group_family_entrepreneur)

# Deskriptive Kennwerte
tapply(df_groups$Q4.1.1_2, df_groups$group_family_entrepreneur, summary)
tapply(df_groups$Q4.1.1_2, df_groups$group_family_entrepreneur, sd)

# Visualisierung
boxplot(Q4.1.1_2 ~ group_family_entrepreneur,
        data = df_groups,
        main = "Q4.1.1_2 nach Unternehmerhintergrund der Eltern (Q5.1)",
        xlab = "Eltern selbständig?",
        ylab = "Q4.1.1_2",
        col = "lightblue",
        border = "darkblue",
        notch = FALSE)

# t-Test (Welch)
t_test_res <- t.test(Q4.1.1_2 ~ group_family_entrepreneur,
                     data = df_groups,
                     var.equal = FALSE)
t_test_res

# Kritischer t-Wert (α = 0.05, zweiseitig)
alpha <- 0.05
t_crit <- qt(1 - alpha/2, df = t_test_res$parameter)
t_crit


# Wilcoxon (Robustheits-Check)
wilcox_res <- wilcox.test(Q4.1.1_2 ~ group_family_entrepreneur,
                          data = df_groups,
                          exact = FALSE)
wilcox_res

# Effektgröße (Cohen's d, grob)
x <- df_groups$Q4.1.1_2[df_groups$group_family_entrepreneur == "No"]
y <- df_groups$Q4.1.1_2[df_groups$group_family_entrepreneur == "Yes (any)"]

cohens_d <- (mean(y) - mean(x)) / sqrt((sd(x)^2 + sd(y)^2) / 2)
cohens_d

