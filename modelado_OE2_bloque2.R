###############################################################################
#  OE2 - MODELADO PREDICTIVO · BLOQUE 2
#  Entrenamiento de la Regresión Lineal Múltiple (MLR) + diagnóstico
###############################################################################
set.seed(123)

if (!exists("dm")) {
  dm <- read.csv("matriz_modelado_OE2.csv", encoding="UTF-8", stringsAsFactors=FALSE)
  dm$familia <- factor(dm$familia, levels=c("FER","EPP","HER"))
}
train <- dm[dm$conjunto=="entrenamiento", ]
valid <- dm[dm$conjunto=="validacion", ]

# 1. Especificación: Modelo A sin familia, Modelo B con familia
f_base <- demanda_semanal ~ lag_1 + lag_2 + lag_3 + lag_4 +
                            media_historica + estacional_det
f_fam  <- update(f_base, . ~ . + familia)
mlr_A <- lm(f_base, data = train)
mlr_B <- lm(f_fam,  data = train)

# 2. ¿Aporta la familia? ANOVA de modelos anidados + AIC + R2 ajustado
cat("=== ¿La familia aporta? ===\n")
comp <- anova(mlr_A, mlr_B); print(comp)
cat(sprintf("AIC sin: %.1f | con: %.1f\n", AIC(mlr_A), AIC(mlr_B)))
cat(sprintf("R2aj sin: %.4f | con: %.4f\n",
    summary(mlr_A)$adj.r.squared, summary(mlr_B)$adj.r.squared))
p_fam <- comp$`Pr(>F)`[2]
usar_familia <- !is.na(p_fam) && p_fam < 0.05
mlr <- if (usar_familia) mlr_B else mlr_A
cat(sprintf("Decision: familia %s (p=%.4g)\n\n",
    ifelse(usar_familia,"SI aporta","NO aporta -> descartada"), p_fam))

# 3. Coeficientes del modelo final
cat("=== Coeficientes MLR final ===\n")
print(round(summary(mlr)$coefficients, 4))
cat(sprintf("\nR2=%.4f | R2aj=%.4f | sigma=%.3f\n",
    summary(mlr)$r.squared, summary(mlr)$adj.r.squared, summary(mlr)$sigma))

# 4. Diagnostico numerico (graficos completos en Bloque 6)
r <- residuals(mlr)
cat("\n=== Diagnostico de residuales ===\n")
cat(sprintf("Media residuales: %.4f (esperado ~0)\n", mean(r)))
dw <- sum(diff(r)^2) / sum(r^2)
cat(sprintf("Durbin-Watson: %.3f (~2 sin autocorrelacion)\n", dw))

saveRDS(mlr, "mlr_OE2.rds")
cat("\nModelo guardado en 'mlr_OE2.rds'.\n")
