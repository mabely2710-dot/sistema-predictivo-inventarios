###############################################################################
#  OBJETIVO ESPECÍFICO 2 - MODELADO PREDICTIVO DE LA DEMANDA
#  BLOQUE 1: Construcción de variables predictoras y partición temporal
#
#  Materializa las decisiones de las secciones 4.3.1 a 4.3.3:
#   - Variable objetivo: demanda_semanal (semana t)
#   - Predictores: lag_1..lag_4, componente estacional DETERMINÍSTICA,
#                  media histórica expansiva, familia (factor)
#   - Horizonte one-step-ahead sin fuga de información
#   - Partición hold-out temporal 80/20 por SKU (train: sem <= 42; val: sem >= 43)
#
#  R base · reproducible · semilla fija
###############################################################################
set.seed(123)   # reproducibilidad (afecta solo pasos aleatorios posteriores)

## ---------------------------------------------------------------------------
## 0. PARÁMETROS
## ---------------------------------------------------------------------------
RUTA_CSV     <- "dataset_inventarios_ferreteria.csv"
N_LAGS       <- 4          # orden máximo de rezagos
FASE_PHI     <- 30         # fase estacional (semana de referencia)
SEMANA_CORTE <- 42         # <= corte: entrenamiento ; > corte: validación
# Amplitud estacional determinística por familia (del diseño del simulador)
EST_A <- c(FER = 0.15, EPP = 0.20, HER = 0.25)

## ---------------------------------------------------------------------------
## 1. LECTURA Y ORDENAMIENTO
## ---------------------------------------------------------------------------
d <- read.csv(RUTA_CSV, encoding = "UTF-8", stringsAsFactors = FALSE)
d <- d[order(d$SKU, d$semana), ]              # orden canónico imprescindible
rownames(d) <- NULL

## ---------------------------------------------------------------------------
## 2. CONSTRUCCIÓN DE VARIABLES PREDICTORAS  (por SKU, respetando la historia)
## ---------------------------------------------------------------------------
# 2.1 Rezagos de la demanda: lag_k(t) = demanda(t-k) dentro del mismo SKU
for (k in 1:N_LAGS) {
  d[[paste0("lag_", k)]] <- ave(
    d$demanda_semanal, d$SKU,
    FUN = function(x) c(rep(NA, k), head(x, -k))
  )
}

# 2.2 Media histórica EXPANSIVA: promedio de la demanda de las semanas
#     ESTRICTAMENTE anteriores a t (sin incluir t) dentro del SKU.
d$media_historica <- ave(
  d$demanda_semanal, d$SKU,
  FUN = function(x) {
    n <- length(x)
    mh <- rep(NA_real_, n)
    if (n > 1) for (i in 2:n) mh[i] <- mean(x[1:(i - 1)])
    mh
  }
)

# 2.3 Componente estacional DETERMINÍSTICA de la semana t (conocida ex ante).
#     NO se usa la columna 'factor_estacional' del dataset (contiene ruido
#     aleatorio no conocible de antemano -> sería fuga de información).
d$estacional_det <- 1 + EST_A[d$Codigo_familia] *
  sin(2 * pi * (d$semana - FASE_PHI) / 52)

# 2.4 Familia como factor (R genera las variables indicadoras en lm/rpart)
d$familia <- factor(d$Codigo_familia, levels = c("FER", "EPP", "HER"))

## ---------------------------------------------------------------------------
## 3. MATRIZ DE MODELADO: target + predictores, descartando filas incompletas
## ---------------------------------------------------------------------------
vars_modelo <- c("SKU", "semana", "clasificacion_ABC", "familia",
                 "demanda_semanal",                    # <- variable objetivo
                 paste0("lag_", 1:N_LAGS),
                 "media_historica", "estacional_det")
dm <- d[, vars_modelo]

# Se descartan las filas con algún predictor NA (primeras N_LAGS semanas por SKU)
completas <- complete.cases(dm[, c(paste0("lag_", 1:N_LAGS), "media_historica")])
dm <- dm[completas, ]
rownames(dm) <- NULL

## ---------------------------------------------------------------------------
## 4. PARTICIÓN HOLD-OUT TEMPORAL 80/20 POR SKU
## ---------------------------------------------------------------------------
dm$conjunto <- ifelse(dm$semana <= SEMANA_CORTE, "entrenamiento", "validacion")
train <- dm[dm$conjunto == "entrenamiento", ]
valid <- dm[dm$conjunto == "validacion", ]

## ---------------------------------------------------------------------------
## 5. RESUMEN DE CONTROL
## ---------------------------------------------------------------------------
cat("=== BLOQUE 1: construcción de variables y partición ===\n")
cat("Filas dataset original      :", nrow(d), "\n")
cat("Filas matriz de modelado    :", nrow(dm),
    " (esperado 120 x", 52 - N_LAGS, "=", 120 * (52 - N_LAGS), ")\n")
cat("  - entrenamiento           :", nrow(train),
    sprintf(" (%.1f%%)", 100 * nrow(train) / nrow(dm)), "\n")
cat("  - validación              :", nrow(valid),
    sprintf(" (%.1f%%)", 100 * nrow(valid) / nrow(dm)), "\n")
cat("Semanas por SKU en train    :", length(unique(train$semana)),
    "(", min(train$semana), "-", max(train$semana), ")\n")
cat("Semanas por SKU en validación:", length(unique(valid$semana)),
    "(", min(valid$semana), "-", max(valid$semana), ")\n\n")

cat("Vista de las primeras filas del primer SKU (semanas 5-8):\n")
ej <- dm[dm$SKU == dm$SKU[1], ][1:4,
        c("SKU","semana","demanda_semanal","lag_1","lag_2","lag_3","lag_4",
          "media_historica","estacional_det","conjunto")]
print(ej, row.names = FALSE)

# Guardar la matriz para el validador independiente (Bloque siguiente)
write.csv(dm, "matriz_modelado_OE2.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\nMatriz de modelado exportada a 'matriz_modelado_OE2.csv'.\n")
