###############################################################################
#  OE2 - MODELADO PREDICTIVO · BLOQUE 4
#  Evaluación sobre el conjunto de validación (semanas 43-52)
#  Métricas: MAE, RMSE, WAPE  ·  global y por clase ABC
###############################################################################
set.seed(123)
library(rpart)

# Retomar matriz y modelos (de bloques previos o desde disco)
if (!exists("dm")) {
  dm <- read.csv("matriz_modelado_OE2.csv", encoding="UTF-8", stringsAsFactors=FALSE)
  dm$familia <- factor(dm$familia, levels=c("FER","EPP","HER"))
}
valid <- dm[dm$conjunto=="validacion", ]
if (!exists("mlr"))   mlr   <- readRDS("mlr_OE2.rds")
if (!exists("arbol")) arbol <- readRDS("cart_OE2.rds")

## ---------------------------------------------------------------------------
## 1. PREDICCIONES SOBRE VALIDACIÓN (datos no vistos por los modelos)
## ---------------------------------------------------------------------------
y      <- valid$demanda_semanal
pred_mlr  <- predict(mlr,   newdata = valid)
pred_cart <- predict(arbol, newdata = valid)

# La demanda no puede ser negativa: se trunca en 0 (regla operativa)
pred_mlr  <- pmax(0, pred_mlr)
pred_cart <- pmax(0, pred_cart)

## ---------------------------------------------------------------------------
## 2. FUNCIONES DE MÉTRICAS
##    MAE  = media |y - yhat|
##    RMSE = raíz de la media (y - yhat)^2
##    WAPE = suma|y - yhat| / suma(y)   (robusta a ceros; en %)
## ---------------------------------------------------------------------------
mae  <- function(y, p) mean(abs(y - p))
rmse <- function(y, p) sqrt(mean((y - p)^2))
wape <- function(y, p) 100 * sum(abs(y - p)) / sum(y)

metricas <- function(y, p) c(MAE = mae(y,p), RMSE = rmse(y,p), WAPE = wape(y,p))

## ---------------------------------------------------------------------------
## 3. MÉTRICAS GLOBALES
## ---------------------------------------------------------------------------
tab_glob <- rbind(MLR  = metricas(y, pred_mlr),
                  CART = metricas(y, pred_cart))
cat("=== Métricas globales (validación, n =", length(y), ") ===\n")
print(round(tab_glob, 3))

## ---------------------------------------------------------------------------
## 4. MÉTRICAS POR CLASE ABC
## ---------------------------------------------------------------------------
cat("\n=== Métricas por clase ABC ===\n")
for (cl in c("A","B","C")) {
  idx <- valid$clasificacion_ABC == cl
  tb <- rbind(MLR  = metricas(y[idx], pred_mlr[idx]),
              CART = metricas(y[idx], pred_cart[idx]))
  cat(sprintf("\n-- Clase %s (n = %d) --\n", cl, sum(idx)))
  print(round(tb, 3))
}

## ---------------------------------------------------------------------------
## 5. GUARDAR PREDICCIONES PARA LOS BLOQUES 5 Y 6
## ---------------------------------------------------------------------------
res <- data.frame(SKU=valid$SKU, semana=valid$semana,
                  clasificacion_ABC=valid$clasificacion_ABC,
                  y=y, pred_mlr=pred_mlr, pred_cart=pred_cart)
write.csv(res, "predicciones_OE2.csv", row.names=FALSE, fileEncoding="UTF-8")
cat("\nPredicciones guardadas en 'predicciones_OE2.csv'.\n")
