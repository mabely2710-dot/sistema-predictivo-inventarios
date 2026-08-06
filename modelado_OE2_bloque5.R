###############################################################################
#  OE2 - MODELADO PREDICTIVO · BLOQUE 5
#  Comparación formal de modelos y selección
#  (No entrena; analiza las predicciones del Bloque 4)
###############################################################################
if (!exists("res")) res <- read.csv("predicciones_OE2.csv", encoding="UTF-8",
                                    stringsAsFactors=FALSE)
y <- res$y; pm <- res$pred_mlr; pc <- res$pred_cart

mae<-function(y,p)mean(abs(y-p)); rmse<-function(y,p)sqrt(mean((y-p)^2))
wape<-function(y,p)100*sum(abs(y-p))/sum(y)

## ---------------------------------------------------------------------------
## 1. TABLA COMPARATIVA CONSOLIDADA (global y por clase)
## ---------------------------------------------------------------------------
fila <- function(sub) {
  yy<-sub$y; a<-sub$pred_mlr; b<-sub$pred_cart
  data.frame(
    n=nrow(sub),
    MAE_MLR=mae(yy,a),  MAE_CART=mae(yy,b),
    RMSE_MLR=rmse(yy,a),RMSE_CART=rmse(yy,b),
    WAPE_MLR=wape(yy,a),WAPE_CART=wape(yy,b))
}
tab <- rbind(
  Global = fila(res),
  A = fila(res[res$clasificacion_ABC=="A",]),
  B = fila(res[res$clasificacion_ABC=="B",]),
  C = fila(res[res$clasificacion_ABC=="C",]))
cat("=== Tabla comparativa consolidada ===\n")
print(round(tab, 2))

## ---------------------------------------------------------------------------
## 2. MEJORA RELATIVA DEL CART FRENTE A LA MLR (%)  (+ = CART mejor)
## ---------------------------------------------------------------------------
mejora <- data.frame(
  MAE_mejora_pct  = 100*(tab$MAE_MLR  - tab$MAE_CART)  / tab$MAE_MLR,
  RMSE_mejora_pct = 100*(tab$RMSE_MLR - tab$RMSE_CART) / tab$RMSE_MLR,
  WAPE_mejora_pct = 100*(tab$WAPE_MLR - tab$WAPE_CART) / tab$WAPE_MLR)
rownames(mejora) <- rownames(tab)
cat("\n=== Mejora relativa del CART frente a la MLR (%) ===\n")
cat("    (positivo = el CART reduce el error; negativo = la MLR es mejor)\n")
print(round(mejora, 2))

## ---------------------------------------------------------------------------
## 3. PRUEBA FORMAL: ¿la diferencia de error absoluto es significativa?
##    Test de Wilcoxon pareado sobre |error| por observación (no asume normalidad)
## ---------------------------------------------------------------------------
ae_mlr  <- abs(y - pm); ae_cart <- abs(y - pc)
w <- wilcox.test(ae_mlr, ae_cart, paired = TRUE)
cat(sprintf("\n=== Prueba de Wilcoxon pareada sobre el error absoluto ===\n"))
cat(sprintf("Mediana |error| MLR: %.3f | CART: %.3f\n",
            median(ae_mlr), median(ae_cart)))
cat(sprintf("p-valor: %.4g  -> %s\n", w$p.value,
    ifelse(w$p.value<0.05,
      "diferencia estadísticamente significativa",
      "sin diferencia estadísticamente significativa")))

## ---------------------------------------------------------------------------
## 4. SÍNTESIS DE SELECCIÓN
## ---------------------------------------------------------------------------
gana_cart_glob <- tab["Global","MAE_CART"] < tab["Global","MAE_MLR"] &&
                  tab["Global","WAPE_CART"] < tab["Global","WAPE_MLR"]
cat("\n=== Síntesis ===\n")
cat(sprintf("Global: el %s presenta menor error promedio.\n",
            ifelse(gana_cart_glob,"CART","MLR")))
cat("RMSE por clase C:", ifelse(tab["C","RMSE_CART"]<tab["C","RMSE_MLR"],
    "CART menor","MLR menor (controla mejor errores extremos)"), "\n")
