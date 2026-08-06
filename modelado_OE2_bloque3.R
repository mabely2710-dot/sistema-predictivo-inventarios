###############################################################################
#  OE2 - MODELADO PREDICTIVO · BLOQUE 3
#  Entrenamiento del Árbol de Decisión para Regresión (CART) + poda
###############################################################################
set.seed(123)
library(rpart)

if (!exists("dm")) {
  dm <- read.csv("matriz_modelado_OE2.csv", encoding="UTF-8", stringsAsFactors=FALSE)
  dm$familia <- factor(dm$familia, levels=c("FER","EPP","HER"))
}
train <- dm[dm$conjunto=="entrenamiento", ]
valid <- dm[dm$conjunto=="validacion", ]

# Mismas variables predictoras que la MLR final (sin familia) -> comparación justa
f_cart <- demanda_semanal ~ lag_1 + lag_2 + lag_3 + lag_4 +
                            media_historica + estacional_det

## ---------------------------------------------------------------------------
## 1. ÁRBOL COMPLETO (cp bajo para dejarlo crecer y luego podar)
## ---------------------------------------------------------------------------
set.seed(123)   # la validación cruzada interna de rpart usa aleatoriedad
arbol_full <- rpart(f_cart, data = train, method = "anova",
                    control = rpart.control(cp = 0.0001, minsplit = 20, xval = 10))

## ---------------------------------------------------------------------------
## 2. SELECCIÓN DEL cp ÓPTIMO POR VALIDACIÓN CRUZADA (regla del mínimo xerror)
## ---------------------------------------------------------------------------
cptab <- arbol_full$cptable
cat("=== Tabla de complejidad (primeras y últimas filas) ===\n")
print(round(head(cptab, 6), 5))
cat("...\n")
print(round(tail(cptab, 3), 5))

i_min   <- which.min(cptab[, "xerror"])
cp_opt  <- cptab[i_min, "CP"]
cat(sprintf("\ncp óptimo (mín. xerror): %.6f  | nsplit = %d | xerror = %.4f\n",
            cp_opt, cptab[i_min,"nsplit"], cptab[i_min,"xerror"]))

## ---------------------------------------------------------------------------
## 3. PODA CON EL cp ÓPTIMO
## ---------------------------------------------------------------------------
arbol <- prune(arbol_full, cp = cp_opt)
cat(sprintf("\nNº de hojas del árbol podado: %d\n", sum(arbol$frame$var == "<leaf>")))

## ---------------------------------------------------------------------------
## 4. IMPORTANCIA DE VARIABLES
## ---------------------------------------------------------------------------
cat("\n=== Importancia relativa de variables (%) ===\n")
imp <- arbol$variable.importance
if (!is.null(imp)) print(round(100 * imp / sum(imp), 1))

saveRDS(arbol, "cart_OE2.rds")
cat("\nÁrbol podado guardado en 'cart_OE2.rds'.\n")
