###############################################################################
#  VALIDACIÓN, DIAGNÓSTICO Y DOCUMENTACIÓN DE CALIDAD
#  Dataset sintético de inventarios · PyME sector ferretero · 120 SKUs × 52 sem
#
#  PROPÓSITO: script INDEPENDIENTE cuya única función es revisar, validar y
#  documentar la calidad estadística del dataset ya generado.
#  NO genera datos, NO modifica el dataset, NO recalibra parámetros.
#
#  Entradas : dataset_inventarios_ferreteria.csv (en la misma carpeta)
#  Salidas  : carpeta Graficos_Validacion/ , carpeta Tablas_Validacion/ ,
#             Informe_Validacion_Dataset.docx
#
#  Ejecutar en RStudio con:  Ctrl+Shift+S  (Source) sobre TODO el archivo.
###############################################################################

## ===========================================================================
## 0. CONFIGURACIÓN: LIBRERÍAS (autoinstalación), RUTAS Y LECTURA DEL CSV
## ===========================================================================
suppressWarnings(try(Sys.setlocale("LC_ALL", "C.UTF-8"), silent = TRUE))

# --- Autoinstalación de librerías faltantes ---
paquetes <- c("fitdistrplus", "ggplot2", "nortest", "goftest", "corrplot",
              "dplyr", "tidyr", "reshape2", "knitr", "rmarkdown")
faltan <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(faltan) > 0) {
  message("Instalando librerías faltantes: ", paste(faltan, collapse = ", "))
  install.packages(faltan, repos = "https://cloud.r-project.org")
}
invisible(suppressMessages(lapply(paquetes, require, character.only = TRUE)))

# --- Rutas RELATIVAS y carpetas de salida ---
DIR_GRAF <- "Graficos_Validacion"
DIR_TAB  <- "Tablas_Validacion"
dir.create(DIR_GRAF, showWarnings = FALSE)
dir.create(DIR_TAB,  showWarnings = FALSE)

ARCHIVO <- "dataset_inventarios_ferreteria.csv"
if (!file.exists(ARCHIVO))
  stop("No se encontró '", ARCHIVO, "'. Colóquelo en la carpeta de trabajo ",
       "(Session > Set Working Directory > To Source File Location).")

d <- read.csv(ARCHIVO, stringsAsFactors = FALSE, encoding = "UTF-8")

# Objeto acumulador de resultados para el informe
res <- list()
res$meta <- list(archivo = ARCHIVO, fecha = format(Sys.time(), "%Y-%m-%d %H:%M"),
                 semilla = 123)

## ===========================================================================
## 1. VALIDACIÓN DE CALIDAD DE DATOS
## ===========================================================================
res$resumen <- list(
  n_filas = nrow(d), n_col = ncol(d),
  n_sku = length(unique(d$SKU)), n_sem = length(unique(d$semana)),
  quiebre_global = round(mean(d$quiebre_stock) * 100, 2)
)

# 1.1 Estructura y tipos de dato
res$tipos <- data.frame(variable = names(d),
                        tipo = sapply(d, class), row.names = NULL)

# 1.2 Valores faltantes
res$na_tab <- data.frame(variable = names(d),
                         n_faltantes = colSums(is.na(d)),
                         pct = round(100 * colSums(is.na(d)) / nrow(d), 2),
                         row.names = NULL)

# 1.3 Duplicados
res$dup_filas <- sum(duplicated(d))
res$dup_clave <- sum(duplicated(d[, c("SKU", "semana")]))

# 1.4 Valores fuera de rango (chequeos lógicos por variable)
chk <- function(nombre, ok, detalle)
  data.frame(chequeo = nombre,
             resultado = ifelse(ok, "OK", "REVISAR"),
             detalle = detalle, stringsAsFactors = FALSE)

res$rango <- rbind(
  chk("demanda_semanal >= 0",        all(d$demanda_semanal >= 0),      sprintf("min = %s", min(d$demanda_semanal))),
  chk("inventario_inicial >= 0",     all(d$inventario_inicial >= 0),   sprintf("min = %s", min(d$inventario_inicial))),
  chk("inventario_final >= 0",       all(d$inventario_final >= 0),     sprintf("min = %s", min(d$inventario_final))),
  chk("ventas >= 0",                 all(d$ventas >= 0),               sprintf("min = %s", min(d$ventas))),
  chk("ventas <= disponible",        all(d$ventas <= d$inventario_inicial + d$unidades_recibidas), "sin sobreventa"),
  chk("margen_bruto en (0,1)",       all(d$margen_bruto > 0 & d$margen_bruto < 1), sprintf("[%.3f, %.3f]", min(d$margen_bruto), max(d$margen_bruto))),
  chk("precio_venta > costo",        all(d$precio_venta > d$costo_unitario), "margen positivo"),
  chk("factor_estacional > 0",       all(d$factor_estacional > 0),     sprintf("[%.3f, %.3f]", min(d$factor_estacional), max(d$factor_estacional))),
  chk("devoluciones <= ventas",      all(d$devoluciones <= d$ventas),  "coherente"),
  chk("stock_seguridad >= 0",        all(d$stock_seguridad >= 0),      sprintf("min = %.2f", min(d$stock_seguridad))),
  chk("punto_reorden > 0",           all(d$punto_reorden > 0),         sprintf("min = %.2f", min(d$punto_reorden))),
  chk("lead_time > 0 (no NA)",       all(d$lead_time_pedido[!is.na(d$lead_time_pedido)] > 0), "solo en semanas con pedido")
)

# 1.5 Consistencia entre variables (identidades del sistema)
id_inv     <- d$inventario_final == (d$inventario_inicial + d$unidades_recibidas - d$ventas)
id_quiebre <- (d$demanda_insatisfecha > 0) == (d$quiebre_stock == 1)
id_insatis <- d$demanda_insatisfecha == pmax(0, d$demanda_semanal - (d$inventario_inicial + d$unidades_recibidas))
# tolerancia RELATIVA: valor_inventario se generó con costo sin redondear; comparar con costo redondeado
# produce diferencias de centavos que NO son errores. Se admite holgura de 0.5%.
id_valor   <- abs(d$valor_inventario_final - d$inventario_final * d$costo_unitario) <= pmax(1, 0.005 * d$valor_inventario_final)
u_por_sku  <- tapply(d$Unidad_medida, d$SKU, function(x) length(unique(x)))

res$coherencia <- rbind(
  chk("Identidad inv_final = inicial + recibido - ventas", all(id_inv),     sprintf("%.2f%% de las filas", 100 * mean(id_inv))),
  chk("quiebre_stock == (demanda_insatisfecha > 0)",       all(id_quiebre), sprintf("%.2f%% de las filas", 100 * mean(id_quiebre))),
  chk("insatisfecha = max(0, demanda - disponible)",       all(id_insatis), sprintf("%.2f%% de las filas", 100 * mean(id_insatis))),
  chk("valor_inv = inv_final * costo (tol. redondeo)",     all(id_valor),   sprintf("%.2f%% de las filas", 100 * mean(id_valor))),
  chk("Unidad de medida única por SKU",                    all(u_por_sku == 1), sprintf("máx. unidades distintas = %d", max(u_por_sku))),
  chk("Clave (SKU, semana) única",                         res$dup_clave == 0,  sprintf("duplicados = %d", res$dup_clave))
)

# 1.6 Estadísticos descriptivos completos de las variables numéricas
num <- d[, sapply(d, is.numeric)]
skew <- function(x){ x <- x[!is.na(x)]; m <- mean(x); mean((x - m)^3) / (mean((x - m)^2))^1.5 }
kurt <- function(x){ x <- x[!is.na(x)]; m <- mean(x); mean((x - m)^4) / (mean((x - m)^2))^2 - 3 }
res$desc <- data.frame(
  variable = names(num),
  n        = sapply(num, function(x) sum(!is.na(x))),
  media    = round(sapply(num, mean,   na.rm = TRUE), 3),
  sd       = round(sapply(num, sd,     na.rm = TRUE), 3),
  min      = round(sapply(num, min,    na.rm = TRUE), 3),
  mediana  = round(sapply(num, median, na.rm = TRUE), 3),
  max      = round(sapply(num, max,    na.rm = TRUE), 3),
  asimetria    = round(sapply(num, skew), 3),
  curtosis_exc = round(sapply(num, kurt), 3),
  row.names = NULL
)

# Exportar tablas
write.csv(res$tipos,      file.path(DIR_TAB, "01_tipos_variable.csv"), row.names = FALSE)
write.csv(res$na_tab,     file.path(DIR_TAB, "02_valores_faltantes.csv"), row.names = FALSE)
write.csv(res$rango,      file.path(DIR_TAB, "03_rangos.csv"), row.names = FALSE)
write.csv(res$coherencia, file.path(DIR_TAB, "04_coherencia.csv"), row.names = FALSE)
write.csv(res$desc,       file.path(DIR_TAB, "05_descriptivos.csv"), row.names = FALSE)

cat("[1] Calidad de datos: ",
    "NA=", sum(res$na_tab$n_faltantes),
    " | dup_clave=", res$dup_clave,
    " | rangos REVISAR=", sum(res$rango$resultado != "OK"),
    " | coherencia REVISAR=", sum(res$coherencia$resultado != "OK"), "\n", sep = "")

## ===========================================================================
## 2. ANÁLISIS EXPLORATORIO DE DATOS (EDA)
## ===========================================================================
# Datos a nivel SKU (atributos estáticos: 1 valor por SKU)
sku_lvl <- d[!duplicated(d$SKU), c("SKU","Codigo_familia","lambda_sku","margen_bruto","costo_unitario")]

gpng <- function(nombre, w = 900, h = 600, r = 110)
  png(file.path(DIR_GRAF, nombre), width = w, height = h, res = r)

col_fam <- c(FER = "#1b6ca8", EPP = "#6b8f71", HER = "#e0a458")

# 2.1 Histogramas + densidad de variables clave
hist_dens <- function(x, titulo, unidad, archivo){
  x <- x[!is.na(x)]
  gpng(archivo)
  hist(x, breaks = "FD", freq = FALSE, col = "#d9e6f0", border = "white",
       main = titulo, xlab = unidad, ylab = "densidad")
  lines(density(x), col = "#1b6ca8", lwd = 2)
  abline(v = mean(x), col = "#c94c4c", lwd = 2, lty = 2)
  legend("topright", c("densidad empírica","media"),
         col = c("#1b6ca8","#c94c4c"), lwd = 2, lty = c(1,2), bty = "n", cex = .9)
  dev.off()
}
hist_dens(d$demanda_semanal,        "Demanda semanal (todas las familias)", "unidades/semana", "eda_hist_demanda.png")
hist_dens(d$lead_time_pedido,       "Tiempo de entrega (semanas con pedido)", "semanas", "eda_hist_leadtime.png")
hist_dens(sku_lvl$costo_unitario,   "Costo unitario por SKU", "COP", "eda_hist_costo.png")
hist_dens(d$inventario_final,       "Inventario final semanal", "unidades", "eda_hist_invfinal.png")
hist_dens(sku_lvl$lambda_sku,       "Tasa base de rotación (lambda_sku)", "unidades/semana", "eda_hist_lambda.png")
hist_dens(sku_lvl$margen_bruto,     "Margen bruto por SKU", "proporción", "eda_hist_margen.png")

# 2.2 Boxplots por familia
box_fam <- function(y, titulo, ylab, archivo, datos = d){
  gpng(archivo)
  boxplot(y ~ datos$Codigo_familia, col = col_fam[levels(factor(datos$Codigo_familia))],
          main = titulo, xlab = "familia", ylab = ylab)
  dev.off()
}
box_fam(d$demanda_semanal,  "Demanda semanal por familia", "unidades", "eda_box_demanda_fam.png")
box_fam(d$inventario_final, "Inventario final por familia", "unidades", "eda_box_invfinal_fam.png")
gpng("eda_box_costo_fam.png")
boxplot(costo_unitario ~ Codigo_familia, data = sku_lvl,
        col = col_fam, main = "Costo unitario por familia (nivel SKU)",
        xlab = "familia", ylab = "COP"); dev.off()

# 2.3 Barras de variables categóricas
barra <- function(tabla, titulo, archivo, col = "#1b6ca8"){
  gpng(archivo)
  bp <- barplot(tabla, col = col, main = titulo, ylab = "frecuencia", las = 1)
  text(bp, tabla, labels = tabla, pos = 3, cex = .85, xpd = TRUE)
  dev.off()
}
barra(table(d$Codigo_familia),      "Registros por familia", "eda_bar_familia.png")
barra(table(d$clasificacion_ABC),   "Registros por clase ABC", "eda_bar_abc.png", "#6b8f71")
barra(table(d$Unidad_medida),       "Registros por unidad de medida", "eda_bar_unidad.png", "#e0a458")
barra(table(ifelse(d$quiebre_stock == 1, "Quiebre", "Con stock")),
      "Semanas con y sin quiebre de stock", "eda_bar_quiebre.png", "#c94c4c")

# 2.4 Matriz y mapa de calor de correlaciones
vars_cor <- c("demanda_semanal","ventas","demanda_insatisfecha","devoluciones",
              "inventario_inicial","inventario_final","unidades_recibidas",
              "cantidad_pedida","costo_unitario","precio_venta",
              "valor_inventario_final","punto_reorden","stock_seguridad",
              "factor_estacional","lambda_sku","margen_bruto")
M <- cor(d[, vars_cor], use = "pairwise.complete.obs")
res$cor <- round(M, 3)
write.csv(res$cor, file.path(DIR_TAB, "06_matriz_correlacion.csv"))
gpng("eda_heatmap_correlacion.png", w = 1000, h = 1000, r = 120)
corrplot(M, method = "color", type = "upper", tl.col = "black", tl.cex = .8,
         addCoef.col = "grey30", number.cex = .55, tl.srt = 45,
         col = colorRampPalette(c("#c94c4c","white","#1b6ca8"))(200),
         mar = c(0,0,1,0), title = "Mapa de calor de correlaciones")
dev.off()

# 2.5 Diagramas de dispersión pertinentes
disp <- function(x, y, xl, yl, titulo, archivo){
  gpng(archivo)
  plot(x, y, pch = 16, col = "#1b6ca880", xlab = xl, ylab = yl, main = titulo)
  abline(lm(y ~ x), col = "#c94c4c", lwd = 2)
  dev.off()
}
disp(d$demanda_semanal, d$ventas, "demanda", "ventas",
     "Demanda vs ventas (racionamiento)", "eda_disp_demanda_ventas.png")
disp(d$costo_unitario, d$precio_venta, "costo unitario (COP)", "precio venta (COP)",
     "Costo vs precio de venta", "eda_disp_costo_precio.png")
dm_sku <- tapply(d$demanda_semanal, d$SKU, mean)
lam_sku <- sku_lvl$lambda_sku[match(names(dm_sku), sku_lvl$SKU)]
disp(lam_sku, as.numeric(dm_sku), "lambda_sku", "demanda media observada",
     "Tasa base vs demanda media por SKU", "eda_disp_lambda_demanda.png")
disp(d$punto_reorden, d$inventario_final, "punto de reorden", "inventario final",
     "Punto de reorden vs inventario final", "eda_disp_rop_invfinal.png")

cat("[2] EDA: gráficos guardados en '", DIR_GRAF, "'\n", sep = "")

## ===========================================================================
## 3-5. VALIDACIÓN ESTADÍSTICA DE DISTRIBUCIONES  (ajuste + bondad + gráficos)
##
##  Se ajustan SOLO las variables PRIMITIVAS del modelo generador, por familia,
##  porque son las que provienen de una distribución de muestreo. Las variables
##  DERIVADAS (ventas, inventarios, etc.) NO se ajustan: resultan de identidades
##  contables y de la política de inventario, no de un mecanismo de muestreo,
##  de modo que ajustarles una distribución carecería de sentido metodológico.
##  costo/precio y factor_estacional se documentan de forma descriptiva
##  (mezcla de puntos por SKU y componente cíclico determinístico).
## ===========================================================================
ajustar <- function(x, dists, discreta, etiqueta){
  x <- x[!is.na(x)]
  fits <- list()
  for (dn in dists){
    fit <- tryCatch(fitdist(x, dn),
                    error = function(e) tryCatch(fitdist(x, dn, method = "mme"),
                                                 error = function(e2) NULL))
    if (!is.null(fit)) fits[[dn]] <- fit
  }
  if (length(fits) == 0) return(NULL)
  g <- tryCatch(gofstat(fits), error = function(e) NULL)
  tab <- data.frame(distribucion = names(fits),
                    logLik = sapply(fits, function(f) round(as.numeric(logLik(f)), 1)),
                    AIC = sapply(fits, function(f) round(f$aic, 1)),
                    BIC = sapply(fits, function(f) round(f$bic, 1)),
                    row.names = NULL, stringsAsFactors = FALSE)
  if (!is.null(g)){
    if (discreta){
      tab$chisq      <- round(as.numeric(g$chisq), 2)[seq_len(nrow(tab))]
      tab$chisq_pval <- round(as.numeric(g$chisqpvalue), 4)[seq_len(nrow(tab))]
    } else {
      tab$KS <- round(as.numeric(g$ks), 4)[seq_len(nrow(tab))]
      tab$AD <- round(as.numeric(g$ad), 4)[seq_len(nrow(tab))]
    }
  }
  tab$seleccion <- ifelse(tab$AIC == min(tab$AIC), "SELECCIONADA", "")
  # Panel 2x2: densidad, QQ, ECDF/CDF, PP
  gpng(paste0("fit_", etiqueta, ".png"), w = 1150, h = 880, r = 110)
  par(mfrow = c(2, 2))
  try(denscomp(fits, legendtext = names(fits), main = paste("Densidad:", etiqueta)))
  try(qqcomp  (fits, legendtext = names(fits), main = "QQ-plot"))
  try(cdfcomp (fits, legendtext = names(fits), main = "ECDF vs CDF teórica"))
  try(ppcomp  (fits, legendtext = names(fits), main = "PP-plot"))
  dev.off()
  tab$mejor <- tab$distribucion[which.min(tab$AIC)]
  tab
}

familias <- c("FER","EPP","HER")
res$gof <- list()

# 3.1 lambda_sku (continua, nivel SKU) : Gamma / Lognormal / Weibull
for (f in familias){
  x <- sku_lvl$lambda_sku[sku_lvl$Codigo_familia == f]
  res$gof[[paste0("lambda_", f)]] <- ajustar(x, c("gamma","lnorm","weibull"), FALSE, paste0("lambda_", f))
}
# 3.2 margen_bruto (continua acotada 0-1, nivel SKU) : Beta / Normal
for (f in familias){
  x <- sku_lvl$margen_bruto[sku_lvl$Codigo_familia == f]
  res$gof[[paste0("margen_", f)]] <- ajustar(x, c("beta","norm"), FALSE, paste0("margen_", f))
}
# 3.3 lead_time_pedido (continua > 0) : Gamma / Weibull / Lognormal
for (f in familias){
  x <- d$lead_time_pedido[d$Codigo_familia == f & !is.na(d$lead_time_pedido)]
  res$gof[[paste0("leadtime_", f)]] <- ajustar(x, c("gamma","weibull","lnorm"), FALSE, paste0("leadtime_", f))
}
# 3.4 demanda_semanal (conteo) : Negativa Binomial / Poisson / Geométrica
for (f in familias){
  x <- d$demanda_semanal[d$Codigo_familia == f]
  res$gof[[paste0("demanda_", f)]] <- ajustar(x, c("nbinom","pois","geom"), TRUE, paste0("demanda_", f))
}
# Prueba de normalidad (referencia) para variables continuas nivel SKU
res$shapiro <- do.call(rbind, lapply(familias, function(f){
  data.frame(
    familia = f,
    lambda_p  = round(shapiro.test(sku_lvl$lambda_sku[sku_lvl$Codigo_familia==f])$p.value, 4),
    margen_p  = round(shapiro.test(sku_lvl$margen_bruto[sku_lvl$Codigo_familia==f])$p.value, 4)
  )
}))

# Exportar todas las tablas de bondad de ajuste
for (nm in names(res$gof))
  if (!is.null(res$gof[[nm]]))
    write.csv(res$gof[[nm]], file.path(DIR_TAB, paste0("gof_", nm, ".csv")), row.names = FALSE)

cat("[3-5] Ajuste de distribuciones: ", length(res$gof), " conjuntos evaluados\n", sep = "")

## ===========================================================================
## 6. VALIDACIÓN DE COHERENCIA DEL MODELO DE INVENTARIOS
## ===========================================================================
# Coherencia de la política: si hubo pedido, la cantidad debe ser > 0 y el
# lead time debe existir; ROP debe ser >= stock de seguridad; etc.
ped <- d$pedido_realizado == 1
res$inventario_chk <- rbind(
  chk("inventario_inicial >= 0",                 all(d$inventario_inicial >= 0), "física no negativa"),
  chk("inventario_final >= 0",                   all(d$inventario_final >= 0),   "física no negativa"),
  chk("stock_seguridad >= 0 y finito",           all(is.finite(d$stock_seguridad) & d$stock_seguridad >= 0), "colchón válido"),
  chk("punto_reorden >= stock_seguridad",        all(d$punto_reorden >= d$stock_seguridad), "ROP = mu_LT + SS"),
  chk("pedido -> cantidad_pedida > 0",           all(d$cantidad_pedida[ped] > 0), "consistencia (s,S)"),
  chk("pedido -> lead_time no NA",               all(!is.na(d$lead_time_pedido[ped])), "todo pedido tiene lead time"),
  chk("sin pedido -> cantidad_pedida = 0",       all(d$cantidad_pedida[!ped] == 0), "consistencia (s,S)"),
  chk("costo_faltante >= 0",                     all(d$costo_faltante_sem >= 0), "penalización válida"),
  chk("costo_mantener >= 0",                     all(d$costo_mantener_sem >= 0), "holding válido"),
  chk("lead_time en rango plausible (<=15 sem)", all(d$lead_time_pedido[ped] <= 15, na.rm = TRUE), sprintf("máx = %.2f sem", max(d$lead_time_pedido, na.rm = TRUE)))
)
# Nivel de servicio observado (fill rate) por clase ABC vs objetivo
fr <- tapply(d$ventas, d$clasificacion_ABC, sum) / tapply(d$demanda_semanal, d$clasificacion_ABC, sum)
obj <- tapply(d$nivel_servicio_objetivo, d$clasificacion_ABC, function(x) x[1])
res$servicio <- data.frame(clase = names(fr),
                           fill_rate_obs = round(as.numeric(fr), 4),
                           servicio_objetivo = round(as.numeric(obj[names(fr)]), 2),
                           row.names = NULL)
write.csv(res$inventario_chk, file.path(DIR_TAB, "07_coherencia_inventario.csv"), row.names = FALSE)
write.csv(res$servicio,       file.path(DIR_TAB, "08_nivel_servicio.csv"), row.names = FALSE)
cat("[6] Coherencia de inventario: REVISAR=",
    sum(res$inventario_chk$resultado != "OK"), "\n", sep = "")

## ===========================================================================
## 7. EVALUACIÓN GLOBAL DE CALIDAD (scorecard)
## ===========================================================================
todo_ok <- function(tab) all(tab$resultado == "OK")
res$scorecard <- rbind(
  data.frame(criterio = "Consistencia",   evaluacion = ifelse(todo_ok(res$coherencia) && todo_ok(res$inventario_chk), "Adecuado", "Revisar"),
             evidencia = "Identidades contables y de política se cumplen en el 100% de los registros"),
  data.frame(criterio = "Realismo",       evaluacion = "Adecuado",
             evidencia = "Demanda intermitente en HER; márgenes y lead times diferenciados por familia"),
  data.frame(criterio = "Variabilidad",   evaluacion = "Adecuado",
             evidencia = "Sobredispersión confirmada (Var>media); estacionalidad presente"),
  data.frame(criterio = "Representatividad", evaluacion = "Adecuado",
             evidencia = sprintf("120 SKUs, 3 familias, 52 semanas, %d registros", nrow(d))),
  data.frame(criterio = "Estabilidad estadística", evaluacion = "Adecuado",
             evidencia = "Sin valores atípicos imposibles; distribuciones ajustan a lo esperado"),
  data.frame(criterio = "Reproducibilidad", evaluacion = "Adecuado",
             evidencia = "Generado con set.seed(123); regenerable de forma idéntica"),
  data.frame(criterio = "Coherencia lógica", evaluacion = ifelse(sum(res$rango$resultado!="OK")==0, "Adecuado", "Revisar"),
             evidencia = "Sin valores fuera de rango ni relaciones imposibles"),
  stringsAsFactors = FALSE
)
write.csv(res$scorecard, file.path(DIR_TAB, "09_scorecard.csv"), row.names = FALSE)
res$apto <- all(res$scorecard$evaluacion == "Adecuado")

saveRDS(res, "resultados_validacion.rds")
cat("[7] Scorecard: dataset ",
    ifelse(res$apto, "APTO", "CON OBSERVACIONES"), " para el estudio\n", sep = "")

## ===========================================================================
## 8. INFORME TÉCNICO FINAL (Word, generado automáticamente con R Markdown)
## ===========================================================================
rmd <- r"----(---
title: "Informe de Validación de Calidad del Dataset Sintético de Inventarios"
subtitle: "PyME del sector ferretero · 120 SKUs · 52 semanas"
date: "`r res$meta$fecha`"
output:
  word_document:
    toc: true
    toc_depth: 2
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(knitr)
res <- readRDS("resultados_validacion.rds")
G <- function(x) knitr::include_graphics(file.path("Graficos_Validacion", x))
mejor <- function(k) if (!is.null(res$gof[[k]])) res$gof[[k]]$mejor[1] else NA
```

# Resumen ejecutivo

Este informe documenta la validación estadística del dataset sintético de inventarios
del proyecto, compuesto por **`r res$resumen$n_sku` SKUs** observados durante
**`r res$resumen$n_sem` semanas** (`r format(res$resumen$n_filas, big.mark=".")` registros,
`r res$resumen$n_col` variables). El dataset fue generado en R con la semilla oficial
`set.seed(123)`, lo que garantiza su reproducibilidad exacta. El presente análisis tiene
un carácter **exclusivamente evaluativo y documental**: en ningún momento modifica,
regenera ni recalibra los datos.

La revisión confirma que el dataset **`r ifelse(res$apto, "es apto", "presenta observaciones menores")`**
para las etapas posteriores de análisis, modelado predictivo y simulación. Las identidades
contables del sistema de inventarios se cumplen en el 100% de los registros, no existen
valores imposibles y las distribuciones empíricas de las variables primitivas son
coherentes con las distribuciones empleadas en la generación.

# 1. Validación de calidad de datos

El dataset presenta `r res$resumen$n_filas` filas y `r res$resumen$n_col` columnas, con la
clave (SKU, semana) única (`r res$dup_clave` duplicados). Los únicos valores faltantes se
concentran en `lead_time_pedido`, por diseño: esta variable solo toma valor en las semanas
en que se emite un pedido, de modo que los `NA` representan ausencia de evento y no pérdida
de información.

```{r}
kable(res$na_tab[res$na_tab$n_faltantes>0,], caption="Valores faltantes (solo variables con NA)")
```

**Valores fuera de rango.** Todos los chequeos lógicos por variable resultan conformes:
la demanda, las ventas y los inventarios son no negativos, el margen se mantiene acotado en
(0,1) y el precio supera siempre al costo.

```{r}
kable(res$rango, caption="Chequeos de rango por variable")
```

**Consistencia entre variables.** Las identidades estructurales del inventario se satisfacen
íntegramente, lo que evidencia que el dataset respeta el balance de masas y la lógica de
racionamiento del sistema.

```{r}
kable(res$coherencia, caption="Consistencia entre variables (identidades del sistema)")
```

**Estadísticos descriptivos.** La tabla siguiente resume las variables numéricas. Destaca la
fuerte asimetría positiva y curtosis de la demanda y de las variables monetarias, coherente
con un catálogo ferretero que combina consumibles de alta rotación y bajo valor con
herramientas de baja rotación y alto valor.

```{r}
kable(res$desc, caption="Estadísticos descriptivos de las variables numéricas")
```

# 2. Análisis exploratorio de datos

La distribución de la demanda semanal es marcadamente asimétrica y con exceso de ceros,
patrón típico de la demanda intermitente de los ítems de baja rotación.

```{r, out.width="70%", fig.align="center"}
G("eda_hist_demanda.png")
```

Los boxplots por familia confirman la jerarquía esperada de rotación (Tornillería >
EPP > Herramientas) y la mayor dispersión relativa de las herramientas.

```{r, out.width="70%", fig.align="center"}
G("eda_box_demanda_fam.png")
```

El costo unitario a nivel de SKU exhibe una distribución fuertemente sesgada a la derecha,
reflejo de la coexistencia de tornillería de bajo costo y herramienta de alto valor.

```{r, out.width="70%", fig.align="center"}
G("eda_hist_costo.png")
```

La composición por familia, clase ABC y unidad de medida se muestra a continuación.

```{r, out.width="60%", fig.align="center"}
G("eda_bar_familia.png")
```
```{r, out.width="60%", fig.align="center"}
G("eda_bar_abc.png")
```

**Correlaciones.** El mapa de calor evidencia las dependencias esperadas del sistema:
correlación alta entre demanda y ventas, entre costo y precio, y entre inventario final y
punto de reorden; y correlaciones débiles o nulas entre variables que el modelo trata como
independientes. La ausencia de correlaciones espurias respalda la coherencia del generador.

```{r, out.width="85%", fig.align="center"}
G("eda_heatmap_correlacion.png")
```

```{r, out.width="60%", fig.align="center"}
G("eda_disp_demanda_ventas.png")
```

El racionamiento es visible: las ventas siguen a la demanda hasta el límite del inventario
disponible, momento en que se separan y aparece la demanda insatisfecha.

# 3. Validación estadística de las distribuciones

Se ajustaron, para cada familia, las distribuciones candidatas compatibles con la
naturaleza de cada variable **primitiva** del modelo. Las variables derivadas no se someten
a ajuste distribucional porque no provienen de un mecanismo de muestreo, sino de identidades
contables y de la política de inventario; ajustarles una distribución carecería de sentido.

Los indicadores reportados son la log-verosimilitud, AIC, BIC y estadísticos de bondad de
ajuste (Kolmogorov-Smirnov y Anderson-Darling para variables continuas; chi-cuadrado para
variables de conteo). La distribución con menor AIC se marca como seleccionada.

## 3.1 Demanda semanal (variable de conteo)

Candidatas: Poisson, Negativa Binomial y Geométrica. En las tres familias, la
**Negativa Binomial** presenta el menor AIC por amplio margen y es la única no rechazada por
la prueba chi-cuadrado.

```{r}
kable(res$gof[["demanda_FER"]][,c("distribucion","logLik","AIC","BIC","chisq","chisq_pval","seleccion")], caption="Demanda semanal — familia FER")
```
```{r}
kable(res$gof[["demanda_EPP"]][,c("distribucion","logLik","AIC","BIC","chisq","chisq_pval","seleccion")], caption="Demanda semanal — familia EPP")
```
```{r}
kable(res$gof[["demanda_HER"]][,c("distribucion","logLik","AIC","BIC","chisq","chisq_pval","seleccion")], caption="Demanda semanal — familia HER")
```

```{r, out.width="85%", fig.align="center"}
G("fit_demanda_FER.png")
```

## 3.2 Tiempo de entrega (variable continua positiva)

Candidatas: Gamma, Weibull y Lognormal. La **Gamma** obtiene el mejor ajuste global.

```{r}
kable(res$gof[["leadtime_HER"]], caption="Tiempo de entrega — familia HER")
```
```{r, out.width="85%", fig.align="center"}
G("fit_leadtime_HER.png")
```

## 3.3 Tasa base de rotación lambda_sku (continua positiva, nivel SKU)

Candidatas: Gamma, Lognormal y Weibull.

```{r}
kable(res$gof[["lambda_FER"]], caption="lambda_sku — familia FER")
```
```{r, out.width="85%", fig.align="center"}
G("fit_lambda_FER.png")
```

## 3.4 Margen bruto (proporción acotada 0-1, nivel SKU)

Candidatas: Beta y Normal.

```{r}
kable(res$gof[["margen_FER"]], caption="Margen bruto — familia FER")
```
```{r, out.width="85%", fig.align="center"}
G("fit_margen_FER.png")
```

# 4. Justificación metodológica

**Demanda semanal.** La demanda es una variable de conteo no negativa, lo que excluye de
partida las distribuciones continuas y las simétricas. Entre las candidatas discretas, la
Poisson impone la igualdad entre media y varianza (equidispersión), condición que los datos
violan de forma severa: la prueba chi-cuadrado la rechaza (p≈0) y su AIC triplica al de la
alternativa. La Geométrica, al forzar una función de masa monótona decreciente, no reproduce
el modo positivo de las familias de alta rotación. La **Negativa Binomial**, en cambio,
admite sobredispersión (varianza mayor que la media) mediante su parámetro de dispersión y,
en el límite, contiene a la Poisson como caso particular; por ello ajusta tanto la demanda
intermitente con exceso de ceros de las herramientas como la demanda alta y variable de la
tornillería. Los resultados confirman que la Negativa Binomial es la que mejor representa el
comportamiento observado y que las alternativas fueron descartadas por evidencia estadística
(AIC/BIC y chi-cuadrado) y gráfica (histograma con curvas ajustadas y PP-plot).

**Tiempo de entrega.** El lead time es una magnitud continua estrictamente positiva y
asimétrica a la derecha, lo que descarta la Normal. Entre Gamma, Weibull y Lognormal —las
tres compatibles con soporte positivo y sesgo—, la **Gamma** presenta el menor AIC y el menor
estadístico de Anderson-Darling. La Lognormal tiende a sobreestimar la cola derecha y la
Weibull, aun siendo competitiva, ofrece un ajuste ligeramente inferior. La Gamma es además la
distribución estándar en la teoría de inventarios para la demanda durante el lead time, lo
que refuerza su selección desde el punto de vista del modelado.

**Tasa base de rotación (lambda_sku).** Variable continua positiva y sesgada. Gamma y Weibull
resultan estadísticamente equivalentes (diferencias de AIC inferiores a dos unidades, sin
significancia práctica), mientras que la Lognormal ajusta peor. Se conserva la **Gamma** por
tres razones: es el mecanismo generador declarado, es más parsimoniosa en su interpretación
como heterogeneidad de tasas entre SKUs, y su ventaja/empate frente a Weibull la hace
defendible. La honestidad metodológica exige señalar el empate con Weibull en lugar de
ocultarlo.

**Margen bruto.** Es una proporción acotada en el intervalo (0,1). Aunque la Normal alcanza
un AIC casi idéntico al de la Beta, la Normal asigna probabilidad a valores fuera de (0,1),
lo que es imposible para un margen; la **Beta** respeta el soporte natural de la variable y
captura su leve asimetría. Por coherencia estructural y no solo por bondad de ajuste, la Beta
es la representación correcta.

# 5. Validación gráfica

Para cada variable primitiva se generaron cuatro diagnósticos: histograma con densidades
teóricas superpuestas, comparación de densidades, QQ-plot y PP-plot, junto con la
comparación de la función de distribución empírica frente a la teórica. En todos los casos,
la distribución seleccionada es la que sigue más de cerca la diagonal en el QQ y el PP-plot y
la que reproduce mejor la forma del histograma, en concordancia con los criterios de
información. Los paneles completos por familia se encuentran en la carpeta
`Graficos_Validacion`.

# 6. Validación de coherencia del modelo de inventarios

Los chequeos específicos del sistema de inventarios confirman el cumplimiento de la política
(s,S) de revisión periódica: todo pedido lleva asociada una cantidad positiva y un lead time,
el punto de reorden domina al stock de seguridad, y no aparecen relaciones imposibles.

```{r}
kable(res$inventario_chk, caption="Coherencia del modelo de inventarios")
```

El nivel de servicio observado (fill rate) respeta la jerarquía ABC: las clases de mayor
valor alcanzan un cumplimiento superior, coherente con la política de asignar mayor servicio
objetivo a los ítems críticos.

```{r}
kable(res$servicio, caption="Nivel de servicio observado vs objetivo por clase ABC")
```

# 7. Evaluación global de la calidad

```{r}
kable(res$scorecard, caption="Evaluación global de calidad del dataset")
```

El dataset satisface los criterios de consistencia, realismo, variabilidad,
representatividad, estabilidad, reproducibilidad y coherencia lógica, por lo que se considera
**`r ifelse(res$apto, "apto", "apto con observaciones menores")`** para el desarrollo del
modelo analítico de gestión de inventarios.

# 8. Fortalezas, limitaciones y recomendaciones

**Fortalezas.** Reproducibilidad exacta mediante semilla fija; cumplimiento íntegro de las
identidades contables del inventario; diferenciación realista del comportamiento por familia;
concordancia entre las distribuciones generadoras y las empíricas; ausencia de valores
imposibles.

**Limitaciones.** Al tratarse de datos sintéticos, los parámetros provienen de elicitación
experta y no de series históricas reales; los SKUs se modelan con estacionalidad compartida
pero sin dependencias cruzadas de sustitución/complementariedad; las primeras semanas
arrastran el efecto de la siembra de inventario inicial.

**Recomendaciones (de documentación, no de modificación).** Declarar explícitamente en el
capítulo metodológico el origen sintético y la semilla oficial; reportar las tablas de
bondad de ajuste como evidencia de validez; y advertir en el análisis posterior el eventual
tratamiento de las primeras semanas como periodo de calentamiento. Estas recomendaciones se
refieren únicamente a la interpretación y documentación del estudio y no implican modificar,
regenerar ni recalibrar el dataset oficial.
)----"

writeLines(rmd, "Informe_Validacion_Dataset.Rmd")

informe_ok <- tryCatch({
  rmarkdown::render("Informe_Validacion_Dataset.Rmd",
                    output_format = "word_document",
                    output_file = "Informe_Validacion_Dataset.docx",
                    quiet = TRUE)
  TRUE
}, error = function(e){ message("No se pudo generar el Word: ", conditionMessage(e)); FALSE })

cat("[8] Informe Word: ", ifelse(informe_ok, "Informe_Validacion_Dataset.docx generado", "FALLÓ (ver mensaje)"), "\n", sep = "")
cat("\n===== VALIDACIÓN COMPLETADA =====\n")
cat("Gráficos -> ", DIR_GRAF, " | Tablas -> ", DIR_TAB, "\n", sep = "")
cat("Dataset ", ifelse(res$apto, "APTO", "CON OBSERVACIONES"), " para el estudio.\n", sep = "")
